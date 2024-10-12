;; (defvar *db* nil)
(defvar *db* '((:TITLE "Lyle Lovett" :ARTIST "Lyle Lovett" :RATING 9 :RIPPED T)
              (:TITLE "Give Us a Break" :ARTIST "Limpopo" :RATING 10 :RIPPED T)
              (:TITLE "Rockin' the Suburbs" :ARTIST "Ben Folds" :RATING 6 :RIPPED T)
              (:TITLE "Home" :ARTIST "Dixie Chicks" :RATING 9 :RIPPED T)
              (:TITLE "Fly" :ARTIST "Dixie Chicks" :RATING 8 :RIPPED T)
              (:TITLE "Roses" :ARTIST "Kathy Mattea" :RATING 9 :RIPPED T)))

;; We gebruken een property list (plist)
;;  A plist is a list where every other element, starting with the first,
;;  is a symbol that describes what the next element in the list is.
(defun make-cd (title artist rating ripped)
  (list :title title :artist artist :rating rating :ripped ripped))

(defun add-record (cd) (push cd *db*))

(defun dump-db ()
  (dolist (cd *db*)
    (format t "~{~a:~10t~a~%~}~%" cd)))

(defun prompt-read (prompt)
  (format *query-io* "~a: " prompt)
  (force-output *query-io*)
  (read-line *query-io*))

(defun prompt-for-cd ()
  (make-cd
   (prompt-read "Title")
   (prompt-read "Artist")
   (or (parse-integer (prompt-read "Rating") :junk-allowed t) 0)
   (y-or-n-p "Ripped [y/n]: ")))

(defun add-cds ()
  (loop (add-record (prompt-for-cd))
      (if (not (y-or-n-p "Another? [y/n]: ")) (return))))

;; out here is not a function.
;; Usually it would be be but with-open-file is a macro that takes this as input.
;; It out becomes a variable where the output of the file is written.
(defun save-db (filename)
  (with-open-file (out filename
                   :direction :output
                   :if-exists :supersede)
    (with-standard-io-syntax
      (print *db* out))))

(defun load-db (filename)
  (with-open-file (in filename)
    (with-standard-io-syntax
      (setf *db* (read in)))))

;; The funny notation #' is shorthand for
;; "Get me the function with the following name."
;; Without the #', Lisp would treat evenp as the name of a variable and look up
;; the value of the variable, not the function.
;;
;; lambda is an anonymous function.
;; remove-if-not neemt als argumenten een functie en een predicate
;; We hoeven hier denk ik geen lambda te gebruiken, we zouden denk ik
;; in de functie select-by-artist ook een predicate function kunnen maken met defun
;; En dan die functie aan remove if not geven.
;; Het moet wel in de select-by-artist want alleen daar weten we artist
(defun select-by-artist (artist)
  (remove-if-not
   #'(lambda (cd) (equal (getf cd :artist) artist))
   *db*))

(defun select (selector-fn)
  (remove-if-not selector-fn *db*))

;; (defun artist-selector (artist)
;;   #'(lambda (cd) (equal (getf cd :artist) artist)))

;; &key betekent dat alles na &key keyword parameters zijn.
;; Dat betekent dat je old-where als volgt gebruikt.
;; (old-where :title title)
;;
;; old-where returned een functie die als argument een cd neemt
;; en true returned als de gegeven keyword parameters overeen komen met de cd
;;
;; We kunnen nu select als volgt gebruiken
;; (select (old-where :artist "Dixie Chicks"))
(defun old-where (&key title artist rating (ripped nil ripped-p))
  #'(lambda (cd)
      (and
       (if title    (equal (getf cd :title)  title)  t)
       (if artist   (equal (getf cd :artist) artist) t)
       (if rating   (equal (getf cd :rating) rating) t)
       (if ripped-p (equal (getf cd :ripped) ripped) t))))

;; (update (where :artist "Dixie Chicks") :rating 11)
(defun update (selector-fn &key title artist rating (ripped nil ripped-p))
  (setf *db*
        (mapcar
         #'(lambda (row)
             (when (funcall selector-fn row)
               (if title    (setf (getf row :title) title))
               (if artist   (setf (getf row :artist) artist))
               (if rating   (setf (getf row :rating) rating))
               (if ripped-p (setf (getf row :ripped) ripped)))
             row) *db*)))

(defun delete-rows (selector-fn)
  (setf *db* (remove-if selector-fn *db*)))


;; Chapter: Removing Duplication and Winning Big
;; Hier gaan we een macro maken van where
;; Ik snap niet helemaal waarom we macros gebruiken.

;; Dit returned een expression, die dan niet geevalueerd is.
;; Alles met een ' wordt niet geevalueerd.
;; Ik denk dat dat iets is wat je alleen voor macros gebruikt.
;;
;; voorbeeld:
;; CL-USER> (make-comparison-expr :rating 10)
;; (EQUAL (GETF CD :RATING) 10)
(defun make-comparison-expr (field value)
  (list 'equal (list 'getf 'cd field) value))

;; Dit is hetzelfde en blijkbaar beter.
;; Bij de backtick is alles unevaluated. Behalve de waardes met een comma ervoor.
(defun make-comparison-expr-2 (field value)
  `(equal (getf cd ,field) ,value))

;; loop is een macro
;; als loop een functie was dan kregen we een error dat 'while ongedefinieerd is.
;; collecting zorgt ervoor dat de resultaten van alle iteraties in een list worden gereturned.
;; denk ik.
(defun make-comparisons-list (fields)
  (loop while fields
     collecting (make-comparison-expr (pop fields) (pop fields))))

;; met het gebruk van macros, wordt dit de nieuwe where functie
;;
;; With a &rest in its parameter list, a function or macro can take
;; an arbitrary number of arguments, which are collected into a single list
;; that becomes the value of the variable whose name follows the &rest.
;;
;; The ,@ "splices" the value of the following expression
;; --which must evaluate to a list-- into the enclosing list.
;; `(and ,(list 1 2 3))   ==> (AND (1 2 3))
;; `(and ,@(list 1 2 3))  ==> (AND 1 2 3)
;; Ik denk dat de , en ,@ alleen beschikbaar zijn wanneer je ` gebruikt.
(defmacro where (&rest clauses)
  `#'(lambda (cd) (and ,@(make-comparisons-list clauses))))
