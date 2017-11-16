;;; 806752 Andrea Gullì
;;; (make-point x y)
;; ritorna un punto con coordinate x e y nel seguente formato ('point x y)
(defun make-point (x y)
  (cond ((and (numberp x) (numberp y)) (list 'point x y))))
;;; (is-point point)
;; ritorna T se point è un punto in formato ('point x y)
(defun is-point (point)
  (and (listp point)
       (eq 'point (first point))
       (numberp (second point))
       (numberp (third point))
       (eq nil (fourth point))))
;;; (is-slope slope)
;; ritorna T se slope è uno slope in formato ('slope angle pivot point)
(defun is-slope (slope)
  (and (listp slope)
       (eq 'slope (first slope))
       (numberp (second slope))
       (is-point (third slope))
       (eq nil (fourth slope))))
;;; (x point)
;; ritorna il valore della coordinata x del punto point
(defun x (point)
  (cond ((is-point point) (second point))))
;;; (y point)
;; ritorna il valore della coordinata y del punto point
(defun y (point)
  (cond ((is-point point) (third point))))
;;; (area2 a b c)
;; ritorna il doppio dell'area del triangolo ABC
(defun area2 (a b c)
  (cond ((and (is-point a) (is-point b) (is-point c))
         (-
           (* (- (x b) (x a)) (- (y c) (y a)))
           (* (- (y b) (y a)) (- (x c) (x a)))))))
;;; (left a b c)
;; Il predicato ritorna T se l'angolo ABC È un angolo a sinistra
(defun left (a b c)
  (cond ((> (area2 a b c) 0) t)))
;;; (left-on a b c)
;; Il predicato ritorna T se l'angolo ABC È un angolo a destra
(defun left-on (a b c)
  (cond ((< (area2 a b c) 0) t)))
;;; (is-collinear a b c)
;; Il predicato ritorna T se i punti A, B e C sono collineari
  (defun is-collinear (a b c)
    (cond ((= (area2 a b c) 0) t)))
;;; (angle2d a b).
;; ritorna l'angolo in radianti tra i punti A e B
(defun angle2d (a b)
  (atan (- (y a) (y b)) (- (x a) (x b))))
;;; (mymember point points)
;; verifico se il punto è ripetuto anche in points
(defun mymember (point points)
  (cond ((null points) nil)
        ((equal point (first points)) t)
        (t (mymember point (rest points)))))
;;; (rm-duplicates points)
;; ritorna la lista di punti senza punti uguali
(defun rm-duplicates (points)
  (cond ((null points) nil)
        ((eq nil (mymember (first points) (rest points)))
         (cons (first points) (rm-duplicates (rest points))))
        (t (rm-duplicates (rest points)))))
;;; (foreach-angle2d point points)
;; ritorna la lista di slopes calcolati chiamando angle2d su point e P
(defun foreach-angle2d (point points)
  (cond ((null points) nil)
        (t (cons
             (list 'slope (angle2d point (first points)) (first points))
             (foreach-angle2d point (rest points))))))
;;; (while-similar oslopes)
;; ritorna il primo segmento generabile dagli slopes
(defun while-similar (oslopes)
  (cond ((null oslopes) nil)
        ((eq (second (first oslopes)) (second (second oslopes)))
         (cons (third (first oslopes)) (while-similar (rest oslopes))))
        (t (cons (third (first oslopes)) nil))))
;;; (delete-similar oslopes)
;; cancella tutti gli slopes finchè sono simili
(defun delete-similar (oslopes)
  (cond ((null oslopes) nil)
        ((eq (second (first oslopes)) (second (second oslopes)))
         (delete-similar (rest oslopes)))
        (t (rest oslopes))))
;;; (point< a b)
;; criterio di sorting dei punti
(defun point< (a b)
  (cond ((null a) (not (null b)))
        ((null b) nil)
        ((= (second a) (second b)) (point< (rest a) (rest b)))
        (t (< (second a) (second b)))))
;;; (inner-segments pivot oslopes)
;; pivot è il punto di riferimento P e oslopes sono slopes ordinati secondo angle
;; dati gli slopes opportunamente ordinati trova le linee
(defun inner-segments (pivot oslopes)
  (cond ((null oslopes) nil)
        (t (cons
             (append '(line)
                      (sort (append (list pivot)
                                    (while-similar oslopes)) #'point<))
             (inner-segments pivot (delete-similar oslopes))))))
;;; (find-segments countdown points)
;; dato un insieme di punti ritorna tutti i segmenti RIPETUTI 2 VOLTE
(defun find-segments (countdown points)
  (cond ((null points) nil)
        ((> 0 countdown) nil)
        (t (append (inner-segments (car points)
                                   (sort
                                     (foreach-angle2d (car points)
                                                      (cdr points))
                                    #'< :key #'second))
                   (find-segments (- countdown 1)
                                  (append (rest points)
                                          (list (first points))))))))
;;; (only-big-lines lines)
;; elimino le linee con meno di 4 punti
(defun only-big-lines (lines)
  (cond ((null lines) nil)
        ((and (listp (first lines)) (> (length (first lines)) 4))
         (cons (first lines) (only-big-lines (rest lines))))
        (t (only-big-lines (rest lines)))))
;;; (line< l1 l2)
;; criterio di sorting delle linee
(defun line< (l1 l2)
  (cond ((null l1) (not (null l2)))
        ((null l2) nil)
        ((equal (second l1) (second l2)) (line< (rest l1) (rest l2)))
        (t (point< (second l1) (second l2)))))
;;; (line-patterns points) FUNZIONE PRINCIPALE
;; data points, una lista di punti ritorna una lista di segmenti
;; da almeno 4 punti collineari
(defun line-patterns (points)
  (cond ((null points) nil)
        (t (sort (only-big-lines
                   (rm-duplicates
                     (find-segments (length points)
                       (rm-duplicates points)))) #'line<))))
;;; (read-lines filename)
;; legge le linee del file
(defun read-lines (filename)
  (with-open-file (in filename)
    (loop for line = (read-line in nil nil)
      while line
      collect line)))
;;; (line-to-list line)
;; data una linea e.g 1 2, crea un punto (POINT 1 2)
(defun line-to-list (line)
  (read-from-string (concatenate 'string "(point " line ")")))
;;; (readpoints filename)
;; per ogni linea del file chiama line-to-list
(defun readpoints (filename)
  (mapcar #'line-to-list (read-lines filename)))
;;; (writelist filename lst)
;; scrive gli elementi della lista su file e va a capo
;; NON SOVRASCRIVE IL FILE, CONTINUA A SCRIVERCI SOPRA
(defun writelist (filename lst)
  (with-open-file (str filename
                   :direction :output
                   :if-exists :append
                   :if-does-not-exist :create)
     (format str (format nil "~~{~~a~~^~C~~}~%" #\Tab) lst)))
;;; (writelists filename points)
;; chiama writelist per ogni punto in points
(defun writepoints (filename points)
  (mapcan (lambda (point)
            (writelist filename (rest point)))
          points))
;;; (myflatten lst)
;; Ritorna la lista composta da tutti gli atomi presenti nella lista, portandoli
;; tutti allo stesso livello (eliminando le sublist) come la flatten
;; ma elimina 'point da ogni sublist
(defun myflatten (x)
	(cond ((null x) nil)
  ((eq 'point x) nil)
	((atom x) (list x))
	(t (append (myflatten (first x)) (myflatten (rest x))))))
;;; (write-segments filename segments)
;; scrive su file un insieme di rette, una per riga
(defun write-segments (filename segments)
  (mapcan (lambda (segment)
            (writelist filename (myflatten (rest segment))))
          segments))
