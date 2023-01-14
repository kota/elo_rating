(in-package :user)

(defvar *origin* nil "Whether this is an European or American tournament")
(defvar *reordering* nil "Whether the original ordering should be preserved.")
(defparameter *inverted-macmahon* nil "Whether macmahon points include the gamepoints")
(defvar *carl-johan* nil "If Performance Rating = Average Opponents' Rating + [(PctScore - 0.50) *850] is to be calculated")
(defparameter *normal-output* t "When doing trace for Wiel, stop the extra call to diff by using nil")
(defparameter *acc-weak-bonus* 0)
(defparameter *acc-uppset-gain* 0)

(defun run (&key (berger nil))
  (setq *handicap* nil
  *origin* nil
  *reordering* :undefined
  *inverted-macmahon* nil
  *special-promotions* nil
  *delayed-promotions* nil
  *importance* 1)
  (les "turnering.txt")
  (read-table)
  (calculate)
  (find-players)
  (unless *origin* (setq *origin* (find-origin)))
  (reorder berger)
  (show-players)
  (+/-)
  (special-promotions)
  (empty-simultan)
  (when (eql *origin* #\A) (change-us-names))
  (skriv1)
  (skriv2)
  (skriv3)
  (skriv4-html "turnering.html")
  (find-promotions)
  (write-promotions "turnering.html")
  (delayed-promotions)
  ;;(update-handicap-scores)
  (write-table)
  (show-players)
  (show-all *date*)
  (when *carl-johan* (carl-johan))
  (count-nationality))

(defun redo (year &key (from 1)(to 72)(berger nil))
  (cd (format nil "~d/t~d/" year from))
  (when (= from 1)
    (format t "~%CP start->pre")
    (run-shell-command "cp ../ratingliste.start ./ratingliste.pre")
    )
  (do ((next (1+ from) (1+ next)))
      ((> next to))
    (run :berger berger)
    (format t "~%CP post -> pre")
    (run-shell-command 
      (format nil "cp ./ratingliste.post ../t~d/ratingliste.pre" next))
    (cd (format nil "../t~d/" next))
    )
  (cd "../../")
  )

#|
(defun redo-2000 (&key (from 1)(to 78)(berger nil))
  (redo 2000 :from from :to to :berger berger))

(defun redo-2001 (&key (from 1)(to 60)(berger nil))
  (redo 2001 :from from :to to :berger berger))

(defun redo-2002 (&key (from 1)(to 63)(berger nil))
  (redo 2002 :from from :to to :berger berger))

(defun redo-2003 (&key (from 1) (to 61) (berger nil))
  (redo 2003 :from from :to to :berger berger))

(defun redo-2004 (&key (from 1) (to 64) (berger nil))
  (redo 2004 :from from :to to :berger berger))

(defun redo-2005 (&key (from 1) (to 61) (berger nil))
  (setq *super-bonus-active* nil)
  (redo 2005 :from from :to to :berger berger))

(defun redo-2006 (&key (from 1) (to 12) (berger nil))
  (setq *super-bonus-active* nil)
  (redo 2006 :from from :to to :berger berger))

(defun redo-2007 (&key (from 1) (to 54) (berger nil))
  (redo 2007 :from from :to (min to 11) :berger berger)
  (when (> to 11)
    (copy-file "2007/t10/ratingliste.post" "2007/t12/ratingliste.pre")
    (redo 2007 :from 12 :to to :berger berger)))

(defun redo-2008 (&key (from 1) (to 28) (berger nil))
  (redo 2008 :from from :to to :berger berger))

(defun redo-2009 (&key (from 1) (to 79) (berger nil))
  (redo 2009 :from from :to to :berger berger))

(defun redo-2011 (&key (from 54) (to 131) (berger nil))
  (redo 2011 :from from :to to :berger berger))

|#

(defun rdo (n &key (berger nil)(year 2012))
  (redo year :from n :to (1+ n) :berger berger))

(defun wiel (n &key (berger nil)(year '2010-wiel-2))
  (redo year :from n :to (1+ n) :berger berger))

(defvar +e-limit+ 18)
(defvar +u-limit+ 9)


(defparameter *rating* nil)
(defparameter *results* nil)
(defparameter *all* nil)
(defparameter *name* nil)
(defparameter *date* nil)
(defparameter *handicap* nil)

(defmacro while (test &rest body)
  `(do ()
       ((not ,test))
     ,@body))

(defun beste (liste &key (test #'>)(key #'identity))
   (when (null liste)(return-from beste nil))
   (let* ((beste (car liste))
    (beste-verdi (funcall key beste))
    ny-best)
      (dolist (ny (cdr liste))
   (when (funcall test (setq ny-best (funcall key ny)) beste-verdi)
      (setq beste ny
      beste-verdi ny-best)))
      beste))
      
(defun les (filename)
  (with-open-file (f filename :direction :input :external-format 'charset:iso-8859-1)
    (let ((nr 0)
    (started nil)
    (ended nil)
    (results nil)
    nr-2)
      (setq *name* (get-item f))
      (setq *date* (get-item f))
      (do ((line (read-line f nil :eof)(read-line f nil :eof)))
    ((eq line :eof))
  (setq nr-2 (parse-integer line :junk-allowed t))
  (cond ((or (= 0 (length line))
       (char= #\; (char line 0))))
        ((not (or (position #\+ line)
      (position #\- line)
      (position #\= line)
      (position #\& line)))
         (format t ".")
         (when started 
     (unless ended (setq ended t))))
        (ended (error "+/-/= starts stops and starts again: ~a" line))
        ((and (not started)(not (equal nr-2 1))) (format t ","))
        (t (format t ":")
     (unless started
       (unless (equal 1 nr-2)
         (error "First player not nr 1: ~a" line))
       (setq started t))
     (push (cons (incf nr)
           (parse-line line))
           results))))
      (unless started (error "No results read"))
      (setq *results* (reverse results)))))

(defun parse-line (line)
  (let ((macmahon 0)
  last-name first-name supplements results pos-[ pos-])
    (setq pos-[ (position #\[ line)
    pos-] (position #\] line))
    (when (< pos-] pos-[) (error "Unable to parse(1) [ and ] in ~a" line))
    (setq last-name (subseq line (1+ pos-[) pos-]))
    
    (setq pos-[ (position #\[ line :start (1+ pos-[)))
    (when (< pos-[ pos-]) (error "Unable to parse(2) [ and ] in ~a" line))
    (setq pos-] (position #\] line :start (1+ pos-])))
    (when (< pos-] pos-[) (error "Unable to parse(3) [ and ] in ~a" line))
    (setq first-name (subseq line (1+ pos-[) pos-]))
    
    (setq pos-[ (position #\[ line :start (1+ pos-[)))
    (when (< pos-[ pos-]) (error "Unable to parse(4) [ and ] in ~a" line))    
    (setq supplements (subseq line (1+ pos-]) pos-[))
    (setq pos-] (position #\] line :start (1+ pos-])))
    (when (< pos-] pos-[) (error "Unable to parse(5) [ and ] in ~a" line))
    (setq results (subseq line (1+ pos-[) pos-]))
    
    (setq pos-[ (position #\[ line :start (1+ pos-[)))
    (when pos-[
      (when (< pos-[ pos-]) (error "Unable to parse(6) [ and ] in ~a" line))
      (setq pos-] (position #\] line :start (1+ pos-])))
      (when (< pos-] pos-[) (error "Unable to parse(5) [ and ] in ~a" line))
      (setq macmahon (read-from-string (subseq line (1+ pos-[) pos-]))))
    
    (list* last-name first-name supplements macmahon (splitt-results results))))

(defun get-item (f)
  (let (start stop)
    (do ((line (read-line f nil :eof)(read-line f nil :eof)))
  ((eq :line :eof))
      (when (and (setq start (position #\[ line))
     (setq stop (position #\] line)))
  (return (subseq line (1+ start) stop))))))

(defvar *white-space* (list #\space #\return #\newline #\tab))

(defun next-none-whitespace (line &key (start 0))
  (let ((end (length line)))
    (while (and (< start end)
    (member (char line start) *white-space*))
     (incf start))
    (when (< start end)(values (char line start) 
             (1+ start)))))

(defun splitt-results (line)
  (let ((results nil)
  (parts nil)
  (pos 0)
  pos2
  (end (length line))
  next result tall side)
    (when (string= "&" (string-trim " " line))
      (return-from splitt-results :simultan))
    (while (< pos end)
     (setq parts nil)
     (setq next (next-none-whitespace line :start pos))
     (unless next (return))
     (unless (digit-char-p next)(error "Forventet tall : ~a " (subseq line pos)))
     (multiple-value-setq (tall pos)(parse-integer line 
               :start pos 
               :junk-allowed t))
     (push tall parts)
     (multiple-value-setq (result pos)
       (next-none-whitespace line :start pos))
     (push result parts)
     (multiple-value-setq (next pos2)
       (next-none-whitespace line :start pos))
     (when (and next
          (char= next #\())
       (multiple-value-setq (side pos)
         (next-none-whitespace line :start pos2))
       (push side parts)
       (setq pos2 (position #\) line :start pos))
       (push (string-trim *white-space* (subseq line pos pos2))
       parts)
       (setq pos (1+ pos2)))
     (push (reverse parts) results))
    (reverse results)))

(defstruct player 
  (last-name "")
  (first-name "")
  nationality-list
  (grade-level 0)
  (grade-name "")
  (nsr-grade-level 0)
  (nsr-grade-name "")
  elo-number
  games
  last-played
  (lb-count nil)
  (mp-count nil)
  (bonus-count 0))

(defstruct spiller 
  last-name 
  first-name
  supplements
  (macmahon 0)
  (result-rounds nil)
  (opponent-rounds nil)
  (handicap-side nil)
  (handicap-type nil)
  (clean-motstander-results nil)
  (u/p-k 1)
  (u-player nil)
  (simultan-p nil)
  (post-ant 0)
  (pts 0)
  sos
  sb
  (cum 0)
  (significance nil)
  nr
  old-spiller
  (rating-change 0))

(defstruct home
  list          ; #\E or #\A
  nationality
  residens
  last)

(defun spiller-navn (spiller)
  (format nil "~a ~a" (spiller-last-name spiller)(spiller-first-name spiller)))


(defvar *dummy* nil)

(defun make-dummy (elo &optional (u/p-k 1))
   (make-spiller :old-spiller (make-player :elo-number elo)
     :u/p-k u/p-k))

(defun pre-ant (spiller)
  (let ((value (player-games (spiller-old-spiller spiller))))
    (cond ((listp value) (length value))
    (t value))))
    
(defun post-ant (spiller)
   (spiller-post-ant spiller))

(defun (setf post-ant)(ant spiller)
   (setf (spiller-post-ant spiller) ant))

(defun calculate ()
  (let ((all (mapcar 
        (lambda (line)
    (make-spiller :last-name (second line)
            :first-name (third line)
            :supplements (string-trim '(#\space #\tab) 
              (fourth line))
            :nr (first line)
            :macmahon (fifth line)
            :pts (if *inverted-macmahon* 0 (fifth line))
            :cum (* (fifth line)
              (if (listp (nthcdr 5 line))
            (length (nthcdr 5 line))
          0))
            :simultan-p (eq :simultan (nthcdr 5 line))))
        *results*))
  motspiller)
    ;; (format t "~%TEST: ~a because inverted is ~a" all *inverted-macmahon*)
    (dolist (spiller all)
      (format t "~% name: ~a cum: ~a simltan-p: ~a" (spiller-last-name spiller) (spiller-cum spiller) (spiller-simultan-p spiller))
      )
    (push (setq *dummy* (make-spiller :last-name "DUMMY" 
              :nr 0 :pts 0) )
    all)
    (mapcar (lambda (spiller line)
        (unless (eq :simultan (nthcdr 5 line))
    (dolist (x (reverse (nthcdr 5 line)))
      (push (nth (first x) all)(spiller-opponent-rounds spiller))
      (push (ecase (second x)
        (#\+ 1)
        (#\- 0)
        (#\= 1/2))
      (spiller-result-rounds spiller))
      (when (third x)(setq *handicap* t))
      (push (third x)(spiller-handicap-side spiller))
      (push (fourth x)(spiller-handicap-type spiller)))))
      (cdr all) *results*)
    (dolist (spiller (cdr all))
      (unless (spiller-simultan-p spiller)
  (dotimes (round (length (spiller-opponent-rounds spiller)))
    (setq motspiller (nth round (spiller-opponent-rounds spiller)))
    (unless motspiller (error "Who did ~a ~a play against in round ~a?"
            (spiller-last-name spiller)
            (spiller-first-name spiller)
            (1+ round)))
    (cond ((eq motspiller *dummy*))
    ((spiller-simultan-p motspiller)
     (push spiller (spiller-opponent-rounds motspiller))
     (push (- 1 (nth round (spiller-result-rounds spiller)))
           (spiller-result-rounds motspiller))
     (push (ecase (nth round (spiller-handicap-side spiller))
       (#\+ #\-)
       (#\- #\+)
       ((nil) nil))
           (spiller-handicap-side motspiller))
     (push (nth round (spiller-handicap-type spiller))
           (spiller-handicap-type motspiller)))
    ((not (eq spiller (nth round (spiller-opponent-rounds motspiller))))
     (error "Spillte ~a og ~a mot hverandre i runde ~a?" 
      (spiller-navn spiller)
      (spiller-navn motspiller)
      (1+ round)))
    ((not (= 1 (+ (nth round (spiller-result-rounds spiller))
            (nth round (spiller-result-rounds motspiller)))))
     (error "~a og ~a fikke ikke et poeng tilsammen i runde ~d (~a ~a  ~a ~a)" 
      (spiller-navn spiller)
      (spiller-navn motspiller)
      (1+ round)
      (spiller-result-rounds spiller)
      (mapcar #'spiller-navn (spiller-opponent-rounds spiller))
      (spiller-result-rounds motspiller)
      (mapcar #'spiller-navn 
        (spiller-opponent-rounds motspiller))))
    (t (sjekk-handicap round spiller motspiller))))))
    (dolist (spiller (cdr all))
      (incf (spiller-pts spiller)
  (reduce #'+ (spiller-result-rounds spiller))))
    (dolist (spiller (cdr all))
      (setf (spiller-sos spiller)
  (reduce #'+ (mapcar (lambda (spiller)(spiller-pts spiller))
          (spiller-opponent-rounds spiller))))
      (setf (spiller-sb spiller)
  (reduce #'+ (mapcar (lambda (spiller result)
            (* result (spiller-pts spiller)))
          (spiller-opponent-rounds spiller)
          (spiller-result-rounds spiller))))
      (incf (spiller-cum spiller)
  (let ((sum 0)
        (sum-sum 0))
    (dolist (res (spiller-result-rounds spiller))
      (incf sum res)
      (incf sum-sum sum))
    sum-sum)))
    (dolist (spiller (cdr all))
      (setf (spiller-clean-motstander-results spiller)
  (remove-if (lambda (x)(eq (first x) *dummy*))
       (mapcar #'list
         (spiller-opponent-rounds spiller)
         (spiller-result-rounds spiller)
         (spiller-handicap-side spiller)
         (spiller-handicap-type spiller)))))
    (setq *all* (cdr all))))

(defun sjekk-handicap (round spiller motspiller)
  (let ((side1 (nth round (spiller-handicap-side spiller)))
  (side2 (nth round (spiller-handicap-side motspiller)))
  (type-1 (nth round (spiller-handicap-type spiller)))
  (type-2 (nth round (spiller-handicap-type motspiller))))
    (unless (or (and (null side1)
         (null side2))
    (and side1
         side2
         (char= #\+ side1)
         (char= #\- side2))
    (and side1
         side2
         (char= #\- side1)
         (char= #\+ side2)))
      (error "With handicap one player has to give and the other receive: Round ~d: ~a -  ~a" 
       (1+ round)
       (spiller-navn spiller)
       (spiller-navn motspiller)))
    (unless (or (and (null side1)
         (null type-1)
         (null type-2))
    (and side1 side2
         (string-equal type-1 type-2)))
      (error "With handicap, both players must be influenced by the same handicap: Round ~d: ~a -  ~a" 
       (1+ round)
       (spiller-navn spiller)
       (spiller-navn motspiller)))))

(defun find-origin ()
  (let ((eu 0)
  (us 0))
    (dolist (spiller *all*)
      (let ((nat-list (player-nationality-list 
           (spiller-old-spiller spiller))))
  (when (member #\E nat-list :key #'home-list)
    (incf eu))
  (when (member #\A nat-list :key #'home-list)
    (incf us))))
    (cond ((> us eu) #\A)
    ((>= eu us) #\E)
    (t (error "EU or US tournament?")))))

(defun change-us-names ()
  (dolist (spiller *all*)
    (let ((old (spiller-first-name spiller)))
      (when (and (> (length old) 3)
     (string-not-equal old "Mrs."))
  (setf (spiller-first-name spiller)
    (format nil "~c." (char old 0)))))))
               
(defun skriv1 ()
  (let ((last-name-size (reduce #'max (mapcar (lambda (spiller)
            (length (spiller-last-name spiller)))
                *all*)))
  (first-name-size (reduce #'max (mapcar (lambda (spiller)
            (length (spiller-first-name spiller)))
                *all*)))
  (supplement-size (reduce #'max (mapcar (lambda (spiller)
             (length (spiller-supplements spiller)))
                 *all*))))
    (dolist (spiller *all*)
      (format t "~%~:[ =~;~:*~2d~] ~va ~va ~v@a ~4d~:[ ~;*~] ~{~{~2d~c~:[~2*~;~:[~*     ~;(~:*~c~2a)~]~]~} ~} ~2d ~:[   ~;~:*~3d~] ~:[   ~;~:*~3d~] ~:[   ~;~:*~3d~] ~:[~;~:*~4@d~]"
        (if (= (spiller-significance spiller) 5) nil (spiller-nr spiller))
        last-name-size
        (spiller-last-name spiller)
        first-name-size       
        (spiller-first-name spiller)
        supplement-size
        (spiller-supplements spiller)
        (if (spiller-u-player spiller)
      (spiller-rating-change spiller)
      (player-elo-number (spiller-old-spiller spiller)))
        (spiller-u-player spiller)
        (mapcar (lambda (motstander result h-side h-type)
      (list (spiller-nr motstander) 
            (ecase result 
        (1 #\+)
        (0 #\-)
        (1/2 #\=))
            *handicap*
            h-side
            h-type))
          (spiller-opponent-rounds spiller)
          (spiller-result-rounds spiller)
          (spiller-handicap-side spiller)
          (spiller-handicap-type spiller))
        (spiller-pts spiller)
        (if (>= (spiller-significance spiller) 2)(spiller-sos spiller) nil)
        (if (>= (spiller-significance spiller) 3)(spiller-sb spiller) nil)
        (if (>= (spiller-significance spiller) 4)(spiller-cum spiller) nil)
        (if (spiller-u-player spiller)
      nil
      (spiller-rating-change spiller))))))

(defun skriv2 ()
  (let ((last-name-size (reduce #'max (mapcar (lambda (spiller)
            (length (spiller-last-name spiller)))
                *all*)))
  (first-name-size (reduce #'max (mapcar (lambda (spiller)
             (length (spiller-first-name spiller)))
                 *all*))))
    (format t "~2%~a~%Nr Name ~vtNat  Grade         ELO  " *name* (+ last-name-size first-name-size 5))
    (dotimes (r (length (spiller-opponent-rounds (second *all*))))
      (format t " ~2d " (1+ r))
      (when *handicap* (format t "     ")))
    (format t " Pts Sos  Sb Cum  +/-")
    (dolist (spiller *all*)
      (format t "~%~:[ =~;~:*~2d~] ~va ~va ~3a ~:[~*  ~;~2d~] ~3a ~:[~2*       ~;(~d ~3a)~] ~4d~:[ ~;*~] ~{~{~2d~c~:[~2*~;~:[~*     ~;(~:*~c~2a)~]~]~} ~} ~2d ~:[   ~;~:*~3d~] ~:[   ~;~:*~3d~] ~:[   ~;~:*~3d~] ~:[~;~:*~4@d~]"
        (if (= (spiller-significance spiller) 5) nil (spiller-nr spiller))
        last-name-size
        (spiller-last-name spiller)
        first-name-size       
        (spiller-first-name spiller)
        (home-nationality (first (player-nationality-list
          (spiller-old-spiller spiller))))
        (plusp (player-grade-level (spiller-old-spiller spiller)))
        (player-grade-level (spiller-old-spiller spiller))
        (player-grade-name (spiller-old-spiller spiller))
        (plusp (player-nsr-grade-level (spiller-old-spiller spiller)))
        (player-nsr-grade-level (spiller-old-spiller spiller))
        (player-nsr-grade-name (spiller-old-spiller spiller))
        (if (spiller-u-player spiller)
      (spiller-rating-change spiller)
      (player-elo-number (spiller-old-spiller spiller)))
        (spiller-u-player spiller)
        (mapcar (lambda (motstander result h-side h-type)
      (list (spiller-nr motstander) 
            (ecase result 
        (1 #\+)
        (0 #\-)
        (1/2 #\=))
            *handicap*
            h-side
            h-type))
          (spiller-opponent-rounds spiller)
          (spiller-result-rounds spiller)
          (spiller-handicap-side spiller)
          (spiller-handicap-type spiller))
        (spiller-pts spiller)
        (if (>= (spiller-significance spiller) 2)(spiller-sos spiller) nil)
        (if (>= (spiller-significance spiller) 3)(spiller-sb spiller) nil)
        (if (>= (spiller-significance spiller) 4)(spiller-cum spiller) nil)
        (if (spiller-u-player spiller)
      nil
      (spiller-rating-change spiller))))))

(defun skriv2-html (filename)
  (with-open-file (f filename :direction :output :if-exists :supersede :external-format 'charset:iso-8859-1)
    (format f "~%<table>~%<tr><td>Nr</td><td>Name</td><td></td><td>Nat </td><td>Grade </td><td>ELO</td><td>")
    (dotimes (r (length (spiller-opponent-rounds (second *all*))))
      (format f " ~2d</td><td>" (1+ r)))
    (format f "Pts</td><td>Sos</td><td>Sb</td><td>Cum</td><td>+/-</td></tr>")
    (dolist (spiller *all*)
      (format f "~%<tr><td>~:[ =~;~:*~2d~]</td><td>~a</td><td>~a</td><td> ~3a</td><td align=\"right\">~:[~*  ~;~2d~] ~3a</td><td align=\"right\">~4d~:[ &nbsp~;*~] ~{~{</td><td>~2d~c~:[~2*~;~:[~*~;(~:*~c~a)~]~]~}~}</td><td align=\"right\">~2d</td><td align=\"right\">~:[   ~;~:*~3d~]</td><td align=\"right\">~:[   ~;~:*~3d~]</td><td align=\"right\">~:[   ~;~:*~3d~]</td><td align=\"right\">~:[~;~:*~4@d~]</td></tr>"
        (if (= (spiller-significance spiller) 5) nil (spiller-nr spiller))
        (spiller-last-name spiller)
        (spiller-first-name spiller)
        (home-nationality (first (player-nationality-list 
          (spiller-old-spiller spiller))))
        (plusp (player-grade-level (spiller-old-spiller spiller)))
        (player-grade-level (spiller-old-spiller spiller))
        (player-grade-name (spiller-old-spiller spiller))
        (if (spiller-u-player spiller)
      (spiller-rating-change spiller)
      (player-elo-number (spiller-old-spiller spiller)))
        (spiller-u-player spiller)
        (mapcar (lambda (motstander result h-side h-type)
      (list (spiller-nr motstander) 
            (ecase result 
        (1 #\+)
        (0 #\-)
        (1/2 #\=))
            *handicap*
            h-side
            h-type))
          (spiller-opponent-rounds spiller)
          (spiller-result-rounds spiller)
          (spiller-handicap-side spiller)
          (spiller-handicap-type spiller))
        (spiller-pts spiller)
        (if (>= (spiller-significance spiller) 2)(spiller-sos spiller) nil)
        (if (>= (spiller-significance spiller) 3)(spiller-sb spiller) nil)
        (if (>= (spiller-significance spiller) 4)(spiller-cum spiller) nil)
        (cond ((zerop (player-elo-number (spiller-old-spiller spiller)))
         nil)
        (t (spiller-rating-change spiller)))))
    (format f "~%</table>~%")))

(defun skriv3 ()
  (let ((last-name-size 
   (reduce #'max (mapcar (lambda (spiller)
         (length (spiller-last-name spiller)))
             *all*)))
  (first-name-size 
   (reduce #'max (mapcar (lambda (spiller)
         (length (spiller-first-name spiller)))
             *all*)))
  (show-macmahon (some #'plusp (mapcar #'spiller-macmahon *all*))))
    (format t "~2%Nr Name ~vtNat  Grade  ELO  " 
      (+ last-name-size first-name-size 5))
    (dotimes (r (length (spiller-opponent-rounds (second *all*))))
      (format t " ~2d " (1+ r))
      (when *handicap* (format t "     ")))
    (when show-macmahon (format t "MMS"))
    (format t " Pts   +/-")
    (dolist (spiller *all*)
      (format t "~%~2d ~va ~va ~3a ~:[~*  ~;~2d~] ~3a  ~4d~:[ ~;*~] ~{~{~2d~c~:[~2*~;~:[~*     ~;(~:*~c~2a)~]~]~} ~}~:[~*~;~2d~] ~2d  ~:[~;~:*~4@d~]"
        (spiller-nr spiller)
        last-name-size
        (spiller-last-name spiller)
        first-name-size       
        (spiller-first-name spiller)
        (home-nationality (first (player-nationality-list 
          (spiller-old-spiller spiller))))
        (plusp (player-grade-level (spiller-old-spiller spiller)))
        (player-grade-level (spiller-old-spiller spiller))
        (player-grade-name (spiller-old-spiller spiller))
        (if (spiller-u-player spiller)
      (spiller-rating-change spiller)
      (player-elo-number (spiller-old-spiller spiller)))
        (spiller-u-player spiller)
        (mapcar (lambda (motstander result h-side h-type)
      (list (spiller-nr motstander) 
            (ecase result 
        (1 #\+)
        (0 #\-)
        (1/2 #\=))
            *handicap*
            h-side
            h-type))
          (spiller-opponent-rounds spiller)
          (spiller-result-rounds spiller)
          (spiller-handicap-side spiller)
          (spiller-handicap-type spiller))
        show-macmahon
        (spiller-macmahon spiller)
        (spiller-pts spiller)
        (if (spiller-u-player spiller)
      nil
      (spiller-rating-change spiller))))))

(defun skriv3-html (filename)
  (with-open-file (f filename :direction :output :if-exists :supersede)
    (format f "~%<table>~%<tr><td>Nr</td><td> Name</td><td></td><td>Nat </td><td> Grade </td><td> ELO</td><td>  " )
    (dotimes (r (length (spiller-opponent-rounds (second *all*))))
      (format f "~2d</td><td>" (1+ r)))
    (format f "Pts</td><td>Sb</td><td>+/-</td></tr>")
    (dolist (spiller *all*)
      (format f "~%<tr><td>~:[ =~;~:*~2d~]</td><td> ~a</td><td> ~a</td><td> ~3a</td><td align=\"right\">~:[~*  ~;~2d~] ~3a</td><td align=\"right\">~4d~:[ &nbsp~;*~] ~{~{</td><td>~2d~c~:[~2*~;~:[~*~;(~:*~c~a)~]~]~} ~}</td><td align=\"right\">~2d</td><td align=\"right\">~:[   ~;~:*~3d~]</td><td align=\"right\">~:[~;~:*~4@d~]</td></tr>"
        (if (>= (spiller-significance spiller) 4) 
      nil 
    (spiller-nr spiller))
        (spiller-last-name spiller)
        (spiller-first-name spiller)
        (home-nationality (first (player-nationality-list 
          (spiller-old-spiller spiller))))
        (plusp (player-grade-level (spiller-old-spiller spiller)))
        (player-grade-level (spiller-old-spiller spiller))
        (player-grade-name (spiller-old-spiller spiller))
        (if (spiller-u-player spiller)
      (spiller-rating-change spiller)
      (player-elo-number (spiller-old-spiller spiller)))
        (spiller-u-player spiller)
        (mapcar (lambda (motstander result h-side h-type)
       (list (spiller-nr motstander) 
             (ecase result 
         (1 #\+)
         (0 #\-)
         (1/2 #\=))
             *handicap*
             h-side
             h-type))
          (spiller-opponent-rounds spiller)
          (spiller-result-rounds spiller)
          (spiller-handicap-side spiller)
          (spiller-handicap-type spiller))
        (spiller-pts spiller)
        (if (>= (spiller-significance spiller) 3)
      (spiller-sb spiller) 
    nil)
        (cond ((zerop (player-elo-number (spiller-old-spiller spiller)))
         nil)
        (t (spiller-rating-change spiller)))))
    (format f "~%</table>~%")))

(defun skriv4-html (filename)
  (with-open-file (f filename :direction :output :if-exists :supersede :external-format 'charset:iso-8859-1)
    (format f "~3%<h3>~a</h3>" *name*)
    (format f "~%<table>~%<tr><td>Nr</td><td> Name</td><td></td><td>Nat </td><td> Grade </td><td> ELO</td><td>  " )
    (dotimes (r (length (spiller-opponent-rounds (second *all*))))
      (format f "~2d</td><td>" (1+ r)))
    (format f "Pts</td><td>+/-</td></tr>")
    (dolist (spiller *all*)
      (format f "~%<tr><td>~2d</td><td> ~a</td><td> ~a</td><td> ~3a</td><td align=\"right\">~:[~*  ~;~2d~] ~3a</td><td align=\"right\">~4d~:[ &nbsp~;*~] ~{~{</td><td>~2d~c~:[~2*~;~:[~*~;(~:*~c~a)~]~]~} ~}</td><td align=\"right\">~2d</td><td align=\"right\">~:[~;~:*~4@d~]</td></tr>"
        (spiller-nr spiller)
        (spiller-last-name spiller)
        (spiller-first-name spiller)
        (home-nationality (first (player-nationality-list 
          (spiller-old-spiller spiller))))
        (plusp (player-grade-level (spiller-old-spiller spiller)))
        (player-grade-level (spiller-old-spiller spiller))
        (player-grade-name (spiller-old-spiller spiller))
        (if (spiller-u-player spiller)
      (spiller-rating-change spiller)
      (player-elo-number (spiller-old-spiller spiller)))
        (spiller-u-player spiller)
        (mapcar (lambda (motstander result h-side h-type)
       (list (spiller-nr motstander) 
             (ecase result 
         (1 #\+)
         (0 #\-)
         (1/2 #\=))
             *handicap*
             h-side
             h-type))
          (spiller-opponent-rounds spiller)
          (spiller-result-rounds spiller)
          (spiller-handicap-side spiller)
          (spiller-handicap-type spiller))
        (spiller-pts spiller)
        (cond ((zerop (player-elo-number 
           (spiller-old-spiller spiller)))
         nil)
        (t (spiller-rating-change spiller)))))
    (format f "~%</table>~%<br>~%")))

(defun write-promotions (filename)
  (with-open-file (f filename :direction :output :if-exists :append :external-format 'charset:iso-8859-1)
    (when *promotions* 
      (format f "~{~a<br>~%~}<br>~%" (reverse *promotions*)))
    (format f "<br>~%<br>~%")))

(defun reorder (berger)
  (when (or (eq *reordering* t)
      (and (eq *reordering* :undefined)
     (eql *origin* #\A)))
    (setq *all* (sort *all* #'spiller>=)))
  (let ((nr 0))
    (dolist (spiller *all*)
      (setf (spiller-nr spiller)(incf nr))))
  (mapl (lambda (spillere)
    (when (> (length spillere) 1)
      (let ((diff (equalness (first spillere)(second spillere) berger)))
        (push diff (spiller-significance (first spillere)))
        (push diff (spiller-significance (second spillere))))))
  *all*)
  (dolist (spiller *all*)
    (format t "~a ~a : ~a~%" 
      (spiller-last-name spiller)
      (spiller-first-name spiller)
      (spiller-significance spiller))
    (setf (spiller-significance spiller)(reduce #'max (spiller-significance spiller)))))


(defun spiller>= (s1 s2)
  (cond ((> (spiller-pts s1)(spiller-pts s2)) t)
  ((< (spiller-pts s1)(spiller-pts s2)) nil)
  ((> (spiller-sos s1)(spiller-sos s2)) t)
  ((< (spiller-sos s1)(spiller-sos s2)) nil)
  ((> (spiller-sb s1)(spiller-sb s2)) t)
  ((< (spiller-sb s1)(spiller-sb s2)) nil)
  ((> (spiller-cum s1)(spiller-cum s2)) t)
  ((< (spiller-cum s1)(spiller-cum s2)) nil)
  (t t)))

(defun equalness (s1 s2 berger)
  (cond ((null s2) 1)
  ((/= (spiller-pts s1)(spiller-pts s2)) 1)
  ((and (not berger)(/= (spiller-sos s1)(spiller-sos s2))) 2)
  ((/= (spiller-sb s1)(spiller-sb s2)) 3)
  ((and (not berger)(/= (spiller-cum s1)(spiller-cum s2))) 4)
  (t (push 5 (spiller-significance s2))
     4)))

(defun read-table (&optional (filnavn1 "ratingliste.pre")(filnavn2 "nye-spillere"))
  (with-open-file (f filnavn1 :direction :input :external-format 'charset:iso-8859-1)
    (setq *rating* (read f)))
  (with-open-file (f filnavn2 :direction :input :if-does-not-exist nil :external-format 'charset:iso-8859-1)
    (when (streamp f)
      (setq *rating* (append (mapcar #'parse-new-player (read f) )
           *rating*))
      (eval (ignore-errors (read f))))))

(defun write-table (&optional (filnavn "ratingliste.post"))
  (setq *print-pretty* nil)
  (dolist (spiller *all*)
    (incf (player-elo-number (spiller-old-spiller spiller))
    (spiller-rating-change spiller)))
  (setq *rating* (stable-sort *rating* #'> :key #'player-elo-number))
  (with-open-file (f filnavn :direction :output :if-exists :supersede :external-format 'charset:iso-8859-1)
    (format f "(")
    (dolist (x *rating*) (format f "~s~%" x))
    (format f "~%)~%")))

(defun parse-new-player (list-player)
  (make-player :last-name (first list-player)
         :first-name (second list-player)
         :nationality-list (third list-player)
         :nsr-grade-level (fourth list-player)
         :nsr-grade-name (fifth list-player)
         :elo-number (sixth list-player)
         :games (seventh list-player)
         :last-played (eighth list-player)
         :lb-count (ninth list-player)
         :mp-count (tenth list-player)))


(defun find-player (last-name first-name)
  (cond ((find-if (lambda (spiller)
        (and (string= (player-last-name spiller) last-name)
       (string= (player-first-name spiller) 
          first-name)))
      *rating*))
  (t (error "Unknown player: ~a ~a" last-name first-name))))

(defun find-players ()
  (dolist (spiller *all*)
    (cond ((new-old-spiller spiller (make-player))
     (setf (spiller-old-spiller spiller)
       (make-player :last-name "Dummy" :first-name "" 
        :nationality-list '(#S(HOME :nationality "")) 
        :grade-level 0 :grade-name "" 
        :nsr-grade-level 0 :nsr-grade-name ""
        :games 0 
        :last-played 0)))
    ((spiller-simultan-p spiller)
     (setf (spiller-old-spiller spiller) 
       (make-player :last-name "" :first-name "" 
        :nationality-list '(#S(HOME :nationality "")) 
        :grade-level 0 :grade-name "" 
        :nsr-grade-level 0 :nsr-grade-name ""
        :games 0 
        :last-played 0)))
    ((setf (spiller-old-spiller spiller)
       (car (member spiller *rating* :test #'new-old-spiller))))
    (t (error "Unknown player : ~s ~s" 
        (spiller-last-name spiller)
        (spiller-first-name spiller))))
    (unless (date-before (player-last-played (spiller-old-spiller spiller))
       *date*)
      (error "~a ~a has played at ~a which is after this date ~a"
       (spiller-last-name spiller)
       (spiller-first-name spiller)
       (player-last-played (spiller-old-spiller spiller))
       *date*))))

(defun date-before (old-date new-date)
  (cond ((null old-date) t)
  ((numberp old-date) t)
  (t (string< old-date new-date))))

(defun new-old-spiller (new old)
  (and (string= (spiller-last-name new)
    (player-last-name old))
       (string= (spiller-first-name new)
    (player-first-name old))))

(defun innsats (poeng motstandere &key (dummy nil)(show t))
  (when dummy (incf poeng 1/2))
  (let ((guess (first motstandere))
  (limit 0.001)
  a b k1 k2 modification)
    (loop 
      (setq a 0
      b 0)
      (dolist (m motstandere)
  (setq k1 (expt 10 (/ (- m guess) 400.0)))
  (setq k2 (/ (1+ k1)))
  (incf a k2)
  (incf b (* k1 k2 k2)))
      (setq modification (/ (- poeng a)
          (* b (/ (log 10) 400))))
      (cond ((> modification 900)(setq modification 900))
      ((< modification -900)(setq modification -900)))
      (when (< (abs modification) (incf limit 0.0001))
  (when show
    (format t " ~d/~d ~a -> ~a  +/- ~a"
      poeng
      (length motstandere)
      motstandere
      (round (+ guess modification))
      limit))
  (return-from innsats (round (+ guess modification))))
      (incf guess modification))))

(defun innsats2 (motstander-poeng-k-list &key (show t))
  (let ((guess (first (first motstander-poeng-k-list)))
  (limit 0.001)
  (log10/400 (/ (log 10) 400))
  u1 u2 a b modification)
    (loop
      (setq a 0
      b 0)
      (dolist (m-p-k motstander-poeng-k-list)
  (setq u1 (expt 10 (/ (- (first m-p-k) guess) 400.0)))
  (setq u2 (/ (1+ u1)))
  (incf a (* (third m-p-k)
       (- (second m-p-k)
          u2)))
  (incf b (* (third m-p-k) 
       u1 u2 u2)))
      (setq modification (/ a b log10/400))
      (cond ((> modification 800)(setq modification 800))
      ((< modification -800)(setq modification -800)))
      (when (< (abs modification)(incf limit 0.0001))
  (when show
    (format t "guess ~a -> ~a +/- ~,4f" 
      motstander-poeng-k-list 
      (round (+ guess modification))
      limit))
  (return-from innsats2 (max (if *old-rule* 400 1)
           (round (+ guess modification)))))
      (incf guess modification))))

(defun innsats3 (motstander-poeng-k-list &key (show t))
  (let ((guess (first (first motstander-poeng-k-list)))
  (limit 0.001)
  (log10/400 (/ (log 10) 400))
  u1 u2 a b modification)
    (loop
      (setq modification 0)
      (format t "~%")
      (dolist (m-p-k motstander-poeng-k-list)
  (setq a 0
        b 0)
  (setq u1 (expt 10 (/ (- (first m-p-k) (+ guess modification))
           400.0)))
  (format t "<~a ~a ~a>" m-p-k (+ guess modification) u1)
  (setq u2 (/ (1+ u1)))
  (incf a (* (third m-p-k)
       (- (second m-p-k)
          u2)))
  (incf b (* (third m-p-k) 
       u1 u2 u2))
  (incf modification (/ a b log10/400))
  (cond ((> modification 800)(setq modification 800))
        ((< modification -800)(setq modification -800))))
      (when (< (abs modification)(incf limit 0.0001))
  (when show
    (format t "~a -> ~a +/- ~,4f" 
      motstander-poeng-k-list 
      (round (+ guess modification))
      limit))
  (return-from innsats3 (max (if *old-rule* 400 1)
           (round (+ guess modification)))))
      (incf guess modification))))
      
(defun remove-nth (n list)
  (remove-if (lambda (x) t) list :start n :count 1))

(defun show-players ()
  (dolist (x *all*)
    (simplified-player (spiller-old-spiller x))))

(defun simplified-player (player)
  (format t "~%~a ~a ~a ~a ~a ~a ~a ~a ~a"
    (player-last-name player)
    (player-first-name player)
    (player-grade-level player)
    (player-grade-name player)
    (player-elo-number player)
    (player-games player)
    (player-last-played player)
    (player-lb-count player)
    (player-mp-count player)))


(defparameter *ignore-u/p-k* nil)

(defun +/- ()
  (all-players)
  (inc-games))

(defun all-players ()
  (let ((sumdiff most-positive-fixnum)
        motstander-poenger-k
  spiller-change pre-ant post-ant runder
  dummy-elo beste bonus-count)
    (dolist (spiller *all*)
       (let ((pre-ant (pre-ant spiller)))
    (cond ((zerop pre-ant)
     (when  (plusp (player-nsr-grade-level 
        (spiller-old-spiller spiller)))
        (setq dummy-elo 
        (make-dummy 
         (grade-elo (player-nsr-grade-level 
               (spiller-old-spiller spiller))
              (player-nsr-grade-name 
               (spiller-old-spiller spiller)))))
        (push (list dummy-elo 1 nil nil)
        (spiller-clean-motstander-results spiller))
        (push (list dummy-elo 0 nil nil)
        (spiller-clean-motstander-results spiller)))
     (setf (post-ant spiller)(length (spiller-clean-motstander-results spiller)))
     (setf (spiller-u-player spiller) t)
     (setf (player-elo-number (spiller-old-spiller spiller)) 
           0))
    ((numberp (player-games (spiller-old-spiller spiller)))
     (setf (post-ant spiller)
           (+ pre-ant (length (spiller-clean-motstander-results spiller)))))
    ((or (every (lambda (x)(= x 0)) 
          (mapcar #'second 
            (player-games (spiller-old-spiller 
               spiller))))
         (every (lambda (x)(= x 1)) 
          (mapcar #'second 
            (player-games (spiller-old-spiller 
               spiller))))
         (zerop (player-elo-number 
           (spiller-old-spiller spiller)))
         (< (+ pre-ant 
         (length (spiller-clean-motstander-results 
            spiller))) 
      +u-limit+))
     (dolist (old-result (player-games 
              (spiller-old-spiller spiller)))
        (push (list (make-dummy (first old-result)
              (third old-result))
        (second old-result)
        nil
        nil)
        (spiller-clean-motstander-results spiller)))
     (setf (post-ant spiller)
           (length (spiller-clean-motstander-results 
        spiller)))
     (setf (spiller-u-player spiller) t)
     (setf (player-elo-number (spiller-old-spiller spiller)) 
           0))
    (t (setf (post-ant spiller)
       (+ pre-ant 
          (length (spiller-clean-motstander-results 
             spiller))))))))
    (loop
      (when (zerop sumdiff) (return))
      (format t "~%~d -> Iterate" sumdiff)
      (setq sumdiff 0)
      (setq *acc-weak-bonus* 0
      *acc-uppset-gain* 0)
      (dolist (spiller *all*)
  (setq motstander-poenger-k
    (get-elo-results spiller))
  (format t "oppoents point = ~a" motstander-poenger-k)
  (format t "~%~a ~a (~d)" 
    (spiller-last-name spiller)
    (spiller-first-name spiller)
    (player-elo-number (spiller-old-spiller spiller)))  
  (setq spiller-change 0)
  (setq pre-ant (pre-ant spiller))
  (cond ((not (spiller-u-player spiller))
         ;; P+E
         (setq bonus-count (player-bonus-count (spiller-old-spiller spiller)))
         (dolist (motstander-poeng-k motstander-poenger-k)
     (when *normal-output*
       (format t "~6,2@f" 
         (diff (+ (player-elo-number (spiller-old-spiller 
              spiller))
            spiller-change)
         (first motstander-poeng-k)
         (second motstander-poeng-k)
         (third motstander-poeng-k)
         bonus-count
         nil)))
     (incf spiller-change 
           (diff (+ (player-elo-number (spiller-old-spiller 
                spiller))
        spiller-change)
           (first motstander-poeng-k)
           (second motstander-poeng-k)
           (third motstander-poeng-k)
           bonus-count
           t))
     (incf bonus-count))
         (setq spiller-change (round spiller-change))
         (format t "= ~@d" spiller-change)
         (setq runder (length (spiller-clean-motstander-results spiller)))
         (format t " -> ~d" 
           (+ spiller-change 
        (player-elo-number (spiller-old-spiller 
                spiller)))))
        ((every (lambda (x)(= (second x) 0))
          (spiller-clean-motstander-results spiller))
         (format t " 0% -> 1")
         (setq spiller-change (if *old-rule* 400 1)))
        ((every (lambda (x)(= (second x) 1))
          (spiller-clean-motstander-results spiller))
         (setq beste (copy-list (beste motstander-poenger-k 
               :key #'first)))
         (setf (second beste) 1/2)
         (setq spiller-change 
         (innsats2 (cons beste 
             motstander-poenger-k))))
        (t 
         (setq spiller-change
         (innsats2 motstander-poenger-k))

         (format t "player_change= ~d" spiller-change) 
         ))
  (incf sumdiff (kvad (- (spiller-rating-change spiller) 
             spiller-change)))
  (setf (spiller-rating-change spiller) spiller-change)))))


(defvar *m-handicap* 15)
(defvar *l-handicap* 100)
(defvar *b-handicap* 200)
(defvar *r-handicap* 250)
(defvar *rl-handicap* 350)
(defvar *2p-handicap* 450)
(defvar *4p-handicap* 550)
(defvar *5p-handicap* 750)
(defvar *6p-handicap* 900)    ; 1000 in 2000
(defvar *s-handicap* 150)

#|
(defun get-elo-result (result-list)
  (let* ((modification 
    (cond ((null (fourth result-list))                 0)
    ((string-equal "M" (fourth result-list)) *m-handicap*)
    ((string-equal "L" (fourth result-list)) *l-handicap*)
    ((string-equal "B" (fourth result-list)) *b-handicap*)
    ((string-equal "R" (fourth result-list)) *r-handicap*)
    ((string-equal "RL" (fourth result-list)) *rl-handicap*)
    ((string-equal "2p" (fourth result-list)) *2p-handicap*)
    ((string-equal "4p" (fourth result-list)) *4p-handicap*)
    ((string-equal "5p" (fourth result-list)) *5p-handicap*)
    ((string-equal "6p" (fourth result-list)) *6p-handicap*)
    ;; ((string-equal "S" (fourth result-list))  *s-handicap*)
    (t (error "Unknown handicap : ~s" 
        (fourth result-list)))))
   (final-modification (case (third result-list)
             (#\+ (- modification))
             (#\- (+ modification))
             (t 0))))
    (list (+ (player-elo-number (spiller-old-spiller (first result-list)))
       (spiller-rating-change (first result-list))
       final-modification)
    (second result-list)
    (spiller-u/p-k (first result-list)))))

 until 2004-12-31
(defparameter *m-handicap-r*  0.15)
(defparameter *l-handicap-r*  0.50)
(defparameter *b-handicap-r*  1.50)
(defparameter *r-handicap-r*  2.00)
(defparameter *rl-handicap-r* 2.50)
(defparameter *2p-handicap-r* 3.50)
(defparameter *4p-handicap-r* 4.50)
(defparameter *5p-handicap-r* 6.00)
(defparameter *6p-handicap-r* 7.00)
(defparameter *s-handicap-r* 1)
|#

(defparameter *m-handicap-r*  0.20)
(defparameter *l-handicap-r*  0.60)
(defparameter *b-handicap-r*  1.50)
(defparameter *r-handicap-r*  2.10)
(defparameter *rl-handicap-r* 2.70)
(defparameter *2p-handicap-r* 3.60)
(defparameter *4p-handicap-r* 5.00)
(defparameter *5p-handicap-r* 6.50)
(defparameter *6p-handicap-r* 8.00)
(defparameter *s-handicap-r* 1)


(defun get-elo-results-old (spiller)
  (mapcar (lambda (result-list)
      (get-single-elo-result-old spiller result-list))
    (spiller-clean-motstander-results spiller)))

(defun get-elo-results-new (spiller)
  (mapcar (lambda (result-list)
      (get-single-elo-result-new spiller result-list))
    (spiller-clean-motstander-results spiller)))

(defparameter *old-rule* nil)

(defun get-elo-results (spiller)
  (if *old-rule*
      (get-elo-results-old spiller)
    (get-elo-results-new spiller)))

(defun get-start-rating (spiller)
  (let ((old (player-elo-number (spiller-old-spiller spiller))))
    (if (zerop old)
      (spiller-rating-change spiller)
      old)))

(defun clean-rating-change (spiller)
  (let ((start (player-elo-number (spiller-old-spiller spiller))))
    (if (zerop start) 0
      (spiller-rating-change spiller))))

(defun get-single-elo-result-old (spiller result-list)
  (let* ((my-rating (get-start-rating spiller))
   (opponent-rating (get-start-rating (first result-list)))
   (rating-change (clean-rating-change (first result-list)))
   (modification 
    (cond ((null (fourth result-list)) 0)
    ((string-equal "M" (fourth result-list)) *m-handicap-r*)
    ((string-equal "L" (fourth result-list)) *l-handicap-r*)
    ((string-equal "B" (fourth result-list)) *b-handicap-r*)
    ((string-equal "R" (fourth result-list)) *r-handicap-r*)
    ((string-equal "RL" (fourth result-list)) *rl-handicap-r*)
    ((string-equal "2p" (fourth result-list)) *2p-handicap-r*)
    ((string-equal "4p" (fourth result-list)) *4p-handicap-r*)
    ((string-equal "5p" (fourth result-list)) *5p-handicap-r*)
    ((string-equal "6p" (fourth result-list)) *6p-handicap-r*)
    ;;((string-equal "S" (fourth result-list))  *s-handicap-r*)
    (t (error "Unknown handicap : ~s" (fourth result-list)))))
   (effective-opponent-rating
    (case (third result-list)
      (#\+ (ranknr-elo (- (elo-ranknr opponent-rating) 
        modification)))
      (#\- (+ opponent-rating
        (- my-rating
           (ranknr-elo (- (elo-ranknr (+ my-rating (clean-rating-change spiller)))
              modification)))))
      (t opponent-rating))))
    (list (+ effective-opponent-rating 
       (clean-rating-change (first result-list)))
    (second result-list)
    (spiller-u/p-k (first result-list)))))

(defun get-single-elo-result-new (spiller result-list)
  (let* ((my-rating (get-start-rating spiller))
   (rating-change (clean-rating-change (first result-list)))
   (opponent-rating (max 400
             (+ (get-start-rating (first result-list))
          rating-change)))
   (modification 
    (cond ((null (fourth result-list)) 0)
    ;; ((string-equal "S" (fourth result-list))  *s-handicap-r*)
    ((string-equal "S" (fourth result-list))  *m-handicap-r*)
    ((string-equal "M" (fourth result-list)) *m-handicap-r*)
    ((string-equal "L" (fourth result-list)) *l-handicap-r*)
    ((string-equal "B" (fourth result-list)) *b-handicap-r*)
    ((string-equal "R" (fourth result-list)) *r-handicap-r*)
    ((string-equal "RL" (fourth result-list)) *rl-handicap-r*)
    ((string-equal "2p" (fourth result-list)) *2p-handicap-r*)
    ((string-equal "4p" (fourth result-list)) *4p-handicap-r*)
    ((string-equal "5p" (fourth result-list)) *5p-handicap-r*)
    ((string-equal "6p" (fourth result-list)) *6p-handicap-r*)
    (t (error "Unknown handicap : ~s" (fourth result-list)))))
   (effective-opponent-rating
    (case (third result-list)
      (#\+ (ranknr-elo (- (elo-ranknr opponent-rating) 
        modification)))
      (#\- (+ opponent-rating
        (- my-rating
           (ranknr-elo (- (elo-ranknr my-rating) 
              modification)))))
      (t opponent-rating))))
    (format t "~% myrating= ~d, rating_change= ~d opponent_rating = ~d opponent_start_rating = ~d" my-rating rating-change opponent-rating (get-start-rating (first result-list)))
    (list effective-opponent-rating
    (second result-list)
    1)))
;; (spiller-u/p-k (first result-list)))))

(defun inc-games ()
  (dolist (spiller *all*)
    (setf (player-games (spiller-old-spiller spiller))
      (if (spiller-u-player spiller)
    (get-elo-results spiller)
  (post-ant spiller)))
    (setf (player-bonus-count (spiller-old-spiller spiller))
      (if (spiller-u-player spiller)
    0
    (min 100
         (+ (player-bonus-count (spiller-old-spiller spiller))
      (length (spiller-clean-motstander-results spiller))))))
    (setf (player-last-played (spiller-old-spiller spiller)) *date*)
    (let ((home-ob (find *origin* (player-nationality-list 
           (spiller-old-spiller spiller)) 
       :key #'home-list)))
      (unless home-ob
  (setq home-ob (copy-home (first (player-nationality-list 
           (spiller-old-spiller spiller)))))
  (setf (home-list home-ob) *origin*)
  (push home-ob (player-nationality-list (spiller-old-spiller spiller))))
      (setf (home-last home-ob) *date*))
    (when *counting*  
      (register (+ (player-elo-number (spiller-old-spiller spiller))
       (spiller-rating-change spiller))
    (length (remove-if (lambda (x) (eq x *dummy*)) 
           (spiller-opponent-rounds spiller)))
    *origin*
    (and (not (spiller-u-player spiller))
         (>= (post-ant spiller) +e-limit+))))))
     

(defvar *special-promotions* nil)
(defvar *delayed-promotions* nil)

(defun special-promotions ()
  (dolist (promotion *special-promotions*)
    (let ((player (find-player (first promotion)(second promotion))))
      (setf (player-nsr-grade-level player)(third promotion)
      (player-nsr-grade-name player)(fourth promotion))))
  (setq *special-promotions* nil))

(defun special-promotions-old ()
  (dolist (promotion *special-promotions*)
    (let ((player (find-player (first promotion)(second promotion))))
      (setf (player-grade-level player)(third promotion)
      (player-grade-name player)(fourth promotion))))
  (setq *special-promotions* nil))

(defun delayed-promotions ()
  (dolist (promotion *delayed-promotions*)
    (let ((player (find-player (first promotion)(second promotion))))
      (setf (player-nsr-grade-level player)(third promotion)
      (player-nsr-grade-name player)(fourth promotion))))
  (setq *delayed-promotions* nil))

(defun special-promotion (last-name first-name value type)
  (push (list last-name first-name value type)
  *special-promotions*))

(defun delay-promotion (last-name first-name value type)
  (push (list last-name first-name value type)
  *delayed-promotions*))
  
(defun empty-simultan ()
  (dolist (spiller *all*)
    (when (spiller-simultan-p spiller)
      (setf (spiller-opponent-rounds spiller) nil
      (spiller-result-rounds spiller) nil
      (spiller-handicap-side spiller) nil
      (spiller-handicap-type spiller) nil))))

(defun elo-k (elo)
  (* *importance*
     (cond ((>= elo (lb 5 "Dan")) 16)
     ((>= elo (lb 3 "Dan")) 20)
     ((>= elo (lb 1 "Kyu")) 24)
     ((>= elo (lb 4 "Kyu")) 28)
     ((>= elo (lb 7 "Kyu")) 32)
     ((>= elo (lb 11 "Kyu")) 36)
     (t 40))))

(defun diff-old (my opponent result &optional (modification 1))
  (let ((k (* modification (elo-k my))))
    (round (* k (- result (/ (1+ (expt 10 (/ (- opponent my) 
               400.0d0)))))))))

(defun diff (my opponent result &optional (modification 1) (bonus-count 200) (for-update nil))
  (let* ((k (* modification (elo-k my)))
   (normal-gain (* k (- result (/ (1+ (expt 10 (/ (- opponent my) 
              400.0d0)))))))
   (upset-gain (/ (* k (- opponent my)) 
      160))
   (weak-bonus (if (and (< bonus-count 100)
            (< my 1800))
       (/ (- 1800 my) 200)
             0)))
    ;;(format t "~%Normal: ~a  upset: ~a  pkb: ~a" normal-gain upset-gain weak-bonus)
    (when (and (> upset-gain normal-gain) 
         (> opponent my) 
         (= 1 result))
      (when for-update
  (incf *acc-uppset-gain* (- upset-gain normal-gain)))
      (when *normal-output*
  (format t "*")))
    (when for-update (incf *acc-weak-bonus* weak-bonus))
    (+ (if (and (= 1 result)
    (> opponent my)
    (> upset-gain normal-gain))
     upset-gain
     normal-gain)
       weak-bonus)))

(defun kvad (x)(* x x))

;;                            1    2    3    4    5    6    7    8   9  10  11  12  13  14  15  16  17  18  19 20

(defparameter *dan-mp* '(0 1740 1860 2000 2160 2340 2540 2760 3000 9999 9999))
(defparameter *dan-lb* '(0 1680 1800 1920 2080 2240 2440 2640 2880 3120 9999))

(defparameter *kyu-mp* '(0 1620 1510 1410 1320 1240 1160 1080 1000 920 840 760 680 600 520 440 360 280 200 120 40))
(defparameter *kyu-lb* '(0 1560 1460 1360 1280 1200 1120 1040  960 880 800 720 640 560 480 400 320 240 160  80  0))

(defparameter *dan-lb-#* '(0 14   14   16   16 9999 9999 9999))
(defparameter *dan-mp-#* '(0  7    7    8    8   10 9999 9999))

(defparameter *kyu-lb-#* '(0 14   12   12   12   10   10   10    8   8   8   8   6   6   6   6   6   6   6   6   6))
(defparameter *kyu-mp-#* '(0  7    6    6    6    5    5    5    4   4   4   4   3   3   3   3   3   3   3   3   3))

(defun grade-elo (value type)
   (cond ((string-equal type "Dan")
    (nth value *dan-mp*))
   ((string-equal type "Kyu")
    (nth value *kyu-mp*))
   ((string-equal type "Pro") 
    (+ (- 2540 30) (* 30 value)))
   (t (error "Unknown rating type : ~a" type))))

(defun lb (value type)
  (cond ((string-equal type "Pro") 9999)
  ((string-equal type "Dan")(nth value *dan-lb*))
  ((string-equal type "Kyu")(nth value *kyu-lb*))
  (t (error "Unknown grade ~a ~a" value type))))

(defun mp (value type)
  (cond ((string-equal type "Pro") 9999)
  ((string-equal type "Dan")(nth value *dan-mp*))
  ((string-equal type "Kyu")(nth value *kyu-mp*))
  (t (error "Unknown grade ~a ~a" value type))))

(defun ub (value type)
  (let ((next-grade (next-grade value type)))
    (lb (first next-grade)(second next-grade))))

(defun next-lb (value type)
  (ub value type))

(defun next-mp (value type)
  (let ((next-grade (next-grade value type)))
    (mp (first next-grade)(second next-grade))))

(defun next-ub (value type)
  (let ((next-grade (next-grade value type)))
    (ub (first next-grade)(second next-grade))))

(defun next-grade (value type)
  (cond ((string-equal type "Pro")(list 0 "Pro"))
  ((string-equal type "Dan")(list (1+ value) type))
  ((string-equal type "")(list 20 "Kyu"))
  ((string-not-equal type "Kyu")(error "No next-grad for ~s" type))
  ((= 1 value)(list 1 "Dan"))
  (t (list (1- value) type))))

(defun limit-mp (value type)
  (cond ((string-equal type "Pro") 9999)
  ((string-equal type "Dan")(nth value *dan-mp-#*))
  ((string-equal type "Kyu")(nth value *kyu-mp-#*))
  (t (error "Unknown grade ~a ~a" value type))))

(defun limit-lb (value type)
  (cond ((string-equal type "Pro") 9999)
  ((string-equal type "Dan")(nth value *dan-lb-#*))
  ((string-equal type "Kyu")(nth value *kyu-lb-#*))
  (t (error "Unknown grade ~a ~a" value type))))

(defvar *promotions* nil)

(defun find-promotions ()
  (setq *promotions* nil)
  (dolist (spiller *all*)
    (unless (spiller-simultan-p spiller)
      (let* ((spiller-stat (spiller-old-spiller spiller))
       (from (player-elo-number spiller-stat))
       (to (+ from (spiller-rating-change spiller)))
       (from (if (= 0 from) to from))
       (games (length (spiller-clean-motstander-results spiller)))
       (tot-games (pre-ant spiller)) ;pre-ant til neste turnering
       (inc (ignore-errors (/ (- to from) games))))
  ;; (format t "~%~a ~a ~a ~a" from to games spiller-stat)
  (do ((round 1 (1+ round))
       (s-tot (1+ (- tot-games games)) (1+ s-tot))
       (rating (ignore-errors (+ from inc))(+ rating inc)))
      ((> round games))
    (cond ((>= s-tot +e-limit+)
     (test-promotion rating spiller-stat round))
    ((>= s-tot +u-limit+)
     (test-p-promotion rating spiller-stat round))))
  (when (>= tot-games +e-limit+)
    (let ((cur-grade-value (player-grade-level spiller-stat))
    (cur-grade-type (player-grade-name spiller-stat))
    (old-spiller-stat (copy-tree spiller-stat)))
      (setq spiller-stat (copy-player spiller-stat))
      (setf (player-lb-count spiller-stat)
        (copy-list (player-lb-count spiller-stat)))
      (setf (player-mp-count spiller-stat)
         (copy-list (player-mp-count spiller-stat)))
      (do ((round (1+ games) (1+ round))
     (s-tot (1+ tot-games) (1+ s-tot))
     (rating (- to (elo-k to)) (- rating (elo-k rating))))
    ((> round (+ games 20)))
        (cond ((>= s-tot +e-limit+)
         (test-promotion rating spiller-stat round))
        ((>= s-tot +u-limit+)
         (error "Dette er umulig")
         (test-p-promotion rating spiller-stat round))))
      (unless (and (= cur-grade-value 
          (player-grade-level spiller-stat))
       (string= cur-grade-type 
          (player-grade-name spiller-stat)))
        (promote (spiller-old-spiller spiller)))))))))
      
(defun test-promotion (rating spiller-stat round)
  (let* ((grade-value (player-grade-level spiller-stat))
   (grade-type (player-grade-name spiller-stat))
   (next-lb (next-lb grade-value grade-type))
   (next-mp (next-mp grade-value grade-type))
   (next-ub (next-ub grade-value grade-type)))
    (cond ((>= rating next-ub)
     (format t "~%~a ~a : Upper bound reached in game ~a" 
       (player-last-name spiller-stat)
       (player-first-name spiller-stat) round)
     (promote spiller-stat)
     (test-promotion rating spiller-stat round))
    ((>= rating next-mp)
     (format t "~%~a ~a: Registering one game above MP in game ~a"
       (player-last-name spiller-stat)
       (player-first-name spiller-stat) round)
     (cond ((eq :mp (first (player-mp-count spiller-stat)))
      (incf (second (player-lb-count spiller-stat)))
      (incf (second (player-mp-count spiller-stat))))
     ((eq :lb (first (player-lb-count spiller-stat)))
      (incf (second (player-lb-count spiller-stat)))
      (setf (third (player-lb-count spiller-stat)) t)
      (setf (player-mp-count spiller-stat) (list :mp 1)))
     (t (setf (player-lb-count spiller-stat) (list :lb 1 t) 
        (player-mp-count spiller-stat) (list :mp 1))))
     (above-limit? spiller-stat rating round 
       grade-value grade-type))
    ((>= rating next-lb)
     (format t "~%~a ~a: Registering one game above LB in game ~a"
       (player-last-name spiller-stat)
       (player-first-name spiller-stat) round)
     (cond ((eq :lb (first (player-lb-count spiller-stat)))
      (setf (player-mp-count spiller-stat) nil)
      (incf (second (player-lb-count spiller-stat))))
     (t (setf (player-lb-count spiller-stat) (list :lb 1 nil)
        (player-mp-count spiller-stat) nil)))
     (above-limit? spiller-stat rating round 
       grade-value grade-type))
    (t (setf (player-lb-count spiller-stat) nil
       (player-mp-count spiller-stat) nil)))))

(defun test-p-promotion (rating spiller-stat round)
   (let* ((old-grade-value (player-grade-level spiller-stat))
    (old-grade-type (player-grade-name spiller-stat))
    (next-grade (next-grade old-grade-value old-grade-type))
    (grade-value (first next-grade))
    (grade-type (second next-grade))
    (next-ub (next-ub grade-value grade-type)))
      (when (>= rating next-ub)
   (format t "~%~a ~a : Upper bound+1 reached in game ~a" 
     (player-last-name spiller-stat)
     (player-first-name spiller-stat) 
     round)
   (promote spiller-stat)
   (test-p-promotion rating spiller-stat round))))


(defun above-limit? (spiller-stat rating round grade-value grade-type)
   (let ((next-grade (next-grade grade-value grade-type)))
      (when (and (player-lb-count spiller-stat)
     (or (and (>= (second (player-lb-count spiller-stat))
            (limit-lb (first next-grade)(second next-grade)))
        (third (player-lb-count spiller-stat)))
         (and (player-mp-count spiller-stat)
        (>= (second (player-mp-count spiller-stat))
            (limit-mp (first next-grade)(second next-grade))))))
   (promote spiller-stat)
   (test-promotion rating spiller-stat round))))
        
(defun promote (spiller-stat)
  (let* ((next-grade (next-grade (player-grade-level spiller-stat)
         (player-grade-name spiller-stat))))
    (format t "~%Promoting ~a ~a to ~d ~a" 
      (player-last-name spiller-stat)
      (player-first-name spiller-stat)
      (first next-grade)
      (second next-grade))
    (setf (player-grade-level spiller-stat) (first next-grade)
    (player-grade-name spiller-stat) (second next-grade)
    (player-lb-count spiller-stat) nil
    (player-mp-count spiller-stat) nil)
    (when (and *promotions*
         (search (format nil "Promoting ~a ~a" (player-last-name spiller-stat) (player-first-name spiller-stat))
           (first *promotions*)))
      (pop *promotions*))
    (push (format nil "Promoting ~a ~a to ~d ~a" 
      (player-last-name spiller-stat)
      (player-first-name spiller-stat)
      (first next-grade)
      (second next-grade))
    *promotions*)))


(defun show-all (date)
  (show-list *rating* date))

(defun show-list (list date)
  (with-open-file (f "all.txt" :direction :output :if-exists :supersede :external-format 'charset:iso-8859-1)
    (format f "Elo list for ~a~2%" date)
    (format f "    Name                               Grades       Elo #games Nationality")
    (let ((teller 0)
    exists)
      (dolist (spiller list)
  (when (active-player (player-last-played spiller) date)
    (unless (or (string-equal (player-grade-name spiller) "Pro")
          (string-equal (player-nsr-grade-name spiller) "Pro"))
      (format f "~%~3d ~17a ~15a ~[  ~:;~:*~2d~] ~3a ~[  ~:;~:*~2d~] ~3a ~4d ~3d     ~a"
        (incf teller)
        (player-last-name spiller)
        (player-first-name spiller)
        (player-grade-level spiller)
        (player-grade-name spiller)
        (player-nsr-grade-level spiller)
        (player-nsr-grade-name spiller)
        (player-elo-number spiller)
        (game-number (player-games spiller))
        (simple-nationality spiller))))))))

(defun show-list-ec ()
  (with-open-file (f "ec.txt" :direction :output :if-exists :supersede :external-format 'charset:iso-8859-1)
    (let ((teller 0)
    (sorted-players (sort (copy-list *rating*) #'string< 
        :key (lambda (player)
               (format nil "~a ~a"
                 (player-last-name player)
                 (player-first-name player)))))
    exists)
      (dolist (spiller sorted-players)
  (format f "~%~a ~a ~[ ~:;~:*~d~] ~a ~[ ~:;~:*~d~] ~a ~d ~a ~a ~a"
    (player-last-name spiller)
    (player-first-name spiller)
    (player-grade-level spiller)
    (player-grade-name spiller)
    (player-nsr-grade-level spiller)
    (player-nsr-grade-name spiller)
    (player-elo-number spiller)
    (player-games spiller)
    (player-nationality-list spiller)
    (player-last-played spiller))))))


(defun simple-nationality (spiller)
  (let ((a-value (find #\A (player-nationality-list spiller) 
           :key #'home-list))
  (e-value (find #\E (player-nationality-list spiller) 
           :key #'home-list)))
    (cond (a-value (home-nationality a-value))
    (e-value (home-nationality e-value)))))

        
(defun active-p (d1 d2)
  (cond ((string= d1 "") t)
  (t (string>= d1 d2 :end2 (length d1)))))

(defun active-player (last-played today)
  (when (= 0 (length last-played)) (setq last-played "2000-01-01"))
  (when (= 7 (length last-played)) (setq last-played (format nil "~a-31" last-played)))
  (when (= 7 (length today)) (setq today (format nil "~a-31" today)))
  (unless (= 10 (length last-played)) (error "~a is not an ok date" last-played))
  (unless (= 10 (length today)) (error "~a is not an ok date" today))
  (let ((2-years-ago (format nil "~4d~a" 
           (- (parse-integer (subseq today 0 4)) 2) 
           (subseq today 4))))
    (string<= 2-years-ago last-played)))
    

(defun game-number (x)
  (if (listp x)(length x) x))



#|

     In one e-mail I sent a list of proposed values for pros.  It may
need adjustment since we made other changes.  For a simple rule, a Pro
1 Dan is considered equal to Amateur six dan (i.e. 2540), and each
step above that is another 30 points.  So Pro 2 Dan = 2570, 3 Dan
=2600, C2 or free class pro 2630, C1 2660, B2 2690, B1 2720, A class
or titleholder 2750, Meijin or multiple title holder 2780.  I propose
to add this to the document. Comments?

|#

(defvar *accumulated-handicap-results* (make-hash-table :test #'equalp))

(defun initiate-handicap-results ()
  (clrhash *accumulated-handicap-results*)
  (dolist (handicap-type '("M" "L" "B" "R" "RL" "2p" "4p" "5p" "6p"))
    (dolist (direction (list #\+ #\-))
      (setf (gethash (list direction handicap-type) *accumulated-handicap-results*) 
  (list 0 0))))
  (setf (gethash (list nil nil) *accumulated-handicap-results*) (list 0 0)))

(defun show-handicap-results ()
  (let ((result (gethash (list nil nil) *accumulated-handicap-results*)))
    (format t "~%*** ~4d / ~2d = ~5,1f"
      (first result) 
      (second result)
      (or (ignore-errors (/ (first result) (second result)))
    0)))
  (dolist (handicap-type '("M" "L" "B" "R" "RL" "2p" "4p" "5p" "6p"))
    (dolist (direction (list #\+ #\-))
      (let ((result (gethash (list direction handicap-type) *accumulated-handicap-results*)))
  (format t "~%~a~2a ~4d / ~2d = ~5,1f" 
    direction 
    handicap-type 
    (first result) 
    (second result)
    (or (ignore-errors (/ (first result) (second result)))
        0))))))


(defun update-handicap-scores ()
  (dolist (spiller *all*)
    (mapc (lambda (type basics)
      (when (or (and (char= (caddr type) #\+)
         (handicap-e-player (first type)))
          (and (char= (caddr type) #\-)
         (handicap-e-player spiller)))
        (let ((place (gethash (cddr type) 
            *accumulated-handicap-results*)))
    (unless place (error "Unknown handicap type : ~a" 
             (cdr type)))
    (incf (first place)
          (diff (- (player-elo-number (spiller-old-spiller 
               spiller)) 
             10000)
          (- (first basics) 10000)
          (second basics)))
    (incf (second place)))))
    (spiller-clean-motstander-results spiller)
    (get-elo-results spiller))))

(defun elo-ranknr (elo)
  (cond ((> elo (nth 1 *dan-lb*))
   :dan-grade
   (let ((main-nr (position elo (rest *dan-lb*) :test #'<)))
     (+ 14.0 main-nr (part-of elo
           (nth main-nr *dan-lb*)
           (nth (1+ main-nr) *dan-lb*)))))
  ((> elo (nth 1 *kyu-lb*))
   :1-kyu-grad
   (+ 14.0 (part-of elo 
      (nth 1 *kyu-lb*)
      (nth 1 *dan-lb*))))
  (t :kyu-grade
     (let ((main-nr (position elo (rest *kyu-lb*) :test #'>)))
       (if main-nr
     (+ (- 14.0 main-nr)
        (part-of elo 
           (nth (1+ main-nr) *kyu-lb*) 
           (nth main-nr *kyu-lb*)))
         0.0)))))

(defun part-of (x lb ub)
  (/ (- x lb)
     (- ub lb)))

(defun ranknr-elo (nr)
  (multiple-value-bind (main-nr part) (floor nr)
    (round 
     (cond ((>= main-nr 15)
      (value-of part 
          (nth (- main-nr 14) *dan-lb*)
          (nth (- main-nr 13) *dan-lb*)))
     ((= main-nr 14)
      (value-of part
          (nth 1 *kyu-lb*)
          (nth 1 *dan-lb*)))
     ((>= main-nr -5)
      (value-of part 
          (nth (- 15 main-nr) *kyu-lb*)
          (nth (- 14 main-nr) *kyu-lb*)))
     (t 1)))))

(defun value-of (part lb ub)
  (+ lb (* part (- ub lb))))

(defun show-handicap-effects (elo)
  (dolist (x `(("Lance" ,*l-handicap-r*)
         ("Bishop" ,*b-handicap-r*)
         ("Rook" ,*r-handicap-r*)
         ("Rook-Lance" ,*rl-handicap-r*)
         ("2piece" ,*2p-handicap-r*)
         ("4 piece" ,*4p-handicap-r*)
         ("5 piece" ,*5p-handicap-r*)
         ("6 piece" ,*6p-handicap-r*)))
    (format t "~%~12a ~4d" (first x) (ranknr-elo (- (elo-ranknr elo) (second x))))))

(defvar *counting* nil)
(defvar *counting-stat* nil)
(defvar *count-not-established* nil)

(defun start-registrer ()
  (setq *counting* t)
  (setq *counting-stat* nil))

(defun register (rating ant origin established)
  (when (or established *count-not-established*)
    (let ((place (assoc origin *counting-stat*)))
      (unless place (push (setq place (list origin 0 0 0)) *counting-stat*))
      (incf (second place) ant)
      (incf (third place) (* ant rating))
      (incf (fourth place) (* ant rating rating)))))

(defun show-register ()
  (dolist (results *counting-stat*)
    (format t "~%~a : Ant= ~d  Avg= ~d  Sd= ~d" 
      (first results)
      (second results)
      (round (/ (third results) (second results)))
      (round (sqrt (/ (- (fourth results) (/ (kvad (third results)) 
               (second results)))
          (1- (second results))))))))


(defun stop-register ()
  (setq *counting* nil))

(defun show-dates ()
  (with-open-file (f "rating-dates.txt" :direction :output :if-exists :supersede 
       :if-does-not-exist :create :external-format 'charset:iso-8859-1)
    (dolist (player *rating*)
      (format f "~a ~a ~a ~a ~a (~a ~a)~%" 
        (player-last-name player)
        (player-first-name player)
        (player-elo-number player)
        (game-number (player-games player))
        (player-last-played player)
        (ignore-errors (home-last (find #\E (player-nationality-list player) 
                :key #'home-list)))
        (ignore-errors (home-last (find #\A (player-nationality-list player) 
                :key #'home-list)))))))




(defun run-perform (level &key (clean nil))
  (setq *print-circle* t)
  (setq *handicap* nil)
  (les "turnering.txt")
  (calculate)
  (read-table)
  (find-players)
  (when clean
    (dolist (spiller *all*)
      (setf (player-elo-number (spiller-old-spiller spiller)) 1000)))
  (dolist (spiller *all*) 
    (setf (spiller-rating-change spiller)
      (list (player-elo-number (spiller-old-spiller spiller)))))
  (dotimes (x level) (find-performance))
  (show-performance))

(defun find-performance-old ()
  (dolist (spiller *all*)
    (let ((poeng 1/2)
    (opponents (list (player-elo-number 
          (spiller-old-spiller spiller)))))
      (dolist (round (spiller-clean-motstander-results spiller))
  (incf poeng (second round))
  (push (player-elo-number 
         (spiller-old-spiller (first round)))
        opponents))
      (push (setf (player-elo-number (spiller-old-spiller spiller))
        (innsats poeng opponents))
      (spiller-rating-change spiller)))))

(defun find-performance ()
  (let ((res (mapcar (lambda (spiller)
           (let ((poeng 1/2)
           (opponents (list (player-elo-number 
                 (spiller-old-spiller spiller)))))
       (dolist (round (spiller-clean-motstander-results 
           spiller))
         (incf poeng (second round))
         (push (player-elo-number 
          (spiller-old-spiller (first round)))
         opponents))
       (innsats poeng opponents)))
         *all*)))
    (mapc (lambda (spiller res)
      (push res (spiller-rating-change spiller))
      (setf (player-elo-number (spiller-old-spiller spiller)) res))
    *all* res)))

(defun show-performance-old ()
  (let ((last-name-size (reduce #'max 
        (mapcar (lambda (spiller)
            (length (spiller-last-name spiller)))
          *all*)))
  (first-name-size (reduce #'max 
         (mapcar (lambda (spiller)
             (length (spiller-first-name spiller)))
           *all*))))
    (dolist (spiller *all*)
      (format t "~%~va ~va ~4d" 
        last-name-size
        (spiller-last-name spiller)
        first-name-size       
        (spiller-first-name spiller)
        (player-elo-number (spiller-old-spiller spiller))))))

(defun show-performance ()
  (let ((last-name-size 
   (reduce #'max (mapcar (lambda (spiller)
         (length (spiller-last-name spiller)))
             *all*)))
  (first-name-size 
   (reduce #'max (mapcar (lambda (spiller)
         (length (spiller-first-name spiller)))
             *all*))))
    (format t "~2%Nr Name ~vt ELO" 
      (+ last-name-size first-name-size 5))
    (dotimes (r (length (spiller-opponent-rounds (second *all*))))
      (format t " ~2d " (1+ r))
      (when *handicap* (format t "     ")))
    (format t " Pts  Perform")
    (dolist (spiller *all*)
      (format t "~%~2d ~va ~va ~4d ~{~{~2d~c~:[~2*~;~:[~*     ~;(~:*~c~2a)~]~]~} ~} ~2d  ~{~4d ~}"
        (spiller-nr spiller)
        last-name-size
        (spiller-last-name spiller)
        first-name-size       
        (spiller-first-name spiller)
        (car (last (spiller-rating-change spiller)))
        (mapcar (lambda (motstander result h-side h-type)
      (list (spiller-nr motstander) 
            (ecase result 
        (1 #\+)
        (0 #\-)
        (1/2 #\=))
            *handicap*
            h-side
            h-type))
          (spiller-opponent-rounds spiller)
          (spiller-result-rounds spiller)
          (spiller-handicap-side spiller)
          (spiller-handicap-type spiller))
        (spiller-pts spiller)
        (rest (reverse (spiller-rating-change spiller)))))))


(defun carl-johan ()
  (let ((last-name-size (reduce #'max (mapcar (lambda (spiller)
            (length (spiller-last-name spiller)))
                *all*)))
  (first-name-size (reduce #'max (mapcar (lambda (spiller)
             (length (spiller-first-name spiller)))
                 *all*)))
  n rating score handicap end-rating)
    (with-open-file (f "carl-johan" :direction :output :if-exists :supersede :external-format 'charset:iso-8859-1)
      (format f "~%~a (~a)~%" *name* *date*)
      (dolist (spiller *all*)
  (setq n 0
        rating 0
        score 0
        handicap nil)
  (dolist (runde (spiller-clean-motstander-results spiller))
    (cond ((fourth runde) (setq handicap t))
    ((>= 1 (setq end-rating (player-elo-number (spiller-old-spiller (first runde))))))
    (t (incf n)
       (incf rating end-rating)
       (incf score (second runde)))))
  (format f "~:[ =~;~:*~2d~] ~va ~va  " 
    (spiller-nr spiller) 
    last-name-size
    (spiller-last-name spiller)
    first-name-size       
    (spiller-first-name spiller))
  (cond (handicap (format f "*HANDICAP*~%"))
        ((zerop n) (format f "*NO GAMES*~%"))
        (t (format f "~4d  ~a / ~a  -> ~4d~%"
       (round rating n)
       score
       n
       (round (+ (/ rating n)
           (* 850 (- (/ score n) 1/2)))))))))))

(defun vektet-bonus (&rest +/-s)
  (let* ((abs-liste (mapcar #'abs +/-s))
   (sum (reduce #'+ +/-s))
   (ant (length +/-s))
   (base-bonus (- sum 20 (* 3 ant)))
   (new-ant (/ sum
         (/ (reduce #'+ (mapcar #'* +/-s abs-liste))
      (reduce #'+ abs-liste))))
   (new-base (- sum 20 (* 3 new-ant))))
    (format t "n= ~a~%" new-ant)
    (list (if (>= base-bonus 0)
        (* base-bonus (1- (/ 25 ant)))
        0)
    (if (>= new-base 0)
        (* new-base (1- (/ 25 new-ant)))
      0))))


(defun run-to-file ()
  (with-open-file (*standard-output* "run.log" :direction :output 
             :external-format 'charset:iso-8859-1)
      (run)))


(defun games-played (player)
  (let ((games (player-games player)))
    (cond ((listp games) (length games))
    (t games))))

(defun us-analysis ()
  (read-table "2010-wiel/t1/ratingliste.pre")
  (let ((start-rating *rating*)
  (akkum-diff 0)
  (akkum-games 0)
  end-rating nationality start-player games diff a-part e-part a-last e-last)
    (read-table "2012/t66/ratingliste.post")
    (setq end-rating *rating*)
    (dolist (player end-rating)
      (setq nationality (player-nationality-list player))
      (setq *oluf-player* player)
      (when (cond ((= 1 (length nationality))
       (char= (home-list (first nationality)) #\A))
      (t (setq a-part (find #\A nationality :key #'home-list :test #'char=))
         (setq e-part (find #\E nationality :key #'home-list :test #'char=))
         (setq a-last (home-last a-part))
         (setq e-last (or (home-last e-part)
              "2000-01-01"))
         (string<= e-last a-last)))
  (setq start-player (find-if (lambda (spiller)
              (and (string= (player-last-name spiller) 
                (player-last-name player))
             (string= (player-first-name spiller) 
                (player-first-name player))))
            start-rating))
  (cond ((null start-player)
         (format t "~%~20a ~20a NEW  ~4d      ~3d                    "
           (player-last-name player)
           (player-first-name player)
           (player-elo-number player)
           (games-played player)
           ;;nationality
           ))
        ((= (games-played start-player) (games-played player)))
        (t (when (> (length nationality) 1) (push player *oluf-duplicates*))
     (setq games (- (games-played player) (games-played start-player)))
     (format t "~%~20a ~20a ~4d ~4d ~4d ~3d ~5,1f   ~4d ~4d ~5,1f" 
       (player-last-name player)
       (player-first-name player)
       (player-elo-number start-player)
       (player-elo-number player)
       (setq diff (- (player-elo-number player) (player-elo-number start-player)))
       games
       (/ diff games)
       (incf akkum-diff diff)
       (incf akkum-games games)
       (/ akkum-diff akkum-games)
       ;; nationality
       )))))))

(defun wiel-analysis ()
  (setq *oluf-changers* nil)
  ;; (read-table "wiel-start.db")
  (read-table "wiel-eu.db")
  (let ((start-rating *rating*)
  (akkum-diff 0)
  (akkum-games 0)
  end-rating nationality end-player games diff a-part e-part a-last e-last)
    (read-table "wiel-end.db")
    (setq end-rating *rating*)
    (dolist (start-player start-rating)
      (setq *oluf-player* start-player)
      (setq end-player (find-if (lambda (spiller)
          (and (string= (player-last-name spiller) 
            (player-last-name start-player))
               (string= (player-first-name spiller) 
            (player-first-name start-player))))
        end-rating))
      (cond ((null end-player)
       (error "What is the new name of ~a ~a?" 
        (player-last-name start-player) (player-first-name start-player)))
      ((= (games-played start-player) (games-played end-player)))
      (t (setq games (- (games-played end-player) (games-played start-player)))
         (push start-player *oluf-changers*)
         (format t "~%~20a ~20a ~4d ~4d ~4d ~3d ~5,1f   ~4d ~4d ~5,1f" 
           (player-last-name start-player)
           (player-first-name start-player)
           (player-elo-number start-player)
           (player-elo-number end-player)
           (setq diff (- (player-elo-number end-player) (player-elo-number start-player)))
           games
           (/ diff games)
           (incf akkum-diff diff)
           (incf akkum-games games)
           (/ akkum-diff akkum-games)
           ))))))

(defun count-nationality ()
  (let ((result (make-hash-table :test #'equal))
  (num (make-hash-table :test #'equal))
  nat)
    (dolist (spiller *all*)
      (unless (spiller-u-player spiller)
  (setq nat (home-nationality (first (player-nationality-list 
              (spiller-old-spiller spiller)))))
  (incf (gethash nat result 0)
        (spiller-rating-change spiller))
  (incf (gethash nat num 0))))
    (maphash (lambda (nat diff)
         (format t "~%~3a ~4d / ~2d = ~3d" nat diff (gethash nat num) (round diff (gethash nat num))))
       result)))
