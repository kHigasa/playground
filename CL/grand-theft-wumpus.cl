(defun dot-name (exp)
  (substitute-if #\_ (complement #'alphanumericp) (prin1-to-string exp)))

(defparameter *max-label-length* 30)

(defun dot-label (exp)
  (if exp
	(let ((s (write-to-string exp :pretty nil)))
	  (if (> (length s) *max-label-length*)
		(concatenate 'string (subseq s 0 (- *max-label-length* 3)) "...")
		s))
	""))

(defun nodes->dot (nodes)
  (mapc (lambda (node)
		  (fresh-line)
		  (princ (dot-name (car node)))
		  (princ "[label=\"")
		  (princ (dot-label node))
		  (princ "\"];"))
		nodes))

(defun edges->dot (edges)
  (mapc (lambda (node)
		  (mapc (lambda (edge)
				  (fresh-line)
				  (princ (dot-name (car node)))
				  (princ "->")
				  (princ (dot-name (car edge)))
				  (princ "[label=\"")
				  (princ (dot-label (cdr edge)))
				  (princ "\"];"))
				(cdr node)))
		edges))

(defun uedges->dot (edges)
  (maplist (lambda (lst)
			 (mapc (lambda (edge)
					 (unless (assoc (car edge) (cdr lst))
					   (fresh-line)
					   (princ (dot-name (caar lst)))
					   (princ "--")
					   (princ (dot-name (car edge)))
					   (princ "[label=\"")
					   (princ (dot-label (cdr edge)))
					   (princ "\"];")))
				   (cdar lst)))
		   edges))

(defun ugraph->dot (nodes edges)
  (princ "graph{")
  (nodes->dot nodes)
  (uedges->dot edges)
  (princ "}"))

(defun dot->png (fname thunk)
  (with-open-file (*standard-output*
				   fname
				   :direction :output
				   :if-exists :supersede)
	(funcall thunk))
  (ext:shell (concatenate 'string "dot -Tpng -O " fname)))

(defun ugraph->png (fname nodes edges)
  (dot->png fname
			(lambda ()
			  (ugraph->dot nodes edges))))

(defparameter *congestion-city-nodes* nil)
(defparameter *congestion-city-edges* nil)
(defparameter *visited-nodes* nil)
(defparameter *node-num* 30)
(defparameter *edge-num* 45)
(defparameter *worm-num* 3)
(defparameter *cop-odds* 15)

(defun random-node ()
  (1+ (random *node-num*)))

(defun edge-pair (a b)
  (unless (eql a b)
	(list (cons a b) (cons b a))))

(defun make-edge-list ()
  (apply #'append (loop repeat *edge-num*
						collect (edge-pair (random-node) (random-node)))))

(defun direct-edges (node edge-list)
  (remove-if-not (lambda (x)
				   (eql (car x) node))
				 edge-list))

(defun get-connected (node edge-list)
  (let ((visited nil))
	(labels ((traverse (node)
			  (unless (member node visited)
				(push node visited)
				(mapc (lambda (edge)
						(traverse (cdr edge)))
					  (direct-edges node edge-list)))))
	(traverse node))
  visited))

(defun find-islands (nodes)
  (let ((islands nil))
	(labels ((find-island (nodes)
			   (let* ((connected (get-connected (car nodes) edge-list))
					  (unconnected (set-difference nodes connected)))
			   (push connected islands)
			   (when unconnected
				 (find-island unconnected)))))
	(find-island nodes))
  islands))

(defun connect-with-bridges (islands)
  (when (cdr islands)
	(append (edge-pair (caar islands) (caadr islands))
			(connect-with-bridges (cdr islands)))))

(defun connect-all-islands (nodes edge-list)
  (append (connect-with-bridges (find-islands nodes edge-list)) edge-list))

