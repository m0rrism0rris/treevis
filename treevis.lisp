;gnu clisp  2.49.60

(defconstant +box-light+ " ╴╷┐╶─┌┬╵┘│┤└┴├┼")

(defmethod extract-dat ((object list)) object
  (loop for i in object collect
        (if (or (listp i) (vectorp i)) (extract-dat i) i)))

(defmethod extract-dat ((object array)
                        &aux (w (array-dimension object 1)))
  (loop for i below (array-dimension object 0) collect
        (loop for j below w collect (aref object i j))))

(defmethod extract-dat ((object vector) &aux (q (coerce object 'list)))
  (if (or (vectorp (car q)) (listp (car q))) (extract-dat q) q))

(defstruct (treechar
            (:conc-name tc-)
            (:constructor make-treechar
                (seq &optional c
                 &aux
                   (dat (extract-dat seq))
                   (w (length (car dat)))
                   (h (length dat)))))
  (arr (make-array (list h w) :element-type 'character
                              :initial-contents dat)
   :type (simple-array character) :read-only t)
  (width w :type integer :read-only t)
  (height h :type integer :read-only t)
  (cent (if c (- (min c w) 1) (- (ceiling w 2) 1)) :type integer :read-only t))

(defmethod extract-dat ((object treechar))
  (extract-dat (tc-arr object)))

(defun print-treechar (inp &optional (stream t)
                       &aux (arr (tc-arr inp))
                         (h (tc-height inp)) (w (tc-width inp)))
  (declare (treechar inp))
  (format stream "~%~{~{~a~}~%~}"
          (loop for i below h collect
                (loop for ii below w collect (aref arr i ii)))))

(defun merge-horiz
    (c &rest tcs &aux (h (loop for tc in tcs maximize (tc-height tc)))
                      (w (loop for tc in tcs sum (tc-width tc)))
                      (b2 (make-array w :element-type '(unsigned-byte 4)))
                      o oo b)
  (setq o (loop for tc in tcs collect
                (nconc (loop repeat (- h (tc-height tc)) collect
                             (make-list (tc-width tc)
                                        :initial-element #\ ))
                       (extract-dat (tc-arr tc)))))
  (loop repeat h do
        (push (loop for i in o collect (cdr i) into z nconc (car i)
                    finally (setq o z))
              oo))
  (setq b (make-array w :element-type '(unsigned-byte 4)
                        :fill-pointer 0))
  (loop repeat (tc-cent (car tcs)) do (vector-push 0 b))
  (vector-push 12 b)
  (loop repeat (- (tc-width (car tcs)) (tc-cent (car tcs)) 1) do
        (vector-push 5 b))
  (setq tcs (cdr tcs))
  (loop with tc do
        (setq tc (pop tcs))
        (loop repeat (tc-cent tc) do (vector-push 5 b))
        (vector-push 9 b)
        (if tcs
            (loop repeat (- (tc-width tc) (tc-cent tc) 1) do
                  (vector-push 5 b))
            (return (loop repeat (- (tc-width tc) (tc-cent tc) 1) do
                          (vector-push 0 b)))))
  (if (not (integerp c)) (setq c (- (ceiling w 2) 1)))
  (incf (elt b c) 2)
  (make-treechar
   (nreverse
    (cons
     (loop for i across b collect
           (elt +box-light+ i))
     oo))))

(defun ext-treechar (tc &optional (r 1) (l 0))
  (make-treechar
   (loop for i in (extract-dat tc)
         with a = (make-list r :initial-element #\ )
         and b = (make-list l :initial-element #\ ) collect
         (nconc a i b))
   (+ (tc-cent tc) (floor l 2))))

;;(defparameter *q* (make-treechar '(" a  " " b  " " c  ")))
;;(print-treechar (merge-horiz t *q* *q* *q* (merge-horiz t *q* *q*)))
