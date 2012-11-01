#|
Copyright (C) 2012  Krzysztof Drewniak <krzysdrewniak <AT> gmail <DOT> com>

This file is part of Singl.

Singl is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

Singl is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with Singl in the file COPYING.  If not, see <http://www.gnu.org/licenses/>.
|#

(in-package :singl)

(defparameter *contexts* nil "The contexts to draw arguments to groups from")

(defparameter *memory* (make-hash-table) "The memory of the program")

(defun push-context (context)
  (if *contexts* (push context *contexts*) 
      (setf *contexts* (list context))))

(defun get-argument ()
  (let ((callees (list (pop *contexts*))))
    (loop until (equal (first callees) :caller) do (push (pop *contexts*) callees))
    (let ((argument (get-value)))
      (map nil #'push-context callees)
      argument)))

(defun get-value-loop ()
  (let ((ret t))
    (loop while (first *contexts*) do (setf ret (or (get-value) ret)))
    (pop *contexts*)
    ret))

(defun get-value ()
  (let ((item (pop (first *contexts*))))
;;    (print (list :c *contexts*))
;;    (format t "item: `~a~% memory ~a~%" item 
;;            (loop for v being the hash-values in *memory* using (hash-key k)
;;               when (integerp v) collect (list :k k :v v)))
    (cond
      ((numberp item) 
       (if (= item -1)
           (get-value)
           (let ((value (gethash item *memory* 0)))
             (if (and (listp value) (eql 'group (car value))) 
                 (progn (push value (first *contexts*)) (get-value))
                 value))))
      ((eql item nil) nil)
      ((listp item)
       (case (car item)
         (address (second item))
         (dereference (gethash (gethash (second item) *memory* 0) *memory* 0))
         (output (format t "~a" (code-char (gethash (second item) *memory* 0))))
         (input 
          (let ((char (read-char *standard-input* nil -1)))
            (setf (gethash (second item) *memory*) (if (equal char -1) char (char-code char)))))
         (group
          (if (first *contexts*)
              (progn
                (push-context :caller)
                (push-context (cdr item))
                (prog1
                    (get-value-loop)
                  (pop *contexts*)))
              item))
         (assignment
          (let* ((location (second item))
                 (value (cdr (cdr item))))
            (when (listp location)
              (if (eql (car location) 'dereference)
                  (setf location (gethash (second location) *memory* 0))
                  (setf location (second location))))
;;            (print (list :l location :v value))
            (push-context value)
            (setf (gethash location *memory*) (get-value-loop))
            (gethash location *memory*)))))
      ((symbolp item)
       (case item
         (argument (get-argument))
         ((if-<=-than-zero if-zero)
          (let ((condition (get-value)))
            (if (funcall (if (eql item 'if-zero) #'= #'<=) condition 0)
                (prog1 (get-value) (pop (first *contexts*))) 
                (progn (pop (first *contexts*)) (get-value)))))
         (loop-until-zero
            (let* ((test (first (first *contexts*)))
                   (body (second (first *contexts*))))
              (loop named find do
                   (let ((ret (get-value)))
                     (if (equal ret 0)
                         (progn (pop (first *contexts*)) (return-from find ret))
                         (progn (get-value) (push body (first *contexts*)) 
                                (push test (first *contexts*))))))))
         ((add subtract multiply divide modulo)
          (let* ((first (get-value)) (second (get-value)))
            (funcall 
             (case item (add #'+) (subtract #'-) (multiply #'*) 
                   (divide #'(lambda (a b) (truncate (float (/ a b)))))
                   (modulo #'rem))
             first second)))))
      (t (error "Something that isn't supposed to be here is here. Whatever shall we do?")))))

(defun interpret (parsed)
  (push-context parsed)
  (get-value-loop))

(defun evaluate (thing)
  (interpret (parse thing)))

(defun clear ()
  (setf *contexts* nil *memory* (make-hash-table)))

(defun main ()
  (let ((input (make-array 1000 :element-type 'character :adjustable t :fill-pointer 0)))
    (loop for c = (read-char t nil nil)
         while c
         do (vector-push-extend c input))
    (clear)
    (evaluate input)
    #+sbcl (sb-ext:quit)
    #+(or clisp cmucl) (ext:quit)
    #-(or sbcl clipt cmucl) (error "Please quit your Lisp implementation and help the pornitg effort.")))

