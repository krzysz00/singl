#|
Copyright (C) 2012  Krzysztof Drewniak <krzysdrewniak <AT> gmail <DOT> com>

This file is part of Single.

Single is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

Single is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with Single in the file COPYING.  If not, see <http://www.gnu.org/licenses/>.
|#

(in-package :single)

(defparameter *contexts* nil "The contexts to draw arguments to groups from")

(defparameter *memory* (make-hash-table) "The memory of the program")

(defun push-context (context)
  (if *contexts* (push context *contexts*) 
      (setf *contexts* (list context))))

(defun get-argument ()
  (let ((callee (pop *contexts*)))
    (let ((argument (get-value)))
      (push-context callee)
      argument)))

(defun get-value-loop ()
  (let ((ret t))
    (loop while (first *contexts*) do (setf ret (get-value)))
    (pop *contexts*)
    ret))

(defun get-value ()
  (let ((item (pop (first *contexts*))))
    (cond
      ((numberp item) 
       (let ((value (gethash item *memory* 0)))
         (if (and (listp value) (eql 'group (car value))) 
             (progn (push value (first *contexts*)) (get-value))
             value)))
      ((eql item nil) (get-value))
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
                (push-context (cdr item))
                (get-value-loop))
              item))
         (assignment
          (let* ((location (second item))
                 (value (cdr (cdr item))))
            (print :location)
            (print location)
            (print :value)
            (print value)
            (when (listp location)
              (if (eql (car location) 'dereference)
                  (setf location (gethash (second location) *memory* 0))
                  (setf location (second location))))
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
