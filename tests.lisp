(in-package #:single)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (use-package :lisp-unit))

(defun %run-tests ()
  (lisp-unit:run-all-tests :dlist))

(defmethod asdf:perform ((o asdf:test-op) (system (eql (asdf:find-system 'single-test))))
  (%run-tests))

