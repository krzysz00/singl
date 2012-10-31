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

(defun =char-code (n)
  (=satisfies #'(lambda (c) (= (char-code c) n))))

(defun variable-char ()
  (=let* ((char (=satisfies #'(lambda (c) (> (char-code c) 64)))))
    (result (char-code char))))

(defun modified-variable (modifier identifier)
  (=let* ((_ (=char modifier))
          (var (variable-char)))
    (result (list identifier var))))

(defun address () (modified-variable #\& 'address)
(defun dereference () (modified-variable #\@ 'dereference))
 

(defun char-as-symbol (char symbol)
  (and (=char char) (result symbol)))

(defun argument-bar ()
  (char-as-symbol #\| 'argument))
