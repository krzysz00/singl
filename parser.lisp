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
  (=let* ((char (=satisfies 
                 #'(lambda (c) 
                     (every #'(lambda (x) (char/= c x)) 
                            (coerce "&@^_$!|()?,~+-*/%" 'list))))))
    (result (char-code char))))

(defun modified-variable (modifier identifier)
  (=let* ((_ (=char modifier))
          (var (variable-char)))
    (result (list identifier var))))

(defmacro syn-mod-var (char var)
  `(defun ,var () (modified-variable ,char ',var)))

(syn-mod-var #\& address)
(syn-mod-var #\@ dereference)
(syn-mod-var #\^ output)
(syn-mod-var #\_ input)

(defun modifiers ()
  (=or (address) (dereference) (output) (input)))

(defun char-as-symbol (char symbol)
  (=let* ((_ (=char char)))
    (result symbol)))

(defmacro char-symbol (char var)
  `(defun ,var () (char-as-symbol ,char ',var)))

(char-symbol #\| argument)
(char-symbol #\+ add)
(char-symbol #\- subtract)
(char-symbol #\* multiply)
(char-symbol #\/ divide)
(char-symbol #\% modulo)
(char-symbol #\? if-zero)
(char-symbol #\, if-<=-than-zero)
(char-symbol #\~ loop-until-zero)

(defun symbols ()
  (=or (argument) (add) (subtract) (multiply) (divide) (modulo)
       (if-zero) (if-<=-than-zero) (loop-until-zero)))

(defun token ()
  (=or (modifiers) (symbols) (variable-char)))

(defun tokens () (one-or-more (token)))

(defun grouping (left right marker)
  (=let* ((_ (=char left))
          (body (entities))
          (_ (=char right)))
    (result (cons marker body))))

(defmacro defgroup (left right var)
  `(defun ,var () (grouping ,left ,right ',var)))

(defgroup #\( #\) group)
(defgroup #\$ #\! assignment)
(defgroup #\ #\ %comment)

(defun comment ()
  (=let* ((_ (%comment)))
    (result -1)))

(defun entity ()
  (=or (token) (group) (assignment) (comment)))

(defun entities () (one-or-more (entity)))

(defun parse (thing)
  (first (first (funcall (entities) thing))))
