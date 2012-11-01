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

(in-package #:singl)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (use-package :lisp-unit))

(defun %run-tests ()
  (lisp-unit:run-all-tests :singl))

(defmethod asdf:perform ((o asdf:test-op) (system (eql (asdf:find-system 'singl-test))))
  (%run-tests))

