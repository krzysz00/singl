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

(defpackage :singl-asd
  (:use :cl :asdf))

(in-package :singl-asd)

(defsystem :singl
  :name "singl"
  :version "0.1"
  :license "GPLv3 or any later version"
  :in-order-to ((test-op (test-op #:dlist-test)))
  :author "Krzysztof Drewniak <krzysdrewniak <AT> gmail <DOT> com>"
  :serial t
  :description "Singl - a single-character-based esolang"
  :components ((:file "defpackage")
               (:file "parser")
               (:file "evalulator"))
  :depends-on (:smug))

(defsystem :singl-test
  :depends-on (:dlist :lisp-unit)
  :name "singl-test"
  :author "Krzysztof Drewniak <krzysdrewniak <AT> gmail <DOT> com"
  :license "GPLv3 or any later version"
  :description "The test suite for Singl, a single-character-based esolang."
  :serial t
  :components ((:file "tests")))
