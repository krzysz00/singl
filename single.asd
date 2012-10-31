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
along with clos-diff in the file COPYING.  If not, see <http://www.gnu.org/licenses/>.
|#

(defpackage #:single-asd
  (:use :cl :asdf))

(in-package :single-asd)

(defsystem :single
  :name "single"
  :version "0.1"
  :license "GPLv3 or any later version"
  :in-order-to ((test-op (test-op #:dlist-test)))
  :author "Krzysztof Drewniak <krzysdrewniak <AT> gmail <DOT> com>"
  :serial t
  :documentation "Single - a single-character-based esolang"
  :components ((:file "defpackage")
               )
  :depends-on (:smug))

(defsystem :single-test
  :depends-on (:dlist :lisp-unit)
  :name "single-test"
  :author "Krzysztof Drewniak <krzysdrewniak <AT> gmail <DOT> com"
  :license "GPLv3 or any later version"
  :description "The test suite for Single, a single-character-based esolang."
  :serial t
  :components ((:file "tests")))
