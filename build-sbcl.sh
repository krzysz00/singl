#!/bin/bash

echo "(progn (load \"singl.asd\") (asdf:load-system :singl)) 

(sb-ext:save-lisp-and-die \"singl\" :toplevel #'singl:main :executable t)" | sbcl
