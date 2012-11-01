#!/bin/bash

echo "(progn (load \"singl.asd\") (asdf:load-system :singl))

 (ext:saveinitmem \"singl\" :init-function #'singl:main :quiet t :norc t :executable t)" | clisp
