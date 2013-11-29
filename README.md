Singl - A Single-Character-Based Esolang
=========================================

Variables
---------
A variable is any single character (that is, anything with a Unicode
code point) that is not reserved for syntactical purposes. These
characters are:

    &@^_$!|()?,~+-*/%

This means that space is a variable. So is newline. 
Special Operators and functions
-------------------------------
 * `&x` the address of `x`. `&x` is the Unicode code point of `x`, as
   returned by the lisp function `code-char`
 * `@x` the value at the variable with the address that is the value of `x` (dereference `x`)
 * `^x` output the value at `x`.
 * `_x` input a character (value) to `x`. If EOF is reached, `x` is
   -1. 
   Output and input use literal variable names. You
   cannot use indirection in these statements.
 * `$xy!` Everything up to the matching `!` is assigned to `x`. `y` is
   implicitly a group. Returns `y`. Assignment to `&x` is equivalent
   to assignment to `x`, as all modifiers other than `@` are ignored.
   Assignment to `@x` will dereference the pointer and assign to the
   pointed-to location.
 * `|` Grab next argument from calling context. If the next argument
   is a group, the group is evaluated and its value is the value of
   `|`. `|` in the group being evaluated to yield `|` will get
   arguments from the same context as the group calling `|`. This
   means that, for two argument functions `p` and `q`, and variables
   `a` `b` `c`, and `d`, `paqbcd` is parsed as `p(a,q(b,c)d`. Groups
   can assign arguments at any time, with the statement `$A|!`
 * `` (`^C`) Comment markers
 * `(xyzab)` Group markers. Code between them is parsed, then
   evaluated unless there are no possible arguments after the group,
   like in assignment. The last value in the group is returned as the
   value of the group.
 * `?txy` If `t` is 0, then evaluate `x` and discard `y`. 
   Otherwise, discard `x` and evaluate `y`.
 * `,txy` See `?` but test `t <= 0`, not `t = 0`.
 * `~tx`. Evaluate `t`. Evaluate `x` if `t ≠ 0`. Repeat until `t = 0`. 
 * No numeric literals. You'll be using `&A = 65` and friends. 

The values (for function call purposes) of `?,~^_` are undefined.
Assignment provides the assigned value. Parsing and evaluation
proceeds left-to-right. 

Precedence (high-to-low)
 1. `@&^_|` (one at a time, chaining is an error)
 2. `,~?` and the arithmetic functions `+-*/%`
 3. `() -!`
 4. comments
 4. function application
 
Functions (built-in)
--------------------
* `+xy`. Add `x` and `y`.
* `-xy`. Subtract.
* `*xy`. Multiply.
* `/xy`. Divide (truncating).
* `%xy`. Modulo.

These functions do not modify any variables.

Memory
------
Memory locations are addressed starting from zero. Each variable is
stored in the memory location that is addressed by its Unicode code
point. Variables may hold either groups or integers, which are signed
and extend infinitely in both directions. All variables are global.
There is no suggested calling convention, roll your own. 

Example programs
----------------
`hello-functiony.singl` is a "Hello, World" program.

Building and running programs
-----------------------------
Run either `build-clisp.sh` or `build-sbcl.sh`. This produces the
executable `singl`. This executable can be called with the filename of
the program as a command-line argument. If no filename is provided,
the program is read from standard input.

Licensing
---------
This language (Singl) can be implemented by anyone under whatever
license they like. This implementation of Singl is under Copyright
(C) 2012 Krzysztof Drewniak and released under the GNU General Public
License version 3 or any later version. 
