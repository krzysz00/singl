Single - A Single-Character-Based Esolang
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
 * `@x` the value at the variable with address `x`
 * `^x` output the value at `x`
 * `_x` input a character (value) to `x`
 * `$xy!` Everything up to the matching `!` is assigned to `x`.
   Returns no value.
 * `|` Grab next argument from calling context. If the next argument
   is a group, the group is evaluated and its value is the value of
   `|`. `|` in the group being evaluated to yield `|` will get
   arguments from the same context as the group calling `|`. This
   means that, for two argument functions `p` and `q`, and variables
   `a` `b` `c`, and `d`, `paqbcd` is parsed as `p(a,q(b,c)d`. Groups
   can assign arguments at any time, with the statement `$A|!`
 * `` (`^C`) Comment markers
 * `(xyzab)` Group markers. Code between them is parsed, then
   evaluated unless at the end of something that groups, like
   assignment. The last value in the group is returned as the value of
   the group.
 * `?txy` If `t` is 0, then evaluate `x` and discard `y`. 
   Otherwise, discard `x` and evaluate `y`.
 * `,txy` See `?` but test `t <= 0`, not `t = 0`.
 * `~tx`. Evaluate `t`. Evaluate `x` if `t ≠ 0`.
 * No numeric literals. You'll be using `&A = 64` and friends. 

Parsing and evaluation proceeds left-to-right. 
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

This language (Single) can be implemented by anyone under whatever
license they like. This implementation of Single is under Copyright
(C) 2012 Krzysztof Drewniak and released under the GNU General Public
License version 3 or any later version. 
