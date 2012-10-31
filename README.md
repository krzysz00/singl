Single - A Single-Character-Based Esolang
=========================================

Variables
---------
A variable is any single character with ASCII value > 64 or any single
character outside the ASCII range that is not otherwise reserved.

Special Operators and functions
-------------------------------
 * `&x` the address of `x`. `&x` is the Unicode code point of `x`, as
   returned by the lisp function `code-char`
 * `@x` the value at the variable with address `x`
 * `^x` output the value at `x`
 * `_x` input a character (value) to `x`
 * `-xy!` Everything up to the matching `!` is assigned to `x`.
   Returns no value.
 * `|` Grab next argument from calling context. These are counted,
   sort of. This means that functions can assign arguments at any
   time, with the statement `-A|!`
 * `` (`^C`) Comment markers
 * `(xyzab)` Group markers. Code between them is parsed, then
   evaluated unless at the end of something that groups, like
   assignment. 
 * `?txy` If `t` is 0, then evaluate `x` and discard `y`. 
   Otherwise, discard `x` and evaluate `y`.
 * `,txy` See `?` but test `t <= 0`, not `t = 0`.
 * `~tx`. Evaluate `t`. Evaluate `x` if `t ≠ 0`.
 * No numeric literals. You'll be using `&A = 64` and friends. 

Precedence (high-to-low)
 1. `@&^_|` (one at a time)
 2. `,~?` 
 3. `() -!`
 5. function application
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
