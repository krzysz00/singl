Single --- A Single-Character (mostly) Esolang
==============================================

Special Operators and functions
-------------------------------
 * `&x` the address of `x`. `&x` is the unicode code point of `x`, as
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
 * `(xyzab)` Group markers. Code between them is parsed, then left
   unevaluated until followed by a value. 
 * `?txy` If `t` is 0, then evaluate `x`, with `t` as its first
   argument and discard `y`. Otherwise, discard `x` and evaluate `y`
   with `t` as its first argument.
 * `,txy` See `?` but test `t <= 0`, not `t = 0`.
 * `~tx`. Evaluate `t`. Evaluate `x` with t as its first argument if
   `t ≠ 0`.
 * No numeric literals. You'll be using `&A = 64` and friends. 

Precedence (high-to-low)
 1. `@&^_|`
 2. `,~?` 
 3. `()`
 4. `-!`
 5. function application
Functions (built-in)
--------------------
* `+xy`. Add `x` and `y`.
* `-xy`. Subtract.
* `*xy`. Multiply.
* `/xy`. Divide (truncating).
* `%xy`. Modulo.
