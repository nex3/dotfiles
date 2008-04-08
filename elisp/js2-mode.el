;;; js2.el -- an improved JavaScript editing mode
;;;
;;; This file was auto-generated on Sun Apr 06 23:28:49 2008 from files:
;;;  js2-vars.el
;;;  js2-util.el
;;;  js2-scan.el
;;;  js2-messages.el
;;;  js2-ast.el
;;;  js2-highlight.el
;;;  js2-parse.el
;;;  js2-indent.el
;;;  js2-mode.el

;;; js2-mode.el --- an improved JavaScript editing mode

;; Author:  Steve Yegge (steve.yegge@gmail.com)
;; Keywords:  javascript languages

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2 of
;; the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be
;; useful, but WITHOUT ANY WARRANTY; without even the implied
;; warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
;; PURPOSE.  See the GNU General Public License for more details.

;; You should have received a copy of the GNU General Public
;; License along with this program; if not, write to the Free
;; Software Foundation, Inc., 59 Temple Place, Suite 330, Boston,
;; MA 02111-1307 USA

;;; Commentary:

;; This JavaScript editing mode supports:
;;
;;  - the full JavaScript language through version 1.7
;;  - accurate syntax highlighting using a recursive-descent parser
;;  - syntax-error and strict-mode warning reporting
;;  - "bouncing" line indentation to choose among alternate indentation points
;;  - smart line-wrapping within comments (Emacs 22+) and strings
;;  - code folding:
;;    - show some or all function bodies as {...}
;;    - show some or all block comments as /*...*/
;;  - context-sensitive menu bar and popup menus
;;  - typing helpers (e.g. inserting matching braces/parens)
;;  - many customization options
;;
;; It is only compatible with GNU Emacs versions 21 and higher (not XEmacs).
;;
;; Installation:
;;
;;  - put `js2.el' somewhere in your emacs load path
;;  - M-x byte-compile-file RET <path-to-js2.el> RET
;;    Note:  it will refuse to run unless byte-compiled
;;  - add these lines to your .emacs file:
;;    (autoload 'js2-mode "js2" nil t)
;;    (add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))
;;
;; To customize how it works:
;;   M-x customize-group RET js2-mode RET
;;
;; The variable `js2-mode-version' is a date stamp.  When you upgrade
;; to a newer version, you must byte-compile the file again.
;;
;; Notes:
;;
;; Some of the functionality does not work in Emacs 21 -- upgrading to
;; Emacs 22 or higher will get you better results.  If you byte-compiled
;; js2.el with Emacs 21, you should re-compile it for Emacs 22.
;;
;; Unlike cc-engine based language modes, js2-mode's line-indentation is not
;; customizable.  It is a surprising amount of work to support customizable
;; indentation.  The current compromise is that the tab key lets you cycle among
;; various likely indentation points, similar to the behavior of python-mode.
;;
;; This code is part of a larger project, in progress, to enable writing
;; Emacs customizations in JavaScript.
;;
;; This mode does not yet work with mmm-mode ("multiple major modes" mode),
;; although it could possibly be made to do so with some effort.
;;
;; Please email bug reports and suggestions to the author.

;; TODO:
;;  - fix Adam's unicode issue
;;  - add a command to show syntax errors on demand (for when they're off)
;;  - fix problem with use-default-fonts (issues w/ multiple frames)
;;  - fix js2-enter-key behavior in line following /"/ regexp literal
;;  - better imenu support (see `js2-parse-record-imenu' for details)
;;  - add in remaining Ecma strict-mode warnings
;;    - option not to warn missing semi on same line as close-curly
;;    - explain how to bypass warning for mistyped == as =
;;  - find some way to support script tags in html files
;;    - maybe use mmm and have scanner skip mmm non-js regions?
;;  - find a way to support JSON
;;  - parse and highlight e4x literals
;;  - parse and highlight regexps
;;  - define Eclipse and IntellJ color schemes
;;  - get more use out of the symbol table:
;;    - jump to declaration
;;    - rename variable/function
;;    - warn on unused var
;;  - add some dabbrev-expansions for built-in keywords like finally, function
;;  - add at least some completion support, e.g. for built-ins
;;  - use fringe to show (and jump to) error locations in Emacs 22
;;  - better context menus
;;  - code formatting

;;; Code:
;;; js2-vars.el -- byte-compiler support for js2-mode

;; Author:  Steve Yegge (steve.yegge@gmail.com)
;; Keywords:  javascript languages

;;; Code:

(eval-when-compile
  (require 'cl))

(eval-and-compile
  (require 'cc-mode)     ; (only) for `c-populate-syntax-table'
  (require 'cc-langs)    ; it's here in Emacs 21...
  (require 'cc-engine))  ; for `c-paragraph-start' et. al.

(defvar emacs22 (>= emacs-major-version 22))

(defcustom js2-highlight-level 2
  "Amount of syntax highlighting to perform.
nil, zero or negative means none.
1 adds basic syntax highlighting.
2 adds highlighting of some Ecma built-in properties.
3 adds highlighting of many Ecma built-in functions."
  :type 'integer
  :group 'js2-mode)

(defvar js2-mode-dev-mode-p nil
  "Non-nil if running in development mode.  Normally nil.")

(defgroup js2-mode nil
  "An improved JavaScript mode."
  :group 'languages)

(defcustom js2-basic-offset (if (and (boundp 'c-basic-offset)
                                     (numberp c-basic-offset))
                                c-basic-offset
                              2)
  "Number of spaces to indent nested statements.
Similar to `c-basic-offset'."
  :group 'js2-mode
  :type 'integer)
(make-variable-buffer-local 'js2-basic-offset)

(defcustom js2-cleanup-whitespace t
  "Non-nil to invoke `delete-trailing-whitespace' before saves."
  :type 'boolean
  :group 'js2-mode)

(defcustom js2-move-point-on-right-click t
  "Non-nil to move insertion point when you right-click.
This makes right-click context menu behavior a bit more intuitive,
since menu operations generally apply to the point.  The exception
is if there is a region selection, in which case the point does -not-
move, so cut/copy/paste etc. can work properly.

Note that IntelliJ moves the point, and Eclipse leaves it alone,
so this behavior is customizable."
  :group 'js2-mode
  :type 'boolean)

(defcustom js2-mirror-mode t
  "Non-nil to insert closing brackets, parens, etc. automatically."
  :group 'js2-mode
  :type 'boolean)

(defcustom js2-auto-indent-flag t
  "Automatic indentation with punctuation characters. If non-nil, the
current line is indented when certain punctuations are inserted."
  :group 'js2-mode
  :type 'boolean)

(defcustom js2-bounce-indent-flag t
  "Non-nil to have indent-line function choose among alternatives.
If nil, the indent-line function will indent to a predetermined column
based on heuristic guessing.  If non-nil, then if the current line is
already indented to that predetermined column, indenting will choose
another likely column and indent to that spot.  Repeated invocation of
the indent-line function will cycle among the computed alternatives.
See the function `js2-bounce-indent' for details."
  :type 'boolean
  :group 'js2-mode)

(defcustom js2-indent-on-enter-key nil
  "Non-nil to have Enter/Return key indent the line.
This is unusual for Emacs modes but common in IDEs like Eclipse."
  :type 'boolean
  :group 'js2-mode)

(defcustom js2-rebind-eol-bol-keys t
  "Non-nil to rebind beginning-of-line and end-of-line keys.
If non-nil, bounce between bol/eol and first/last non-whitespace char."
  :group 'js2-mode
  :type 'boolean)

(defcustom js2-electric-keys '("{" "}" "(" ")" "[" "]" ":" ";" "," "*")
  "Keys that auto-indent when `js2-auto-indent-flag' is non-nil.
Each value in the list is passed to `define-key'."
  :type 'list
  :group 'js2-mode)

(defcustom js2-idle-timer-delay 0.2
  "Delay in secs before re-parsing after user makes changes.
Multiplied by `js2-dynamic-idle-timer-adjust', which see."
  :type 'number
  :group 'js2-mode)
(make-variable-buffer-local 'js2-idle-timer-delay)

(defcustom js2-dynamic-idle-timer-adjust 0
  "Positive to adjust `js2-idle-timer-delay' based on file size.
The idea is that for short files, parsing is faster so we can be
more responsive to user edits without interfering with editing.
The buffer length in characters (typically bytes) is divided by
this value and used to multiply `js2-idle-timer-delay' for the
buffer.  For example, a 21k file and 10k adjust yields 21k/10k
== 2, so js2-idle-timer-delay is multiplied by 2.
If `js2-dynamic-idle-timer-adjust' is 0 or negative,
`js2-idle-timer-delay' is not dependent on the file size."
  :type 'number
  :group 'js2-mode)

(defcustom js2-mode-escape-quotes t
  "Non-nil to disable automatic quote-escaping inside strings."
  :type 'boolean
  :group 'js2-mode)

(defcustom js2-mode-squeeze-spaces t
  "Non-nil to normalize whitespace when filling in comments.
Multiple runs of spaces are converted to a single space."
  :type 'boolean
  :group 'js2-mode)

(defcustom js2-mode-show-parse-errors t
  "True to highlight parse errors."
  :type 'boolean
  :group 'js2-mode)

(defcustom js2-mode-show-strict-warnings t
  "Non-nil to emit Ecma strict-mode warnings.
Some of the warnings can be individually disabled by other flags,
even if this flag is non-nil."
  :type 'boolean
  :group 'js2-mode)

(defcustom js2-strict-trailing-comma-warning t
  "Non-nil to warn about trailing commas in array literals.
Ecma-262 forbids them, but many browsers permit them.  IE is the
big exception, and can produce bugs if you have trailing commas."
  :type 'boolean
  :group 'js2-mode)

(defcustom js2-strict-missing-semi-warning t
  "Non-nil to warn about semicolon auto-insertion after statement.
Technically this is legal per Ecma-262, but some style guides disallow
depending on it."
  :type 'boolean
  :group 'js2-mode)

(defcustom js2-missing-semi-one-line-override nil
  "Non-nil to permit missing semicolons in one-line functions.
In one-liner functions such as `function identity(x) {return x}'
people often omit the semicolon for a cleaner look.  If you are
such a person, you can suppress the missing-semicolon warning
by setting this variable to t."
  :type 'boolean
  :group 'js2-mode)

(defcustom js2-strict-inconsistent-return-warning t
  "Non-nil to warn about mixing returns with value-returns.
It's perfectly legal to have a `return' and a `return foo' in the
same function, but it's often an indicator of a bug, and it also
interferes with type inference (in systems that support it.)"
  :type 'boolean
  :group 'js2-mode)

(defcustom js2-strict-cond-assign-warning t
  "Non-nil to warn about expressions like if (a = b).
This often should have been '==' instead of '='.  If the warning
is enabled, you can suppress it on a per-expression basis by
parenthesizing the expression, e.g. if ((a = b)) ..."
  :type 'boolean
  :group 'js2-mode)

(defcustom js2-basic-offset c-basic-offset
  "Functions like `c-basic-offset' in js2-mode buffers."
  :type 'integer
  :group 'js2-mode)
(make-variable-buffer-local 'js2-basic-offset)

(defcustom js2-language-version 170
  "Configures what JavaScript language version to recognize.
Currently only 150, 160 and 170 are supported, corresponding
to JavaScript 1.5, 1.6 and 1.7, respectively.  In a nutshell,
1.6 adds E4X support, and 1.7 adds let, yield, and Array
comprehensions."
  :type 'integer
  :group 'js2-mode)

(defcustom js2-allow-rhino-new-expr-initializer t
  "Non-nil to support a Rhino's experimental syntactic construct.

Rhino supports the ability to follow a `new' expression with an object
literal, which is used to set additional properties on the new object
after calling its constructor.  Syntax:

  new <expr> [ ( arglist ) ] [initializer]

Hence, this expression:

  new Object {a: 1, b: 2}

results in an Object with properties a=1 and b=2.  This syntax is
apparently not configurable in Rhino - it's currently always enabled,
as of Rhino version 1.7R2."
  :type 'boolean
  :group 'js2-mode)

(defcustom js2-allow-member-expr-as-function-name nil
  "Non-nil to support experimental Rhino syntax for function names.

NOTE:  this is currently a placeholder, and `js2-mode' does not yet
support this syntax.

Rhino supports an experimental syntax configured via the Rhino Context
setting `allowMemberExprAsFunctionName'.  The experimental syntax is:

  function <member-expr> ( [ arg-list ] ) { <body> }

Where member-expr is a non-parenthesized 'member expression', which
is anything at the grammar level of a new-expression or lower, meaning
any expression that does not involve infix or unary operators.

When <member-expr> is not a simple identifier, then it is syntactic
sugar for assigning the anonymous function to the <member-expr>.  Hence,
this code:

  function a.b().c[2] (x, y) { ... }

is rewritten as:

  a.b().c[2] = function(x, y) {...}

which doesn't seem particularly useful, but Rhino permits it."
  :type 'boolean
  :group 'js2-mode)

(defvar js2-mode-version 20080406
  "Release number for `js2-mode'.")

;; scanner variables

;; We record the start and end position of each token.
(defvar js2-token-beg 1)
(make-variable-buffer-local 'js2-token-beg)
(defvar js2-token-end -1)
(make-variable-buffer-local 'js2-token-end)

(defvar js2-EOF_CHAR -1
  "Represents end of stream.  Distinct from js2-EOF token type.")

;; I originally used symbols to represent tokens, but Rhino uses
;; ints and then sets various flag bits in them, so ints it is.
;; The upshot is that we need a `js2-' prefix in front of each name.
(defvar js2-ERROR -1)
(defvar js2-EOF 0)
(defvar js2-EOL 1)
(defvar js2-ENTERWITH 2)       ; begin interpreter bytecodes
(defvar js2-LEAVEWITH 3)
(defvar js2-RETURN 4)
(defvar js2-GOTO 5)
(defvar js2-IFEQ 6)
(defvar js2-IFNE 7)
(defvar js2-SETNAME 8)
(defvar js2-BITOR 9)
(defvar js2-BITXOR 10)
(defvar js2-BITAND 11)
(defvar js2-EQ 12)
(defvar js2-NE 13)
(defvar js2-LT 14)
(defvar js2-LE 15)
(defvar js2-GT 16)
(defvar js2-GE 17)
(defvar js2-LSH 18)
(defvar js2-RSH 19)
(defvar js2-URSH 20)
(defvar js2-ADD 21)            ; infix plus
(defvar js2-SUB 22)            ; infix minus
(defvar js2-MUL 23)
(defvar js2-DIV 24)
(defvar js2-MOD 25)
(defvar js2-NOT 26)
(defvar js2-BITNOT 27)
(defvar js2-POS 28)            ; unary plus
(defvar js2-NEG 29)            ; unary minus
(defvar js2-NEW 30)
(defvar js2-DELPROP 31)
(defvar js2-TYPEOF 32)
(defvar js2-GETPROP 33)
(defvar js2-GETPROPNOWARN 34)
(defvar js2-SETPROP 35)
(defvar js2-GETELEM 36)
(defvar js2-SETELEM 37)
(defvar js2-CALL 38)
(defvar js2-NAME 39)           ; an identifier
(defvar js2-NUMBER 40)
(defvar js2-STRING 41)
(defvar js2-NULL 42)
(defvar js2-THIS 43)
(defvar js2-FALSE 44)
(defvar js2-TRUE 45)
(defvar js2-SHEQ 46)           ; shallow equality (===)
(defvar js2-SHNE 47)           ; shallow inequality (!==)
(defvar js2-REGEXP 48)
(defvar js2-BINDNAME 49)
(defvar js2-THROW 50)
(defvar js2-RETHROW 51)        ; rethrow caught exception: catch (e if ) uses it
(defvar js2-IN 52)
(defvar js2-INSTANCEOF 53)
(defvar js2-LOCAL_LOAD 54)
(defvar js2-GETVAR 55)
(defvar js2-SETVAR 56)
(defvar js2-CATCH_SCOPE 57)
(defvar js2-ENUM_INIT_KEYS 58)
(defvar js2-ENUM_INIT_VALUES 59)
(defvar js2-ENUM_INIT_ARRAY 60)
(defvar js2-ENUM_NEXT 61)
(defvar js2-ENUM_ID 62)
(defvar js2-THISFN 63)
(defvar js2-RETURN_RESULT 64)  ; to return previously stored return result
(defvar js2-ARRAYLIT 65)       ; array literal
(defvar js2-OBJECTLIT 66)      ; object literal
(defvar js2-GET_REF 67)        ; *reference
(defvar js2-SET_REF 68)        ; *reference = something
(defvar js2-DEL_REF 69)        ; delete reference
(defvar js2-REF_CALL 70)       ; f(args) = something or f(args)++
(defvar js2-REF_SPECIAL 71)    ; reference for special properties like __proto
(defvar js2-YIELD 72)          ; JS 1.7 yield pseudo keyword

;; XML support
(defvar js2-DEFAULTNAMESPACE 73)
(defvar js2-ESCXMLATTR 74)
(defvar js2-ESCXMLTEXT 75)
(defvar js2-REF_MEMBER 76)     ; Reference for x.@y, x..y etc.
(defvar js2-REF_NS_MEMBER 77)  ; Reference for x.ns::y, x..ns::y etc.
(defvar js2-REF_NAME 78)       ; Reference for @y, @[y] etc.
(defvar js2-REF_NS_NAME 79)    ; Reference for ns::y, @ns::y@[y] etc.

(defvar js2-first-bytecode js2-ENTERWITH)
(defvar js2-last-bytecode js2-REF_NS_NAME)

(defvar js2-TRY 80)
(defvar js2-SEMI 81)           ; semicolon
(defvar js2-LB 82)             ; left and right brackets
(defvar js2-RB 83)
(defvar js2-LC 84)             ; left and right curly-braces
(defvar js2-RC 85)
(defvar js2-LP 86)             ; left and right parens
(defvar js2-RP 87)
(defvar js2-COMMA 88)          ; comma operator

(defvar js2-ASSIGN 89)         ; simple assignment (=)
(defvar js2-ASSIGN_BITOR 90)   ; |=
(defvar js2-ASSIGN_BITXOR 91)  ; ^=
(defvar js2-ASSIGN_BITAND 92)  ; &=
(defvar js2-ASSIGN_LSH 93)     ; <<=
(defvar js2-ASSIGN_RSH 94)     ; >>=
(defvar js2-ASSIGN_URSH 95)    ; >>>=
(defvar js2-ASSIGN_ADD 96)     ; +=
(defvar js2-ASSIGN_SUB 97)     ; -=
(defvar js2-ASSIGN_MUL 98)     ; *=
(defvar js2-ASSIGN_DIV 99)     ; /=
(defvar js2-ASSIGN_MOD 100)    ; %=

(defvar js2-first-assign js2-ASSIGN)
(defvar js2-last-assign js2-ASSIGN_MOD)

(defvar js2-HOOK 101)          ; conditional (?:)
(defvar js2-COLON 102)
(defvar js2-OR 103)            ; logical or (||)
(defvar js2-AND 104)           ; logical and (&&)
(defvar js2-INC 105)           ; increment/decrement (++ --)
(defvar js2-DEC 106)
(defvar js2-DOT 107)           ; member operator (.)
(defvar js2-FUNCTION 108)      ; function keyword
(defvar js2-EXPORT 109)        ; export keyword
(defvar js2-IMPORT 110)        ; import keyword
(defvar js2-IF 111)            ; if keyword
(defvar js2-ELSE 112)          ; else keyword
(defvar js2-SWITCH 113)        ; switch keyword
(defvar js2-CASE 114)          ; case keyword
(defvar js2-DEFAULT 115)       ; default keyword
(defvar js2-WHILE 116)         ; while keyword
(defvar js2-DO 117)            ; do keyword
(defvar js2-FOR 118)           ; for keyword
(defvar js2-BREAK 119)         ; break keyword
(defvar js2-CONTINUE 120)      ; continue keyword
(defvar js2-VAR 121)           ; var keyword
(defvar js2-WITH 122)          ; with keyword
(defvar js2-CATCH 123)         ; catch keyword
(defvar js2-FINALLY 124)       ; finally keyword
(defvar js2-VOID 125)          ; void keyword
(defvar js2-RESERVED 126)      ; reserved keywords

(defvar js2-EMPTY 127)

;; Types used for the parse tree - never returned by scanner.

(defvar js2-BLOCK 128)         ; statement block
(defvar js2-LABEL 129)         ; label
(defvar js2-TARGET 130)
(defvar js2-LOOP 131)
(defvar js2-EXPR_VOID 132)     ; expression statement in functions
(defvar js2-EXPR_RESULT 133)   ; expression statement in scripts
(defvar js2-JSR 134)
(defvar js2-SCRIPT 135)        ; top-level node for entire script
(defvar js2-TYPEOFNAME 136)    ; for typeof(simple-name)
(defvar js2-USE_STACK 137)
(defvar js2-SETPROP_OP 138)    ; x.y op= something
(defvar js2-SETELEM_OP 139)    ; x[y] op= something
(defvar js2-LOCAL_BLOCK 140)
(defvar js2-SET_REF_OP 141)    ; *reference op= something

;; For XML support:
(defvar js2-DOTDOT 142)        ; member operator (..)
(defvar js2-COLONCOLON 143)    ; namespace::name
(defvar js2-XML 144)           ; XML type
(defvar js2-DOTQUERY 145)      ; .() -- e.g., x.emps.emp.(name == "terry")
(defvar js2-XMLATTR 146)       ; @
(defvar js2-XMLEND 147)

;; Optimizer-only tokens
(defvar js2-TO_OBJECT 148)
(defvar js2-TO_DOUBLE 149)

(defvar js2-GET 150)           ; JS 1.5 get pseudo keyword
(defvar js2-SET 151)           ; JS 1.5 set pseudo keyword
(defvar js2-LET 152)           ; JS 1.7 let pseudo keyword
(defvar js2-CONST 153)
(defvar js2-SETCONST 154)
(defvar js2-SETCONSTVAR 155)
(defvar js2-ARRAYCOMP 156)
(defvar js2-LETEXPR 157)
(defvar js2-WITHEXPR 158)
(defvar js2-DEBUGGER 159)

(defvar js2-COMMENT 160)  ; not yet in Rhino

(defvar js2-num-tokens (1+ js2-COMMENT))

(defconst js2-debug-print-trees nil)

;; Rhino accepts any string or stream as input.
;; Emacs character processing works best in buffers, so we'll
;; assume the input is a buffer.  JavaScript strings can be
;; copied into temp buffers before scanning them.

(defmacro deflocal (name value comment)
  `(progn
     (defvar ,name ,value ,comment)
     (make-variable-buffer-local ',name)))

;; Buffer-local variables yield much cleaner code than using `defstruct'.
;; They're the Emacs equivalent of instance variables, more or less.

(deflocal js2-ts-dirty-line nil
  "Token stream buffer-local variable.
Indicates stuff other than whitespace since start of line.")

(deflocal js2-ts-regexp-flags nil
  "Token stream buffer-local variable.")

(deflocal js2-ts-string ""
  "Token stream buffer-local variable.
Last string scanned.")

(deflocal js2-ts-number nil
  "Token stream buffer-local variable.
Last literal number scanned.")

(deflocal js2-ts-hit-eof nil
  "Token stream buffer-local variable.")

(deflocal js2-ts-line-start 0
  "Token stream buffer-local variable.")

(deflocal js2-ts-lineno 1
  "Token stream buffer-local variable.")

(deflocal js2-ts-line-end-char -1
  "Token stream buffer-local variable.")

(deflocal js2-ts-cursor 1  ; emacs buffers are 1-indexed
  "Token stream buffer-local variable.
Current scan position.")

(deflocal js2-ts-is-xml-attribute nil
  "Token stream buffer-local variable.")

(deflocal js2-ts-xml-is-tag-content nil
  "Token stream buffer-local variable.")

(deflocal js2-ts-xml-open-tags-count 0
  "Token stream buffer-local variable.")

(deflocal js2-ts-string-buffer nil
  "Token stream buffer-local variable.
List of chars built up while scanning various tokens.")

(deflocal js2-ts-comment-type nil
  "Token stream buffer-local variable.")

;;; Parser variables

(defvar js2-parsed-errors nil
  "List of errors produced during scanning/parsing.")
(make-variable-buffer-local 'js2-parsed-errors)

(defvar js2-parsed-warnings nil
  "List of warnings produced during scanning/parsing.")
(make-variable-buffer-local 'js2-parsed-warnings)

(defvar js2-recover-from-parse-errors t
  "Non-nil to continue parsing after a syntax error.

In recovery mode, the AST will be built in full, and any error
nodes will be flagged with appropriate error information.  If
this flag is nil, a syntax error will result in an error being
signaled.

The variable is automatically buffer-local, because different
modes that use the parser will need different settings.")
(make-variable-buffer-local 'js2-recover-from-parse-errors)

(defvar js2-parse-hook nil
  "List of callbacks for receiving parsing progress.")
(make-variable-buffer-local 'js2-parse-hook)

(defvar js2-parse-finished-hook nil
  "List of callbacks to notify when parsing finishes.
Not called if parsing was interrupted.")

(defvar js2-is-eval-code nil
  "True if we're evaluating code in a string.
If non-nil, the tokenizer will record the token text, and the AST nodes
will record their source text.  Off by default for IDE modes, since the
text is available in the buffer.")
(make-variable-buffer-local 'js2-is-eval-code)

(defvar js2-parse-ide-mode t
  "Non-nil if the parser is being used for `js2-mode'.
If non-nil, the parser will set text properties for fontification
and the syntax-table.  The value should be nil when using the
parser as a frontend to an interpreter or byte compiler.")

;;; Parser instance variables (buffer-local vars for js2-parse)

(defconst js2-clear-ti-mask #xFFFF
  "Mask to clear token information bits.")

(defconst js2-ti-after-eol (lsh 1 16)
  "Flag:  first token of the source line.")

(defconst js2-ti-check-label (lsh 1 17)
  "Flag:  indicates to check for label.")

(defconst js2-ti-after-comment (lsh 1 18)
  "Flag:  indicates token followed a comment.")

;; Inline Rhino's CompilerEnvirons vars as buffer-locals.

(defvar js2-compiler-generate-debug-info t)
(make-variable-buffer-local 'js2-compiler-generate-debug-info)

(defvar js2-compiler-use-dynamic-scope nil)
(make-variable-buffer-local 'js2-compiler-use-dynamic-scope)

(defvar js2-compiler-reserved-keywords-as-identifier nil)
(make-variable-buffer-local 'js2-compiler-reserved-keywords-as-identifier)

(defvar js2-compiler-xml-available t)
(make-variable-buffer-local 'js2-compiler-xml-available)

(defvar js2-compiler-optimization-level 0)
(make-variable-buffer-local 'js2-compiler-optimization-level)

(defvar js2-compiler-generating-source t)
(make-variable-buffer-local 'js2-compiler-generating-source)

(defvar js2-compiler-strict-mode nil)
(make-variable-buffer-local 'js2-compiler-strict-mode)

(defvar js2-compiler-report-warning-as-error nil)
(make-variable-buffer-local 'js2-compiler-report-warning-as-error)

(defvar js2-compiler-generate-observer-count nil)
(make-variable-buffer-local 'js2-compiler-generate-observer-count)

(defvar js2-compiler-activation-names nil)
(make-variable-buffer-local 'js2-compiler-activation-names)

;; SKIP:  sourceURI

;; There's a compileFunction method in Context.java - may need it.
(defvar js2-called-by-compile-function nil
  "True if `js2-parse' was called by `js2-compile-function'.")
(make-variable-buffer-local 'js2-called-by-compile-function)

;; SKIP:  ts  (we just call `js2-init-scanner' and use its vars)

(defvar js2-current-flagged-token js2-EOF)
(make-variable-buffer-local 'js2-current-flagged-token)

(defvar js2-current-token js2-EOF)
(make-variable-buffer-local 'js2-current-token)

;; SKIP:  node factory - we're going to just call functions directly,
;; and eventually go to a unified AST format.

(defvar js2-nesting-of-function 0)
(make-variable-buffer-local 'js2-nesting-of-function)

;; SKIP:  decompiler
;; SKIP:  encoded-source

;;; These variables are per-function and should be saved/restored
;;; during function parsing.

(defvar js2-current-script-or-fn nil)
(make-variable-buffer-local 'js2-current-script-or-fn)

(defvar js2-current-scope nil)
(make-variable-buffer-local 'js2-current-scope)

(defvar js2-nesting-of-with 0)
(make-variable-buffer-local 'js2-nesting-of-with)

(defvar js2-label-set nil
  "An alist mapping label names to nodes.")
(make-variable-buffer-local 'js2-label-set)

(defvar js2-loop-set nil)
(make-variable-buffer-local 'js2-loop-set)

(defvar js2-loop-and-switch-set nil)
(make-variable-buffer-local 'js2-loop-and-switch-set)

(defvar js2-has-return-value nil)
(make-variable-buffer-local 'js2-has-return-value)

(defvar js2-end-flags 0)
(make-variable-buffer-local 'js2-end-flags)

;; These flags enumerate the possible ways a statement/function can
;; terminate. These flags are used by endCheck() and by the Parser to
;; detect inconsistent return usage.
;;
;; END_UNREACHED is reserved for code paths that are assumed to always be
;; able to execute (example: throw, continue)
;;
;; END_DROPS_OFF indicates if the statement can transfer control to the
;; next one. Statement such as return dont. A compound statement may have
;; some branch that drops off control to the next statement.
;;
;; END_RETURNS indicates that the statement can return (without arguments)
;; END_RETURNS_VALUE indicates that the statement can return a value.
;;
;; A compound statement such as
;; if (condition) {
;;   return value;
;; }
;; Will be detected as (END_DROPS_OFF | END_RETURN_VALUE) by endCheck()

(defconst js2-end-unreached     #x0)
(defconst js2-end-drops-off     #x1)
(defconst js2-end-returns       #x2)
(defconst js2-end-returns-value #x4)
(defconst js2-end-yields        #x8)

;; Rhino awkwardly passes a statementLabel parameter to the
;; statementHelper() function, the main statement parser, which
;; is then used by quite a few of the sub-parsers.  We just make
;; it a buffer-local variable and make sure it's cleaned up properly.
(defvar js2-statement-label nil)  ; type `js2-labeled-stmt-node'
(make-variable-buffer-local 'js2-statement-label)

;; Similarly, Rhino passes an inForInit boolean through about half
;; the expression parsers.  We use a dynamically-scoped variable,
;; which makes it easier to funcall the parsers individually without
;; worrying about whether they take the parameter or not.
(defvar js2-in-for-init nil)
(make-variable-buffer-local 'js2-in-for-init)

(defvar js2-temp-name-counter 0)
(make-variable-buffer-local 'js2-temp-name-counter)

(defvar js2-parse-stmt-count 0)
(make-variable-buffer-local 'js2-parse-stmt-count)

(defsubst js2-get-next-temp-name ()
  (format "$%d" (incf js2-temp-name-counter)))

;;; end of per function variables

(defvar js2-parse-interruptable-p t
  "Set this to nil to force parse to continue until finished.
This will mostly be useful for interpreters.")

(defvar js2-statements-per-pause 50
  "Pause after this many statements to check for user input.
If user input is pending, stop the parse and discard the tree.
This makes for a smoother user experience for large files.
You may have to wait a second or two before the highlighting
and error-reporting appear, but you can always type ahead if
you wish.  This appears to be more or less how Eclipse, IntelliJ
and other editors work.")

(defvar js2-record-comments t
  "Instructs the scanner to record comments in `js2-scanned-comments'.")
(make-variable-buffer-local 'js2-record-comments)

(defvar js2-scanned-comments nil
  "List of all comments from the current parse.")
(make-variable-buffer-local 'js2-scanned-comments)

(defun js2-underline-color (color)
  "Return a legal value for the :underline face attribute based on COLOR."
  ;; In XEmacs the :underline attribute can only be a boolean.
  ;; In GNU it can be the name of a colour.
  (if (featurep 'xemacs)
      (if color t nil)
    color))

(defcustom js2-mode-indent-inhibit-undo nil
  "Non-nil to disable collection of Undo information when indenting lines.
Some users have requested this behavior.  It's nil by default because
other Emacs modes don't work this way."
  :type 'boolean
  :group 'js2-mode)

(defcustom js2-mode-indent-ignore-first-tab nil
  "If non-nil, ignore first TAB keypress if we look indented properly.
It's fairly common for users to navigate to an already-indented line
and press TAB for reassurance that it's been indented.  For this class
of users, we want the first TAB press on a line to be ignored if the
line is already indented to one of the precomputed alternatives.

This behavior is only partly implemented.  If you TAB-indent a line,
navigate to another line, and then navigate back, it fails to clear
the last-indented variable, so it thinks you've already hit TAB once,
and performs the indent.  A full solution would involve getting on the
point-motion hooks for the entire buffer.  If we come across another
use cases that requires watching point motion, I'll consider doing it.

If you set this variable to nil, then the TAB key will always change
the indentation of the current line, if more than one alternative
indentation spot exists."
  :type 'boolean
  :group 'js2-mode)

(defcustom js2-use-font-lock-faces nil
  "Non-nil to use the current font-lock faces for language elements.

The default is to use faces closer to those of Eclipse and IntelliJ.
For example, Emacs has for some reason decided that comments are red,
which makes them feel rather antagonistic.

Users who have their own font-lock settings, or who prefer the Emacs
built-ins to the softer colors chosen by many other popular IDEs,
can set this variable to `t' to use the Emacs font-lock defaults.
Note that some js2 faces do not have font-lock equivalents and must
be customized separately.

Note also that the `font-lock' library must be loaded before `js2-mode'
is loaded if you want to use the font-lock defaults.

Changing this setting currently requires restarting Emacs."
  :type 'boolean
  :group 'js2-mode)

(defface js2-warning-face
  `((((class color) (background light))
     (:underline ,(js2-underline-color "orange")))
    (((class color) (background dark))
     (:underline ,(js2-underline-color "orange")))
    (t (:underline t)))
  "Face for JavaScript warnings."
  :group 'js2-mode)

(defface js2-error-face
  `((((class color) (background light))
     (:foreground "red"))
    (((class color) (background dark))
     (:foreground "red"))
    (t (:foreground "red")))
  "Face for JavaScript errors."
  :group 'js2-mode)

(defface js2-constant-face
  '((t :foreground "gray"))
  "Face used to highlight UPPER_CASE constants, and undefined."
  :group 'js2-mode)

(defface js2-comment-face
  '((t :foreground "DarkGreen"))
  "Face used to highlight comments."
  :group 'js2-mode)

(defface js2-string-face
  '((t :foreground "RoyalBlue"))
  "Face used to highlight strings."
  :group 'js2-mode)

(defface js2-regexp-face
  '((t :foreground "RoyalBlue"))
  "Face used to highlight regexps."
  :group 'js2-mode)

(defface js2-type-face
  '((t :foreground "SteelBlue"))
  "Face used to highlight types."
  :group 'js2-mode)

(defface js2-builtin-face
  '((t :foreground "SlateBlue"))
  "Face used to highlight built-ins."
  :group 'js2-mode)

(defface js2-keyword-face
  '((t :foreground "MediumBlue"))
  "Face used to highlight keywords."
  :group 'js2-mode)

(defface js2-function-name-face
  '((t :foreground "firebrick"))
  "Face used to highlight function names."
  :group 'js2-mode)

(defface js2-variable-name-face
  '((t :foreground "SeaGreen"))
  "Face used to highlight variable names."
  :group 'js2-mode)

(defface js2-jsdoc-tag-face
  '((t :foreground "SlateGray"))
  "Face used to highlight @whatever tags in jsdoc comments."
  :group 'js2-mode)

(defface js2-jsdoc-type-face
  '((t :foreground "SteelBlue"))
  "Face used to highlight {FooBar} types in jsdoc comments."
  :group 'js2-mode)

(defface js2-jsdoc-value-face
  '((t :foreground "PeachPuff3"))
  "Face used to highlight tag values in jsdoc comments."
  :group 'js2-mode)

(defface js2-function-param-face
  '((t :foreground "SeaGreen"))
  "Face used to highlight function parameters in javascript."
  :group 'js2-mode)

(defface js2-instance-member-face
  '((t :foreground "DarkOrchid"))
  "Face used to highlight instance variables in javascript.
Not currently used."
  :group 'js2-mode)

(defface js2-private-member-face
  '((t :foreground "PeachPuff3"))
  "Face used to highlight calls to private methods in javascript.
Not currently used."
  :group 'js2-mode)

(defface js2-private-function-call-face
  '((t :foreground "goldenrod"))
  "Face used to highlight calls to private functions in javascript.
Not currently used."
  :group 'js2-mode)

(defface js2-jsdoc-html-tag-name-face
  (if emacs22
      '((((class color) (min-colors 88) (background light))
         (:foreground "rosybrown"))
        (((class color) (min-colors 8) (background dark))
         (:foreground "yellow"))
        (((class color) (min-colors 8) (background light))
         (:foreground "magenta")))
    '((((type tty pc) (class color) (background light))
       (:foreground "magenta"))
      (((type tty pc) (class color) (background dark))
       (:foreground "yellow"))
      (t (:foreground "RosyBrown"))))
    "Face used to highlight jsdoc html tag names"
  :group 'js2-mode)

(defface js2-jsdoc-html-tag-delimiter-face
  (if emacs22
      '((((class color) (min-colors 88) (background light))
         (:foreground "dark khaki"))
        (((class color) (min-colors 8) (background dark))
         (:foreground "green"))
        (((class color) (min-colors 8) (background light))
         (:foreground "green")))
    '((((type tty pc) (class color) (background light))
       (:foreground "green"))
      (((type tty pc) (class color) (background dark))
       (:foreground "green"))
      (t (:foreground "dark khaki"))))
  "Face used to highlight brackets in jsdoc html tags."
  :group 'js2-mode)

(defface js2-external-variable-face
  '((t :foreground "orange"))
  "Face used to highlight assignments to undeclared variables.
An undeclared variable is any variable not declared with var or let
in the current scope or any lexically enclosing scope.  If you assign
to such a variable, then you are either expecting it to originate from
another file, or you've got a potential bug."
  :group 'js2-mode)

(eval-when-compile
  (ignore-errors
    (require 'font-lock)))

(when (and js2-use-font-lock-faces
           (featurep 'font-lock))
  (copy-face font-lock-builtin-face       'js2-builtin-face)
  (copy-face font-lock-comment-face       'js2-comment-face)
  (copy-face font-lock-constant-face      'js2-constant-face)
  (copy-face font-lock-function-name-face 'js2-function-name-face)
  (copy-face font-lock-keyword-face       'js2-keyword-face)
  (copy-face font-lock-string-face        'js2-regexp-face)
  (copy-face font-lock-string-face        'js2-string-face)
  (copy-face font-lock-type-face          'js2-type-face)
  (copy-face font-lock-variable-name-face 'js2-function-param-face)
  (copy-face font-lock-variable-name-face 'js2-variable-name-face))

(defcustom js2-highlight-external-variables t
  "Non-nil to higlight assignments to undeclared variables."
  :type 'boolean
  :group 'js2-mode)

(defvar js2-mode-map
  (let ((map (make-sparse-keymap))
        keys)
    (define-key map [mouse-1] #'js2-mode-show-node)
    (define-key map "\C-m" #'js2-enter-key)
    (when js2-rebind-eol-bol-keys
      (define-key map "\C-a" #'js2-beginning-of-line)
      (define-key map "\C-e" #'js2-end-of-line))
    (define-key map "\C-c\C-e" #'js2-mode-hide-element)
    (define-key map "\C-c\C-s" #'js2-mode-show-element)
    (define-key map "\C-c\C-a" #'js2-mode-show-all)
    (define-key map "\C-c\C-f" #'js2-mode-toggle-hide-functions)
    (define-key map "\C-c\C-t" #'js2-mode-toggle-hide-comments)
    (define-key map (kbd "C-c C-'") #'js2-next-error)
    ;; also define user's preference for next-error, if available
    (if (setq keys (where-is-internal #'next-error))
        (define-key map (car keys) #'js2-next-error))
    (define-key map (or (car (where-is-internal #'mark-defun))
                        (kbd "M-C-h"))
      #'js2-mark-defun)
    (define-key map (or (car (where-is-internal #'narrow-to-defun))
                        (kbd "C-x nd"))
      #'js2-narrow-to-defun)
    (define-key map [down-mouse-3] #'js2-mouse-3)
    (when js2-auto-indent-flag
      (mapc (lambda (key)
              (define-key map key #'js2-insert-and-indent))
            js2-electric-keys))

    (define-key map [menu-bar javascript]
      (cons "JavaScript" (make-sparse-keymap "JavaScript")))

    (define-key map [menu-bar javascript customize-js2-mode]
      '(menu-item "Customize js2-mode" js2-mode-customize
                  :help "Customize the behavior of this mode"))

    (define-key map [menu-bar javascript separator-1]
      '("--"))

    (define-key map [menu-bar javascript js2-toggle-function]
      '(menu-item "Show/collapse element" js2-mode-toggle-element
                  :help "Hide or show function body or comment"))

    (define-key map [menu-bar javascript separator-1]
      '("--"))

    (define-key map [menu-bar javascript next-error]
      '(menu-item "Next warning or error" js2-next-error
                  :enabled (and js2-mode-ast
                                (or (js2-ast-root-errors js2-mode-ast)
                                    (js2-ast-root-warnings js2-mode-ast)))
                  :help "Move to next warning or error"))

    (define-key map [menu-bar javascript show-comments]
      '(menu-item "Show block comments" js2-mode-toggle-hide-comments
                  :visible js2-mode-comments-hidden
                  :help "Expand all hidden block comments"))

    (define-key map [menu-bar javascript hide-comments]
      '(menu-item "Hide block comments" js2-mode-toggle-hide-comments
                  :visible (not js2-mode-comments-hidden)
                  :help "Show block comments as /*...*/"))

    (define-key map [menu-bar javascript show-all-functions]
      '(menu-item "Show function bodies" js2-mode-toggle-hide-functions
                  :visible js2-mode-functions-hidden
                  :help "Expand all hidden function bodies"))

    (define-key map [menu-bar javascript hide-all-functions]
      '(menu-item "Hide function bodies" js2-mode-toggle-hide-functions
                  :visible (not js2-mode-functions-hidden)
                  :help "Show {...} for all top-level function bodies"))

    map)
  "Keymap used in `js2-mode' buffers.")

(defconst js2-mode-identifier-re "[a-zA-Z_$][a-zA-Z0-9_$]*")

(defvar js2-mode-ast nil "Private variable.")
(make-variable-buffer-local 'js2-mode-ast)

(defvar js2-mode-hook nil)

(defvar js2-mode-parse-timer nil "Private variable.")
(make-variable-buffer-local 'js2-mode-parse-timer)

(defvar js2-mode-buffer-dirty-p nil "Private variable.")
(make-variable-buffer-local 'js2-mode-buffer-dirty-p)

(defvar js2-mode-parsing nil "Private variable.")
(make-variable-buffer-local 'js2-mode-parsing)

(defvar js2-mode-node-overlay nil)
(make-variable-buffer-local 'js2-mode-node-overlay)

(defvar js2-mode-show-overlay js2-mode-dev-mode-p
  "Debug:  Non-nil to highlight AST nodes on mouse-down.")

(defvar js2-mode-fontifications nil "Private variable")
(make-variable-buffer-local 'js2-mode-fontifications)

(defvar js2-mode-deferred-properties nil "Private variable")
(make-variable-buffer-local 'js2-mode-deferred-properties)

(defvar js2-imenu-recorder nil "Private variable")
(make-variable-buffer-local 'js2-imenu-recorder)

(defvar js2-paragraph-start
  "\\(@[a-zA-Z]+\\>\\|$\\)")

;; Note that we also set a 'c-in-sws text property in html comments,
;; so that `c-forward-sws' and `c-backward-sws' work properly.
(defvar js2-syntactic-ws-start
  "\\s \\|/[*/]\\|[\n\r]\\|\\\\[\n\r]\\|\\s!\\|<!--\\|^\\s-*-->")

(defvar js2-syntactic-ws-end
  "\\s \\|[\n\r/]\\|\\s!")

(defvar js2-syntactic-eol
  (concat "\\s *\\(/\\*[^*\n\r]*"
          "\\(\\*+[^*\n\r/][^*\n\r]*\\)*"
          "\\*+/\\s *\\)*"
          "\\(//\\|/\\*[^*\n\r]*"
          "\\(\\*+[^*\n\r/][^*\n\r]*\\)*$"
          "\\|\\\\$\\|$\\)")
  "Copied from java-mode.  Needed for some cc-engine functions.")

(defvar js2-comment-prefix-regexp
  "//+\\|\\**")

(defvar js2-comment-start-skip
  "\\(//+\\|/\\*+\\)\\s *")

(defvar js2-mode-verbose-parse-p js2-mode-dev-mode-p
  "Non-nil to emit status messages during parsing.")

(defvar js2-mode-functions-hidden nil "private variable")
(defvar js2-mode-comments-hidden nil "private variable")

(defvar js2-mode-syntax-table
  (let ((table (make-syntax-table)))
    (c-populate-syntax-table table)
    table)
  "Syntax table used in js2-mode buffers.")

(defvar js2-mode-must-byte-compile (not js2-mode-dev-mode-p)
  "Non-nil to have `js2-mode' signal an error if not byte-compiled.")

(defvar js2-mode-pending-parse-callbacks nil
  "List of functions waiting to be notified that parse is finished.")

(defvar js2-mode-last-indented-line -1)

(eval-when-compile
  (defvar c-paragraph-start nil)
  (defvar c-paragraph-separate nil)
  (defvar c-syntactic-ws-start nil)
  (defvar c-syntactic-ws-end nil)
  (defvar c-syntactic-eol nil)
  (defvar running-xemacs nil))

(eval-when-compile
  (if (< emacs-major-version 22)
      (defun c-setup-paragraph-variables () nil)))

(provide 'js2-vars)

;;; js2-vars.el ends here
;;; js2-util.el -- JavaScript utilities

;; Author:  Steve Yegge (steve.yegge@gmail.com)
;; Keywords:  javascript languages

;;; Code

(eval-when-compile
  (require 'cl))


;; Emacs21 compatibility

(eval-and-compile
  (unless (fboundp #'looking-back)
    (defun looking-back (regexp &optional limit greedy)
      "Return non-nil if text before point matches regular expression REGEXP.
Like `looking-at' except matches before point, and is slower.
LIMIT if non-nil speeds up the search by specifying a minimum
starting position, to avoid checking matches that would start
before LIMIT.

If GREEDY is non-nil, extend the match backwards as far as possible,
stopping when a single additional previous character cannot be part
of a match for REGEXP."
      (let ((start (point))
            (pos
             (save-excursion
               (and (re-search-backward (concat "\\(?:" regexp "\\)\\=") limit t)
                    (point)))))
        (if (and greedy pos)
            (save-restriction
              (narrow-to-region (point-min) start)
              (while (and (> pos (point-min))
                          (save-excursion
                            (goto-char pos)
                            (backward-char 1)
                            (looking-at (concat "\\(?:"  regexp "\\)\\'"))))
                (setq pos (1- pos)))
              (save-excursion
                (goto-char pos)
                (looking-at (concat "\\(?:"  regexp "\\)\\'")))))
        (not (null pos)))))

  (unless (fboundp #'copy-overlay)
    (defun copy-overlay (o)
      "Return a copy of overlay O."
      (let ((o1 (make-overlay (overlay-start o) (overlay-end o)
                              ;; FIXME: there's no easy way to find the
                              ;; insertion-type of the two markers.
                              (overlay-buffer o)))
            (props (overlay-properties o)))
        (while props
          (overlay-put o1 (pop props) (pop props)))
        o1)))

  (unless (fboundp #'remove-overlays)
    (defun remove-overlays (&optional beg end name val)
      "Clear BEG and END of overlays whose property NAME has value VAL.
Overlays might be moved and/or split.
BEG and END default respectively to the beginning and end of buffer."
      (unless beg (setq beg (point-min)))
      (unless end (setq end (point-max)))
      (if (< end beg)
          (setq beg (prog1 end (setq end beg))))
      (save-excursion
        (dolist (o (overlays-in beg end))
          (when (eq (overlay-get o name) val)
            ;; Either push this overlay outside beg...end
            ;; or split it to exclude beg...end
            ;; or delete it entirely (if it is contained in beg...end).
            (if (< (overlay-start o) beg)
                (if (> (overlay-end o) end)
                    (progn
                      (move-overlay (copy-overlay o)
                                    (overlay-start o) beg)
                      (move-overlay o end (overlay-end o)))
                  (move-overlay o (overlay-start o) beg))
              (if (> (overlay-end o) end)
                  (move-overlay o end (overlay-end o))
                (delete-overlay o))))))))

  ;; a version of delete-if that only uses macros from 'cl package
  (unless (fboundp #'delete-if)
    (defun delete-if (predicate list)
      "Remove all items satisfying PREDICATE in LIST."
      (loop for item in list
            if (not (funcall predicate item))
            collect item)))

  ;; ditto
  (unless (fboundp #'position)
    (defun position (element list)
      "Find 0-indexed position of ELEMENT in LIST comparing with `eq'.
Returns nil if element is not found in the list."
      (let ((count 0)
            found)
        (while (and list (not found))
          (if (eq element (car list))
              (setq found t)
            (setq count (1+ count)
                  list (cdr list))))
        (if found count)))))

(defmacro js2-time (form)
  "Evaluate FORM, discard result, and return elapsed time in sec"
  (let ((beg (make-symbol "--js2-time-beg--"))
        (delta (make-symbol "--js2-time-end--")))
    `(let ((,beg (current-time))
           ,delta)
       ,form
       (/ (truncate (* (- (float-time (current-time))
                          (float-time ,beg)))
                    10000)
          10000.0))))

(defsubst neq (expr1 expr2)
  "Return (not (eq expr1 expr2))."
  (not (eq expr1 expr2)))

(defsubst js2-same-line (pos)
  "Return t if POS is on the same line as current point."
  (and (>= pos (point-at-bol))
       (<= pos (point-at-eol))))

(defsubst js2-same-line-2 (p1 p2)
  "Return t if p1 is on the same line as p2."
  (save-excursion
    (goto-char p1)
    (js2-same-line p2)))

(defun js2-code-bug ()
  "Signal an error when we encounter an unexpected code path."
  (error "failed assertion"))

;; I'd like to associate errors with nodes, but for now the
;; easiest thing to do is get the context info from the last token.
(defsubst js2-record-parse-error (msg &optional arg pos len)
  (push (list (list msg arg)
              (or pos js2-token-beg)
              (or len (- js2-token-end js2-token-beg)))
        js2-parsed-errors))

(defsubst js2-report-error (msg &optional msg-arg pos len)
  "Signal a syntax error or record a parse error."
  (if js2-recover-from-parse-errors
      (js2-record-parse-error msg msg-arg pos len)
  (signal 'js2-syntax-error
          (list msg
                js2-ts-lineno
                (save-excursion
                  (goto-char js2-ts-cursor)
                  (current-column))
                js2-ts-hit-eof))))

(defsubst js2-report-warning (msg &optional msg-arg pos len)
  (if js2-compiler-report-warning-as-error
      (js2-report-error msg msg-arg pos len)
    (push (list (list msg msg-arg)
                (or pos js2-token-beg)
                (or len (- js2-token-end js2-token-beg)))
          js2-parsed-warnings)))

(defsubst js2-add-strict-warning (msg-id &optional msg-arg beg end)
  (if js2-compiler-strict-mode
      (js2-report-warning msg-id msg-arg beg
                          (and beg end (- end beg)))))

(put 'js2-syntax-error 'error-conditions
     '(error syntax-error js2-syntax-error))
(put 'js2-syntax-error 'error-message "Syntax error")

(put 'js2-parse-error 'error-conditions
     '(error parse-error js2-parse-error))
(put 'js2-parse-error 'error-message "Parse error")

(defalias 'set-flag 'logior)

(defsubst flag-set-p (flags flag)
  (plusp (logand flags flag)))

;; Stolen shamelessly from James Clark's nxml-mode.
(defmacro js2-with-unmodifying-text-property-changes (&rest body)
  "Evaluate BODY without any text property changes modifying the buffer.
Any text properties changes happen as usual but the changes are not treated as
modifications to the buffer."
  (let ((modified (make-symbol "modified")))
    `(let ((,modified (buffer-modified-p))
	   (inhibit-read-only t)
	   (inhibit-modification-hooks t)
	   (buffer-undo-list t)
	   (deactivate-mark nil)
	   ;; Apparently these avoid file locking problems.
	   (buffer-file-name nil)
	   (buffer-file-truename nil))
       (unwind-protect
	   (progn ,@body)
	 (unless ,modified
	   (restore-buffer-modified-p nil))))))

(put 'js2-with-unmodifying-text-property-changes 'lisp-indent-function 0)
(def-edebug-spec js2-with-unmodifying-text-property-changes t)

(defmacro js2-with-underscore-as-word-syntax (&rest body)
  "Evaluate BODY with the _ character set to be word-syntax."
  (let ((old-syntax (make-symbol "old-syntax")))
  `(let ((,old-syntax (string (char-syntax ?_))))
     (unwind-protect
         (progn
           (modify-syntax-entry ?_ "w" js2-mode-syntax-table)
           ,@body)
       (modify-syntax-entry ?_ ,old-syntax js2-mode-syntax-table)))))

(put 'js2-with-underscore-as-word-syntax 'lisp-indent-function 0)

(provide 'js2-util)

;;; js2-util.el ends here
;;; js2-scan.el --- JavaScript scanner

;; Author:  Steve Yegge (steve.yegge@gmail.com)
;; Keywords:  javascript languages

;;; Commentary:

;; A port of Mozilla Rhino's scanner.
;; Corresponds to Rhino files Token.java and TokenStream.java.

;;; Code:


(eval-when-compile
  (require 'cl))

(defvar js2-tokens nil
  "List of all defined token names.")  ; intialized below

(defvar js2-token-names
  (let* ((names (make-vector js2-num-tokens -1))
         (case-fold-search nil)  ; only match js2-UPPER_CASE
         (syms (apropos-internal "^js2-\\(?:[A-Z_]+\\)")))
    (loop for sym in syms
          for i from 0
          do
          (unless (or (memq sym '(js2-EOF_CHAR js2-ERROR))
                      (not (boundp sym)))
            (aset names (symbol-value sym)         ; code, e.g. 152
                  (substring (symbol-name sym) 4)) ; name, e.g. "LET"
            (push sym js2-tokens)))
    names)
  "Vector mapping int values to token string names, sans `js2-' prefix.")

(defun js2-token-name (tok)
  "Return a string name for TOK, a token symbol or code.
Signals an error if it's not a recognized token."
  (let ((code tok))
    (if (symbolp tok)
        (setq code (symbol-value tok)))
    (if (eq code -1)
        "ERROR"
      (if (and (numberp code)
               (not (minusp code))
               (< code js2-num-tokens))
          (aref js2-token-names code)
        (error "Invalid token: %s" code)))))

(defsubst js2-token-sym (tok)
  "Return symbol for TOK given its code, e.g. 'js2-LP for code 86."
  (intern (js2-token-name tok)))

(defvar js2-token-codes
  (let ((table (make-hash-table :test 'eq :size 256)))
    (loop for name across js2-token-names
          for sym = (intern (concat "js2-" name))
          do
          (puthash sym (symbol-value sym) table))
    ;; clean up a few that are "wrong" in Rhino's token codes
    (puthash 'js2-DELETE js2-DELPROP table)
    table)
  "Hashtable mapping token symbols to their bytecodes.")

(defsubst js2-token-code (sym)
  "Return code for token symbol SYM, e.g. 86 for 'js2-LP."
  (or (gethash sym js2-token-codes)
      (error "Invalid token symbol: %s " sym)))  ; signal code bug

(defsubst js2-report-scan-error (msg &optional no-throw)
  (setq js2-token-end js2-ts-cursor)
  (js2-report-error msg nil js2-token-beg
                    (- js2-token-end js2-token-beg))
  (unless no-throw
    (throw 'return js2-ERROR)))

(defsubst js2-get-string-from-buffer ()
  "Reverse the char accumulator and return it as a string."
  (setq js2-token-end js2-ts-cursor)
  (if js2-ts-string-buffer
      (apply #'string (nreverse js2-ts-string-buffer))
    ""))

;; TODO:  could potentially avoid a lot of consing by allocating a
;; char buffer the way Rhino does.
(defsubst js2-add-to-string (c)
  (push c js2-ts-string-buffer))

;; Note that when we "read" the end-of-file, we advance js2-ts-cursor
;; to (1+ (point-max)), which lets the scanner treat end-of-file like
;; any other character:  when it's not part of the current token, we
;; unget it, allowing it to be read again by the following call.
(defsubst js2-unget-char ()
  (decf js2-ts-cursor))

;; Rhino distinguishes \r and \n line endings.  We don't need to
;; because we only scan from Emacs buffers, which always use \n.
(defsubst js2-get-char ()
  "Read and return the next character from the input buffer.
Increments `js2-ts-lineno' if the return value is a newline char.
Updates `js2-ts-cursor' to the point after the returned char.
Returns `js2-EOF_CHAR' if we hit the end of the buffer.
Also updates `js2-ts-hit-eof' and `js2-ts-line-start' as needed."
  (let (c)
    ;; check for end of buffer
    (if (>= js2-ts-cursor (point-max))
        (setq js2-ts-hit-eof t
              js2-ts-cursor (1+ js2-ts-cursor)
              c js2-EOF_CHAR)  ; return value

      ;; otherwise read next char
      (setq c (char-before (incf js2-ts-cursor)))

      ;; if we read a newline, update counters
      (if (= c ?\n)
          (setq js2-ts-line-start js2-ts-cursor
                js2-ts-lineno (1+ js2-ts-lineno)))

      ;; TODO:  skip over format characters
      c)))

(defsubst js2-read-unicode-escape ()
  "Read a \\uNNNN sequence from the input.
Assumes the ?\ and ?u have already been read.
Returns the unicode character, or nil if it wasn't a valid character.
Doesn't change the values of any scanner variables."
  ;; I really wish I knew a better way to do this, but I can't
  ;; find the Emacs function that takes a 16-bit int and converts
  ;; it to a Unicode/utf-8 character.  So I basically eval it with (read).
  ;; Have to first check that it's 4 hex characters or it may stop
  ;; the read early.
  (ignore-errors
    (let ((s (buffer-substring-no-properties js2-ts-cursor
                                             (+ 4 js2-ts-cursor))))
      (if (string-match "[a-zA-Z0-9]\\{4\\}" s)
          (read (concat "?\\u" s))))))

(defsubst js2-match-char (test)
  "Consume and return next character if it matches TEST, a character.
Returns nil and consumes nothing if TEST is not the next character."
  (let ((c (js2-get-char)))
    (if (eq c test)
        t
      (js2-unget-char)
      nil)))

(defsubst js2-peek-char ()
  (prog1
      (js2-get-char)
    (js2-unget-char)))

(defsubst js2-java-identifier-start-p (c)
  "Implementation of java.lang.Character.isJavaIdentifierStart()"
  ;; TODO:  make me Unicode-friendly.  For speed, make a 64k bit vector
  ;; as is done in jsre.el.
  (or
   (memq c '(?$ ?_))
   (and (>= c ?a) (<= c ?z))
   (and (>= c ?A) (<= c ?Z))))

(defsubst js2-java-identifier-part-p (c)
  "Implementation of java.lang.Character.isJavaIdentifierPart()"
  ;; TODO:  make me Unicode-friendly.  See comments above.
  (or
   (memq c '(?$ ?_))
   (and (>= c ?a) (<= c ?z))
   (and (>= c ?A) (<= c ?Z))
   (and (>= c ?0) (<= c ?9))))

(defsubst js2-alpha-p (c)
  ;; Use 'Z' < 'a'
  (if (<= c ?Z)
      (<= ?A c)
    (and (<= ?a c)
         (<= c ?z))))

(defsubst js2-digit-p (c)
  (and (<= ?0 c) (<= c ?9)))

(defsubst js2-js-space-p (c)
  (if (<= c 127)
      (memq c '(#x20 #x9 #xC #xB))
    (or
     (eq c #xA0)
     ;; TODO:  change this nil to check for Unicode space character
     nil)))

(defsubst js2-skip-line ()
  "Skip to end of line"
  (let (c)
    (while (and (/= js2-EOF_CHAR (setq c (js2-get-char)))
                (/= c ?\n)))
    (setq js2-token-end js2-ts-cursor)
    (js2-unget-char)))

(defun js2-init-scanner (&optional buf line)
  "Create token stream for BUF starting on LINE.
BUF defaults to current-buffer and line defaults to 1.

A buffer can only have one scanner active at a time, which yields
dramatically simpler code than using a defstruct.  If you need to
have simultaneous scanners in a buffer, copy the regions to scan
into temp buffers."
  (save-excursion
    (when buf
      (set-buffer buf))
    (setq js2-ts-dirty-line nil
          js2-ts-regexp-flags nil
          js2-ts-string ""
          js2-ts-number nil
          js2-ts-hit-eof nil
          js2-ts-line-start 0
          js2-ts-lineno (or line 1)
          js2-ts-line-end-char -1
          js2-ts-cursor (point-min)
          js2-ts-is-xml-attribute nil
          js2-ts-xml-is-tag-content nil
          js2-ts-xml-open-tags-count 0
          js2-ts-string-buffer nil)))

;; This function uses the cached op, string and number fields in
;; TokenStream; if getToken has been called since the passed token
;; was scanned, the op or string printed may be incorrect.
(defun js2-token-to-string (token)
  ;; Not sure where this function is used in Rhino.  Not tested.
  (if (not js2-debug-print-trees)
      ""
    (let ((name (js2-token-name token)))
      (cond
       ((memq token (list js2-STRING js2-REGEXP js2-NAME))
        (concat name " `" js2-ts-string "'"))
       ((eq token js2-NUMBER)
        (format "NUMBER %g" js2-ts-number))
       (t
        name)))))

(defconst js2-keywords
  '(break
    case catch const continue
    debugger default delete do
    else enum
    false finally for function
    if in instanceof import
    let
    new null
    return
    switch
    this throw true try typeof
    var void
    while with
    yield))

;; Token names aren't exactly the same as the keywords, unfortunately.
;; E.g. enum isn't in the tokens, and delete is js2-DELPROP.
(defconst js2-kwd-tokens
  (let ((table (make-vector js2-num-tokens nil))
        (tokens
         (list js2-BREAK
               js2-CASE js2-CATCH js2-CONST js2-CONTINUE
               js2-DEBUGGER js2-DEFAULT js2-DELPROP js2-DO
               js2-ELSE
               js2-FALSE js2-FINALLY js2-FOR js2-FUNCTION
               js2-IF js2-IN js2-INSTANCEOF js2-IMPORT
               js2-LET
               js2-NEW js2-NULL
               js2-RETURN
               js2-SWITCH
               js2-THIS js2-THROW js2-TRUE js2-TRY js2-TYPEOF
               js2-VAR
               js2-WHILE js2-WITH
               js2-YIELD)))
    (dolist (i tokens)
      (aset table i 'js2-keyword-face))
    (aset table js2-STRING 'js2-string-face)
    (aset table js2-REGEXP 'js2-regexp-face)
    (aset table js2-COMMENT 'js2-comment-face)
    (aset table js2-THIS 'js2-builtin-face)
    (aset table js2-VOID 'js2-constant-face)
    (aset table js2-NULL 'js2-constant-face)
    (aset table js2-TRUE 'js2-constant-face)
    (aset table js2-FALSE 'js2-constant-face)
    table)
  "Vector whose values are t for tokens that are keywords")

(defconst js2-reserved-words
  '(abstract
    boolean byte
    char class
    double
    enum extends
    final float
    goto
    implements int interface
    long
    native
    package private protected public
    short static super synchronized
    throws transient
    volatile))

(defconst js2-keyword-names
  (let ((table (make-hash-table :test 'equal)))
    (loop for k in js2-keywords
          do (puthash
              (symbol-name k)                            ; instanceof
              (intern (concat "js2-"
                              (upcase (symbol-name k)))) ; js2-INSTANCEOF
              table))
    table)
  "JavaScript keywords by name, mapped to their symbols.")

(defconst js2-reserved-word-names
  (let ((table (make-hash-table :test 'equal)))
    (loop for k in js2-reserved-words
          do
          (puthash (symbol-name k) 'js2-RESERVED table))
    table)
  "JavaScript reserved words by name, mapped to 'js2-RESERVED.")

(defsubst js2-collect-string (buf)
  "Convert BUF, a list of chars, to a string.
Reverses BUF before converting."
  (cond
   ((stringp buf)
    buf)
   ((null buf)  ; for emacs21 compat
    "")
   (t
    (if buf
        (apply #'string (nreverse buf))
      ""))))

(defun js2-string-to-keyword (s)
  "Return token for S, a string, if S is a keyword or reserved word.
Returns a symbol such as 'js2-BREAK, or nil if not keyword/reserved."
  (or (gethash s js2-keyword-names)
      (gethash s js2-reserved-word-names)))

(defsubst js2-ts-set-char-token-bounds ()
  "Used when next token is one character."
  (setq js2-token-beg (1- js2-ts-cursor)
        js2-token-end js2-ts-cursor))

(defsubst js2-ts-return (token)
  "Return an N-character TOKEN from `js2-get-token'.
Updates `js2-token-end' accordingly."
  (setq js2-token-end js2-ts-cursor)
  (throw 'return token))

(defsubst js2-x-digit-to-int (c accumulator)
  "Build up a hex number.
If C is a hexadecimal digit, return ACCUMULATOR * 16 plus
corresponding number.  Otherwise return -1."
  (catch 'return
    (catch 'check
      ;; Use 0..9 < A..Z < a..z
      (cond
       ((<= c ?9)
        (decf c ?0)
        (if (<= 0 c)
            (throw 'check nil)))
       ((<= c ?F)
        (when (<= ?A c)
          (decf c (- ?A 10))
          (throw 'check nil)))
       ((<= c ?f)
        (when (<= ?a c)
          (decf c (- ?a 10))
          (throw 'check nil))))
      (throw 'return -1))
    (logior c (lsh accumulator 4))))

(defun js2-get-token ()
  "Return next JavaScript token, an int such as js2-RETURN."
  (let (c
        c1
        identifier-start
        is-unicode-escape-start
        contains-escape
        escape-val
        escape-start
        str
        result
        base
        is-integer
        num-string
        quote-char
        val
        look-for-slash
        continue)
    (catch 'return
      (while t
        ;; Eat whitespace, possibly sensitive to newlines.
        (setq continue t)
        (while continue
          (setq c (js2-get-char))
          (cond
           ((eq c js2-EOF_CHAR)
            (js2-ts-set-char-token-bounds)
            (throw 'return js2-EOF))
           ((eq c ?\n)
            (js2-ts-set-char-token-bounds)
            (setq js2-ts-dirty-line nil)
            (throw 'return js2-EOL))
           ((not (js2-js-space-p c))
            (if (/= c ?-)               ; in case end of HTML comment
                (setq js2-ts-dirty-line t))
            (setq continue nil))))

        ;; Assume the token will be 1 char - fixed up below.
        (setq js2-token-beg (1- js2-ts-cursor)
              js2-token-end js2-ts-cursor)

        (when (eq c ?@)
          (throw 'return js2-XMLATTR))

        ;; identifier/keyword/instanceof?
        ;; watch out for starting with a <backslash>
        (cond
         ((eq c ?\\)
          (setq c (js2-get-char))
          (if (eq c ?u)
              (setq identifier-start t
                    is-unicode-escape-start t
                    js2-ts-string-buffer nil)
            (setq identifier-start nil)
            (js2-unget-char)
            (setq c ?\\)))
         (t
          (when (setq identifier-start (js2-java-identifier-start-p c))
            (setq js2-ts-string-buffer nil)
            (js2-add-to-string c))))

        (when identifier-start
          (setq contains-escape is-unicode-escape-start)
          (catch 'break
            (while t
              (if is-unicode-escape-start
                  ;; strictly speaking we should probably push-back
                  ;; all the bad characters if the <backslash>uXXXX
                  ;; sequence is malformed. But since there isn't a
                  ;; correct context(is there?) for a bad Unicode
                  ;; escape sequence in an identifier, we can report
                  ;; an error here.
                  (progn
                    (setq escape-val 0)
                    (dotimes (i 4)
                      (setq c (js2-get-char)
                            escape-val (js2-x-digit-to-int c escape-val))
                      ;; Next check takes care of c < 0 and bad escape
                      (if (minusp escape-val)
                          (throw 'break nil)))
                    (if (minusp escape-val)
                        (js2-report-scan-error "msg.invalid.escape" t))
                    (js2-add-to-string escape-val)
                    (setq is-unicode-escape-start nil))
                (setq c (js2-get-char))
                (cond
                 ((eq c ?\\)
                  (setq c (js2-get-char))
                  (if (eq c ?u)
                      (setq is-unicode-escape-start t
                            contains-escape t)
                    (js2-report-scan-error "msg.illegal.character" t)))
                 (t
                  (if (or (eq c js2-EOF_CHAR)
                          (not (js2-java-identifier-part-p c)))
                      (throw 'break nil))
                  (js2-add-to-string c))))))
          (js2-unget-char)

          (setq str (js2-get-string-from-buffer))
          (unless contains-escape
            ;; OPT we shouldn't have to make a string (object!) to
            ;; check if it's a keyword.

            ;; Return the corresponding token if it's a keyword
            (when (setq result (js2-string-to-keyword str))
              (if (and (< js2-language-version 170)
                       (memq result '(js2-LET js2-YIELD)))
                  ;; LET and YIELD are tokens only in 1.7 and later
                  (setq result 'js2-NAME))
              (if (neq result js2-RESERVED)
                  (throw 'return (js2-token-code result)))
              (js2-report-warning "msg.reserved.keyword" str)))

          ;; If we want to intern these as Rhino does, just use (intern str)
          (setq js2-ts-string str)
          (throw 'return js2-NAME))     ; end identifier/kwd check

        ;; is it a number?
        (when (or (js2-digit-p c)
                  (and (eq c ?.) (js2-digit-p (js2-peek-char))))
          (setq js2-ts-string-buffer nil
                base 10)
          (when (eq c ?0)
            (setq c (js2-get-char))
            (cond
             ((or (eq c ?x) (eq c ?X))
              (setq base 16)
              (setq c (js2-get-char)))
             ((js2-digit-p c)
              (setq base 8))
             (t
              (js2-add-to-string ?0))))

          (if (eq base 16)
              (while (<= 0 (js2-x-digit-to-int c 0))
                (js2-add-to-string c)
                (setq c (js2-get-char)))
            (while (and (<= ?0 c) (<= c ?9))
              ;; We permit 08 and 09 as decimal numbers, which
              ;; makes our behavior a superset of the ECMA
              ;; numeric grammar.  We might not always be so
              ;; permissive, so we warn about it.
              (when (and (eq base 8) (>= c ?8))
                (js2-report-warning "msg.bad.octal.literal"
                                    (if (eq c ?8) "8" "9"))
                (setq base 10))
              (js2-add-to-string c)
              (setq c (js2-get-char))))

          (setq is-integer t)

          (when (and (eq base 10) (memq c '(?. ?e ?E)))
            (setq is-integer nil)
            (when (eq c ?.)
              (loop do
                    (js2-add-to-string c)
                    (setq c (js2-get-char))
                    while (js2-digit-p c)))
            (when (memq c '(?e ?E))
              (js2-add-to-string c)
              (setq c (js2-get-char))
              (when (memq c '(?+ ?-))
                (js2-add-to-string c)
                (setq c (js2-get-char)))
              (unless (js2-digit-p c)
                (js2-report-scan-error "msg.missing.exponent" t))
              (loop do
                    (js2-add-to-string c)
                    (setq c (js2-get-char))
                    while (js2-digit-p c))))

          (js2-unget-char)
          (setq num-string (js2-get-string-from-buffer)
                js2-ts-number
                (if (and (eq base 10) (not is-integer))
                    (string-to-number num-string)
                  ;; TODO:  call runtime number-parser.  Some of it is in
                  ;; js2-util.el, but I need to port ScriptRuntime.stringToNumber.
                  (string-to-number num-string)))
          (throw 'return js2-NUMBER))

        ;; is it a string?
        (when (memq c '(?\" ?\'))
          ;; We attempt to accumulate a string the fast way, by
          ;; building it directly out of the reader.  But if there
          ;; are any escaped characters in the string, we revert to
          ;; building it out of a string buffer.
          (setq quote-char c
                js2-ts-string-buffer nil
                c (js2-get-char))
          (catch 'break
            (while (/= c quote-char)
              (catch 'continue
                (when (or (eq c ?\n) (eq c js2-EOF_CHAR))
                  (js2-unget-char)
                  (setq js2-token-end js2-ts-cursor)
                  (js2-report-error "msg.unterminated.string.lit")
                  (throw 'return js2-STRING))

                (when (eq c ?\\)
                  ;; We've hit an escaped character
                  (setq c (js2-get-char))
                  (case c
                    (?b (setq c ?\b))
                    (?f (setq c ?\f))
                    (?n (setq c ?\n))
                    (?r (setq c ?\r))
                    (?t (setq c ?\t))
                    (?v (setq c ?\v))
                    (?u
                     (setq c1 (js2-read-unicode-escape))
                     (if js2-parse-ide-mode
                         (if c1
                             (progn
                               ;; just copy the string in IDE-mode
                               (js2-add-to-string ?\\)
                               (js2-add-to-string ?u)
                               (dotimes (i 3)
                                 (js2-add-to-string (js2-get-char)))
                               (setq c (js2-get-char))) ; added at end of loop
                           ;; flag it as an invalid escape
                           (js2-report-warning "msg.invalid.escape"
                                               nil (- js2-ts-cursor 2) 6))
                       ;; Get 4 hex digits; if the u escape is not
                       ;; followed by 4 hex digits, use 'u' + the
                       ;; literal character sequence that follows.
                       (js2-add-to-string ?u)
                       (setq escape-val 0)
                       (dotimes (i 4)
                         (setq c (js2-get-char)
                               escape-val (js2-x-digit-to-int c escape-val))
                         (if (minusp escape-val)
                             (throw 'continue nil))
                         (js2-add-to-string c))
                       ;; prepare for replace of stored 'u' sequence by escape value
                       (setq js2-ts-string-buffer (nthcdr 5 js2-ts-string-buffer)
                             c escape-val)))
                    (?x
                     ;; Get 2 hex digits, defaulting to 'x'+literal
                     ;; sequence, as above.
                     (setq c (js2-get-char)
                           escape-val (js2-x-digit-to-int c 0))
                     (if (minusp escape-val)
                         (progn
                           (js2-add-to-string ?x)
                           (throw 'continue nil))
                       (setq c1 c
                             c (js2-get-char)
                             escape-val (js2-x-digit-to-int c escape-val))
                       (if (minusp escape-val)
                           (progn
                             (js2-add-to-string ?x)
                             (js2-add-to-string c1)
                             (throw 'continue nil))
                         ;; got 2 hex digits
                         (setq c escape-val))))
                    (?\n
                     ;; Remove line terminator after escape to follow
                     ;; SpiderMonkey and C/C++
                     (setq c (js2-get-char))
                     (throw 'continue nil))
                    (t
                     (when (and (<= ?0 c) (< c ?8))
                       (setq val (- c ?0)
                             c (js2-get-char))
                       (when (and (<= ?0 c) (< c ?8))
                         (setq val (- (+ (* 8 val) c) ?0)
                               c (js2-get-char))
                         (when (and (<= ?0 c)
                                    (< c ?8)
                                    (< val #o37))
                           ;; c is 3rd char of octal sequence only
                           ;; if the resulting val <= 0377
                           (setq val (- (+ (* 8 val) c) ?0)
                                 c (js2-get-char))))
                       (js2-unget-char)
                       (setq c val)))))
                (js2-add-to-string c)
                (setq c (js2-get-char)))))
          (setq js2-ts-string (js2-get-string-from-buffer))
          (throw 'return js2-STRING))

        (case c
          (?\;
           (throw 'return js2-SEMI))
          (?\[
           (throw 'return js2-LB))
          (?\]
           (throw 'return js2-RB))
          (?{
           (throw 'return js2-LC))
          (?}
           (throw 'return js2-RC))
          (?\(
           (throw 'return js2-LP))
          (?\)
           (throw 'return js2-RP))
          (?,
           (throw 'return js2-COMMA))
          (??
           (throw 'return js2-HOOK))
          (?:
           (if (js2-match-char ?:)
               (js2-ts-return js2-COLONCOLON)
             (throw 'return js2-COLON)))
          (?.
           (if (js2-match-char ?.)
               (js2-ts-return js2-DOTDOT)
             (if (js2-match-char ?\()
                 (js2-ts-return js2-DOTQUERY)
               (throw 'return js2-DOT))))
          (?|
           (if (js2-match-char ?|)
               (throw 'return js2-OR)
             (if (js2-match-char ?=)
                 (js2-ts-return js2-ASSIGN_BITOR)
               (throw 'return js2-BITOR))))
          (?^
           (if (js2-match-char ?=)
               (js2-ts-return js2-ASSIGN_BITOR)
             (throw 'return js2-BITXOR)))
          (?&
           (if (js2-match-char ?&)
               (throw 'return js2-AND)
             (if (js2-match-char ?=)
                 (js2-ts-return js2-ASSIGN_BITAND)
               (throw 'return js2-BITAND))))
          (?=
           (if (js2-match-char ?=)
               (if (js2-match-char ?=)
                   (js2-ts-return js2-SHEQ)
                 (throw 'return js2-EQ))
             (throw 'return js2-ASSIGN)))
          (?!
           (if (js2-match-char ?=)
               (if (js2-match-char ?=)
                   (js2-ts-return js2-SHNE)
                 (js2-ts-return js2-NE))
             (throw 'return js2-NOT)))
          (?<
           ;; NB:treat HTML begin-comment as comment-till-eol
           (when (js2-match-char ?!)
             (when (js2-match-char ?-)
               (when (js2-match-char ?-)
                 (js2-skip-line)
                 (setq js2-ts-comment-type 'html)
                 (throw 'return js2-COMMENT)))
             (js2-unget-char))

           (if (js2-match-char ?<)
               (if (js2-match-char ?=)
                   (js2-ts-return js2-ASSIGN_LSH)
                 (js2-ts-return js2-LSH))
             (if (js2-match-char ?=)
                 (js2-ts-return js2-LE)
               (throw 'return js2-LT))))
          (?>
           (if (js2-match-char ?>)
               (if (js2-match-char ?>)
                   (if (js2-match-char ?=)
                       (js2-ts-return js2-ASSIGN_URSH)
                     (js2-ts-return js2-URSH))
                 (if (js2-match-char ?=)
                     (js2-ts-return js2-ASSIGN_RSH)
                   (js2-ts-return js2-RSH)))
             (if (js2-match-char ?=)
                 (js2-ts-return js2-GE)
               (throw 'return js2-GT))))
          (?*
           (if (js2-match-char ?=)
               (js2-ts-return js2-ASSIGN_MUL)
             (throw 'return js2-MUL)))

          (?/
           ;; is it a // comment?
           (when (js2-match-char ?/)
             (setq js2-token-beg (- js2-ts-cursor 2))
             (js2-skip-line)
             (setq js2-ts-comment-type 'line
                   js2-token-end js2-ts-cursor)
             (throw 'return js2-COMMENT))

           ;; is it a /* comment?
           (when (js2-match-char ?*)
             (setq look-for-slash nil
                   js2-token-beg (- js2-ts-cursor 2)
                   js2-ts-comment-type
                   (if (js2-match-char ?*)
                       'jsdoc
                     'block))
             (while t
               (setq c (js2-get-char))
               (cond
                ((eq c js2-EOF_CHAR)
                 (setq js2-token-end (1- js2-ts-cursor))
                 (js2-report-error "msg.unterminated.comment")
                 (throw 'return js2-COMMENT))
                ((eq c ?*)
                 (setq look-for-slash t))
                ((eq c ?/)
                 (if look-for-slash
                   (js2-ts-return js2-COMMENT)))
                (t
                 (setq look-for-slash nil
                       js2-token-end js2-ts-cursor)))))

           (if (js2-match-char ?=)
               (js2-ts-return js2-ASSIGN_DIV)
             (throw 'return js2-DIV)))

          (?%
           (if (js2-match-char ?=)
               (js2-ts-return js2-ASSIGN_MOD)
             (throw 'return js2-MOD)))
          (?~
           (throw 'return js2-BITNOT))
          (?+
           (if (js2-match-char ?=)
               (js2-ts-return js2-ASSIGN_ADD)
             (if (js2-match-char ?+)
                 (js2-ts-return js2-INC)
               (throw 'return js2-ADD))))
          (?-
           (cond
            ((js2-match-char ?=)
             (setq c js2-ASSIGN_SUB))
            ((js2-match-char ?-)
             (unless js2-ts-dirty-line
               ;; treat HTML end-comment after possible whitespace
               ;; after line start as comment-until-eol
               (when (js2-match-char ?>)
                 (js2-skip-line)
                 (setq js2-ts-comment-type 'html)
                 (throw 'return js2-COMMENT)))
             (setq c js2-DEC))
            (t
             (setq c js2-SUB)))
           (setq js2-ts-dirty-line t)
           (js2-ts-return c))

          (otherwise
           (js2-report-scan-error "msg.illegal.character")))))))

(defun js2-read-regexp (start-token)
  "Called by parser when it gets / or /= in literal context."
  (let (c
        err
        (continue t))
    (setq js2-token-beg js2-ts-cursor
          js2-ts-string-buffer nil
          js2-ts-regexp-flags nil)
    (if (eq start-token js2-ASSIGN_DIV)
        ;; mis-scanned /=
        (js2-add-to-string ?=)
      (if (neq start-token js2-DIV)
          (error "failed assertion")))
    (while (and (not err) (/= (setq c (js2-get-char)) ?/))
      (if (or (eq c ?\n) (eq c js2-EOF_CHAR))
          (progn
            (setq js2-token-end (1- js2-ts-cursor)
                  err t
                  js2-ts-string (js2-collect-string js2-ts-string-buffer))
            (js2-report-error "msg.unterminated.re.lit"))
        (when (eq c ?\\)
          (js2-add-to-string c)
          (setq c (js2-get-char)))
        (js2-add-to-string c)))
    (unless err
      (while continue
        (cond
         ((js2-match-char ?g)
          (push ?g js2-ts-regexp-flags))
         ((js2-match-char ?i)
          (push ?i js2-ts-regexp-flags))
         ((js2-match-char ?m)
          (push ?m js2-ts-regexp-flags))
         (t
          (setq continue nil))))
      (if (js2-alpha-p (js2-peek-char))
          (js2-report-scan-error "msg.invalid.re.flag" t))
      (setq js2-ts-string (js2-collect-string js2-ts-string-buffer)
            js2-ts-regexp-flags (js2-collect-string js2-ts-regexp-flags)
            js2-token-end js2-ts-cursor))))

(defun js2-get-first-xml-token ()
  (setq js2-ts-xml-open-tags-count 0
        js2-ts-is-xml-attribute nil
        js2-ts-xml-is-tag-content nil)
  (js2-unget-char)
  (js2-get-next-xml-token))

(defsubst js2-xml-discard-string ()
  "Throw away the string in progress and flag an XML parse error."
  (setq js2-ts-string-buffer nil
        js2-ts-string nil)
  (js2-report-scan-error "msg.XML.bad.form" t))

(defun js2-get-next-xml-token ()
  (setq js2-ts-string-buffer nil  ; for recording the XML
        js2-token-beg js2-ts-cursor)
  (loop for c = (js2-get-char)
        while (/= c js2-EOF_CHAR)
        do
        (if js2-ts-xml-is-tag-content
            (progn
              (case c
                (?>
                 (js2-add-to-string c)
                 (setq js2-ts-xml-is-tag-content nil
                       js2-ts-is-xml-attribute nil))
                (?/
                 (js2-add-to-string c)
                 (when (eq ?> (js2-peek-char))
                   (setq c (js2-get-char))
                   (js2-add-to-string c)
                   (setq js2-ts-xml-is-tag-content nil)
                   (decf js2-ts-xml-open-tags-count)))
                (?{
                 (js2-unget-char)
                 (setq js2-ts-string (js2-get-string-from-buffer))
                 (return js2-XML))
                ((?\' ?\")
                 (js2-add-to-string c)
                 (unless (js2-read-quoted-string c)
                   (return js2-ERROR)))
                (?=
                 (js2-add-to-string c)
                 (setq js2-ts-is-xml-attribute t))
                ((? ?\t ?\r ?\n)
                 (js2-add-to-string c))
                (t
                 (js2-add-to-string c)
                 (setq js2-ts-is-xml-attribute nil)))
              (when (and (not js2-ts-xml-is-tag-content)
                         (zerop js2-ts-xml-open-tags-count))
                (setq js2-ts-string (js2-get-string-from-buffer))
                (return js2-XMLEND)))
          ;; else not tag content
          (case c
            (?<
             (js2-add-to-string c)
             (setq c (js2-peek-char))
             (case c
               (?!
                (setq c (js2-get-char))  ;; skip !
                (js2-add-to-string c)
                (setq c (js2-peek-char))
                (case c
                  (?-
                   (setq c (js2-get-char))  ;; skip -
                   (js2-add-to-string c)
                   (if (eq c ?-)
                       (progn
                         (js2-add-to-string c)
                         (unless (js2-read-xml-comment)
                           (return js2-ERROR)))
                     (js2-xml-discard-string)
                     (return js2-ERROR)))
                  (?\[
                   (setq c (js2-get-char))  ;; skip [
                   (js2-add-to-string c)
                   (if (and (= (js2-get-char) ?C)
                            (= (js2-get-char) ?D)
                            (= (js2-get-char) ?A)
                            (= (js2-get-char) ?T)
                            (= (js2-get-char) ?A)
                            (= (js2-get-char) ?\[))
                       (progn
                         (js2-add-to-string ?C)
                         (js2-add-to-string ?D)
                         (js2-add-to-string ?A)
                         (js2-add-to-string ?T)
                         (js2-add-to-string ?A)
                         (js2-add-to-string ?\[)
                         (unless (js2-read-cdata)
                           (return js2-ERROR)))
                     (js2-xml-discard-string)
                     (return js2-ERROR)))
                  (t
                   (unless (js2-read-entity)
                     (return js2-ERROR)))))
               (??
                (setq c (js2-get-char))  ;; skip ?
                (js2-add-to-string c)
                (unless (js2-read-PI)
                  (return js2-ERROR)))
               (?/
                ;; end tag
                (setq c (js2-get-char))  ;; skip /
                (js2-add-to-string c)
                (when (zerop js2-ts-xml-open-tags-count)
                  (js2-xml-discard-string)
                  (return js2-ERROR))
                (setq js2-ts-xml-is-tag-content t)
                (decf js2-ts-xml-open-tags-count))
               (t
                ;; start tag
                (setq js2-ts-xml-is-tag-content t)
                (incf js2-ts-xml-open-tags-count))))
            (?{
             (js2-unget-char)
             (setq js2-ts-string (js2-get-string-from-buffer))
             (return js2-XML))
            (t
             (js2-add-to-string c))))
        finally  ; seemingly not triggered?
        (setq js2-token-end js2-ts-cursor))) ; doesn't affect return value

(defun js2-read-quoted-string (quote)
  (let (c)
    (catch 'return
      (while (/= (setq c (js2-get-char)) js2-EOF_CHAR)
        (js2-add-to-string c)
        (if (eq c quote)
            (throw 'return t)))
      (js2-xml-discard-string)  ;; throw away string in progress
      nil)))

(defun js2-read-xml-comment ()
  (let ((c (js2-get-char)))
    (catch 'return
      (while (/= c js2-EOF_CHAR)
        (catch 'continue
          (js2-add-to-string c)
          (when (and (eq c ?-) (eq ?- (js2-peek-char)))
            (setq c (js2-get-char))
            (js2-add-to-string c)
            (if (eq (js2-peek-char) ?>)
                (progn
                  (setq c (js2-get-char)) ;; skip >
                  (js2-add-to-string c)
                  (throw 'return t))
              (throw 'continue nil)))
          (setq c (js2-get-char))))
      (js2-xml-discard-string)
      nil)))

(defun js2-read-cdata ()
  (let ((c (js2-get-char)))
    (catch 'return
      (while (/= c js2-EOF_CHAR)
        (catch 'continue
          (js2-add-to-string c)
          (when (and (eq c ?\]) (eq (js2-peek-char) ?\]))
            (setq c (js2-get-char))
            (js2-add-to-string c)
            (if (eq (js2-peek-char) ?>)
                (progn
                  (setq c (js2-get-char)) ;; Skip >
                  (js2-add-to-string c)
                  (throw 'return t))
              (throw 'continue nil)))
          (setq c (js2-get-char))))
      (js2-xml-discard-string)
      nil)))

(defun js2-read-entity ()
  (let ((decl-tags 1)
        c)
    (catch 'return
      (while (/= js2-EOF_CHAR (setq c (js2-get-char)))
        (js2-add-to-string c)
        (case c
          (?<
           (incf decl-tags))
          (?>
           (decf decl-tags)
           (if (zerop decl-tags)
               (throw 'return t)))))
      (js2-xml-discard-string)
      nil)))

(defun js2-read-PI ()
  "Scan an XML processing instruction."
  (let (c)
    (catch 'return
      (while (/= js2-EOF_CHAR (setq c (js2-get-char)))
        (js2-add-to-string c)
        (when (and (eq c ??) (eq (js2-peek-char) ?>))
          (setq c (js2-get-char))  ;; Skip >
          (js2-add-to-string c)
          (throw 'return t)))
      (js2-xml-discard-string)
      nil)))

(defun js2-scanner-get-line ()
  "Return the text of the current scan line."
  (buffer-substring (point-at-bol) (point-at-eol)))

(provide 'js2-scan)

;;; js2-scan.el ends here
;;; js2-messages:  localizable messages for js2-mode

;; Author:  Steve Yegge (steve.yegge@gmail.com)
;; Keywords:  javascript languages

;;; Commentary:

;; Messages are copied from Rhino's Messages.properties.
;; Many of the Java-specific messages have been elided.
;; Add any js2-specific ones at the end, so we can keep
;; this file synced with changes to Rhino's.
;;
;; TODO:
;;  - move interpreter messages into separate file

;;; Code:

(defvar js2-message-table
  (make-hash-table :test 'equal :size 250)
  "Contains localized messages for js2-mode.")

;; TODO:  construct this hashtable at compile-time.
(defmacro js2-msg (key &rest strings)
  `(puthash ,key (funcall #'concat ,@strings)
            js2-message-table))

(defun js2-get-msg (msg-key)
  "Look up a localized message.
MSG-KEY is a list of (MSG ARGS).  If the message takes parameters,
the correct number of ARGS must be provided."
  (let* ((key (if (listp msg-key) (car msg-key) msg-key))
         (args (if (listp msg-key) (cdr msg-key)))
         (msg (gethash key js2-message-table)))
    (if msg
        (apply #'format msg args)
      key)))  ; default to showing the key

(js2-msg "msg.dup.parms"
         "Duplicate parameter name '%s'.")

(js2-msg "msg.too.big.jump"
         "Program too complex: jump offset too big.")

(js2-msg "msg.too.big.index"
         "Program too complex: internal index exceeds 64K limit.")

(js2-msg "msg.while.compiling.fn"
         "Encountered code generation error while compiling function '%s': %s")

(js2-msg "msg.while.compiling.script"
         "Encountered code generation error while compiling script: %s")

;; Context
(js2-msg "msg.ctor.not.found"
         "Constructor for '%s' not found.")

(js2-msg "msg.not.ctor"
         "'%s' is not a constructor.")

;; FunctionObject
(js2-msg "msg.varargs.ctor"
         "Method or constructor '%s' must be static "
         "with the signature (Context cx, Object[] args, "
         "Function ctorObj, boolean inNewExpr) "
         "to define a variable arguments constructor.")

(js2-msg "msg.varargs.fun"
         "Method '%s' must be static with the signature "
         "(Context cx, Scriptable thisObj, Object[] args, Function funObj) "
         "to define a variable arguments function.")

(js2-msg "msg.incompat.call"
         "Method '%s' called on incompatible object.")

(js2-msg "msg.bad.parms"
         "Unsupported parameter type '%s' in method '%s'.")

(js2-msg "msg.bad.method.return"
         "Unsupported return type '%s' in method '%s'.")

(js2-msg "msg.bad.ctor.return"
         "Construction of objects of type '%s' is not supported.")

(js2-msg "msg.no.overload"
         "Method '%s' occurs multiple times in class '%s'.")

(js2-msg "msg.method.not.found"
         "Method '%s' not found in '%s'.")

;; IRFactory

(js2-msg "msg.bad.for.in.lhs"
         "Invalid left-hand side of for..in loop.")

(js2-msg "msg.mult.index"
         "Only one variable allowed in for..in loop.")

(js2-msg "msg.bad.for.in.destruct"
         "Left hand side of for..in loop must be an array of "
         "length 2 to accept key/value pair.")

(js2-msg "msg.cant.convert"
         "Can't convert to type '%s'.")

(js2-msg "msg.bad.assign.left"
         "Invalid assignment left-hand side.")

(js2-msg "msg.bad.decr"
         "Invalid decerement operand.")

(js2-msg "msg.bad.incr"
         "Invalid increment operand.")

(js2-msg "msg.bad.yield"
         "yield must be in a function.")

(js2-msg "msg.yield.parenthesized"
         "yield expression must be parenthesized.")

;; NativeGlobal
(js2-msg "msg.cant.call.indirect"
          "Function '%s' must be called directly, and not by way of a "
          "function of another name.")

(js2-msg "msg.eval.nonstring"
          "Calling eval() with anything other than a primitive "
          "string value will simply return the value. "
          "Is this what you intended?")

(js2-msg "msg.eval.nonstring.strict"
         "Calling eval() with anything other than a primitive "
         "string value is not allowed in strict mode.")

(js2-msg "msg.bad.destruct.op"
         "Invalid destructuring assignment operator")

;; NativeCall
(js2-msg "msg.only.from.new"
         "'%s' may only be invoked from a `new' expression.")

(js2-msg "msg.deprec.ctor"
         "The '%s' constructor is deprecated.")

;; NativeFunction
(js2-msg "msg.no.function.ref.found"
         "no source found to decompile function reference %s")

(js2-msg "msg.arg.isnt.array"
         "second argument to Function.prototype.apply must be an array")

;; NativeGlobal
(js2-msg "msg.bad.esc.mask"
         "invalid string escape mask")

;; NativeRegExp
(js2-msg "msg.bad.quant"
  "Invalid quantifier %s")

(js2-msg "msg.overlarge.backref"
  "Overly large back reference %s")

(js2-msg "msg.overlarge.min"
  "Overly large minimum %s")

(js2-msg "msg.overlarge.max"
  "Overly large maximum %s")

(js2-msg "msg.zero.quant"
  "Zero quantifier %s")

(js2-msg "msg.max.lt.min"
  "Maximum %s less than minimum")

(js2-msg "msg.unterm.quant"
  "Unterminated quantifier %s")

(js2-msg "msg.unterm.paren"
  "Unterminated parenthetical %s")

(js2-msg "msg.unterm.class"
  "Unterminated character class %s")

(js2-msg "msg.bad.range"
  "Invalid range in character class.")

(js2-msg "msg.trail.backslash"
  "Trailing \\ in regular expression.")

(js2-msg "msg.re.unmatched.right.paren"
  "unmatched ) in regular expression.")

(js2-msg "msg.no.regexp"
  "Regular expressions are not available.")

(js2-msg "msg.bad.backref"
  "back-reference exceeds number of capturing parentheses.")

(js2-msg "msg.bad.regexp.compile"
         "Only one argument may be specified if the first "
         "argument to RegExp.prototype.compile is a RegExp object.")

;; Parser
(js2-msg "msg.got.syntax.errors"
         "Compilation produced %s syntax errors.")

(js2-msg "msg.var.redecl"
         "TypeError: redeclaration of var %s.")

(js2-msg "msg.const.redecl"
         "TypeError: redeclaration of const %s.")

(js2-msg "msg.let.redecl"
         "TypeError: redeclaration of variable %s.")

(js2-msg "msg.parm.redecl"
         "TypeError: redeclaration of formal parameter %s.")

(js2-msg "msg.fn.redecl"
         "TypeError: redeclaration of function %s.")

;; NodeTransformer
(js2-msg "msg.dup.label"
         "duplicated label")

(js2-msg "msg.undef.label"
         "undefined label")

(js2-msg "msg.bad.break"
         "unlabelled break must be inside loop or switch")

(js2-msg "msg.continue.outside"
         "continue must be inside loop")

(js2-msg "msg.continue.nonloop"
         "continue can only use labels of iteration statements")

(js2-msg "msg.bad.throw.eol"
         "Line terminator is not allowed between the throw "
         "keyword and throw expression.")

(js2-msg "msg.no.paren.parms"
         "missing ( before function parameters.")

(js2-msg "msg.no.parm"
         "missing formal parameter")

(js2-msg "msg.no.paren.after.parms"
         "missing ) after formal parameters")

(js2-msg "msg.no.brace.body"
         "missing '{' before function body")

(js2-msg "msg.no.brace.after.body"
         "missing } after function body")

(js2-msg "msg.no.paren.cond"
         "missing ( before condition")

(js2-msg "msg.no.paren.after.cond"
         "missing ) after condition")

(js2-msg "msg.no.semi.stmt"
         "missing ; before statement")

(js2-msg "msg.missing.semi"
         "missing ; after statement")

(js2-msg "msg.no.name.after.dot"
         "missing name after . operator")

(js2-msg "msg.no.name.after.coloncolon"
         "missing name after :: operator")

(js2-msg "msg.no.name.after.dotdot"
         "missing name after .. operator")

(js2-msg "msg.no.name.after.xmlAttr"
         "missing name after .@")

(js2-msg "msg.no.bracket.index"
         "missing ] in index expression")

(js2-msg "msg.no.paren.switch"
         "missing ( before switch expression")

(js2-msg "msg.no.paren.after.switch"
         "missing ) after switch expression")

(js2-msg "msg.no.brace.switch"
         "missing '{' before switch body")

(js2-msg "msg.bad.switch"
         "invalid switch statement")

(js2-msg "msg.no.colon.case"
         "missing : after case expression")

(js2-msg "msg.double.switch.default"
         "double default label in the switch statement")

(js2-msg "msg.no.while.do"
         "missing while after do-loop body")

(js2-msg "msg.no.paren.for"
         "missing ( after for")

(js2-msg "msg.no.semi.for"
         "missing ; after for-loop initializer")

(js2-msg "msg.no.semi.for.cond"
         "missing ; after for-loop condition")

(js2-msg "msg.in.after.for.name"
         "missing in after for")

(js2-msg "msg.no.paren.for.ctrl"
         "missing ) after for-loop control")

(js2-msg "msg.no.paren.with"
         "missing ( before with-statement object")

(js2-msg "msg.no.paren.after.with"
         "missing ) after with-statement object")

(js2-msg "msg.no.paren.after.let"
         "missing ( after let")

(js2-msg "msg.no.paren.let"
         "missing ) after variable list")

(js2-msg "msg.no.curly.let"
         "missing } after let statement")

(js2-msg "msg.bad.return"
         "invalid return")

(js2-msg "msg.no.brace.block"
         "missing } in compound statement")

(js2-msg "msg.bad.label"
         "invalid label")

(js2-msg "msg.bad.var"
         "missing variable name")

(js2-msg "msg.bad.var.init"
         "invalid variable initialization")

(js2-msg "msg.no.colon.cond"
         "missing : in conditional expression")

(js2-msg "msg.no.paren.arg"
         "missing ) after argument list")

(js2-msg "msg.no.bracket.arg"
         "missing ] after element list")

(js2-msg "msg.bad.prop"
         "invalid property id")

(js2-msg "msg.no.colon.prop"
         "missing : after property id")

(js2-msg "msg.no.brace.prop"
         "missing } after property list")

(js2-msg "msg.no.paren"
         "missing ) in parenthetical")

(js2-msg "msg.reserved.id"
         "identifier is a reserved word")

(js2-msg "msg.no.paren.catch"
         "missing ( before catch-block condition")

(js2-msg "msg.bad.catchcond"
         "invalid catch block condition")

(js2-msg "msg.catch.unreachable"
         "any catch clauses following an unqualified catch are unreachable")

(js2-msg "msg.no.brace.try"
         "missing '{' before try block")

(js2-msg "msg.no.brace.catchblock"
         "missing '{' before catch-block body")

(js2-msg "msg.try.no.catchfinally"
         "'try' without 'catch' or 'finally'")

(js2-msg "msg.no.return.value"
         "function %s does not always return a value")

(js2-msg "msg.anon.no.return.value"
         "anonymous function does not always return a value")

(js2-msg "msg.return.inconsistent"
         "return statement is inconsistent with previous usage")

(js2-msg "msg.generator.returns"
         "TypeError: generator function '%s' returns a value")

(js2-msg "msg.anon.generator.returns"
         "TypeError: anonymous generator function returns a value")

(js2-msg "msg.syntax"
         "syntax error")

(js2-msg "msg.unexpected.eof"
         "Unexpected end of file")

(js2-msg "msg.XML.bad.form"
         "illegally formed XML syntax")

(js2-msg "msg.XML.not.available"
         "XML runtime not available")

(js2-msg "msg.too.deep.parser.recursion"
         "Too deep recursion while parsing")

(js2-msg "msg.no.side.effects"
         "Code has no side effects")

(js2-msg "msg.extra.trailing.comma"
         "Trailing comma is not legal in an ECMA-262 object initializer")

(js2-msg "msg.array.trailing.comma"
         "Trailing comma yields different behavior across browsers")

(js2-msg "msg.equal.as.assign"
         "Test for equality (==) mistyped as assignment (=)?")

(js2-msg "msg.var.hides.arg"
         "Variable %s hides argument")

(js2-msg "msg.destruct.assign.no.init"
         "Missing = in destructuring declaration")

;; ScriptRuntime
(js2-msg "msg.no.properties"
         "%s has no properties.")

(js2-msg "msg.invalid.iterator"
         "Invalid iterator value")

(js2-msg "msg.iterator.primitive"
         "__iterator__ returned a primitive value")

(js2-msg "msg.assn.create.strict"
         "Assignment to undeclared variable %s")

(js2-msg "msg.ref.undefined.prop"
         "Reference to undefined property '%s'")

(js2-msg "msg.prop.not.found"
         "Property %s not found.")

(js2-msg "msg.invalid.type"
         "Invalid JavaScript value of type %s")

(js2-msg "msg.primitive.expected"
         "Primitive type expected (had %s instead)")

(js2-msg "msg.namespace.expected"
         "Namespace object expected to left of :: (found %s instead)")

(js2-msg "msg.null.to.object"
         "Cannot convert null to an object.")

(js2-msg "msg.undef.to.object"
         "Cannot convert undefined to an object.")

(js2-msg "msg.cyclic.value"
         "Cyclic %s value not allowed.")

(js2-msg "msg.is.not.defined"
         "'%s' is not defined.")

(js2-msg "msg.undef.prop.read"
         "Cannot read property '%s' from %s")

(js2-msg "msg.undef.prop.write"
         "Cannot set property '%s' of %s to '%s'")

(js2-msg "msg.undef.prop.delete"
         "Cannot delete property '%s' of %s")

(js2-msg "msg.undef.method.call"
         "Cannot call method '%s' of %s")

(js2-msg "msg.undef.with"
         "Cannot apply 'with' to %s")

(js2-msg "msg.isnt.function"
         "%s is not a function, it is %s.")

(js2-msg "msg.isnt.function.in"
         "Cannot call property %s in object %s. "
         "It is not a function, it is '%s'.")

(js2-msg "msg.function.not.found"
         "Cannot find function %s.")

(js2-msg "msg.function.not.found.in"
         "Cannot find function %s in object %s.")

(js2-msg "msg.isnt.xml.object"
         "%s is not an xml object.")

(js2-msg "msg.no.ref.to.get"
         "%s is not a reference to read reference value.")

(js2-msg "msg.no.ref.to.set"
         "%s is not a reference to set reference value to %s.")

(js2-msg "msg.no.ref.from.function"
         "Function %s can not be used as the left-hand "
         "side of assignment or as an operand of ++ or -- operator.")

(js2-msg "msg.bad.default.value"
         "Object's getDefaultValue() method returned an object.")

(js2-msg "msg.instanceof.not.object"
         "Can't use instanceof on a non-object.")

(js2-msg "msg.instanceof.bad.prototype"
         "'prototype' property of %s is not an object.")

(js2-msg "msg.bad.radix"
         "illegal radix %s.")

;; ScriptableObject
(js2-msg "msg.default.value"
         "Cannot find default value for object.")

(js2-msg "msg.zero.arg.ctor"
         "Cannot load class '%s' which has no zero-parameter constructor.")

(js2-msg "msg.ctor.multiple.parms"
         "Can't define constructor or class %s since more than "
         "one constructor has multiple parameters.")

(js2-msg "msg.extend.scriptable"
         "%s must extend ScriptableObject in order to define property %s.")

(js2-msg "msg.bad.getter.parms"
         "In order to define a property, getter %s must have zero "
         "parameters or a single ScriptableObject parameter.")

(js2-msg "msg.obj.getter.parms"
         "Expected static or delegated getter %s to take "
         "a ScriptableObject parameter.")

(js2-msg "msg.getter.static"
         "Getter and setter must both be static or neither be static.")

(js2-msg "msg.setter.return"
         "Setter must have void return type: %s")

(js2-msg "msg.setter2.parms"
         "Two-parameter setter must take a ScriptableObject as "
         "its first parameter.")

(js2-msg "msg.setter1.parms"
         "Expected single parameter setter for %s")

(js2-msg "msg.setter2.expected"
         "Expected static or delegated setter %s to take two parameters.")

(js2-msg "msg.setter.parms"
         "Expected either one or two parameters for setter.")

(js2-msg "msg.setter.bad.type"
         "Unsupported parameter type '%s' in setter '%s'.")

(js2-msg "msg.add.sealed"
         "Cannot add a property to a sealed object: %s.")

(js2-msg "msg.remove.sealed"
         "Cannot remove a property from a sealed object: %s.")

(js2-msg "msg.modify.sealed"
         "Cannot modify a property of a sealed object: %s.")

(js2-msg "msg.modify.readonly"
         "Cannot modify readonly property: %s.")

;; TokenStream
(js2-msg "msg.missing.exponent"
         "missing exponent")

(js2-msg "msg.caught.nfe"
         "number format error")

(js2-msg "msg.unterminated.string.lit"
         "unterminated string literal")

(js2-msg "msg.unterminated.comment"
         "unterminated comment")

(js2-msg "msg.unterminated.re.lit"
         "unterminated regular expression literal")

(js2-msg "msg.invalid.re.flag"
         "invalid flag after regular expression")

(js2-msg "msg.no.re.input.for"
         "no input for %s")

(js2-msg "msg.illegal.character"
         "illegal character")

(js2-msg "msg.invalid.escape"
         "invalid Unicode escape sequence")

(js2-msg "msg.bad.namespace"
         "not a valid default namespace statement. "
         "Syntax is: default xml namespace = EXPRESSION;")

;; TokensStream warnings
(js2-msg "msg.bad.octal.literal"
         "illegal octal literal digit %s; "
         "interpreting it as a decimal digit")

(js2-msg "msg.reserved.keyword"
         "illegal usage of future reserved keyword %s; "
         "interpreting it as ordinary identifier")

(js2-msg "msg.script.is.not.constructor"
         "Script objects are not constructors.")

;; Arrays
(js2-msg "msg.arraylength.bad"
         "Inappropriate array length.")

;; Arrays
(js2-msg "msg.arraylength.too.big"
         "Array length %s exceeds supported capacity limit.")

;; URI
(js2-msg "msg.bad.uri"
         "Malformed URI sequence.")

;; Number
(js2-msg "msg.bad.precision"
         "Precision %s out of range.")

;; NativeGenerator
(js2-msg "msg.send.newborn"
         "Attempt to send value to newborn generator")

(js2-msg "msg.already.exec.gen"
         "Already executing generator")

(js2-msg "msg.StopIteration.invalid"
         "StopIteration may not be changed to an arbitrary object.")

;; Interpreter
(js2-msg "msg.yield.closing"
         "Yield from closing generator")

(provide 'js2-messages)
;;; js2-ast.el --- JavaScript syntax tree node definitions

;; Author:  Steve Yegge (steve.yegge@gmail.com)
;; Keywords:  javascript languages

;;; Code:

(eval-and-compile
  (require 'cl))


;; flags for ast node property 'member-type (used for e4x operators)
(defvar js2-property-flag    #x1 "property access: element is valid name")
(defvar js2-attribute-flag   #x2 "x.@y or x..@y")
(defvar js2-descendants-flag #x4 "x..y or x..@i")

(defsubst js2-relpos (pos anchor)
  "Convert POS to be relative to ANCHOR.
If POS is nil, returns nil."
  (and pos (- pos anchor)))

(defsubst js2-make-pad (indent)
  (if (zerop indent)
      ""
    (make-string (* indent js2-basic-offset) ? )))

(defsubst js2-visit-ast (node callback)
  "Visit every node in ast NODE with visitor CALLBACK.

CALLBACK is a function that takes two arguments:  (NODE END-P).  It is
called twice:  once to visit the node, and again after all the node's
children have been processed.  The END-P argument is nil on the first
call and non-nil on the second call.  The return value of the callback
affects the traversal:  if non-nil, the children of NODE are processed.
If the callback returns nil, or if the node has no children, then the
callback is called immediately with a non-nil END-P argument.

The node traversal is approximately lexical-order, although there
are currently no guarantees around this."
  (let ((vfunc (get (aref node 0) 'js2-visitor)))
    ;; visit the node
    (when  (funcall callback node nil)
      ;; visit the kids
      (cond
       ((eq vfunc 'js2-visit-none)
        nil)                            ; don't even bother calling it
       ;; Each AST node type has to define a `js2-visitor' function
       ;; that takes a node and a callback, and calls `js2-visit-ast'
       ;; on each child of the node.
       (vfunc
        (funcall vfunc node callback))
       (t
        (error "%s does not define a visitor-traversal function"
               (aref node 0)))))
    ;; call the end-visit
    (funcall callback node t)))

(defstruct (js2-node
            (:constructor nil))  ; abstract
  "Base AST node type."
  (type -1)  ; token type
  (pos -1)   ; start position of this AST node in parsed input
  (len 1)    ; num characters spanned by the node
  props      ; optional node property list (an alist)
  parent)    ; link to parent node; null for root

(defsubst js2-node-get-prop (node prop)
  (assoc prop (js2-node-props node)))

(defsubst js2-node-set-prop (node prop value)
  (setf (js2-node-props node)
        (cons (list prop value) (js2-node-props node))))

(defsubst js2-fixup-starts (n nodes)
  "Adjust the start positions of NODES to be relative to N.
Any node in the list may be nil, for convenience."
  (dolist (node nodes)
    (when node
      (setf (js2-node-pos node) (- (js2-node-pos node)
                                   (js2-node-pos n))))))

(defsubst js2-node-add-children (parent &rest nodes)
  "Set parent node of NODES to PARENT.
Does nothing if we're not recording parent links.
If any given node in NODES is nil, doesn't record that link."
  (js2-fixup-starts parent nodes)
  (dolist (node nodes)
    (and node
         (setf (js2-node-parent node) parent))))

;; Non-recursive since it's called a frightening number of times.
(defsubst js2-node-abs-pos (n)
  (let ((pos (js2-node-pos n)))
    (while (setq n (js2-node-parent n))
      (setq pos (+ pos (js2-node-pos n))))
    pos))

(defsubst js2-node-abs-end (n)
  "Return absolute buffer position of end of N."
  (+ (js2-node-abs-pos n) (js2-node-len n)))

(defstruct (js2-stmt-node
            (:include js2-node)
            (:constructor nil))  ; abstract
  "Abstract supertype of (most) statement nodes.")

(defstruct (js2-expr-node
            (:include js2-node)
            (:constructor nil))  ; abstract
  "Abstract supertype of (most) expression nodes.")

(defstruct (js2-error-node
            (:include js2-node)
            (:constructor nil) ; silence emacs21 byte-compiler
            (:constructor make-js2-error-node (&key (type js2-ERROR)
                                                    (pos js2-token-beg)
                                                    len)))
  "AST node representing a parse error.")

(put 'cl-struct-js2-error-node 'js2-visitor 'js2-visit-none)
(put 'cl-struct-js2-error-node 'js2-printer 'js2-print-none)

;; It's important to make sure block nodes have a lisp list for the
;; child nodes, to limit printing recursion depth in an AST that
;; otherwise consists of defstruct vectors.  Emacs will crash printing
;; a sufficiently large vector tree.

(defstruct (js2-block-node
            (:include js2-stmt-node)
            (:constructor nil)
            (:constructor make-js2-block-node (&key (type js2-BLOCK)
                                                    (pos js2-token-beg)
                                                    len
                                                    props
                                                    kids)))
  "A block of statements."
  scope  ; a `js2-scope'
  kids)  ; a lisp list of the child statement nodes

(put 'cl-struct-js2-block-node 'js2-visitor 'js2-visit-block)
(put 'cl-struct-js2-block-node 'js2-printer 'js2-print-block)

(defsubst js2-visit-block (ast callback)
  "Visit the `js2-block-node' children of AST."
  (dolist (kid (js2-block-node-kids ast))
    (js2-visit-ast kid callback)))

(defun js2-print-block (n i)
  (let ((pad (js2-make-pad i)))
    (insert pad "{\n")
    (dolist (kid (js2-block-node-kids n))
      (js2-print-ast kid (1+ i)))
    (insert pad "}")))

(defstruct (js2-script-node
            (:include js2-block-node)
            (:constructor nil)
            (:constructor make-js2-script-node (&key (type js2-SCRIPT)
                                                     (pos js2-token-beg)
                                                     len
                                                     var-decls
                                                     fun-decls)))
  functions   ; lisp list of nested functions
  regexps     ; lisp list of (string . flags)
  symbols     ; alist (every symbol gets unique index)
  (param-count 0)
  var-names   ; vector of string names
  consts      ; bool-vector matching var-decls
  (temp-number 0))  ; for generating temp variables

(put 'cl-struct-js2-script-node 'js2-visitor 'js2-visit-block)
(put 'cl-struct-js2-script-node 'js2-printer 'js2-print-script)

(defun js2-print-script (node indent)
  (dolist (kid (js2-block-node-kids node))
    (js2-print-ast kid indent)))

(defstruct (js2-ast-root
            (:include js2-script-node)
            (:constructor nil)
            (:constructor make-js2-ast-root (&key (type js2-SCRIPT)
                                                  (pos js2-token-beg)
                                                  len
                                                  buffer)))
  "The root node of a js2 AST."
  buffer         ; the source buffer from which the code was parsed
  comments       ; a lisp list of comments, ordered by start position
  errors         ; a lisp list of errors found during parsing
  warnings       ; a lisp list of warnings found during parsing
  node-count)    ; number of nodes in the tree, including the root

(put 'cl-struct-js2-ast-root 'js2-visitor 'js2-visit-ast-root)
(put 'cl-struct-js2-ast-root 'js2-printer 'js2-print-script)

(defun js2-visit-ast-root (ast callback)
  (dolist (kid (js2-ast-root-kids ast))
    (js2-visit-ast kid callback))
  (dolist (comment (js2-ast-root-comments ast))
    (js2-visit-ast comment callback)))

(defstruct (js2-comment-node
            (:include js2-node)
            (:constructor nil)
            (:constructor make-js2-comment-node (&key (type js2-COMMENT)
                                                      (pos js2-token-beg)
                                                      len
                                                      (format js2-ts-comment-type))))
  format)  ; 'line, 'block, 'jsdoc or 'html

(put 'cl-struct-js2-comment-node 'js2-visitor 'js2-visit-none)
(put 'cl-struct-js2-comment-node 'js2-printer 'js2-print-comment)

(defun js2-print-comment (n i)
  ;; We really ought to link end-of-line comments to their nodes.
  ;; Or maybe we could add a new comment type, 'endline.
  (insert (js2-make-pad i)
          (js2-node-string n)))

(defstruct (js2-expr-stmt-node
            (:include js2-stmt-node)
            (:constructor nil)
            (:constructor make-js2-expr-stmt-node (&key (type js2-EXPR_RESULT)
                                                        (pos js2-ts-cursor)
                                                        len
                                                        expr)))
  "An expression statement."
  expr)

(put 'cl-struct-js2-expr-stmt-node 'js2-visitor 'js2-visit-expr-stmt-node)
(put 'cl-struct-js2-expr-stmt-node 'js2-printer 'js2-print-expr-stmt-node)

(defun js2-visit-expr-stmt-node (n v)
  (js2-visit-ast (js2-expr-stmt-node-expr n) v))

(defun js2-print-expr-stmt-node (n indent)
  (js2-print-ast (js2-expr-stmt-node-expr n) indent)
  (insert ";\n"))

(defstruct (js2-loop-node
            (:include js2-stmt-node)
            (:constructor nil))
  "Abstract supertype of loop nodes."
  label     ; optional `js2-labeled-stmt-node'
  body      ; a `js2-block-node'
  scope     ; a `js2-scope'
  lp        ; position of left-paren, nil if omitted
  rp)       ; position of right-paren, nil if omitted

(defstruct (js2-do-node
            (:include js2-loop-node)
            (:constructor nil)
            (:constructor make-js2-do-node (&key (type js2-DO)
                                                 (pos js2-token-beg)
                                                 len
                                                 label
                                                 body
                                                 condition
                                                 while-pos
                                                 lp
                                                 rp)))
  "AST node for do-loop."
  condition  ; while (expression)
  while-pos) ; buffer position of 'while' keyword

(put 'cl-struct-js2-do-node 'js2-visitor 'js2-visit-do-node)
(put 'cl-struct-js2-do-node 'js2-printer 'js2-print-do-node)

(defun js2-visit-do-node (n v)
  (js2-visit-ast (js2-do-node-body n) v)
  (js2-visit-ast (js2-do-node-condition n) v))

(defun js2-print-do-node (n i)
  (let ((pad (js2-make-pad i)))
    (insert pad "do {\n")
    (dolist (kid (js2-block-node-kids (js2-do-node-body n)))
      (js2-print-ast kid (1+ i)))
    (insert pad "} while (")
    (js2-print-ast (js2-do-node-condition n) 0)
    (insert ");\n")))

(defstruct (js2-while-node
            (:include js2-loop-node)
            (:constructor nil)
            (:constructor make-js2-while-node (&key (type js2-WHILE)
                                                    (pos js2-token-beg)
                                                    len
                                                    label
                                                    body
                                                    condition
                                                    lp
                                                    rp)))
  "AST node for while-loop."
  condition)    ; while-condition

(put 'cl-struct-js2-while-node 'js2-visitor 'js2-visit-while-node)
(put 'cl-struct-js2-while-node 'js2-printer 'js2-print-while-node)

(defun js2-visit-while-node (n v)
  (js2-visit-ast (js2-while-node-condition n) v)
  (js2-visit-ast (js2-while-node-body n) v))

(defun js2-print-while-node (n i)
  (let ((pad (js2-make-pad i)))
    (insert pad "while (")
    (js2-print-ast (js2-while-node-condition n) 0)
    (insert ") {\n")
    (js2-print-body (js2-while-node-body n) (1+ i))
    (insert pad "}\n")))

(defstruct (js2-for-node
            (:include js2-loop-node)
            (:constructor nil)
            (:constructor make-js2-for-node (&key (type js2-FOR)
                                                  (pos js2-ts-cursor)
                                                  len
                                                  label
                                                  body
                                                  init
                                                  condition
                                                  update
                                                  lp
                                                  rp)))
  "AST node for a C-style for-loop."
  init       ; initialization expression
  condition  ; loop condition
  update)    ; update clause

(put 'cl-struct-js2-for-node 'js2-visitor 'js2-visit-for-node)
(put 'cl-struct-js2-for-node 'js2-printer 'js2-print-for-node)

(defun js2-visit-for-node (n v)
  (js2-visit-ast (js2-for-node-init n) v)
  (js2-visit-ast (js2-for-node-condition n) v)
  (js2-visit-ast (js2-for-node-update n) v)
  (js2-visit-ast (js2-for-node-body n) v))

(defun js2-print-for-node (n i)
  (let ((pad (js2-make-pad i)))
    (insert pad "for (")
    (js2-print-ast (js2-for-node-init n) 0)
    (insert "; ")
    (js2-print-ast (js2-for-node-condition n) 0)
    (insert "; ")
    (js2-print-ast (js2-for-node-update n) 0)
    (insert ") {\n")
    (js2-print-body (js2-for-node-body n) (1+ i))
    (insert pad "}\n")))

(defstruct (js2-for-in-node
            (:include js2-loop-node)
            (:constructor nil)
            (:constructor make-js2-for-in-node (&key (type js2-FOR)
                                                     (pos js2-ts-cursor)
                                                     len
                                                     label
                                                     body
                                                     iterator
                                                     object
                                                     in-pos
                                                     each-pos
                                                     foreach-p
                                                     lp
                                                     rp)))
  "AST node for a for..in loop."
  iterator  ; [var] foo in ...
  object    ; object over which we're iterating
  in-pos    ; buffer position of 'in' keyword
  each-pos  ; buffer position of 'each' keyword, if foreach-p
  foreach-p) ; t if it's a for-each loop

(put 'cl-struct-js2-for-in-node 'js2-visitor 'js2-visit-for-in-node)
(put 'cl-struct-js2-for-in-node 'js2-printer 'js2-print-for-in-node)

(defun js2-visit-for-in-node (n v)
  (js2-visit-ast (js2-for-in-node-iterator n) v)
  (js2-visit-ast (js2-for-in-node-object n) v)
  (js2-visit-ast (js2-for-in-node-body n) v))

(defun js2-print-for-in-node (n i)
  (let ((pad (js2-make-pad i))
        (foreach (js2-for-in-node-foreach-p n)))
    (insert pad "for ")
    (if foreach
        (insert "each "))
    (insert "(")
    (js2-print-ast (js2-for-in-node-iterator n) 0)
    (insert " in ")
    (js2-print-ast (js2-for-in-node-object n) 0)
    (insert ") {\n")
    (js2-print-body (js2-for-in-node-body n) (1+ i))
    (insert pad "}\n")))

(defstruct (js2-return-node
            (:include js2-stmt-node)
            (:constructor nil)
            (:constructor make-js2-return-node (&key (type js2-RETURN)
                                                     (pos js2-ts-cursor)
                                                     len
                                                     retval)))
  "AST node for a return statement."
  retval)  ; expression to return, or 'undefined

(put 'cl-struct-js2-return-node 'js2-visitor 'js2-visit-return-node)
(put 'cl-struct-js2-return-node 'js2-printer 'js2-print-return-node)

(defun js2-visit-return-node (n v)
  (if (js2-return-node-retval n)
      (js2-visit-ast (js2-return-node-retval n) v)))

(defun js2-print-return-node (n i)
  (insert (js2-make-pad i) "return")
  (when (js2-return-node-retval n)
    (insert " ")
    (js2-print-ast (js2-return-node-retval n) 0))
  (insert ";\n"))

(defstruct (js2-if-node
            (:include js2-stmt-node)
            (:constructor nil)
            (:constructor make-js2-if-node (&key (type js2-IF)
                                                 (pos js2-ts-cursor)
                                                 len
                                                 condition
                                                 then-part
                                                 else-pos
                                                 else-part
                                                 lp
                                                 rp)))
  "AST node for an if-statement."
  condition   ; expression
  then-part   ; statement or block
  else-pos    ; optional buffer position of 'else' keyword
  else-part   ; optional statement or block
  lp          ; position of left-paren, nil if omitted
  rp)         ; position of right-paren, nil if omitted

(put 'cl-struct-js2-if-node 'js2-visitor 'js2-visit-if-node)
(put 'cl-struct-js2-if-node 'js2-printer 'js2-print-if-node)

(defun js2-visit-if-node (n v)
  (js2-visit-ast (js2-if-node-condition n) v)
  (js2-visit-ast (js2-if-node-then-part n) v)
  (if (js2-if-node-else-part n)
      (js2-visit-ast (js2-if-node-else-part n) v)))

(defun js2-print-if-node (n i)
  (let ((pad (js2-make-pad i))
        (then-part (js2-if-node-then-part n))
        (else-part (js2-if-node-else-part n)))
    (insert pad "if (")
    (js2-print-ast (js2-if-node-condition n) 0)
    (insert ") {\n")
    (js2-print-body then-part (1+ i))
    (insert pad "}")
    (cond
     ((not else-part)
      (insert "\n"))
     ((js2-if-node-p else-part)
      (insert " else ")
      (js2-print-body else-part i))
     (t
      (insert " else {\n")
      (js2-print-body else-part (1+ i))
      (insert pad "}\n")))))

(defstruct (js2-try-node
            (:include js2-stmt-node)
            (:constructor nil)
            (:constructor make-js2-try-node (&key (type js2-TRY)
                                                  (pos js2-ts-cursor)
                                                  len
                                                  try-block
                                                  catch-clauses
                                                  finally-block)))
  "AST node for a try-statement."
  try-block
  catch-clauses  ; a lisp list of js2-catch-node
  finally-block) ; a `js2-finally-node'

(put 'cl-struct-js2-try-node 'js2-visitor 'js2-visit-try-node)
(put 'cl-struct-js2-try-node 'js2-printer 'js2-print-try-node)

(defun js2-visit-try-node (n v)
  (js2-visit-ast (js2-try-node-try-block n) v)
  (dolist (clause (js2-try-node-catch-clauses n))
    (js2-visit-ast clause v))
  (if (js2-try-node-finally-block n)
      (js2-visit-ast (js2-try-node-finally-block n) v)))

(defun js2-print-try-node (n i)
  (let ((pad (js2-make-pad i))
        (catches (js2-try-node-catch-clauses n))
        (finally (js2-try-node-finally-block n)))
    (insert pad "try {\n")
    (js2-print-body (js2-try-node-try-block n) (1+ i))
    (insert pad "}")
    (when catches
      (dolist (catch catches)
        (js2-print-ast catch i)))
    (if finally
        (js2-print-ast finally i)
      (insert "\n"))))

(defstruct (js2-catch-node
            (:include js2-stmt-node)
            (:constructor nil)
            (:constructor make-js2-catch-node (&key (type js2-CATCH)
                                                    (pos js2-ts-cursor)
                                                    len
                                                    var-name
                                                    guard-kwd
                                                    guard-expr
                                                    block
                                                    lp
                                                    rp)))
  "AST node for a catch clause."
  var-name    ; a `js2-name-node'
  guard-kwd   ; relative buffer position of "if" in "catch (x if ...)"
  guard-expr  ; catch condition, a `js2-expr-node'
  block       ; statements, a `js2-block-node'
  lp          ; buffer position of left-paren, nil if omitted
  rp)         ; buffer position of right-paren, nil if omitted

(put 'cl-struct-js2-catch-node 'js2-visitor 'js2-visit-catch-node)
(put 'cl-struct-js2-catch-node 'js2-printer 'js2-print-catch-node)

(defun js2-visit-catch-node (n v)
  (js2-visit-ast (js2-catch-node-var-name n) v)
  (when (js2-catch-node-guard-kwd n)
    (js2-visit-ast (js2-catch-node-guard-expr n) v))
  (js2-visit-ast (js2-catch-node-block n) v))

(defun js2-print-catch-node (n i)
  (let ((pad (js2-make-pad i))
        (guard-kwd (js2-catch-node-guard-kwd n))
        (guard-expr (js2-catch-node-guard-expr n)))
    (insert " catch (")
    (js2-print-ast (js2-catch-node-var-name n) 0)
    (when guard-kwd
      (insert " if ")
      (js2-print-ast guard-expr 0))
    (insert ") {\n")
    (js2-print-body (js2-catch-node-block n) (1+ i))
    (insert pad "}")))

(defstruct (js2-finally-node
            (:include js2-stmt-node)
            (:constructor nil)
            (:constructor make-js2-finally-node (&key (type js2-FINALLY)
                                                      (pos js2-ts-cursor)
                                                      len
                                                      block)))
  "AST node for a finally clause."
  block)

(put 'cl-struct-js2-finally-node 'js2-visitor 'js2-visit-finally-node)
(put 'cl-struct-js2-finally-node 'js2-printer 'js2-print-finally-node)

(defun js2-visit-finally-node (n v)
  (js2-visit-ast (js2-finally-node-block n) v))

(defun js2-print-finally-node (n i)
  (let ((pad (js2-make-pad i)))
    (insert " finally {\n")
    (js2-print-body (js2-finally-node-block n) (1+ i))
    (insert pad "}\n")))

(defstruct (js2-switch-node
            (:include js2-stmt-node)
            (:constructor nil)
            (:constructor make-js2-switch-node (&key (type js2-SWITCH)
                                                     (pos js2-ts-cursor)
                                                     len
                                                     discriminant
                                                     cases
                                                     lp
                                                     rp)))
  "AST node for a switch statement."
  discriminant
  cases  ; a lisp list of `js2-case-node'
  lp     ; position of open-paren for discriminant, nil if omitted
  rp)    ; position of close-paren for discriminant, nil if omitted

(put 'cl-struct-js2-switch-node 'js2-visitor 'js2-visit-switch-node)
(put 'cl-struct-js2-switch-node 'js2-printer 'js2-print-switch-node)

(defun js2-visit-switch-node (n v)
  (js2-visit-ast (js2-switch-node-discriminant n) v)
  (dolist (c (js2-switch-node-cases n))
    (js2-visit-ast c v)))

(defun js2-print-switch-node (n i)
  (let ((pad (js2-make-pad i))
        (cases (js2-switch-node-cases n)))
    (insert pad "switch (")
    (js2-print-ast (js2-switch-node-discriminant n) 0)
    (insert ") {\n")
    (dolist (case cases)
      (js2-print-ast case i))
    (insert pad "}\n")))

(defstruct (js2-case-node
            (:include js2-block-node)
            (:constructor nil)
            (:constructor make-js2-case-node (&key (type js2-CASE)
                                                   (pos js2-ts-cursor)
                                                   len
                                                   kids
                                                   expr)))
  "AST node for a case clause of a switch statement."
  expr)   ; the case expression (nil for default)

(put 'cl-struct-js2-case-node 'js2-visitor 'js2-visit-case-node)
(put 'cl-struct-js2-case-node 'js2-printer 'js2-print-case-node)

(defun js2-visit-case-node (n v)
  (if (js2-case-node-expr n)  ; nil for default: case
      (js2-visit-ast (js2-case-node-expr n) v))
  (js2-visit-block n v))

(defun js2-print-case-node (n i)
  (let ((pad (js2-make-pad i))
        (expr (js2-case-node-expr n)))
    (insert pad)
    (if (null expr)
        (insert "default:\n")
      (insert "case ")
      (js2-print-ast expr 0)
      (insert ":\n"))
    (dolist (kid (js2-case-node-kids n))
      (js2-print-ast kid (1+ i)))))

(defstruct (js2-throw-node
            (:include js2-stmt-node)
            (:constructor nil)
            (:constructor make-js2-throw-node (&key (type js2-THROW)
                                                    (pos js2-ts-cursor)
                                                    len
                                                    expr)))
  "AST node for a throw statement."
  expr)   ; the expression to throw

(put 'cl-struct-js2-throw-node 'js2-visitor 'js2-visit-throw-node)
(put 'cl-struct-js2-throw-node 'js2-printer 'js2-print-throw-node)

(defun js2-visit-throw-node (n v)
  (js2-visit-ast (js2-throw-node-expr n) v))

(defun js2-print-throw-node (n i)
  (insert (js2-make-pad i) "throw ")
  (js2-print-ast (js2-throw-node-expr n) 0)
  (insert ";\n"))

(defstruct (js2-with-node
            (:include js2-stmt-node)
            (:constructor nil)
            (:constructor make-js2-with-node (&key (type js2-WITH)
                                                   (pos js2-ts-cursor)
                                                   len
                                                   object
                                                   body
                                                   lp
                                                   rp)))
  "AST node for a with-statement."
  object
  body
  lp    ; buffer position of left-paren around object, nil if omitted
  rp)   ; buffer position of right-paren around object, nil if omitted

(put 'cl-struct-js2-with-node 'js2-visitor 'js2-visit-with-node)
(put 'cl-struct-js2-with-node 'js2-printer 'js2-print-with-node)

(defun js2-visit-with-node (n v)
  (js2-visit-ast (js2-with-node-object n) v)
  (js2-visit-ast (js2-with-node-body n) v))

(defun js2-print-with-node (n i)
  (let ((pad (js2-make-pad i)))
    (insert pad "with (")
    (js2-print-ast (js2-with-node-object n) 0)
    (insert ") {\n")
    (js2-print-body (js2-with-node-body n) (1+ i))
    (insert pad "}\n")))

(defstruct (js2-label-node
            (:include js2-node)
            (:constructor nil)
            (:constructor make-js2-label-node (&key (type js2-LABEL)
                                                    (pos js2-ts-cursor)
                                                    len
                                                    label)))
  "AST node for a statement label or case label."
  label)  ; a string

(put 'cl-struct-js2-label-node 'js2-visitor 'js2-visit-none)
(put 'cl-struct-js2-label-node 'js2-printer 'js2-print-label)

(defun js2-print-label (n i)
  (insert (js2-make-pad i)
          (js2-label-node-label n)
          ":\n"))

(defstruct (js2-labeled-stmt-node
            (:include js2-stmt-node)
            (:constructor nil)
            ;; type needs to be in `js2-side-effecting-tokens' to avoid spurious
            ;; no-side-effects warnings
            (:constructor make-js2-labeled-stmt-node (&key (type js2-EXPR_RESULT)
                                                           (pos js2-ts-cursor)
                                                           len
                                                           labels
                                                           stmt)))
  "AST node for a statement with one or more labels.
Multiple labels for a statement are collapsed into the labels field."
  labels  ; lisp list of `js2-label-node'
  stmt)   ; the statement these labels are for

(put 'cl-struct-js2-labeled-stmt-node 'js2-visitor 'js2-visit-labeled-stmt)
(put 'cl-struct-js2-labeled-stmt-node 'js2-printer 'js2-print-labeled-stmt)

(defun js2-visit-labeled-stmt (n v)
  (dolist (label (js2-labeled-stmt-node-labels n))
    (js2-visit-ast label v))
  (js2-visit-ast (js2-labeled-stmt-node-stmt n) v))

(defun js2-print-labeled-stmt (n i)
  (dolist (label (js2-labeled-stmt-node-labels n))
    (js2-print-ast label i))
  (js2-print-ast (js2-labeled-stmt-node-stmt n) (1+ i)))

(defun js2-labeled-stmt-node-contains (node label)
  "Return t if NODE contains LABEL in its label set.
NODE is a `js2-labels-node'.  LABEL is an identifier."
  (loop for nl in (js2-labeled-stmt-node-labels node)
        if (string= label (js2-label-node-label nl))
          return t
        finally return nil))

(defsubst js2-labeled-stmt-node-add-label (node label)
  "Add a `js2-label-node' to the label set for this statement."
  (setf (js2-labeled-stmt-node-labels node)
        (nconc (js2-labeled-stmt-node-labels node) (list label))))

(defstruct (js2-jump-node
            (:include js2-stmt-node)
            (:constructor nil))
  "Abstract supertype of break and continue nodes."
  label   ; `js2-name-node' for location of label identifier, if present
  target) ; target js2-labels-node or loop/switch statement

(defun js2-visit-jump-node (n v)
  ;; we don't visit the target, since it's a back-link
  (if (js2-jump-node-label n)
      (js2-visit-ast (js2-jump-node-label n) v)))

(defstruct (js2-break-node
            (:include js2-jump-node)
            (:constructor nil)
            (:constructor make-js2-break-node (&key (type js2-BREAK)
                                                    (pos js2-ts-cursor)
                                                    len
                                                    label
                                                    target)))
  "AST node for a break statement.
The label field is a `js2-name-node', possibly nil, for the named label
if provided.  E.g. in 'break foo', it represents 'foo'.  The target field
is the target of the break - a label node or enclosing loop/switch statement.")

(put 'cl-struct-js2-break-node 'js2-visitor 'js2-visit-jump-node)
(put 'cl-struct-js2-break-node 'js2-printer 'js2-print-break-node)

(defun js2-print-break-node (n i)
  (insert (js2-make-pad i) "break")
  (when (js2-break-node-label n)
    (insert " ")
    (js2-print-ast (js2-break-node-label n) 0))
  (insert ";\n"))

(defstruct (js2-continue-node
            (:include js2-jump-node)
            (:constructor nil)
            (:constructor make-js2-continue-node (&key (type js2-CONTINUE)
                                                       (pos js2-ts-cursor)
                                                       len
                                                       label
                                                       target)))
  "AST node for a continue statement.
The label field is the user-supplied enclosing label name, a `js2-name-node'.
It is nil if continue specifies no label.  The target field is the jump target:
a `js2-label-node' or the innermost enclosing loop.")

(put 'cl-struct-js2-continue-node 'js2-visitor 'js2-visit-jump-node)
(put 'cl-struct-js2-continue-node 'js2-printer 'js2-print-continue-node)

(defun js2-print-continue-node (n i)
  (insert (js2-make-pad i) "continue")
  (when (js2-continue-node-label n)
    (insert " ")
    (js2-print-ast (js2-continue-node-label n) 0))
  (insert ";\n"))

(defstruct (js2-function-node
            (:include js2-script-node)
            (:constructor nil)
            (:constructor make-js2-function-node (&key (type js2-FUNCTION)
                                                       (pos js2-ts-cursor)
                                                       len
                                                       (ftype 'FUNCTION)
                                                       (form 'STATEMENT_FORM)
                                                       (name "")
                                                       params
                                                       body
                                                       lp
                                                       rp)))
  "AST node for a function declaration.
The `params' field is a lisp list of nodes.  Each node is either a simple
`js2-name-node', or if it's a destructuring-assignment parameter, a
`js2-array-node' or `js2-object-node'."
  ftype            ; FUNCTION, GETTER or SETTER
  form             ; DECLARED_FORM, STATEMENT_FORM or EXPRESSED_FORM
  name             ; function name (a `js2-name-node', or nil if anonymous)
  params           ; a lisp list of `js2-function-arg-node'
  body             ; a `js2-block-node'
  lp               ; position of arg-list open-paren, or nil if omitted
  rp               ; position of arg-list close-paren, or nil if omitted
  needs-activation ; t if we need an activation object for this frame
  is-generator)    ; t if this function contains a yield

(put 'cl-struct-js2-function-node 'js2-visitor 'js2-visit-function-node)
(put 'cl-struct-js2-function-node 'js2-printer 'js2-print-function-node)

(defun js2-visit-function-node (n v)
  (if (js2-function-node-name n)
      (js2-visit-ast (js2-function-node-name n) v))
  (dolist (p (js2-function-node-params n))
    (js2-visit-ast p v))
  (js2-visit-ast (js2-function-node-body n) v))

(defun js2-print-function-node (n i)
  (let ((pad (js2-make-pad i))
        (getter (js2-node-get-prop n 'GETTER_SETTER))
        (name (js2-function-node-name n))
        (params (js2-function-node-params n))
        (body (js2-function-node-body n))
        (expr (eq (js2-function-node-form n) 'FUNCTION_EXPRESSION)))
    (unless getter
      (insert pad "function"))
    (when name
        (insert " ")
        (js2-print-ast name 0))
    (insert "(")
    (loop with len = (length params)
          for param in params
          for count from 1
          do
          (js2-print-ast param 0)
          (if (< count len)
              (insert ", ")))
    (insert ") {")
    (unless expr
      (insert "\n"))
    ;; TODO:  fix this to be smarter about indenting, etc.
    (js2-print-body body (1+ i))
    (insert pad "}")
    (unless expr
      (insert "\n"))))

(defsubst js2-function-name (node)
  "Return function name for NODE, a `js2-function-node', or nil if anonymous."
  (and (js2-function-node-name node)
       (js2-name-node-name (js2-function-node-name node))))

(defstruct (js2-function-arg-node
            (:include js2-expr-node)
            (:constructor nil)
            (:constructor make-js2-function-arg-node (&key (type js2-NAME)
                                                           (pos js2-token-beg)
                                                           len
                                                           value)))
  "AST node for a function formal parameter.
The value field is either an array/object literal or a name node."
  value)

(put 'cl-struct-js2-function-arg-node 'js2-visitor 'js2-visit-function-arg-node)
(put 'cl-struct-js2-function-arg-node 'js2-printer 'js2-print-function-arg-node)

(defun js2-visit-function-arg-node (n v)
  (js2-visit-ast (js2-function-arg-node-value n) v))

(defun js2-print-function-arg-node (n i)
  (js2-print-ast (js2-function-arg-node-value n) i))

;; Having this be an expression node makes it more flexible.
;; There are IDE contexts, such as indentation in a for-loop initializer,
;; that work better if you assume it's an expression.  Whenever we have
;; a standalone var/const declaration, we just wrap with an expr stmt.
;; Eclipse apparently screwed this up and now has two versions, expr and stmt.
(defstruct (js2-var-decl-node
            (:include js2-expr-node)
            (:constructor nil)
            (:constructor make-js2-var-decl-node (&key (type js2-VAR)
                                                       (pos js2-token-beg)
                                                       len
                                                       kids
                                                       decl-type)))
  "AST node for a variable declaration list (VAR, CONST or LET).
The node bounds differ depending on the declaration type.  For VAR or
CONST declarations, the bounds include the var/const keyword.  For LET
declarations, the node begins at the position of the first child."
  kids        ; a lisp list of `js2-var-init-node' structs.
  decl-type)  ; js2-VAR, js2-CONST or js2-LET

(put 'cl-struct-js2-var-decl-node 'js2-visitor 'js2-visit-var-decl)
(put 'cl-struct-js2-var-decl-node 'js2-printer 'js2-print-var-decl)

(defun js2-visit-var-decl (n v)
  (dolist (kid (js2-var-decl-node-kids n))
    (js2-visit-ast kid v)))

(defun js2-print-var-decl (n i)
  (let ((pad (js2-make-pad i))
        (tt (js2-var-decl-node-decl-type n)))
    (insert pad)
    (insert (cond
             ((= tt js2-VAR) "var ")
             ((= tt js2-LET) "")  ; handled by parent let-{expr/stmt}
             ((= tt js2-CONST) "const ")
             (t
              (error "malformed var-decl node"))))
    (loop with kids = (js2-var-decl-node-kids n)
          with len = (length kids)
          for kid in kids
          for count from 1
          do
          (js2-print-ast kid 0)
          (if (< count len)
              (insert ", ")))))

(defstruct (js2-var-init-node
            (:include js2-expr-node)
            (:constructor nil)
            (:constructor make-js2-var-init-node (&key (type js2-VAR)
                                                       (pos js2-ts-cursor)
                                                       len
                                                       name
                                                       initializer)))
  "AST node for a variable declaration.
The type field will be js2-CONST for a const decl."
  name          ; foo (a `js2-name-node')
  initializer)  ; = bar * baz  (a `js2-expr-node')

(put 'cl-struct-js2-var-init-node 'js2-visitor 'js2-visit-var-init-node)
(put 'cl-struct-js2-var-init-node 'js2-printer 'js2-print-var-init-node)

(defun js2-visit-var-init-node (n v)
  (js2-visit-ast (js2-var-init-node-name n) v)
  (if (js2-var-init-node-initializer n)
      (js2-visit-ast (js2-var-init-node-initializer n) v)))

(defun js2-print-var-init-node (n i)
  (let ((pad (js2-make-pad i))
        (name (js2-var-init-node-name n))
        (init (js2-var-init-node-initializer n)))
    (insert pad)
    (js2-print-ast name 0)
    (when init
      (insert " = ")
      (js2-print-ast init 0))))

(defstruct (js2-destructuring-init-node
            (:include js2-var-init-node)
            (:constructor nil)
            (:constructor make-js2-destructuring-init-node (&key (type js2-VAR)
                                                                 (pos js2-ts-cursor)
                                                                 len
                                                                 lhs
                                                                 initializer)))
  "AST node for a destructuring-assignment expression.
It will likely have a `js2-var-decl-node' as its direct parent.
The lhs field is an array literal or object literal node, and the initializer
is the expression that will be destructured."
  lhs)  ; a `js2-expr-node' (array literal or obj literal)

(put 'cl-struct-js2-destructuring-init-node 'js2-visitor 'js2-visit-destructuring-init)
(put 'cl-struct-js2-destructuring-init-node 'js2-printer 'js2-print-destructuring-init)

(defun js2-visit-destructuring-init (n v)
  (js2-visit-ast (js2-destructuring-init-node-lhs n) v)
  (if (js2-destructuring-init-node-initializer n) ; not present in for..in loops
      (js2-visit-ast (js2-destructuring-init-node-initializer n) v)))

(defun js2-print-destructuring-init (n i)
  (insert (js2-make-pad i))
  (js2-print-ast (js2-destructuring-init-node-lhs n) 0)
  (when (js2-destructuring-init-node-initializer n)
    (insert " = ")
    (js2-print-ast (js2-destructuring-init-node-initializer n) 0)))

(defstruct (js2-cond-node
            (:include js2-expr-node)
            (:constructor nil)
            (:constructor make-js2-cond-node (&key (type js2-HOOK)
                                                   (pos js2-ts-cursor)
                                                   len
                                                   test-expr
                                                   true-expr
                                                   false-expr
                                                   q-pos
                                                   c-pos)))
  "AST node for the ternary operator"
  test-expr
  true-expr
  false-expr
  q-pos   ; buffer position of ?
  c-pos)  ; buffer position of :

(put 'cl-struct-js2-cond-node 'js2-visitor 'js2-visit-cond-node)
(put 'cl-struct-js2-cond-node 'js2-printer 'js2-print-cond-node)

(defun js2-visit-cond-node (n v)
  (js2-visit-ast (js2-cond-node-test-expr n) v)
  (js2-visit-ast (js2-cond-node-true-expr n) v)
  (js2-visit-ast (js2-cond-node-false-expr n) v))

(defun js2-print-cond-node (n i)
  (let ((pad (js2-make-pad i)))
    (insert pad)
    (js2-print-ast (js2-cond-node-test-expr n) 0)
    (insert " ? ")
    (js2-print-ast (js2-cond-node-true-expr n) 0)
    (insert " : ")
    (js2-print-ast (js2-cond-node-false-expr n) 0)))

(defstruct (js2-infix-node
            (:include js2-expr-node)
            (:constructor nil)
            (:constructor make-js2-infix-node (&key type
                                                    (pos js2-ts-cursor)
                                                    len
                                                    op-pos
                                                    left
                                                    right)))
  "Abstract base type for infix expressions.
Includes assignment ops like `|=', and the comma operator.
The type field inherited from `js2-node' holds the operator."
  op-pos ; buffer position where operator begins
  left      ; a `js2-expr-node'
  right)    ; a `js2-expr-node'

(put 'cl-struct-js2-infix-node 'js2-visitor 'js2-visit-infix-node)
(put 'cl-struct-js2-infix-node 'js2-printer 'js2-print-infix-node)

(defun js2-visit-infix-node (n v)
  (js2-visit-ast (js2-infix-node-left n) v)
  (js2-visit-ast (js2-infix-node-right n) v))

(defconst js2-operator-tokens
  (let ((table (make-hash-table :test 'eq))
        (tokens
         (list (cons js2-IN "in")
               (cons js2-TYPEOF "typeof")
               (cons js2-INSTANCEOF "instanceof")
               (cons js2-DELPROP "delete")
               (cons js2-COMMA ",")
               (cons js2-COLON ":")
               (cons js2-OR "||")
               (cons js2-AND "&&")
               (cons js2-INC "++")
               (cons js2-DEC "--")
               (cons js2-BITOR "|")
               (cons js2-BITXOR "^")
               (cons js2-BITAND "&")
               (cons js2-EQ "==")
               (cons js2-NE "!=")
               (cons js2-LT "<")
               (cons js2-LE "<=")
               (cons js2-GT ">")
               (cons js2-GE ">=")
               (cons js2-LSH "<<")
               (cons js2-RSH ">>")
               (cons js2-URSH ">>>")
               (cons js2-ADD "+")       ; infix plus
               (cons js2-SUB "-")       ; infix minus
               (cons js2-MUL "*")
               (cons js2-DIV "/")
               (cons js2-MOD "%")
               (cons js2-NOT "!")
               (cons js2-BITNOT "~")
               (cons js2-POS "+")       ; unary plus
               (cons js2-NEG "-")       ; unary minus
               (cons js2-SHEQ "===")    ; shallow equality
               (cons js2-SHNE "!==")    ; shallow inequality
               (cons js2-ASSIGN "=")
               (cons js2-ASSIGN_BITOR "|=")
               (cons js2-ASSIGN_BITXOR "^=")
               (cons js2-ASSIGN_BITAND "&=")
               (cons js2-ASSIGN_LSH "<<=")
               (cons js2-ASSIGN_RSH ">>=")
               (cons js2-ASSIGN_URSH ">>>=")
               (cons js2-ASSIGN_ADD "+=")
               (cons js2-ASSIGN_SUB "-=")
               (cons js2-ASSIGN_MUL "*=")
               (cons js2-ASSIGN_DIV "/=")
               (cons js2-ASSIGN_MOD "%="))))
    (loop for (k . v) in tokens do
          (puthash k v table))
    table))

(defun js2-print-infix-node (n i)
  (let* ((tt (js2-node-type n))
         (op (gethash tt js2-operator-tokens)))
    (unless op
      (error "unrecognized infix operator %s" (js2-node-type n)))
    (insert (js2-make-pad i))
    (js2-print-ast (js2-infix-node-left n) 0)
    (unless (= tt js2-COMMA)
      (insert " "))
    (insert op)
    (insert " ")
    (js2-print-ast (js2-infix-node-right n) 0)))

(defstruct (js2-in-node
            (:include js2-infix-node)
            (:constructor nil)
            (:constructor make-js2-in-node (&key (type js2-IN)
                                                 (pos js2-ts-cursor)
                                                 len
                                                 op-pos
                                                 left
                                                 right)))
  "AST node for an in-expression such as `foo in bar'")

(put 'cl-struct-js2-in-node 'js2-visitor 'js2-visit-infix-node)
(put 'cl-struct-js2-in-node 'js2-printer 'js2-print-infix-node)

(defstruct (js2-assign-node
            (:include js2-infix-node)
            (:constructor nil)
            (:constructor make-js2-assign-node (&key type
                                                     (pos js2-ts-cursor)
                                                     len
                                                     op-pos
                                                     left
                                                     right)))
  "Represents any assignment.
The type field holds the actual assignment operator.")

(put 'cl-struct-js2-assign-node 'js2-visitor 'js2-visit-infix-node)
(put 'cl-struct-js2-assign-node 'js2-printer 'js2-print-infix-node)

(defstruct (js2-unary-node
            (:include js2-expr-node)
            (:constructor nil)
            (:constructor make-js2-unary-node (&key type ; required
                                                    (pos js2-ts-cursor)
                                                    len
                                                    operand)))
  "AST node type for unary operator nodes.
The type field can be NOT, BITNOT, POS, NEG, INC, DEC,
TYPEOF, or DELPROP.  For INC or DEC, a 'postfix node
property is added if the operator follows the operand."
  operand)  ; a `js2-expr-node'

(put 'cl-struct-js2-unary-node 'js2-visitor 'js2-visit-unary-node)
(put 'cl-struct-js2-unary-node 'js2-printer 'js2-print-unary-node)

(defun js2-visit-unary-node (n v)
  (js2-visit-ast (js2-unary-node-operand n) v))

(defun js2-print-unary-node (n i)
  (let* ((tt (js2-node-type n))
         (op (gethash tt js2-operator-tokens))
         (postfix (js2-node-get-prop n 'postfix)))
    (unless op
      (error "unrecognized unary operator %s" tt))
    (insert (js2-make-pad i))
    (unless postfix
      (insert op))
    (if (or (= tt js2-TYPEOF)
            (= tt js2-DELPROP))
        (insert " "))
    (js2-print-ast (js2-unary-node-operand n) 0)
    (when postfix
      (insert op))))

(defstruct (js2-let-expr-node
            (:include js2-expr-node)
            (:constructor nil)
            (:constructor make-js2-let-expr-node (&key (type js2-LETEXPR)
                                                       (pos js2-token-beg)
                                                       len
                                                       vars
                                                       expr
                                                       lp
                                                       rp)))
  "AST node for a let expression such as let (i=6) i+2."
  vars
  expr   ; a `js2-expr-node'
  scope  ; a `js2-scope'
  lp
  rp)

(put 'cl-struct-js2-let-expr-node 'js2-visitor 'js2-visit-let-expr-node)
(put 'cl-struct-js2-let-expr-node 'js2-printer 'js2-print-let-expr-node)

(defun js2-visit-let-expr-node (n v)
  (js2-visit-ast (js2-let-expr-node-vars n) v)
  (if (js2-let-expr-node-expr n)
      (js2-visit-ast (js2-let-expr-node-expr n) v)))

(defun js2-print-let-expr-node (n i)
  (let ((expr (js2-let-expr-node-expr n)))
  (insert (js2-make-pad i) "let ")
  (if expr
      (insert "("))
  (js2-print-ast (js2-let-expr-node-vars n) 0)
  (when expr
    (insert ") ")
    (js2-print-ast expr 0))))

(defstruct (js2-let-stmt-node
            (:include js2-stmt-node)
            (:constructor nil)
            (:constructor make-js2-let-stmt-node (&key (type js2-LET)
                                                       (pos js2-token-beg)
                                                       len
                                                       vars
                                                       body
                                                       lp
                                                       rp)))
  "AST node for a let statement such as let (i=6) {x += i}."
  vars
  body   ; a statement node
  scope  ; a `js2-scope'
  lp     ; position of left-paren, nil if omitted
  rp)    ; position of right-paren, nil if omitted

(put 'cl-struct-js2-let-stmt-node 'js2-visitor 'js2-visit-let-stmt-node)
(put 'cl-struct-js2-let-stmt-node 'js2-printer 'js2-print-let-stmt-node)

(defun js2-visit-let-stmt-node (n v)
  (let ((body (js2-let-stmt-node-body n)))
    (js2-visit-ast (js2-let-stmt-node-vars n) v)
    (when body
      (js2-visit-ast body v))))

(defun js2-print-let-stmt-node (n i)
  (let ((pad (js2-make-pad i))
        body (js2-let-stmt-node-body))
    (insert pad "let ")
    (if body
        (insert "("))
    (js2-print-ast (js2-let-stmt-node-vars n) 0)
    (if (not body)
        (insert ";\n")
      (insert ") {\n")
      (js2-print-body (js2-let-stmt-node-body n) (1+ i))
      (insert pad "}\n"))))

;; `defstruct' doesn't provide multiple inheritance for fields, alas.
(defsubst js2-let-node-p (node)
  "Interface predicate for let-stmt and let-expr nodes."
  (or (js2-let-expr-node-p node)
      (js2-let-stmt-node-p node)))

(defsubst js2-let-node-lp (node)
  (if (js2-let-expr-node-p node)
      (js2-let-expr-node-lp node)
    (js2-let-stmt-node-lp node)))

(defsubst js2-let-node-rp (node)
  (if (js2-let-expr-node-p node)
      (js2-let-expr-node-rp node)
    (js2-let-stmt-node-rp node)))

(defstruct (js2-debugger-node
            (:include js2-stmt-node)
            (:constructor nil)
            (:constructor make-js2-debugger-node (&key (type js2-DEBUGGER)
                                                       (pos js2-token-beg)
                                                       (len (- js2-ts-cursor pos)))))
  "AST node for debugger statement.")

(put 'cl-struct-js2-debugger-node 'js2-visitor 'js2-visit-none)
(put 'cl-struct-js2-debugger-node 'js2-printer 'js2-print-debugger)

(defun js2-print-debugger (n i)
  (insert (js2-make-pad i) "debugger;\n"))

(defstruct (js2-new-node
            (:include js2-expr-node)
            (:constructor nil)
            (:constructor make-js2-new-node (&key (type js2-NEW)
                                                  (pos js2-token-beg)
                                                  len
                                                  target
                                                  args
                                                  initializer
                                                  lp
                                                  rp)))
  "AST node for new-expression such as new Foo()."
  target  ; an identifier or reference
  args    ; a lisp list of argument nodes
  lp      ; position of left-paren, nil if omitted
  rp      ; position of right-paren, nil if omitted
  initializer) ; experimental Rhino syntax:  optional `js2-object-node'

(put 'cl-struct-js2-new-node 'js2-visitor 'js2-visit-new-node)
(put 'cl-struct-js2-new-node 'js2-printer 'js2-print-new-node)

(defun js2-visit-new-node (n v)
  (js2-visit-ast (js2-new-node-target n) v)
  (dolist (arg (js2-new-node-args n))
    (js2-visit-ast arg v))
  (when (js2-new-node-initializer n)
    (js2-visit-ast (js2-new-node-initializer n) v)))

(defun js2-print-new-node (n i)
  (insert (js2-make-pad i) "new ")
  (js2-print-ast (js2-new-node-target n))
  (insert "(")
  (js2-print-list (js2-new-node-args n))
  (insert ")")
  (when (js2-new-node-initializer n)
    (insert " ")
    (js2-print-ast (js2-new-node-initializer n))))

(defstruct (js2-name-node
            (:include js2-expr-node)
            (:constructor nil)
            (:constructor make-js2-name-node (&key (type js2-NAME)
                                                   (pos js2-token-beg)
                                                   (len (- js2-ts-cursor
                                                           js2-token-beg))
                                                   (name js2-ts-string))))
  "AST node for a JavaScript identifier"
  name)  ; a string

(put 'cl-struct-js2-name-node 'js2-visitor 'js2-visit-none)
(put 'cl-struct-js2-name-node 'js2-printer 'js2-print-name)

(defun js2-print-name (n i)
  (insert (js2-make-pad i)
          (js2-name-node-name n)))

(defstruct (js2-number-node
            (:include js2-expr-node)
            (:constructor nil)
            (:constructor make-js2-number-node (&key (type js2-NUMBER)
                                                     (pos js2-token-beg)
                                                     (len (- js2-ts-cursor
                                                             js2-token-beg))
                                                     (value js2-ts-number))))
  "AST node for a number literal."
  value)

(put 'cl-struct-js2-number-node 'js2-visitor 'js2-visit-none)
(put 'cl-struct-js2-number-node 'js2-printer 'js2-print-number-node)

(defun js2-print-number-node (n i)
  (insert (js2-make-pad i)
          (number-to-string (js2-number-node-value n))))

(defstruct (js2-regexp-node
            (:include js2-expr-node)
            (:constructor nil)
            (:constructor make-js2-regexp-node (&key (type js2-REGEXP)
                                                     (pos js2-token-beg)
                                                     (len (- js2-ts-cursor
                                                             js2-token-beg))
                                                     value
                                                     flags)))
  "AST node for a regular expression literal."
  value  ; the regexp string, without // delimiters
  flags) ; a string of flags, e.g. `mi'.

(put 'cl-struct-js2-regexp-node 'js2-visitor 'js2-visit-none)
(put 'cl-struct-js2-regexp-node 'js2-printer 'js2-print-regexp)

(defun js2-print-regexp (n i)
  (insert (js2-make-pad i)
          "/"
          (js2-regexp-node-value n)
          "/")
  (if (js2-regexp-node-flags n)
      (insert (js2-regexp-node-flags n))))

(defstruct (js2-string-node
            (:include js2-expr-node)
            (:constructor nil)
            (:constructor make-js2-string-node (&key (type js2-STRING)
                                                     (pos js2-token-beg)
                                                     (len (- js2-ts-cursor
                                                             js2-token-beg))
                                                     (value js2-ts-string))))
  "String literal.
Escape characters are not evaluated; e.g. \n is 2 chars in value field.
You can tell the quote type by looking at the first character."
  value) ; the characters of the string, including the quotes

(put 'cl-struct-js2-string-node 'js2-visitor 'js2-visit-none)
(put 'cl-struct-js2-string-node 'js2-printer 'js2-print-string)

(defun js2-print-string (n i)
  (insert (js2-make-pad i)
          (js2-node-string n)))

(defstruct (js2-array-node
            (:include js2-expr-node)
            (:constructor nil)
            (:constructor make-js2-array-node (&key (type js2-ARRAYLIT)
                                                    (pos js2-ts-cursor)
                                                    len
                                                    elems)))
  "AST node for an array literal."
  elems)  ; list of expressions.  [foo,,bar] yields a nil middle element.

(put 'cl-struct-js2-array-node 'js2-visitor 'js2-visit-array-node)
(put 'cl-struct-js2-array-node 'js2-printer 'js2-print-array-node)

(defun js2-visit-array-node (n v)
  (dolist (e (js2-array-node-elems n))
    (when e  ; can be nil, e.g. [a, ,b]
      (js2-visit-ast e v))))

(defun js2-print-array-node (n i)
  (insert (js2-make-pad i) "[")
  (js2-print-list (js2-array-node-elems n))
  (insert "]"))

(defstruct (js2-object-node
            (:include js2-expr-node)
            (:constructor nil)
            (:constructor make-js2-object-node (&key (type js2-OBJECTLIT)
                                                     (pos js2-ts-cursor)
                                                     len
                                                     elems)))
  "AST node for an object literal expression."
  elems)  ; a lisp list of `js2-object-prop-node'

(put 'cl-struct-js2-object-node 'js2-visitor 'js2-visit-object-node)
(put 'cl-struct-js2-object-node 'js2-printer 'js2-print-object-node)

(defun js2-visit-object-node (n v)
  (dolist (e (js2-object-node-elems n))
    (js2-visit-ast e v)))

(defun js2-print-object-node (n i)
  (insert (js2-make-pad i) "{")
  (js2-print-list (js2-object-node-elems n))
  (insert "}"))

(defstruct (js2-object-prop-node
            (:include js2-infix-node)
            (:constructor nil)
            (:constructor make-js2-object-prop-node (&key (type js2-COLON)
                                                          (pos js2-ts-cursor)
                                                          len
                                                          left
                                                          right
                                                          op-pos)))
  "AST node for an object literal prop:value entry.
The `left' field is the property:  a name node, string node or number node.
The `right' field is a `js2-expr-node' representing the initializer value.")

(put 'cl-struct-js2-object-prop-node 'js2-visitor 'js2-visit-infix-node)
(put 'cl-struct-js2-object-prop-node 'js2-printer 'js2-print-object-prop-node)

(defun js2-print-object-prop-node (n i)
  (insert (js2-make-pad i))
  (js2-print-ast (js2-object-prop-node-left n) 0)
  (insert ":")
  (js2-print-ast (js2-object-prop-node-right n) 0))

(defstruct (js2-getter-setter-node
            (:include js2-infix-node)
            (:constructor nil)
            (:constructor make-js2-getter-setter-node (&key type ; GET or SET
                                                            (pos js2-ts-cursor)
                                                            len
                                                            left
                                                            right
                                                            kwd)))
  "AST node for a getter/setter property in an object literal.
The `left' field is the `js2-name-node' naming the getter/setter prop.
The `right' field is always an anonymous `js2-function-node' with a node
property `GETTER_SETTER' set to js2-GET or js2-SET. "
  kwd)  ; a `js2-string-node' representing the keyword

(put 'cl-struct-js2-getter-setter-node 'js2-visitor 'js2-visit-infix-node)
(put 'cl-struct-js2-getter-setter-node 'js2-printer 'js2-print-getter-setter)

(defun js2-print-getter-setter (n i)
  (let ((pad (js2-make-pad i))
        (kwd (js2-getter-setter-node-kwd n))
        (left (js2-getter-setter-node-left n))
        (right (js2-getter-setter-node-right n)))
    (insert pad (js2-string-node-value kwd) " ")
    (js2-print-ast left 0)
    (js2-print-ast right 0)))

(defstruct (js2-prop-get-node
            (:include js2-expr-node)
            (:constructor nil)
            (:constructor make-js2-prop-get-node (&key (type js2-GETPROP)
                                                       (pos js2-ts-cursor)
                                                       len
                                                       target
                                                       prop)))
  "AST node for a dotted property reference, e.g. foo.bar or foo().bar"
  target  ; foo (can be any expression)
  prop)   ; bar (also any expression)

(put 'cl-struct-js2-prop-get-node 'js2-visitor 'js2-visit-prop-get-node)
(put 'cl-struct-js2-prop-get-node 'js2-printer 'js2-print-prop-get-node)

(defun js2-visit-prop-get-node (n v)
  (js2-visit-ast (js2-prop-get-node-target n) v)
  (js2-visit-ast (js2-prop-get-node-prop n) v))

(defun js2-print-prop-get-node (n i)
  (insert (js2-make-pad i))
  (js2-print-ast (js2-prop-get-node-target n) 0)
  (insert ".")
  (js2-print-ast (js2-prop-get-node-prop n) 0))

(defstruct (js2-elem-get-node
            (:include js2-expr-node)
            (:constructor nil)
            (:constructor make-js2-elem-get-node (&key (type js2-GETELEM)
                                                       (pos js2-ts-cursor)
                                                       len
                                                       target
                                                       prop
                                                       lb
                                                       rb)))
  "AST node for an array index expression such as foo[bar]."
  target  ; foo (can be any expression)
  prop    ; bar (also any expression)
  lb      ; position of left-bracket, nil if omitted
  rb)     ; position of right-bracket, nil if omitted

(put 'cl-struct-js2-elem-get-node 'js2-visitor 'js2-visit-elem-get-node)
(put 'cl-struct-js2-elem-get-node 'js2-printer 'js2-print-elem-get-node)

(defun js2-visit-elem-get-node (n v)
  (js2-visit-ast (js2-elem-get-node-target n) v)
  (js2-visit-ast (js2-elem-get-node-prop n) v))

(defun js2-print-elem-get-node (n i)
  (insert (js2-make-pad i))
  (js2-print-ast (js2-elem-get-node-target n) 0)
  (insert "[")
  (js2-print-ast (js2-elem-get-node-prop n) 0)
  (insert "]"))

(defstruct (js2-call-node
            (:include js2-expr-node)
            (:constructor nil)
            (:constructor make-js2-call-node (&key (type js2-CALL)
                                                   (pos js2-ts-cursor)
                                                   len
                                                   expr
                                                   args
                                                   lp
                                                   rp)))
  "AST node for a JavaScript function call."
  expr  ; a `js2-expr-node' evaluating to the function to call
  args  ; a lisp list of argument AST nodes
  lp    ; position of open-paren, or nil if missing
  rp)   ; position of close-paren, or nil if missing

(put 'cl-struct-js2-call-node 'js2-visitor 'js2-visit-call-node)
(put 'cl-struct-js2-call-node 'js2-printer 'js2-print-call-node)

(defun js2-visit-call-node (n v)
  (js2-visit-ast (js2-call-node-expr n) v)
  (dolist (arg (js2-call-node-args n))
    (js2-visit-ast arg v)))

(defun js2-print-call-node (n i)
  (insert (js2-make-pad i))
  (js2-print-ast (js2-call-node-expr n) 0)
  (insert "(")
  (js2-print-list (js2-call-node-args n))
  (insert ")"))

(defstruct (js2-literal-node
            (:include js2-expr-node)
            (:constructor nil)
            (:constructor make-js2-literal-node (&key type  ; required
                                                      (pos js2-token-beg)
                                                      (len (- js2-ts-cursor
                                                              js2-token-beg)))))
  "AST node representing a literal value such as `null'.
Used for `null', `this', `true' and `false'.
The node type is set to js2-NULL, js2-THIS, etc.")

(put 'cl-struct-js2-literal-node 'js2-visitor 'js2-visit-none)
(put 'cl-struct-js2-literal-node 'js2-printer 'js2-print-literal-node)

(defun js2-print-literal-node (n i)
  (insert (js2-make-pad i))
  (insert
   (let ((type (js2-node-type n)))
     (cond
      ((= type js2-NULL) "null")
      ((= type js2-VOID) "void")
      ((= type js2-THIS) "this")
      ((= type js2-TRUE) "true")
      ((= type js2-FALSE) "false")
      (t
       (error "Unknown literal value %s" type))))))

;; This can be an expr or statment, unfortunately.
(defstruct (js2-yield-node
            (:include js2-node)
            (:constructor nil)
            (:constructor make-js2-yield-node (&key (type js2-YIELD)
                                                    (pos js2-ts-cursor)
                                                    len
                                                    value)))
  "AST node for yield statement."
  value) ; value to be yielded

(put 'cl-struct-js2-yield-node 'js2-visitor 'js2-visit-yield-node)
(put 'cl-struct-js2-yield-node 'js2-printer 'js2-print-yield-node)

(defun js2-visit-yield-node (n v)
  (js2-visit-ast (js2-yield-node-value n) v))

(defun js2-print-yield-node (n i)
  (insert (js2-make-pad i))
  (insert "yield")
  (when (js2-yield-node-value n)
    (insert " ")
    (js2-print-ast (js2-yield-node-value n) 0)))

(defstruct (js2-paren-node
            (:include js2-expr-node)
            (:constructor nil)
            (:constructor make-js2-paren-node (&key (type js2-LP)
                                                    (pos js2-ts-cursor)
                                                    len
                                                    expr)))
  "AST node for a parenthesized expression.
In particular, used when the parens are syntactically optional,
as opposed to required parens such as those enclosing an if-conditional."
  expr)   ; `js2-expr-node'

(put 'cl-struct-js2-paren-node 'js2-visitor 'js2-visit-paren-node)
(put 'cl-struct-js2-paren-node 'js2-printer 'js2-print-paren-node)

(defun js2-visit-paren-node (n v)
  (js2-visit-ast (js2-paren-node-expr n) v))

(defun js2-print-paren-node (n i)
  (insert (js2-make-pad i))
  (insert "(")
  (js2-print-ast (js2-paren-node-expr n) 0)
  (insert ")"))

(defstruct (js2-array-comp-node
            (:include js2-expr-node)
            (:constructor nil)
            (:constructor make-js2-array-comp-node (&key (type js2-ARRAYCOMP)
                                                         (pos js2-ts-cursor)
                                                         len
                                                         result
                                                         loops
                                                         filter
                                                         if-pos
                                                         lp
                                                         rp)))
  "AST node for an Array comprehension such as [[x,y] for (x in foo) for (y in bar)]."
  result  ; result expression (just after left-bracket)
  loops   ; a lisp list of `js2-array-comp-loop-node'
  filter  ; guard/filter expression
  if-pos  ; buffer pos of 'if' keyword, if present, else nil
  lp      ; buffer position of if-guard left-paren, or nil if not present
  rp)     ; buffer position of if-guard right-paren, or nil if not present

(put 'cl-struct-js2-array-comp-node 'js2-visitor 'js2-visit-array-comp-node)
(put 'cl-struct-js2-array-comp-node 'js2-printer 'js2-print-array-comp-node)

(defun js2-visit-array-comp-node (n v)
  (js2-visit-ast (js2-array-comp-node-result n) v)
  (dolist (l (js2-array-comp-node-loops n))
    (js2-visit-ast l v))
  (if (js2-array-comp-node-filter n)
      (js2-visit-ast (js2-array-comp-node-filter n) v)))

(defun js2-print-array-comp-node (n i)
  (let ((pad (js2-make-pad i))
        (result (js2-array-comp-node-result n))
        (loops (js2-array-comp-node-loops n))
        (filter (js2-array-comp-node-filter n)))
    (insert pad "[")
    (js2-print-ast result 0)
    (dolist (l loops)
      (insert " ")
      (js2-print-ast l 0))
    (when filter
      (insert " if (")
      (js2-print-ast filter 0))
    (insert ")]")))

(defstruct (js2-array-comp-loop-node
            (:include js2-for-in-node)
            (:constructor nil)
            (:constructor make-js2-array-comp-loop-node (&key (type js2-FOR)
                                                              (pos js2-ts-cursor)
                                                              len
                                                              iterator
                                                              object
                                                              in-pos
                                                              foreach-p
                                                              each-pos
                                                              lp
                                                              rp)))
  "AST subtree for each 'for (foo in bar)' loop in an array comprehension.")

(put 'cl-struct-js2-array-comp-loop-node 'js2-visitor 'js2-visit-array-comp-loop)
(put 'cl-struct-js2-array-comp-loop-node 'js2-printer 'js2-print-array-comp-loop)

(defun js2-visit-array-comp-loop (n v)
  (js2-visit-ast (js2-array-comp-loop-node-iterator n) v)
  (js2-visit-ast (js2-array-comp-loop-node-object n) v))

(defun js2-print-array-comp-loop (n i)
  (insert "for (")
  (js2-print-ast (js2-array-comp-loop-node-iterator n) 0)
  (insert " in ")
  (js2-print-ast (js2-array-comp-loop-node-object n) 0)
  (insert ")"))

(defstruct (js2-empty-stmt-node
            (:include js2-stmt-node)
            (:constructor nil)
            (:constructor make-js2-empty-stmt-node (&key (type js2-EMPTY)
                                                         (pos js2-token-beg)
                                                         len)))
  "AST node for an empty statement.")

(put 'cl-struct-js2-empty-stmt-node 'js2-visitor 'js2-visit-none)
(put 'cl-struct-js2-empty-stmt-node 'js2-printer 'js2-print-none)

(defstruct (js2-empty-expr-node
            (:include js2-expr-node)
            (:constructor nil)
            (:constructor make-js2-empty-expr-node (&key (type js2-EMPTY)
                                                         (pos js2-token-beg)
                                                         len)))
  "AST node for an empty expression.")

(put 'cl-struct-js2-empty-expr-node 'js2-visitor 'js2-visit-none)
(put 'cl-struct-js2-empty-expr-node 'js2-printer 'js2-print-none)

(defstruct (js2-xml-node
            (:include js2-block-node)
            (:constructor nil)
            (:constructor make-js2-xml-node (&key (type js2-XML)
                                                  (pos js2-token-beg)
                                                  len
                                                  kids)))
  "AST node for initial parse of E4X literals.
The kids field is a list of XML fragments:  strings and js exprs.
Rhino just creates a concatenation of them (binary plus) and passes
the concatenation to the constructor of a new XML object.")

(put 'cl-struct-js2-xml-node 'js2-visitor 'js2-visit-block)
(put 'cl-struct-js2-xml-node 'js2-printer 'js2-print-xml-node)

(defun js2-print-xml-node (n i)
  (dolist (kid (js2-xml-node-kids n))
    (js2-print-ast kid i)))

(defstruct (js2-xml-start-tag-node
            (:include js2-xml-node)
            (:constructor nil)
            (:constructor make-js2-xml-start-tag-node (&key (type js2-XML)
                                                            (pos js2-ts-cursor)
                                                            len
                                                            name
                                                            attrs
                                                            kids
                                                            empty-p)))
  "AST node for an XML start-tag.
The `kids' field is a lisp list of child content nodes."
  name      ; a `js2-xml-name-node'
  attrs     ; a lisp list of `js2-xml-attr-node'
  empty-p)  ; t if this is an empty element such as <foo bar="baz"/>

(put 'cl-struct-js2-xml-start-tag-node 'js2-visitor 'js2-visit-xml-start-tag)
(put 'cl-struct-js2-xml-start-tag-node 'js2-printer 'js2-print-xml-start-tag)

(defun js2-visit-xml-start-tag (n v)
  (js2-visit-ast (js2-xml-start-tag-node-name n) v)
  (dolist (attr (js2-xml-start-tag-node-attrs n))
    (js2-visit-ast attr v))
  (js2-visit-block n v))

(defun js2-print-xml-start-tag (n i)
  (insert (js2-make-pad i) "<")
  (js2-print-ast (js2-xml-start-tag-node-name n) 0)
  (when (js2-xml-start-tag-node-attrs n)
    (insert " ")
    (js2-print-list (js2-xml-start-tag-node-attrs n) " "))
  (insert ">"))

;; I -think- I'm going to make the parent node the corresponding start-tag,
;; and add the end-tag to the kids list of the parent as well.
(defstruct (js2-xml-end-tag-node
            (:include js2-xml-node)
            (:constructor nil)
            (:constructor make-js2-xml-end-tag-node (&key (type js2-XML)
                                                          (pos js2-ts-cursor)
                                                          len
                                                          name)))
  "AST node for an XML end-tag."
  name)  ; a `js2-xml-name-node'

(put 'cl-struct-js2-xml-end-tag-node 'js2-visitor 'js2-visit-xml-end-tag)
(put 'cl-struct-js2-xml-end-tag-node 'js2-printer 'js2-print-xml-end-tag)

(defun js2-visit-xml-end-tag (n v)
  (js2-visit-ast (js2-xml-end-tag-node-name n) v))

(defun js2-print-xml-end-tag (n i)
  (insert (js2-make-pad i))
  (insert "</")
  (js2-print-ast (js2-xml-end-tag-node-name n) 0)
  (insert ">"))

(defstruct (js2-xml-name-node
            (:include js2-xml-node)
            (:constructor nil)
            (:constructor make-js2-xml-name-node (&key (type js2-XML)
                                                       (pos js2-ts-cursor)
                                                       len
                                                       namespace
                                                       kids)))
  "AST node for an E4X XML name.
Any XML name can be qualified with a namespace, hence the namespace field.
Further, any E4X name can be comprised of arbitrary JavaScript {} expressions.
The kids field is a list of `js2-name-node' and `js2-xml-js-expr-node'.
For a simple name, the kids list has exactly one node, a `js2-name-node'."
  namespace)  ; a `js2-string-node'

(put 'cl-struct-js2-xml-name-node 'js2-visitor 'js2-visit-xml-name-node)
(put 'cl-struct-js2-xml-name-node 'js2-printer 'js2-print-xml-name-node)

(defun js2-visit-xml-name-node (n v)
  (js2-visit-ast (js2-xml-name-node-namespace n) v))

(defun js2-print-xml-name-node (n i)
  (insert (js2-make-pad i))
  (when (js2-xml-name-node-namespace n)
    (js2-print-ast (js2-xml-name-node-namespace n) 0)
    (insert "::"))
  (dolist (kid (js2-xml-name-node-kids n))
    (js2-print-ast kid 0)))

(defstruct (js2-xml-pi-node
            (:include js2-xml-node)
            (:constructor nil)
            (:constructor make-js2-xml-pi-node (&key (type js2-XML)
                                                     (pos js2-ts-cursor)
                                                     len
                                                     name
                                                     attrs)))
  "AST node for an E4X XML processing instruction."
  name   ; a `js2-xml-name-node'
  attrs) ; a list of `js2-xml-attr-node'

(put 'cl-struct-js2-xml-pi-node 'js2-visitor 'js2-visit-xml-pi-node)
(put 'cl-struct-js2-xml-pi-node 'js2-printer 'js2-print-xml-pi-node)

(defun js2-visit-xml-pi-node (n v)
  (js2-visit-ast (js2-xml-pi-node-name n) v)
  (dolist (attr (js2-xml-pi-node-attrs n))
    (js2-visit-ast attr v)))

(defun js2-print-xml-pi-node (n i)
  (insert (js2-make-pad i) "<?")
  (js2-print-ast (js2-xml-pi-node-name n))
  (when (js2-xml-pi-node-attrs n)
    (insert " ")
    (js2-print-list (js2-xml-pi-node-attrs n)))
  (insert "?>"))

(defstruct (js2-xml-cdata-node
            (:include js2-xml-node)
            (:constructor nil)
            (:constructor make-js2-xml-cdata-node (&key (type js2-XML)
                                                        (pos js2-ts-cursor)
                                                        len
                                                        content)))
  "AST node for a CDATA escape section."
  content)  ; a `js2-string-node' with node-property 'quote-type 'cdata

(put 'cl-struct-js2-xml-cdata-node 'js2-visitor 'js2-visit-xml-cdata-node)
(put 'cl-struct-js2-xml-cdata-node 'js2-printer 'js2-print-xml-cdata-node)

(defun js2-visit-xml-cdata-node (n v)
  (js2-visit-ast (js2-xml-cdata-node-content n) v))

(defun js2-print-xml-cdata-node (n i)
  (insert (js2-make-pad i))
  (js2-print-ast (js2-xml-cdata-node-content n)))

(defstruct (js2-xml-js-expr-node
            (:include js2-xml-node)
            (:constructor nil)
            (:constructor make-js2-xml-js-expr-node (&key (type js2-XML)
                                                          (pos js2-ts-cursor)
                                                          len
                                                          expr)))
  "AST node for an embedded JavaScript {expression} in an E4X literal.
The start and end fields correspond to the curly-braces."
  expr)  ; a `js2-expr-node' of some sort

(put 'cl-struct-js2-xml-js-expr-node 'js2-visitor 'js2-visit-xml-js-expr)
(put 'cl-struct-js2-xml-js-expr-node 'js2-printer 'js2-print-xml-js-expr)

(defun js2-visit-xml-js-expr (n v)
  (js2-visit-ast (js2-xml-js-expr-node-expr n) v))

(defun js2-print-xml-js-expr (n i)
  (insert (js2-make-pad i))
  (insert "{")
  (js2-print-ast (js2-xml-js-expr-node-expr n) 0)
  (insert "}"))

(defstruct (js2-xml-attr-node
            (:include js2-xml-node)
            (:constructor nil)
            (:constructor make-js2-attr-node (&key (type js2-XML)
                                                   (pos js2-ts-cursor)
                                                   len
                                                   name
                                                   value
                                                   eq-pos
                                                   quote-type)))
  "AST node representing a foo='bar' XML attribute value."
  name   ; a `js2-xml-name-node'
  value  ; a `js2-xml-name-node'
  eq-pos ; buffer position of "=" sign
  quote-type) ; 'single or 'double

(put 'cl-struct-js2-xml-attr-node 'js2-visitor 'js2-visit-xml-attr-node)
(put 'cl-struct-js2-xml-attr-node 'js2-printer 'js2-print-xml-attr-node)

(defun js2-visit-xml-attr-node (n v)
  (js2-visit-ast (js2-xml-attr-node-name n) v)
  (js2-visit-ast (js2-xml-attr-node-value n) v))

(defun js2-print-xml-attr-node (n i)
  (let ((quote (if (eq (js2-xml-attr-node-quote-type n) 'single)
                   "'"
                 "\"")))
    (insert (js2-make-pad i))
    (js2-print-ast (js2-xml-attr-node-name n) 0)
    (insert "=" quote)
    (js2-print-ast (js2-xml-attr-node-value n) 0)
    (insert quote)))

(defstruct (js2-xml-text-node
            (:include js2-xml-node)
            (:constructor nil)
            (:constructor make-js2-text-node (&key (type js2-XML)
                                                   (pos js2-ts-cursor)
                                                   len
                                                   content)))
  "AST node for an E4X XML text node."
  content)  ; a lisp list of `js2-string-node' and `js2-xml-js-expr-node'

(put 'cl-struct-js2-xml-text-node 'js2-visitor 'js2-visit-xml-text-node)
(put 'cl-struct-js2-xml-text-node 'js2-printer 'js2-print-xml-text-node)

(defun js2-visit-xml-text-node (n v)
  (js2-visit-ast (js2-xml-text-node-content n) v))

(defun js2-print-xml-text-node (n i)
  (insert (js2-make-pad i))
  (dolist (kid (js2-xml-text-node-content n))
    (js2-print-ast kid)))

;; Should this be a `js2-comment-node' instead?
(defstruct (js2-xml-comment-node
            (:include js2-xml-node)
            (:constructor nil)
            (:constructor make-js2-xml-comment-node (&key (type js2-XML)
                                                          (pos js2-ts-cursor)
                                                          len)))
  "AST node for E4X XML comment.")

(put 'cl-struct-js2-xml-comment-node 'js2-visitor 'js2-visit-none)
(put 'cl-struct-js2-xml-comment-node 'js2-printer 'js2-print-xml-comment)

(defun js2-print-xml-comment (n i)
  (insert (js2-make-pad i)
          (js2-node-string n)))

(defstruct (js2-xml-dot-query-node
            (:include js2-infix-node)
            (:constructor nil)
            (:constructor make-js2-xml-dot-query-node (&key (type js2-DOTQUERY)
                                                            (pos js2-ts-cursor)
                                                            op-pos
                                                            len
                                                            left
                                                            right)))
  "AST node for an E4X foo.(bar) filter expression.")

(put 'cl-struct-js2-xml-dot-query-node 'js2-visitor 'js2-visit-infix-node)
(put 'cl-struct-js2-xml-dot-query-node 'js2-printer 'js2-print-xml-dot-query)

(defun js2-print-xml-dot-query (n i)
  (insert (js2-make-pad i))
  (js2-print-ast (js2-xml-dot-query-node-left n) 0)
  (insert ".(")
  (js2-print-ast (js2-xml-dot-query-node-right n) 0)
  (insert ")"))

(defstruct (js2-xml-ref-node
            (:include js2-expr-node)
            (:constructor nil)
            (:constructor make-js2-xml-ref-node (&key (type js2-REF_NAME)
                                                      (pos js2-ts-cursor)
                                                      op-pos
                                                      len
                                                      target
                                                      namespace
                                                      propname
                                                      ref-expr
                                                      flags)))
  "AST node for a simple E4X XML attribute or member expression.
This can take a variety of forms:
  name, ns::name, *::name, ns::*, *::*
  @attr, @*, @ns::attr, @ns::*, @*::attr, @*::*
  @[expr], @ns::[expr], @*::[expr]
Not all the fields will be set for any given format.
This node type will be the child of a '.' or '..' operator.
The target field is the expression preceding the operator.
The flags field specifies whether it's an attribute access, and
also whether it was a .. (descendants) operator."
  target    ; `js2-expr-node'
  namespace ; a `js2-string-node' representing a prop name or "*"
  propname  ; a `js2-string-node' representing a prop name or "*"
  ref-expr  ; if [expr], a `js2-expr-node' for the expression
  flags     ; member type flags (mostly for runtime execution)
  op-pos)   ; position of the start of the operator

(put 'cl-struct-js2-xml-ref-node 'js2-visitor 'js2-visit-xml-ref-node)
(put 'cl-struct-js2-xml-ref-node 'js2-printer 'js2-print-xml-ref-node)

(defun js2-visit-xml-ref-node (n v)
  (if (js2-xml-ref-node-target n)
      (js2-visit-ast (js2-xml-ref-node-target n) v))
  (if (js2-xml-ref-node-namespace n)
      (js2-visit-ast (js2-xml-ref-node-namespace n) v))
  (if (js2-xml-ref-node-propname n)
      (js2-visit-ast (js2-xml-ref-node-propname n) v))
  (if (js2-xml-ref-node-ref-expr n)
      (js2-visit-ast (js2-xml-ref-node-ref-expr n) v)))

(defun js2-print-xml-ref-node (n i)
  (insert (js2-make-pad i))
  (js2-print-ast (js2-xml-ref-node-target n) 0)
  (insert (if (flag-set-p (js2-xml-ref-node-flags n)
                          js2-descendants-flag)
              ".."
            ","))
  (when (js2-xml-ref-node-namespace n)
    (js2-print-ast (js2-xml-ref-node-namespace n) 0)
    (insert "::"))
  (if (js2-xml-ref-node-propname n)
      (js2-print-ast (js2-xml-ref-node-propname n) 0)
    (insert "[")
    (js2-print-ast (js2-xml-ref-node-ref-expr n) 0)
    (insert "]")))

(defsubst js2-node-line (n)
  "Fetch the source line number at the start of node N.
This is O(n) in the length of the source buffer; use prudently."
  (1+ (count-lines (point-min) (js2-node-abs-pos n))))

(defsubst js2-block-node-kid (n i)
  "Return child I of node N, or nil if there aren't that many."
  (nth i (js2-block-node-kids n)))

(defsubst js2-block-node-first (n)
  "Return first child of block node N, or nil if there is none."
  (first (js2-block-node-kids n)))

(defun js2-node-root (n)
  "Return the root of the AST containing N.
If N has no parent pointer, returns N."
  (let ((parent (js2-node-parent n)))
    (if parent
        (js2-node-root parent)
      n)))

(defun js2-node-parent-stmt (node)
  "Return the node's first ancestor that is a statement.
Returns nil if NODE is a `js2-ast-root'."
  (let ((parent (js2-node-parent node)))
    (if (or (null parent)
            (js2-stmt-node-p parent)
            (and (js2-function-node-p parent)
                 (neq (js2-function-node-form parent) 'FUNCTION_EXPRESSION)))
        parent
      (js2-node-parent-stmt parent))))

(defun js2-node-position-in-parent (node &optional parent)
  "Return the position of NODE in parent's block-kids list.
PARENT can be supplied if known.  Positioned returned is zero-indexed.
Returns 0 if NODE is not a child of a block statement, or if NODE
is not a statement node."
  (let ((p (or parent (js2-node-parent node)))
        (i 0))
    (if (not (js2-block-node-p p))
        i
      (or (position node (js2-block-node-kids p))
          0))))

(defsubst js2-node-short-name (n)
  "Return the short name of node N as a string, e.g. `js2-if-node'."
  (substring (symbol-name (aref n 0))
             (length "cl-struct-")))

(defsubst js2-node-child-list (node)
  "Return the child list for NODE, a lisp list of nodes.
Works for block nodes, array nodes, obj literals, funarg lists,
var decls and try nodes (for catch clauses).  Note that you should call
`js2-block-node-kids' on the function body for the body statements.
Returns nil for zero-length child lists or unsupported nodes."
  (cond
   ((js2-function-node-p node)
    (js2-function-node-params node))
   ((js2-block-node-p node)
    (js2-block-node-kids node))
   ((js2-try-node-p node)
    (js2-try-node-catch-clauses node))
   ((js2-array-node-p node)
    (js2-array-node-elems node))
   ((js2-object-node-p node)
    (js2-object-node-elems node))
   ((js2-call-node-p node)
    (js2-call-node-args node))
   ((js2-new-node-p node)
    (js2-new-node-args node))
   ((js2-var-decl-node-p node)
    (js2-var-decl-node-kids node))
   (t
    nil)))

(defsubst js2-node-set-child-list (node kids)
  "Set the child list for NODE to KIDS."
   (cond
    ((js2-function-node-p node)
     (setf (js2-function-node-params node) kids))
    ((js2-block-node-p node)
     (setf (js2-block-node-kids node) kids))
    ((js2-try-node-p node)
     (setf (js2-try-node-catch-clauses node) kids))
    ((js2-array-node-p node)
     (setf (js2-array-node-elems node) kids))
    ((js2-object-node-p node)
     (setf (js2-object-node-elems node) kids))
    ((js2-call-node-p node)
     (setf (js2-call-node-args node) kids))
    ((js2-new-node-p node)
     (setf (js2-new-node-args node) kids))
    ((js2-var-decl-node-p node)
     (setf (js2-var-decl-node-kids node) kids))
    (t
     (error "Unsupported node type: %s" (js2-node-short-name node))))
   kids)

;; All because Common Lisp doesn't support multiple inheritance for defstructs.
(defconst js2-paren-expr-nodes
  '(cl-struct-js2-array-comp-loop-node
    cl-struct-js2-array-comp-node
    cl-struct-js2-call-node
    cl-struct-js2-catch-node
    cl-struct-js2-do-node
    cl-struct-js2-elem-get-node
    cl-struct-js2-for-in-node
    cl-struct-js2-for-node
    cl-struct-js2-function-node
    cl-struct-js2-if-node
    cl-struct-js2-let-expr-node
    cl-struct-js2-let-stmt-node
    cl-struct-js2-new-node
    cl-struct-js2-paren-node
    cl-struct-js2-switch-node
    cl-struct-js2-while-node
    cl-struct-js2-with-node)
  "Node types that can have a parenthesized child expression.
In particular, nodes that respond to `js2-node-lp' and `js2-node-rp'.")

(defsubst js2-paren-expr-node-p (node)
  "Return t for nodes that typically have a parenthesized child expression.
Useful for computing the indentation anchors for arg-lists and conditions.
Note that it may return a false positive, for instance when NODE is
a `js2-new-node' and there are no arguments or parentheses."
  (memq (aref node 0) js2-paren-expr-nodes))

;; Fake polymorphism... yech.
(defsubst js2-node-lp (node)
  "Return relative left-paren position for NODE, if applicable.
For `js2-elem-get-node' structs, returns left-bracket position.
Note that the position may be nil in the case of a parse error."
  (cond
   ((js2-elem-get-node-p node)
    (js2-elem-get-node-lb node))
   ((js2-loop-node-p node)
    (js2-loop-node-lp node))
   ((js2-function-node-p node)
    (js2-function-node-lp node))
   ((js2-if-node-p node)
    (js2-if-node-lp node))
   ((js2-new-node-p node)
    (js2-new-node-lp node))
   ((js2-call-node-p node)
    (js2-call-node-lp node))
   ((js2-paren-node-p node)
    (js2-node-pos node))
   ((js2-switch-node-p node)
    (js2-switch-node-lp node))
   ((js2-catch-node-p node)
    (js2-catch-node-lp node))
   ((js2-let-expr-node-p node)
    (js2-let-expr-node-lp node))
   ((js2-let-stmt-node-p node)
    (js2-let-stmt-node-lp node))
   ((js2-array-comp-node-p node)
    (js2-array-comp-node-lp node))
   ((js2-with-node-p node)
    (js2-with-node-lp node))
   (t
    (error "Unsupported node type: %s" (js2-node-short-name node)))))

;; Fake polymorphism... blech.
(defsubst js2-node-rp (node)
  "Return relative right-paren position for NODE, if applicable.
For `js2-elem-get-node' structs, returns right-bracket position.
Note that the position may be nil in the case of a parse error."
  (cond
   ((js2-elem-get-node-p node)
    (js2-elem-get-node-lb node))
   ((js2-loop-node-p node)
    (js2-loop-node-rp node))
   ((js2-function-node-p node)
    (js2-function-node-rp node))
   ((js2-if-node-p node)
    (js2-if-node-rp node))
   ((js2-new-node-p node)
    (js2-new-node-rp node))
   ((js2-call-node-p node)
    (js2-call-node-rp node))
   ((js2-paren-node-p node)
    (+ (js2-node-pos node) (js2-node-len node)))
   ((js2-switch-node-p node)
    (js2-switch-node-rp node))
   ((js2-catch-node-p node)
    (js2-catch-node-rp node))
   ((js2-let-expr-node-p node)
    (js2-let-expr-node-rp node))
   ((js2-let-stmt-node-p node)
    (js2-let-stmt-node-rp node))
   ((js2-array-comp-node-p node)
    (js2-array-comp-node-rp node))
   ((js2-with-node-p node)
    (js2-with-node-rp node))
   (t
    (error "Unsupported node type: %s" (js2-node-short-name node)))))

(defsubst js2-node-first-child (node)
  "Returns the first element of `js2-node-child-list' for NODE."
  (car (js2-node-child-list node)))

(defsubst js2-node-last-child (node)
  "Returns the last element of `js2-node-last-child' for NODE."
  (car (last (js2-node-child-list node))))

(defun js2-node-prev-sibling (node)
  "Return the previous statement in parent.
Works for parents supported by `js2-node-child-list'.
Returns nil if NODE is not in the parent, or PARENT is
not a supported node, or if NODE is the first child."
  (let* ((p (js2-node-parent node))
         (kids (js2-node-child-list p))
         (sib (car kids)))
    (while (and kids
                (neq node (cadr kids)))
      (setq kids (cdr kids)
            sib (car kids)))
    sib))

(defun js2-node-next-sibling (node)
  "Return the next statement in parent block.
Returns nil if NODE is not in the block, or PARENT is not
a block node, or if NODE is the last statement."
  (let* ((p (js2-node-parent node))
         (kids (js2-node-child-list p)))
    (while (and kids
                (neq node (car kids)))
      (setq kids (cdr kids)))
    (cadr kids)))

(defun js2-node-find-child-before (pos parent &optional after)
  "Find the last child that starts before POS in parent.
If AFTER is non-nil, returns first child starting after POS.
POS is an absolute buffer position.  PARENT is any node
supported by `js2-node-child-list'.
Returns nil if no applicable child is found."
  (let ((kids (if (js2-function-node-p parent)
                  (js2-block-node-kids (js2-function-node-body parent))
                (js2-node-child-list parent)))
        (beg (if (js2-function-node-p parent)
                 (js2-node-abs-pos (js2-function-node-body parent))
               (js2-node-abs-pos parent)))
        kid
        result
        fn
        (continue t))
    (setq fn (if after '> '<))
    (while (and kids continue)
      (setq kid (car kids))
      (if (funcall fn (+ beg (js2-node-pos kid)) pos)
          (setq result kid
                continue (if after nil t))
        (setq continue (if after t nil)))
      (setq kids (cdr kids)))
    result))

(defun js2-node-find-child-after (pos parent)
  "Find first child that starts after POS in parent.
POS is an absolute buffer position.  PARENT is any node
supported by `js2-node-child-list'.
Returns nil if no applicable child is found."
  (js2-node-find-child-before pos parent 'after))

(defun js2-node-replace-child (pos parent new-node)
  "Replace node at index POS in PARENT with NEW-NODE.
Only works for parents supported by `js2-node-child-list'."
  (let ((kids (js2-node-child-list parent))
        (i 0))
    (while (< i pos)
      (setq kids (cdr kids)
            i (1+ i)))
    (setcar kids new-node)
    (js2-node-add-children parent new-node)))

(defun js2-node-buffer (n)
  "Return the buffer associated with AST N.
Returns nil if the buffer is not set as a property on the root
node, or if parent links were not recorded during parsing."
  (let ((root (js2-node-root n)))
    (and root
         (js2-ast-root-p root)
         (js2-ast-root-buffer root))))

(defsubst js2-block-node-push (n kid)
  "Push js2-node KID onto the end of js2-block-node N's child list.
KID is always added to the -end- of the kids list.
Function also calls `js2-node-add-children' to add the parent link."
  (let ((kids (js2-node-child-list n)))
    (if kids
        (setcdr kids (nconc (cdr kids) (list kid)))
      (js2-node-set-child-list n (list kid)))
    (js2-node-add-children n kid)))

(defun js2-node-string (node)
  (let ((buf (js2-node-buffer node))
        pos)
    (unless buf
      (error "No buffer available for node %s" node))
    (save-excursion
      (set-buffer buf)
      (buffer-substring-no-properties (setq pos (js2-node-abs-pos node))
                                      (+ pos (js2-node-len node))))))

;; Container for storing the node we're looking for in a traversal.
(defvar js2-discovered-node nil)
(make-variable-buffer-local 'js2-discovered-node)

;; Keep track of absolute node position during traversals.
(defvar js2-visitor-offset nil)
(make-variable-buffer-local 'js2-visitor-offset)

(defvar js2-node-search-point nil)
(make-variable-buffer-local 'js2-node-search-point)

(when js2-mode-dev-mode-p
  (defun js2-find-node-at-point ()
    (interactive)
    (let ((node (js2-node-at-point)))
      (message "%s" (or node "No node found at point"))))
  (defun js2-node-name-at-point ()
    (interactive)
    (let ((node (js2-node-at-point)))
      (message "%s" (if node
                        (js2-node-short-name node)
                      "No node found at point.")))))

(defun js2-node-at-point (&optional pos skip-comments)
  "Return AST node at POS, a buffer position, defaulting to current point.
The `js2-mode-ast' variable must be set to the current parse tree.
Signals an error if the AST (`js2-mode-ast') is nil.
Always returns a node - if it can't find one, it returns the root.
If SKIP-COMMENTS is non-nil, comment nodes are ignored."
  (let ((ast js2-mode-ast)
        result)
    (unless ast
      (error "No JavaScript AST available"))
    ;; Look through comments first, since they may be inside nodes that
    ;; would otherwise report a match.
    (setq pos (or pos (point))
          result (if (> pos (js2-node-abs-end ast))
                     ast
                   (if (not skip-comments)
                       (js2-comment-at-point pos))))
    (unless result
      (setq js2-discovered-node nil
            js2-visitor-offset 0
            js2-node-search-point pos)
      (unwind-protect
          (catch 'js2-visit-done
            (js2-visit-ast ast #'js2-node-at-point-visitor))
        (setq js2-visitor-offset nil
              js2-node-search-point nil))
      (setq result js2-discovered-node))
    ;; may have found a comment beyond end of last child node,
    ;; since visiting the ast-root looks at the comment-list last.
    (if (and skip-comments
             (js2-comment-node-p result))
        (setq result nil))
    (or result js2-mode-ast)))

(defun js2-node-at-point-visitor (node end-p)
  (let ((rel-pos (js2-node-pos node))
        abs-pos
        abs-end
        (point js2-node-search-point))
    (cond
     (end-p
      ;; this evaluates to a non-nil return value, even if it's zero
      (decf js2-visitor-offset rel-pos))
     ;; we already looked for comments before visiting, and don't want them now
     ((js2-comment-node-p node)
      nil)
     (t
      (setq abs-pos (incf js2-visitor-offset rel-pos)
            ;; we only want to use the node if the point is before
            ;; the last character position in the node, so we decrement
            ;; the absolute end by 1.
            abs-end (+ abs-pos (js2-node-len node) -1))
      (cond
       ;; If this node starts after search-point, stop the search.
       ((> abs-pos point)
        (throw 'js2-visit-done nil))
       ;; If this node ends before the search-point, don't check kids.
       ((> point abs-end)
        nil)
       (t
        ;; Otherwise point is within this node, possibly in a child.
        (setq js2-discovered-node node)
        t))))))  ; keep processing kids to look for more specific match

(defsubst js2-block-comment-p (node)
  "Return non-nil if NODE is a comment node of format `jsdoc' or `block'."
  (and (js2-comment-node-p node)
       (memq (js2-comment-node-format node) '(jsdoc block))))

;; TODO:  put the comments in a vector and binary-search them instead
(defun js2-comment-at-point (&optional pos)
  "Look through scanned comment nodes for one containing POS.
POS is a buffer position that defaults to current point.
Function returns nil if POS was not in any comment node."
  (let ((ast js2-mode-ast)
        (x (or pos (point)))
        beg
        end)
    (unless ast
      (error "No JavaScript AST available"))
    (catch 'done
      ;; Comments are stored in lexical order.
      (dolist (comment (js2-ast-root-comments ast) nil)
        (setq beg (js2-node-abs-pos comment)
              end (+ beg (js2-node-len comment)))
        (if (and (>= x beg)
                 (<= x end))
            (throw 'done comment))))))

(defun js2-mode-find-enclosing-fn (node)
  (if (js2-ast-root-p node)
      node
    (setq node (js2-node-parent node))
    (while (not (or (js2-ast-root-p node)
                    (js2-function-node-p node)))
      (setq node (js2-node-parent node)))
    node))

(defun js2-mode-find-enclosing-node (beg end)
  "Find script or function fully enclosing BEG and END."
  (let ((node (js2-node-at-point beg))
        pos
        (continue t))
    (while continue
      (if (or (js2-ast-root-p node)
              (and (js2-function-node-p node)
                   (<= (setq pos (js2-node-abs-pos node)) beg)
                   (>= (+ pos (js2-node-len node)) end)))
          (setq continue nil)
        (setq node (js2-node-parent node))))
    node))

(defsubst js2-mode-find-first-stmt (node)
  "Search upward starting from NODE looking for a statement.
For purposes of this function, a `js2-function-node' counts."
  (while (not (or (js2-stmt-node-p node)
                  (js2-function-node-p node)))
    (setq node (js2-node-parent node)))
  node)

(defsubst js2-mode-shift-kids (kids start offset)
  (dolist (kid kids)
    (if (> (js2-node-pos kid) start)
        (incf (js2-node-pos kid) offset))))

(defsubst js2-mode-shift-children (parent start offset)
  "Update start-positions of all children of PARENT beyond START."
  (let ((root (js2-node-root parent)))
    (js2-mode-shift-kids (js2-node-child-list parent) start offset)
    (js2-mode-shift-kids (js2-ast-root-comments root) start offset)))

(defsubst js2-node-is-descendant (node ancestor)
  "Return t if NODE is a descendant of ANCESTOR."
  (while (and node
              (neq node ancestor))
    (setq node (js2-node-parent node)))
  node)

;;; visitor infrastructure

(defun js2-visit-none (node callback)
  "Visitor for AST node that have no node children."
  nil)

(defun js2-print-none (node indent)
  "Visitor for AST node with no printed representation.")

(defun js2-print-body (node indent)
  "Print a statement, or a block without braces."
  (if (js2-block-node-p node)
      (dolist (kid (js2-block-node-kids node))
        (js2-print-ast kid indent))
    (js2-print-ast node indent)))

(defun js2-print-list (args &optional delimiter)
  (loop with len = (length args)
        for arg in args
        for count from 1
        do
        (js2-print-ast arg 0)
        (if (< count len)
            (insert (or delimiter ", ")))))

(defun js2-print-tree (ast)
  "Prints an AST to the current buffer.
Makes `js2-ast-parent-nodes' available to the printer functions."
  (let ((max-lisp-eval-depth (max max-lisp-eval-depth 1500)))
    (js2-print-ast ast)))

(defun js2-print-ast (node &optional indent)
  "Helper function for printing AST nodes.
Requires `js2-ast-parent-nodes' to be non-nil.
You should use `js2-print-tree' instead of this function."
  (let ((printer (get (aref node 0) 'js2-printer))
        (i (or indent 0))
        (pos (js2-node-abs-pos node)))
    ;; TODO:  wedge comments in here somewhere
    (if printer
        (funcall printer node i))))

;; Symbol-table support.  Unfortunately defstruct doesn't support
;; multiple inheritance, so we use delegation:  any node type that
;; introduces a new scope (a script, function or let-node) has
;; an associated scope field containing a `js2-scope', which
;; we must fetch when doing scope operations.  This scope node has
;; two "parent" pointers:  its parent scope node, and its associated
;; AST node.  This lets us climb up the scope chain and then figure
;; out which AST node we're at.

(defstruct (js2-symbol
            (:constructor nil)
            (:constructor make-js2-symbol (decl-type name &optional ast-node)))
  "A symbol table entry."
  ;; One of js2-FUNCTION, js2-LP (for parameters), js2-VAR,
  ;; js2-LET, or js2-CONST
  decl-type
  name  ; string
  ast-node) ; a `js2-node'

(defstruct (js2-scope
            (:constructor nil)
            (:constructor make-js2-scope (&key type
                                               (ast-node nil))))
  ;; The symbol-table is a LinkedHashMap<String,Symbol> in Rhino.
  ;; I don't have one of those handy, so I'll use an alist for now.
  ;; It's as fast as an emacs hashtable for up to about 50 elements,
  ;; and is much lighter-weight to construct (both CPU and mem).
  ;; The keys are interned strings (symbols) for faster lookup.
  ;; Should switch to hybrid alist/hashtable eventually.
  type      ; the AST token type, e.g. js2-FUNCTION
  symbols   ; an alist of (symbol . js2-symbol)
  parent    ; a `js2-scope'
  ast-node)     ; ast node: a `js2-node'

(defun js2-node-scope (node)
  "Return the `js2-scope' for NODE.
Signals an error if NODE is not a scope node."
  (let ((tt (js2-node-type node)))
    (cond
     ;; fake polymorphism
     ((= tt js2-FUNCTION)
      (js2-function-node-scope node))
     ((= tt js2-SCRIPT)
      (js2-script-node-scope node))
     ((= tt js2-BLOCK)
      (js2-block-node-scope node))
     ((js2-loop-node-p node)
      (js2-loop-node-scope node))
     ((= tt js2-LETEXPR)
      (js2-let-expr-node-scope node))
     ((= tt js2-LET)
      (js2-let-stmt-node-scope node))
     (t
      (error "%s is not a scope node" (js2-node-short-name node))))))

(defun js2-node-set-scope (node scope)
  "Set the `js2-scope' for NODE, and return SCOPE.
Signals an error if NODE is not a scope node."
  (let ((tt (js2-node-type node)))
    (cond
     ;; fake polymorphism
     ((= tt js2-FUNCTION)
      (setf (js2-function-node-scope node) scope))
     ((= tt js2-SCRIPT)
      (setf (js2-script-node-scope node) scope))
     ((= tt js2-BLOCK)
      (setf (js2-block-node-scope node) scope))
     ((js2-loop-node-p node)
      (setf (js2-loop-node-scope node) scope))
     ((= tt js2-LETEXPR)
      (setf (js2-let-expr-node-scope node) scope))
     ((= tt js2-LET)
      (setf (js2-let-stmt-node-scope node) scope))
     (t
      (error "%s is not a scope node" (js2-node-short-name node))))
    scope))

(defun js2-get-defining-scope (scope name)
  "Search up scope chain from SCOPE looking for NAME, a string or symbol.
Returns `js2-scope' in which NAME is defined, or nil if not found."
  (let ((sym (if (symbolp name)
                 name
               (intern name)))
        table
        result
        (continue t))
    (while (and scope continue)
      (if (and (setq table (js2-scope-symbols scope))
               (assq sym table))
          (setq continue nil
                result scope)
        (setq scope (js2-scope-parent scope))))
    result))

(defsubst js2-scope-get-symbol (scope name)
  "Return symbol table entry for NAME in SCOPE.
NAME can be a string or symbol.   Returns a `js2-symbol' or nil if not found."
  (and (js2-scope-symbols scope)
       (cdr (assq (if (symbolp name)
                      name
                    (intern name))
                  (js2-scope-symbols scope)))))

(defsubst js2-scope-put-symbol (scope name symbol)
  "Enter SYMBOL into symbol-table for SCOPE under NAME.
NAME can be a lisp symbol or string.  SYMBOL is a `js2-symbol'."
  (let* ((table (js2-scope-symbols scope))
         (sym (if (symbolp name) name (intern name)))
         (entry (assq sym table)))
    (if entry
        (setcdr entry symbol)
      (push (cons sym symbol)
            (js2-scope-symbols scope)))))

(defconst js2-side-effecting-tokens
  (let ((tokens (make-bool-vector js2-num-tokens nil)))
    (dolist (tt (list js2-EXPR_RESULT
                      js2-ASSIGN
                      js2-ASSIGN_ADD
                      js2-ASSIGN_SUB
                      js2-ASSIGN_MUL
                      js2-ASSIGN_DIV
                      js2-ASSIGN_MOD
                      js2-ASSIGN_BITOR
                      js2-ASSIGN_BITXOR
                      js2-ASSIGN_BITAND
                      js2-ASSIGN_LSH
                      js2-ASSIGN_RSH
                      js2-ASSIGN_URSH
                      js2-ENTERWITH
                      js2-LEAVEWITH
                      js2-RETURN
                      js2-GOTO
                      js2-IFEQ
                      js2-IFNE
                      js2-NEW
                      js2-DELPROP
                      js2-SETNAME
                      js2-SETPROP
                      js2-SETELEM
                      js2-CALL
                      js2-THROW
                      js2-RETHROW
                      js2-SETVAR
                      js2-CATCH_SCOPE
                      js2-RETURN_RESULT
                      js2-SET_REF
                      js2-DEL_REF
                      js2-REF_CALL
                      js2-TRY
                      js2-SEMI
                      js2-INC
                      js2-DEC
                      js2-EXPORT
                      js2-IMPORT
                      js2-IF
                      js2-ELSE
                      js2-SWITCH
                      js2-WHILE
                      js2-DO
                      js2-FOR
                      js2-BREAK
                      js2-CONTINUE
                      js2-VAR
                      js2-CONST
                      js2-LET
                      js2-LETEXPR
                      js2-WITH
                      js2-WITHEXPR
                      js2-CATCH
                      js2-FINALLY
                      js2-BLOCK
                      js2-LABEL
                      js2-TARGET
                      js2-LOOP
                      js2-JSR
                      js2-SETPROP_OP
                      js2-SETELEM_OP
                      js2-LOCAL_BLOCK
                      js2-SET_REF_OP
                      js2-YIELD))
      (aset tokens tt t))
    tokens))

(defun js2-node-has-side-effects (node)
  "Return t if NODE has side effects."
  (let ((tt (js2-node-type node))
        expr)
    (cond
     ;; This doubtless needs some work, since EXPR_VOID is used
     ;; in several ways in Rhino, and I may not have caught them all.
     ;; I'll wait for people to notice incorrect warnings.
     ((and (= tt js2-EXPR_VOID)
           (js2-expr-stmt-node-p node))  ; but not if EXPR_RESULT
      (if (setq expr (js2-expr-stmt-node-expr node))
          (js2-node-has-side-effects expr)
        t))
     ((= tt js2-COMMA)
      (if (setq expr (js2-infix-node-right node))
          (js2-node-has-side-effects expr)
        t))
     ((= tt js2-HOOK)
      (and (js2-node-has-side-effects (js2-cond-node-true-expr node))
           (js2-node-has-side-effects (js2-cond-node-false-expr node))))
     ((= tt js2-ERROR) ; avoid cascaded error messages
      nil)
     (t
      (aref js2-side-effecting-tokens tt)))))


(provide 'js2-ast)

;;; js2-ast.el ends here
;;; js2-highlight.el --- JavaScript syntax coloring support

;; Author:  Steve Yegge (steve.yegge@gmail.com)
;; Keywords:  javascript languages

;;; Code:


(defsubst js2-set-face (beg end face &optional record)
  "Fontify a region.  If RECORD is non-nil, record for later."
  (when (plusp js2-highlight-level)
    (setq beg (min (point-max) beg)
          beg (max (point-min) beg)
          end (min (point-max) end)
          end (max (point-min) end))
    (if record
        (push (list beg end face) js2-mode-fontifications)
      (put-text-property beg end 'face face))))

(defsubst js2-set-kid-face (pos kid len face)
  "Set-face on a child node.
POS is absolute buffer position of parent.
KID is the child node.
LEN is the length to fontify.
FACE is the face to fontify with."
  (js2-set-face (+ pos (js2-node-pos kid))
                (+ pos (js2-node-pos kid) (js2-node-len kid))
                face))

(defsubst js2-fontify-kwd (start length)
  (js2-set-face start (+ start length) 'js2-keyword-face))

(defsubst js2-clear-face (beg end)
  (remove-text-properties beg end '(face nil
                                    help-echo nil
                                    point-entered nil
                                    c-in-sws nil)))

(defsubst js2-record-text-property (beg end prop value)
  "Record a text property to set when parsing finishes."
  (push (list beg end prop value) js2-mode-deferred-properties))

(defconst js2-ecma-global-props
  (concat "^"
          (regexp-opt
           '("Infinity" "NaN" "undefined" "arguments") t)
          "$")
  "Value properties of the Ecma-262 Global Object.
Shown at or above `js2-highlight-level' 2.")

;; might want to add the name "arguments" to this list?
(defconst js2-ecma-object-props
  (concat "^"
          (regexp-opt
           '("prototype" "__proto__" "__parent__") t)
          "$")
  "Value properties of the Ecma-262 Object constructor.
Shown at or above `js2-highlight-level' 2.")

(defconst js2-ecma-global-funcs
  (concat
   "^"
   (regexp-opt
    '("decodeURI" "decodeURIComponent" "encodeURI" "encodeURIComponent"
      "eval" "isFinite" "isNaN" "parseFloat" "parseInt") t)
   "$")
  "Function properties of the Ecma-262 Global object.
Shown at or above `js2-highlight-level' 2.")

(defconst js2-ecma-number-props
  (concat "^"
          (regexp-opt '("MAX_VALUE" "MIN_VALUE" "NaN"
                        "NEGATIVE_INFINITY"
                        "POSITIVE_INFINITY") t)
          "$")
  "Properties of the Ecma-262 Number constructor.
Shown at or above `js2-highlight-level' 2.")

(defconst js2-ecma-date-props "^\\(parse\\|UTC\\)$"
  "Properties of the Ecma-262 Date constructor.
Shown at or above `js2-highlight-level' 2.")


(defconst js2-ecma-math-props
  (concat "^"
          (regexp-opt
           '("E" "LN10" "LN2" "LOG2E" "LOG10E" "PI" "SQRT1_2" "SQRT2")
           t)
          "$")
  "Properties of the Ecma-262 Math object.
Shown at or above `js2-highlight-level' 2.")


(defconst js2-ecma-math-funcs
  (concat "^"
          (regexp-opt
           '("abs" "acos" "asin" "atan" "atan2" "ceil" "cos" "exp" "floor"
             "log" "max" "min" "pow" "random" "round" "sin" "sqrt" "tan") t)
          "$")
  "Function properties of the Ecma-262 Math object.
Shown at or above `js2-highlight-level' 2.")

(defconst js2-ecma-function-props
  (concat
   "^"
   (regexp-opt
    '(;; properties of the Object prototype object
      "hasOwnProperty" "isPrototypeOf" "propertyIsEnumerable"
      "toLocaleString" "toString" "valueOf"
      ;; properties of the Function prototype object
      "apply" "call"
      ;; properties of the Array prototype object
      "concat" "join" "pop" "push" "reverse" "shift" "slice" "sort"
      "splice" "unshift"
      ;; properties of the String prototype object
      "charAt" "charCodeAt" "fromCharCode" "indexOf" "lastIndexOf"
      "localeCompare" "match" "replace" "search" "split" "substring"
      "toLocaleLowerCase" "toLocaleUpperCase" "toLowerCase"
      "toUpperCase"
      ;; properties of the Number prototype object
      "toExponential" "toFixed" "toPrecision"
      ;; properties of the Date prototype object
      "getDate" "getDay" "getFullYear" "getHours" "getMilliseconds"
      "getMinutes" "getMonth" "getSeconds" "getTime"
      "getTimezoneOffset" "getUTCDate" "getUTCDay" "getUTCFullYear"
      "getUTCHours" "getUTCMilliseconds" "getUTCMinutes" "getUTCMonth"
      "getUTCSeconds" "setDate" "setFullYear" "setHours"
      "setMilliseconds" "setMinutes" "setMonth" "setSeconds" "setTime"
      "setUTCDate" "setUTCFullYear" "setUTCHours" "setUTCMilliseconds"
      "setUTCMinutes" "setUTCMonth" "setUTCSeconds" "toDateString"
      "toLocaleDateString" "toLocaleString" "toLocaleTimeString"
      "toTimeString" "toUTCString"
      ;; properties of the RegExp prototype object
      "exec" "test"
      ;; SpiderMonkey/Rhino extensions, versions 1.5+
      "toSource" "__defineGetter__" "__defineSetter__"
      "__lookupGetter__" "__lookupSetter__" "__noSuchMethod__"
      "every" "filter" "forEach" "lastIndexOf" "map" "some")
    t)
   "$")
  "Built-in functions defined by Ecma-262 and SpiderMonkey extensions.
Shown at or above `js2-highlight-level' 3.")

(defsubst js2-parse-highlight-prop-get (parent target prop call-p)
  (let ((target-name (and target
                          (js2-name-node-p target)
                          (js2-name-node-name target)))
        (prop-name (if prop (js2-name-node-name prop)))
        (level1 (>= js2-highlight-level 1))
        (level2 (>= js2-highlight-level 2))
        (level3 (>= js2-highlight-level 3))
        pos
        face)
    (when level2
      (if call-p
          (cond
           ((and target prop)
            (cond
             ((and level3 (string-match js2-ecma-function-props prop-name))
              (setq face 'js2-builtin-face))
             ((and target-name prop)
              (cond
               ((string= target-name "Date")
                (if (string-match js2-ecma-date-props prop-name)
                    (setq face 'js2-builtin-face)))
               ((string= target-name "Math")
                (if (string-match js2-ecma-math-funcs prop-name)
                    (setq face 'js2-builtin-face)))))))
           (prop
            (if (string-match js2-ecma-global-funcs prop-name)
                (setq face 'js2-builtin-face))))
        (cond
         ((and target prop)
          (cond
           ((string= target-name "Number")
            (if (string-match js2-ecma-number-props prop-name)
                (setq face 'js2-constant-face)))
           ((string= target-name "Math")
            (if (string-match js2-ecma-math-props prop-name)
                (setq face 'js2-constant-face)))))
         (prop
          (if (string-match js2-ecma-object-props prop-name)
              (setq face 'js2-constant-face)))))
      (when face
        (js2-set-face (setq pos (+ (js2-node-pos parent) ; absolute
                                   (js2-node-pos prop))) ; relative
                      (+ pos (js2-node-len prop))
                      face)))))

(defun js2-parse-highlight-member-expr-node (node)
  "Perform syntax highlighting of EcmaScript built-in properties.
The variable `js2-highlight-level' governs this highighting."
  (let (face target prop name pos end parent call-p callee)
    (cond
     ;; case 1:  simple name, e.g. foo
     ((js2-name-node-p node)
      (setq name (js2-name-node-name node)
            face (if (string-match js2-ecma-global-props name)
                     'js2-constant-face))
      (when face
        (setq pos (js2-node-pos node)
              end (+ pos (js2-node-len node)))
        (js2-set-face pos end face)))

     ;; case 2:  property access or function call
     ((or (js2-prop-get-node-p node)
          ;; highlight function call if expr is a prop-get node
          ;; or a plain name (i.e. unqualified function call)
          (and (setq call-p (js2-call-node-p node))
               (setq callee (js2-call-node-expr node)) ; separate setq!
               (or (js2-prop-get-node-p callee)
                   (js2-name-node-p callee))))
      (setq parent node
            node (if call-p callee node))
      (if (and call-p (js2-name-node-p callee))
          (setq prop callee)
        (setq target (js2-prop-get-node-target node)
              prop (js2-prop-get-node-prop node)))
      (cond
       ((js2-name-node-p target)
        (if (js2-name-node-p prop)
            ;; case 2a:  simple target, simple prop name, e.g. foo.bar
            (js2-parse-highlight-prop-get parent target prop call-p)
          ;; case 2b:  simple target, complex name, e.g. foo.x[y]
          (js2-parse-highlight-prop-get parent target nil call-p)))
       ((js2-name-node-p prop)
        ;; case 2c:  complex target, simple name, e.g. x[y].bar
        (js2-parse-highlight-prop-get parent target prop call-p)))))))

;; source:  http://jsdoc.sourceforge.net/
;; Note - this syntax is for Google's enhanced jsdoc parser that
;; allows type specifications, and needs work before entering the wild.

(defconst js2-jsdoc-param-tag-regexp
  (concat "^\\s-*\\*+\\s-*\\(@"
          "\\(?:param\\|argument\\)"
          "\\)"
          "\\s-*\\({[^}]+}\\)?"         ; optional type
          "\\s-*\\([a-zA-Z0-9_$]+\\)?"  ; name
          "\\>")
  "Matches jsdoc tags with optional type and optional param name.")

(defconst js2-jsdoc-typed-tag-regexp
  (concat "^\\s-*\\*+\\s-*\\(@\\(?:"
          (regexp-opt
           '("requires" "return" "returns" "throw" "throws"))
          "\\)\\)\\s-*\\({[^}]+}\\)?")
  "Matches jsdoc tags with optional type.")

(defconst js2-jsdoc-arg-tag-regexp
  (concat "^\\s-*\\*+\\s-*\\(@\\(?:"
          (regexp-opt
           '("author" "base" "extends" "member" "type" "version"))
          "\\)\\)\\s-+\\([^ \t]+\\)")
  "Matches jsdoc tags with a single argument.")

(defconst js2-jsdoc-empty-tag-regexp
  (concat "^\\s-*\\*+\\s-*\\(@\\(?:"
          (regexp-opt
           '("addon" "class" "constructor" "deprecated" "exec"
             "exception" "fileoverview" "final" "ignore" "private"))
          "\\)\\)\\s-*")
  "Matches empty jsdoc tags.")

(defconst js2-jsdoc-link-tag-regexp
  "{\\(@link\\)\\s-+\\([^#}\n]+\\)\\(#.+\\)?}"
  "Matches a jsdoc link tag.")

(defconst js2-jsdoc-see-tag-regexp
  "^\\s-*\\*+\\s-*\\(@see\\)\\s-+\\([^#}\n]+\\)\\(#.+\\)?"
  "Matches a jsdoc @see tag.")

(defconst js2-jsdoc-html-tag-regexp
  "\\(</?\\)\\([a-zA-Z]+\\)\\s-*\\(/?>\\)"
  "Matches a simple (no attributes) html start- or end-tag.")

(defsubst js2-jsdoc-highlight-helper ()
  (js2-set-face (match-beginning 1)
                (match-end 1)
                'js2-jsdoc-tag-face)
  (if (match-beginning 2)
      (if (save-excursion
            (goto-char (match-beginning 2))
            (= (char-after) ?{))
          (js2-set-face (1+ (match-beginning 2))
                        (1- (match-end 2))
                        'js2-jsdoc-type-face)
        (js2-set-face (match-beginning 2)
                      (match-end 2)
                      'js2-jsdoc-value-face)))
  (if (match-beginning 3)
      (js2-set-face (match-beginning 3)
                    (match-end 3)
                    'js2-jsdoc-value-face)))

(defun js2-highlight-jsdoc (ast)
  "Highlight doc comment tags."
  (let ((comments (js2-ast-root-comments ast))
        beg end)
    (dolist (node comments)
      (when (eq (js2-comment-node-format node) 'jsdoc)
        (setq beg (js2-node-abs-pos node)
              end (+ beg (js2-node-len node)))
        (save-restriction
          (narrow-to-region beg end)
          (dolist (re (list js2-jsdoc-param-tag-regexp
                            js2-jsdoc-typed-tag-regexp
                            js2-jsdoc-arg-tag-regexp
                            js2-jsdoc-link-tag-regexp
                            js2-jsdoc-see-tag-regexp
                            js2-jsdoc-empty-tag-regexp))
          (goto-char beg)
          (while (re-search-forward re nil t)
            (js2-jsdoc-highlight-helper)))
          ;; simple highlighting for html tags
          (goto-char beg)
          (while (re-search-forward js2-jsdoc-html-tag-regexp nil t)
            (js2-set-face (match-beginning 1)
                          (match-end 1)
                          'js2-jsdoc-html-tag-delimiter-face)
            (js2-set-face (match-beginning 2)
                          (match-end 2)
                          'js2-jsdoc-html-tag-name-face)
            (js2-set-face (match-beginning 3)
                          (match-end 3)
                          'js2-jsdoc-html-tag-delimiter-face)))))))

(defun js2-highlight-assign-targets (node left right)
  "Highlight function properties and external variables."
  (let (leftpos end name)
    ;; highlight vars and props assigned function values
    (when (js2-function-node-p right)
      (cond
       ;; var foo = function() {...}
       ((js2-name-node-p left)
        (setq name left))
       ;; foo.bar.baz = function() {...}
       ((and (js2-prop-get-node-p left)
             (js2-name-node-p (js2-prop-get-node-prop left)))
        (setq name (js2-prop-get-node-prop left))))
      (when (and name
                 (js2-get-defining-scope js2-current-scope
                                         (js2-name-node-name name)))
        (js2-set-face (setq leftpos (js2-node-abs-pos name))
                      (+ leftpos (js2-node-len name))
                      'js2-function-name-face
                      'record)))
    ;; highlight external variable assignments
    (when (and js2-highlight-external-variables
               (js2-name-node-p left))
      (unless (js2-get-defining-scope js2-current-scope
                                      (js2-name-node-name left))
        (js2-set-face (setq leftpos (js2-node-abs-pos left))
                      (setq end (+ leftpos (js2-node-len left)))
                      'js2-external-variable-face
                      'record)
        (js2-record-text-property leftpos end 'help-echo
                                  "Undeclared variable")
        (js2-record-text-property leftpos end 'point-entered
                                  #'js2-echo-help)))))

(provide 'js2-highlight)

;;; js2-highlight.el ends here
;;; js2-parse.el --- JavaScript parser

;; Author:  Steve Yegge (steve.yegge@gmail.com)
;; Keywords:  javascript languages

;; Commentary:

;; This is based on Rhino's parser and tries to follow its code
;; structure as closely as practical, so that changes to the Rhino
;; parser can easily be propagated into this code.  However, Rhino
;; does not currently generate a usable AST representation, at least
;; from an IDE perspective, so we build our own more suitable AST.

;; The AST node structures are defined in `js2-ast.el'.
;; Every parser function that creates and returns an AST node has
;; the following responsibilities:

;;   1) set the node start to the absolute buffer start position
;;   2) set the node length to include any closing chars (RC, SEMI)
;;   3) fix up any child-node starts to be relative to this node
;;   4) set any field positions (e.g. keywords) relative to this node
;;   5) report any child nodes with `js2-node-add-children'
;;      (note that this call fixes up start positions by default)

;; The resulting AST has all node start positions relative to the
;; parent nodes; only the root has an absolute start position.

;; Note: fontification is done inline while parsing.  It used to be
;; done in a second pass over the AST, but doing it inline is about
;; twice as fast.  Most of the fontification happens when tokens are
;; scanned, and the parser has a few spots that perform extra
;; fontification.  In addition to speed, a second benefit of inline
;; parsing is that if a long parse is interrupted, everything parsed
;; so far is still fontified.

;; The editing mode that uses this parser, `js2-mode', directs the
;; parser to check periodically for user input.  If user input
;; arrives, the parse is abandoned, except for the highlighting that
;; has occurred so far, and a re-parse is rescheduled for when Emacs
;; becomes idle again.  This works pretty well, but could be better.
;; In particular, when the user input has not resulted in changes to
;; the buffer (for instance, navigation input), the parse tree built
;; so far should not be discarded, and the parse should continue where
;; it left off.  It will be some work to create what amounts to a
;; continuation, but it should not be unreasonably difficult.

;; TODO:
;; - get nested function scopes working (undecl var warning shows prob)
;; - make non-editing input restart parse at previous continuation
;; - in Eclipse, sibling nodes never overlap start/end ranges
;;   - for getters, prop name and function nodes overlap
;;   - should write a debug tree visitor to look for overlaps
;; - mark array and object literals as "destructuring" (node prop?)
;;   so we can syntax-highlight them properly.
;; - figure out a way not to store value in string/name nodes
;;   - needs a solution for synthetic nodes

;;; Code

(eval-and-compile
  (require 'cl))  ; for delete-if


(defconst js2-version "1.7.0"
  "Version of JavaScript supported, plus minor js2 version.")

(defsubst js2-record-comment ()
  (push (make-js2-comment-node :len (- js2-token-end js2-token-beg)
                               :format js2-ts-comment-type)
        js2-scanned-comments)
  (when js2-parse-ide-mode
    (js2-set-face js2-token-beg js2-token-end 'js2-comment-face 'record)
    (when (eq js2-ts-comment-type 'html)
      ;; Tell cc-engine the bounds of the comment.
      (put-text-property js2-token-beg (1- js2-token-end) 'c-in-sws t))))

(defsubst js2-node-end (n)
  "Computes the absolute end of node N.
Use with caution!  Assumes `js2-node-pos' is -absolute-, which
is only true until the node is added to its parent."
  (+ (js2-node-pos n)
     (js2-node-len n)))

;; This function is called depressingly often, so it should be fast.
;; Most of the time it's looking at the same token it peeked before.
(defsubst js2-peek-token ()
  "Returns the next token without consuming it.
If previous token was consumed, calls scanner to get new token.
If previous token was -not- consumed, returns it (idempotent).

This function will not return a newline (js2-EOL) - instead, it
gobbles newlines until it finds a non-newline token, and flags
that token as appearing just after a newline.

This function will also not return a js2-COMMENT.  Instead, it
records comments found in `js2-scanned-comments'.  If the token
returned by this function immediately follows a jsdoc comment,
the token is flagged as such.

Note that this function always returned the un-flagged token!
The flags, if any, are saved in `js2-current-flagged-token'."
  (if (/= js2-current-flagged-token js2-EOF) ; last token not consumed
      js2-current-token  ; most common case - return already-peeked token
    (let ((tt (js2-get-token))          ; call scanner
          saw-comment
          saw-eol
          flags
          face)
      ;; process comments and whitespace
      (while (or (= tt js2-EOL)
                 (= tt js2-COMMENT))
        (if (= tt js2-EOL)
            (setq saw-eol t)
          ;; this is sticky - subsequent whitespace doesn't affect
          ;; the fact that we saw a comment.
          (setq saw-comment t
                saw-eol nil)
          (if js2-record-comments
              (js2-record-comment)))
        (setq tt (js2-get-token)))      ; call scanner

      (setq js2-current-token tt
            js2-current-flagged-token (set-flag tt
                                                (logior (if saw-eol
                                                            js2-ti-after-eol
                                                          0)
                                                        (if saw-comment
                                                            js2-ti-after-comment
                                                          0))))
      ;; perform lexical fontification as soon as token is scanned
      (when js2-parse-ide-mode
        (cond
         ((minusp tt)
          (js2-set-face js2-token-beg js2-token-end 'js2-error-face 'record))
         ((setq face (aref js2-kwd-tokens tt))
          (js2-set-face js2-token-beg js2-token-end face 'record))
         ((and (= tt js2-NAME)
               (equal js2-ts-string "undefined"))
          (js2-set-face js2-token-beg js2-token-end 'js2-constant-face 'record))))
      tt)))  ; return unflagged token

(defsubst js2-peek-flagged-token ()
  "Returns the current token along with any flags set for it."
  (js2-peek-token)
  js2-current-flagged-token)

(defsubst js2-consume-token ()
  (setq js2-current-flagged-token js2-EOF))

(defsubst js2-next-token ()
  (prog1
      (js2-peek-token)
    (js2-consume-token)))

(defsubst js2-next-flagged-token ()
  (js2-peek-token)
  (prog1 js2-current-flagged-token
    (js2-consume-token)))

(defsubst js2-match-token (match)
  "Consume and return t if next token matches MATCH, a bytecode.
Returns nil and consumes nothing if MATCH is not the next token."
  (if (/= (js2-peek-token) match)
      nil
    (js2-consume-token)
    t))

(defsubst js2-peek-token-or-eol ()
  "Return js2-EOL if the current token immediately follows a newline.
Else returns the current token.  Used in situations where we don't
consider certain token types valid if they are preceded by a newline.
One example is the postfix ++ or -- operator, which has to be on the
same line as its operand."
  (let ((tt (js2-peek-token)))
    ;; Check for last peeked token flags
    (if (/= 0 (logand js2-current-flagged-token js2-ti-after-eol))
        js2-EOL
      tt)))

(defsubst js2-set-check-for-label ()
  (if (/= (logand js2-current-flagged-token js2-clear-ti-mask) js2-NAME)
      (js2-code-bug))
  (setq js2-current-flagged-token
        (set-flag js2-current-flagged-token js2-ti-check-label)))

(defsubst js2-must-match (token msg-id &optional pos len)
  "Match next token to token code TOKEN, or record a syntax error.
MSG-ID is the error message to report if the match fails.
Returns t on match, nil if no match."
  (if (js2-match-token token)
      t
    (js2-report-error msg-id nil pos len)
    nil))

(defsubst js2-set-requires-activation ()
  (if (js2-function-node-p js2-current-script-or-fn)
      (setf (js2-function-node-needs-activation js2-current-script-or-fn) t)))

(defsubst js2-set-is-generator ()
  (if (js2-function-node-p js2-current-script-or-fn)
      (setf (js2-function-node-is-generator js2-current-script-or-fn) t)))

(defsubst js2-must-have-xml ()
  (unless js2-compiler-xml-available
    (js2-report-error "msg.XML.not.available")))

(defsubst js2-inside-function ()
  (/= 0 js2-nesting-of-function))

(defsubst js2-push-scope (node)
  "Push `js2-scope' associated with NODE, a `js2-node'.
NODE must either be a scope node supported by `js2-node-scope'
or a `js2-scope' itself."
  (unless (js2-scope-p node)
    (setq node (js2-node-scope node)))
  (if (js2-scope-parent node)
      (js2-code-bug))
  (if (eq js2-current-scope node)
      (js2-code-bug))
  (setf (js2-scope-parent node) js2-current-scope
        js2-current-scope node))

(defsubst js2-pop-scope ()
  (setq js2-current-scope
        (js2-scope-parent js2-current-scope)))

(defsubst js2-enter-loop (loop-node)
  (push loop-node js2-loop-set)
  (push loop-node js2-loop-and-switch-set)
  (setf (js2-loop-node-scope loop-node)
        (make-js2-scope :type js2-LOOP
                        :ast-node loop-node))
  (js2-push-scope loop-node))

(defsubst js2-exit-loop ()
  (setq js2-loop-set (cdr js2-loop-set))
  (setq js2-loop-and-switch-set (cdr js2-loop-and-switch-set))
  (js2-pop-scope))

(defsubst js2-enter-switch (switch-node)
  (push switch-node js2-loop-and-switch-set))

(defsubst js2-exit-switch ()
  (pop js2-loop-and-switch-set))

(defun js2-parse (&optional buf cb)
  "Tells the js2 parser to parse a region of JavaScript.

BUF is a buffer or buffer name containing the code to parse.
Call `narrow-to-region' first to parse only part of the buffer.

The returned AST root node is given some additional properties:
  `node-count' - total number of nodes in the AST
  `buffer' - BUF.  The buffer it refers to may change or be killed,
             so the value is not necessarily reliable.

An optional callback CB can be specified to report parsing
progress.  If `(functionp CB)' returns t, it will be called with
the current line number once before parsing begins, then again
each time the lexer reaches a new line number.

CB can also be a list of the form `(symbol cb ...)' to specify
multiple callbacks with different criteria.  Each symbol is a
criterion keyword, and the following element is the callback to
call

  :line  - called whenever the line number changes
  :token - called for each new token consumed

The list of criteria could be extended to include entering or
leaving a statement, an expression, or a function definition."
  (if (and cb (not (functionp cb)))
      (error "criteria callbacks not yet implemented"))
  (let ((inhibit-point-motion-hooks t)
        (js2-compiler-xml-available (>= js2-language-version 160))
        ;; This is a recursive-descent parser, so give it a big stack.
        (max-lisp-eval-depth (max max-lisp-eval-depth 3000))
        (max-specpdl-size (max max-specpdl-size 3000))
        (case-fold-search nil)
        ast)
    (or buf (setq buf (current-buffer)))
    (setq js2-scanned-comments nil
          js2-parsed-errors nil
          js2-parsed-warnings nil
          js2-imenu-recorder nil)
    (save-excursion
      (set-buffer buf)
      (js2-init-scanner)
      (setq ast (js2-with-unmodifying-text-property-changes
                  (js2-do-parse)))
      (unless js2-ts-hit-eof
        (js2-report-error "msg.syntax"))
      (setf (js2-ast-root-errors ast) js2-parsed-errors
            (js2-ast-root-warnings ast) js2-parsed-warnings)
      (if (>= js2-highlight-level 1)
          (js2-highlight-jsdoc ast))
      (run-hooks 'js2-parse-finished-hook)
      ast)))

;; Corresponds to Rhino's Parser.parse() method.
(defun js2-do-parse ()
  "Parse current buffer starting from current point.
Scanner should be initialized."
  (let ((pos js2-ts-cursor)
        root
        n
        tt
        msg
        (end js2-ts-cursor)) ; in case file is empty
    ;; initialize buffer-local vars
    (setf root (make-js2-ast-root :buffer (buffer-name)
                                  :pos js2-ts-cursor)
          js2-current-script-or-fn root
          js2-current-scope (js2-node-set-scope
                             root
                             (make-js2-scope :type js2-SCRIPT
                                             :ast-node root))
          js2-current-flagged-token js2-EOF
          js2-nesting-of-function 0
          js2-statement-label nil)
    (catch 'break
      (while t
        (setq tt (js2-peek-token))
        (if (<= tt js2-EOF)
            (throw 'break nil))
        (if (= tt js2-FUNCTION)
            (progn
              (js2-consume-token)
              (setq n (js2-parse-function
                       (if js2-called-by-compile-function
                           'FUNCTION_EXPRESSION
                         'FUNCTION_STATEMENT)))
              (js2-parse-record-imenu n))
          ;; not a function - parse a statement
          (setq n (js2-parse-statement)))
        ;; add to script
        (setq end (js2-node-end n))
        (js2-block-node-push root n)))
    (setf (js2-node-pos root) pos
          (js2-node-len root) (- end pos))
    ;; add comments to root in lexical order
    (when js2-scanned-comments
      ;; if we find a comment beyond end of normal kids, use its end
      (setq end (max end (js2-node-end (first js2-scanned-comments))))
      (dolist (comment js2-scanned-comments)
        (push comment (js2-ast-root-comments root))
        (js2-node-add-children root comment))
      (setf (js2-node-len root) (- end pos)))
    root))

(defun js2-parse-function-body ()
  (incf js2-nesting-of-function)
  (let ((pn (make-js2-block-node))  ; starts at LC position
        n
        tt)
    (unwind-protect
        (catch 'break-loop
          (while t
            (setq tt (js2-peek-token))
            (cond
             ((or (= tt js2-ERROR)
                  (= tt js2-EOF)
                  (= tt js2-RC))
               (throw 'break-loop nil))
             ((= tt js2-FUNCTION)
              (js2-consume-token)
              (setq n (js2-parse-function 'FUNCTION_STATEMENT)))
             (t
              (setq n (js2-parse-statement))))
            (js2-block-node-push pn n)))
      (decf js2-nesting-of-function))
    pn))

(defun js2-function-parser ()
  (js2-consume-token)
  (js2-parse-function 'FUNCTION_EXPRESSION_STATEMENT))

;; The function parser is simpler than Rhino's.  It doesn't define
;; symbols, doesn't support the experimental member-expr syntax,
;; doesn't do tree-rewriting for destructuring assignment, and doesn't
;; keep track of dynamic scope.  That stuff can (and should) be done
;; in a separate codegen pass.  Parsing is already hairy enough.

(defun js2-parse-function (function-type)
  "Function parser.  FUNCTION-TYPE is a symbol."
  (let ((pos js2-token-beg)
        name
        fn-node
        function-index
        saved-script-or-fn
        saved-current-scope
        saved-nesting-of-with
        saved-label-set
        saved-loop-set
        saved-loop-and-switch-set
        saved-has-return-value
        saved-end-flags
        body
        n
        n2
        pos2
        len
        tt
        params
        scope
        lp)
    (when (js2-match-token js2-NAME)
      (js2-set-face js2-token-beg js2-token-end 'js2-function-name-face 'record)
      (setq name (make-js2-name-node :pos js2-token-beg
                                     :len (- js2-token-end js2-token-beg))))
    (if (js2-must-match js2-LP "msg.no.paren.parms")
        (setq lp js2-token-beg))

    (setf fn-node (make-js2-function-node :pos pos
                                          :name name
                                          :form function-type
                                          :lp (- lp pos)))

    (if (and name (neq function-type 'FUNCTION_EXPRESSION))
        ;; Function statements define a symbol in the enclosing scope
        (js2-define-symbol js2-FUNCTION (js2-name-node-name name) fn-node))

    (setf scope (make-js2-scope :type js2-FUNCTION
                                :ast-node fn-node)
          (js2-function-node-scope fn-node) scope
          saved-script-or-fn         js2-current-script-or-fn
          js2-current-script-or-fn   fn-node
          saved-current-scope        js2-current-scope
          js2-current-scope          scope
          saved-nesting-of-with      js2-nesting-of-with
          js2-nesting-of-with        0
          saved-label-set            js2-label-set
          js2-label-set              nil
          saved-loop-set             js2-loop-set
          js2-loop-set               nil
          saved-loop-and-switch-set  js2-loop-and-switch-set
          js2-loop-and-switch-set    nil
          saved-has-return-value     js2-has-return-value
          saved-end-flags            js2-end-flags)
    (unwind-protect
        (progn
          ;; parse function parameter list
          (if (js2-match-token js2-RP)
              (setf (js2-function-node-rp fn-node) (- js2-token-beg pos))
            (loop
             do
             (setq tt (js2-peek-token))
             ;; Rhino does some tree rewriting to support destructuring
             ;; assignment; we'll do that later during codegen, and just
             ;; create array-literal or object-literal nodes for any
             ;; destructuring-assignment parameters we find.
             (cond
              ((= tt js2-LB)
               (js2-consume-token)
               (push (js2-parse-array-literal) params))
              ((= tt js2-LC)
               (js2-consume-token)
               (push (js2-parse-object-literal) params))
              (t
               (js2-must-match js2-NAME "msg.no.parm")
               (js2-set-face js2-token-beg js2-token-end
                             'js2-function-param-face 'record)
               (setq pos2 js2-token-beg
                     len (- js2-token-end js2-token-beg)
                     n (make-js2-name-node :pos pos2
                                           :len len)
                     n2 (make-js2-function-arg-node :pos pos2
                                                    :len len
                                                    :value n))
               (js2-define-symbol js2-LP js2-ts-string n2)
               (js2-node-add-children n2 n)
               (push n2 params)))
             while
             (js2-match-token js2-COMMA))
            (if (js2-must-match js2-RP "msg.no.paren.after.parms")
                (setf (js2-function-node-rp fn-node) (- js2-token-beg pos))))
          (dolist (p params)
            (js2-node-add-children fn-node p)
            (push p (js2-function-node-params fn-node)))
          ;; parse function body
          (js2-must-match js2-LC "msg.no.brace.body")
          (setf pos2 js2-token-beg
                body (js2-parse-function-body)
                (js2-node-pos body) pos2
                (js2-node-len body) (- js2-ts-cursor pos2)
                (js2-function-node-body fn-node) body)
          (js2-node-add-children fn-node body)
          (js2-must-match js2-RC "msg.no.brace.after.body" pos2)

          (if (and name
                   (eq function-type 'FUNCTION_EXPRESSION)
                   (null (js2-scope-get-symbol js2-current-scope
                                               (js2-name-node-name name))))
              ;; Function expressions define a name only in the body of the
              ;; function, and only if not hidden by a parameter name
              (js2-define-symbol js2-FUNCTION
                                 (js2-name-node-name name)
                                 fn-node)))
      ;; finally
      (setq js2-has-return-value     saved-has-return-value
            js2-end-flags            saved-end-flags
            js2-loop-and-switch-set  saved-loop-and-switch-set
            js2-loop-set             saved-loop-set
            js2-label-set            saved-label-set
            js2-nesting-of-with      saved-nesting-of-with
            js2-current-script-or-fn saved-script-or-fn
            js2-current-scope        saved-current-scope))

    (setf (js2-node-len fn-node) (- js2-ts-cursor pos))
    (js2-node-add-children fn-node name)
    fn-node))

(defun js2-parse-statements (&optional parent)
  "Parse a statement list.  Last token consumed must be js2-LC.

PARENT can be a `js2-block-node', in which case the statements are
appended to PARENT.  Otherwise a new `js2-block-node' is created
and returned.

This function does not match the closing js2-RC: the caller
matches the RC so it can provide a suitable error message if not
matched.  This means it's up to the caller to set the length of
the node to include the closing RC.  The node start pos is set to
the absolute buffer start position, and the caller should fix it
up to be relative to the parent node.  All children of this block
node are given relative start positions and correct lengths."
  (let ((pn (or (and parent
                     (prog1 parent
                       (setf (js2-node-pos parent) js2-token-beg)))
                (make-js2-block-node)))
        stmt
        tt)
      (while (and (> (setq tt (js2-peek-token)) js2-EOF)
                  (/= tt js2-RC))
        (setq stmt (js2-parse-statement))
        (js2-block-node-push pn stmt))
      pn))

(defun js2-parse-condition ()
  "Parse a parenthesized boolean expression, e.g. in an if- or while-stmt.
The parens are discarded and the expression node is returned.
The `pos' field of the return value is set to an absolute position
that must be fixed up by the caller.
Return value is a list (EXPR LP RP), with absolute paren positions."
  (let (pn lp rp)
    (if (js2-must-match js2-LP "msg.no.paren.cond")
        (setq lp js2-token-beg))
    (setq pn (js2-parse-expr))
    (if (js2-must-match js2-RP "msg.no.paren.after.cond")
        (setq rp js2-token-beg))
    ;; Report strict warning on code like "if (a = 7) ..."
    (if (and (boundp 'js2-strict-cond-assign-warning)
             js2-strict-cond-assign-warning
             (js2-assign-node-p pn))
        (js2-add-strict-warning "msg.equal.as.assign" nil
                                (js2-node-pos pn)
                                (+ (js2-node-pos pn)
                                   (js2-node-len pn))))
    (list pn lp rp)))

(defun js2-parse-statement ()
  (let ((js2-statement-label nil)
        tt pn beg end)
    (when js2-parse-interruptable-p
      (if (zerop (% (incf js2-parse-stmt-count)
                    js2-statements-per-pause))
          (if (input-pending-p)
              (throw 'interrupted t))))
    (setq pn (js2-statement-helper))
    (unless (js2-node-has-side-effects pn)
      (setq end (js2-node-end pn))
      (save-excursion
        (goto-char end)
        (setq beg (max (js2-node-pos pn) (point-at-bol))))
      (js2-add-strict-warning "msg.no.side.effects" nil beg end))
    pn))

;; These correspond to the switch cases in Parser.statementHelper
(defconst js2-parsers
  (let ((parsers (make-vector js2-num-tokens
                                #'js2-parse-expr-stmt)))
    (aset parsers js2-BREAK     #'js2-parse-break)
    (aset parsers js2-CONST     #'js2-parse-const-var)
    (aset parsers js2-CONTINUE  #'js2-parse-continue)
    (aset parsers js2-DEBUGGER  #'js2-parse-debugger)
    (aset parsers js2-DEFAULT   #'js2-parse-default-xml-namespace)
    (aset parsers js2-DO        #'js2-parse-do)
    (aset parsers js2-FOR       #'js2-parse-for)
    (aset parsers js2-FUNCTION  #'js2-function-parser)
    (aset parsers js2-IF        #'js2-parse-if)
    (aset parsers js2-LC        #'js2-parse-block)
    (aset parsers js2-LET       #'js2-parse-let)
    (aset parsers js2-NAME      #'js2-parse-name)
    (aset parsers js2-RETURN    #'js2-parse-ret-yield)
    (aset parsers js2-SEMI      #'js2-parse-semi)
    (aset parsers js2-SWITCH    #'js2-parse-switch)
    (aset parsers js2-THROW     #'js2-parse-throw)
    (aset parsers js2-TRY       #'js2-parse-try)
    (aset parsers js2-VAR       #'js2-parse-const-var)
    (aset parsers js2-WHILE     #'js2-parse-while)
    (aset parsers js2-WITH      #'js2-parse-with)
    (aset parsers js2-YIELD     #'js2-parse-ret-yield)
    parsers)
  "A vector mapping token types to parser functions.")

(defsubst js2-parse-warn-missing-semi (beg end)
  (and js2-mode-show-strict-warnings
       js2-strict-missing-semi-warning
       (not (and js2-missing-semi-one-line-override
                 (save-excursion
                   (goto-char beg)
                   (skip-chars-backward " \t")
                   (eq (char-before) ?{))
                 (save-excursion
                   (goto-char end)
                   (skip-chars-forward " \t")
                   (eq (char-after) ?}))))
       (js2-add-strict-warning
        "msg.missing.semi" nil
        ;; back up to beginning of statement or line
        (max beg (save-excursion
                   (goto-char end)
                   (point-at-bol)))
        end)))

(defconst js2-no-semi-insertion
  (list js2-IF
        js2-SWITCH
        js2-WHILE
        js2-DO
        js2-FOR
        js2-TRY
        js2-WITH
        js2-LC
        js2-ERROR
        js2-SEMI
        js2-FUNCTION)
  "List of tokens that don't do automatic semicolon insertion.")

(defun js2-statement-helper ()
  (let* ((tt (js2-peek-token))
         (first-tt tt)
         (parser (if (= tt js2-ERROR)
                     #'js2-parse-semi
                   (aref js2-parsers tt)))
         pn
         (beg js2-token-beg)
         tt-flagged)

    (setq pn (funcall parser)
          tt-flagged (js2-peek-flagged-token)
          tt (logand tt-flagged js2-clear-ti-mask))

    ;; Don't do auto semi insertion for certain statement types.
    (unless (or (memq first-tt js2-no-semi-insertion)
                ;; check for labeled statement
                (js2-labeled-stmt-node-p pn))
      (cond
       ((= tt js2-SEMI)
        ;; Consume ';' as a part of expression
        (js2-consume-token)
        ;; extend the node bounds to include the semicolon.
        (setf (js2-node-len pn) (- js2-token-end beg)))
       ((memq tt (list js2-ERROR js2-EOF js2-RC))
        ;; Autoinsert ;
        (js2-parse-warn-missing-semi beg (js2-node-end pn)))
       (t
        (if (zerop (logand tt-flagged js2-ti-after-eol))
            ;; Report error if no EOL or autoinsert ';' otherwise
            (js2-report-error "msg.no.semi.stmt")
          (js2-parse-warn-missing-semi beg (js2-node-end pn))))))
    pn))

(defun js2-parse-if ()
  "Parser for if-statement.  Last matched token must be js2-IF."
  (let ((pos js2-token-beg)
        cond
        if-true
        if-false
        else-pos
        end
        pn)
    (js2-consume-token)
    (setq cond (js2-parse-condition)
          if-true (js2-parse-statement)
          if-false (if (js2-match-token js2-ELSE)
                       (progn
                         (setq else-pos (- js2-token-beg pos))
                         (js2-parse-statement)))
          end (js2-node-end (or if-false if-true))
          pn (make-js2-if-node :pos pos
                               :len (- end pos)
                               :condition (car cond)
                               :then-part if-true
                               :else-part if-false
                               :else-pos else-pos
                               :lp (js2-relpos (second cond) pos)
                               :rp (js2-relpos (third cond) pos)))
    (js2-node-add-children pn (car cond) if-true if-false)
    pn))

(defun js2-parse-switch ()
  "Parser for if-statement.  Last matched token must be js2-SWITCH."
  (let ((pos js2-token-beg)
        tt
        pn
        discriminant
        has-default
        case-expr
        case-node
        case-pos
        cases
        stmt
        lp
        rp)
    (js2-consume-token)
    (if (js2-must-match js2-LP "msg.no.paren.switch")
        (setq lp js2-token-beg))
    (setq discriminant (js2-parse-expr)
          pn (make-js2-switch-node :discriminant discriminant
                                   :pos pos
                                   :lp (js2-relpos lp pos)))
    (js2-node-add-children pn discriminant)
    (js2-enter-switch pn)
    (unwind-protect
        (progn
          (if (js2-must-match js2-RP "msg.no.paren.after.switch")
              (setf (js2-switch-node-rp pn) (- js2-token-beg pos)))
          (js2-must-match js2-LC "msg.no.brace.switch")
          (catch 'break
            (while t
              (setq tt (js2-next-token)
                    case-pos js2-token-beg)
              (cond
               ((= tt js2-RC)
                (setf (js2-node-len pn) (- js2-token-end pos))
                (throw 'break nil))  ; done

               ((= tt js2-CASE)
                (setq case-expr (js2-parse-expr))
                (js2-must-match js2-COLON "msg.no.colon.case"))

               ((= tt js2-DEFAULT)
                (if has-default
                    (js2-report-error "msg.double.switch.default"))
                (setq has-default t
                      case-expr nil)
                (js2-must-match js2-COLON "msg.no.colon.case"))

               (t
                (js2-report-error "msg.bad.switch")
                (throw 'break nil)))

              (setq case-node (make-js2-case-node :pos case-pos
                                                  :expr case-expr))
              (js2-node-add-children case-node case-expr)
              (setf (js2-node-len case-node) (- js2-token-end case-pos))
              (while (and (/= (setq tt (js2-peek-token)) js2-RC)
                          (/= tt js2-CASE)
                          (/= tt js2-DEFAULT)
                          (/= tt js2-EOF))
                (setf stmt (js2-parse-statement)
                      (js2-node-len case-node) (- (js2-node-end stmt) case-pos))
                (js2-block-node-push case-node stmt))
              (push case-node cases)))
          (dolist (kid cases)
            (js2-node-add-children pn kid)
            (push kid (js2-switch-node-cases pn)))
          pn)  ; return value
      (js2-exit-switch))))

(defun js2-parse-while ()
  "Parser for while-statement.  Last matched token must be js2-WHILE."
  (let ((pos js2-token-beg)
        (pn (make-js2-while-node))
        cond
        body)
    (js2-consume-token)
    (js2-enter-loop pn)
    (unwind-protect
        (progn
          (setf cond (js2-parse-condition)
                (js2-while-node-condition pn) (car cond)
                body (js2-parse-statement)
                (js2-while-node-body pn) body
                (js2-node-len pn) (- (js2-node-end body) pos)
                (js2-while-node-label pn) js2-statement-label
                (js2-while-node-lp pn) (js2-relpos (second cond) pos)
                (js2-while-node-rp pn) (js2-relpos (third cond) pos))
          (js2-node-add-children pn body (car cond)))
      (js2-exit-loop))
    pn))

(defun js2-parse-do ()
  "Parser for do-statement.  Last matched token must be js2-DO."
  (let ((pos js2-token-beg)
        (pn (make-js2-do-node))
        cond
        body
        end)
    (js2-consume-token)
    (js2-enter-loop pn)
    (unwind-protect
        (progn
          (setq body (js2-parse-statement))
          (js2-must-match js2-WHILE "msg.no.while.do")
          (setf (js2-do-node-while-pos pn) (- js2-token-beg pos)
                cond (js2-parse-condition)
                (js2-do-node-condition pn) (car cond)
                (js2-do-node-body pn) body
                end js2-ts-cursor
                (js2-do-node-label pn) js2-statement-label
                (js2-do-node-lp pn) (js2-relpos (second cond) pos)
                (js2-do-node-rp pn) (js2-relpos (third cond) pos))
          (js2-node-add-children pn (car cond) body))
      (js2-exit-loop))
    ;; Always auto-insert semicolon to follow SpiderMonkey:
    ;; It is required by ECMAScript but is ignored by the rest of
    ;; world, see bug 238945
    (if (js2-match-token js2-SEMI)
        (setq end js2-ts-cursor))
    (setf (js2-node-len pn) (- end pos))
    pn))

(defun js2-parse-for ()
  "Parser for for-statement.  Last matched token must be js2-FOR.
Parses for, for-in, and for each-in statements."
  (let ((for-pos js2-token-beg)
        let-pos
        pn
        is-for-each
        is-for-in
        in-pos
        each-pos
        init  ; Node init is also foo in 'foo in object'
        cond  ; Node cond is also object in 'foo in object'
        incr  ; 3rd section of for-loop initializer
        body
        end
        tt
        expr
        lp
        rp)
    (js2-consume-token)
    ;; See if this is a for each () instead of just a for ()
    (when (js2-match-token js2-NAME)
      (if (string= "each" js2-ts-string)
          (setq is-for-each t
                each-pos (- js2-token-beg for-pos)) ; relative
        (js2-report-error "msg.no.paren.for")))

    (if (js2-must-match js2-LP "msg.no.paren.for")
        (setq lp (- js2-token-beg for-pos)))
    (setq tt (js2-peek-token))

    ;; parse init clause
    (let ((js2-in-for-init t))  ; set as dynamic variable
      (if (= tt js2-SEMI)
          (setq init (make-js2-empty-expr-node))
        (if (or (= tt js2-VAR) (= tt js2-LET))
            (progn
              (js2-consume-token)
              (setq let-pos js2-token-beg
                    expr (js2-parse-variables tt))
              (if (= tt js2-VAR)
                  (setq init expr)
                (prog1
                    (setq init
                          (make-js2-let-expr-node :pos let-pos
                                                  :len (- (js2-node-end expr)
                                                          let-pos)
                                                  :vars expr))
                  (js2-node-add-children init expr))))
          (setq init (js2-parse-expr)))))

    (if (js2-match-token js2-IN)
        ;; 'cond' is the object over which we're iterating
        (setq is-for-in t
              in-pos (- js2-token-beg for-pos)
              cond (js2-parse-expr))
      ;; else ordinary for loop - parse cond and incr
      (js2-must-match js2-SEMI "msg.no.semi.for")
      (setq cond (if (= (js2-peek-token) js2-SEMI)
                     (make-js2-empty-expr-node) ; no loop condition
                   (js2-parse-expr)))
      (js2-must-match js2-SEMI "msg.no.semi.for.cond")
      (setq incr (if (= (js2-peek-token) js2-RP)
                     (make-js2-empty-expr-node)
                   (js2-parse-expr))))

    (if (js2-must-match js2-RP "msg.no.paren.for.ctrl")
        (setq rp (- js2-token-beg for-pos)))
    (if (not is-for-in)
        (setq pn (make-js2-for-node :init init
                                    :condition cond
                                    :update incr
                                    :lp lp
                                    :rp rp))
      ;; cond could be null if 'in obj' got eaten by the init node.
      (when (null cond)
        (setq cond (js2-infix-node-right init)
              init (js2-infix-node-left init)))
      (setq pn (make-js2-for-in-node :iterator init
                                     :object cond
                                     :in-pos in-pos
                                     :foreach-p is-for-each
                                     :each-pos each-pos
                                     :lp lp
                                     :rp rp)))
    (unwind-protect
        (progn
          (js2-enter-loop pn)
          ;; We have to parse the body -after- creating the loop node,
          ;; so that the loop node appears in the js2-loop-set, allowing
          ;; break/continue statements to find the enclosing loop.
          (setf body (js2-parse-statement)
                (js2-loop-node-label pn) js2-statement-label
                (js2-loop-node-body pn) body
                (js2-node-pos pn) for-pos
                end (js2-node-end body)
                (js2-node-len pn) (- end for-pos))
          (js2-node-add-children pn init cond incr body))
      ;; finally
      (js2-exit-loop))
    pn))

(defun js2-parse-try ()
  "Parser for try-statement.  Last matched token must be js2-TRY."
  (let ((try-pos js2-token-beg)
        try-end
        try-block
        catch-blocks
        finally-block
        saw-default-catch
        peek
        var-name
        catch-cond
        catch-node
        guard-kwd
        catch-pos
        pn
        block
        lp
        rp)
    (js2-consume-token)
    (if (/= (js2-peek-token) js2-LC)
        (js2-report-error "msg.no.brace.try"))
    (setq try-block (js2-parse-statement)
          peek (js2-peek-token))
    (cond
     ((= peek js2-CATCH)
      (while (js2-match-token js2-CATCH)
        (setq guard-kwd nil
              catch-cond nil
              catch-pos js2-token-beg)
        (if saw-default-catch
            (js2-report-error "msg.catch.unreachable"))
        (if (js2-must-match js2-LP "msg.no.paren.catch")
            (setq lp (- js2-token-beg catch-pos)))
        (js2-must-match js2-NAME "msg.bad.catchcond")
        (setq var-name (make-js2-name-node))
        (if (js2-match-token js2-IF)
            (setq guard-kwd (- js2-token-beg catch-pos)
                  catch-cond (js2-parse-expr))
          (setq saw-default-catch t))
        (if (js2-must-match js2-RP "msg.bad.catchcond")
            (setq rp (- js2-token-beg catch-pos)))
        (js2-must-match js2-LC "msg.no.brace.catchblock")
        (setq block (js2-parse-statements)
              catch-node (make-js2-catch-node :var-name var-name
                                              :guard-expr catch-cond
                                              :guard-kwd guard-kwd
                                              :pos catch-pos
                                              :block block
                                              :lp lp
                                              :rp rp))
        (js2-must-match js2-RC "msg.no.brace.after.body")
        (setf (js2-node-len block) (- js2-ts-cursor (js2-node-pos block)))
        (js2-node-add-children catch-node
                               var-name
                               catch-cond
                               block)
        (push catch-node catch-blocks)
        (setf try-end js2-token-end
              (js2-node-len catch-node) (- js2-ts-cursor catch-pos))))
     ((/= peek js2-FINALLY)
      (js2-must-match js2-FINALLY "msg.try.no.catchfinally"
                      (js2-node-pos try-block)
                      (- (setq try-end (js2-node-end try-block))
                         (js2-node-pos try-block)))))

    (when (js2-match-token js2-FINALLY)
      (setq catch-pos js2-token-beg
            block (js2-parse-statement)
            try-end (js2-node-end block)
            finally-block (make-js2-finally-node :pos catch-pos
                                                 :len (- try-end catch-pos)
                                                 :block block))
      (js2-node-add-children finally-block block))
    (setq pn (make-js2-try-node :pos try-pos
                                :len (- try-end try-pos)
                                :try-block try-block
                                :finally-block finally-block))
    (js2-node-add-children pn try-block finally-block)
    (dolist (cb catch-blocks)
      (js2-node-add-children pn cb)
      (push cb (js2-try-node-catch-clauses pn)))
    pn))

(defun js2-parse-throw ()
  "Parser for throw-statement.  Last matched token must be js2-THROW."
  (let ((pos js2-token-beg)
        expr
        node)
    (js2-consume-token)
    (if (= (js2-peek-token-or-eol) js2-EOL)
        ;; ECMAScript does not allow new lines before throw expression,
        ;; see bug 256617
        (js2-report-error "msg.bad.throw.eol"))
    (setq expr (js2-parse-expr)
          node (make-js2-throw-node :pos pos
                                    :len (- (js2-node-end expr) pos)
                                    :expr expr))
    (js2-node-add-children node expr)
    node))

(defsubst js2-match-jump-label-name ()
  "If we match a js2-NAME, return the labeled statement for that label.
If the name does not match an existing label, reports an error and returns nil.
Returns the `js2-labeled-stmt-node', or nil if the token was not a name.
Side effect:  sets token information for the label identifier."
  (let ((tt (js2-peek-token-or-eol))
        label
        name)
    (when (= tt js2-NAME)
      (js2-consume-token)
      (setq name js2-ts-string
            label (assoc name js2-label-set))
      (if (null label)
          (progn
            (js2-report-error "msg.undef.label")
            nil)
        label))))

(defun js2-parse-break ()
  "Parser for break-statement.  Last matched token must be js2-BREAK."
  (let ((pos js2-token-beg)
        (end js2-token-end)
        break-target ; statement to break from
        break-label  ; in "break foo", name-node representing the foo
        break-node)
    (js2-consume-token)
    ;; matchJumpLabelName only matches if there is one
    (setq break-target (js2-match-jump-label-name))
    (if break-target
        (setq break-label (make-js2-name-node)
              end (js2-node-end break-label))
      ;; no break target specified - try for innermost enclosing loop/switch
      (if (null js2-loop-and-switch-set)
          (js2-report-error "msg.bad.break" nil pos (length "break"))
        (setq break-target (first js2-loop-and-switch-set))))
    (setq break-node (make-js2-break-node :pos pos
                                          :len (- end pos)
                                          :label break-label
                                          :target break-target))
    (js2-node-add-children break-node break-label)
    break-node))

(defun js2-parse-continue ()
  "Parser for continue-statement.  Last matched token must be js2-CONTINUE."
  (let ((pos js2-token-beg)
        label   ; optional user-specified label, a `js2-name-node'
        target  ; a `js2-loop-node' or `js2-labeled-stmt-node' w/ loop stmt
        node
        (end js2-token-end))
    (js2-consume-token)
    ;; matchJumpLabelName only matches if there is one
    (setq target (js2-match-jump-label-name))  ; a `js2-label-node' or nil
    (cond
     ((null target)
      (if (null js2-loop-set)
          (js2-report-error "msg.continue.outside" nil pos (length "continue"))
        (setq target (first js2-loop-set))))  ; innermost enclosing loop
     (t
      (setq label (make-js2-name-node)
            end (js2-node-end label))
      (unless (or (js2-loop-node-p target)
                  (and (js2-labeled-stmt-node-p target)
                       (js2-loop-node-p (js2-labeled-stmt-node-stmt target))))
        (js2-report-error "msg.continue.nonloop"))))
    (setq node (make-js2-continue-node :pos pos
                                       :len (- end pos)
                                       :label label
                                       :target target))
    (js2-node-add-children node label)
    node))

(defun js2-parse-with ()
  "Parser for with-statement.  Last matched token must be js2-WITH."
  (js2-consume-token)
  (let ((pos js2-token-beg)
        obj
        body
        node
        end
        lp
        rp)
    (if (js2-must-match js2-LP "msg.no.paren.with")
        (setq lp js2-token-beg))
    (setq obj (js2-parse-expr))
    (if (js2-must-match js2-RP "msg.no.paren.after.with")
        (setq rp js2-token-beg))
    (incf js2-nesting-of-with)
    (unwind-protect
        (setq body (js2-parse-statement)
              end (js2-node-end body))
      (decf js2-nesting-of-with))
    (setq node (make-js2-with-node :pos pos
                                   :len (- end pos)
                                   :object obj
                                   :body body
                                   :lp (js2-relpos lp pos)
                                   :rp (js2-relpos rp pos)))
    (js2-node-add-children node obj body)
    node))

(defun js2-parse-const-var ()
  "Parser for var- or const-statement.
Last matched token must be js2-CONST or js2-VAR."
  (let ((tt (js2-peek-token))
        (pos js2-token-beg)
        expr
        pn)
    (js2-consume-token)
    (setq expr (js2-parse-variables tt)
          pn (make-js2-expr-stmt-node :pos pos
                                      :len (- (js2-node-end expr) pos)
                                      :expr expr))
    (js2-node-add-children pn expr)
    pn))

(defun js2-parse-let ()
  "Parser for let-statement.  Last matched token must be js2-LET."
  (js2-consume-token)
  (let ((let-pos js2-token-beg)
        (tt (js2-peek-token))
        expr
        pn)
    (if (= tt js2-LP)
        (js2-parse-let-stmt t let-pos)  ; return value
      ;; Else we're looking at a statement like let x = 6, y = 7;
      ;; Rhino just returns a variables Node of type Token.LET,
      ;; but we'll wrap it in a let-stmt without a block.
      (setq expr (js2-parse-variables js2-LET)
            pn (make-js2-let-stmt-node :pos let-pos
                                       :len (- (js2-node-end expr) let-pos)
                                       :vars expr))
      (js2-node-add-children pn expr)
      pn)))

(defun js2-parse-ret-yield ()
  (js2-parse-return-or-yield (js2-peek-token) nil))

(defconst js2-parse-return-stmt-enders
  (list js2-SEMI js2-RC js2-EOF js2-EOL js2-ERROR js2-RB js2-RP js2-YIELD))

(defsubst js2-now-all-set (before after mask)
  "Return whether or not the bits in the mask have changed to all set.
BEFORE is bits before change, AFTER is bits after change, and MASK is
the mask for bits.  Returns t if all the bits in the mask are set in AFTER
but not BEFORE."
  (and (/= (logand before mask) mask)
       (= (logand after mask) mask)))

(defun js2-parse-return-or-yield (tt expr-context)
  (let ((pos js2-token-beg)
        (end js2-token-end)
        (before js2-end-flags)
        (inside-function (js2-inside-function))
        e
        ret
        name)
    (unless inside-function
      (js2-report-error (if (= tt js2-RETURN)
                            "msg.bad.return"
                          "msg.bad.yield")))
    (js2-consume-token)
    ;; This is ugly, but we don't want to require a semicolon.
    (unless (memq (js2-peek-token-or-eol) js2-parse-return-stmt-enders)
      (setq e (js2-parse-expr)
            end (js2-node-end e)))
    (cond
     ((= tt js2-RETURN)
      (when inside-function
        (if (null e)
            (setq js2-end-flags (set-flag js2-end-flags js2-end-returns))
          (setq js2-end-flags (set-flag js2-end-flags js2-end-returns-value)
                js2-has-return-value t)))
      (setq ret (make-js2-return-node :pos pos
                                      :len (- end pos)
                                      :retval e))
      (js2-node-add-children ret e)
      ;; see if we need a strict mode warning
      (if (and inside-function
               js2-strict-inconsistent-return-warning
               (js2-now-all-set before js2-end-flags
                                (logior js2-end-returns js2-end-returns-value)))
          (js2-add-strict-warning "msg.return.inconsistent" nil pos end)))
     (t
      (setq js2-end-flags (set-flag js2-end-flags js2-end-yields)
            ret (make-js2-yield-node :pos pos
                                     :len (- end pos)
                                     :value e))
      (js2-node-add-children ret e)
      (unless expr-context
        (setq e ret
              ret (make-js2-expr-stmt-node :pos pos
                                           :len (- end pos)
                                           :expr e))
        (js2-node-add-children ret e))
      (js2-set-requires-activation)
      (js2-set-is-generator)))

    ;; see if we are mixing yields and value returns.
    (when (and inside-function
               (js2-now-all-set before js2-end-flags
                                (logior js2-end-yields js2-end-returns-value)))
      (setq name (js2-function-node-name js2-current-script-or-fn))
      (if name
          (setq name (js2-name-node-name name)))  ; get string from name-node
      (if (zerop (length name))
          (js2-report-error "msg.anon.generator.returns" nil pos end)
        (js2-report-error "msg.generator.returns" name pos end)))

    ret))

(defun js2-parse-debugger ()
  (js2-consume-token)
  (make-js2-debugger-node))

(defun js2-parse-default-xml-namespace ()
  "Parse a `default xml namespace = <expr>' e4x statement."
  (let ((pos js2-token-beg)
        end expr unary es)
    (js2-consume-token)
    (js2-must-have-xml)
    (js2-set-requires-activation)
    (unless (and (js2-match-token js2-NAME)
                 (string= js2-ts-string "xml"))
      (js2-report-error "msg.bad.namespace" nil pos js2-ts-cursor))
    (unless (and (js2-match-token js2-NAME)
                 (string= js2-ts-string "namespace"))
      (js2-report-error "msg.bad.namespace" nil pos js2-ts-cursor))
    (unless (js2-match-token js2-ASSIGN)
      (js2-report-error "msg.bad.namespace" nil pos js2-ts-cursor))
    (setq expr (js2-parse-expr)
          end (js2-node-end expr)
          unary (make-js2-unary-node :type js2-DEFAULTNAMESPACE
                                     :pos pos
                                     :len (- end pos)
                                     :operand expr))
    (js2-node-add-children unary expr)
    (make-js2-expr-stmt-node :pos pos
                             :len (- end pos)
                             :expr unary)))

(defun js2-parse-block ()
  "Parser for a curly-delimited statement block.
Last token matched must be js2-LC."
  (let* ((pos js2-token-beg)
         (block (make-js2-block-node :pos pos)))
    (js2-consume-token)
    (setf (js2-block-node-scope block) (make-js2-scope :type js2-BLOCK
                                                       :ast-node block))
    (js2-push-scope block)
    (unwind-protect
        (progn
          (js2-parse-statements block)
          (js2-must-match js2-RC "msg.no.brace.block")
          (setf (js2-node-len block) (- js2-token-end pos)))
      (js2-pop-scope))
    block))

;; for js2-ERROR too, to have a node for error recovery to work on
(defun js2-parse-semi ()
  "Parse a statement or handle an error.
Last matched token is js-SEMI or js-ERROR."
  (let ((tt (js2-peek-token)))
    (js2-consume-token)
    (if (= tt js2-SEMI)
        (make-js2-empty-stmt-node :len 1)
      (make-js2-error-node :len 1))))

(defun js2-parse-name ()
  "Parser for identifier.  Last token matched must be js2-NAME.

If the name is not followed by a colon, we return the name as a
`js2-expr-stmt-node'.

If it's followed by a colon, we parse the statement following the
colon and return the whole thing as a `js2-labeled-stmt-node'.
This node is used to accumulate any consecutive labels we find.
We store this node in buffer-local var `js2-statement-label' so
that if the non-label statement following the label(s) is a loop,
the loop can be told its own label for break/continue statements.

This strategy came from Rhino, presumably via SpiderMonkey."
  (let ((pos js2-token-beg)
        (end js2-token-end)
        (name js2-ts-string)
        stmt
        pn)
    (js2-set-check-for-label)
    (setq pn (js2-parse-expr))
    (if (/= (js2-node-type pn) js2-LABEL)
        (progn
          ;; Parsed non-label expression - wrap with statement node.
          (setq pn (make-js2-expr-stmt-node :pos pos
                                            :len (js2-node-len pn)
                                            :type (if (js2-inside-function)
                                                      js2-EXPR_VOID
                                                    js2-EXPR_RESULT)
                                            :expr pn))
          (js2-node-add-children pn (js2-expr-stmt-node-expr pn)))
      ;; Else parsed a label.  First add to buffer-local label set.
      (js2-consume-token)
      (if (assoc name js2-label-set)
          (js2-report-error "msg.dup.label"))
      (if (null js2-statement-label)
          (setq js2-statement-label
                (make-js2-labeled-stmt-node :labels (list pn)
                                            :pos (js2-node-pos pn)))
        (js2-labeled-stmt-node-add-label js2-statement-label pn))
      (js2-node-add-children js2-statement-label pn)

      ;; Add one reference to the bundle per label in `js2-label-set'
      (push (cons name js2-statement-label) js2-label-set)

      ;; Parse the following statement, then remove label from label set.
      (unwind-protect
          (setf stmt (js2-statement-helper))
        (setq js2-label-set (delete-if (lambda (entry)
                                         (string= (car entry) name))
                                       js2-label-set)))
      ;; At this point we've got at least one label in `js2-statement-label'
      ;; and we've parsed stmt, a statement.  stmt may be a labeled statement
      ;; node, in which case we just pass it up tail-recursively.  Otherwise
      ;; we save it in the accumulator labled-stmt node and return that.
      (if (js2-labeled-stmt-node-p stmt)
          (setq pn stmt)  ; return tail-recursively
        (setf (js2-labeled-stmt-node-stmt js2-statement-label) stmt
              ;; set entire length now that stmt is parsed
              (js2-node-len js2-statement-label)
              (- (js2-node-end stmt)
                 (js2-node-pos js2-statement-label)))
        (js2-node-add-children js2-statement-label stmt)
        (setq pn js2-statement-label)))      ; return `js2-labeled-stmt-node'
    pn))

(defun js2-parse-expr-stmt ()
  (let* ((pos js2-token-beg)
         (expr (js2-parse-expr))
         (pn (make-js2-expr-stmt-node :pos pos
                                      :len (js2-node-len expr)
                                      :type (if (js2-inside-function)
                                                js2-EXPR_VOID
                                              js2-EXPR_RESULT)
                                      :expr expr)))
    (js2-node-add-children pn expr)
    pn))

(defun js2-parse-variables (decl-type)
  "Parse a comma-separated list of variable declarations.
Could be a 'var', 'const' or 'let' stmt/expr or for-loop initializer.
DECL-TYPE is a token value: either VAR, CONST, or LET depending on context.
For 'var' or 'const', the keyword must be the token last scanned.
Returns the parsed `js2-var-decl-node' expression node.  If var/const, the
node length includes the var/const keyword.  If it's a let-stmt/expr, then
the node position coincides with the first var-init child."
  (let* ((pos (and (/= decl-type js2-LET) js2-token-beg))
         (result (make-js2-var-decl-node :decl-type decl-type
                                         :pos pos))
         destructuring-init
         destructuring
         kid-pos
         s
         tt
         init
         name
         node
         end
         nbeg nend
         (continue t))
    ;; Example:
    ;; var foo = {a: 1, b: 2}, bar = [3, 4];
    ;; var {b: s2, a: s1} = foo, x = 6, y, [s3, s4] = bar;
    (while continue
      (setq destructuring nil
            s nil
            tt (js2-peek-token)
            kid-pos js2-token-beg
            init nil)
      (unless pos
        (setf pos kid-pos
              (js2-node-pos result) kid-pos))
      (if (or (= tt js2-LB) (= tt js2-LC))
          ;; Destructuring assignment, e.g., var [a, b] = ...
          (setq destructuring (js2-parse-primary-expr))
        ;; Simple variable name
        (js2-must-match js2-NAME "msg.bad.var")
        (setq name (make-js2-name-node)
              nbeg js2-token-beg
              nend js2-token-end)
        (js2-define-symbol decl-type js2-ts-string name))
      (setq end js2-token-end)

      (when (js2-match-token js2-ASSIGN)
        (setq init (js2-parse-assign-expr)
              end (js2-node-end init))
        (if (js2-object-node-p init)
            (js2-parse-record-imenu init name)))

      (when nbeg
        (js2-set-face nbeg nend
                      (if (js2-function-node-p init)
                          'js2-function-name-face
                        'js2-variable-name-face)
                      'record))
      (if destructuring
          (progn
            (if (null init)
                ;; for (var [k, v] in foo) is initialized differently
                (unless js2-in-for-init
                  (js2-report-error "msg.destruct.assign.no.init")))
            (setq node (make-js2-destructuring-init-node :pos kid-pos
                                                         :len (- end kid-pos)
                                                         :lhs destructuring
                                                         :initializer init))
            (js2-node-add-children node destructuring init))
        ;; simple variable, possibly with initializer
        (setq node (make-js2-var-init-node :pos kid-pos
                                           :len (- end kid-pos)
                                           :name name
                                           :initializer init))
        (js2-node-add-children node name init))

      (js2-block-node-push result node)
      (unless (js2-match-token js2-COMMA)
        (setq continue nil)))
    (setf (js2-node-len result) (- end pos))
    result))

(defun js2-parse-let-stmt (&optional stmt-p let-pos)
  (let ((pos (or let-pos js2-token-beg))
        (scope (make-js2-scope :type js2-LET))
        pn vars stmt expr
        pos2 len lp rp)
    (if (js2-must-match js2-LP "msg.no.paren.after.let")
        (setq lp js2-token-beg))
    (js2-push-scope scope)
    (unwind-protect
        (progn
          (setq vars (js2-parse-variables js2-LET))
          (if (js2-must-match js2-RP "msg.no.paren.let")
              (setq rp js2-token-beg))
          (if (and stmt-p (= (js2-peek-token) js2-LC))
              ;; let statement
              (progn
                (js2-consume-token)
                (setf pos2 js2-token-beg ; pos stmt at LC
                      stmt (js2-parse-statements)
                      (js2-node-len stmt) (- js2-ts-cursor pos2)
                      pn (make-js2-let-stmt-node :pos pos
                                                 :vars vars
                                                 :body stmt
                                                 :lp (js2-relpos lp pos)
                                                 :rp (js2-relpos rp pos))
                      (js2-let-stmt-node-scope pn) scope
                      (js2-scope-ast-node scope) pn)
                (js2-node-add-children pn vars stmt)
                (js2-must-match js2-RC "msg.no.curly.let")
                (setf (js2-node-len pn) (- js2-ts-cursor pos)))
            ;; let expression
            (setf expr (js2-parse-expr)
                  (js2-scope-type scope) js2-LETEXPR
                  pn (make-js2-let-expr-node :pos pos
                                             :len (- (js2-node-end expr) pos)
                                             :vars vars
                                             :expr expr
                                             :lp (js2-relpos lp pos)
                                             :rp (js2-relpos rp pos))
                  (js2-let-expr-node-scope pn) scope
                  (js2-scope-ast-node scope) pn)
            (js2-node-add-children pn vars expr)
            (when stmt-p
              ;; let expression in statement context
              (setq stmt (make-js2-expr-stmt-node :pos pos
                                                  :len (js2-node-len pn)
                                                  :type (if (js2-inside-function)
                                                            js2-EXPR_VOID
                                                          js2-EXPR_RESULT)
                                                  :expr pn))
              (js2-node-add-children stmt pn)
              (setq pn stmt))))
      (js2-pop-scope))
    pn))

(defsubst js2-define-new-symbol (decl-type name node)
  (js2-scope-put-symbol js2-current-scope
                        name
                        (make-js2-symbol decl-type name node)))

(defun js2-define-symbol (decl-type name &optional node)
  "Define a symbol in the current scope.
If NODE is non-nil, it is the AST node associated with the symbol."
  (let* ((defining-scope (js2-get-defining-scope js2-current-scope name))
         (symbol (if defining-scope
                     (js2-scope-get-symbol defining-scope name)))
         (sdt (if symbol (js2-symbol-decl-type symbol))))
    (cond
     ((and symbol                       ; already defined
           ;; new version is const
           (or (= decl-type js2-CONST)
               ;; old version is const
               (= sdt js2-CONST)
               ;; two let-bound vars in this block have same name
               (and (= decl-type js2-LET)
                    (eq defining-scope js2-current-scope))))
      (js2-report-error
       (cond
        ((= sdt js2-CONST) "msg.const.redecl")
        ((= sdt js2-LET) "msg.let.redecl")
        ((= sdt js2-VAR) "msg.var.redecl")
        ((= sdt js2-FUNCTION) "msg.function.redecl")
        (t "msg.parm.redecl"))
       name))

     ((= decl-type js2-LET)
      (js2-define-new-symbol decl-type name node))

     ((or (= decl-type js2-VAR)
          (= decl-type js2-CONST)
          (= decl-type js2-FUNCTION))
      (if symbol
          (if (= sdt js2-VAR)
              (js2-add-strict-warning "msg.var.redecl" name)
            (if (= sdt js2-LP)
                (js2-add-strict-warning "msg.var.hides.arg" name)))
        (js2-scope-put-symbol js2-current-scope
                              name
                              (make-js2-symbol decl-type name node))))
     ((= decl-type js2-LP)
      (if symbol
          ;; must be duplicate parameter. Second parameter hides the
          ;; first, so go ahead and add the second pararameter
          (js2-report-warning "msg.dup.parms" name))
      (js2-define-new-symbol decl-type name node))
     (t
      (js2-code-bug)))))

(defun js2-parse-expr ()
  (let* ((pn (js2-parse-assign-expr))
         (pos (js2-node-pos pn))
         left
         right
         op-pos)
    (while (js2-match-token js2-COMMA)
      (setq op-pos (- js2-token-beg pos))  ; relative
      (unless (js2-node-has-side-effects pn)
        (js2-add-strict-warning "msg.no.side.effects" nil
                                pos
                                (js2-node-end pn)))
      (if (= (js2-peek-token) js2-YIELD)
          (js2-report-error "msg.yield.parenthesized"))
      (setq right (js2-parse-assign-expr)
            left pn
            pn (make-js2-infix-node :type js2-COMMA
                                    :pos pos
                                    :len (- js2-ts-cursor pos)
                                    :op-pos op-pos
                                    :left left
                                    :right right))
      (js2-node-add-children pn left right))
    pn))

(defun js2-parse-assign-expr ()
  (let ((tt (js2-peek-token))
        (pos js2-token-beg)
        pn
        left
        right
        op-pos)
    (if (= tt js2-YIELD)
        (js2-parse-return-or-yield tt t)
      ;; not yield - parse assignment expression
      (setq pn (js2-parse-cond-expr)
            tt (js2-peek-token))
      (when (and (<= js2-first-assign tt)
                 (<= tt js2-last-assign))
        (js2-consume-token)
        (setq op-pos (- js2-token-beg pos)  ; relative
              left pn
              right (js2-parse-assign-expr)
              pn (make-js2-assign-node :type tt
                                       :pos pos
                                       :len (- (js2-node-end right) pos)
                                       :op-pos op-pos
                                       :left left
                                       :right right))
        (js2-highlight-assign-targets pn left right)
        (js2-node-add-children pn left right))
      pn)))

(defun js2-parse-cond-expr ()
  (let ((pos js2-token-beg)
        (pn (js2-parse-or-expr))
        test-expr
        if-true
        if-false
        q-pos
        c-pos)
    (when (js2-match-token js2-HOOK)
      (setq q-pos (- js2-token-beg pos)
            if-true (js2-parse-assign-expr))
      (js2-must-match js2-COLON "msg.no.colon.cond")
      (setq c-pos (- js2-token-beg pos)
            if-false (js2-parse-assign-expr)
            test-expr pn
            pn (make-js2-cond-node :pos pos
                                   :len (- (js2-node-end if-false) pos)
                                   :test-expr test-expr
                                   :true-expr if-true
                                   :false-expr if-false
                                   :q-pos q-pos
                                   :c-pos c-pos))
      (js2-node-add-children pn test-expr if-true if-false))
    pn))

(defun js2-make-binary (type left parser)
  "Helper for constructing a binary-operator AST node.
LEFT is the left-side-expression, already parsed, and the
binary operator should have just been matched.
PARSER is a function to call to parse the right operand,
or a `js2-node' struct if it has already been parsed."
  (let* ((pos (js2-node-pos left))
         (op-pos (- js2-token-beg pos))
         (right (if (js2-node-p parser)
                    parser
                  (funcall parser)))
         (pn (make-js2-infix-node :type type
                                  :pos pos
                                  :len (- (js2-node-end right) pos)
                                  :op-pos op-pos
                                  :left left
                                  :right right)))
    (js2-node-add-children pn left right)
    pn))

(defun js2-parse-or-expr ()
  (let ((pn (js2-parse-and-expr)))
    (when (js2-match-token js2-OR)
      (setq pn (js2-make-binary js2-OR
                                pn
                                'js2-parse-or-expr)))
    pn))

(defun js2-parse-and-expr ()
  (let ((pn (js2-parse-bit-or-expr)))
    (when (js2-match-token js2-AND)
      (setq pn (js2-make-binary js2-AND
                                pn
                                'js2-parse-and-expr)))
    pn))

(defun js2-parse-bit-or-expr ()
  (let ((pn (js2-parse-bit-xor-expr)))
    (while (js2-match-token js2-BITOR)
      (setq pn (js2-make-binary js2-BITOR
                                pn
                                'js2-parse-bit-xor-expr)))
    pn))

(defun js2-parse-bit-xor-expr ()
  (let ((pn (js2-parse-bit-and-expr)))
    (while (js2-match-token js2-BITXOR)
      (setq pn (js2-make-binary js2-BITXOR
                                pn
                                'js2-parse-bit-and-expr)))
    pn))

(defun js2-parse-bit-and-expr ()
  (let ((pn (js2-parse-eq-expr)))
    (while (js2-match-token js2-BITAND)
      (setq pn (js2-make-binary js2-BITAND
                                pn
                                'js2-parse-eq-expr)))
    pn))

(defconst js2-parse-eq-ops
  (list js2-EQ js2-NE js2-SHEQ js2-SHNE))

(defun js2-parse-eq-expr ()
  (let ((pn (js2-parse-rel-expr))
        tt)
    (while (memq (setq tt (js2-peek-token)) js2-parse-eq-ops)
      (js2-consume-token)
      (setq pn (js2-make-binary tt
                                pn
                                'js2-parse-rel-expr)))
    pn))

(defconst js2-parse-rel-ops
  (list js2-IN js2-INSTANCEOF js2-LE js2-LT js2-GE js2-GT))

(defun js2-parse-rel-expr ()
  (let ((pn (js2-parse-shift-expr))
        (continue t)
        tt)
    (while continue
      (setq tt (js2-peek-token))
      (cond
       ((and js2-in-for-init (= tt js2-IN))
        (setq continue nil))
       ((memq tt js2-parse-rel-ops)
        (js2-consume-token)
        (setq pn (js2-make-binary tt pn 'js2-parse-shift-expr)))
       (t
        (setq continue nil))))
    pn))

(defconst js2-parse-shift-ops
  (list js2-LSH js2-URSH js2-RSH))

(defun js2-parse-shift-expr ()
  (let ((pn (js2-parse-add-expr))
        tt
        (continue t))
    (while continue
      (setq tt (js2-peek-token))
      (if (memq tt js2-parse-shift-ops)
          (progn
            (js2-consume-token)
            (setq pn (js2-make-binary tt pn 'js2-parse-add-expr)))
        (setq continue nil)))
    pn))

(defun js2-parse-add-expr ()
  (let ((pn (js2-parse-mul-expr))
        tt
        (continue t))
    (while continue
      (setq tt (js2-peek-token))
      (if (or (= tt js2-ADD) (= tt js2-SUB))
          (progn
            (js2-consume-token)
            (setq pn (js2-make-binary tt pn 'js2-parse-mul-expr)))
        (setq continue nil)))
    pn))

(defconst js2-parse-mul-ops
  (list js2-MUL js2-DIV js2-MOD))

(defun js2-parse-mul-expr ()
  (let ((pn (js2-parse-unary-expr))
        tt
        (continue t))
    (while continue
      (setq tt (js2-peek-token))
      (if (memq tt js2-parse-mul-ops)
          (progn
            (js2-consume-token)
            (setq pn (js2-make-binary tt pn 'js2-parse-unary-expr)))
        (setq continue nil)))
    pn))

(defsubst js2-make-unary (type parser &rest args)
  "Make a unary node of type TYPE.
PARSER is either a node (for postfix operators) or a function to call
to parse the operand (for prefix operators)."
  (let* ((pos js2-token-beg)
         (postfix (js2-node-p parser))
         (expr (if postfix
                   parser
                 (apply parser args)))
         end
         pn)
    (if postfix  ; e.g. i++
        (setq pos (js2-node-pos expr)
              end js2-token-end)
      (setq end (js2-node-end expr)))
    (setq pn (make-js2-unary-node :type type
                                  :pos pos
                                  :len (- end pos)
                                  :operand expr))
    (js2-node-add-children pn expr)
    pn))

(defconst js2-incrementable-node-types
  (list js2-NAME js2-GETPROP js2-GETELEM js2-GET_REF js2-CALL)
  "Node types that can be the operand of a ++ or -- operator.")

(defsubst js2-check-bad-inc-dec (tt beg end unary)
  (unless (memq (js2-node-type (js2-unary-node-operand unary))
                js2-incrementable-node-types)
    (js2-report-error (if (eq tt js2-INC)
                          "msg.bad.incr"
                        "msg.bad.decr")
                      nil beg (- end beg))))

(defun js2-parse-unary-expr ()
  (let ((tt (js2-peek-token))
        pn expr beg end)
    (cond
     ((or (= tt js2-VOID)
          (= tt js2-NOT)
          (= tt js2-BITNOT)
          (= tt js2-TYPEOF))
      (js2-consume-token)
      (js2-make-unary tt 'js2-parse-unary-expr))

     ((= tt js2-ADD)
      (js2-consume-token)
      ;; Convert to special POS token in decompiler and parse tree
      (js2-make-unary js2-POS 'js2-parse-unary-expr))

     ((= tt js2-SUB)
      (js2-consume-token)
      ;; Convert to special NEG token in decompiler and parse tree
      (js2-make-unary js2-NEG 'js2-parse-unary-expr))

     ((or (= tt js2-INC)
          (= tt js2-DEC))
      (js2-consume-token)
      (prog1
          (setq beg js2-token-beg
                end js2-token-end
                expr (js2-make-unary tt 'js2-parse-member-expr t))
        (js2-check-bad-inc-dec tt beg end expr)))

     ((= tt js2-DELPROP)
      (js2-consume-token)
      (js2-make-unary js2-DELPROP 'js2-parse-unary-expr))

     ((= tt js2-ERROR)
      (js2-consume-token)
      (make-js2-error-node))  ; try to continue

     ((and (= tt js2-LT)
           js2-compiler-xml-available)
      ;; XML stream encountered in expression.
      (js2-consume-token)
      (js2-parse-member-expr-tail t (js2-parse-xml-initializer)))
     (t
      (setq pn (js2-parse-member-expr t)
            ;; Don't look across a newline boundary for a postfix incop.
            tt (js2-peek-token-or-eol))
      (when (or (= tt js2-INC) (= tt js2-DEC))
        (js2-consume-token)
        (setf expr pn
              pn (js2-make-unary tt expr))
        (js2-node-set-prop pn 'postfix t)
        (js2-check-bad-inc-dec tt js2-token-beg js2-token-end pn))
      pn))))

(defun js2-parse-xml-initializer ()
  "Parse an E4X XML initializer.
I'm parsing it the way Rhino parses it, but without the tree-rewriting.
Then I'll postprocess the result, depending on whether we're in IDE
mode or codegen mode, and generate the appropriate rewritten AST.
IDE mode uses a rich AST that models the XML structure.  Codegen mode
just concatenates everything and makes a new XML or XMLList out of it."
  (let ((tt (js2-get-first-xml-token))
        (pos js2-token-beg)
        pn-xml
        pn
        n
        expr
        kids
        expr-pos
        prepend)
    (when (not (or (= tt js2-XML) (= tt js2-XMLEND)))
      (js2-report-error "msg.syntax"))
    (setq pn-xml (make-js2-xml-node))
    (catch 'return
      (while t
        (cond
         ;; js2-XML means we found a {expr} in the XML stream.
         ;; The js2-ts-string is the XML up to the left-curly.
         ((= tt js2-XML)
          (push (make-js2-string-node :pos js2-token-beg
                                      :len (- js2-ts-cursor js2-token-beg))
                kids)
          (js2-must-match js2-LC "msg.syntax")
          (setq expr-pos js2-ts-cursor
                expr (if (= (js2-peek-token) js2-RC)
                         (make-js2-string-node :pos expr-pos
                                               :len 0
                                               :value "")
                       (js2-parse-expr)))
          (js2-must-match js2-RC "msg.syntax")
          (setq pn (make-js2-xml-js-expr-node :pos expr-pos
                                              :len (js2-node-len expr)
                                              :expr expr))
          (js2-node-add-children pn expr)
          (push pn kids))

         ;; a js2-XMLEND token means we hit the final close-tag.
         ((= tt js2-XMLEND)
          (push (make-js2-string-node :pos js2-token-beg
                                      :len (- js2-ts-cursor js2-token-beg))
                kids)
          (dolist (kid (nreverse kids))
            (js2-block-node-push pn-xml kid))
          (setf (js2-node-len pn-xml) (- js2-ts-cursor
                                         (js2-node-pos pn-xml)))
          (throw 'return pn-xml))

         (t
          (js2-report-error "msg.syntax")
          (throw 'return pn-xml)))
        (setq tt (js2-get-next-xml-token))))))

(defun js2-parse-argument-list ()
  "Parse an argument list and return it as a lisp list of nodes.
Returns the list in reverse order.  Consumes the right-paren token."
  (let (result)
    (unless (js2-match-token js2-RP)
      (loop do
            (if (= (js2-peek-token) js2-YIELD)
                (js2-report-error "msg.yield.parenthesized"))
            (push (js2-parse-assign-expr) result)
            while
            (js2-match-token js2-COMMA))
      (js2-must-match js2-RP "msg.no.paren.arg")
      result)))

(defun js2-parse-member-expr (&optional allow-call-syntax)
  (let ((tt (js2-peek-token))
        pn
        pos
        target
        args
        beg
        end
        init
        result)
    (if (/= tt js2-NEW)
        (setq pn (js2-parse-primary-expr))
      (js2-consume-token)
      (setq pos js2-token-beg
            target (js2-parse-member-expr)
            end (js2-node-end target)
            pn (make-js2-new-node :pos pos
                                  :target target
                                  :len (- end pos)))
      (js2-node-add-children pn (js2-new-node-target pn))
      (when (js2-match-token js2-LP)
        ;; Add the arguments to pn, if any are supplied.
        (setf beg pos  ; start of "new" keyword
              pos js2-token-beg
              args (js2-parse-argument-list)
              end js2-token-end
              (js2-new-node-lp pn) (- pos beg)
              (js2-new-node-rp pn) (- end 1 beg))

        (dolist (arg args)
          (push arg (js2-new-node-args pn))
          (js2-node-add-children pn arg)))

      (when (and js2-allow-rhino-new-expr-initializer
                 (= (js2-peek-token) js2-LC))
        (js2-consume-token)
        (setf init (js2-parse-object-literal)
              end (js2-node-end init)
              (js2-new-node-initializer pn) init)
        (js2-node-add-children pn init))

        (incf (js2-node-len pn) (- end pos)))
    (setq result (js2-parse-member-expr-tail allow-call-syntax pn))
    (if (>= js2-highlight-level 2)
        (js2-parse-highlight-member-expr-node result))
    result))

(defun js2-parse-member-expr-tail (allow-call-syntax pn)
  "Parse a chain of property/array accesses or function calls.
Includes parsing for E4X operators like `..' and `.@'.
If ALLOW-CALL-SYNTAX is nil, stops when we encounter a left-paren.
Returns an expression tree that includes PN, the parent node."
  (let (tt
        args
        pos
        expr
        lb
        rb
        (beg (js2-node-pos pn)))
    (catch 'tail-loop
      (while t
        (setq tt (js2-peek-token))
        (catch 'break-outer
          (cond
           ((or (= tt js2-DOT) (= tt js2-DOTDOT))
            (setq pn (js2-parse-property-access tt pn)))

           ((= tt js2-DOTQUERY)
            (js2-consume-token)
            (js2-must-have-xml)
            (setq pos js2-token-beg
                  expr (js2-parse-expr)
                  pn (make-js2-xml-dot-query-node :left pn
                                                  :pos beg
                                                  :op-pos pos
                                                  :right expr))
            (js2-node-add-children pn
                                   (js2-xml-dot-query-node-left pn)
                                   (js2-xml-dot-query-node-right pn))
            (js2-must-match js2-RP "msg.no.paren")
            (setf (js2-node-len pn) (- js2-token-end beg)))

           ((= tt js2-LB)
            (js2-consume-token)
            (setq lb js2-token-beg
                  pos (js2-node-pos pn)
                  expr (js2-parse-expr))
            (if (js2-must-match js2-RB "msg.no.bracket.index")
                (setq rb js2-token-beg))
            (setq pn (make-js2-elem-get-node :target pn
                                             :pos pos
                                             :prop expr
                                             :lb (js2-relpos lb pos)
                                             :rb (js2-relpos rb pos)
                                             :len (- js2-token-end pos)))
            (js2-node-add-children pn
                                   (js2-elem-get-node-target pn)
                                   (js2-elem-get-node-prop pn)))

           ((= tt js2-LP)
            (unless allow-call-syntax
              (throw 'tail-loop nil))
            (js2-consume-token)
            (setq pn (make-js2-call-node :pos (setq pos (js2-node-pos pn))
                                         :expr pn
                                         :lp (- js2-token-beg pos)))
            (js2-node-add-children pn (js2-call-node-expr pn))
            ;; Add the arguments to pn, if any are supplied.
            (setf args (js2-parse-argument-list)
                  (js2-call-node-rp pn) (- js2-token-beg pos))
            (dolist (arg args)
              (push arg (js2-call-node-args pn))
              (js2-node-add-children pn arg))
            (setf (js2-node-len pn) (- js2-ts-cursor (js2-node-pos pn))))
           (t
            (throw 'tail-loop nil))))))
    pn))

(defun js2-parse-property-access (tt pn)
  "Parse a property access, XML descendants access, or XML attr access."
  (let ((member-type-flags 0)
        (dot-pos js2-token-beg)
        (dot-len (if (= tt js2-DOTDOT) 2 1)))
    (js2-consume-token)
    (when (= tt js2-DOTDOT)
      (js2-must-have-xml)
      (setq member-type-flags js2-descendants-flag))
    (if (not js2-compiler-xml-available)
        (prog2
            (js2-must-match js2-NAME "msg.no.name.after.dot")
            (make-js2-prop-get-node :target pn
                                    :pos js2-token-beg
                                    :prop (make-js2-name-node)
                                    :len (- js2-token-end js2-token-beg))
          (js2-node-add-children pn
                                 (js2-prop-get-node-target pn)
                                 (js2-prop-get-node-prop pn)))
      (setq tt (js2-next-token))
      (cond
       ;; needed for generator.throw();
       ((= tt js2-THROW)
        (js2-parse-property-name pn
                                 (make-js2-name-node) ; "throw"
                                 member-type-flags))

       ;; handles: name, ns::name, ns::*, ns::[expr]
       ((= tt js2-NAME)
        (js2-parse-property-name pn
                                 (make-js2-name-node)  ; name|ns
                                 member-type-flags))

       ;; handles: *, *::name, *::*, *::[expr]
       ((= tt js2-MUL)
        (js2-parse-property-name pn
                                 (make-js2-name-node :name "*")
                                 member-type-flags))
       ;; handles: '@attr', '@ns::attr', '@ns::*', '@ns::*',
       ;;          '@::attr', '@::*', '@*', '@*::attr', '@*::*'
       ((= tt js2-XMLATTR)
        (js2-parse-attribute-access pn member-type-flags))
       (t
        (js2-report-error "msg.no.name.after.dot" nil dot-pos dot-len)
        (make-js2-error-node :pos dot-pos
                             :len dot-len))))))

(defun js2-parse-attribute-access (pn member-type-flags)
  "Parse an E4X XML attribute expression.
PN (parent node) is the XML target to the left of the @ operator."
  (let ((tt (js2-next-token))
        expr pos beg len)
    (setq member-type-flags (set-flag member-type-flags js2-attribute-flag))
    (cond
     ((= tt js2-NAME)
      ;; handles: @name, @ns::name, @ns::*, @ns::[expr]
      (setq pn (js2-parse-property-name pn
                                        (make-js2-name-node)
                                        member-type-flags)))
     ;; handles: @*, @*::name, @*::*, @*::[expr]
     ((= tt js2-MUL)
      (setq pn (js2-parse-property-name pn
                                        (make-js2-name-node :name "*")
                                        member-type-flags)))
     ;; handles @[expr]
     ((= tt js2-LB)
      (setq pos (1- js2-ts-cursor)
            pn (make-js2-xml-ref-node :pos pos
                                      :ref-expr (js2-parse-expr)
                                      :len (- js2-ts-cursor pos)
                                      :flags member-type-flags))
      (js2-node-add-children pn (js2-xml-ref-node-ref-expr pn))
      (js2-must-match js2-RB "msg.no.bracket.index"))
     (t
      (if pn
          (setq len (js2-node-len pn)
                beg (- js2-ts-cursor len))
        (save-excursion
          (goto-char js2-ts-cursor)
          (setq beg (point-at-bol)
                len (- (point-at-eol) beg))))
      (js2-report-error "msg.no.name.after.xmlAttr" nil beg len)
      (setq pos (1- js2-ts-cursor)
            pn (make-js2-xml-ref-node :pos pos
                                      :target pn
                                      :len (if pn (js2-node-len pn))
                                      :flags member-type-flags))
      (js2-node-add-children pn (js2-xml-ref-node-target pn))))
    pn))

(defsubst js2-create-property-get (target namespace name flags)
  "Create either a simple property-get or an XML element-get.
We potentially need to examine quite a few tokens before we can be
sure which one to create, so we pass the state along to this helper
function.  NAMESPACE is nil or a string node."
  (let (pn pos)
    (if (and (null namespace) (zerop flags))
        (if (null target)
            name
          (prog1
              (setq pos (js2-node-pos target)
                    pn
                    (make-js2-prop-get-node :pos pos
                                            :len (- (js2-node-end name) pos)
                                            :target target
                                            :prop name))
            (js2-node-add-children pn target name)))
      (setq flags (set-flag flags js2-property-flag))
      (js2-create-member-ref-get target namespace name nil flags))))

(defsubst js2-create-element-get (target namespace elem flags)
  "Create either a simple element-get or an XML element-get.
ELEM is an expression node.  NAMESPACE is nil or a string node."
  (let (pn)
    (if (and (null namespace) (zerop flags))
        (prog1
            (setq pn (make-js2-elem-get-node :pos (js2-node-pos elem)
                                             :target target
                                             :prop elem))
          (js2-node-add-children pn target elem))
      (js2-create-member-ref-get target namespace nil elem flags))))

(defun js2-create-member-ref-get (target namespace name expr flags)
  (let ((node (make-js2-xml-ref-node :pos (js2-node-pos (or namespace name))
                                     :target target
                                     :namespace namespace
                                     :propname name
                                     :ref-expr expr
                                     :flags flags)))
    (js2-node-add-children node namespace name expr)
    node))

(defun js2-parse-property-name (pn name flags)
  "Check if :: follows name in which case it becomes qualified name.
PN is the target - the left side of the preceding '.' or '..' operator.
NAME is the identifier we just parsed, a `js2-name-node'.  In some
e4x/xml cases, it may not be a valid identifier (e.g. `*')."
  (let (namespace
        tt
        lb
        (pos (if pn (js2-node-pos pn))))
    (catch 'return
      (when (js2-match-token js2-COLONCOLON)
        (setq namespace (make-js2-string-node :value name)
              tt (js2-next-token))
        (cond
         ((= tt js2-NAME)               ; handles name::name
          (setq name (make-js2-string-node)))
         ((= tt js2-MUL)                ; handles name::*
          (setq name (make-js2-string-node :value "*")))
         ((= tt js2-LB)                 ; handles name::[expr]
          (setq lb (- js2-token-beg pos)
                pn (js2-create-element-get pn
                                           namespace
                                           (js2-parse-expr)
                                           flags))
          (setf (js2-elem-get-node-lb pn) lb)
          (if (js2-must-match js2-RB "msg.no.bracket.index")
              (setf (js2-elem-get-node-rb pn) (- js2-token-beg pos)))
          (throw 'return pn))
         (t
          (js2-report-error "msg.no.name.after.coloncolon"))))
      (js2-create-property-get pn namespace name flags))))

(defun js2-parse-primary-expr ()
  (let* (pn  ; parent node
         (tt-flagged (js2-next-flagged-token))
         (tt (logand tt-flagged js2-clear-ti-mask))
         px-pos
         name
         name-pos
         name-end
         flags
         index
         expr)
    (cond
     ((= tt js2-FUNCTION)
      (js2-parse-function 'FUNCTION_EXPRESSION))

     ((= tt js2-LB)
      (js2-parse-array-literal))

     ((= tt js2-LC)
      (js2-parse-object-literal))

     ((= tt js2-LET)
      (js2-parse-let-stmt))

     ((= tt js2-LP)
      (setq px-pos js2-token-beg
            expr (js2-parse-expr))
      (js2-must-match js2-RP "msg.no.paren")
      (setq pn (make-js2-paren-node :pos px-pos
                                    :expr expr
                                    :len (- js2-token-end px-pos)))
      (js2-node-add-children pn (js2-paren-node-expr pn))
      pn)

     ((= tt js2-XMLATTR)
      (js2-must-have-xml)
      (js2-parse-attribute-access nil 0))

     ((= tt js2-NAME)
      (setq name js2-ts-string
            name-pos js2-token-beg
            name-end js2-token-end)
      (if (and (flag-set-p tt-flagged js2-ti-check-label)
               (= (js2-peek-token) js2-COLON))
          (prog1
            ;; Do not consume colon, it is used as unwind indicator
            ;; to return to statementHelper.
            (make-js2-label-node :pos name-pos
                                 :len (- js2-token-end name-pos)
                                 :label name)
            (js2-set-face name-pos
                          js2-token-end
                          'js2-variable-name-face 'record))
        ;; otherwise not a label, just a name
        (setq name (make-js2-name-node :name name
                                       :pos name-pos
                                       :len (- name-end name-pos)))
        (if js2-compiler-xml-available
            (js2-parse-property-name nil name 0)
          name)))

     ((= tt js2-NUMBER)
      (make-js2-number-node))

     ((= tt js2-STRING)
      (prog1
          (make-js2-string-node)
        (js2-set-face js2-token-beg js2-token-end 'js2-string-face 'record)))

     ((or (= tt js2-DIV) (= tt js2-ASSIGN_DIV))
      ;; Got / or /= which should be treated as regexp in fact
      (setq px-pos js2-token-beg)
      (js2-read-regexp tt)
      (setq flags js2-ts-regexp-flags
            js2-ts-regexp-flags nil)
      (prog1
          (make-js2-regexp-node :pos px-pos
                                :len (- js2-ts-cursor px-pos)
                                :value js2-ts-string
                                :flags flags)
        (js2-set-face px-pos js2-ts-cursor 'js2-string-face 'record)))
     ((or (= tt js2-NULL)
          (= tt js2-THIS)
          (= tt js2-FALSE)
          (= tt js2-TRUE))
      (make-js2-literal-node :type tt))

     ((= tt js2-RESERVED)
      (js2-report-error "msg.reserved.id")
      (make-js2-name-node))

     ((= tt js2-ERROR)
      ;; the scanner or one of its subroutines reported the error.
      (make-js2-error-node))

     ((= tt js2-EOF)
      (js2-report-error "msg.unexpected.eof")
      (make-js2-error-node))

     (t
      (js2-report-error "msg.syntax")
      (make-js2-error-node)))))

(defsubst js2-parse-warn-trailing-comma (msg pos elems comma-pos)
  (js2-add-strict-warning
   msg nil
   ;; back up from comma to beginning of line or array/objlit
   (max (if elems
            (js2-node-pos (car elems))
          pos)
        (save-excursion
          (goto-char comma-pos)
          (back-to-indentation)
          (point)))
   comma-pos))

;; Pulled out of giant switch case in Parser.primaryExpr()
(defun js2-parse-array-literal ()
  (let ((pos js2-token-beg)
        (after-lb-or-comma t)
        after-comma
        tt
        elems
        result
        (continue t))
    (while continue
      (setq tt (js2-peek-token))
      (cond
       ((= tt js2-COMMA)
        (js2-consume-token)
        (setq after-comma js2-ts-cursor)
        (if (not after-lb-or-comma)
            (setq after-lb-or-comma t)
          (push nil elems)))

       ((or (= tt js2-RB)
            (= tt js2-EOF))
        (if (= tt js2-EOF)
            (js2-report-error "msg.no.bracket.arg" nil pos)
          (js2-consume-token))
        (setq continue nil
              result (make-js2-array-node :pos pos
                                          :len (- js2-ts-cursor pos)))
        (when after-comma
          (js2-parse-warn-trailing-comma "msg.array.trailing.comma"
                                         pos elems after-comma))
        (dolist (elem elems)
          (js2-node-add-children result elem)
          (push elem (js2-array-node-elems result))))

       ((and (>= js2-language-version 170)
             (= tt js2-FOR)          ; check for array comprehension
             (not after-lb-or-comma) ; "for" can't follow a comma
             elems                   ; must have at least 1 element
             (not (cdr elems)))      ; but no 2nd element
        (setf continue nil
              result (js2-parse-array-comprehension (car elems) pos)))
       (t
        (unless after-lb-or-comma
          (js2-report-error "msg.no.bracket.arg"))
        (push (js2-parse-assign-expr) elems)
        (setq after-lb-or-comma nil
              after-comma nil))))
    result))

(defun js2-parse-array-comprehension (expr pos)
  "Parse a JavaScript 1.7 Array Comprehension.
EXPR is the first expression after the opening left-bracket.
POS is the beginning of the LB token preceding EXPR.
We should have just parsed the 'for' keyword before calling this function."
  (let (loops
        filter
        if-pos
        result
        (continue t))
    (while continue
      (if (= (js2-peek-token) js2-FOR)
          (push (js2-parse-array-comp-loop) loops)
        (setq continue nil)))
    (when (= (js2-peek-token) js2-IF)
      (js2-consume-token)
      (setq if-pos (- js2-token-beg pos)  ; relative
            filter (js2-parse-condition)))
    (js2-must-match js2-RB "msg.no.bracket.arg" pos)
    (setq result (make-js2-array-comp-node :pos pos
                                           :len (- js2-ts-cursor pos)
                                           :result expr
                                           :filter (car filter)
                                           :lp (js2-relpos (second filter) pos)
                                           :rp (js2-relpos (third filter) pos)
                                           :if-pos if-pos))
    (js2-node-add-children result expr (car filter))
    ;; reverse and append loops
    (dolist (lp loops)
      (push lp (js2-array-comp-node-loops result))
      (js2-node-add-children result lp))
    result))

(defun js2-parse-array-comp-loop ()
  "Parse a 'for [each] (foo in bar)' expression in an Array comprehension.
Last token peeked should be the initial FOR."
  (let ((pos js2-token-beg)
        (pn (make-js2-array-comp-loop-node))
        tt
        iter
        obj
        foreach-p
        in-pos
        each-pos
        lp
        rp)
  (unless (= (js2-next-token) js2-FOR)  ; consumes token
    (js2-code-bug))   ; shouldn't be here if next token isn't 'for'

  (js2-enter-loop pn)
  (unwind-protect
      (progn
        (when (js2-match-token js2-NAME)
          (if (string= js2-ts-string "each")
              (progn
                (setq foreach-p t
                      each-pos (- js2-token-beg pos)) ; relative
                (js2-set-face js2-token-beg js2-token-end
                              'js2-keyword-face 'record))
            (js2-report-error "msg.no.paren.for")))
        (if (js2-must-match js2-LP "msg.no.paren.for")
            (setq lp (- js2-token-beg pos)))
        (setq tt (js2-peek-token))
        (cond
         ((or (= tt js2-LB) (= tt js2-LC))
          ;; handle destructuring assignment
          (setq iter (js2-parse-primary-expr)))
         ((= tt js2-NAME)
          (js2-consume-token)
          (setq iter (make-js2-name-node)))
         (t
          (js2-report-error "msg.bad.var")))

        ;; Define as a let since we want the scope of the variable to
        ;; be restricted to the array comprehension
        (if iter
            (js2-define-symbol js2-LET (js2-name-node-name iter) pn))

        (if (js2-must-match js2-IN "msg.in.after.for.name")
            (setq in-pos (- js2-token-beg pos)))
        (setq obj (js2-parse-expr))
        (if (js2-must-match js2-RP "msg.no.paren.for.ctrl")
            (setq rp (- js2-token-beg pos)))
        (setf (js2-node-pos pn) pos
              (js2-node-len pn) (- js2-ts-cursor pos)
              (js2-array-comp-loop-node-iterator pn) iter
              (js2-array-comp-loop-node-object pn) obj
              (js2-array-comp-loop-node-in-pos pn) in-pos
              (js2-array-comp-loop-node-each-pos pn) each-pos
              (js2-array-comp-loop-node-foreach-p pn) foreach-p
              (js2-array-comp-loop-node-lp pn) lp
              (js2-array-comp-loop-node-rp pn) rp)
        (js2-node-add-children pn iter obj))
    (js2-exit-loop))
  pn))

(defun js2-parse-object-literal ()
  (let ((pos js2-token-beg)
        tt
        elems
        result
        prop
        node
        ppos
        pend
        expr
        get-or-set
        after-comma
        (continue t))
    (while continue
      (setq tt (js2-peek-token))
      (cond
       ((or (= tt js2-NAME)
            (= tt js2-STRING))
        (js2-consume-token)
        (setq after-comma nil
              ppos js2-token-beg
              pend js2-token-end
              prop js2-ts-string
              node (make-js2-name-node))
        (if (not (and (= tt js2-NAME)
                      (= (js2-peek-token) js2-NAME)
                      (or (string= prop "get")
                          (string= prop "set"))))
            (progn
              (setq expr (js2-parse-plain-property node))
              (js2-set-face ppos pend
                            (if (js2-function-node-p
                                 (js2-object-prop-node-right expr))
                                'js2-function-name-face
                              'js2-variable-name-face)
                            'record)
              (push expr elems))
          (js2-consume-token)
          (js2-set-face ppos pend 'js2-keyword-face 'record)
          (js2-set-face js2-token-beg js2-token-end
                        'js2-function-name-face 'record)
          (setq get-or-set (make-js2-string-node :pos ppos
                                                 :len (- pend ppos)
                                                 :value prop)
                result (js2-parse-getter-setter-prop (make-js2-name-node)
                                                     get-or-set))
          (when (and (null result)
                     (not js2-recover-from-parse-errors))
            (setq continue nil))
          (push result elems)))

       ((= tt js2-NUMBER)
        (js2-consume-token)
        (setq after-comma nil)
        (push (js2-parse-plain-property (make-js2-number-node)) elems))

       ;; trailing comma
       ((= tt js2-RC)
        (setq continue nil)
        (if after-comma
            (js2-parse-warn-trailing-comma "msg.extra.trailing.comma"
                                           pos elems after-comma)))
       (t
        (js2-report-error "msg.bad.prop")
        (unless js2-recover-from-parse-errors
          (setq continue nil))))  ; end switch

      (if (js2-match-token js2-COMMA)
          (setq after-comma js2-token-end)
        (setq continue nil))) ; end loop

    (js2-must-match js2-RC "msg.no.brace.prop")
    (setq result (make-js2-object-node :pos pos
                                       :len (- js2-ts-cursor pos)))
    (dolist (e elems)
      (js2-node-add-children result e)
      (push e (js2-object-node-elems result)))
    result))

(defun js2-parse-plain-property (prop)
  "Parse a non-getter/setter property in an object literal.
PROP is the node representing the property:  a number, name or string."
  (let ((pos (js2-node-pos prop))
        colon
        result
        expr)
    (js2-must-match js2-COLON "msg.no.colon.prop")
    (setq colon (- js2-token-beg pos)
          expr (js2-parse-assign-expr)
          result (make-js2-object-prop-node
                  :pos pos
                  ;; don't include last consumed token in length
                  :len (- (+ (js2-node-pos expr)
                             (js2-node-len expr))
                          pos)
                  :left prop
                  :right expr
                  :op-pos colon))
    (js2-node-add-children result prop expr)
    result))

(defun js2-parse-getter-setter-prop (prop get-or-set)
  "Parse getter or setter property in object literal.
PROP is the `js2-name-node' representing the property name.
GET-OR-SET is a `js2-string-node' representing the get/set keyword."
  (let ((f (js2-parse-function 'FUNCTION_EXPRESSION))
        result
        (pos (js2-node-pos get-or-set))
        (type (if (string= "get" (js2-string-node-value get-or-set))
                  js2-GET
                js2-SET)))
    (if (/= (js2-node-type f) js2-FUNCTION)
        (js2-report-error "msg.bad.prop")
      (if (plusp (length (js2-function-name f)))
          (js2-report-error "msg.bad.prop")))
    (js2-node-set-prop f 'GETTER_SETTER type)
    (setq result (make-js2-getter-setter-node :type type
                                              :pos pos
                                              :len (- js2-ts-cursor pos)
                                              :left prop
                                              :right f
                                              :kwd get-or-set))
    (js2-node-add-children result prop f get-or-set)
    result))

;; TODO:
;;  - support Foo.Bar.Baz = { ... }
;;  - support nested functions
(defun js2-parse-record-imenu (node &optional var)
  (when js2-parse-ide-mode
    (let (name left fname fun funs beg)
      (cond
       ;; top-level function?
       ((and (js2-function-node-p node)
             (setq name (js2-function-name node)))
        (push (cons name (js2-node-pos node)) js2-imenu-recorder))
       ;; object literal assigned to a non-destructuring variable?
       ((and (js2-object-node-p node)
             (js2-name-node-p var))
        ;; elements are relative to parent position (which is still absolute)
        (setq beg (js2-node-pos node))
        (dolist (e (js2-object-node-elems node))
          (when (js2-function-node-p (setq fun (js2-infix-node-right e)))
            (setq left (js2-infix-node-left e))
            (cond
             ((js2-name-node-p left)
              (setq fname (js2-name-node-name left)))
             ((js2-string-node-p left)
              (setq fname (js2-string-node-value left))))
            (when fname
              ;; don't create entry unless we find at least one function
              (unless name
                (setq name (js2-name-node-name var)))
              ;; use property name as jump location
              (push (cons fname (+ beg (js2-node-pos e)))
                    funs))))
        (when name
          (push (cons name (nreverse funs))
                js2-imenu-recorder)))))))


(provide 'js2-parse)

;;; js2-parse.el ends here
;;; js2-indent.el --- indentation for js2-mode
;;
;; Copyright (C) 2008 Steve Yegge
;; Author:  Steve Yegge (steve.yegge@gmail.com)
;; Maintainer:  Steve Yegge (steve.yegge@gmail.com)

;; Commentary:
;;
;; This indenter is based on Karl Landström's "javascript.el" indenter.
;; Karl cleverly deduces that the desired indentation level is often a
;; function of paren/bracket/brace nesting depth, which can be determined
;; quickly via the built-in `parse-partial-sexp' function.  His indenter
;; then does some equally clever checks to see if we're in the context of a
;; substatement of a possibly braceless statement keyword such as if, while,
;; or finally.  This approach yields pretty good results.
;;
;; The indenter is often "wrong", however, and needs to be overridden.
;; The right long-term solution is probably to emulate (or modify)
;; cc-engine, but it's thousands upon thousands of lines of code.  Even
;; if you were to assume the accurate parse tree from `js2-parse' is
;; present, indentation is still thousands of lines of code (I've been
;; down that path) to handle every possible syntactic edge case, and in
;; any case, relying on the parse tree is undesirable because parsing is
;; slow.  So you might as well go the cc-engine approach, but it's a
;; huge pile of work that I'm just not up for any time soon.
;;
;; In the meantime, the compromise solution is that we offer a
;; "bounce indenter", configured with `js2-bounce-indent-flag', which
;; cycles the current line indent among various likely guess points.
;; This approach is far from perfect, but should at least make it
;; slightly easier to move the line towards its desired indentation
;; when manually overriding Karl's heuristic nesting guesser.
;;
;; I've made miscellaneous tweaks to Karl's code to handle some Ecma
;; extensions such as `let' and Array comprehensions, and will likely
;; make further tweaks to it, but major kudos to Karl for coming up with
;; the initial approach, which packs a lot of punch for so little code.

;;; Code:

(defconst js-possibly-braceless-keyword-re
  (regexp-opt
   '("catch" "do" "else" "finally" "for" "if" "try" "while" "with" "let")
   'words)
  "Regular expression matching keywords that are optionally
followed by an opening brace.")

(defconst js-indent-operator-re
  (concat "[-+*/%<>=&^|?:]\\([^-+*/]\\|$\\)\\|"
          (regexp-opt '("in" "instanceof") 'words))
  "Regular expression matching operators that affect indentation
of continued expressions.")

(defun js2-insert-and-indent (key)
  "Run command bound to key and indent current line. Runs the command
bound to KEY in the global keymap and indents the current line."
  (interactive (list (this-command-keys)))
  (let ((cmd (lookup-key (current-global-map) key)))
    (if (commandp cmd)
        (call-interactively cmd)))
  ;; don't do the electric keys inside comments or strings,
  ;; and don't do bounce-indent with them.
  (let ((parse-state (parse-partial-sexp (point-min) (point)))
        (js2-bounce-indent-flag nil))
    (unless (or (nth 3 parse-state)
                (nth 4 parse-state))
      (indent-according-to-mode))))

(defun js-re-search-forward-inner (regexp &optional bound count)
  "Auxiliary function for `js-re-search-forward'."
  (let ((parse)
        (saved-point (point-min)))
    (while (> count 0)
      (re-search-forward regexp bound)
      (setq parse (parse-partial-sexp saved-point (point)))
      (cond ((nth 3 parse)
             (re-search-forward
              (concat "\\([^\\]\\|^\\)" (string (nth 3 parse)))
              (save-excursion (end-of-line) (point)) t))
            ((nth 7 parse)
             (forward-line))
            ((or (nth 4 parse)
                 (and (eq (char-before) ?\/) (eq (char-after) ?\*)))
             (re-search-forward "\\*/"))
            (t
             (setq count (1- count))))
      (setq saved-point (point))))
  (point))

(defun js-re-search-forward (regexp &optional bound noerror count)
  "Search forward but ignore strings and comments. Invokes
`re-search-forward' but treats the buffer as if strings and
comments have been removed."
  (let ((saved-point (point))
        (search-expr
         (cond ((null count)
                '(js-re-search-forward-inner regexp bound 1))
               ((< count 0)
                '(js-re-search-backward-inner regexp bound (- count)))
               ((> count 0)
                '(js-re-search-forward-inner regexp bound count)))))
    (condition-case err
        (eval search-expr)
      (search-failed
       (goto-char saved-point)
       (unless noerror
         (error (error-message-string err)))))))

(defun js-re-search-backward-inner (regexp &optional bound count)
  "Auxiliary function for `js-re-search-backward'."
  (let ((parse)
        (saved-point (point-min)))
    (while (> count 0)
      (re-search-backward regexp bound)
      (setq parse (parse-partial-sexp saved-point (point)))
      (cond ((nth 3 parse)
             (re-search-backward
              (concat "\\([^\\]\\|^\\)" (string (nth 3 parse)))
              (save-excursion (beginning-of-line) (point)) t))
            ((nth 7 parse)
             (goto-char (nth 8 parse)))
            ((or (nth 4 parse)
                 (and (eq (char-before) ?/) (eq (char-after) ?*)))
             (re-search-backward "/\\*"))
            (t
             (setq count (1- count))))))
  (point))

(defun js-re-search-backward (regexp &optional bound noerror count)
  "Search backward but ignore strings and comments. Invokes
`re-search-backward' but treats the buffer as if strings and
comments have been removed."
  (let ((saved-point (point))
        (search-expr
         (cond ((null count)
                '(js-re-search-backward-inner regexp bound 1))
               ((< count 0)
                '(js-re-search-forward-inner regexp bound (- count)))
               ((> count 0)
                '(js-re-search-backward-inner regexp bound count)))))
    (condition-case err
        (eval search-expr)
      (search-failed
       (goto-char saved-point)
       (unless noerror
         (error (error-message-string err)))))))

(defun js-looking-at-operator-p ()
  "Return non-nil if text after point is an operator (that is not
a comma)."
  (save-match-data
    (and (looking-at js-indent-operator-re)
         (or (not (looking-at ":"))
             (save-excursion
               (and (js-re-search-backward "[?:{]\\|\\<case\\>" nil t)
                    (looking-at "?")))))))

(defun js-continued-expression-p ()
  "Returns non-nil if the current line continues an expression."
  (save-excursion
    (back-to-indentation)
    (or (js-looking-at-operator-p)
        (and (js-re-search-backward "\n" nil t)
	     (progn
	       (skip-chars-backward " \t")
	       (backward-char)
	       (and (js-looking-at-operator-p)
		    (and (progn (backward-char)
				(not (looking-at "++\\|--\\|/[/*]"))))))))))

(defun js-end-of-do-while-loop-p ()
  "Returns non-nil if word after point is `while' of a do-while
statement, else returns nil. A braceless do-while statement
spanning several lines requires that the start of the loop is
indented to the same column as the current line."
  (interactive)
  (save-excursion
    (save-match-data
      (when (looking-at "\\s-*\\<while\\>")
	(if (save-excursion
	      (skip-chars-backward "[ \t\n]*}")
	      (looking-at "[ \t\n]*}"))
	    (save-excursion
	      (backward-list) (backward-word 1) (looking-at "\\<do\\>"))
	  (js-re-search-backward "\\<do\\>" (point-at-bol) t)
	  (or (looking-at "\\<do\\>")
	      (let ((saved-indent (current-indentation)))
		(while (and (js-re-search-backward "^[ \t]*\\<" nil t)
			    (/= (current-indentation) saved-indent)))
		(and (looking-at "[ \t]*\\<do\\>")
		     (not (js-re-search-forward
			   "\\<while\\>" (point-at-eol) t))
		     (= (current-indentation) saved-indent)))))))))

(defun js-ctrl-statement-indentation ()
  "Returns the proper indentation of the current line if it
starts the body of a control statement without braces, else
returns nil."
  (let (forward-sexp-function)  ; temporarily unbind it
    (save-excursion
      (back-to-indentation)
      (when (save-excursion
              (and (not (js2-same-line (point-min)))
                   (not (looking-at "{"))
                   (js-re-search-backward "[[:graph:]]" nil t)
                   (not (looking-at "[{([]"))
                   (progn
                     (forward-char)
                     (backward-sexp)
                     (when (looking-at "(") (backward-word 1))
                     (and (save-excursion
                            (skip-chars-backward " \t}" (point-at-bol))
                            (bolp))
                          (looking-at js-possibly-braceless-keyword-re)
                          (not (js-end-of-do-while-loop-p))))))
        (save-excursion
          (goto-char (match-beginning 0))
          (+ (current-indentation) js2-basic-offset))))))

(defun js2-indent-in-array-comp (parse-status)
  "Return non-nil if we think we're in an array comprehension.
In particular, return the buffer position of the first `for' kwd."
  (let ((end (point)))
    (when (nth 1 parse-status)
      (save-excursion
        (goto-char (nth 1 parse-status))
        (when (looking-at "\\[")
          (forward-char 1)
          (js2-forward-sws)
          (if (looking-at "[[{]")
              (let (forward-sexp-function) ; use lisp version
                (forward-sexp)             ; skip destructuring form
                (js2-forward-sws)
                (if (and (/= (char-after) ?,) ; regular array
                         (looking-at "for"))
                    (match-beginning 0)))
            ;; to skip arbitrary expressions we need the parser,
            ;; so we'll just guess at it.
            (if (re-search-forward "[^,]* \\(for\\) " end t)
                (match-beginning 1))))))))

(defun js2-array-comp-indentation (parse-status for-kwd)
  (if (js2-same-line for-kwd)
      ;; first continuation line
      (save-excursion
        (goto-char (nth 1 parse-status))
        (forward-char 1)
        (skip-chars-forward " \t")
        (current-column))
    (save-excursion
      (goto-char for-kwd)
      (current-column))))

(defun js-proper-indentation (parse-status)
  "Return the proper indentation for the current line."
  (save-excursion
    (back-to-indentation)
    (let ((ctrl-stmt-indent (js-ctrl-statement-indentation))
          (same-indent-p (looking-at "[]})]\\|\\<case\\>\\|\\<default\\>"))
          (continued-expr-p (js-continued-expression-p))
          (bracket (nth 1 parse-status))
          beg)
      (cond
       ;; indent array comprehension continuation lines specially
       ((and bracket
             (not (js2-same-line bracket))
             (setq beg (js2-indent-in-array-comp parse-status))
             (>= (point) (save-excursion
                           (goto-char beg)
                           (point-at-bol)))) ; at or after first loop?
        (js2-array-comp-indentation parse-status beg))

       (ctrl-stmt-indent)

       (bracket
        (goto-char bracket)
        (cond
         ((looking-at "[({[][ \t]*\\(/[/*]\\|$\\)")
          (let ((p (parse-partial-sexp (point-at-bol) (point))))
            (when (save-excursion (skip-chars-backward " \t)")
                                  (looking-at ")"))
              (backward-list))
            (if (nth 1 p)
                (progn (goto-char (1+ (nth 1 p)))
                       (skip-chars-forward " \t"))
              (back-to-indentation))
            (cond (same-indent-p
                   (current-column))
                  (continued-expr-p
                   (+ (current-column) (* 2 js2-basic-offset)))
                  (t
                   (+ (current-column) js2-basic-offset)))))
         (t
          (unless same-indent-p
            (forward-char)
            (skip-chars-forward " \t"))
          (current-column))))

       (continued-expr-p js2-basic-offset)
       (t 0)))))

(defun js2-lineup-comment (parse-status)
  "Indent a multi-line block comment continuation line."
  (let* ((beg (nth 8 parse-status))
         (first-line (js2-same-line beg))
         (offset (save-excursion
                   (goto-char beg)
                   (if (looking-at "/\\*")
                       (+ 1 (current-column))
                     0))))
    (unless first-line
      (indent-line-to offset))))

(defun js2-backward-sws ()
  "Move backward through whitespace and comments."
  (interactive)
  (while (forward-comment -1)))

(defun js2-forward-sws ()
  "Move forward through whitespace and comments."
  (interactive)
  (while (forward-comment 1)))

(defsubst js2-current-indent (&optional pos)
  "Return column of indentation on current line.
If POS is non-nil, go to that point and return indentation for that line."
  (save-excursion
    (if pos
        (goto-char pos))
    (back-to-indentation)
    (current-column)))

(defsubst js2-arglist-close ()
  "Return non-nil if we're on a line beginning with a close-paren/brace."
  (save-match-data
    (save-excursion
      (goto-char (point-at-bol))
      (js2-forward-sws)
      (looking-at "[])}]"))))

(defsubst js2-indent-looks-like-label-p ()
  (goto-char (point-at-bol))
  (js2-forward-sws)
  (looking-at (concat js2-mode-identifier-re ":")))

(defun js2-indent-in-objlit-p (parse-status)
  "Return non-nil if this looks like an object-literal entry."
  (let ((start (nth 1 parse-status)))
    (and
     start
     (save-excursion
       (and (zerop (forward-line -1))
            (not (< (point) start))     ; crossed a {} boundary
            (js2-indent-looks-like-label-p)))
     (save-excursion
       (js2-indent-looks-like-label-p)))))

;; TODO:  add a `js2-bounce-indent-function' so users can hook this
(defun js2-bounce-indent (normal-col parse-status)
  "Cycle among alternate computed indentation positions.
PARSE-STATUS is the result of `parse-partial-sexp' from the beginning
of the buffer to the current point.  NORMAL-COL is the indentation
column computed by the heuristic guesser based on current paren,
bracket, brace and statement nesting."
  (let ((cur-indent (js2-current-indent))
        (old-buffer-undo-list buffer-undo-list)
        ;; Emacs 21 only has `count-lines', not `line-number-at-pos'
        (current-line (save-excursion
                        (forward-line 0)  ; move to bol
                        (1+ (count-lines (point-min) (point)))))
        positions
        pos
        anchor
        arglist-cont
        same-indent
        prev-line-col
        basic-offset)
    ;; temporarily don't record undo info, if user requested this
    (if js2-mode-indent-inhibit-undo
        (setq buffer-undo-list t))
    (unwind-protect
        (progn
          ;; first likely point:  indent from beginning of previous code line
          (push (setq basic-offset
                      (+ (save-excursion
                           (back-to-indentation)
                           (js2-backward-sws)
                           (back-to-indentation)
                           (setq prev-line-col (current-column)))
                         js2-basic-offset))
                positions)

          ;; second likely point:  indent from assign-expr RHS.  This
          ;; is just a crude guess based on finding " = " on the previous
          ;; line containing actual code.
          (setq pos (save-excursion
                      (save-match-data
                        (forward-line -1)
                        (goto-char (point-at-bol))
                        (when (re-search-forward "\\s-+\\(=\\)\\s-+"
                                                 (point-at-eol) t)
                          (goto-char (match-end 1))
                          (skip-chars-forward " \t\r\n")
                          (current-column)))))
          (when pos
            (incf pos js2-basic-offset)
            (unless (member pos positions)
              (push pos positions)))

          ;; third likely point:  same indent as previous line of code.
          ;; Make it the first likely point if we're not on an
          ;; arglist-close line and previous line ends in a comma, or
          ;; both this line and prev line look like object-literal
          ;; elements.
          (setq pos (save-excursion
                      (goto-char (point-at-bol))
                      (js2-backward-sws)
                      (back-to-indentation)
                      (prog1
                          (current-column)
                        ;; while we're here, look for trailing comma
                        (if (save-excursion
                              (goto-char (point-at-eol))
                              (js2-backward-sws)
                              (eq (char-before) ?,))
                            (setq arglist-cont (1- (point)))))))
          (when pos
            (if (and (or arglist-cont
                         (js2-indent-in-objlit-p parse-status))
                     (not (js2-arglist-close)))
                (setq same-indent pos))
            (unless (member pos positions)
              (push pos positions)))

          ;; fourth likely position:  first preceding code with less indentation
          ;; than the immediately preceding code line.
          (setq pos (save-excursion
                      (js2-backward-sws)
                      (back-to-indentation)
                      (setq anchor (current-column))
                      (while (and (zerop (forward-line -1))
                                  (>= (progn
                                        (back-to-indentation)
                                        (current-column))
                                      anchor)))
                      (setq pos (current-column))))
          (unless (member pos positions)
            (push pos positions))

          ;; put nesting-heuristic position first in list, sort rest
          (setq positions (nreverse (sort positions '<)))
          (setq positions (cons normal-col (delete normal-col positions)))

          ;; comma-list continuation lines:  prev line indent takes precedence
          (if same-indent
              (setq positions
                    (cons same-indent
                          (sort (delete same-indent positions) '<))))

          ;; common special case:  if prev line looks like foobar({ then
          ;; we're passing an object literal to a function call, and people
          ;; pretty much always want to de-dent back to the previous line,
          ;; so move the 'basic-offset' position to the front.
          (if (save-excursion
                (back-to-indentation)
                (js2-backward-sws)
                (and (eq (1- (point)) (nth 1 parse-status))
                     (eq (char-before) ?{)
                     (progn
                       (forward-char -1)
                       (skip-chars-backward " \t")
                       (eq (char-before) ?\())))
              (setq positions
                    (cons basic-offset
                          (delete basic-offset positions))))

          ;; record whether we're already sitting on one of the alternatives
          (setq pos (member cur-indent positions))
          (cond
           ;; case 0:  we're one one of the alternatives and this is the
           ;; first time they've pressed TAB on this line (best-guess).
           ((and js2-mode-indent-ignore-first-tab
                 pos
                 ;; first time pressing TAB on this line?
                 (not (eq js2-mode-last-indented-line current-line)))
            ;; do nothing
            nil)
           ;; case 1:  only one computed position => use it
           ((null (cdr positions))
            (indent-line-to (first positions)))
           ;; case 2:  not on any of the computed spots => use main spot
           ((not pos)
            (indent-line-to (first positions)))
           ;; case 3:  on last position:  cycle to first position
           ((null (cdr pos))
            (indent-line-to (first positions)))
           ;; case 4:  on intermediate position:  cycle to next position
           (t
            (indent-line-to (second pos)))))
      ;; finally
      (if js2-mode-indent-inhibit-undo
          (setq buffer-undo-list old-buffer-undo-list))
      ;; see commentary for `js2-mode-last-indented-line'
      (setq js2-mode-last-indented-line current-line))))

(defun js2-indent-line ()
  "Indent the current line as JavaScript source text."
  (interactive)
  (let (parse-status
        current-indent
        offset
        indent-col
        moved
        ;; don't whine about errors/warnings when we're indenting.
        ;; This has to be set before calling parse-partial-sexp below.
        (inhibit-point-motion-hooks t))
    (setq parse-status (save-excursion
                          (parse-partial-sexp (point-min)
                                              (point-at-bol)))
          offset (- (point) (save-excursion
                               (back-to-indentation)
                               (setq current-indent (current-column))
                               (point))))
    (js2-with-underscore-as-word-syntax
     (if (nth 4 parse-status)
         (js2-lineup-comment parse-status)
       (setq indent-col (js-proper-indentation parse-status))
       ;; see comments below about js2-mode-last-indented-line
       (when
           (cond
            ;; bounce-indenting is disabled during electric-key indent.
            ;; It doesn't work well on first line of buffer.
            ((and js2-bounce-indent-flag
                  (not (js2-same-line (point-min))))
             (js2-bounce-indent indent-col parse-status)
             (setq moved t))
            ;; just indent to the guesser's likely spot
            ((/= current-indent indent-col)
             (indent-line-to indent-col)
             (setq moved t)))
         (when (and moved (plusp offset))
           (forward-char offset)))))))

(defun js2-indent-region (start end)
  "Indent the region, but don't use bounce indenting."
  (let ((js2-bounce-indent-flag nil)
        (indent-region-function nil))
    (indent-region start end nil)))  ; nil for byte-compiler

(provide 'js2-indent)

;;; js2-indent.el ends here

(eval-when-compile
  (require 'cl))

(require 'imenu)
(require 'cc-cmds)  ; for `c-fill-paragraph'


(defun js2-mode ()
  "Major mode for editing JavaScript code."
  (interactive)
  (js2-mode-check-compat)
  (kill-all-local-variables)
  (set-syntax-table js2-mode-syntax-table)
  (use-local-map js2-mode-map)
  (setq major-mode 'js2-mode
        mode-name "JavaScript-IDE"
        comment-start "//"  ; used by comment-region; don't change it
        comment-end "")
  (set (make-local-variable 'max-lisp-eval-depth)
       (max max-lisp-eval-depth 3000))
  (set (make-local-variable 'indent-line-function) #'js2-indent-line)
  (set (make-local-variable 'indent-region-function) #'js2-indent-region)
  (set (make-local-variable 'fill-paragraph-function) #'js2-fill-paragraph)
  (set (make-local-variable 'before-save-hook) #'js2-before-save)
  (set (make-local-variable 'next-error-function) #'js2-next-error)
  (put 'js2-mode 'find-tag-default-function #'js2-mode-find-tag)
  ;; some variables needed by cc-engine for paragraph-fill, etc.
  (setq c-buffer-is-cc-mode t
        c-comment-prefix-regexp js2-comment-prefix-regexp
        c-paragraph-start js2-paragraph-start
        c-paragraph-separate "$"
        comment-start-skip js2-comment-start-skip
        c-syntactic-ws-start js2-syntactic-ws-start
        c-syntactic-ws-end js2-syntactic-ws-end
        c-syntactic-eol js2-syntactic-eol)
  (if (>= emacs-major-version 22)
      (c-setup-paragraph-variables))
  ;; We do our own syntax highlighting based on the parse tree.
  (if (fboundp #'font-lock-mode)
      (font-lock-mode -1))
  ;; Don't let forced fontification ruin our lovely highlighting.
  (dolist (var '(font-lock-fontify-buffer-function
                 font-lock-unfontify-buffer-function
                 font-lock-fontify-region-function
                 font-lock-unfontify-region-function))
    (set (make-local-variable var) (lambda (&rest args) t)))
  ;; Experiment:  make reparse-delay longer for longer files.
  (if (plusp js2-dynamic-idle-timer-adjust)
      (setq js2-idle-timer-delay
            (* js2-idle-timer-delay
               (/ (point-max) js2-dynamic-idle-timer-adjust))))
  (add-hook 'change-major-mode-hook #'js2-mode-exit nil t)
  (add-hook 'after-change-functions #'js2-mode-edit nil t)
  ;; this needs work.  see python-mode's version.
  (setq imenu-create-index-function #'js2-mode-create-imenu-index)
  (imenu-add-to-menubar (concat "IM-" mode-name))
  (when js2-mirror-mode
    (js2-enter-mirror-mode))
  (add-to-invisibility-spec '(js2-outline . t))
  (set (make-local-variable 'line-move-ignore-invisible) t)
  (set (make-local-variable 'forward-sexp-function)
       #'js2-mode-forward-sexp)
  (setq js2-mode-functions-hidden nil
        js2-mode-comments-hidden nil)
  (setq js2-mode-buffer-dirty-p t
        js2-mode-parsing nil)
  (js2-reparse)
  (run-hooks 'js2-mode-hook))

(defun js2-mode-check-compat ()
  "Signal an error if we can't run with this version of Emacs."
  (if (and js2-mode-must-byte-compile
           (not (byte-code-function-p (symbol-function 'js2-mode))))
      (error "You must byte-compile js2-mode before using it."))
  (if (and (boundp 'running-xemacs)
           running-xemacs)
      (error "js2-mode is not compatible with XEmacs"))
  (unless (>= emacs-major-version 21)
    (error "js2-mode requires GNU Emacs version 21 or higher")))

(defun js2-mode-exit ()
  (interactive)
  (when js2-mode-node-overlay
    (delete-overlay js2-mode-node-overlay)
    (setq js2-mode-node-overlay nil))
  (setq js2-mode-ast nil)
  (remove-hook 'change-major-mode-hook #'js2-mode-exit t)
  (remove-from-invisibility-spec '(js2-outline . t))
  (js2-mode-show-all)
  (js2-with-unmodifying-text-property-changes
    (js2-clear-face (point-min) (point-max))))

(defun js2-before-save ()
  "Clean up whitespace before saving file.
You can disable this by customizing `js2-cleanup-whitespace'."
  (when js2-cleanup-whitespace
    (delete-trailing-whitespace)))

(defsubst js2-mode-reset-timer ()
  (if js2-mode-parse-timer
      (cancel-timer js2-mode-parse-timer))
  (setq js2-mode-parsing nil)
  (setq js2-mode-parse-timer
        (run-with-idle-timer js2-idle-timer-delay nil #'js2-reparse)))

(defun js2-mode-edit (beg end len)
  "Schedule a new parse after buffer is edited."
  (setq js2-mode-buffer-dirty-p t)
  (js2-mode-reset-timer))

(defun js2-reparse ()
  "Re-parse current buffer after user finishes some data entry.
If we get any user input while parsing, including cursor motion,
we discard the parse and reschedule it."
  (let (time
        interrupted-p
        (js2-compiler-strict-mode js2-mode-show-strict-warnings))
    (unless js2-mode-parsing
      (setq js2-mode-parsing t)
      (unwind-protect
          (when js2-mode-buffer-dirty-p
            (js2-with-unmodifying-text-property-changes
              (setq js2-mode-buffer-dirty-p nil
                    js2-mode-fontifications nil
                    js2-mode-deferred-properties nil)
              (if js2-mode-verbose-parse-p
                  (message "parsing..."))
              (setq time
                    (js2-time
                     (setq interrupted-p
                           (catch 'interrupted
                             (setq js2-mode-ast (js2-parse))
                             (js2-mode-fontify-regions)
                             (js2-mode-show-warnings)
                             (js2-mode-show-errors)
                             nil))))
              (if interrupted-p
                  (progn
                    ;; unfinished parse => try again
                    (setq js2-mode-buffer-dirty-p t)
                    (js2-mode-reset-timer))
                (if js2-mode-verbose-parse-p
                    (message "Parse time: %s" time)))))
        ;; finally
        (setq js2-mode-parsing nil)
        (unless interrupted-p
          (setq js2-mode-parse-timer nil))))))

(defun js2-mode-show-node ()
  "Debugging aid:  highlight selected AST node on mouse click."
  (interactive)
  (let ((node (js2-node-at-point))
        beg
        end)
    (when js2-mode-show-overlay
      (if (null node)
          (message "No node found at location %s" (point))
        (setq beg (js2-node-abs-pos node)
              end (+ beg (js2-node-len node)))
        (if js2-mode-node-overlay
            (move-overlay js2-mode-node-overlay beg end)
          (setq js2-mode-node-overlay (make-overlay beg end))
          (overlay-put js2-mode-node-overlay 'face 'highlight))
        (js2-with-unmodifying-text-property-changes
          (put-text-property beg end 'point-left #'js2-mode-hide-overlay))
        (message "%s, parent: %s"
                 (js2-node-short-name node)
                 (if (js2-node-parent node)
                     (js2-node-short-name (js2-node-parent node))
                   "nil"))))))

(defun js2-mode-hide-overlay (&optional p1 p2)
  "Remove the debugging overlay when the point moves."
  (when js2-mode-node-overlay
    (let ((beg (overlay-start js2-mode-node-overlay))
          (end (overlay-end js2-mode-node-overlay)))
      ;; Sometimes we're called spuriously.
      (unless (and p2
                   (>= p2 beg)
                   (<= p2 end))
        (js2-with-unmodifying-text-property-changes
          (remove-text-properties beg end '(point-left nil)))
        (delete-overlay js2-mode-node-overlay)
        (setq js2-mode-node-overlay nil)))))

(defun js2-reset ()
  "Debugging helper; resets everything."
  (interactive)
  (js2-mode-exit)
  (js2-mode))

(defsubst js2-mode-show-warn-or-err (e face)
  (let* ((key (first e))
         (beg (second e))
         (end (+ beg (third e)))
         ;; Don't inadvertently go out of bounds.
         (beg (max (point-min) (min beg (point-max))))
         (end (max (point-min) (min end (point-max))))
         (js2-highlight-level 3))    ; so js2-set-face is sure to fire
    (js2-set-face beg end face)
    (put-text-property beg end 'help-echo (js2-get-msg key))
    (put-text-property beg end 'point-entered #'js2-echo-error)))

(defun js2-mode-fontify-regions ()
  "Apply fontifications recorded during parsing."
  ;; We defer clearing faces as long as possible to eliminate flashing.
  (js2-clear-face (point-min) (point-max))
  (dolist (f js2-mode-fontifications)
    (put-text-property (first f) (second f) 'face (third f)))
  (setq js2-mode-fontifications nil)
  (dolist (p js2-mode-deferred-properties)
    (apply #'put-text-property p))
  (setq js2-mode-deferred-properties nil))

(defun js2-mode-show-errors ()
  "Highlight syntax errors."
  (when js2-mode-show-parse-errors
    (dolist (e (js2-ast-root-errors js2-mode-ast))
      (js2-mode-show-warn-or-err e 'js2-error-face))))

(defun js2-mode-show-warnings ()
  "Highlight strict-mode warnings."
  (when js2-mode-show-strict-warnings
    (dolist (e (js2-ast-root-warnings js2-mode-ast))
      (let ((key (car e)))
        (unless
            ;; allow individual overrides
            (or
             (and (not js2-strict-trailing-comma-warning)
                  (string-match "trailing\\.comma" key))
             (and (not js2-strict-cond-assign-warning)
                  (string= key "msg.equal.as.assign")))
          (js2-mode-show-warn-or-err e 'js2-warning-face))))))

(defun js2-echo-error (old-point new-point)
  (let ((msg (get-text-property new-point 'help-echo)))
    (if msg
        (message msg))))

(defalias #'js2-echo-help #'js2-echo-error)

(defun js2-enter-key ()
  (interactive)
  (let ((parse-status (save-excursion
                        (parse-partial-sexp (point-min) (point)))))
    (cond
     ((nth 3 parse-status)
      (js2-mode-split-string parse-status))
     ((nth 4 parse-status)
      (js2-mode-extend-comment))
     (t
      ;; should probably figure out what the mode-map says we should do
      (if js2-indent-on-enter-key
          (let ((js2-bounce-indent-flag nil))
            (js2-indent-line)))
      (insert "\n")))))

(defun js2-mode-split-string (parse-status)
  "Turn a newline in mid-string into a string concatenation."
  (let* ((col (current-column))
         (quote-char (nth 3 parse-status))
         (quote-string (string quote-char))
         (string-beg (nth 8 parse-status))
         (indent (save-match-data
                   (or
                    (save-excursion
                      (back-to-indentation)
                      (if (looking-at "\\+")
                          (current-column)))
                    (save-excursion
                      (goto-char string-beg)
                      (if (looking-back "\\+\\s-+")
                          (goto-char (match-beginning 0)))
                      (current-column))))))
    (insert quote-char "\n")
    (indent-to indent)
    (insert "+ " quote-string)
    (when (eolp)
      (insert quote-string)
      (backward-char 1))))

(defun js2-mode-extend-comment ()
  "When inside a comment block, add comment prefix."
  (let (star single col first-line needs-close)
    (save-excursion
      (back-to-indentation)
      (cond
       ((looking-at "\\*[^/]")
        (setq star t
              col (current-column)))
       ((looking-at "/\\*")
        (setq star t
              first-line t
              col (1+ (current-column))))
       ((looking-at "//")
        (setq single t
              col (current-column)))))
    ;; Heuristic for whether we need to close the comment:
    ;; if we've got a parse error here, assume it's an unterminated
    ;; comment.
    (setq needs-close
          (eq (get-text-property (point) 'point-entered)
              'js2-echo-error))
    (insert "\n")
    (cond
     (star
      (indent-to col)
      (insert "* ")
      (if (and first-line needs-close)
          (save-excursion
            (insert "\n")
            (indent-to col)
            (insert "*/"))))
     (single
      (when (save-excursion
              (and (zerop (forward-line 1))
                   (looking-at "\\s-*//")))
        (indent-to col)
        (insert "// "))))))

(defun js2-fill-string (beg quote)
  "Line-wrap a single-line string into a multi-line string.
BEG is the string beginning, QUOTE is the quote char."
  (let* ((squote (string quote))
         (end (if (re-search-forward (concat "[^\\]" squote)
                                     (point-at-eol) t)
                  (1+ (match-beginning 0))
                (point-at-eol)))
         (tag (make-marker))
         (fill-column (- fill-column 4)))  ; make room
    (unwind-protect
        (progn
          (move-marker tag end)
          (fill-paragraph nil)
          (goto-char beg)
          (while (not (js2-same-line tag))
            (goto-char (point-at-eol))
            (insert squote)
            (when (zerop (forward-line 1))
              (back-to-indentation)
              (if (looking-at (concat squote "\\s-*$"))
                  (progn
                    (setq end (point-at-eol))
                    (forward-line -1)
                    (delete-region (point-at-eol) end))
                (insert "+ " squote)))))
      (move-marker tag nil))))

(defun js2-fill-paragraph (arg)
  "Fill paragraph after point.  Prefix ARG means justify as well.
Has special handling for filling in comments and strings."
  (let* ((parse-status (save-excursion
                         (parse-partial-sexp (point-min) (point))))
         (quote-char (or (nth 3 parse-status)
                         (save-match-data
                           (if (looking-at "[\"\']")
                               (char-after))))))
    (cond
     (quote-char
      (js2-fill-string (or (nth 8 parse-status)
                           (point))
                       quote-char)
      t) ; or fill-paragraph does evil things afterwards
     ((nth 4 parse-status)  ; in block comment?
      (js2-fill-comment parse-status arg))
     (t
      (fill-paragraph arg)))))

(defun js2-fill-comment (parse-status arg)
  "Fill-paragraph in a block comment."
  (let* ((beg (nth 8 parse-status))
         (end (save-excursion
                (goto-char beg)
                (re-search-forward "[^\\]\\*/" nil t)))
         indent
         end-marker)
    (when end
      (setq end-marker (make-marker))
      (move-marker end-marker end))
    (when (and end js2-mode-squeeze-spaces)
      (save-excursion
        (save-restriction
          (narrow-to-region beg end)
          (goto-char (point-min))
          (while (re-search-forward "[ \t][ \t]+" nil t)
            (replace-match " ")))))
    ;; `c-fill-paragraph' doesn't indent the continuation stars properly
    ;; if the comment isn't left-justified.  They align to the first star
    ;; on the first continuation line after the comment-open, so we make
    ;; sure the first continuation line has the proper indentation.
    (save-excursion
      (goto-char beg)
      (setq indent (1+ (current-column)))
      (goto-char (point-at-eol))
      (skip-chars-forward " \t\r\n")
      (indent-line-to indent)

      ;; Invoke `c-fill-paragraph' from the first continuation line,
      ;; since it provides better results.  Otherwise if you're on the
      ;; last line, it doesn't prefix with stars the way you'd expect.
      ;; TODO:  write our own fill function that works in Emacs 21
      (c-fill-paragraph arg))

    ;; last line is typically indented wrong, so fix it
    (when end-marker
      (save-excursion
        (goto-char end-marker)
        (js2-indent-line)))))

(defun js2-beginning-of-line ()
  "Toggles point between bol and first non-whitespace char in line.
Also moves past comment delimiters when inside comments."
  (interactive)
  (let (node beg)
    (cond
     ((bolp)
      (back-to-indentation))
     ((looking-at "//")
      (skip-chars-forward "/ \t"))
     ((and (eq (char-after) ?*)
           (setq node (js2-comment-at-point))
           (memq (js2-comment-node-format node) '(jsdoc block))
           (save-excursion
             (skip-chars-backward " \t")
             (bolp)))
      (skip-chars-forward "\* \t"))
     (t
      (goto-char (point-at-bol))))))

(defun js2-end-of-line ()
  "Toggles point between eol and last non-whitespace char in line."
  (interactive)
  (if (eolp)
      (skip-chars-backward " \t")
    (goto-char (point-at-eol))))

(defun js2-enter-mirror-mode()
  "Turns on mirror mode, where quotes, brackets etc are mirrored automatically
  on insertion."
  (interactive)
  (define-key js2-mode-map (read-kbd-macro "{")  'js2-mode-match-curly)
  (define-key js2-mode-map (read-kbd-macro "\"") 'js2-mode-match-double-quote)
  (define-key js2-mode-map (read-kbd-macro "'")  'js2-mode-match-single-quote)
  (define-key js2-mode-map (read-kbd-macro "(")  'js2-mode-match-paren)
  (define-key js2-mode-map (read-kbd-macro ")")  'js2-mode-magic-close-paren)
  (define-key js2-mode-map (read-kbd-macro "[")  'js2-mode-match-bracket)
  (define-key js2-mode-map (read-kbd-macro "]")  'js2-mode-magic-close-paren))

(defun js2-leave-mirror-mode()
  "Turns off mirror mode."
  (interactive)
  (dolist (key '("{" "\"" "'" "(" ")" "[" "]"))
    (define-key js2-mode-map (read-kbd-macro key) 'self-insert-command)))

(defsubst js2-mode-inside-string ()
  "Return non-nil if inside a string.
Actually returns the quote character that begins the string."
   (let ((parse-state (save-excursion
                        (parse-partial-sexp (point-min) (point)))))
      (nth 3 parse-state)))

(defsubst js2-mode-inside-comment-or-string ()
  "Return non-nil if inside a comment or string."
  (or
   (let ((comment-start
          (save-excursion
            (goto-char (point-at-bol))
            (if (re-search-forward "//" (point-at-eol) t)
                (match-beginning 0)))))
     (and comment-start
          (<= comment-start (point))))
   (let ((parse-state (save-excursion
                        (parse-partial-sexp (point-min) (point)))))
     (or (nth 3 parse-state)
         (nth 4 parse-state)))))

(defun js2-mode-match-curly ()
  "Insert matching curly-brace."
  (interactive)
  (insert "{")
  (unless (or (not (looking-at "\\s-*$"))
              (js2-mode-inside-comment-or-string))
    (insert "\n")
    (js2-indent-line)
    (save-excursion
      (insert "\n}")
      (let ((js2-bounce-indent-flag nil))
        (js2-indent-line)))))

(defun js2-mode-match-bracket ()
  "Insert matching bracket."
  (interactive)
  (insert "[")
  (unless (or (not (looking-at "\\s-*$"))
              (js2-mode-inside-comment-or-string))
    (save-excursion
      (insert "]"))
    (when js2-auto-indent-flag
      (let ((js2-bounce-indent-flag nil))
        (js2-indent-line)))))

(defun js2-mode-match-paren ()
  "Insert matching paren unless already inserted."
  (interactive)
  (insert "(")
  (unless (or (not (looking-at "\\s-*$"))
              (js2-mode-inside-comment-or-string))
    (save-excursion
      (insert ")"))
    (when js2-auto-indent-flag
      (let ((js2-bounce-indent-flag nil))
        (js2-indent-line)))))

(defsubst js2-match-quote (quote-string)
  (let ((start-quote (js2-mode-inside-string)))
    (cond
     ((not start-quote)
      ;; not in string => insert matched quotes
      (insert quote-string)
      ;; exception:  if we're just before a word, don't double it.
      (unless (looking-at "[^ \t\r\n]")
        (save-excursion
          (insert quote-string))))
     ((looking-at quote-string)
      (forward-char 1))
     ((and js2-mode-escape-quotes
           (save-excursion
             (save-match-data
               (re-search-forward quote-string (point-at-eol) t))))
      ;; inside terminated string, escape quote
      (insert (concat "\\" quote-string)))
     (t
      (insert quote-string)))))        ; else terminate the string

(defun js2-mode-match-single-quote ()
  "Insert matching single-quote."
  (interactive)
  (let ((parse-status (parse-partial-sexp (point-min) (point))))
    ;; don't match inside comments, since apostrophe is more common
    (if (nth 4 parse-status)
        (insert "'")
      (js2-match-quote "'"))))

(defun js2-mode-match-double-quote ()
  "Insert matching double-quote."
  (interactive)
  (js2-match-quote "\""))

(defun js2-mode-magic-close-paren ()
  "Skip over close-paren rather than inserting, where appropriate.
Uses some heuristics to try to figure out the right thing to do."
  (interactive)
  (let* ((parse-status (parse-partial-sexp (point-min) (point)))
         (open-pos (nth 1 parse-status))
         (close last-input-char)
         (open (cond
                ((eq close 41)  ; close-paren
                 40)            ; open-paren
                ((eq close 93)  ; close-bracket
                 91)            ; open-bracket
                (t nil))))
    (if (and (looking-at (string close))
             (eq open (char-after open-pos))
             (js2-same-line open-pos))
        (forward-char 1)
      (insert (string close)))))

(defun js2-mode-wait-for-parse (callback)
  "Invoke CALLBACK when parsing is finished.
If parsing is already finished, calls CALLBACK immediately."
  (if (not js2-mode-buffer-dirty-p)
      (funcall callback)
    (push callback js2-mode-pending-parse-callbacks)
    (add-hook 'js2-parse-finished-hook #'js2-mode-parse-finished)))

(defun js2-mode-parse-finished ()
  "Invoke callbacks in `js2-mode-pending-parse-callbacks'."
  ;; We can't let errors propagate up, since it prevents the
  ;; `js2-parse' method from completing normally and returning
  ;; the ast, which makes things mysteriously not work right.
  (unwind-protect
      (dolist (cb js2-mode-pending-parse-callbacks)
        (condition-case err
            (funcall cb)
          (error (message "%s" err))))
    (setq js2-mode-pending-parse-callbacks nil)))

(defun js2-mode-flag-region (from to flag)
  "Hide or show text from FROM to TO, according to FLAG.
If FLAG is nil then text is shown, while if FLAG is t the text is hidden.
Returns the created overlay if FLAG is non-nil."
  (remove-overlays from to 'invisible 'js2-outline)
  (when flag
    (let ((o (make-overlay from to)))
      (overlay-put o 'invisible 'js2-outline)
      (overlay-put o 'isearch-open-invisible
                   'js2-isearch-open-invisible)
      o)))

;; Function to be set as an outline-isearch-open-invisible' property
;; to the overlay that makes the outline invisible (see
;; `js2-mode-flag-region').
(defun js2-isearch-open-invisible (overlay)
  ;; We rely on the fact that isearch places point on the matched text.
  (js2-mode-show-element))

(defun js2-mode-invisible-overlay-bounds (&optional pos)
  "Return cons cell of bounds of folding overlay at POS.
Returns nil if not found."
  (let ((overlays (overlays-at (or pos (point))))
        o)
    (while (and overlays
                (not o))
      (if (overlay-get (car overlays) 'invisible)
          (setq o (car overlays))
        (setq overlays (cdr overlays))))
    (if o
        (cons (overlay-start o) (overlay-end o)))))

(defun js2-mode-function-at-point (&optional pos)
  "Return the innermost function node enclosing current point.
Returns nil if point is not in a function."
  (let ((node (js2-node-at-point pos)))
    (while (and node (not (js2-function-node-p node)))
      (setq node (js2-node-parent node)))
    (if (js2-function-node-p node)
        node)))

(defun js2-mode-toggle-element ()
  "Hide or show the foldable element at the point."
  (interactive)
  (let (comment fn pos)
    (goto-char (point))
    (save-excursion
      (cond
       ((js2-block-comment-p (setq comment (js2-comment-at-point)))
        (if (js2-mode-invisible-overlay-bounds
             (setq pos (+ 3 (js2-node-abs-pos comment))))
            (progn
              (goto-char pos)
              (js2-mode-show-element))
          (js2-mode-hide-element)))
       ((setq fn (js2-mode-function-at-point))
        (setq pos (and (js2-function-node-body fn)
                       (js2-node-abs-pos (js2-function-node-body fn))))
        (goto-char (1+ pos))
        (if (js2-mode-invisible-overlay-bounds)
            (js2-mode-show-element)
          (js2-mode-hide-element)))))))

(defun js2-mode-hide-element ()
  "Fold/hide contents of a block, showing ellipses.
Show the hidden text with \\[js2-mode-show-element]."
  (interactive)
  (if js2-mode-buffer-dirty-p
      (js2-mode-wait-for-parse #'js2-mode-hide-element))
  (let (node body beg end)
    (cond
     ((js2-mode-invisible-overlay-bounds)
      (message "already hidden"))
     (t
      (setq node (js2-node-at-point))
      (cond
       ((js2-block-comment-p node)
        (js2-mode-hide-comment node))
       (t
        (while (and node (not (js2-function-node-p node)))
          (setq node (js2-node-parent node)))
        (if (and node
                 (setq body (js2-function-node-body node)))
            (progn
              (setq beg (js2-node-abs-pos body)
                    end (+ beg (js2-node-len body)))
              (js2-mode-flag-region (1+ beg) (1- end) t))
          (message "No collapsable element found at point"))))))))

(defun js2-mode-show-element ()
  "Show the hidden element at current point."
  (interactive)
  (let ((bounds (js2-mode-invisible-overlay-bounds)))
    (if bounds
        (js2-mode-flag-region (car bounds) (cdr bounds) nil)
      (message "Nothing to un-hide"))))

(defun js2-mode-show-all ()
  "Show all of the text in the buffer."
  (interactive)
  (js2-mode-flag-region (point-min) (point-max) nil))

(defun js2-mode-toggle-hide-functions ()
  (interactive)
  (if js2-mode-functions-hidden
      (js2-mode-show-functions)
    (js2-mode-hide-functions)))

(defun js2-mode-hide-functions ()
  "Hides all non-nested function bodies in the buffer.
Use \\[js2-mode-show-all] to reveal them, or \\[js2-mode-show-element]
to open an individual entry."
  (interactive)
  (if js2-mode-buffer-dirty-p
      (js2-mode-wait-for-parse #'js2-mode-hide-functions))
  (if (null js2-mode-ast)
      (message "Oops - parsing failed")
    (setq js2-mode-functions-hidden t)
    (js2-visit-ast js2-mode-ast #'js2-mode-function-hider)))

(defun js2-mode-function-hider (n endp)
  (when (not endp)
    (let ((tt (js2-node-type n))
          body beg end)
      (cond
       ((and (= tt js2-FUNCTION)
             (setq body (js2-function-node-body n)))
        (setq beg (js2-node-abs-pos body)
              end (+ beg (js2-node-len body)))
        (js2-mode-flag-region (1+ beg) (1- end) t)
        nil)   ; don't process children of function
       (t
        t))))) ; keep processing other AST nodes

(defun js2-mode-show-functions ()
  "Un-hide any folded function bodies in the buffer."
  (interactive)
  (setq js2-mode-functions-hidden nil)
  (save-excursion
    (goto-char (point-min))
    (while (/= (goto-char (next-overlay-change (point)))
               (point-max))
      (dolist (o (overlays-at (point)))
        (when (and (overlay-get o 'invisible)
                   (not (overlay-get o 'comment)))
          (js2-mode-flag-region (overlay-start o) (overlay-end o) nil))))))

(defun js2-mode-hide-comment (n)
  (let* ((head (if (eq (js2-comment-node-format n) 'jsdoc)
                   3                    ; /**
                 2))                    ; /*
         (beg (+ (js2-node-abs-pos n) head))
         (end (- (+ beg (js2-node-len n)) head 2))
         (o (js2-mode-flag-region beg end t)))
    (overlay-put o 'comment t)))

(defun js2-mode-toggle-hide-comments ()
  "Folds all block comments in the buffer.
Use \\[js2-mode-show-all] to reveal them, or \\[js2-mode-show-element]
to open an individual entry."
  (interactive)
  (if js2-mode-comments-hidden
      (js2-mode-show-comments)
    (js2-mode-hide-comments)))

(defun js2-mode-hide-comments ()
  (interactive)
  (if js2-mode-buffer-dirty-p
      (js2-mode-wait-for-parse #'js2-mode-hide-comments))
  (if (null js2-mode-ast)
      (message "Oops - parsing failed")
    (setq js2-mode-comments-hidden t)
    (dolist (n (js2-ast-root-comments js2-mode-ast))
      (let ((format (js2-comment-node-format n)))
        (when (js2-block-comment-p n)
          (js2-mode-hide-comment n))))))

(defun js2-mode-show-comments ()
  "Un-hide any hidden comments, leaving other hidden elements alone."
  (interactive)
  (setq js2-mode-comments-hidden nil)
  (save-excursion
    (goto-char (point-min))
    (while (/= (goto-char (next-overlay-change (point)))
               (point-max))
      (dolist (o (overlays-at (point)))
        (when (overlay-get o 'comment)
          (js2-mode-flag-region (overlay-start o) (overlay-end o) nil))))))

(defun js2-mode-customize ()
  (interactive)
  (customize-group 'js2-mode))

(defun js2-mode-forward-sexp (&optional arg)
  "Move forward across one statement or balanced expression.
With ARG, do it that many times.  Negative arg -N means
move backward across N balanced expressions."
  (interactive "p")
  (setq arg (or arg 1))
  (if js2-mode-buffer-dirty-p
      (js2-mode-wait-for-parse #'js2-mode-forward-sexp))
  (let (node end (start (point)))
    (cond
     ;; backward-sexp
     ;; could probably make this "better" for some cases:
     ;;  - if in statement block (e.g. function body), go to parent
     ;;  - infix exprs like (foo in bar) - maybe go to beginning
     ;;    of infix expr if in the right-side expression?
     ((and arg (minusp arg))
      (dotimes (i (- arg))
        (js2-backward-sws)
        (forward-char -1)  ; enter the node we backed up to
        (setq node (js2-node-at-point (point) t))
        (goto-char (if node
                       (js2-node-abs-pos node)
                     (point-min)))))
    (t
     ;; forward-sexp
     (js2-forward-sws)
     (dotimes (i arg)
       (js2-forward-sws)
       (setq node (js2-node-at-point (point) t)
             end (if node (+ (js2-node-abs-pos node)
                             (js2-node-len node))))
       (goto-char (or end (point-max))))))))

(defun js2-next-error (&optional arg reset)
  "Move to next parse error.
Typically invoked via \\[next-error].
ARG is the number of errors, forward or backward, to move.
RESET means start over from the beginning."
  (interactive "p")
  (if (or (null js2-mode-ast)
          (and (null (js2-ast-root-errors js2-mode-ast))
               (null (js2-ast-root-warnings js2-mode-ast))))
      (message "No errors")
    (when reset
      (goto-char (point-min)))
    (let* ((errs (copy-sequence
                  (append (js2-ast-root-errors js2-mode-ast)
                          (js2-ast-root-warnings js2-mode-ast))))
           (continue t)
           (here (point))
           (count (or arg 1))
           (backward (minusp count))
           (sorter (if backward '> '<))
           (stopper (if backward '< '>))
           (count (abs count))
           err)
      ;; sort by start position
      (setq errs (sort errs (lambda (e1 e2)
                              (funcall sorter (second e1) (second e2)))))
      ;; find nth error with pos > here
      (while (and errs continue)
        (when (funcall stopper (cadar errs) here)
          (setq err (car errs))
          (if (zerop (decf count))
              (setq continue nil)))
        (setq errs (cdr errs)))
      (if err
          (goto-char (second err))
        ;; wrap around to first error
        (js2-next-error 1 t)))))

(defun js2-mouse-3 ()
  "Make right-click move the point to the click location.
This makes right-click context menu operations a bit more intuitive.
The point will not move if the region is active, however, to avoid
destroying the region selection."
  (interactive)
  (when (and js2-move-point-on-right-click
             (not mark-active))
    (let ((e last-input-event))
      (ignore-errors
        (goto-char (cadadr e))))))

(defun js2-mode-create-imenu-index ()
  "Return an alist for `imenu--index-alist'."
  ;; This is built up in `js2-parse-record-imenu' during parsing.
  (nreverse js2-imenu-recorder))

(defun js2-mode-find-tag ()
  "Replacement for `find-tag-default'.
`find-tag-default' returns a ridiculous answer inside comments."
  (let (beg end)
    (js2-with-underscore-as-word-syntax
      (save-excursion
        (setq beg (progn (forward-word 1) (point))
              end (progn (forward-word -1) (point))))
      (replace-regexp-in-string
       "[\"']" ""
       (buffer-substring-no-properties beg end)))))

(defun js2-mode-forward-sibling ()
  "Move to the end of the sibling following point in parent.
Returns non-nil if successful, or nil if there was no following sibling."
  (let* ((node (js2-node-at-point))
         (parent (js2-mode-find-enclosing-fn node))
         sib)
    (when (setq sib (js2-node-find-child-after (point) parent))
      (goto-char (+ (js2-node-abs-pos sib)
                    (js2-node-len sib))))))

(defun js2-mark-defun (&optional allow-extend)
  "Put mark at end of this function, point at beginning.
The function marked is the one that contains point.

Interactively, if this command is repeated,
or (in Transient Mark mode) if the mark is active,
it marks the next defun after the ones already marked."
  (interactive "p")
  (let (extended)
    (when (and allow-extend
               (or (and (eq last-command this-command) (mark t))
                   (and transient-mark-mode mark-active)))
      (let ((sib (save-excursion
                   (goto-char (mark))
                   (if (js2-mode-forward-sibling)
                       (point))))
            node)
        (if sib
            (progn
              (set-mark sib)
              (setq extended t))
          ;; no more siblings - try extending to enclosing node
          (goto-char (mark t)))))
   (when (not extended)
     (let ((node (js2-node-at-point (point) t)) ; skip comments
           ast fn stmt parent beg end)
       (when (js2-ast-root-p node)
         (setq ast node
               node (or (js2-node-find-child-after (point) node)
                        (js2-node-find-child-before (point) node))))
       ;; only mark whole buffer if we can't find any children
       (if (null node)
           (setq node ast))
       (if (js2-function-node-p node)
           (setq parent node)
         (setq fn (js2-mode-find-enclosing-fn node)
               stmt (if (or (null fn)
                            (js2-ast-root-p fn))
                        (js2-mode-find-first-stmt node))
               parent (or stmt fn)))
       (setq beg (js2-node-abs-pos parent)
             end (+ beg (js2-node-len parent)))
       (push-mark beg)
       (goto-char end)
       (exchange-point-and-mark)))))

(defun js2-narrow-to-defun ()
  "Narrow to the function enclosing point."
  (interactive)
  (let* ((node (js2-node-at-point (point) t)) ;; skip comments
         (fn (if (js2-script-node-p node)
                 node
               (js2-mode-find-enclosing-fn node)))
         (beg (js2-node-abs-pos fn)))
    (unless (js2-ast-root-p fn)
      (narrow-to-region beg (+ beg (js2-node-len fn))))))

(defalias 'js2r 'js2-reset)

(provide 'js2-mode)

;;; js2-mode.el ends here
