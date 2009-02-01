;;; its/aynu.el --- Aynu Katakana Input in Egg Input Method Architecture

;; Copyright (C) 1999,2000 PFU LIMITED

;; Author: KATAYAMA Yoshio <kate@pfu.co.jp>

;; Maintainer: TOMURA Satoru <tomura@etl.go.jp>

;; Keywords: mule, multilingual, input method

;; This file is part of EGG.

;; EGG is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; EGG is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:


;;; Code:

(eval-when-compile
  (require 'its)
  (require 'cl))

(eval-when (compile)
  (defconst its-compaction-enable t))

(defvar its-aynu-enable-zenkaku-alphabet
  (if (boundp 'its-enable-fullwidth-alphabet)
      its-enable-fullwidth-alphabet
    t)
  "*Enable Zenkaku alphabet")

(defvar its-aynu-horizontal      "$(O!<(B" "*-")	; "-" "$(O!=(B"
(defvar	its-aynu-period          "$(O!#(B " "*.")	; "." "$(O!#(B"
(defvar	its-aynu-comma           "$(O!$(B " "*,")	; "," "$(O!$(B"
(defvar its-aynu-open-bracket    "$(O!V(B" "*[")	; "$(O!N(B"
(defvar its-aynu-close-bracket   "$(O!W(B" "*]")	; "$(O!O(B"

(defvar its-aynu-enable-double-n nil "*Enable \"nn\" input for \"$(O%s(B\"")

(defvar its-aynu-kick-conversion-on-space nil "*Start conversion on SPACE")

(eval-when-compile
  (defun its-define-state-aynu (input i-tail output o-tail otherwise)
    "Define following rules:
INPUT + I-TAIL            --> OUTPUT + O-TAIL
INPUT + I-TAIL + '        --> OUTPUT + O-TAIL
INPUT + I-TAIL + vowel    --> (translate INPUT) + I-tail + vowel
INPUT + I-TAIL + OTHERWISE  (see `its-defrule-otherwise')."
    (let ((out (concat output o-tail))
	  state)
      (setq state (its-defrule (concat input i-tail) out))
      (its-defrule (concat input i-tail "'") out)
      (its-defrule-otherwise state nil "[aiueo]" -2)
      (while otherwise
	(its-defrule-otherwise state (concat output (caar otherwise))
			       (nth 1 (car otherwise)) (nth 2 (car otherwise)))
	(setq otherwise (cdr otherwise)))
      (setq state (its-defrule (concat input i-tail "y") (concat out "$(O%#(B")))
      (its-make-next-state state -1 out -1)
      (its-defrule-otherwise state out nil -2)
      (its-defrule-otherwise state nil "[u]" -3)
))

  (defconst its-aynu-tail-alist
    (let ((common '(("k" "$(O&n(B" (("$(O%C(B" "[k]"  -1)))
		    ("s" "$(O&o(B" (("$(O%C(B" "[s]"  -1) (nil "[h]" -2)))
		    ("p" "$(O&x(B" (("$(O%C(B" "[p]"  -1)))
		    ("m" "$(O&y(B" (("$(O%s(B" "[mp]" -1)))
		    ("t" "$(O%C(B") ("y" "$(O%#(B") ("w" "$(O%%(B"))))
      `((?a ("h" "$(O&s(B") ("x" "$(O&s(B") ("r" "$(O&z(B") ,@common)
	(?i ("h" "$(O&t(B") ("x" "$(O&t(B") ("r" "$(O&{(B") ,@common)
	(?u ("h" "$(O&u(B") ("x" "$(O&u(B") ("r" "$(O&|(B") ,@common)
	(?e ("h" "$(O&v(B") ("x" "$(O&v(B") ("r" "$(O&}(B") ,@common)
	(?o ("h" "$(O&w(B") ("x" "$(O&w(B") ("r" "$(O&~(B") ,@common))))

  (defun its-defrule-aynu (conso vowel output)
    (let ((input (concat conso vowel))
	  (tails (and vowel (cdr (assq (aref vowel 0) its-aynu-tail-alist)))))
      (its-defrule input output)
      (while tails
	(its-define-state-aynu input (caar tails) output (nth 1 (car tails))
			       (nth 2 (car tails)))
	(setq tails (cdr tails)))))

  (defmacro its-define-aynu (&rest rules)
    (let ((defs (list 'progn))
	  conso vowels output)
      (while rules
	(setq vowels '(nil "a" "i" "u" "e" "o")
	      conso  (caar rules)
	      output (cdar rules)
	      rules (cdr rules))
	(while output
	  (when (car output)
	    (setq defs (cons `(its-defrule-aynu ,conso ,(car vowels)
						,(car output))
			     defs)))
	  (setq output (cdr output)
		vowels (cdr vowels))))
      (nreverse defs)))

  (defun its-defrule-aynu-override-yu (conso)
    (let ((output (its-get-output (its-goto-state conso)))
	  state)
      (its-defrule (concat conso "yu")
		   (concat (its-get-output (its-goto-state (concat conso "i")))
			   "$(O%e!<(B"))
      (setq state (its-goto-state (concat conso "y")))
      (its-set-output state (concat output "$(O%#(B"))
      (its-make-next-state state -1 output -1)
      (its-defrule-otherwise state output nil -2))))

(define-its-state-machine its-aynu-map
  "roma-aynu-kata" "$(O%"(B" Aynu
  "Map for Romaji-Aynu-Katakana translation. (Japanese)"

  (defconst its-zenkaku-escape "Z")  ;; Escape character to Zenkaku inputs
  (defconst its-hankaku-escape "~")  ;; Escape character to Hankaku inputs

  (its-defrule-select-mode-temporally "q" downcase)
  (its-defrule-select-mode-temporally "Q" zenkaku-downcase)

  (dolist (small '(("a"  "$(O%!(B") ("i"  "$(O%#(B") ("u"  "$(O%%(B") ("e"  "$(O%'(B") ("o"  "$(O%)(B")
		   ("ka" "$(O%u(B")             ("ku" "$(O&n(B") ("ke" "$(O%v(B")
		               ("si" "$(O&o(B") ("su" "$(O&p(B")
		                           ("tu" "$(O%C(B")             ("to" "$(O&q(B")
		                           ("nu" "$(O&r(B")
		   ("ha" "$(O&s(B") ("hi" "$(O&t(B") ("hu" "$(O&u(B") ("he" "$(O&v(B") ("ho" "$(O&w(B")
		                           ("pu" "$(O&x(B")
		                           ("mu" "$(O&y(B")
		   ("ya" "$(O%c(B")             ("yu" "$(O%e(B")             ("yo" "$(O%g(B")
		   ("ra" "$(O&z(B") ("ri" "$(O&{(B") ("ru" "$(O&|(B") ("re" "$(O&}(B") ("ro" "$(O&~(B")
		   ("wa" "$(O%n(B")))
    (its-defrule (concat "x" (car small)) (cadr small)))

  (its-define-aynu
   (""   nil	"$(O%"(B"   "$(O%$(B"   "$(O%&(B"   "$(O%((B"   "$(O%*(B")
   ("k"  "$(O&n(B"		"$(O%+(B"   "$(O%-(B"   "$(O%/(B"   "$(O%1(B"   "$(O%3(B")
   ("g"  "$(O%0(B"   "$(O%,(B"   "$(O%.(B"   "$(O%0(B"   "$(O%2(B"   "$(O%4(B")
   ("s"  "$(O&p(B"		"$(O%5(B"   "$(O%7(B"   "$(O%9(B"   "$(O%;(B"   "$(O%=(B")
   ("z"  nil    "$(O%6(B"   "$(O%8(B"   "$(O%:(B"   "$(O%<(B"   "$(O%>(B")
   ("vs" nil    nil    nil    nil    "$(O%|(B"   nil)
   ("sh" "$(O%7%c(B" "$(O%7%c(B" "$(O%7(B"   "$(O%7%e(B" "$(O%7%'(B" "$(O%7%g(B")
   ("j"  nil    "$(O%8%c(B" "$(O%8(B"   "$(O%8%e(B" "$(O%8%'(B" "$(O%8%g(B")
   ("t"  "$(O%C(B"   "$(O%?(B"   "$(O%A(B"   "$(O%H%%(B" "$(O%F(B"   "$(O%H(B")
   ("vt" nil    nil    nil    "$(O%}(B"   nil    "$(O%~(B")
   ("d"  nil    "$(O%@(B"   "$(O%B(B"   "$(O%E(B"   "$(O%G(B"   "$(O%I(B")
   ("c"  "$(O%C(B"   "$(O%A%c(B" "$(O%A(B"   "$(O%A%e(B" "$(O%A%'(B" "$(O%A%g(B")
   ("ch" "$(O%C(B"   "$(O%A%c(B" "$(O%A(B"   "$(O%A%e(B" "$(O%A%'(B" "$(O%A%g(B")
   ("n"  "$(O%s(B"   "$(O%J(B"   "$(O%K(B"   "$(O%L(B"   "$(O%M(B"   "$(O%N(B")
   ("h"  "$(O&s(B"   "$(O%O(B"   "$(O%R(B"   "$(O%U(B"   "$(O%X(B"   "$(O%[(B")
   ("b"  nil    "$(O%P(B"   "$(O%S(B"   "$(O%V(B"   "$(O%Y(B"   "$(O%\(B")
   ("p"  "$(O&x(B"   "$(O%Q(B"   "$(O%T(B"   "$(O%W(B"   "$(O%Z(B"   "$(O%](B")
   ("m"  "$(O&y(B"   "$(O%^(B"   "$(O%_(B"   "$(O%`(B"   "$(O%a(B"   "$(O%b(B")
   ("y"  "$(O%#(B"   "$(O%d(B"   "$(O%#(B"   "$(O%f(B"   "$(O%$%'(B" "$(O%h(B")
   ("r"  "$(O&|(B"   "$(O%i(B"   "$(O%j(B"   "$(O%k(B"   "$(O%l(B"   "$(O%m(B")
   ("w"  "$(O%%(B"   "$(O%o(B"   "$(O%&%#(B" "$(O%%(B"   "$(O%&%'(B" "$(O%&%)(B"))

  (dolist (yu '("k" "g" "s" "z" "sh" "j" "t" "d"
		"c" "ch" "n" "h" "b" "p" "m" "r"))
    (its-defrule-aynu-override-yu yu))

  (its-defrule "kk" "$(O%C(B" -1)
  (its-defrule "ss" "$(O%C(B" -1)
  (its-defrule "pp" "$(O%C(B" -1)
  (its-defrule "vv" "$(O%C(B" -1)

;; SYMBOL Input
  (its-defrule   "z1"   "$(O!{(B")	(its-defrule   "z!"   "$(O!|(B")
  (its-defrule   "z2"   "$(O"&(B")	(its-defrule   "z@"   "$(O"'(B")
  (its-defrule   "z3"   "$(O"$(B")	(its-defrule   "z#"   "$(O"%(B")
  (its-defrule   "z4"   "$(O""(B")	(its-defrule   "z$"   "$(O"#(B")
  (its-defrule   "z5"   "$(O!~(B")	(its-defrule   "z%"   "$(O"!(B")
  (its-defrule   "z6"   "$(O!y(B")	(its-defrule   "z^"   "$(O!z(B")
  (its-defrule   "z7"   "$(O!}(B")	(its-defrule   "z&"   "$(O!r(B")
  (its-defrule   "z8"   "$(O!q(B")	(its-defrule   "z*"   "$(O!_(B")
  (its-defrule   "z9"   "$(O!i(B")	(its-defrule   "z("   "$(O!Z(B")
  (its-defrule   "z0"   "$(O!j(B")	(its-defrule   "z)"   "$(O![(B")
  (its-defrule   "z-"   "$(O!A(B")	(its-defrule   "z_"   "$(O!h(B")
  (its-defrule   "z="   "$(O!b(B")	(its-defrule   "z+"   "$(O!^(B")
  (its-defrule   "z\\"  "$(O!@(B")	(its-defrule   "z|"   "$(O!B(B")
  (its-defrule   "z`"   "$(O!-(B")	(its-defrule   "z~"   "$(O!/(B")

  (its-defrule   "zq"   "$(O!T(B")	(its-defrule   "zQ"   "$(O!R(B")
  (its-defrule   "zw"   "$(O!U(B")	(its-defrule   "zW"   "$(O!S(B")
					; e
  (its-defrule   "zr"   "$(O!9(B")	(its-defrule   "zR"   "$(O!8(B")
  (its-defrule   "zt"   "$(O!:(B")	(its-defrule   "zT"   "$(O!x(B")
					; y u i o
  (its-defrule   "zp"   "$(O")(B")	(its-defrule   "zP"   "$(O",(B")
  (its-defrule   "z["   "$(O!X(B")	(its-defrule   "z{"   "$(O!L(B")
  (its-defrule   "z]"   "$(O!Y(B")	(its-defrule   "z}"   "$(O!M(B")
					; a
  (its-defrule   "zs"   "$(O!3(B")	(its-defrule   "zS"   "$(O!4(B")
  (its-defrule   "zd"   "$(O!5(B")	(its-defrule   "zD"   "$(O!6(B")
  (its-defrule   "zf"   "$(O!7(B")	(its-defrule   "zF"   "$(O"*(B")
  (its-defrule   "zg"   "$(O!>(B")	(its-defrule   "zG"   "$(O!=(B")
  (its-defrule   "zh"   "$(O"+(B")
  (its-defrule   "zj"   "$(O"-(B")
  (its-defrule   "zk"   "$(O",(B")
  (its-defrule   "zl"   "$(O"*(B")
  (its-defrule   "z;"   "$(O!+(B")	(its-defrule   "z:"   "$(O!,(B")
  (its-defrule   "z\'"  "$(O!F(B")	(its-defrule   "z\""  "$(O!H(B")
					; z
  (its-defrule   "zx"   ":-")	(its-defrule   "zX"   ":-)")
  (its-defrule   "zc"   "$(O!;(B")	(its-defrule   "zC"   "$(O!n(B")
  (its-defrule   "zv"   "$(O"((B")	(its-defrule   "zV"   "$(O!`(B")
  (its-defrule   "zb"   "$(O!k(B")	(its-defrule   "zB"   "$(O"+(B")
  (its-defrule   "zn"   "$(O!l(B")	(its-defrule   "zN"   "$(O"-(B")
  (its-defrule   "zm"   "$(O!m(B")	(its-defrule   "zM"   "$(O".(B")
  (its-defrule   "z,"   "$(O!E(B")	(its-defrule   "z<"   "$(O!e(B")
  (its-defrule   "z."   "$(O!D(B")	(its-defrule   "z>"   "$(O!f(B")
  (its-defrule   "z/"   "$(O!&(B")	(its-defrule   "z?"   "$(O!g(B")
  )

(define-its-state-machine-append its-aynu-map
  (if its-aynu-enable-double-n
      (its-defrule "nn" "$(O%s(B"))

  (its-defrule "-" its-aynu-horizontal)
  (its-defrule "." its-aynu-period)
  (its-defrule "," its-aynu-comma)
  (its-defrule "[" its-aynu-open-bracket)
  (its-defrule "]" its-aynu-close-bracket)

  (unless its-aynu-kick-conversion-on-space
    (its-defrule " " " "))

  (if its-aynu-enable-zenkaku-alphabet
      (progn
	(its-defrule   "1"   "$(O#1(B")  (its-defrule   "2"   "$(O#2(B")
	(its-defrule   "3"   "$(O#3(B")  (its-defrule   "4"   "$(O#4(B")
	(its-defrule   "5"   "$(O#5(B")  (its-defrule   "6"   "$(O#6(B")
	(its-defrule   "7"   "$(O#7(B")  (its-defrule   "8"   "$(O#8(B")
	(its-defrule   "9"   "$(O#9(B")  (its-defrule   "0"   "$(O#0(B")
	(its-defrule   "!"   "$(O!*(B")  (its-defrule   "@"   "$(O!w(B")
	(its-defrule   "#"   "$(O!t(B")  (its-defrule   "$"   "$(O!p(B")
	(its-defrule   "%"   "$(O!s(B")  (its-defrule   "^"   "$(O!0(B")
	(its-defrule   "&"   "$(O!u(B")  (its-defrule   "*"   "$(O!v(B")
	(its-defrule   "("   "$(O!J(B")  (its-defrule   ")"   "$(O!K(B")
	(its-defrule   "="   "$(O!a(B")  (its-defrule   "`"   "$(O!.(B")
	(its-defrule   "\\"  "$(O!o(B")  (its-defrule   "|"   "$(O!C(B")
	(its-defrule   "_"   "$(O!2(B")  (its-defrule   "+"   "$(O!\(B")
	(its-defrule   "{"   "$(O!P(B")  (its-defrule   "}"   "$(O!Q(B")
	(its-defrule   ":"   "$(O!'(B")  (its-defrule   ";"   "$(O!((B")
	(its-defrule   "\""  "$(O!I(B")  (its-defrule   "'"   "$(O!G(B")
	(its-defrule   "<"   "$(O!c(B")  (its-defrule   ">"   "$(O!d(B")
	(its-defrule   "?"   "$(O!)(B")  (its-defrule   "/"   "$(O!?(B"))
    (progn
      (its-defrule   "1"   "1")  (its-defrule   "2"   "2")
      (its-defrule   "3"   "3")  (its-defrule   "4"   "4")
      (its-defrule   "5"   "5")  (its-defrule   "6"   "6")
      (its-defrule   "7"   "7")  (its-defrule   "8"   "8")
      (its-defrule   "9"   "9")  (its-defrule   "0"   "0")
      (its-defrule   "!"   "!")  (its-defrule   "@"   "@")
      (its-defrule   "#"   "#")  (its-defrule   "$"   "$")
      (its-defrule   "%"   "%")  (its-defrule   "^"   "^")
      (its-defrule   "&"   "&")  (its-defrule   "*"   "*")
      (its-defrule   "("   "(")  (its-defrule   ")"   ")")
      (its-defrule   "="   "=")  (its-defrule   "`"   "`")
      (its-defrule   "\\"  "\\") (its-defrule   "|"   "|")
      (its-defrule   "_"   "_")  (its-defrule   "+"   "+")
      (its-defrule   "{"   "{")  (its-defrule   "}"   "}")
      (its-defrule   ":"   ":")  (its-defrule   ";"   ";")
      (its-defrule   "\""  "\"") (its-defrule   "'"   "'")
      (its-defrule   "<"   "<")  (its-defrule   ">"   ">")
      (its-defrule   "?"   "?")  (its-defrule   "/"   "/"))))

(provide 'its/aynu)

;;; its/aynu.el ends here
