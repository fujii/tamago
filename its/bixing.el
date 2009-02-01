;;; its/bixing.el --- Bixing (stroke) Input in Egg Input Method Architecture

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

(defvar its-qianma-enable-quanjioao-alphabet
  (if (boundp 'its-enable-fullwidth-alphabet)
      its-enable-fullwidth-alphabet
    t)
  "*Enable Quanjiao alphabet")

(defvar its-qianma-open-braket  "$A!8(B" "*[") ; "$A#[(B"
(defvar its-qianma-close-braket "$A!9(B" "*]") ; "$A#](B"

(defvar its-wubi-enable-quanjioao-alphabet
  (if (boundp 'its-enable-fullwidth-alphabet)
      its-enable-fullwidth-alphabet
    t)
  "*Enable Quanjiao alphabet")

(defvar its-wubi-open-braket  "$A!8(B" "*[") ; "$A#[(B"
(defvar its-wubi-close-braket "$A!9(B" "*]") ; "$A#](B"

(its-set-stroke-input '((QianMa . 3) (WuBi . 4)))

(egg-set-message-language-alist '((QianMa . Chinese-GB) (WuBi . Chinese-GB)))

(define-its-state-machine its-qianma-map
  "qinama" "$AG.(B" QianMa
  "Map for QianMa input. (Chinese-GB)"

  (defconst its-quanjiao-escape "Z")
  (defconst its-banjiao-escape  "X")

  (its-defrule-select-mode-temporally "B" downcase)
  (its-defrule-select-mode-temporally "Q" quanjiao-downcase-cn)

  (let ((ch (string-to-list "0123456789abcdefghijklmnopqrstuvwxyz;=@[]")))
    (while ch
      (its-defrule (char-to-string (car ch)) (char-to-string (car ch)))
      (setq ch (cdr ch))))

  (dolist (ascii '(("0" . "$A#0(B")  ("1" . "$A#1(B")  ("2" . "$A#2(B")  ("3" . "$A#3(B")
		   ("4" . "$A#4(B")  ("5" . "$A#5(B")  ("6" . "$A#6(B")  ("7" . "$A#7(B")
		   ("8" . "$A#8(B")  ("9" . "$A#9(B") 
		   (" " . "$A!!(B")  ("!" . "$A#!(B")  ("@" . "$A#@(B")  ("#" . "$A##(B")
		   ("$" . "$A!g(B")  ("%" . "$A#%(B")  ("^" . "$A#^(B")  ("&" . "$A#&(B")
		   ("*" . "$A#*(B")  ("(" . "$A#((B")  (")" . "$A#)(B")
		   ("-" . "$A#-(B")  ("=" . "$A#=(B")  ("`" . "$A#`(B")  ("\\" . "$A#\(B")
		   ("|" . "$A#|(B")  ("_" . "$A#_(B")  ("+" . "$A#+(B")  ("~" . "$A!+(B")
		   ("[" . "$A#[(B")  ("]" . "$A#](B")  ("{" . "$A#{(B")  ("}" . "$A#}(B")
		   (":" . "$A#:(B")  (";" . "$A#;(B")  ("\"" . "$A#"(B") ("'" . "$A#'(B")
		   ("<" . "$A#<(B")  (">" . "$A#>(B")  ("?" . "$A#?(B")  ("/" . "$A#/(B")
		   ("," . "$A#,(B")  ("." . "$A#.(B")
		   ("a" . "$A#a(B")  ("b" . "$A#b(B")  ("c" . "$A#c(B")  ("d" . "$A#d(B")
		   ("e" . "$A#e(B")  ("f" . "$A#f(B")  ("g" . "$A#g(B")  ("h" . "$A#h(B")
		   ("i" . "$A#i(B")  ("j" . "$A#j(B")  ("k" . "$A#k(B")  ("l" . "$A#l(B")
		   ("m" . "$A#m(B")  ("n" . "$A#n(B")  ("o" . "$A#o(B")  ("p" . "$A#p(B")
		   ("q" . "$A#q(B")  ("r" . "$A#r(B")  ("s" . "$A#s(B")  ("t" . "$A#t(B")
		   ("u" . "$A#u(B")  ("v" . "$A#v(B")  ("w" . "$A#w(B")  ("x" . "$A#x(B")
		   ("y" . "$A#y(B")  ("z" . "$A#z(B")
		   ("A" . "$A#A(B")  ("B" . "$A#B(B")  ("C" . "$A#C(B")  ("D" . "$A#D(B")
		   ("E" . "$A#E(B")  ("F" . "$A#F(B")  ("G" . "$A#G(B")  ("H" . "$A#H(B")
		   ("I" . "$A#I(B")  ("J" . "$A#J(B")  ("K" . "$A#K(B")  ("L" . "$A#L(B")
		   ("M" . "$A#M(B")  ("N" . "$A#N(B")  ("O" . "$A#O(B")  ("P" . "$A#P(B")
		   ("Q" . "$A#Q(B")  ("R" . "$A#R(B")  ("S" . "$A#S(B")  ("T" . "$A#T(B")
		   ("U" . "$A#U(B")  ("V" . "$A#V(B")  ("W" . "$A#W(B")  ("X" . "$A#X(B")
		   ("Y" . "$A#Y(B")  ("Z" . "$A#Z(B")))
    (let ((in (car ascii)) (out (cdr ascii)))
      (its-defrule (concat its-banjiao-escape in) in t)
      (its-defrule (concat its-quanjiao-escape in) out t)))

  (its-defrule	","	"$A#,(B"	t)
  (its-defrule	"."	"$A!#(B"	t)
  (its-defrule	"/"	"$A!"(B"	t)
  (its-defrule	":"	"$A#:(B"	t)
  (its-defrule	"?"	"$A#?(B"	t)
  (its-defrule	"!"	"$A#!(B"	t))

(define-its-state-machine its-wubi-map
  "wubi" "$ANe(B" WuBi
  "Map for WuBi input. (Chinese-GB)"

  (defconst its-quanjiao-escape "Z")
  (defconst its-banjiao-escape  "X")

  (its-defrule-select-mode-temporally "B" downcase)
  (its-defrule-select-mode-temporally "Q" quanjiao-downcase-cn)

  (let ((ch (string-to-list "abcdefghijklmnopqrstuvwxy")))
    (while ch
      (its-defrule (char-to-string (car ch)) (char-to-string (car ch)))
      (setq ch (cdr ch))))

  (dolist (ascii '(("0" . "$A#0(B")  ("1" . "$A#1(B")  ("2" . "$A#2(B")  ("3" . "$A#3(B")
		   ("4" . "$A#4(B")  ("5" . "$A#5(B")  ("6" . "$A#6(B")  ("7" . "$A#7(B")
		   ("8" . "$A#8(B")  ("9" . "$A#9(B") 
		   (" " . "$A!!(B")  ("!" . "$A#!(B")  ("@" . "$A#@(B")  ("#" . "$A##(B")
		   ("$" . "$A!g(B")  ("%" . "$A#%(B")  ("^" . "$A#^(B")  ("&" . "$A#&(B")
		   ("*" . "$A#*(B")  ("(" . "$A#((B")  (")" . "$A#)(B")
		   ("-" . "$A#-(B")  ("=" . "$A#=(B")  ("`" . "$A#`(B")  ("\\" . "$A#\(B")
		   ("|" . "$A#|(B")  ("_" . "$A#_(B")  ("+" . "$A#+(B")  ("~" . "$A!+(B")
		   ("[" . "$A#[(B")  ("]" . "$A#](B")  ("{" . "$A#{(B")  ("}" . "$A#}(B")
		   (":" . "$A#:(B")  (";" . "$A#;(B")  ("\"" . "$A#"(B") ("'" . "$A#'(B")
		   ("<" . "$A#<(B")  (">" . "$A#>(B")  ("?" . "$A#?(B")  ("/" . "$A#/(B")
		   ("," . "$A#,(B")  ("." . "$A#.(B")
		   ("a" . "$A#a(B")  ("b" . "$A#b(B")  ("c" . "$A#c(B")  ("d" . "$A#d(B")
		   ("e" . "$A#e(B")  ("f" . "$A#f(B")  ("g" . "$A#g(B")  ("h" . "$A#h(B")
		   ("i" . "$A#i(B")  ("j" . "$A#j(B")  ("k" . "$A#k(B")  ("l" . "$A#l(B")
		   ("m" . "$A#m(B")  ("n" . "$A#n(B")  ("o" . "$A#o(B")  ("p" . "$A#p(B")
		   ("q" . "$A#q(B")  ("r" . "$A#r(B")  ("s" . "$A#s(B")  ("t" . "$A#t(B")
		   ("u" . "$A#u(B")  ("v" . "$A#v(B")  ("w" . "$A#w(B")  ("x" . "$A#x(B")
		   ("y" . "$A#y(B")  ("z" . "$A#z(B")
		   ("A" . "$A#A(B")  ("B" . "$A#B(B")  ("C" . "$A#C(B")  ("D" . "$A#D(B")
		   ("E" . "$A#E(B")  ("F" . "$A#F(B")  ("G" . "$A#G(B")  ("H" . "$A#H(B")
		   ("I" . "$A#I(B")  ("J" . "$A#J(B")  ("K" . "$A#K(B")  ("L" . "$A#L(B")
		   ("M" . "$A#M(B")  ("N" . "$A#N(B")  ("O" . "$A#O(B")  ("P" . "$A#P(B")
		   ("Q" . "$A#Q(B")  ("R" . "$A#R(B")  ("S" . "$A#S(B")  ("T" . "$A#T(B")
		   ("U" . "$A#U(B")  ("V" . "$A#V(B")  ("W" . "$A#W(B")  ("X" . "$A#X(B")
		   ("Y" . "$A#Y(B")  ("Z" . "$A#Z(B")))
    (let ((in (car ascii)) (out (cdr ascii)))
      (its-defrule (concat its-banjiao-escape in) in t)
      (its-defrule (concat its-quanjiao-escape in) out t)))

  (its-defrule	","	"$A#,(B"	t)
  (its-defrule	"."	"$A!#(B"	t)
  (its-defrule	"/"	"$A!"(B"	t)
  (its-defrule	";"	"$A#;(B"	t)
  (its-defrule	":"	"$A#:(B"	t)
  (its-defrule	"?"	"$A#?(B"	t)
  (its-defrule	"!"	"$A#!(B"	t))

(define-its-state-machine-append its-qianma-map
  (its-defrule "{" its-qianma-open-braket)
  (its-defrule "}" its-qianma-close-braket)

  (if its-qianma-enable-quanjioao-alphabet
      (progn
	(its-defrule "#"  "$A##(B"  t)  (its-defrule "$"  "$A!g(B"  t)
	(its-defrule "%"  "$A#%(B"  t)  (its-defrule "^"  "$A#^(B"  t)
	(its-defrule "&"  "$A#&(B"  t)  (its-defrule "*"  "$A#*(B"  t)
	(its-defrule "("  "$A#((B"  t)  (its-defrule ")"  "$A#)(B"  t)
	(its-defrule "-"  "$A#-(B"  t)  (its-defrule "~"  "$A!+(B"  t)
	(its-defrule "`"  "$A#`(B"  t)
	(its-defrule "\\" "$A#\(B"  t)  (its-defrule "|"  "$A#|(B"  t)
	(its-defrule "_"  "$A#_(B"  t)  (its-defrule "+"  "$A#+(B"  t)
	(its-defrule "\"" "$A#"(B"  t)  (its-defrule "'"  "$A#'(B"  t)
	(its-defrule "<"  "$A#<(B"  t)  (its-defrule ">"  "$A#>(B"  t))
    (progn
      (its-defrule "#"  "#"  t)  (its-defrule "$"  "$"  t)
      (its-defrule "%"  "%"  t)  (its-defrule "^"  "^"  t)
      (its-defrule "&"  "&"  t)  (its-defrule "*"  "*"  t)
      (its-defrule "("  "("  t)  (its-defrule ")"  ")"  t)
      (its-defrule "-"  "-"  t)  (its-defrule "~"  "~"  t)
      (its-defrule "`"  "`"  t)
      (its-defrule "\\" "\\" t)  (its-defrule "|"  "|"  t)
      (its-defrule "_"  "_"  t)  (its-defrule "+"  "+"  t)
      (its-defrule "\"" "\"" t)  (its-defrule "'"  "'"  t)
      (its-defrule "<"  "<"  t)  (its-defrule ">"  ">"  t))))

(define-its-state-machine-append its-wubi-map
  (its-defrule "[" its-wubi-open-braket)
  (its-defrule "]" its-wubi-close-braket)

  (if its-wubi-enable-quanjioao-alphabet
      (progn
	(its-defrule "1"  "$A#1(B"  t)  (its-defrule "2"  "$A#2(B"  t)
	(its-defrule "3"  "$A#3(B"  t)  (its-defrule "4"  "$A#4(B"  t)
	(its-defrule "5"  "$A#5(B"  t)  (its-defrule "6"  "$A#6(B"  t)
	(its-defrule "7"  "$A#7(B"  t)  (its-defrule "8"  "$A#8(B"  t)
	(its-defrule "9"  "$A#9(B"  t)  (its-defrule "0"  "$A#0(B"  t)
	(its-defrule "@"  "$A#@(B"  t)
	(its-defrule "#"  "$A##(B"  t)  (its-defrule "$"  "$A!g(B"  t)
	(its-defrule "%"  "$A#%(B"  t)  (its-defrule "^"  "$A#^(B"  t)
	(its-defrule "&"  "$A#&(B"  t)  (its-defrule "*"  "$A#*(B"  t)
	(its-defrule "("  "$A#((B"  t)  (its-defrule ")"  "$A#)(B"  t)
	(its-defrule "-"  "$A#-(B"  t)  (its-defrule "~"  "$A!+(B"  t)
	(its-defrule "="  "$A#=(B"  t)  (its-defrule "`"  "$A#`(B"  t)
	(its-defrule "\\" "$A#\(B"  t)  (its-defrule "|"  "$A#|(B"  t)
	(its-defrule "_"  "$A#_(B"  t)  (its-defrule "+"  "$A#+(B"  t)
	(its-defrule "{"  "$A#{(B"  t)  (its-defrule "}"  "$A#}(B"  t)
	(its-defrule "\"" "$A#"(B"  t)  (its-defrule "'"  "$A#'(B"  t)
	(its-defrule "<"  "$A#<(B"  t)  (its-defrule ">"  "$A#>(B"  t))
    (progn
      (its-defrule "1"  "1"  t)  (its-defrule "2"  "2"  t)
      (its-defrule "3"  "3"  t)  (its-defrule "4"  "4"  t)
      (its-defrule "5"  "5"  t)  (its-defrule "6"  "6"  t)
      (its-defrule "7"  "7"  t)  (its-defrule "8"  "8"  t)
      (its-defrule "9"  "9"  t)  (its-defrule "0"  "0"  t)
      (its-defrule "@"  "@"  t)
      (its-defrule "#"  "#"  t)  (its-defrule "$"  "$"  t)
      (its-defrule "%"  "%"  t)  (its-defrule "^"  "^"  t)
      (its-defrule "&"  "&"  t)  (its-defrule "*"  "*"  t)
      (its-defrule "("  "("  t)  (its-defrule ")"  ")"  t)
      (its-defrule "-"  "-"  t)  (its-defrule "~"  "~"  t)
      (its-defrule "="  "="  t)  (its-defrule "`"  "`"  t)
      (its-defrule "\\" "\\" t)  (its-defrule "|"  "|"  t)
      (its-defrule "_"  "_"  t)  (its-defrule "+"  "+"  t)
      (its-defrule "{"  "{"  t)  (its-defrule "}"  "}"  t)
      (its-defrule "\"" "\"" t)  (its-defrule "'"  "'"  t)
      (its-defrule "<"  "<"  t)  (its-defrule ">"  ">"  t))))

(provide 'its/bixing)
