;;; its/jeonkak.el --- Jeonkak ASCII Input in Egg Input Method Architecture

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
  (require 'its))

(define-its-state-machine its-jeonkak-up-map
  "jeonkak-upcase" "$(C#A(B" Korean
  "Map for jeonkak-upcase input."

  (dolist (ascii '(("1" . "$(C#1(B")  ("2" . "$(C#2(B")  ("3" . "$(C#3(B")  ("4" . "$(C#4(B")
		   ("5" . "$(C#5(B")  ("6" . "$(C#6(B")  ("7" . "$(C#7(B")  ("8" . "$(C#8(B")
		   ("9" . "$(C#9(B")  ("0" . "$(C#0(B")
		   (" " . "$(C!!(B")  ("!" . "$(C#!(B")  ("@" . "$(C#@(B")  ("#" . "$(C##(B")
		   ("$" . "$(C#$(B")  ("%" . "$(C#%(B")  ("^" . "$(C#^(B")  ("&" . "$(C#&(B")
		   ("*" . "$(C#*(B")  ("(" . "$(C#((B")  (")" . "$(C#)(B")
		   ("-" . "$(C#-(B")  ("=" . "$(C#=(B")  ("`" . "$(C#`(B")  ("\\" . "$(C#\(B")
		   ("|" . "$(C#|(B")  ("_" . "$(C#_(B")  ("+" . "$(C#+(B")  ("~" . "$(C#~(B")
		   ("[" . "$(C!8(B")  ("]" . "$(C!9(B")  ("{" . "$(C#{(B")  ("}" . "$(C#}(B")
		   (":" . "$(C#:(B")  (";" . "$(C#;(B")  ("\"" . "$(C#"(B") ("'" . "$(C#'(B")
		   ("<" . "$(C#<(B")  (">" . "$(C#>(B")  ("?" . "$(C#?(B")  ("/" . "$(C#/(B")
		   ("," . "$(C#,(B")  ("." . "$(C#.(B")
		   ("a" . "$(C#A(B")  ("b" . "$(C#B(B")  ("c" . "$(C#C(B")  ("d" . "$(C#D(B")
		   ("e" . "$(C#E(B")  ("f" . "$(C#F(B")  ("g" . "$(C#G(B")  ("h" . "$(C#H(B")
		   ("i" . "$(C#I(B")  ("j" . "$(C#J(B")  ("k" . "$(C#K(B")  ("l" . "$(C#L(B")
		   ("m" . "$(C#M(B")  ("n" . "$(C#N(B")  ("o" . "$(C#O(B")  ("p" . "$(C#P(B")
		   ("q" . "$(C#Q(B")  ("r" . "$(C#R(B")  ("s" . "$(C#S(B")  ("t" . "$(C#T(B")
		   ("u" . "$(C#U(B")  ("v" . "$(C#V(B")  ("w" . "$(C#W(B")  ("x" . "$(C#X(B")
		   ("y" . "$(C#Y(B")  ("z" . "$(C#Z(B")
		   ("A" . "$(C#A(B")  ("B" . "$(C#B(B")  ("C" . "$(C#C(B")  ("D" . "$(C#D(B")
		   ("E" . "$(C#E(B")  ("F" . "$(C#F(B")  ("G" . "$(C#G(B")  ("H" . "$(C#H(B")
		   ("I" . "$(C#I(B")  ("J" . "$(C#J(B")  ("K" . "$(C#K(B")  ("L" . "$(C#L(B")
		   ("M" . "$(C#M(B")  ("N" . "$(C#N(B")  ("O" . "$(C#O(B")  ("P" . "$(C#P(B")
		   ("Q" . "$(C#Q(B")  ("R" . "$(C#R(B")  ("S" . "$(C#S(B")  ("T" . "$(C#T(B")
		   ("U" . "$(C#U(B")  ("V" . "$(C#V(B")  ("W" . "$(C#W(B")  ("X" . "$(C#X(B")
		   ("Y" . "$(C#Y(B")  ("Z" . "$(C#Z(B")))
    (let ((in (car ascii)) (out (cdr ascii)))
      (its-defrule in out))))

(define-its-state-machine-append its-jeonkak-up-map)

(define-its-state-machine its-jeonkak-down-map
  "jeonkak-downcase" "$(C#a(B" Korean
  "Map for jeonkak-downcase input."

  (dolist (ascii '(("1" . "$(C#1(B")  ("2" . "$(C#2(B")  ("3" . "$(C#3(B")  ("4" . "$(C#4(B")
		   ("5" . "$(C#5(B")  ("6" . "$(C#6(B")  ("7" . "$(C#7(B")  ("8" . "$(C#8(B")
		   ("9" . "$(C#9(B")  ("0" . "$(C#0(B")
		   (" " . "$(C!!(B")  ("!" . "$(C#!(B")  ("@" . "$(C#@(B")  ("#" . "$(C##(B")
		   ("$" . "$(C#$(B")  ("%" . "$(C#%(B")  ("^" . "$(C#^(B")  ("&" . "$(C#&(B")
		   ("*" . "$(C#*(B")  ("(" . "$(C#((B")  (")" . "$(C#)(B")
		   ("-" . "$(C#-(B")  ("=" . "$(C#=(B")  ("`" . "$(C#`(B")  ("\\" . "$(C#\(B")
		   ("|" . "$(C#|(B")  ("_" . "$(C#_(B")  ("+" . "$(C#+(B")  ("~" . "$(C#~(B")
		   ("[" . "$(C!8(B")  ("]" . "$(C!9(B")  ("{" . "$(C#{(B")  ("}" . "$(C#}(B")
		   (":" . "$(C#:(B")  (";" . "$(C#;(B")  ("\"" . "$(C#"(B") ("'" . "$(C#'(B")
		   ("<" . "$(C#<(B")  (">" . "$(C#>(B")  ("?" . "$(C#?(B")  ("/" . "$(C#/(B")
		   ("," . "$(C#,(B")  ("." . "$(C#.(B")
		   ("a" . "$(C#a(B")  ("b" . "$(C#b(B")  ("c" . "$(C#c(B")  ("d" . "$(C#d(B")
		   ("e" . "$(C#e(B")  ("f" . "$(C#f(B")  ("g" . "$(C#g(B")  ("h" . "$(C#h(B")
		   ("i" . "$(C#i(B")  ("j" . "$(C#j(B")  ("k" . "$(C#k(B")  ("l" . "$(C#l(B")
		   ("m" . "$(C#m(B")  ("n" . "$(C#n(B")  ("o" . "$(C#o(B")  ("p" . "$(C#p(B")
		   ("q" . "$(C#q(B")  ("r" . "$(C#r(B")  ("s" . "$(C#s(B")  ("t" . "$(C#t(B")
		   ("u" . "$(C#u(B")  ("v" . "$(C#v(B")  ("w" . "$(C#w(B")  ("x" . "$(C#x(B")
		   ("y" . "$(C#y(B")  ("z" . "$(C#z(B")
		   ("A" . "$(C#A(B")  ("B" . "$(C#B(B")  ("C" . "$(C#C(B")  ("D" . "$(C#D(B")
		   ("E" . "$(C#E(B")  ("F" . "$(C#F(B")  ("G" . "$(C#G(B")  ("H" . "$(C#H(B")
		   ("I" . "$(C#I(B")  ("J" . "$(C#J(B")  ("K" . "$(C#K(B")  ("L" . "$(C#L(B")
		   ("M" . "$(C#M(B")  ("N" . "$(C#N(B")  ("O" . "$(C#O(B")  ("P" . "$(C#P(B")
		   ("Q" . "$(C#Q(B")  ("R" . "$(C#R(B")  ("S" . "$(C#S(B")  ("T" . "$(C#T(B")
		   ("U" . "$(C#U(B")  ("V" . "$(C#V(B")  ("W" . "$(C#W(B")  ("X" . "$(C#X(B")
		   ("Y" . "$(C#Y(B")  ("Z" . "$(C#Z(B")))
    (let ((in (car ascii)) (out (cdr ascii)))
      (its-defrule in out))))

(define-its-state-machine-append its-jeonkak-down-map)

(provide 'its/jeonkak)
