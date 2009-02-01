;;; its/zenkau.el --- Zenkaku ASCII Input in Egg Input Method Architecture

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

(define-its-state-machine its-zenkaku-up-map
  "zenkaku-upcase" "$B#A(B" Japanese
  "Map for zenkaku-upcase input."

  (dolist (ascii '(("0" . "$B#0(B")  ("1" . "$B#1(B")  ("2" . "$B#2(B")  ("3" . "$B#3(B")
		   ("4" . "$B#4(B")  ("5" . "$B#5(B")  ("6" . "$B#6(B")  ("7" . "$B#7(B")
		   ("8" . "$B#8(B")  ("9" . "$B#9(B") 
		   (" " . "$B!!(B")  ("!" . "$B!*(B")  ("@" . "$B!w(B")  ("#" . "$B!t(B")
		   ("$" . "$B!p(B")  ("%" . "$B!s(B")  ("^" . "$B!0(B")  ("&" . "$B!u(B")
		   ("*" . "$B!v(B")  ("(" . "$B!J(B")  (")" . "$B!K(B")
		   ("-" . "$B!](B")  ("=" . "$B!a(B")  ("`" . "$B!.(B")  ("\\" . "$B!@(B")
		   ("|" . "$B!C(B")  ("_" . "$B!2(B")  ("+" . "$B!\(B")  ("~" . "$B!A(B")
		   ("[" . "$B!N(B")  ("]" . "$B!O(B")  ("{" . "$B!P(B")  ("}" . "$B!Q(B")
		   (":" . "$B!'(B")  (";" . "$B!((B")  ("\"" . "$B!I(B") ("'" . "$B!-(B")
		   ("<" . "$B!c(B")  (">" . "$B!d(B")  ("?" . "$B!)(B")  ("/" . "$B!?(B")
		   ("," . "$B!$(B")  ("." . "$B!%(B")
		   ("a" . "$B#A(B")  ("b" . "$B#B(B")  ("c" . "$B#C(B")  ("d" . "$B#D(B")
		   ("e" . "$B#E(B")  ("f" . "$B#F(B")  ("g" . "$B#G(B")  ("h" . "$B#H(B")
		   ("i" . "$B#I(B")  ("j" . "$B#J(B")  ("k" . "$B#K(B")  ("l" . "$B#L(B")
		   ("m" . "$B#M(B")  ("n" . "$B#N(B")  ("o" . "$B#O(B")  ("p" . "$B#P(B")
		   ("q" . "$B#Q(B")  ("r" . "$B#R(B")  ("s" . "$B#S(B")  ("t" . "$B#T(B")
		   ("u" . "$B#U(B")  ("v" . "$B#V(B")  ("w" . "$B#W(B")  ("x" . "$B#X(B")
		   ("y" . "$B#Y(B")  ("z" . "$B#Z(B")
		   ("A" . "$B#A(B")  ("B" . "$B#B(B")  ("C" . "$B#C(B")  ("D" . "$B#D(B")
		   ("E" . "$B#E(B")  ("F" . "$B#F(B")  ("G" . "$B#G(B")  ("H" . "$B#H(B")
		   ("I" . "$B#I(B")  ("J" . "$B#J(B")  ("K" . "$B#K(B")  ("L" . "$B#L(B")
		   ("M" . "$B#M(B")  ("N" . "$B#N(B")  ("O" . "$B#O(B")  ("P" . "$B#P(B")
		   ("Q" . "$B#Q(B")  ("R" . "$B#R(B")  ("S" . "$B#S(B")  ("T" . "$B#T(B")
		   ("U" . "$B#U(B")  ("V" . "$B#V(B")  ("W" . "$B#W(B")  ("X" . "$B#X(B")
		   ("Y" . "$B#Y(B")  ("Z" . "$B#Z(B")))
    (let ((in (car ascii)) (out (cdr ascii)))
      (its-defrule in out))))

(define-its-state-machine-append its-zenkaku-up-map)

(define-its-state-machine its-zenkaku-down-map
  "zenkaku-downcase" "$B#a(B" Japanese
  "Map for zenkaku-downcase input."

  (dolist (ascii '(("0" . "$B#0(B")  ("1" . "$B#1(B")  ("2" . "$B#2(B")  ("3" . "$B#3(B")
		   ("4" . "$B#4(B")  ("5" . "$B#5(B")  ("6" . "$B#6(B")  ("7" . "$B#7(B")
		   ("8" . "$B#8(B")  ("9" . "$B#9(B") 
		   (" " . "$B!!(B")  ("!" . "$B!*(B")  ("@" . "$B!w(B")  ("#" . "$B!t(B")
		   ("$" . "$B!p(B")  ("%" . "$B!s(B")  ("^" . "$B!0(B")  ("&" . "$B!u(B")
		   ("*" . "$B!v(B")  ("(" . "$B!J(B")  (")" . "$B!K(B")
		   ("-" . "$B!](B")  ("=" . "$B!a(B")  ("`" . "$B!.(B")  ("\\" . "$B!@(B")
		   ("|" . "$B!C(B")  ("_" . "$B!2(B")  ("+" . "$B!\(B")  ("~" . "$B!A(B")
		   ("[" . "$B!N(B")  ("]" . "$B!O(B")  ("{" . "$B!P(B")  ("}" . "$B!Q(B")
		   (":" . "$B!'(B")  (";" . "$B!((B")  ("\"" . "$B!I(B") ("'" . "$B!-(B")
		   ("<" . "$B!c(B")  (">" . "$B!d(B")  ("?" . "$B!)(B")  ("/" . "$B!?(B")
		   ("," . "$B!$(B")  ("." . "$B!%(B")
		   ("a" . "$B#a(B")  ("b" . "$B#b(B")  ("c" . "$B#c(B")  ("d" . "$B#d(B")
		   ("e" . "$B#e(B")  ("f" . "$B#f(B")  ("g" . "$B#g(B")  ("h" . "$B#h(B")
		   ("i" . "$B#i(B")  ("j" . "$B#j(B")  ("k" . "$B#k(B")  ("l" . "$B#l(B")
		   ("m" . "$B#m(B")  ("n" . "$B#n(B")  ("o" . "$B#o(B")  ("p" . "$B#p(B")
		   ("q" . "$B#q(B")  ("r" . "$B#r(B")  ("s" . "$B#s(B")  ("t" . "$B#t(B")
		   ("u" . "$B#u(B")  ("v" . "$B#v(B")  ("w" . "$B#w(B")  ("x" . "$B#x(B")
		   ("y" . "$B#y(B")  ("z" . "$B#z(B")
		   ("A" . "$B#A(B")  ("B" . "$B#B(B")  ("C" . "$B#C(B")  ("D" . "$B#D(B")
		   ("E" . "$B#E(B")  ("F" . "$B#F(B")  ("G" . "$B#G(B")  ("H" . "$B#H(B")
		   ("I" . "$B#I(B")  ("J" . "$B#J(B")  ("K" . "$B#K(B")  ("L" . "$B#L(B")
		   ("M" . "$B#M(B")  ("N" . "$B#N(B")  ("O" . "$B#O(B")  ("P" . "$B#P(B")
		   ("Q" . "$B#Q(B")  ("R" . "$B#R(B")  ("S" . "$B#S(B")  ("T" . "$B#T(B")
		   ("U" . "$B#U(B")  ("V" . "$B#V(B")  ("W" . "$B#W(B")  ("X" . "$B#X(B")
		   ("Y" . "$B#Y(B")  ("Z" . "$B#Z(B")))
    (let ((in (car ascii)) (out (cdr ascii)))
      (its-defrule in out))))

(define-its-state-machine-append its-zenkaku-down-map)

(provide 'its/zenkaku)
