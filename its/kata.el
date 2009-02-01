;;; its/kata.el --- Katakana Input in Egg Input Method Architecture


;; Copyright (C) 1995 Electrotechnical Laboratory, JAPAN.
;; Licensed to the Free Software Foundation.

;; Author: Satoru Tomura <tomura@etl.go.jp>

;; Maintainer: TOMURA Satoru <tomura@etl.go.jp>

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
;;
;; Symbol input is desined by jiro@math.keio.ac.jp (TANAKA Jiro)
;; This file is based on the rules of its/hira.el in Mule-2.3 distribution.
;;

;;; Code:

(eval-when-compile
  (require 'its)
  (require 'cl))

(eval-when (compile)
  (defconst its-compaction-enable t))

(defvar its-kata-enable-zenkaku-alphabet
  (if (boundp 'its-enable-fullwidth-alphabet)
      its-enable-fullwidth-alphabet
    t)
  "*Enable Zenkaku alphabet")

(defvar its-kata-enable-double-n nil "*Enable \"nn\" input for \"$B%s(B\" ")
(defvar its-kata-period "$B!#(B" "*$B%T%j%*%I(B")  ; ". " "$B!%(B"
(defvar its-kata-comma  "$B!"(B" "*$B%3%s%^(B")    ; ", " "$B!$(B"
(defvar its-kata-open-bracket  "$B!V(B" "*[")  ; "$B!N(B"
(defvar its-kata-close-bracket  "$B!W(B" "*]") ; "$B!O(B"
(defvar its-kata-horizontal  "$B!<(B" "*-")    ; "$B!](B"

(define-its-state-machine its-kata-map
  "roma-kata" "$B%"(B" Japanese
  "Map for Romaji-Katakana translation. (Japanese)"

  (defconst its-zenkaku-escape "Z")  ;; Escape character to Zenkaku inputs
  (defconst its-hankaku-escape "~")  ;; Escape character to Hankaku inputs

  (its-defrule-select-mode-temporally "q" downcase)
  (its-defrule-select-mode-temporally "Q" zenkaku-downcase)

;;; k      k
;;; kk     $B%C(Bk
;;; kka    $B%C%+(B
;;;
;;; kkk    $B%C(Bk DING!

  (its-defrule "tch"  "$B%C(B" -2)

;;; $B!V%s!W$NF~NO(B

  (dolist (q1 '("b" "m" "p"))
    (its-defrule (concat "m" q1) "$B%s(B" -1))

  (its-defrule* "n"  "$B%s(B")
  (its-defrule  "n'" "$B%s(B")
  (its-defrule  "N"  "$B%s(B")

  (let ((small '"x" ))
    (its-defrule (concat small "a") "$B%!(B")
    (its-defrule (concat small "i") "$B%#(B")
    (its-defrule (concat small "u") "$B%%(B")
    (its-defrule (concat small "e") "$B%'(B")
    (its-defrule (concat small "o") "$B%)(B")
    (its-defrule (concat small "ya") "$B%c(B")
    (its-defrule (concat small "yu") "$B%e(B")
    (its-defrule (concat small "yo") "$B%g(B")
    (its-defrule (concat small "tu") "$B%C(B")
    (its-defrule (concat small "tsu") "$B%C(B")
    (its-defrule (concat small "wa") "$B%n(B")
    )

  (its-defrule   "a"    "$B%"(B")
  (its-defrule   "i"    "$B%$(B")
  (its-defrule   "u"    "$B%&(B")
  (its-defrule   "e"    "$B%((B")
  (its-defrule   "o"    "$B%*(B")

  (dolist (k '(("ka"  "$B%+(B") ("ki"  "$B%-(B") ("ku"  "$B%/(B") ("ke"  "$B%1(B") ("ko"  "$B%3(B")
	       ("kya" "$B%-%c(B") ("kyu"  "$B%-%e(B") ("kye"  "$B%-%'(B") ("kyo"  "$B%-%g(B")))
    (its-defrule (car k) (cadr k))
    (its-defrule (concat "k" (car k)) (concat "$B%C(B" (cadr k))))
  (its-defoutput "kk" "$B%C(Bk")
  (its-defoutput "kky" "$B%C(Bky")

  (dolist (s '(("sa"  "$B%5(B") ("si"  "$B%7(B") ("su"  "$B%9(B") ("se"  "$B%;(B") ("so"  "$B%=(B")
	       ("sya"  "$B%7%c(B") ("syu"  "$B%7%e(B") ("sye"  "$B%7%'(B") ("syo"  "$B%7%g(B")
	       ("sha"  "$B%7%c(B") ("shi"  "$B%7(B") ("shu"  "$B%7%e(B") ("she"  "$B%7%'(B")
	       ("sho"  "$B%7%g(B")))
    (its-defrule (car s) (cadr s))
    (its-defrule (concat "s" (car s)) (concat "$B%C(B" (cadr s))))
  (its-defoutput "ss" "$B%C(Bs")
  (its-defoutput "ssy" "$B%C(Bsy")
  (its-defoutput "ssh" "$B%C(Bsh")

  (dolist (T '(("ta"  "$B%?(B") ("ti"  "$B%A(B") ("tu"  "$B%D(B") ("te"  "$B%F(B") ("to"  "$B%H(B")
	       ("tya"  "$B%A%c(B") ("tyi"  "$B%F%#(B") ("tyu"  "$B%A%e(B") ("tye"  "$B%A%'(B")
	       ("tyo"  "$B%A%g(B") ("tsu"  "$B%D(B")))
    (its-defrule (car T) (cadr T))
    (its-defrule (concat "t" (car T)) (concat "$B%C(B" (cadr T))))
  (its-defoutput "tt" "$B%C(Bt")
  (its-defoutput "tty" "$B%C(Bty")
  (its-defoutput "tts" "$B%C(Bts")

  (dolist (c '(("cha"  "$B%A%c(B") ("chi"  "$B%A(B") ("chu"  "$B%A%e(B")
	       ("che"  "$B%A%'(B") ("cho"  "$B%A%g(B")))
    (its-defrule (car c) (cadr c))
    (its-defrule (concat "c" (car c)) (concat "$B%C(B" (cadr c))))
  (its-defoutput "cc" "$B%C(Bc")
  (its-defoutput "cch" "$B%C(Bch")

  (dolist (h '(("ha"  "$B%O(B") ("hi"  "$B%R(B") ("hu"  "$B%U(B") ("he"  "$B%X(B") ("ho"  "$B%[(B")
	       ("hya"  "$B%R%c(B") ("hyu"  "$B%R%e(B") ("hye"  "$B%R%'(B") ("hyo"  "$B%R%g(B")))
    (its-defrule (car h) (cadr h))
    (its-defrule (concat "h" (car h)) (concat "$B%C(B" (cadr h))))
  (its-defoutput "hh" "$B%C(Bh")
  (its-defoutput "hhy" "$B%C(Bhy")

  (dolist (f '(("fa"  "$B%U%!(B") ("fi"  "$B%U%#(B") ("fu"  "$B%U(B") ("fe"  "$B%U%'(B")
	       ("fo"  "$B%U%)(B")))
    (its-defrule (car f) (cadr f))
    (its-defrule (concat "f" (car f)) (concat "$B%C(B" (cadr f))))
  (its-defoutput "ff" "$B%C(Bf")

  (dolist (r '(("ra"  "$B%i(B") ("ri"  "$B%j(B") ("ru"  "$B%k(B") ("re"  "$B%l(B") ("ro"  "$B%m(B")
	       ("rya"  "$B%j%c(B") ("ryu"  "$B%j%e(B") ("rye"  "$B%j%'(B") ("ryo"  "$B%j%g(B")))
    (its-defrule (car r) (cadr r))
    (its-defrule (concat "r" (car r)) (concat "$B%C(B" (cadr r))))
  (its-defoutput "rr" "$B%C(Br")
  (its-defoutput "rry" "$B%C(Bry")

  (dolist (l '(("la"  "$B%i(B") ("li"  "$B%j(B") ("lu"  "$B%k(B") ("le"  "$B%l(B") ("lo"  "$B%m(B")
	       ("lya"  "$B%j%c(B") ("lyu"  "$B%j%e(B") ("lye"  "$B%j%'(B") ("lyo"  "$B%j%g(B")))
    (its-defrule (car l) (cadr l))
    (its-defrule (concat "l" (car l)) (concat "$B%C(B" (cadr l))))
  (its-defoutput "ll" "$B%C(Bl")
  (its-defoutput "lly" "$B%C(Bly")

  (dolist (g '(("ga"  "$B%,(B") ("gi"  "$B%.(B") ("gu"  "$B%0(B") ("ge"  "$B%2(B") ("go"  "$B%4(B")
	       ("gya"  "$B%.%c(B") ("gyu"  "$B%.%e(B") ("gye"  "$B%.%'(B") ("gyo"  "$B%.%g(B")))
    (its-defrule (car g) (cadr g))
    (its-defrule (concat "g" (car g)) (concat "$B%C(B" (cadr g))))
  (its-defoutput "gg" "$B%C(Bg")
  (its-defoutput "ggy" "$B%C(Bgy")

  (dolist (z '(("za"  "$B%6(B") ("zi"  "$B%8(B") ("zu"  "$B%:(B") ("ze"  "$B%<(B") ("zo"  "$B%>(B")
	       ("zya"  "$B%8%c(B") ("zyu"  "$B%8%e(B") ("zye"  "$B%8%'(B") ("zyo"  "$B%8%g(B")))
    (its-defrule (car z) (cadr z))
    (its-defrule (concat "z" (car z)) (concat "$B%C(B" (cadr z))))
  (its-defoutput "zz" "$B%C(Bz")
  (its-defoutput "zzy" "$B%C(Bzy")

  (dolist (j '(("ja"  "$B%8%c(B") ("ji"  "$B%8(B") ("ju"  "$B%8%e(B") ("je"  "$B%8%'(B")
	       ("jo"  "$B%8%g(B") ("jya"  "$B%8%c(B") ("jyu"  "$B%8%e(B") ("jye"  "$B%8%'(B")
	       ("jyo"  "$B%8%g(B")))
    (its-defrule (car j) (cadr j))
    (its-defrule (concat "j" (car j)) (concat "$B%C(B" (cadr j))))
  (its-defoutput "jj" "$B%C(Bj")
  (its-defoutput "jjy" "$B%C(Bjy")

  (dolist (d '(("da"  "$B%@(B") ("di"  "$B%B(B") ("du"  "$B%E(B") ("de"  "$B%G(B") ("do"  "$B%I(B")
	       ("dya"  "$B%B%c(B") ("dyi"  "$B%G%#(B") ("dyu"  "$B%B%e(B") ("dye"  "$B%B%'(B")
	       ("dyo"  "$B%B%g(B")))
    (its-defrule (car d) (cadr d))
    (its-defrule (concat "d" (car d)) (concat "$B%C(B" (cadr d))))
  (its-defoutput "dd" "$B%C(Bd")
  (its-defoutput "ddy" "$B%C(Bdy")

  (dolist (b '(("ba"  "$B%P(B") ("bi"  "$B%S(B") ("bu"  "$B%V(B") ("be"  "$B%Y(B") ("bo"  "$B%\(B")
	       ("bya"  "$B%S%c(B") ("byu"  "$B%S%e(B") ("bye"  "$B%S%'(B") ("byo"  "$B%S%g(B")))
    (its-defrule (car b) (cadr b))
    (its-defrule (concat "b" (car b)) (concat "$B%C(B" (cadr b))))
  (its-defoutput "bb" "$B%C(Bb")
  (its-defoutput "bby" "$B%C(Bby")

  (dolist (p '(("pa"  "$B%Q(B") ("pi"  "$B%T(B") ("pu"  "$B%W(B") ("pe"  "$B%Z(B") ("po"   "$B%](B")
	       ("pya"  "$B%T%c(B") ("pyu"  "$B%T%e(B") ("pye"  "$B%T%'(B") ("pyo"  "$B%T%g(B")))
    (its-defrule (car p) (cadr p))
    (its-defrule (concat "p" (car p)) (concat "$B%C(B" (cadr p))))
  (its-defoutput "pp" "$B%C(Bp")
  (its-defoutput "ppy" "$B%C(Bpy")

  (dolist (v '(("va" "$B%t%!(B") ("vi" "$B%t%#(B") ("vu" "$B%t(B") ("ve" "$B%t%'(B")
	       ("vo" "$B%t%)(B")))
    (its-defrule (car v) (cadr v))
    (its-defrule (concat "v" (car v)) (concat "$B%C(B" (cadr v))))
  (its-defoutput "vv" "$B%C(Bv")

  (its-defrule   "ma"   "$B%^(B")
  (its-defrule   "mi"   "$B%_(B")
  (its-defrule   "mu"   "$B%`(B")
  (its-defrule   "me"   "$B%a(B")
  (its-defrule   "mo"   "$B%b(B")
  (its-defrule   "mya"  "$B%_%c(B")
  (its-defrule   "myu"  "$B%_%e(B")
  (its-defrule   "mye"  "$B%_%'(B")
  (its-defrule   "myo"  "$B%_%g(B")
  (its-defrule   "ya"   "$B%d(B")
  (its-defrule   "yi"   "$B%$(B")
  (its-defrule   "yu"   "$B%f(B")
  (its-defrule   "yo"   "$B%h(B")
  (its-defrule   "ye"   "$B%$%'(B")
  (its-defrule   "wa"   "$B%o(B")
  (its-defrule   "wi"   "$B%p(B")
  (its-defrule   "wu"   "$B%&(B")
  (its-defrule   "we"   "$B%q(B")
  (its-defrule   "wo"   "$B%r(B")

  (its-defrule   "kwa"  "$B%/%n(B")
  (its-defrule   "kwi"  "$B%/%#(B")
  (its-defrule   "kwu"  "$B%/(B")
  (its-defrule   "kwe"  "$B%/%'(B")
  (its-defrule   "kwo"  "$B%/%)(B")
  (its-defrule   "gwa"  "$B%0%n(B")
  (its-defrule   "gwi"  "$B%0%#(B")
  (its-defrule   "gwu"  "$B%0(B")
  (its-defrule   "gwe"  "$B%0%'(B")
  (its-defrule   "gwo"  "$B%0%)(B")
  (its-defrule   "tsa"  "$B%D%!(B")
  (its-defrule   "tsi"  "$B%D%#(B")
  (its-defrule   "tse"  "$B%D%'(B")
  (its-defrule   "tso"  "$B%D%)(B")

  (its-defrule   "na"   "$B%J(B")
  (its-defrule   "ni"   "$B%K(B")
  (its-defrule   "nu"   "$B%L(B")
  (its-defrule   "ne"   "$B%M(B")
  (its-defrule   "no"   "$B%N(B")
  (its-defrule   "nya"  "$B%K%c(B")
  (its-defrule   "nyu"  "$B%K%e(B")
  (its-defrule   "nye"  "$B%K%'(B")
  (its-defrule   "nyo"  "$B%K%g(B")

  (its-defrule   "xka"  "$B%u(B")
  (its-defrule   "xke"  "$B%v(B")
  (its-defrule   "xti"  "$B%F%#(B")
  (its-defrule   "xdi"  "$B%G%#(B")
  (its-defrule   "xdu"  "$B%I%%(B")
  (its-defrule   "xde"  "$B%G%'(B")
  (its-defrule   "xdo"  "$B%I%)(B")
  (its-defrule   "xwi"  "$B%&%#(B")
  (its-defrule   "xwe"  "$B%&%'(B")
  (its-defrule   "xwo"  "$B%&%)(B")

;;;
;;; Zenkaku inputs
;;;

  (its-defrule (concat its-zenkaku-escape "0") "$B#0(B")
  (its-defrule (concat its-zenkaku-escape "1") "$B#1(B")
  (its-defrule (concat its-zenkaku-escape "2") "$B#2(B")
  (its-defrule (concat its-zenkaku-escape "3") "$B#3(B")
  (its-defrule (concat its-zenkaku-escape "4") "$B#4(B")
  (its-defrule (concat its-zenkaku-escape "5") "$B#5(B")
  (its-defrule (concat its-zenkaku-escape "6") "$B#6(B")
  (its-defrule (concat its-zenkaku-escape "7") "$B#7(B")
  (its-defrule (concat its-zenkaku-escape "8") "$B#8(B")
  (its-defrule (concat its-zenkaku-escape "9") "$B#9(B")

  (its-defrule (concat its-zenkaku-escape "A") "$B#A(B")
  (its-defrule (concat its-zenkaku-escape "B") "$B#B(B")
  (its-defrule (concat its-zenkaku-escape "C") "$B#C(B")
  (its-defrule (concat its-zenkaku-escape "D") "$B#D(B")
  (its-defrule (concat its-zenkaku-escape "E") "$B#E(B")
  (its-defrule (concat its-zenkaku-escape "F") "$B#F(B")
  (its-defrule (concat its-zenkaku-escape "G") "$B#G(B")
  (its-defrule (concat its-zenkaku-escape "H") "$B#H(B")
  (its-defrule (concat its-zenkaku-escape "I") "$B#I(B")
  (its-defrule (concat its-zenkaku-escape "J") "$B#J(B")
  (its-defrule (concat its-zenkaku-escape "K") "$B#K(B")
  (its-defrule (concat its-zenkaku-escape "L") "$B#L(B")
  (its-defrule (concat its-zenkaku-escape "M") "$B#M(B")
  (its-defrule (concat its-zenkaku-escape "N") "$B#N(B")
  (its-defrule (concat its-zenkaku-escape "O") "$B#O(B")
  (its-defrule (concat its-zenkaku-escape "P") "$B#P(B")
  (its-defrule (concat its-zenkaku-escape "Q") "$B#Q(B")
  (its-defrule (concat its-zenkaku-escape "R") "$B#R(B")
  (its-defrule (concat its-zenkaku-escape "S") "$B#S(B")
  (its-defrule (concat its-zenkaku-escape "T") "$B#T(B")
  (its-defrule (concat its-zenkaku-escape "U") "$B#U(B")
  (its-defrule (concat its-zenkaku-escape "V") "$B#V(B")
  (its-defrule (concat its-zenkaku-escape "W") "$B#W(B")
  (its-defrule (concat its-zenkaku-escape "X") "$B#X(B")
  (its-defrule (concat its-zenkaku-escape "Y") "$B#Y(B")
  (its-defrule (concat its-zenkaku-escape "Z") "$B#Z(B")

  (its-defrule (concat its-zenkaku-escape "a") "$B#a(B")
  (its-defrule (concat its-zenkaku-escape "b") "$B#b(B")
  (its-defrule (concat its-zenkaku-escape "c") "$B#c(B")
  (its-defrule (concat its-zenkaku-escape "d") "$B#d(B")
  (its-defrule (concat its-zenkaku-escape "e") "$B#e(B")
  (its-defrule (concat its-zenkaku-escape "f") "$B#f(B")
  (its-defrule (concat its-zenkaku-escape "g") "$B#g(B")
  (its-defrule (concat its-zenkaku-escape "h") "$B#h(B")
  (its-defrule (concat its-zenkaku-escape "i") "$B#i(B")
  (its-defrule (concat its-zenkaku-escape "j") "$B#j(B")
  (its-defrule (concat its-zenkaku-escape "k") "$B#k(B")
  (its-defrule (concat its-zenkaku-escape "l") "$B#l(B")
  (its-defrule (concat its-zenkaku-escape "m") "$B#m(B")
  (its-defrule (concat its-zenkaku-escape "n") "$B#n(B")
  (its-defrule (concat its-zenkaku-escape "o") "$B#o(B")
  (its-defrule (concat its-zenkaku-escape "p") "$B#p(B")
  (its-defrule (concat its-zenkaku-escape "q") "$B#q(B")
  (its-defrule (concat its-zenkaku-escape "r") "$B#r(B")
  (its-defrule (concat its-zenkaku-escape "s") "$B#s(B")
  (its-defrule (concat its-zenkaku-escape "t") "$B#t(B")
  (its-defrule (concat its-zenkaku-escape "u") "$B#u(B")
  (its-defrule (concat its-zenkaku-escape "v") "$B#v(B")
  (its-defrule (concat its-zenkaku-escape "w") "$B#w(B")
  (its-defrule (concat its-zenkaku-escape "x") "$B#x(B")
  (its-defrule (concat its-zenkaku-escape "y") "$B#y(B")
  (its-defrule (concat its-zenkaku-escape "z") "$B#z(B")

  (its-defrule (concat its-zenkaku-escape " ")  "$B!!(B")
  (its-defrule (concat its-zenkaku-escape "!")  "$B!*(B")
  (its-defrule (concat its-zenkaku-escape "@")  "$B!w(B")
  (its-defrule (concat its-zenkaku-escape "#")  "$B!t(B")
  (its-defrule (concat its-zenkaku-escape "$")  "$B!p(B")
  (its-defrule (concat its-zenkaku-escape "%")  "$B!s(B")
  (its-defrule (concat its-zenkaku-escape "^")  "$B!0(B")
  (its-defrule (concat its-zenkaku-escape "&")  "$B!u(B")
  (its-defrule (concat its-zenkaku-escape "*")  "$B!v(B")
  (its-defrule (concat its-zenkaku-escape "(")  "$B!J(B")
  (its-defrule (concat its-zenkaku-escape ")")  "$B!K(B")
  (its-defrule (concat its-zenkaku-escape "-")  "$B!](B")
  (its-defrule (concat its-zenkaku-escape "=")  "$B!a(B")
  (its-defrule (concat its-zenkaku-escape "`")  "$B!.(B")
  (its-defrule (concat its-zenkaku-escape "\\") "$B!o(B")
  (its-defrule (concat its-zenkaku-escape "|")  "$B!C(B")
  (its-defrule (concat its-zenkaku-escape "_")  "$B!2(B")
  (its-defrule (concat its-zenkaku-escape "+")  "$B!\(B")
  (its-defrule (concat its-zenkaku-escape "~")  "$B!1(B")
  (its-defrule (concat its-zenkaku-escape "[")  "$B!N(B")
  (its-defrule (concat its-zenkaku-escape "]")  "$B!O(B")
  (its-defrule (concat its-zenkaku-escape "{")  "$B!P(B")
  (its-defrule (concat its-zenkaku-escape "}")  "$B!Q(B")
  (its-defrule (concat its-zenkaku-escape ":")  "$B!'(B")
  (its-defrule (concat its-zenkaku-escape ";")  "$B!((B")
  (its-defrule (concat its-zenkaku-escape "\"") "$B!I(B")
  (its-defrule (concat its-zenkaku-escape "'")  "$B!G(B")
  (its-defrule (concat its-zenkaku-escape "<")  "$B!c(B")
  (its-defrule (concat its-zenkaku-escape ">")  "$B!d(B")
  (its-defrule (concat its-zenkaku-escape "?")  "$B!)(B")
  (its-defrule (concat its-zenkaku-escape "/")  "$B!?(B")
  (its-defrule (concat its-zenkaku-escape ",")  "$B!$(B")
  (its-defrule (concat its-zenkaku-escape ".")  "$B!%(B")

;;;
;;; Hankaku inputs
;;;

  (dolist (digit '( "1"  "2"  "3"  "4" "5"  "6"  "7"  "8"  "9"  "0" ))
    (its-defrule (concat its-hankaku-escape digit)  digit))

  (dolist (symbol '( " "  "!"  "@"  "#"  "$"  "%"  "^"  "&"  "*"  "("  ")"
		     "-"  "="  "`"  "\\" "|"  "_"  "+"  "~" "["  "]"  "{"  "}"
		     ":"  ";"  "\"" "'"  "<"  ">"  "?"  "/"  ","  "." ))
    (its-defrule (concat its-hankaku-escape symbol) symbol))

  (dolist (downcase '("a" "b" "c" "d" "e" "f" "g" "h" "i" "j" "k" "l" "m" "n"
		      "o" "p" "q" "r" "s" "t" "u" "v" "w" "x" "y" "z"))
    (its-defrule (concat its-hankaku-escape downcase) downcase))

  (dolist (upcase    '("A" "B" "C" "D" "E" "F" "G" "H" "I" "J" "K" "L" "M" "N"
		       "O" "P" "Q" "R" "S" "T" "U" "V" "W" "X" "Y" "Z"))
    (its-defrule (concat its-hankaku-escape upcase) upcase))

;; SYMBOL Input
  (its-defrule   "z1"   "$B!{(B")	(its-defrule   "z!"   "$B!|(B")
  (its-defrule   "z2"   "$B"&(B")	(its-defrule   "z@"   "$B"'(B")
  (its-defrule   "z3"   "$B"$(B")	(its-defrule   "z#"   "$B"%(B")
  (its-defrule   "z4"   "$B""(B")	(its-defrule   "z$"   "$B"#(B")
  (its-defrule   "z5"   "$B!~(B")	(its-defrule   "z%"   "$B"!(B")
  (its-defrule   "z6"   "$B!y(B")	(its-defrule   "z^"   "$B!z(B")
  (its-defrule   "z7"   "$B!}(B")	(its-defrule   "z&"   "$B!r(B")
  (its-defrule   "z8"   "$B!q(B")	(its-defrule   "z*"   "$B!_(B")
  (its-defrule   "z9"   "$B!i(B")	(its-defrule   "z("   "$B!Z(B")
  (its-defrule   "z0"   "$B!j(B")	(its-defrule   "z)"   "$B![(B")
  (its-defrule   "z-"   "$B!A(B")	(its-defrule   "z_"   "$B!h(B")
  (its-defrule   "z="   "$B!b(B")	(its-defrule   "z+"   "$B!^(B")
  (its-defrule   "z\\"  "$B!@(B")	(its-defrule   "z|"   "$B!B(B")
  (its-defrule   "z`"   "$B!-(B")	(its-defrule   "z~"   "$B!/(B")

  (its-defrule   "zq"   "$B!T(B")	(its-defrule   "zQ"   "$B!R(B")
  (its-defrule   "zw"   "$B!U(B")	(its-defrule   "zW"   "$B!S(B")
					; e
  (its-defrule   "zr"   "$B!9(B")	(its-defrule   "zR"   "$B!8(B")
  (its-defrule   "zt"   "$B!:(B")	(its-defrule   "zT"   "$B!x(B")
					; y u i o
  (its-defrule   "zp"   "$B")(B")	(its-defrule   "zP"   "$B",(B")
  (its-defrule   "z["   "$B!X(B")	(its-defrule   "z{"   "$B!L(B")
  (its-defrule   "z]"   "$B!Y(B")	(its-defrule   "z}"   "$B!M(B")

					; a
  (its-defrule   "zs"   "$B!3(B")	(its-defrule   "zS"   "$B!4(B")
  (its-defrule   "zd"   "$B!5(B")	(its-defrule   "zD"   "$B!6(B")
  (its-defrule   "zf"   "$B!7(B")	(its-defrule   "zF"   "$B"*(B")
  (its-defrule   "zg"   "$B!>(B")	(its-defrule   "zG"   "$B!=(B")
  (its-defrule   "zh"   "$B"+(B")
  (its-defrule   "zj"   "$B"-(B")
  (its-defrule   "zk"   "$B",(B")
  (its-defrule   "zl"   "$B"*(B")
  (its-defrule   "z;"   "$B!+(B")	(its-defrule   "z:"   "$B!,(B")
  (its-defrule   "z\'"  "$B!F(B")	(its-defrule   "z\""  "$B!H(B")

					; z
  (its-defrule   "zx"   ":-")	(its-defrule   "zX"   ":-)")
  (its-defrule   "zc"   "$B!;(B")	(its-defrule   "zC"   "$B!n(B")
  (its-defrule   "zv"   "$B"((B")	(its-defrule   "zV"   "$B!`(B")
  (its-defrule   "zb"   "$B!k(B")	(its-defrule   "zB"   "$B"+(B")
  (its-defrule   "zn"   "$B!l(B")	(its-defrule   "zN"   "$B"-(B")
  (its-defrule   "zm"   "$B!m(B")	(its-defrule   "zM"   "$B".(B")
  (its-defrule   "z,"   "$B!E(B")	(its-defrule   "z<"   "$B!e(B")
  (its-defrule   "z."   "$B!D(B")	(its-defrule   "z>"   "$B!f(B")
  (its-defrule   "z/"   "$B!&(B")	(its-defrule   "z?"   "$B!g(B")
  )

(define-its-state-machine-append its-kata-map
  (if its-kata-enable-double-n
      (its-defrule "nn" "$B%s(B"))

  (its-defrule "-" its-kata-horizontal)
  (its-defrule "[" its-kata-open-bracket)
  (its-defrule "]" its-kata-close-bracket)
  (its-defrule "." its-kata-period)
  (its-defrule "," its-kata-comma)

  (if its-kata-enable-zenkaku-alphabet
      (progn
	(its-defrule   "1"   "$B#1(B")  (its-defrule   "2"   "$B#2(B")
	(its-defrule   "3"   "$B#3(B")  (its-defrule   "4"   "$B#4(B")
	(its-defrule   "5"   "$B#5(B")  (its-defrule   "6"   "$B#6(B")
	(its-defrule   "7"   "$B#7(B")  (its-defrule   "8"   "$B#8(B")
	(its-defrule   "9"   "$B#9(B")  (its-defrule   "0"   "$B#0(B")
	(its-defrule   "!"   "$B!*(B")  (its-defrule   "@"   "$B!w(B")
	(its-defrule   "#"   "$B!t(B")  (its-defrule   "$"   "$B!p(B")
	(its-defrule   "%"   "$B!s(B")  (its-defrule   "^"   "$B!0(B")
	(its-defrule   "&"   "$B!u(B")  (its-defrule   "*"   "$B!v(B")
	(its-defrule   "("   "$B!J(B")  (its-defrule   ")"   "$B!K(B")
	(its-defrule   "="   "$B!a(B")  (its-defrule   "`"   "$B!.(B")
	(its-defrule   "\\"  "$B!o(B")  (its-defrule   "|"   "$B!C(B")
	(its-defrule   "_"   "$B!2(B")  (its-defrule   "+"   "$B!\(B")
	(its-defrule   "{"   "$B!P(B")  (its-defrule   "}"   "$B!Q(B")
	(its-defrule   ":"   "$B!'(B")  (its-defrule   ";"   "$B!((B")
	(its-defrule   "\""  "$B!I(B")  (its-defrule   "'"   "$B!G(B")
	(its-defrule   "<"   "$B!c(B")  (its-defrule   ">"   "$B!d(B")
	(its-defrule   "?"   "$B!)(B")  (its-defrule   "/"   "$B!?(B"))
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
      (its-defrule   "?"   "?")  (its-defrule   "/"   "/")))
  )

(provide 'its/kata)
;;; its/kata.el ends here.
