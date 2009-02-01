;;; its/hankata.el --- Hnakaku Katakana Input in Egg Input Method Architecture

;; Copyright (C) 1999, 2000 Free Software Foundation, Inc

;; Author: NIIBE Yutaka <gniibe@chroot.org>

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


;;; Code:

(eval-when-compile
  (require 'its)
  (require 'cl))

(eval-when (compile)
  (defconst its-compaction-enable t))

(defvar its-han-kata-enable-double-n nil "*Enable \"nn\" input for \"$B%s(B\" ")
(defvar its-han-kata-enable-zenkaku-alphabet t "*Enable Zenkaku alphabet")
(defvar its-han-kata-period "(I!(B" "*$B%T%j%*%I(B")  ; ". " "$B!%(B"
(defvar its-han-kata-comma  "(I$(B" "*$B%3%s%^(B")    ; ", " "$B!$(B"
(defvar its-han-kata-open-bracket  "(I"(B" "*[")  ; "$B!N(B"
(defvar its-han-kata-close-bracket  "(I#(B" "*]") ; "$B!O(B"
(defvar its-han-kata-horizontal  "(I0(B" "*-")    ; "$B!](B"

(define-its-state-machine its-han-kata-map
  "roma-han-kata" "(I11(B" Japanese
  "Map for Romaji-Hankaku-Katakana translation. (Japanese)"

  (defconst its-hankaku-escape "~")  ;; Escape character to Hankaku inputs

  (its-defrule-select-mode-temporally "q" downcase)

;;; k      k
;;; kk     $B%C(Bk
;;; kka    $B%C%+(B
;;;
;;; kkk    $B%C(Bk DING!

  (its-defrule "tch"  "(I/(B" -2)

;;; $B!V%s!W$NF~NO(B

  (dolist (q1 '("b" "m" "p"))
    (its-defrule (concat "m" q1) "(I](B" -1))

  (its-defrule* "n"  "(I](B")
  (its-defrule  "n'" "(I](B")
  (its-defrule  "N"  "(I](B")

  (let ((small '"x" ))
    (its-defrule (concat small "a") "(I'(B")
    (its-defrule (concat small "i") "(I((B")
    (its-defrule (concat small "u") "(I)(B")
    (its-defrule (concat small "e") "(I*(B")
    (its-defrule (concat small "o") "(I+(B")
    (its-defrule (concat small "ya") "(I,(B")
    (its-defrule (concat small "yu") "(I-(B")
    (its-defrule (concat small "yo") "(I.(B")
    (its-defrule (concat small "tu") "(I/(B")
    (its-defrule (concat small "tsu") "(I/(B")
    (its-defrule (concat small "wa") "(I\(B")
    )

  (its-defrule   "a"    "(I1(B")
  (its-defrule   "i"    "(I2(B")
  (its-defrule   "u"    "(I3(B")
  (its-defrule   "e"    "(I4(B")
  (its-defrule   "o"    "(I5(B")

  (dolist (k '(("ka"  "(I6(B") ("ki"  "(I7(B") ("ku"  "(I8(B") ("ke"  "(I9(B") ("ko"  "(I:(B")
	       ("kya" "(I7,(B") ("kyu"  "(I7-(B") ("kye"  "(I7*(B") ("kyo"  "(I7.(B")))
    (its-defrule (car k) (cadr k))
    (its-defrule (concat "k" (car k)) (concat "(I/(B" (cadr k))))
  (its-defoutput "kk" "(I/(Bk")
  (its-defoutput "kky" "(I/(Bky")

  (dolist (s '(("sa"  "(I;(B") ("si"  "(I<(B") ("su"  "(I=(B") ("se"  "(I>(B") ("so"  "(I?(B")
	       ("sya"  "(I<,(B") ("syu"  "(I<-(B") ("sye"  "(I<*(B") ("syo"  "(I<.(B")
	       ("sha"  "(I<,(B") ("shi"  "(I<(B") ("shu"  "(I<-(B") ("she"  "(I<*(B")
	       ("sho"  "(I<.(B")))
    (its-defrule (car s) (cadr s))
    (its-defrule (concat "s" (car s)) (concat "(I/(B" (cadr s))))
  (its-defoutput "ss" "(I/(Bs")
  (its-defoutput "ssy" "(I/(Bsy")
  (its-defoutput "ssh" "(I/(Bsh")

  (dolist (T '(("ta"  "(I@(B") ("ti"  "(IA(B") ("tu"  "(IB(B") ("te"  "(IC(B") ("to"  "(ID(B")
	       ("tya"  "(IA,(B") ("tyi"  "(IC((B") ("tyu"  "(IA-(B") ("tye"  "(IA*(B")
	       ("tyo"  "(IA.(B") ("tsu"  "(IB(B")))
    (its-defrule (car T) (cadr T))
    (its-defrule (concat "t" (car T)) (concat "(I/(B" (cadr T))))
  (its-defoutput "tt" "(I/(Bt")
  (its-defoutput "tty" "(I/(Bty")
  (its-defoutput "tts" "(I/(Bts")

  (dolist (c '(("cha"  "(IA,(B") ("chi"  "(IA(B") ("chu"  "(IA-(B")
	       ("che"  "(IA*(B") ("cho"  "(IA.(B")))
    (its-defrule (car c) (cadr c))
    (its-defrule (concat "c" (car c)) (concat "(I/(B" (cadr c))))
  (its-defoutput "cc" "(I/(Bc")
  (its-defoutput "cch" "(I/(Bch")

  (dolist (h '(("ha"  "(IJ(B") ("hi"  "(IK(B") ("hu"  "(IL(B") ("he"  "(IM(B") ("ho"  "(IN(B")
	       ("hya"  "(IK,(B") ("hyu"  "(IK-(B") ("hye"  "(IK*(B") ("hyo"  "(IK.(B")))
    (its-defrule (car h) (cadr h))
    (its-defrule (concat "h" (car h)) (concat "(I/(B" (cadr h))))
  (its-defoutput "hh" "(I/(Bh")
  (its-defoutput "hhy" "(I/(Bhy")

  (dolist (f '(("fa"  "(IL'(B") ("fi"  "(IL((B") ("fu"  "(IL(B") ("fe"  "(IL*(B")
	       ("fo"  "(IL+(B")))
    (its-defrule (car f) (cadr f))
    (its-defrule (concat "f" (car f)) (concat "(I/(B" (cadr f))))
  (its-defoutput "ff" "(I/(Bf")

  (dolist (r '(("ra"  "(IW(B") ("ri"  "(IX(B") ("ru"  "(IY(B") ("re"  "(IZ(B") ("ro"  "(I[(B")
	       ("rya"  "(IX,(B") ("ryu"  "(IX-(B") ("rye"  "(IX*(B") ("ryo"  "(IX.(B")))
    (its-defrule (car r) (cadr r))
    (its-defrule (concat "r" (car r)) (concat "(I/(B" (cadr r))))
  (its-defoutput "rr" "(I/(Br")
  (its-defoutput "rry" "(I/(Bry")

  (dolist (l '(("la"  "(IW(B") ("li"  "(IX(B") ("lu"  "(IY(B") ("le"  "(IZ(B") ("lo"  "(I[(B")
	       ("lya"  "(IX,(B") ("lyu"  "(IX-(B") ("lye"  "(IX*(B") ("lyo"  "(IX.(B")))
    (its-defrule (car l) (cadr l))
    (its-defrule (concat "l" (car l)) (concat "(I/(B" (cadr l))))
  (its-defoutput "ll" "(I/(Bl")
  (its-defoutput "lly" "(I/(Bly")

  (dolist (g '(("ga"  "(I6^(B") ("gi"  "(I7^(B") ("gu"  "(I8^(B") ("ge"  "(I9^(B") ("go"  "(I:^(B")
	       ("gya"  "(I7^,(B") ("gyu"  "(I7^-(B") ("gye"  "(I7^*(B") ("gyo"  "(I7^.(B")))
    (its-defrule (car g) (cadr g))
    (its-defrule (concat "g" (car g)) (concat "(I/(B" (cadr g))))
  (its-defoutput "gg" "(I/(Bg")
  (its-defoutput "ggy" "(I/(Bgy")

  (dolist (z '(("za"  "(I;^(B") ("zi"  "(I<^(B") ("zu"  "(I=^(B") ("ze"  "(I>^(B") ("zo"  "(I?^(B")
	       ("zya"  "(I<^,(B") ("zyu"  "(I<^-(B") ("zye"  "(I<^*(B") ("zyo"  "(I<^.(B")))
    (its-defrule (car z) (cadr z))
    (its-defrule (concat "z" (car z)) (concat "(I/(B" (cadr z))))
  (its-defoutput "zz" "(I/(Bz")
  (its-defoutput "zzy" "(I/(Bzy")

  (dolist (j '(("ja"  "(I<^,(B") ("ji"  "(I<^(B") ("ju"  "(I<^-(B") ("je"  "(I<^*(B")
	       ("jo"  "(I<^.(B") ("jya"  "(I<^,(B") ("jyu"  "(I<^-(B") ("jye"  "(I<^*(B")
	       ("jyo"  "(I<^.(B")))
    (its-defrule (car j) (cadr j))
    (its-defrule (concat "j" (car j)) (concat "(I/(B" (cadr j))))
  (its-defoutput "jj" "(I/(Bj")
  (its-defoutput "jjy" "(I/(Bjy")

  (dolist (d '(("da"  "(I@^(B") ("di"  "(IA^(B") ("du"  "(IB^(B") ("de"  "(IC^(B") ("do"  "(ID^(B")
	       ("dya"  "(IA^,(B") ("dyi"  "(IC^((B") ("dyu"  "(IA^-(B") ("dye"  "(IA^*(B")
	       ("dyo"  "(IA^.(B")))
    (its-defrule (car d) (cadr d))
    (its-defrule (concat "d" (car d)) (concat "(I/(B" (cadr d))))
  (its-defoutput "dd" "(I/(Bd")
  (its-defoutput "ddy" "(I/(Bdy")

  (dolist (b '(("ba"  "(IJ^(B") ("bi"  "(IK^(B") ("bu"  "(IL^(B") ("be"  "(IM^(B") ("bo"  "(IN^(B")
	       ("bya"  "(IK^,(B") ("byu"  "(IK^-(B") ("bye"  "(IK^*(B") ("byo"  "(IK^.(B")))
    (its-defrule (car b) (cadr b))
    (its-defrule (concat "b" (car b)) (concat "(I/(B" (cadr b))))
  (its-defoutput "bb" "(I/(Bb")
  (its-defoutput "bby" "(I/(Bby")

  (dolist (p '(("pa"  "(IJ_(B") ("pi"  "(IK_(B") ("pu"  "(IL_(B") ("pe"  "(IM_(B") ("po"   "(IN_(B")
	       ("pya"  "(IK_,(B") ("pyu"  "(IK_-(B") ("pye"  "(IK_*(B") ("pyo"  "(IK_.(B")))
    (its-defrule (car p) (cadr p))
    (its-defrule (concat "p" (car p)) (concat "(I/(B" (cadr p))))
  (its-defoutput "pp" "(I/(Bp")
  (its-defoutput "ppy" "(I/(Bpy")

  (dolist (v '(("va" "(I3^'(B") ("vi" "(I3^((B") ("vu" "(I3^(B") ("ve" "(I3^*(B")
	       ("vo" "(I3^+(B")))
    (its-defrule (car v) (cadr v))
    (its-defrule (concat "v" (car v)) (concat "(I/(B" (cadr v))))
  (its-defoutput "vv" "(I/(Bv")

  (its-defrule   "ma"   "(IO(B")
  (its-defrule   "mi"   "(IP(B")
  (its-defrule   "mu"   "(IQ(B")
  (its-defrule   "me"   "(IR(B")
  (its-defrule   "mo"   "(IS(B")
  (its-defrule   "mya"  "(IP,(B")
  (its-defrule   "myu"  "(IP-(B")
  (its-defrule   "mye"  "(IP*(B")
  (its-defrule   "myo"  "(IP.(B")
  (its-defrule   "ya"   "(IT(B")
  (its-defrule   "yi"   "(I2(B")
  (its-defrule   "yu"   "(IU(B")
  (its-defrule   "yo"   "(IV(B")
  (its-defrule   "ye"   "(I2*(B")
  (its-defrule   "wa"   "(I\(B")
  (its-defrule   "wi"   "(I((B")
  (its-defrule   "wu"   "(I3(B")
  (its-defrule   "we"   "(I*(B")
  (its-defrule   "wo"   "(I&(B")

  (its-defrule   "kwa"  "(I8\(B")
  (its-defrule   "kwi"  "(I8((B")
  (its-defrule   "kwu"  "(I8(B")
  (its-defrule   "kwe"  "(I8*(B")
  (its-defrule   "kwo"  "(I8+(B")
  (its-defrule   "gwa"  "(I8^\(B")
  (its-defrule   "gwi"  "(I8^((B")
  (its-defrule   "gwu"  "(I8^(B")
  (its-defrule   "gwe"  "(I8^*(B")
  (its-defrule   "gwo"  "(I8^+(B")
  (its-defrule   "tsa"  "(IB'(B")
  (its-defrule   "tsi"  "(IB((B")
  (its-defrule   "tse"  "(IB*(B")
  (its-defrule   "tso"  "(IB+(B")

  (its-defrule   "na"   "(IE(B")
  (its-defrule   "ni"   "(IF(B")
  (its-defrule   "nu"   "(IG(B")
  (its-defrule   "ne"   "(IH(B")
  (its-defrule   "no"   "(II(B")
  (its-defrule   "nya"  "(IF,(B")
  (its-defrule   "nyu"  "(IF-(B")
  (its-defrule   "nye"  "(IF*(B")
  (its-defrule   "nyo"  "(IF.(B")

  (its-defrule   "xti"  "(IC((B")
  (its-defrule   "xdi"  "(IC^((B")
  (its-defrule   "xdu"  "(ID^)(B")
  (its-defrule   "xde"  "(IC^*(B")
  (its-defrule   "xdo"  "(ID^+(B")
  (its-defrule   "xwi"  "(I3((B")
  (its-defrule   "xwe"  "(I3*(B")
  (its-defrule   "xwo"  "(I3+(B")

;;;
;;; Symbol inputs
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
    (its-defrule (concat its-hankaku-escape upcase) upcase)))

(define-its-state-machine-append its-han-kata-map
  (if its-han-kata-enable-double-n
      (its-defrule "nn" "(I](B"))

  (its-defrule "-" its-han-kata-horizontal)
  (its-defrule "[" its-han-kata-open-bracket)
  (its-defrule "]" its-han-kata-close-bracket)
  (its-defrule "." its-han-kata-period)
  (its-defrule "," its-han-kata-comma)
  )

(provide 'its/hankata)
;;; its/kata.el ends here.
