;;; egg/anthy.el --- ANTHY Support (high level interface) in Egg
;;;                Input Method Architecture

;; Copyright (C) 2002 The Free Software Initiative of Japan

;; Author: NIIBE Yutaka <gniibe@m17n.org>

;; Maintainer: NIIBE Yutaka <gniibe@m17n.org>

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

(require 'egg)
(require 'egg-edep)

(defgroup anthy nil
  "Anthy interface for Tamago 4."
  :group 'egg)

(setplist 'anthy-conversion-backend
	  '(egg-start-conversion          anthy-convert
	    egg-get-bunsetsu-source       anthy-get-bunsetsu-source
	    egg-get-bunsetsu-converted    anthy-get-bunsetsu-converted
	    egg-list-candidates           anthy-get-candidates
	    egg-decide-candidate          anthy-select-candidate
	    egg-change-bunsetsu-length    anthy-resize-segment
	    egg-end-conversion            anthy-commit
	    ;;
	    egg-get-source-language       anthy-get-source-language
	    egg-get-converted-language    anthy-get-converted-language))

(defconst anthy-backend-alist '((Japanese ((anthy-conversion-backend)))))

(egg-set-finalize-backend '(anthy-finalize-backend))

(defvar anthy-proc nil
  "Process of ANTHY helper agent.")

;; <environments> ::= ( <env> ... <env> )
;;
;; <env> ::= <context-descriptor>
;; <context-descriptor> ::= <integer>
(defvar anthy-environment-pool nil
  "Environments for ANTHY kana-kanji conversion, to be used.")

(defvar anthy-environments-in-use nil
  "Environments for ANTHY kana-kanji conversion, in use.")

;;
;; <anthy-bunsetsu> ::=
;;  [ <env> <source> <converted> <candidates> <candidate-pos> <seg-no> ]
(defsubst anthy-make-bunsetsu (env source converted seg-no)
  (egg-bunsetsu-create
   'anthy-conversion-backend
   (vector env source converted nil 0 seg-no)))

(defsubst anthybunsetsu-get-env (b)
  (aref (egg-bunsetsu-get-info b) 0))
(defsubst anthybunsetsu-get-source (b)
  (aref (egg-bunsetsu-get-info b) 1))
(defsubst anthybunsetsu-get-converted (b)
  (aref (egg-bunsetsu-get-info b) 2))
(defsubst anthybunsetsu-get-candidates (b)
  (aref (egg-bunsetsu-get-info b) 3))
(defsubst anthybunsetsu-set-candidates (b z)
  (aset (egg-bunsetsu-get-info b) 3 z))
(defsubst anthybunsetsu-get-candidate-pos (b)
  (aref (egg-bunsetsu-get-info b) 4))
(defsubst anthybunsetsu-set-candidate-pos (b zp)
  (aset (egg-bunsetsu-get-info b) 4 zp))
(defsubst anthybunsetsu-get-seg-no (b)
  (aref (egg-bunsetsu-get-info b) 5))

(defun anthy-get-bunsetsu-source (b)
  (anthybunsetsu-get-source b))

(defun anthy-get-bunsetsu-converted (b)
  (let ((cands (anthybunsetsu-get-candidates b)))
    (if cands
	(nth (anthybunsetsu-get-candidate-pos b) cands)
      (anthybunsetsu-get-converted b))))

(defun anthy-get-source-language (b) 'Japanese)
(defun anthy-get-converted-language (b) 'Japanese)

;; Getting new context-descriptor, and returns environment with 'inuse' bit
(defun anthy-new-environment ()
  (if (null anthy-proc)
      (let ((buf (generate-new-buffer " *ANTHY*"))
	    (process-connection-type nil)) ; avoid using pty
	(setq anthy-proc
	      (start-process "anthy-agent" buf "anthy-agent" "--egg"))
	(process-kill-without-query anthy-proc)
	(set-process-coding-system anthy-proc 'euc-jp-dos 'euc-jp-dos)
	(set-process-sentinel anthy-proc 'anthy-proc-sentinel)
	(set-marker-insertion-type (process-mark anthy-proc) t)
	(save-excursion
	  (set-buffer buf)
	  (erase-buffer)
	  (buffer-disable-undo))))
  (anthyipc-get-greeting anthy-proc)
  (anthyipc-new-context anthy-proc))

;;; XXX: Don't kill buffer (for now) so that I can debug this program
(defun anthy-proc-sentinel (proc reason)
;  (kill-buffer (process-buffer proc))
  (setq anthy-proc nil
	anthy-environments-in-use nil
	anthy-environment-pool nil))

;;; anthyipc-release-context


(defun anthy-get-environment ()
  "Return the ANTHY environment."
  (if anthy-environment-pool
      (let ((env (car anthy-environment-pool)))
	(setq anthy-environment-pool (cdr anthy-environment-pool))
	(setq anthy-environments-in-use (cons env anthy-environments-in-use))
	env)
    (let ((env (anthy-new-environment)))
      (setq anthy-environments-in-use (cons env anthy-environments-in-use))
      env)))

;;
;; Returns list of bunsetsu
;;
(defun anthy-convert (backend yomi &optional context)
  "Convert YOMI string to kanji, and enter conversion mode.
Return the list of bunsetsu."
  (let ((env (anthy-get-environment)))
    (anthyipc-convert anthy-proc env yomi)))

;;
;;
;;
(defun anthy-commit (bunsetsu-list abort)
  (let ((env (anthybunsetsu-get-env (car bunsetsu-list))))
    (anthyipc-commit anthy-proc env (if abort 1 0))
    (setq anthy-environment-pool (cons env anthy-environment-pool))
    (setq anthy-environments-in-use (delq env anthy-environments-in-use))))

;;
;; Returns ( <pos> <candidates> )
;;
(defun anthy-get-candidates (bunsetsu-list prev-bunsetsu next-bunsetsu major)
  (let ((bunsetsu (car bunsetsu-list)))
    (if (anthybunsetsu-get-candidates bunsetsu)
	(cons (anthybunsetsu-get-candidate-pos bunsetsu)
	      (anthybunsetsu-get-candidates bunsetsu))
      (let* ((env (anthybunsetsu-get-env bunsetsu))
	     (seg-no (anthybunsetsu-get-seg-no bunsetsu))
	     (cands (anthyipc-get-candidates anthy-proc env seg-no)))
	(cons (anthybunsetsu-set-candidate-pos bunsetsu 0)
	      (anthybunsetsu-set-candidates bunsetsu cands))))))

;; Returns list of list of bunsetsu
(defun anthy-select-candidate (bunsetsu-list candidate-pos prev-b next-b)
  (let* ((bunsetsu (car bunsetsu-list))
	 (candidate-list (anthybunsetsu-get-candidates bunsetsu))
	 (candidate (nth candidate-pos candidate-list))
	 (env (anthybunsetsu-get-env bunsetsu))
	 (seg-no (anthybunsetsu-get-seg-no bunsetsu)))
    (anthybunsetsu-set-candidate-pos bunsetsu candidate-pos)
    ;; Anthy doesn't have capability of changing another segment
    ;; at the selection of a segment.
    ;; So, just ignore the result of "SELECT-CANDIDATE"
    (anthyipc-select-candidate anthy-proc env seg-no candidate-pos)
    (list (list bunsetsu))))

;; Returns list of list of bunsetsu
(defun anthy-resize-segment (bunsetsu-list prev-b next-b len major)
  (let ((bunsetsu (car bunsetsu-list)))
    (let ((env (anthybunsetsu-get-env bunsetsu))
	  (seg-no (anthybunsetsu-get-seg-no bunsetsu))
	  (prevlen (length (anthybunsetsu-get-source bunsetsu))))
      (let ((r (anthyipc-resize-segment anthy-proc env seg-no
					(if (< prevlen len) 0 1))))
	;; XXX: I don't know what this means, 
	;; but this works.  Blame EGG.
	(list (list (car r)) nil (cdr r))))))

(defun anthy-finalize-backend ()
  (if anthy-proc
      (progn
	(delete-process anthy-proc)
	(setq anthy-proc nil))))

;;; setup

(load "egg/anthyipc")
(run-hooks 'anthy-load-hook)

;;;###autoload
(defun egg-activate-anthy (&rest arg)
  "Activate ANTHY backend of Tamago 4."
  (apply 'egg-mode (append arg anthy-backend-alist)))

;;; egg/anthy.el ends here.
