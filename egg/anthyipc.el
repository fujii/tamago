;;; egg/anthyipc.el --- ANTHY IPC Support (low level interface) in Egg
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

(defmacro anthyipc-call-with-proc (proc vlist send-expr &rest receive-exprs)
  `(let* ((proc ,proc)
	  (buffer (process-buffer proc))
	  ,@vlist)
     (if (and (eq (process-status proc) 'run)
	      (buffer-live-p buffer))
	 (save-excursion
	   (set-buffer buffer)
	   (erase-buffer)
	   ,send-expr
	   (goto-char (point-max))
	   (process-send-region proc (point-min) (point-max))
	   ,@receive-exprs)
       (egg-error "process %s was killed" proc))))

(defun anthyipc-wait-line ()
  (let ((start (point)))
    (while (not (search-forward "\n" nil 1))
      (accept-process-output proc 1000)
      (goto-char start))
    (goto-char start)))

(defun anthyipc-accept-ok ()
  (anthyipc-wait-line)
  (if (eq (char-after) ?+)
      ;; "+OK"
      (goto-char (point-max))
    (egg-error "protocol error: %s" (buffer-substring (point) (point-max)))))

(defun anthyipc-accept-number ()
  (anthyipc-wait-line)
  (if (eq (char-after)  ?+)
      ;; "+OK <number>"
      (progn
	(forward-char 4)
	(prog1
	    (read (current-buffer))
	  (goto-char (point-max))))
    (egg-error "protocol error: %s" (buffer-substring (point) (point-max)))))

(defun anthyipc-read-string ()
  (if (eq (char-after) ?\ )
    (forward-char 1))
  (let ((start (point)))
    (while (and (char-after)
		(not (eq (char-after) ?\ ))
		(not (eq (char-after) ?\n)))
      (forward-char 1))
    (buffer-substring start (point))))

(defun anthyipc-accept-segments (env seg-no-orig)
  (anthyipc-wait-line)
  (if (eq (char-after) ?+)
      (progn
	(forward-char 1)
	(if (eq (char-after) ?O)
	    ;; "+OK"
	    (progn
	      (goto-char (point-max))
	      t)
	  ;; "+DATA <seg-no> <num-segments-removed> <num-segments-inserted>"
	  ;; "<num-candidates> <converted> <yomi>"*N
	  ;; ""
	  ;;
	  (forward-char 5)
	  (let* ((seg-no (read (current-buffer)))
		 (num-segments-removed (read (current-buffer)))
		 (num-segments-inserted (read (current-buffer)))
		 (segment-list nil)
		 (in-loop t)
		 (i seg-no))
	    (while in-loop
	      (forward-char 1)
	      (anthyipc-wait-line)
	      (if (eq (char-after) ?\n)
		  (setq in-loop nil)
		(let* ((num-candidates (read (current-buffer)))
		       (converted (anthyipc-read-string))
		       (source (anthyipc-read-string))
		       (segment (anthy-make-bunsetsu env source converted i)))
		  (setq i (1+ i))
		  (setq segment-list (cons segment segment-list)))))
	    ;; XXX check if seg-no == seg-no-orig
	    ;; XXX check inserted and length of segment-list???
	    (forward-char 1)
	    (cons seg-no (cons num-segments-removed (reverse segment-list))))))
    (egg-error "protocol error: %s" (buffer-substring (point) (point-max)))))

(defun anthyipc-accept-candidates ()
  (anthyipc-wait-line)
  (if (eq (char-after) ?+)
      (progn
	;; "+DATA <offset> <num-candidates>"
	;; "<converted>"*N
	;; ""
	(forward-char 6)
	(let* ((offset (read (current-buffer)))
	       (num-candidates (read (current-buffer)))
	       (candidate-list nil)
	       (in-loop t))
	  (while in-loop
	    (forward-char 1)
	    (anthyipc-wait-line)
	    (if (eq (char-after) ?\n)
		(setq in-loop nil)
	      (let ((candidate (anthyipc-read-string)))
		(setq candidate-list (cons candidate candidate-list)))))
	  ;; XXX check num-candidates and length of candidate-list???
	  (forward-char 1)
	  (cons offset (reverse candidate-list))))
    (egg-error "protocol error: %s" (buffer-substring (point) (point-max)))))

(defun anthyipc-get-greeting (proc)
  (anthyipc-call-with-proc proc ()
    nil
    (anthyipc-wait-line)
    (message (buffer-substring (point-min) (1- (point-max))))))

(defun anthyipc-new-context (proc)
  (anthyipc-call-with-proc proc ()
    (insert "NEW-CONTEXT INPUT=#18 OUTPUT=#18\n")
    (anthyipc-accept-number)))

(defun anthyipc-release-context (proc cont)
  (anthyipc-call-with-proc proc ()
    (insert (format "RELEASE-CONTEXT %d\n" cont))
    (anthyipc-accept-ok)))

;; Returns list of bunsetsu
(defun anthyipc-convert (proc cont yomi)
  (anthyipc-call-with-proc proc ()
    (insert (format "CONVERT %d %s\n" cont yomi))
    (let ((r (anthyipc-accept-segments cont 0)))
      (cdr (cdr r)))))

(defun anthyipc-commit (proc cont cancel)
  (anthyipc-call-with-proc proc ()
    (insert (format "COMMIT %d %d\n" cont cancel))
    (anthyipc-accept-ok)))

;;; Returns list of candidate
(defconst anthy-max-candidates 9999)
(defun anthyipc-get-candidates (proc cont seg-no)
  (anthyipc-call-with-proc proc ()
    (insert
     (format "GET-CANDIDATES %d %d %d %d\n" cont seg-no 0 anthy-max-candidates))
    (let ((r (anthyipc-accept-candidates)))
      (cdr r))))

;;; Returns segments
(defun anthyipc-select-candidate (proc cont seg-no candidate-no)
  (anthyipc-call-with-proc proc ()
    (insert (format "SELECT-CANDIDATE %d %d %d\n" cont seg-no candidate-no))
    (anthyipc-accept-segments cont seg-no)))

;;; Returns segments
(defun anthyipc-resize-segment (proc cont seg-no inc-dec)
  (anthyipc-call-with-proc proc ()
    (insert (format "RESIZE-SEGMENT %d %d %d\n" cont seg-no inc-dec))
    (cddr (anthyipc-accept-segments cont seg-no))))

;;; egg/anthyipc.el ends here.
