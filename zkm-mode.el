;;; zkm-mode.el -- Edit of Zelix KlassMaster script ;; -*- coding: utf-8 -*-
;; Copyright (C) 2009 by Yongmun Kim
;; Author: Yongmun Kim
;; Create: 2009-12-17
;; Version 0.1
;; Keywords: 

;; This file is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by the
;; Free Software Foundation; either version 2, or (at your option) any
;; later version.

;; This file is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING. If not, write to the Free
;; Software Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
;; 02111-1307, USA.

;;; Commentary:

;; http://www.zelix.com/klassmaster/docs/langZKMScript.html

;;; Code:

(require 'font-lock)

(defun zkm-comment-dwim (arg)
  "Comment or uncomment current line or region in a smart way.
For detail, see `comment-dwim'."
  (interactive "*P")
  (require 'newcomment)
  (let ((deactivate-mark nil) (comment-start "//") (comment-end ""))
    (comment-dwim arg)))

(defvar zkm-font-lock-keywords
  (list
   (list zkm-comment-pattern)))
;; keywords for syntax coloring
(setq zkm-keywords
      '(("classpath[\s\t\n]\\|open[\s\t\n]\\|trimExclude[\s\t\n]\\|trimUnexclude[\s\t\n]\\|resetTrimExclusions[\s\t\n]\\|trim[\s\t\n]\\|exclude[\s\t\n]\\|unexclude[\s\t\n]\\|resetExclusions[\s\t\n]\\|obfuscateFlowExclude[\s\t\n]\\|obfuscateFlowUnexclude[\s\t\n]\\|resetObfuscateFlowExclusions[\s\t\n]\\|stringEncryptionExclude[\s\t\n]\\|stringEncryptionUnexclude[\s\t\n]\\|resetStringEncryptionExclusions[\s\t\n]\\|existingSerializedClasses[\s\t\n]\\|resetExistingSerializedClasses[\s\t\n]\\|fixedClasses[\s\t\n]\\|resetFixedClasses[\s\t\n]\\|groupings[\s\t\n]\\|resetGroupings[\s\t\n]\\|obfuscate[\s\t\n]\\|unobfuscate[\s\t\n]\\|saveAll[\s\t\n]\\|saveAllOld[\s\t\n]\\|gc[\s\t\n]\\|execute[\s\t\n]\\|print[\s\t\n]" . font-lock-function-name-face)
        ("and[\s\t\n]" . font-lock-constant-face)))

(defun zkm-comment-dwim ()
  "Comment or uncomment the current line or text selection."
  (interactive)
  
  ;; If there's no text selection, comment or uncomment the line
  ;; depending whether the WHOLE line is a comment. If there is a text
  ;; selection, using the first line to determine whether to
  ;; comment/uncomment.
  (let (p1 p2)
    (if (and transient-mark-mode mark-active)
        (save-excursion
          (setq p1 (region-beginning) p2 (region-end))
          (goto-char p1)
          (if (wholeLineIsCmt-p)
              (zkm-uncomment-region p1 p2)
            (zkm-comment-region p1 p2)
            ))
      (progn
        (if (wholeLineIsCmt-p)
            (zkm-uncomment-current-line)
          (zkm-comment-current-line)
          )) )))

(defun wholeLineIsCmt-p ()
  (save-excursion
    (move-beginning-of-line 1)
    (looking-at "[\s\t]*//")
    ))

(defun my-comment-current-line ()
  (interactive)
  (move-beginning-of-line 1)
  (insert "//")
  )

(defun my-uncomment-current-line ()
  "Remove “//” (if any) in the beginning of current line."
  (interactive)
  (when (wholeLineIsCmt-p)
    (move-beginning-of-line 1)
    (search-forward "//")
    (delete-backward-char 2)
    ))

(defun my-comment-region (p1 p2)
  "Add “//” to the beginning of each line of selected text."
  (interactive "r")
  (let ((deactivate-mark nil))
    (save-excursion
      (goto-char p2)
      (while (>= (point) p1)
        (zkm-comment-current-line)
        (previous-line)
        ))))

(defun my-uncomment-region (p1 p2)
  "Remove “//” (if any) in the beginning of each line of selected text."
  (interactive "r")
  (let ((deactivate-mark nil))
    (save-excursion
      (goto-char p2)
      (while (>= (point) p1)
        (zkm-uncomment-current-line)
        (previous-line) )) ))


;;; SYNTAX TABLE
(defvar zkm-mode-syntax-table
  (let ((table (make-syntax-table)))
    (modify-syntax-entry ?\/ ". 124b" table)
    (modify-syntax-entry ?* ". 23" table)
    (modify-syntax-entry ?\n "> b" table)
    table)
  "Syntax table used in ZKM mode.")

;; define the major mode.
(defun zkm-mode ()
  "Major mode for editing ZKM script."
  (interactive)
  (kill-all-local-variables)
  
  (set-syntax-table zkm-mode-syntax-table)
  (setq font-lock-defaults '(zkm-keywords))
  
  ;; modify the keymap
  (define-key zkm-mode-map [remap comment-dwim] 'zkm-comment-dwim))

(provide 'zkm-mode)

;;; zkm-mode.el ends here
