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

;; keywords for syntax coloring
(setq zkm-keywords
      '(("classpath[\s\t\n]\\|open[\s\t\n]\\|trimExclude[\s\t\n]\\|trimUnexclude[\s\t\n]\\|resetTrimExclusions[\s\t\n]\\|trim[\s\t\n]\\|exclude[\s\t\n]\\|unexclude[\s\t\n]\\|resetExclusions[\s\t\n]\\|obfuscateFlowExclude[\s\t\n]\\|obfuscateFlowUnexclude[\s\t\n]\\|resetObfuscateFlowExclusions[\s\t\n]\\|stringEncryptionExclude[\s\t\n]\\|stringEncryptionUnexclude[\s\t\n]\\|resetStringEncryptionExclusions[\s\t\n]\\|existingSerializedClasses[\s\t\n]\\|resetExistingSerializedClasses[\s\t\n]\\|fixedClasses[\s\t\n]\\|resetFixedClasses[\s\t\n]\\|groupings[\s\t\n]\\|resetGroupings[\s\t\n]\\|obfuscate[\s\t\n]\\|unobfuscate[\s\t\n]\\|saveAll[\s\t\n]\\|saveAllOld[\s\t\n]\\|gc[\s\t\n]\\|execute[\s\t\n]\\|print[\s\t\n]" . font-lock-function-name-face)
        ("and[\s\t\n]" . font-lock-constant-face)))

;; define the major mode.
(define-derived-mode zkm-mode fundamental-mode
  "zkm-mode is a major mode for editing language zkm."
  (setq font-lock-defaults '(zkm-keywords))
  
  ;; modify the keymap
  (define-key zkm-mode-map [remap comment-dwim] 'zkm-comment-dwim)
  
  (modify-syntax-entry ?\/ ". 124b" zkm-mode-syntax-table)
  (modify-syntax-entry ?* ". 23" zkm-mode-syntax-table)
  (modify-syntax-entry ?\n "> b")
  )
