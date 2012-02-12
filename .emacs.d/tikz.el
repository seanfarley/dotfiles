;;; tikz.el --- AUCTeX style for the `TikZ' package. 

;; Copyright (C) 2011 by Florêncio Neves 

;; Author: florencioneves@... 
;; Keywords: tex 

;; This program is free software; you can redistribute it and/or modify 
;; it under the terms of the GNU General Public License as published by 
;; the Free Software Foundation; either version 2, or (at your option) 
;; any later version. 

;; This program is distributed in the hope that it will be useful, 
;; but WITHOUT ANY WARRANTY; without even the implied warranty of 
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the 
;; GNU General Public License for more details. 

;; You should have received a copy of the GNU General Public License 
;; along with this program; see the file COPYING.  If not, write to 
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330, 
;; Boston, MA 02111-1307, USA. 

;;; Commentary: 

;; AUCTeX style file for TikZ 

;;; Code: 

;; TO BE DONE 
;; 
;; * The pgfkeys package allow keys and values to contain spaces. 
;; Thus, the minibuffer keymap used by TeX-arg-pgfkeys should bind SPC 
;; to `self-insert-command'. Also, pgfkeys allows extra whitespace 
;; around "," and "=" in the key-val list, but `multi-prompt' 
;; considers the current key to be everything between a "," and "=" or 
;; the point, and similarly for values.  Thus it need to be adapted to 
;; work well with pgfkeys-based packages. 
;; 
;; * A parser for \tikuselibrary: Presence of the command 
;; \tikzuselibrary{<lib1>,...,<libn>} must cause the style hooks 
;; tikzlibrary<lib1>, ..., tikzlibrary<libn> to be run.  I couldn't 
;; decipher the internals of the parser. 

(defvar TeX-tikz-values-dimension '() 
  "List of values of type dimension.") 

(defvar TeX-tikz-values-code '() 
  "List of values of type code.") 

(defvar TeX-tikz-values-style '() 
  "Placeholder to indicate option is a style.") 

(defvar TeX-tikz-values-boolean 
  '("true" "false") 
  "List of values of type boolean.") 

(defvar TeX-tikz-values-color 
  '("black" "darkgray" "gray" "lightgray" "white" 
    "red" "green" "blue" "cyan" "magenta" "yellow") 
  "List of values of type color.") 

(defvar TeX-tikz-keys-color (mapcar 'list TeX-tikz-values-color) 
  "List of colors to be used as keys") 

(defvar TeX-tikz-keys-path 
  '(("color" TeX-tikz-values-color) 
    ("draw" TeX-tikz-values-color) 
    ("line width" TeX-tikz-values-dimension) 
    ("dashed" TeX-tikz-values-style)) 
  "List of options relevant for TikZ paths.") 
;;(make-variable-buffer-local 'TeX-tikz-keys-path) 

(defvar TeX-tikz-keys-node 
  '(("name" TeX-tikz-values-color) 
    ("at" TeX-tikz-values-style)) 
  "List of options relevant for TikZ nodes.") 
;; (make-variable-buffer-local 'TeX-tikz-keys-node) 

(defvar TeX-tikz-keys-matrix 
  '(("row sep" TeX-tikz-values-dimension) 
    ("column sep" TeX-tikz-values-dimension)) 
  "List of options relevant for TikZ matrices.") 
(make-variable-buffer-local 'TeX-tikz-keys-matrix) 

(defvar TeX-tikz-keys-picture 
  '(("baseline" TeX-tikz-values-dimension) 
    ("execute at begin picture" TeX-tikz-values-code) 
    ("execute at end picture" TeX-tikz-values-code)) 
  "List of options relevant for TikZ's tikzpicture environment.") 

(defvar TeX-tikz-keys-scope 
  '(("execute at begin scope" TeX-tikz-values-code) 
    ("execute at end scope" TeX-tikz-values-code)) 
  "List of options relevant for TikZ's scope environment.") 

(defun TeX-tikz-arg-pgfkeys (optional &rest key-val-alists) 
  "Prompt for keys and values in KEY-VAL-ALISTS. 
Insert the given value as a TeX macro argument.  If OPTIONAL is 
non-nil, insert it as an optional argument.  Each of the 
KEY-VAL-ALISTS is an alist or a symbol with value an alist.  The 
car of each element should be a string representing a key and the 
optional cdr should be a list with strings to be used as values 
for the key. 

This function is similar to `TeX-arg-key-val', but is (or will 
be) adapted to pgfkeys." 
  (TeX-arg-key-val 
   optional 
   (apply 'append 
          (mapcar '(lambda (x) (if (symbolp x) (symbol-value x) x)) 
                  key-val-alists)))) 


(defun TeX-tikz-arg-string (optional &optional prompt pre pos 
                                     mandatory initial-input) 
  "Prompt for a string. 

Unless MANDATORY, PROMPT will start with ``(Optional) ''. 
INITIAL-INPUT is a string to insert before reading input. 

The strings PRE and POS are used as openining and closing 
delimiters.  When not supplied, the values \" (\" and \")\" are 
used. 

This function can be used when an optional argument should be 
queried even if a previous optional argument was rejected." 
 (let ((TeX-arg-opening-brace (if pre pre " (")) 
       (TeX-arg-closing-brace (if pos pos ")")) 
       (optnl (not mandatory))) 
   (TeX-argument-insert 
    (read-string (TeX-argument-prompt optnl prompt "Name") initial-input) 
    optnl))) 

(defun TeX-tikz-arg-point (optional &optional pre pos) 
  "Leave the point here when finished inserting command.  The 
strings PRE and POS are inserted before and after point, and 
default to \" \" and \";\" respectively." 
 (insert (if pre pre " ")) 
 (if (null (marker-position exit-mark)) (set-marker exit-mark (point))) 
 (insert (if pos pos ";"))) 

(defun TeX-tikz-arg-library (optional) 
  "Insert arguments to \\usetikzlibrary." 
  (let ((files (TeX-search-files '("./") '("tex") t nil)) 
        (regexp "^tikzlibrary\\([[:alnum:]]+\\)\\.code\\.tex") 
        libs) 
    (while files 
      (if (string-match regexp (car files)) 
          (setq libs (cons (list (match-string 1 (car files))) libs))) 
      (setq files (cdr files))) 
    (TeX-argument-insert 
     (mapconcat 'identity 
                (multi-prompt "," t 
                              (TeX-argument-prompt optional "TikZ library" nil) 
                              libs) 
                ",") 
     optional))) 

(defun TeX-tikz-complete-operation () 
  "Prompt for optional arguments of TikZ path operations (--, circle, 
rectangle, edge, node, etc.). 

This function is intended to be called by `TeX-complete-symbol'. " 
  (let ((TeX-arg-opening-brace "") 
        (TeX-arg-closing-brace "")) 
    (apply 'TeX-tikz-arg-pgfkeys nil 
           (cdr (assoc (match-string 1) TeX-tikz-complete-operation-alist))) 
    (unless (looking-at "]") (insert "]")))) 

(defvar TeX-tikz-complete-operation-alist 
  '(("edge" 
     TeX-tikz-keys-path) 
    ("node" 
     TeX-tikz-keys-path 
     TeX-tikz-keys-node)) 
  "Alist used by `TeX-tikz-complete-operation' to determine the 
  keys relevant to each TikZ path operation.") 

(TeX-add-style-hook 
 "tikzlibrarymatrix" 
 (lambda () 
   (setq TeX-tikz-keys-matrix 
         (append '(("matrix of nodes" TeX-tikz-values-style) 
                   ("matrix of math nodes" TeX-tikz-values-style)) 
                 TeX-tikz-keys-matrix)))) 

(TeX-add-style-hook 
 "tikz" 
 (lambda () 
   (LaTeX-add-environments 
    '("tikzpicture" LaTeX-env-args [TeX-tikz-arg-pgfkeys TeX-tikz-keys-picture]) 
    '("scope" LaTeX-env-args [TeX-tikz-arg-pgfkeys TeX-tikz-keys-scope])) 
   (TeX-add-symbols 
    '("usetikzlibrary" TeX-tikz-arg-library) 
    '("tikz" [TeX-tikz-arg-pgfkeys TeX-tikz-keys-picture] 1) 
    '("path" 
      [TeX-tikz-arg-pgfkeys TeX-tikz-keys-path 
                            TeX-tikz-keys-color] 
      TeX-tikz-arg-point) 
    '("draw" 
      [TeX-tikz-arg-pgfkeys TeX-tikz-keys-path 
                            TeX-tikz-keys-color] 
      TeX-tikz-arg-point) 
    '("node" 
      [TeX-tikz-arg-pgfkeys TeX-tikz-keys-node 
                            TeX-tikz-keys-path] 
      TeX-tikz-arg-string 
      (TeX-tikz-arg-string "At" " at (") 
      (TeX-tikz-arg-point " {" "};")) 
    '("matrix" 
      [TeX-tikz-arg-pgfkeys TeX-tikz-keys-matrix 
                            TeX-tikz-keys-node 
                            TeX-tikz-keys-path] 
      TeX-tikz-arg-string 
      (TeX-tikz-arg-string "At" " at (") 
      (TeX-tikz-arg-point " {" "};"))) 
   (setq TeX-complete-list 
         (append '(("\\b\\(edge\\|node\\)[[:blank:]]*\\[" 
                    TeX-tikz-complete-operation)) 
                 TeX-complete-list)))) 

;; While we do not have a parser for \usetikzlibrary, we run all hooks 
(TeX-add-style-hook 
 "tikz" 
 (lambda () (TeX-run-style-hooks "tikzlibrarymatrix")))
