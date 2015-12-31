;;; ld-mode-el -- Major mode for editing LD scripts

;; Author: Spencer Nelson <s@spenczar.com>
;; Created: 29 Dec 2015

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2 of
;; the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be
;; useful, but WITHOUT ANY WARRANTY; without even the implied
;; warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
;; PURPOSE.  See the GNU General Public License for more details.

;; You should have received a copy of the GNU General Public
;; License along with this program; if not, write to the Free
;; Software Foundation, Inc., 59 Temple Place, Suite 330, Boston,
;; MA 02111-1307 USA

;;; Commentary:
;;
;; This mode is used for editing LD scripts.

;;; Code:

(defvar ld-mode-hook nil)
(defvar ld-mode-map
  (let (map (make-sparse-keymap))
    ;; Example:   (define-key map "\C-j" 'newline-and-indent)
    map)
  "Keymap for LD major mode.")

(defconst ld--keywords
  '(
    ;; 3.4.1 Setting the Entry Point
    "ENTRY"
    ;; 3.4.2 Commands Dealing with Files
    "INCLUDE" "INPUT" "GROUP" "AS_NEEDED" "OUTPUT" "SEARCH_DIR" "STARTUP"
    ;; 3.4.3 Commands Dealing with Object File Formats
    "OUTPUT_FORMAT" "TARGET"
    ;; 3.4.4 Assign alias names to memory regions
    "REGION_ALIAS"
    ;; 3.4.5 Other Linker Script Commands
    "ASSERT" "EXTERN" "FORCE_COMMON_ALLOCATION"
    "INHIBIT_COMMON_ALLOCATION" "INSERT" "AFTER" "BEFORE"
    "NOCROSSREFS" "OUTPUT_ARCH" "LD_FEATURE"
    ;; 3.5.2 PROVIDE
    "PROVIDE"
    ;; 3.5.3 PROVIDE_HIDDEN
    "PROVIDE_HIDDEN"
    ;; 3.6 SECTIONS Command
    "SECTIONS"
    ;; 3.6.4.2 Input Section Wildcard Patterns
    "SORT" "SORT_BY_NAME" "SORT_BY_ALIGNMENT" "SORT_BY_INIT_PRIORITY"
    ;; 3.6.4.3 Input Section for Common Symbols
    "COMMON"
    ;; 3.6.4.4 Input Section and Garbage Collection
    "KEEP"
    ;; 3.6.5 Output Section Data
    "BYTE" "SHORT" "LONG" "QUAD" "SQUAD" "FILL"
    ;; 3.6.6 Output Section Keywords
    "CREATE_OBJECT_SYMBOLS" "CONSTRUCTORS"
    "__CTOR_LIST__" "__CTOR_END__" "__DTOR_LIST__" "__DTOR_END__"
    ;; 3.6.7 Output Section Discarding
    ;; See `ld-script-font-lock-keywords'
    ;; 3.6.8.1 Output Section Type
    "NOLOAD" "DSECT" "COPY" "INFO" "OVERLAY"
    ;; 3.6.8.2 Output Section LMA
    "AT"
    ;; 3.6.8.4 Forced Input Alignment
    "SUBALIGN"
    ;; 3.6.8.5 Output Section Constraint
    "ONLY_IF_RO" "ONLY_IF_RW"
    ;; 3.6.8.7 Output Section Phdr
    ":PHDR"
    ;; 3.7 MEMORY Command
    "MEMORY"
    ;; 3.8 PHDRS Command
    "PHDRS" "FILEHDR" "FLAGS"
    "PT_NULL" "PT_LOAD" "PT_DYNAMIC" "PT_INTERP" "PT_NOTE" "PT_SHLIB" "PT_PHDR"
    ;; 3.9 VERSION Command
    "VERSION")
  "Keywords provided in the GNU LD script language.")

(defconst ld-script-builtins
  '("ABSOLUTE"
    "ADDR"
    "ALIGN"
    "BLOCK"
    "DATA_SEGMENT_ALIGN"
    "DATA_SEGMENT_END"
    "DATA_SEGMENT_RELRO_END"
    "DEFINED"
    "LENGTH" "len" "l"
    "LOADADDR"
    "MAX"
    "MIN"
    "NEXT"
    "ORIGIN" "org" "o"
    "SEGMENT_START"
    "SIZEOF"
    "SIZEOF_HEADERS"
    "sizeof_headers")
  "Builtin functions of GNU ld script.")

(defconst ld--section-regexp "\\.\\w*"
  "Regular expression to match sections, which look like `.text` or `.bss`.")

(defconst ld--wildcard-section-regexp
  (concat "\\(\\*\\)(" ld--section-regexp ")"))

(defconst ld--comment-start "/\\*")
(defconst ld--comment-end "\\*/")

(defconst ld--comment-regexp (concat ld--comment-start "\\(.*\\)" ld--comment-end) )

(defconst ld--hex-address-regexp "0x[[:digit:]]+")

(defun ld--word (str)
  "Wrap STR with \< and \> to make it break on word boundaries."
  (concat "\\<" str "\\>"))

(defvar ld-font-lock-keywords
  (append
   `((,(ld--word (regexp-opt ld--keywords t)) . font-lock-keyword-face)
     (,(ld--word (regexp-opt ld-script-builtins t)) . font-lock-builtin-face)
     (,ld--wildcard-section-regexp 1 font-lock-variable-name-face)
     (,ld--section-regexp . font-lock-variable-name-face)
     (,(ld--word ld--hex-address-regexp) . font-lock-constant-face)
     ("/DISCARD/\\|EXCLUDE_FILE\\|:NONE" . font-lock-warning-face)
     )
   )
  )

(defvar ld-mode-syntax-table
  (let ((st (make-syntax-table)))
    ; Comments are like /* */
    (modify-syntax-entry ?/ ". 14" st)
    (modify-syntax-entry ?* ". 23" st)
    st))

(defgroup ld nil
  "Major mode for editing LD scripts"
  :group 'languages)

(defcustom ld-mode-indent-width 4
  "Number of columns to indent in ld-mode."
  :type 'integer
  :group 'ld)

(defun ld--in-comment ()
  "Is the current point within a comment?"
  (nth 8 (syntax-ppss)))

(defun ld--desired-indentation ()
  "Compute the desired indentation level at point."
  ; BUG: { or } characters inside comments will confuse this
  (save-excursion
    (back-to-indentation)
    (let ((indent-to nil))
      (cond
       ((bobp)
        (setq indent-to 0))
       ((ld--in-comment))
       ((looking-at ".*{.*")
        (forward-line -1)
        (setq indent-to (current-indentation)))
       ((looking-at ".*}.*")
        (forward-line -1)
        (setq indent-to (- (current-indentation) ld-mode-indent-width))))
      (while (not indent-to)
        (back-to-indentation)
        (cond
         ((bobp)
          (setq indent-to 0))
         ((ld--in-comment)
          (forward-line -1))
         ((looking-at ".*}.*")
          (forward-line -1)
          (setq indent-to (- (current-indentation) ld-mode-indent-width)))
         ((looking-at ".*{.*")
          (forward-line -1)
          (setq indent-to (+ (current-indentation) ld-mode-indent-width)))
         (t
          (forward-line -1))))
      indent-to)))

(defun ld-indent-line ()
  "Indent line of LD script."
  (interactive)
  (indent-line-to (ld--desired-indentation)))

(defun ld-mode ()
  "Major mode for editing LD script files."
  (interactive)
  (kill-all-local-variables)

  ;; Syntax
  (set-syntax-table ld-mode-syntax-table)
  
  ;; Font lock
  (set (make-local-variable 'font-lock-defaults) '(ld-font-lock-keywords))

  ;; Indentation
  (set (make-local-variable 'indent-line-function) 'ld-indent-line)

  ;; Comments
  (set (make-local-variable 'comment-start) "/*")
  (set (make-local-variable 'comment-end) "*/")
  (set (make-local-variable 'comment-use-syntax) t)
  
  (setq major-mode 'ld-mode)
  (setq mode-name "LD2")
  (run-hooks 'ld-mode-hook))

;;;###autoload
(add-to-list 'auto-mode-alist (cons "\\.ld\\'" 'ld-mode))

(provide 'ld-mode)

;;; ld-mode.el ends here
