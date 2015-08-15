;;; ork.el --- edit keymaps as Org tables

;; Copyright (C) 2012-2015  Jonas Bernoulli

;; Author: Jonas Bernoulli <jonas@bernoul.li>
;; Package-Requires: ((emacs "25.0.50") (keymap-utils "0.5.1") (org "8.3.1"))
;; Homepage: https://github.com/tarsius/ork
;; Keywords: bindings org

;; This file is not part of Org.
;; This file is not part of GNU Emacs.

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; For a full copy of the GNU General Public License
;; see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Edit keymaps as Org tables.

;; This package is not quite ready for use.  I might at some point
;; work on it again, but right now I am busy doing other things.
;; It's a nice proof-of-concept though.

;;; Code:

(require 'cl-lib)
(require 'keymap-utils)
(require 'org)
(require 'org-element)
(require 'org-table)

;;; Options

(defgroup ork nil
  "Edit keymaps as Org tables."
  :group 'local)

(defconst ork-link-format "[[help:%s][%s]]")

(defconst ork-key-marker "@")
(defconst ork-disabled-key-marker "$")

(defface ork-key
  '((t :foreground "#afd8af" :background "#4f4f4f"))
  "Face used for active keys."
  :group 'ork)

(defface ork-shadowed-key
  '((t :foreground "#cc9393" :background "#4f4f4f"))
  "Face used for shadowed keys."
  :group 'ork)

;; TODO implement help link-type
(org-add-link-type "help" 'org-elisp-symbol-open)

;;; Commands

(defun ork-update-custom-bindings ()
  "Update bindings defined in the keymap variable section at point.
Update and evaluate the keymap's `kmu-define-bindings' form."
  (interactive)
  (ork-tangle-custom-bindings)
  (with-current-buffer
      (find-file-noselect
       (concat (file-name-sans-extension buffer-file-name) ".el"))
    (eval-last-sexp nil)))

(defun ork-tangle-custom-bindings ()
  "Tangle bindings defined in the keymap variable section at point.
Update and return the keymap's `kmu-define-bindings' form but
don't evaluate it."
  (interactive)
  (save-excursion
    (when (ork-goto-mapvar-table)
      (ork-goto-mapvar)
      (org-toggle-tag "disabled" 'off)
      (ork-save-kmu-define-keys))))

(defun ork-save-kmu-define-keys ()
  (ork-save-kmu-define-keys-1
   (concat (file-name-sans-extension buffer-file-name) ".el")
   (ork-entry-get-symbol "mapvar")
   (ork-entry-get-symbol "feature")
   (ork--orgtbl->diff-descs)))

(defun ork-save-kmu-define-keys-1 (file mapvar feature bindings)
  (with-current-buffer (find-file-noselect file)
    (widen)
    (if (re-search-forward (format "^(kmu-define-keys %s "
                                   (ork-entry-get-symbol "mapvar"))
                           nil t)
        (progn (beginning-of-line)
               (delete-region (point) (progn (end-of-defun) (point))))
      (goto-char (point-max)))
    (when bindings
      (insert (format "(kmu-define-keys %s %s" mapvar feature))
      (--each bindings
        (insert (format "\n  %S %s" (car it) (cadr it))))
      (insert ")\n"))))

(defun ork-tangle-all-custom-bindings (&optional purge)
  "Tangle bindings of all keymap variables in the current buffer.
Update all `kmu-define-bindings' forms for all keymap variables
customized and enabled in the current buffer.  But do not
evaluate these forms."
  (interactive)
  (org-with-wide-buffer
   (org-element-map
       (org-element-parse-buffer 'greater-element)
       'headline
     (lambda (h)
       (-when-let (mapvar (org-element-property :MAPVAR h))
         (condition-case nil
             (save-excursion
               (goto-char (org-element-property :begin h))
               (unless (member "disabled" (org-get-tags))
                 (message "Tangling %s ..." mapvar)
                 (ork-tangle-custom-bindings)
                 (message "Tangling %s...done" mapvar)))
           (error
            (message "Tangling %s...failed" mapvar))))))))

(defun ork-disable-custom-bindings ()
  "Disable custom bindings in keymap variable section at point.
Remove the keymap's `kmu-define-bindings' form to disable it for
future sections.  To also undo the custom bindings in the current
sessions use `ork-revert-custom-bindings' instead."
  (interactive)
  (save-excursion
    (ork-goto-mapvar-table)
    (ork-goto-mapvar)
    (org-toggle-tag "disabled" 'on)
    (ork-save-kmu-define-keys-1
     (concat (file-name-sans-extension buffer-file-name) ".el")
     (ork-entry-get-symbol "mapvar")
     nil nil)))

(defun ork-revert-custom-bindings ()
  "Restore the vanilla bindings of the keymap variable at point.
Also disable custom bindings for future sessions."
  (interactive)
  (ork-disable-custom-bindings)
  (kmu-restore-vanilla-keymap (ork-mapvar-at-point)))

(defun ork-insert-mapvar-section (mapvar)
  "Insert a keymap variable section for MAPVAR at point."
  (interactive (list (kmu-read-mapvar "Insert mapvar: ")))
  (let ((name (symbol-name mapvar)))
    (org-insert-heading)
    (org-todo "TODO")
    (insert name)
    (org-set-property "mapvar" name)
    (outline-show-entry)
    (org-toggle-tag "disabled" 'on)
    (re-search-forward org-property-end-re)
    (ork-update-mapvar-section 'insert)))

(cl-defun ork-update-mapvar-section (&optional (action 'update))
  "Update the keymap variable section at point.
Update the mapvar properties and table."
  (interactive)
  (org-back-to-heading)
  (re-search-forward org-property-end-re)
  (let ((mapvar (ork-entry-get-symbol "mapvar")))
    (if (eq action 'insert)
        (org-create-dblock (list :name (symbol-name mapvar)))
      (forward-line))
    (ork-update-mapvar-dblock action)))

(defun ork-insert-key ()
  "Insert a key sequence at point."
  (interactive)
  (let (sequence tail)
    (while (setq tail (read-char-exclusive
                       (if sequence
                           (kmu-key-description sequence)
                         "Insert key sequence (timeout after 2 seconds): ")
                       nil 2))
      (setq sequence (vconcat sequence (list tail))))
    (when sequence
      (insert (ork--format-key (kmu-key-description sequence)))
      (org-table-align))))

(defun ork-toggle-key ()
  "Toggle key at point between active and shadowed state."
  (interactive)
  (if (org-in-regexp org-emph-re)
      (-when-let (marker (cond ((equal (match-string 3) "@") "$")
                               ((equal (match-string 3) "$") "@")))
        (replace-match (concat marker (match-string 4) marker) t t nil 2))
    (error "No key at point")))

(cl-defun ork-goto-mapvar (&optional (mapvar (ork-mapvar-at-point)))
  "Jump to the section of MAPVAR."
  (interactive (list (kmu-read-mapvar "Goto mapvar: ")))
  (if (org-element-map
       (org-element-parse-buffer 'greater-element)
       'headline
       (lambda (h)
         (when (equal (org-element-property :MAPVAR h)
                      (symbol-name mapvar))
           (goto-char (org-element-property :begin h))))
       nil t)
      (progn (org-show-context)
             (org-show-entry)
             (point))
    (error "%s cannot be found" mapvar)))

(defun ork-goto-mapvar-table ()
  "Jump to the table of the section at point."
  (interactive)
  (org-reveal)
  (org-back-to-heading)
  (when (re-search-forward org-dblock-start-re nil t)
    (beginning-of-line)
    (forward-line)
    (point)))

(defun ork-find-keymap-variable ()
  "Find the definition of the keymap variable at point."
  (interactive)
  (find-variable-other-window (ork-mapvar-at-point)))

(defun ork-describe-keymap-variable ()
  "Display documentation of the keymap variable at point."
  (interactive)
  (describe-variable (ork-mapvar-at-point)))

(defun ork-describe-keymap-parent ()
  "Display documentation of the parent of the keymap variable at point."
  (interactive)
  (--if-let (ork-entry-get-symbol "parmap")
      (describe-variable it)
    (error "No parent keymap")))

(defun ork-describe-keymap-mode ()
  "Display documentation of the mode of the keymap variable at point."
  (interactive)
  (--if-let (ork-entry-get-symbol "mode")
      (describe-function it)
    (error "No mode")))

;;; Mode

(magit-define-popup ork-popup
  "Popup console for ork."
  'ork
  :actions '((?e "Update custom bindings"     ork-update-custom-bindings)
             (?t "Tangle custom bindings"     ork-tangle-custom-bindings)
             (?a "Tangle all custom bindings" ork-tangle-all-custom-bindings)
             (?d "Disable custom bindings"    ork-disable-custom-bindings)
             (?r "Revert custom bindings"     ork-revert-custom-bindings)
             (?i "Insert mapvar section"      ork-insert-mapvar-section)
             (?u "Update mapvar section"      ork-update-mapvar-section)
             (?I "Insert key"                 ork-insert-key)
             (?T "Toggle key"                 ork-toggle-key)
             (?G "Goto mapvar"                ork-goto-mapvar)
             (?g "Goto mapvar table"          ork-goto-mapvar-table)
             (?F "Goto keymap variable"       ork-find-keymap-variable)
             (?v "Describe variable"          ork-describe-keymap-variable)
             (?p "Describe parent"            ork-describe-keymap-parent)
             (?m "Describe mode"              ork-describe-keymap-mode))
  :max-action-columns 1)

(defvar ork-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") 'ork-popup)
    (define-key map (kbd "C-c C-e") 'ork-update-custom-bindings)
    (define-key map (kbd "C-c C-t") 'ork-tangle-custom-bindings)
    (define-key map (kbd "C-c C-a") 'ork-tangle-all-custom-bindings)
    (define-key map (kbd "C-c C-d") 'ork-disable-custom-bindings)
    (define-key map (kbd "C-c C-r") 'ork-revert-custom-bindings)
    (define-key map (kbd "C-c C-i") 'ork-insert-mapvar-section)
    (define-key map (kbd "C-c C-u") 'ork-update-mapvar-section)
    (define-key map (kbd "C-c i")   'ork-insert-key)
    (define-key map (kbd "C-c t")   'ork-toggle-key)
    (define-key map (kbd "C-c g")   'ork-goto-mapvar)
    (define-key map (kbd "C-c C-g") 'ork-goto-mapvar-table)
    (define-key map (kbd "C-c f")   'ork-find-keymap-variable)
    (define-key map (kbd "C-c d v") 'ork-describe-keymap-variable)
    (define-key map (kbd "C-c d p") 'ork-describe-keymap-parent)
    (define-key map (kbd "C-c d m") 'ork-describe-keymap-mode)
    map)
  "Keymap for Ork mode.")

(defvar ork-emphasis-alist
  (list (list ork-key-marker
              'ork-key
              "<code>" "</code>")
        (list ork-disabled-key-marker
              'ork-shadowed-key
              "<code>" "</code>"))
  "Additional emphasis markers for Ork mode.")

(defvar ork-mode-lighter " Ork"
  "Mode lighter for Ork mode.")

;;;###autoload
(define-minor-mode ork-mode
  "Minor mode for editing Org files containing keymap variable tables."
  :keymap ork-mode-map
  :lighter ork-mode-lighter
  (cond (ork-mode
         (setq-local indent-tabs-mode nil)
         (setq-local org-adapt-indentation nil)
         (setq-local org-hierarchical-todo-statistics nil)
         (setq-local org-hide-emphasis-markers t)
         (setq-local org-emphasis-alist
                     (append ork-emphasis-alist org-emphasis-alist))
         (make-local-variable 'org-emph-re)
         (make-local-variable 'org-verbatim-re)
         (make-local-variable 'org-emphasis-regexp-components)
         (setf (caddr org-emphasis-regexp-components) " \t\r\n")
         (org-set-emph-re 'org-emphasis-regexp-components
                          org-emphasis-regexp-components))
        (t
         (kill-local-variable 'indent-tabs-mode)
         (kill-local-variable 'org-adapt-indentation)
         (kill-local-variable 'org-hierarchical-todo-statistics)
         (kill-local-variable 'org-hide-emphasis-markers)
         (kill-local-variable 'org-emphasis-alist)
         (kill-local-variable 'org-emph-re)
         (kill-local-variable 'org-verbatim-re)
         (kill-local-variable 'org-emphasis-regexp-components)))
  (font-lock-flush))

;;; Utilities

(defun ork-entry-get-symbol (property &optional inherit)
  "Return value of PROPERTY as a symbol."
  (let ((v (org-entry-get nil property inherit)))
    (and v (intern v))))

(defun ork-entry-get-symbol-value (property &optional inherit)
  "Return value of the variable that is the value of PROPERTY."
  (let ((v (ork-entry-get-symbol property inherit)))
    (and v (symbol-value v))))

(defun ork-mapvar-at-point ()
  "Return keymap variable at point."
  (let ((v (ork-entry-get-symbol "mapvar")))
    (or  v (error "No mapvar at point"))))

;;; Orgtbl -> X

(defun ork--orgtbl->diff-descs ()
  (mapcar (lambda (b) (list (kmu-key-description (car b)) (cadr b)))
          (ork--orgtbl->diff-vectors)))

(defun ork--orgtbl->diff-vectors ()
  (let (define remove)
    (dolist (row (ork--orgtbl->table))
      (unless (eq row 'hline)
        (cl-destructuring-bind (ckeys vkeys1 vkeys2 def) row
          (when (and (symbolp def)
                     (listp ckeys))
            (dolist (ckey ckeys)
              (when (cdr ckey)
                (push (list (car ckey) def) define))))
          (dolist (vkey (append (and (listp vkeys1) vkeys1)
                                (and (listp vkeys2) vkeys2)))
            ;; TODO split ranges into individual events
            (unless (cdr vkey)
              (push (car vkey) remove))))))
    (nconc (cl-mapcan (lambda (r)
                        (unless (assoc r define)
                          (list (list r :remove))))
                      (nreverse remove))
           (nreverse define))))

(defun ork--orgtbl->table ()
  (mapcar
   (lambda (row)
     (if (symbolp row)
         row
       (cl-destructuring-bind (ckeys vkeys1 vkeys2 def) row
         (list
          (or (ork--parse-keys ckeys)  (unless (equal ckeys  "") ckeys))
          (or (ork--parse-keys vkeys1) (unless (equal vkeys1 "") vkeys1))
          (or (ork--parse-keys vkeys2) (unless (equal vkeys2 "") vkeys2))
          (or (ork--parse-def  def)    (unless (equal def    "") def))
          ))))
   (ork--orgtbl->orgtbl)))

(defun ork--orgtbl->orgtbl ()
  (mapcar
   (lambda (row)
     (if (string-match org-table-hline-regexp row)
         'hline
       (org-split-string (org-trim row) "\\s-*|\\s-*")))
   (org-split-string (buffer-substring-no-properties
                      (org-table-begin)
                      (org-table-end))
                     "[ \t]*\n[ \t]*")))

(defun ork--parse-keys (string)
  (let ((key-re org-emph-re)
        keys)
    (unless (equal string "")
      (with-temp-buffer
        (insert string)
        (goto-char (point-min))
        (while (re-search-forward key-re nil t)
          (cond ((equal (match-string 3) ork-key-marker)
                 (push (cons (kmu-parse-key-description (match-string 4) t)
                             t) keys))
                ((equal (match-string 3) ork-disabled-key-marker)
                 (push (cons (kmu-parse-key-description (match-string 4) t)
                             nil) keys)))
          (backward-char)))
      (nreverse keys))))

(defun ork--parse-def (string)
  (unless (equal string "")
    (with-temp-buffer
      (insert string)
      (goto-char (point-min))
      (when (and (re-search-forward org-bracket-link-analytic-regexp nil t)
                 (equal (match-string 2) "help"))
        (intern (match-string 5))))))

;;; Rules

(defun ork--default-rules ()
  (save-excursion
    (save-restriction
      (widen)
      (ork-goto-mapvar 'template)
      (ork-goto-mapvar-table)
      (ork--orgtbl->table))))

(defun ork--default-key-order ()
  (ork--rules->key-order (ork--default-rules)))

(defun ork--default-replace ()
  (ork--rules->replace (ork--default-rules)))

(defun ork--rules->key-order (rules)
  (--mapcat (when (listp it)
              (let* ((ckey (car it))
                     (vkey (cadr it))
                     (okey (or (and (listp vkey) (vectorp (caar vkey)) vkey)
                               (and (listp ckey) (vectorp (caar ckey)) ckey))))
                (when okey
                  (list (aref (caar okey) 0)))))
            rules))

(defun ork--rules->replace (rules)
  (--mapcat (when (listp it)
              (let ((ckey (car it))
                    (vkey (cadr it)))
                (and (listp vkey)
                     (vectorp (caar vkey))
                     (list (cons (aref (caar vkey) 0)
                                 (and (listp ckey)
                                      (vectorp (caar ckey))
                                      (aref (caar ckey) 0)))))))
            rules))

;;; X -> Orgtbl

(defun ork--keymap->orgtbl (keymap)
  (--map (list nil
               (ork--format-key      (cadr it))
               (ork--format-key-cell (cddr it))
               (ork--format-def      (car  it)))
         (kmu-keymap-definitions keymap)))

(defun ork--keymap+rules->orgtbl (keymap rules)
  (let ((binds (kmu-keymap-definitions keymap))
        group)
    (nconc
     (--mapcat (cond ((eq it 'hline)
                      (if (memq 'hline group)
                          (setq group (list 'hline))
                        (setq group (nconc group (list 'hline))))
                      nil)
                     ((stringp (car it))
                      (setq group (nconc group (list it)))
                      nil)
                     (t
                      (let* ((ckey (car (car  it)))
                             (vkey (car (cadr it)))
                             (bind (rassoc (list (car vkey)) binds))
                             (def  (car bind)))
                        (when def
                          (setq binds (delete bind binds))
                          (nconc (when group
                                   (prog1 group
                                     (setq group nil)))
                                 (list (ork--format-row (list ckey)
                                                        (list vkey)
                                                        nil def)))))))
               rules)
     (when binds
       (cons 'hline
             (--map (ork--format-row nil
                                     (list (cons (cadr it) t))
                                     (--map (cons it t) (cddr it))
                                     (car it))
                    (cl-sort binds 'string<
                             :key (lambda (b) (kmu-key-description (cadr b))))))))))

(defun ork--keymap+defaults->orgtbl (keymap)
  (ork--keymap+rules->orgtbl keymap (ork--default-rules)))

(defun ork--sort-top-keys (order defs)
  (--mapcat (ork--sort-keys order it) defs))

(defun ork--sort-keys (order keys)
  (sort keys (apply-partially #'ork--sort-keys-predicate order)))

(defun ork--sort-keys-predicate (order a b)
  (let ((la (--map (length (memq it order)) a))
        (lb (--map (length (memq it order)) b))
        (ret nil))
    (while (or la lb)
      (cond ((not la)              (setq ret t   la nil lb nil))
            ((not lb)              (setq ret nil la nil lb nil))
            ((> (car la) (car lb)) (setq ret t   la nil lb nil))
            ((< (car la) (car lb)) (setq ret nil la nil lb nil))
            ((= (car la) (car lb)) (setq la (cdr la)
                                         lb (cdr lb)))))
    ret))

(defun ork--format-row (&rest args)
  (ork--format-binding args))

(defun ork--format-binding (bind)
  (cl-destructuring-bind (ckeys vkeys1 vkeys2 def) bind
    (list (ork--format-key-cell (delq nil ckeys))
          (ork--format-key-cell (delq nil vkeys1))
          (ork--format-key-cell (delq nil vkeys2))
          (ork--format-def-cell ; FIXME
           def (not (or ckeys (rassq t (nconc vkeys1 vkeys2))))))))

(defun ork--format-key-cell (keys)
  (if (stringp keys)
      keys
    (setq keys (--map (ork--format-key (car it) (not (cdr it))) keys))
    (when keys
      (mapconcat 'identity keys " "))))

(defun ork--format-key (key &optional shadowedp)
  (setq key (kmu-key-description key))
  (let ((marker (if shadowedp
                    ork-disabled-key-marker
                  ork-key-marker)))
    (concat marker
            (replace-regexp-in-string "|" "¦" key) ; "quote" for table
            marker)))

(defun ork--format-def-cell (def &optional shadowedp)
  (and def (ork--format-def def shadowedp)))

(defun ork--format-def (def &optional shadowedp)
  (concat (and shadowedp "(")
          (cond ((not def)
                 "nil")
                ((symbolp def)
                 (format ork-link-format def def))
                ((listp def)
                 (cond ((eq 'lambda (car def))
                        "#<lambda>")
                       ((eq 'closure (car def))
                        "#<closure>")
                       ((eq 'keymap (car def))
                        "#<keymap>")
                       ((eq 'menu-item (car def))
                        (format "#<menu-item %S>" (cadr def)))
                       ((and (stringp (car def))
                             (eq (cadr def) 'keymap))
                        (format "#<menu-keymap %s>" (car def)))
                       (t
                        "#<unknown>")))
                ((keymapp def)
                 "#<keymap>")
                ;; TODO vectorp
                (t
                 "#<compiled-function>"))
          (and shadowedp ")")))

(defun ork-update-mapvar-dblock (action)
  "Update the mapvar table dblock at point.
This empties the block and then inserts the table.

Standard `org-update-dblock' cannot be used because it calls a
function whose name is constructed from the dblock name but the
same function `ork-dblock-write-mapvar-table' can be used to
write any mapvar table.  Most of the code is copied from
`org-update-dblock'.

Optional ACTION controls what is used as input for the created
table."
  (save-window-excursion
    (let* ((pos    (point))
           (line   (org-current-line))
           (params (ork-prepare-mapvar-dblock))
           (name   (plist-get params :name))
           (mapvar (plist-get params :mapvar))
           (indent (plist-get params :indentation-column)))
      (message "Updating dynamic block `%s' at line %d..." name line)
      (ork-dblock-write-mapvar-table
       (nconc (list :action action) params))
      (message "Updating dynamic block `%s' at line %d...done" name line)
      (goto-char pos)
      (when (and indent (> indent 0))
        (setq indent (make-string indent ?\s))
        (save-excursion
          (org-beginning-of-dblock)
          (forward-line 1)
          (while (not (looking-at org-dblock-end-re))
            (insert indent)
            (beginning-of-line 2))
          (when (looking-at org-dblock-end-re)
            (and (looking-at "[ \t]+")
                 (replace-match ""))
            (insert indent)))))))

(defun ork-prepare-mapvar-dblock ()
  "Prepare the mapvar table dblock at point for refresh.
This empties the block, puts the cursor at the insert position
and returns the mapvar property list.

This is a wrapper around standard `org-refresh-dblock', which
cannot be used on it's own because the mapvar properties have
to be inherited from the section heading."
  (append (org-prepare-dblock)
          (save-excursion
            (org-back-to-heading)
            (list :mapvar (ork-entry-get-symbol "mapvar")))))

(defun ork-dblock-write-mapvar-table (params)
  (let ((mapvar (plist-get params :mapvar))
        (action (plist-get params :action)))
    (insert (orgtbl-to-orgtbl
             (pcase action
               (`insert  (ork--keymap+defaults->orgtbl
                          (kmu-vanilla-keymap mapvar)))
               (`update  (ork--keymap+rules->orgtbl
                          (symbol-value mapvar)
                          (ork--orgtbl->table)))
               (`arrange (error "Not implemented")))
             nil)))
  (org-table-align))

(provide 'ork)
;; Local Variables:
;; indent-tabs-mode: nil
;; End:
;;; ork.el ends here
