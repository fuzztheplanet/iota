;;; export.el --- batch HTML export of the iota docs  -*- lexical-binding: t -*-

;; The Makefile loads this before exporting each page.  The HTML is
;; checked in and served by GitHub Pages, so the same .org file has to
;; give the same .html every time.

(require 'subr-x)
(require 'ox-html)

;; Prism highlights source blocks in the browser.
(setq org-html-htmlize-output-type nil)

;; Org gives every heading a random id.  Use the heading text instead,
;; so a link to a section is readable and survives a rebuild.
(defun iota-docs--slug (title)
  (let ((slug (string-trim (replace-regexp-in-string
                            "[^a-z0-9]+" "-" (downcase title))
                           "-+" "-+")))
    (if (string-empty-p slug) "section" slug)))

(defun iota-docs--reference (orig datum info)
  (if (not (eq (org-element-type datum) 'headline))
      (funcall orig datum info)
    (let ((cache (plist-get info :internal-references)))
      (or (car (rassq datum cache))
          (let* ((base (iota-docs--slug
                        (org-element-property :raw-value datum)))
                 (ref base)
                 (n 1))
            (while (assoc ref cache)
              (setq n (1+ n)
                    ref (format "%s-%d" base n)))
            (plist-put info :internal-references (cons (cons ref datum) cache))
            ref)))))

(advice-add 'org-export-get-reference :around #'iota-docs--reference)

;; The few other elements that get an id, such as example blocks, still
;; go through `random'.  A fixed seed keeps those stable too.
(random "iota-docs")

;;; export.el ends here
