;;; tex2html.el --- routines for generating html from latex  -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Paul D. Nelson

;; Author: Paul D. Nelson <nelson.paul.david@gmail.com>
;; Keywords: tex

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'subr-x)
(require 'ox-html)

(defcustom tex2html-config-file "config.json"
  "JSON configuration file for the TeX notes site."
  :type 'string
  :group 'tex2html)

(defun tex2html-read-config (&optional directory)
  "Read `tex2html-config-file' from DIRECTORY as an alist.
Return nil when the file does not exist."
  (let ((file (expand-file-name tex2html-config-file
                                (or directory default-directory)))
        (json-object-type 'alist)
        (json-array-type 'list)
        (json-key-type 'string))
    (when (file-exists-p file)
      (json-read-file file))))

(defun tex2html-config-get (keys &optional default directory)
  "Return nested config value at KEYS, or DEFAULT.
KEYS is a list of string keys read from `tex2html-config-file'."
  (let ((value (tex2html-read-config directory)))
    (while (and keys value)
      (setq value (alist-get (pop keys) value nil nil #'string=)))
    (or value default)))

(defun tex2html-exclude-from-config (&optional directory)
  "Read and parse the `exclude' field from config.json in DIRECTORY."
  (tex2html-config-get '("exclude") nil directory))

(defun tex2html--excluded-file-p (filename exclude-list)
  "Return non-nil when FILENAME is named by EXCLUDE-LIST.
EXCLUDE-LIST may contain either `foo.tex' or `foo'."
  (let ((base (file-name-nondirectory filename))
        (stem (file-name-sans-extension
               (file-name-nondirectory filename))))
    (or (member base exclude-list)
        (member stem exclude-list))))

(defun tex2html-buildable-tex-files (&optional directory)
  "Return tracked standalone top-level TeX files in DIRECTORY."
  (let* ((default-directory (or directory default-directory))
         (exclude-list (tex2html-exclude-from-config default-directory))
         (files (split-string
                 (shell-command-to-string "git ls-files -- '*.tex'")
                 "\n" t)))
    (cl-remove-if
     (lambda (file)
       (or (string-match-p "/" file)
           (tex2html--excluded-file-p file exclude-list)))
     files)))

(defun tex2html-buildable-org-files (&optional directory)
  "Return tracked standalone top-level Org files in DIRECTORY.
`index.org' is reserved for the site index template."
  (let* ((default-directory (or directory default-directory))
         (exclude-list (tex2html-exclude-from-config default-directory))
         (files (split-string
                 (shell-command-to-string "git ls-files -- '*.org'")
                 "\n" t)))
    (cl-remove-if
     (lambda (file)
       (or (string-match-p "/" file)
           (string= (file-name-nondirectory file) "index.org")
           (tex2html--excluded-file-p file exclude-list)))
     files)))

(defcustom tex2html-theorem-names
  '("theorem" "lemma" "proposition" "corollary" "conjecture"
    "definition" "example" "remark" "exercise" "problem" "question"
    "solution" "note" "remark" "notation" "assumption" "hypothesis"
    "claim" "summary" "answer" "criterion" "summary")
  "List of theorem-like environments."
  :type '(repeat string)
  :group 'tex2html)

(defun label-number-hash-table (aux-file)
  "Return a hash table of label numbers from the given aux-file."
  (let ((hash (make-hash-table :test 'equal)))
    (when (file-exists-p aux-file)
      (with-temp-buffer
        (insert-file-contents aux-file)
        (goto-char (point-min))
        (while (re-search-forward
                "\\\\newlabel{\\([^}]+\\)}.*?{{\\([^}]+\\)}" nil t)
          (let ((label (match-string 1))
                (number (match-string 2)))
            (puthash label number hash)))))
    hash))


(defun tex2html-collect-external-documents-in-tex-buffer ()
  "Collect external documents referenced in current TeX buffer.
Return a list of plists with keys `:prefix', `:stem', `:tex', and
`:aux'.  Missing aux files are reported as warnings, not errors:
conversion should still produce visible `??' markers and CI should
report the dangling dependency."
  (save-restriction
    (widen)
    (save-excursion
      (goto-char (point-min))
      (let ((documents nil))
        (while (re-search-forward
                "^\\s-*\\\\externaldocument\\(?:\\[\\([^]]+\\)\\]\\)?{\\([^}]+\\)}" nil t)
          (let* ((prefix (or (match-string 1) ""))
                 (stem (match-string 2))
                 (aux-file (concat stem ".aux"))
                 (tex-file (concat stem ".tex")))
            (unless (file-exists-p aux-file)
              (message "Warning: external .aux file %s does not exist" aux-file))
            (push (list :prefix prefix
                        :stem stem
                        :tex tex-file
                        :aux aux-file)
                  documents)))
        documents))))

(defun tex2html-collect-external-auxfiles-in-tex-buffer ()
  "Collect names of .aux files referenced in current TeX buffer.
This compatibility wrapper keeps the old return shape.  Missing aux files
are included and later treated as empty label tables."
  (mapcar
   (lambda (document) (plist-get document :aux))
   (tex2html-collect-external-documents-in-tex-buffer)))

(defun tex2html--external-document-from-auxfile (auxfile)
  "Construct an external-document plist from AUXFILE."
  (let ((stem (file-name-sans-extension auxfile)))
    (list :prefix ""
          :stem stem
          :tex (concat stem ".tex")
          :aux auxfile)))

(defvar tex2html-scripts)

(defun tex2html-postprocess-make-proof-links-toggleable ()
  "Make proof links toggleable."
  (interactive)
  (goto-char (point-min))
  (when (re-search-forward "</body>" nil t)
    (replace-match (concat tex2html-scripts "\n</body>")))
  (goto-char (point-min))
  (while (re-search-forward
          "<div class=\"proof\">[\n\r[:blank:]]*<p>\\(<em>\\(.*?\\)</em>\\)"
          nil t)
    (save-match-data
      (search-backward "<div" nil t)
      (sgml-skip-tag-forward 1)
      (replace-match "</span></div>"))
    (let* ((proof-text (match-string 2))
           (folded-text (concat proof-text " (...)"))
           (replacement
            (format
             (concat
              "<div class=\"proof\"><p>\n"
              "<a href=\"#\" class=\"toggle-proof\">"
              "<em data-default-text=\"%s\" data-folded-text=\"%s\">"
              "%s</em></a>\n"
              "<span class=\"proof-content\">")
             proof-text folded-text proof-text)))
      (replace-match replacement))
    ;; (re-search-forward "</p>\n</div>")
    ;; (replace-match "</span>\n</div>")
    ))

(defun tex2html-postprocess-html-buffer (&optional auxfile external-auxfiles)
  "Update an HTML buffer with MathJax created using pandoc from a LaTeX
file."
  (interactive)
  (if (not auxfile)
      (setq auxfile (concat (file-name-sans-extension (buffer-file-name)) ".aux")))
  (let*
      ((external-documents
        (if external-auxfiles
            (mapcar #'tex2html--external-document-from-auxfile external-auxfiles)
          ;; open corresponding tex file and look for \externaldocument
          (with-temp-buffer
            (insert-file-contents (concat (file-name-sans-extension auxfile) ".tex"))
            (tex2html-collect-external-documents-in-tex-buffer))))
       (external-auxfiles
        (mapcar (lambda (document) (plist-get document :aux))
                external-documents))
       (label-number-hash (label-number-hash-table auxfile))
       ;; construct a list of pairs consisting of the html file
       ;; associated to an external aux file and the hash table for
       ;; that file
       (external-label-number-hash-list
        (mapcar
         (lambda (document)
           (let ((auxfile (plist-get document :aux)))
             (list :prefix (plist-get document :prefix)
                   :html (concat
                          (file-name-sans-extension auxfile)
                          ".html")
                   :labels (label-number-hash-table auxfile))))
         external-documents)))
    (save-excursion
      (goto-char (point-min))
      ;; Step 1: Handle \label{eqn:blah}
      (while (search-forward-regexp "\\\\label{\\([^}]+\\)}" nil t)
        (let* ((label (match-string 1))
               (tag-number (or (gethash label label-number-hash) "??")))
          (replace-match (concat "\\\\label{" label "}\\\\tag{" tag-number "}"))
          (save-excursion
            (if (search-backward-regexp "<span[[:space:]]+class=\"math display\"" nil t)
                (progn
                  (replace-match (concat "<span id=\"" label "\" class=\"math display\""))
                  t)
              (message "Warning: something went wrong"))))))
    ;; Step 2: Handle links with data-reference-type="ref" or "eqref"
    (goto-char (point-min))
    ;; (while (search-forward "<a" nil t)
    ;;   (when-let*
    ;;     ((beg (match-beginning 0))
    ;;      (end (save-excursion
    ;;       (goto-char beg)
    ;;       (sgml-skip-tag-forward 1)))
    ;;      (parsed (libxml-parse-xml-region beg end))
    ;;      (link (alist-get 'href parsed))
    ;;      (ref-type (alist-get 'data-reference-type parsed))
    ;;      (label (alist-get 'data-reference parsed)))
    ;;   (let (tag-number external-html)
    ;;     (unless (setq tag-number (gethash label label-number-hash))
    ;;       (cl-loop
    ;;        for (html-file . hash) in external-label-number-hash-list
    ;;        for number = (gethash label hash)
    ;;        when number
    ;;        do (setq tag-number number
    ;;           external-html html-file)))
    ;;     (let ((new-content
    ;;      (if (string= ref-type "ref")
    ;;          (concat "" tag-number "")
    ;;        (concat "\\((" tag-number ")\\)"))))
    ;;       (replace-match new-content t t nil 4)
    ;;       (if external-html
    ;;     (replace-match (concat external-html link) t t nil 1))))))

    (while (search-forward-regexp "href=\"\\([^\"]+\\)\"[^>]+data-reference-type=\"\\(ref\\|eqref\\)\"[^>]+data-reference=\"\\([^\"]+\\)\"[^>]*>\\([^<]+\\)</a>" nil t)
      (let* ((link (match-string 1))
             (ref-type (match-string 2))
             (label (match-string 3))
             tag-number external-html
             )
        (unless (setq tag-number (gethash label label-number-hash))
          (cl-loop
           for document in external-label-number-hash-list
           for prefix = (plist-get document :prefix)
           for html-file = (plist-get document :html)
           for hash = (plist-get document :labels)
           for external-label = (if (and (not (string-empty-p prefix))
                                         (string-prefix-p prefix label))
                                    (substring label (length prefix))
                                  label)
           for number = (and (or (string-empty-p prefix)
                                 (string-prefix-p prefix label))
                             (gethash external-label hash))
           when number
           do (setq tag-number number
                    external-html html-file)))
        (let* ((display-number (or tag-number "??"))
               (new-content
                (if (string= ref-type "ref")
                    (concat "" display-number "")
                  (concat "\\((" display-number ")\\)"))))
          (replace-match
           new-content
           ;; (if external-html
           ;;     (concat "["
           ;;          ;; take substring up to first underscore
           ;;          (let ((name (file-name-sans-extension external-html)))
           ;;      (substring name 0 (string-match "_" name)))
           ;;          ", " new-content
           ;;          "]")
           ;;   new-content)
           t t nil 4)
          (if external-html
              (replace-match (concat external-html link) t t nil 1)))))
    ;; Step 3: put braces around mathop's followed by subscripts and superscripts
    (goto-char (point-min))
    (while (re-search-forward "\\\\mathop" nil t)
      (when (save-match-data
              (forward-sexp 1)
              (when (looking-at "[_^]")
                (insert "}")
                t))
        (replace-match "{\\\\mathop")))


    ;; Step 4: add section numbering
    (goto-char (point-min))
    (while (re-search-forward "<h[1-9][[:space:]]id=\"\\([^\"]+\\)\">" nil t)
      (let ((contents (match-string 0))
            (label (match-string 1)))
        (when-let* ((number (gethash label label-number-hash)))
          (replace-match (concat contents "§" number ". ") t t nil 0))))

    ;; Step 5: bibliography links
    (goto-char (point-min))
    (while (search-forward-regexp "<\\(span\\)[[:space:]]+class=\"citation\"[[:space:]]+data-cites=\"\\([^\"]+\\)\">([^<]*)</\\(span\\)>" nil t)
      (let ((label (match-string 2)))
        (replace-match "a" t t nil 3)
        (replace-match (format "a href=\"#ref-%s\"" label) t t nil 1)))

    ;; Step 6: replace \begin{aligned} ... \end{aligned} with \begin{align} ... \end{align}
    (goto-char (point-min))
    (while (search-forward-regexp "\\\\begin{aligned}" nil t)
      (replace-match "\\begin{align}" t t nil 0))
    (goto-char (point-min))
    (while (search-forward-regexp "\\\\end{aligned}" nil t)
      (replace-match "\\end{align}" t t nil 0))

    ;; Step 7: adjust hyperlinks *.pdf -> *.html, where * is anything
    ;; that appears as *.aux inside external-auxfiles
    (goto-char (point-min))
    (let* ((auxfiles-base (mapcar 'file-name-sans-extension external-auxfiles)))
      (when auxfiles-base
        (let ((auxfiles-regexp (regexp-opt auxfiles-base)))
          ;; search for a href="blah.pdf" where blah is in auxfiles-base
          (while (re-search-forward (concat "href=\"\\(" auxfiles-regexp "\\.pdf\\)\"") nil t)
            (let* ((pdf-file (match-string 1))
                   (html-file (concat (file-name-sans-extension pdf-file) ".html")))
              (replace-match html-file t t nil 1))))))

    ;; Step 8: make proof environments toggleable
    (tex2html-postprocess-make-proof-links-toggleable)

    ;; Step 9: fix theorem numbering.
    ;; Disabling for now due to a bug in pandoc: https://github.com/jgm/pandoc/issues/8872
    ;; (goto-char (point-min))
    ;; (while (search-forward "<div" nil t)
    ;;   (let* ((opening-end (save-excursion
    ;;           (goto-char (match-beginning 0))
    ;;           (forward-sexp 1) (point)))
    ;;        (id (save-excursion
    ;;        (when (re-search-forward "id=\"\\([^\"]+\\)\"" opening-end t)
    ;;          (match-string 1))))
    ;;        (class (save-excursion
    ;;           (when (re-search-forward "class=\"\\([^\"]+\\)\"" opening-end t)
    ;;       (match-string 1))))
    ;;        (closing (save-excursion (search-forward "</div>" nil t) (point))))
    ;;   (when (member class tex2html-theorem-names)
    ;;     (let ((number
    ;;      (when id
    ;;        (concat " "
    ;;          (gethash id label-number-hash)))))
    ;;       (when (re-search-forward "<strong>\\([A-Za-z]+\\)\\( [0-9]+\\)</strong>"
    ;;              closing t)
    ;;         (replace-match (or number "") t t nil 2))))))
    ))

(defcustom giscus-comment-script
  ""
  "Fallback script to add comments to HTML files.
When config.json has site.commentsScript, that value takes precedence.
Leave empty to disable comments."
  :type 'string
  :group 'tex2html)

(defcustom tex2html-scripts
  "<script>
document.querySelectorAll(\".toggle-proof\").forEach(function(toggle) {
  toggle.addEventListener(\"click\", function(e) {
    e.preventDefault();
    const content = this.nextElementSibling;
    const em = this.querySelector('em');
    if (window.getComputedStyle(content).display === \"none\") {
      content.style.display = \"inline\";
      em.textContent = em.dataset.defaultText;
    } else {
      content.style.display = \"none\";
      em.textContent = em.dataset.foldedText;
    }
  });
});
</script>
<script>
document.querySelector(\"#toggle-all-proofs\").addEventListener(\"click\", function(e) {
  e.preventDefault();
  const proofs = document.querySelectorAll(\".proof-content\");
  proofs.forEach(function(proof) {
    const proofToggle = proof.previousElementSibling;
    if (window.getComputedStyle(proof).display === \"none\") {
      proof.style.display = \"inline\";
      proofToggle.innerHTML = `<em>${proofToggle.dataset.defaultText}</em>`;
    } else {
      proof.style.display = \"none\";
      proofToggle.innerHTML = `<em>${proofToggle.dataset.foldedText}</em>`;
    }
  });
});
</script>
"
  "Scripts to add to HTML files."
  :type 'string
  :group 'tex2html)

(defun tex2html-add-comment-script ()
  "Add script to HTML buffer that allows users to add comments."
  (interactive)
  (let ((script (or (tex2html-config-get '("site" "commentsScript"))
                    giscus-comment-script)))
    (when (and script (not (string-empty-p script)))
      (goto-char (point-min))
      (when (re-search-forward "</body>" nil t)
        (replace-match (concat script "\n</body>"))))))

(require 'sgml-mode)
(require 'dom)

;; make the html{} under <html> <head> <style> the following:
;; line-height: 1.5;
;; font-family: Georgia, serif;
;; font-size: 20px;
;; color: #1a1a1a;
;; background-color: #fdfdfd;
(defun tex2html-add-style-to-html-head ()
  "Add style to HTML buffer."
  (interactive)
  (goto-char (point-min))
  (when (re-search-forward "<style\\(?:[[:space:]][^>]*\\)?>" nil t)
    (beginning-of-line)
    (insert "  <link rel=\"stylesheet\" href=\"tex.css\">
")))

(defun tex2html-add-tex-pdf-links (&optional file-name)
  (interactive)
  (goto-char (point-min))
  (when-let* ((style-beg (re-search-forward "<style\\(?:[[:space:]][^>]*\\)?>" nil t))
              (body-beg (search-forward "<body>" nil t))
              (base-filename
               (file-name-nondirectory (file-name-sans-extension
                                        (or file-name
                                            (buffer-file-name))))))
    (goto-char body-beg)
    (let* ((repo (tex2html-config-get '("site" "githubRepository")))
           (branch (tex2html-config-get '("site" "sourceBranch") "main"))
           (history-link
            (if (and repo (not (string-empty-p repo)))
                (format
                 "      <a href=\"https://github.com/%s/commits/%s/%s.tex\" class=\"my-link\">history</a>\n"
                 repo branch base-filename)
              "")))
      (insert
       (format "
    <div class=\"my-links-container\">
%s
      <a href=\"%s.tex\" class=\"my-link\">tex</a>
      <a href=\"%s.pdf\" class=\"my-link\">pdf</a>
%s
      <a href=\".\" class=\"my-link\">home</a>
    </div>"
               (czm/format-git-time-string
                (shell-command-to-string
                 (concat "git log -1 --format=%aI -- "
                         (concat base-filename ".tex"))))
               base-filename
               base-filename
               history-link)))
    (goto-char style-beg)
      (insert "
      .my-links-container {
        position: absolute;
        top: 0;
        right: 0;
        padding-right: 20px;
        padding-top: 10px;
      }
      .my-link {
        margin-left: 10px;
      }
      .my-links-container-2 { /* new CSS class for the second row */
        position: absolute;
        top: 40px; /* adjust this value based on the height of your links */
        right: 0;
        padding-right: 20px;
      }
      .my-link-2 {
        margin-left: 10px;
      }
")))

(defun tex2html-add-org-links (&optional file-name)
  "Add source and home links to an Org-generated HTML buffer."
  (interactive)
  (goto-char (point-min))
  (when-let* ((style-beg (re-search-forward "<style\\(?:[[:space:]][^>]*\\)?>" nil t))
              (body-beg (search-forward "<body>" nil t))
              (base-filename
               (file-name-nondirectory (file-name-sans-extension
                                        (or file-name
                                            (buffer-file-name))))))
    (goto-char body-beg)
    (let* ((repo (tex2html-config-get '("site" "githubRepository")))
           (branch (tex2html-config-get '("site" "sourceBranch") "main"))
           (history-link
            (if (and repo (not (string-empty-p repo)))
                (format
                 "      <a href=\"https://github.com/%s/commits/%s/%s.org\" class=\"my-link\">history</a>\n"
                 repo branch base-filename)
              "")))
      (insert
       (format "
    <div class=\"my-links-container\">
%s
      <a href=\"%s.org\" class=\"my-link\">org</a>
%s
      <a href=\".\" class=\"my-link\">home</a>
    </div>"
               (czm/format-git-time-string
                (shell-command-to-string
                 (concat "git log -1 --format=%aI -- "
                         (concat base-filename ".org"))))
               base-filename
               history-link)))
    (goto-char style-beg)
    (insert "
      .my-links-container {
        position: absolute;
        top: 0;
        right: 0;
        padding-right: 20px;
        padding-top: 10px;
      }
      .my-link {
        margin-left: 10px;
      }
")))

(defun tex2html-convert-file (&optional filename out-dir out-filename)
  "Converts a LaTeX file to HTML using pandoc and applies postprocessing.
If no FILENAME is provided, uses the current buffer's file name. 
The output directory and output filename can be optionally specified."
  (interactive)
  ;; if no filename is provided, try to use current buffer's
  (unless filename
    (setq filename (or (buffer-file-name (buffer-base-buffer))
                       (buffer-file-name))))
  ;; if still no filename, error out
  (unless filename
    (error "No LaTeX file specified and current buffer does not seem to be associated with a file"))
  ;; check if filename is a .tex file
  (unless (string= (file-name-extension filename) "tex")
    (error "File is not a LaTeX (.tex) file"))
  ;; check .aux file
  (let* ((basename (file-name-sans-extension filename))
         (auxfile (concat basename ".aux"))
         (external-auxfiles
          (with-temp-buffer
            (insert-file-contents filename)
            (tex2html-collect-external-auxfiles-in-tex-buffer))
          ))
    (unless (file-exists-p auxfile)
      (error "Associated .aux file does not exist"))
    (let ((tex-mod-time (nth 5 (file-attributes filename)))
          (aux-mod-time (nth 5 (file-attributes auxfile))))
      (if (and aux-mod-time (time-less-p aux-mod-time tex-mod-time))
          (message "Warning: .aux file is older than .tex file.")))
    ;; define output dir and filename
    (unless out-dir
      (setq out-dir (file-name-directory filename)))
    (unless out-filename
      (setq out-filename (concat (file-name-base filename) ".html")))
    (let* ((output-file (expand-file-name out-filename out-dir))
           (pandoc-status
            (call-process "pandoc" nil "*pandoc output*" t
                          "--standalone" filename "-o"
                          output-file "--mathjax" "--citeproc" "--toc")))
      ;; call pandoc
      (unless (equal 0 pandoc-status)
        (error (format "pandoc failed with exit status %d" pandoc-status)))
      ;; open html file in a buffer
      (let (
            ;; ((inhibit-file-io t))
            ;; (revert-without-query (list (expand-file-name output-file)))
            ;; (global-auto-revert-mode 0)
            ;; (auto-revert-mode 0)
            )
        (with-temp-buffer
          ;; read contents of html file into buffer
          (insert-file-contents output-file)
          (tex2html-add-style-to-html-head)
          (tex2html-postprocess-html-buffer auxfile external-auxfiles)
          (tex2html-add-tex-pdf-links output-file)
          (write-file output-file)
          )
        ;; (or
        ;;  (when-let ((buffer (get-file-buffer output-file)))
        ;;    (set-buffer buffer)
        ;;    ;;  check if file exists, and if so, revert
        ;;    (when (file-exists-p output-file)
        ;;      (revert-buffer t t)))
        ;;  (find-file output-file))
        ;; (tex2html-postprocess-html-buffer auxfile external-auxfiles)
        ;; (write-file output-file)
        )
      ;; (let ( )
      ;;   (revert-buffer t t) ;; run tex2html-postprocess-html-buffer function
      ;;   (tex2html-postprocess-html-buffer auxfile) ;; save changes (save-buffer))
      )))

(defun tex2html-convert-org-file (&optional filename out-dir out-filename)
  "Export an Org note FILENAME to HTML and apply site postprocessing."
  (interactive)
  (unless filename
    (setq filename (or (buffer-file-name (buffer-base-buffer))
                       (buffer-file-name))))
  (unless filename
    (error "No Org file specified and current buffer does not seem to be associated with a file"))
  (unless (string= (file-name-extension filename) "org")
    (error "File is not an Org (.org) file"))
  (unless out-dir
    (setq out-dir (file-name-directory filename)))
  (unless out-filename
    (setq out-filename (concat (file-name-base filename) ".html")))
  (let ((output-file (expand-file-name out-filename out-dir))
        (org-html-validation-link nil))
    (with-current-buffer (find-file-noselect filename)
      (org-export-to-file 'html output-file nil nil nil nil nil))
    (with-temp-buffer
      (insert-file-contents output-file)
      (tex2html-add-style-to-html-head)
      (tex2html-add-comment-script)
      (tex2html-add-org-links output-file)
      (write-file output-file))))

(defun tex2html-process-directory (&optional directory)
  "Converts and processes tex files in DIRECTORY.
Uses pandoc and then applies some postprocessing.  The file
config.json is expected to contain configuration options, such as
which files to exclude.  This function looks at every .tex file
in DIRECTORY that is not in the exclude list provided by
config.json.  It then operates in two passes.

In the first pass, for each .tex file that was modified more
recently than the corresponding .pdf file (or for which no .pdf
file exists), it runs latexmk to fully compile the .tex file.

In the second pass, for each .tex file that was modified more
recently than the corresponding .html file (or for which no .html
file exists), it calls tex2html-convert-file to convert the .tex
file to .html and apply postprocessing."
  (interactive "D")
  (unless directory
    (setq directory default-directory))
  (let ((default-directory directory))
    (let* ((exclude-list (tex2html-exclude-from-config directory))
           (files (directory-files directory t "\\.tex$"))
           (files
            (cl-remove-if
             (lambda (file)
               (or (member (file-name-nondirectory file) exclude-list)
                   (member (file-name-sans-extension
                            (file-name-nondirectory file))
                           exclude-list)))
             files))
           (files-with-old-pdf
            (cl-remove-if
             (lambda (file)
               (let ((pdffile
                      (concat (file-name-sans-extension file) ".pdf")))
                 (and (file-exists-p pdffile)
                      (time-less-p (nth 5 (file-attributes file))
                                   (nth 5 (file-attributes pdffile))))))
             files))
           (files-with-old-html
            (cl-remove-if
             (lambda (file)
               (let ((htmlfile
                      (concat (file-name-sans-extension file) ".html")))
                 (and (file-exists-p htmlfile)
                      (time-less-p (nth 5 (file-attributes file))
                                   (nth 5 (file-attributes htmlfile))))))
             files)))
      (dolist (file files-with-old-pdf)
        (let* ((cmd-string
                (mapconcat
                 #'identity
                 `("latexmk" "-shell-escape" "-view=none" "-pdf" ,file)
                 " "))
               (latexmk-status
                (progn
                  (message "Compiling %s: %s" file cmd-string)
                  (call-process
                   "latexmk" nil
                   "*latexmk output*" t
                   "-shell-escape"
                   "-view=none"
                   "-pdf" file))))
          (unless (equal 0 latexmk-status)
            (error (format "latexmk failed with exit status %d"
                           latexmk-status)))))
      (dolist (file files-with-old-html)
        (message "Converting %s to html: " file)
        (tex2html-convert-file file))
      (message "Done processing %d files" (length files)))))


(require 'json)
(require 'seq)

(defun czm/format-git-time-string (str)
  (if (< (length str) 19)
      ""
    (let ((substr (substring str 0 19)))
      (replace-regexp-in-string "T" " " substr))))

(defun tex2html-detex (s)
  "Convert common TeX accents and ligatures in S to unicode.
Math ($...$) is left intact for MathJax to render client-side."
  (if (not s) s
    (require 'ucs-normalize)
    (let ((accents '((?' . #x301) (?` . #x300) (?\" . #x308) (?^ . #x302)
                     (?~ . #x303) (?= . #x304) (?. . #x307)))
          (cedilla #x327))
      (with-temp-buffer
        (insert s)
        ;; \'e, \'{e}, \"u, ... -> letter + combining accent
        (goto-char (point-min))
        (while (re-search-forward
                "\\\\\\([\"'`^~=.]\\)\\(?:{\\([A-Za-z]\\)}\\|\\([A-Za-z]\\)\\)" nil t)
          (let ((acc (string-to-char (match-string 1)))
                (ch (or (match-string 2) (match-string 3))))
            (replace-match (concat ch (string (cdr (assq acc accents)))) t t)))
        ;; \c{c} -> c + combining cedilla
        (goto-char (point-min))
        (while (re-search-forward "\\\\c{\\([A-Za-z]\\)}" nil t)
          (replace-match (concat (match-string 1) (string cedilla)) t t))
        ;; ligatures, special letters, dashes, quotes
        (dolist (pair '(("\\\\ss\\_>" . "ß") ("\\\\ae\\_>" . "æ") ("\\\\AE\\_>" . "Æ")
                        ("\\\\o\\_>" . "ø") ("\\\\O\\_>" . "Ø")
                        ("\\\\aa\\_>" . "å") ("\\\\AA\\_>" . "Å")
                        ("\\\\l\\_>" . "ł") ("\\\\L\\_>" . "Ł")
                        ("---" . "—") ("--" . "–")
                        ("``" . "“") ("''" . "”")))
          (goto-char (point-min))
          (while (re-search-forward (car pair) nil t)
            (replace-match (cdr pair) t t)))
        (ucs-normalize-NFC-string (buffer-string))))))

(defun tex2html--escaped-position-p (s pos)
  "Return non-nil when character POS in S is escaped by backslashes."
  (let ((count 0)
        (i (1- pos)))
    (while (and (>= i 0) (= (aref s i) ?\\))
      (setq count (1+ count))
      (setq i (1- i)))
    (= 1 (% count 2))))

(defun tex2html--strip-tex-comment (line)
  "Strip an unescaped TeX comment from LINE."
  (let ((i 0)
        pos)
    (while (and (< i (length line)) (not pos))
      (when (and (= (aref line i) ?%)
                 (not (tex2html--escaped-position-p line i)))
        (setq pos i))
      (setq i (1+ i)))
    (if pos (substring line 0 pos) line)))

(defun tex2html--tex-file-lines (filename)
  "Return comment-stripped lines from FILENAME."
  (with-temp-buffer
    (insert-file-contents filename)
    (mapcar #'tex2html--strip-tex-comment
            (split-string (buffer-string) "\n"))))

(defun tex2html--hash-keys (hash)
  "Return keys from HASH."
  (let (keys)
    (maphash (lambda (key _value) (push key keys)) hash)
    keys))

(defun tex2html--source-labels (tex-file)
  "Return labels declared in TEX-FILE."
  (let (labels)
    (when (file-exists-p tex-file)
      (dolist (line (tex2html--tex-file-lines tex-file))
        (with-temp-buffer
          (insert line)
          (goto-char (point-min))
          (while (re-search-forward "\\\\label{\\([^}]+\\)}" nil t)
            (push (match-string 1) labels)))))
    labels))

(defun tex2html--labels-for-document (tex-file)
  "Return a hash table of labels known for TEX-FILE.
Labels are read from both source and the corresponding aux file when
available."
  (let ((labels (make-hash-table :test 'equal))
        (aux-file (concat (file-name-sans-extension tex-file) ".aux")))
    (dolist (label (tex2html--source-labels tex-file))
      (puthash label t labels))
    (when (file-exists-p aux-file)
      (dolist (label (tex2html--hash-keys (label-number-hash-table aux-file)))
        (puthash label t labels)))
    labels))

(defun tex2html--external-documents-in-file (tex-file)
  "Return external-document plists declared in TEX-FILE."
  (with-temp-buffer
    (insert-file-contents tex-file)
    (tex2html-collect-external-documents-in-tex-buffer)))

(defun tex2html--reference-labels (tex-file)
  "Return reference plists from TEX-FILE.
Each plist contains `:file', `:line', `:command', and `:label'."
  (let (references
        (line-number 0))
    (dolist (line (tex2html--tex-file-lines tex-file))
      (setq line-number (1+ line-number))
      (with-temp-buffer
        (insert line)
        (goto-char (point-min))
        (while (re-search-forward
                "\\\\\\(eqref\\|ref\\|autoref\\|pageref\\|cref\\|Cref\\)\\*?\\(?:\\[[^]]*\\]\\)?{\\([^}]+\\)}"
                nil t)
          (let ((command (match-string 1)))
            (dolist (label (split-string (match-string 2) "," t "[[:space:]\n\r]+"))
              (push (list :file tex-file
                          :line line-number
                          :command command
                          :label label)
                    references))))))
    (nreverse references)))

(defun tex2html-link-check-issues (&optional directory)
  "Return a list of TeX cross-reference issues in DIRECTORY."
  (let ((default-directory (or directory default-directory))
        issues)
    (dolist (tex-file (tex2html-buildable-tex-files default-directory))
      (let* ((available-labels (tex2html--labels-for-document tex-file))
             (aux-file (concat (file-name-sans-extension tex-file) ".aux")))
        (unless (file-exists-p aux-file)
          (push (format "%s: missing own aux file %s" tex-file aux-file)
                issues))
        (dolist (document (tex2html--external-documents-in-file tex-file))
          (let* ((prefix (plist-get document :prefix))
                 (external-tex (plist-get document :tex))
                 (external-aux (plist-get document :aux))
                 (external-labels (tex2html--labels-for-document external-tex)))
            (unless (file-exists-p external-aux)
              (push (format "%s: missing external aux %s from \\externaldocument{%s}"
                            tex-file external-aux (plist-get document :stem))
                    issues))
            (maphash
             (lambda (label _value)
               (puthash (concat prefix label) t available-labels))
             external-labels)))
        (dolist (reference (tex2html--reference-labels tex-file))
          (unless (gethash (plist-get reference :label) available-labels)
            (push (format "%s:%s: dangling \\%s{%s}"
                          (plist-get reference :file)
                          (plist-get reference :line)
                          (plist-get reference :command)
                          (plist-get reference :label))
                  issues)))))
    (sort issues #'string<)))

(defun tex2html-check-links (&optional report-file directory)
  "Write a Markdown TeX link-check report to REPORT-FILE.
The check is warning-only; it returns the number of issues found."
  (interactive)
  (let* ((issues (tex2html-link-check-issues directory))
         (report-file (or report-file "link-check-report.md")))
    (with-temp-file report-file
      (insert "# TeX Link Check\n\n")
      (if issues
          (progn
            (insert (format "%d issue(s) found.\n\n" (length issues)))
            (dolist (issue issues)
              (insert (format "- %s\n" issue))))
        (insert "No issues found.\n")))
    (dolist (issue issues)
      (message "Warning: %s" issue))
    (length issues)))

(defun tex2html--tex-title-and-abstract (filename)
  "Return (TITLE . ABSTRACT) parsed from TeX FILENAME."
  (with-temp-buffer
    (insert-file-contents filename)
    (goto-char (point-min))
    (let ((title (if (re-search-forward "\\\\title\\(\\[.*?\\]\\)?{\\(.*?\\)}" nil t)
                     (match-string 2)
                   filename))
          abstract)
      (goto-char (point-min))
      (when (re-search-forward "\\\\begin{abstract}[[:space:]]+" nil t)
        (let ((beg (match-end 0)))
          (when (re-search-forward "[[:space:]]+\\\\end{abstract}" nil t)
            (setq abstract
                  (buffer-substring-no-properties beg (match-beginning 0))))))
      (cons title abstract))))

(defun tex2html--org-keyword (filename keyword)
  "Return Org KEYWORD value from FILENAME, or nil."
  (with-temp-buffer
    (insert-file-contents filename)
    (goto-char (point-min))
    (when (re-search-forward
           (format "^#\\+%s:[[:space:]]*\\(.*\\)$" (regexp-quote keyword))
           nil t)
      (string-trim (match-string 1)))))

(defun tex2html--listing-entry (filename type)
  "Return a listing entry for FILENAME of TYPE."
  (let* ((created (czm/format-git-time-string
                   (shell-command-to-string
                    (concat "git log --format=%aI -- " filename
                            " | tail -1"))))
         (modified (czm/format-git-time-string
                    (shell-command-to-string
                     (concat "git log -1 --format=%aI -- " filename))))
         (base (file-name-sans-extension filename))
         title abstract source pdf)
    (pcase type
      ("tex"
       (let ((metadata (tex2html--tex-title-and-abstract filename)))
         (setq title (car metadata)
               abstract (cdr metadata)
               source filename
               pdf (concat base ".pdf"))))
      ("org"
       (setq title (or (tex2html--org-keyword filename "TITLE") filename)
             abstract (tex2html--org-keyword filename "DESCRIPTION")
             source filename)))
    `((title . ,(tex2html-detex title))
      (abstract . ,(tex2html-detex abstract))
      (dateCreated . ,created)
      (dateModified . ,modified)
      (file . ,base)
      (type . ,type)
      (source . ,source)
      (pdf . ,pdf))))

(defun populate-listing-json ()
  (interactive)
  (let* ((data-file "listing.json")
         (tex-files (tex2html-buildable-tex-files))
         (org-files (tex2html-buildable-org-files))
         (data-list (append
                     (mapcar (lambda (filename)
                               (tex2html--listing-entry filename "tex"))
                             tex-files)
                     (mapcar (lambda (filename)
                               (tex2html--listing-entry filename "org"))
                             org-files))))
    (with-temp-file data-file
      (insert (json-encode data-list)))))

(defun tex2html-make-index ()
  (interactive)
  (populate-listing-json)
  (find-file "index.org")
  (setq org-html-validation-link nil)
  (org-html-export-to-html))

(require 'org)
(org-link-set-parameters
 "tex-html"
 :follow (lambda (path)
           (browse-url (format "%s.html" path)))
 :export (lambda (path _desc backend)
           (let* ((json-array-type 'list)
                  (json-object-type 'plist)
                  (data (json-read-file "listing.json"))
                  (entry (cl-find-if
                          (lambda (e) (string= (plist-get e :file) path))
                          data))
                  (title (plist-get entry :title)))
             (cond ((eq backend 'html)
                    (format "<a href=\"%s.html\">%s</a>" path title))))))


(provide 'tex2html)
;;; tex2html.el ends here
