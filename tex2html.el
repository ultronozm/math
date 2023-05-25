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

(defun label-number-hash-table (aux-file)
  "Return a hash table of label numbers from the given aux-file."
  (let ((hash (make-hash-table :test 'equal)))
    (when (file-exists-p aux-file)
      (with-current-buffer (find-file-noselect aux-file)
	(save-excursion
	  (goto-char (point-min))
	  (while (re-search-forward "\\\\newlabel{\\([^}]+\\)}.*?{{\\([^}]+\\)}" nil t)
	    (let ((label (match-string 1))
		  (number (match-string 2)))
	      (puthash label number hash))))
	(kill-buffer)))
    hash))


(defun tex2html-collect-external-auxfiles-in-tex-buffer ()
  "Collect names of .aux files referenced in current buffer."
  (save-restriction
    (widen)
    (save-excursion
      (goto-char (point-min))
      (let ((files nil))
	(while (re-search-forward "\\\\externaldocument\\(\\[[^]]+\\]\\)?{\\([^}]+\\)}" nil t)
	  (let ((file (concat (match-string 2) ".aux")))
	    (unless (file-exists-p file)
	      (error "External .aux file %s does not exist" file))
	    (push file files))
	  )
	files))))

(defun tex2html-postprocess-html-buffer (&optional auxfile external-auxfiles)
  "Update an HTML buffer with MathJax code created using pandoc from a LaTeX file."
  (interactive)
  (if (not auxfile)
      (setq auxfile (concat (file-name-sans-extension (buffer-file-name)) ".aux")))
  (if (not external-auxfiles)
      (setq external-auxfiles
	    ;; open corresponding tex file and look for \externaldocument
	    (with-temp-buffer
	      (insert-file-contents (concat (file-name-sans-extension auxfile) ".tex"))
	      (tex2html-collect-external-auxfiles-in-tex-buffer))))
  (let
      ((label-number-hash (label-number-hash-table auxfile))
       ;; construct a list of pairs consisting of the html file
       ;; associated to an external aux file and the hash table for
       ;; that file
       (external-label-number-hash-list
	(mapcar
	 (lambda (auxfile)
	   (cons
	    (concat
	     (file-name-sans-extension auxfile)
	     ".html")
	    (label-number-hash-table auxfile)))
	 external-auxfiles)))
    (save-excursion
      (goto-char (point-min))
      ;; Step 1: Handle \label{eqn:blah}
      (while (search-forward-regexp "\\\\label{\\([^}]+\\)}" nil t)
	(let* ((label (match-string 1))
	       (tag-number (gethash label label-number-hash)))
          (replace-match (concat "\\\\label{" label "}\\\\tag{" tag-number "}"))
          (save-excursion
	    (if (search-backward-regexp "<span[[:space:]]+class=\"math display\"" nil t)
		(progn
		  (replace-match (concat "<span id=\"" label "\" class=\"math display\""))
		  t)
	      (message "Warning: something went wrong"))))))
    ;; Step 2: Handle links with data-reference-type="ref" or "eqref"
    (goto-char (point-min))
    (while (search-forward-regexp "href=\"\\([^\"]+\\)\"[^>]+data-reference-type=\"\\(ref\\|eqref\\)\"[^>]+data-reference=\"\\([^\"]+\\)\"[^>]*>\\(\\[[^]]+\\]\\)" nil t)
      (let* ((link (match-string 1))
	     (ref-type (match-string 2))
	     (label (match-string 3))
	     tag-number external-html
	     )
	(unless (setq tag-number (gethash label label-number-hash))
	  (cl-loop
	   for (html-file . hash) in external-label-number-hash-list
	   for number = (gethash label hash)
	   when number
	   do (setq tag-number number
		    external-html html-file)))
	(let ((new-content
	       (if (string= ref-type "ref")
		   (concat "" tag-number "")
		 (concat "\\((" tag-number ")\\)"))))
	  (replace-match
	   new-content
	   ;; (if external-html
	   ;;     (concat "["
	   ;; 	       ;; take substring up to first underscore
	   ;; 	       (let ((name (file-name-sans-extension external-html)))
	   ;; 		 (substring name 0 (string-match "_" name)))
	   ;; 	       ", " new-content
	   ;; 	       "]")
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
	(when-let ((number (gethash label label-number-hash)))
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
    (let* ((auxfiles-base (mapcar 'file-name-sans-extension external-auxfiles))
	   (auxfiles-regexp (regexp-opt auxfiles-base)))
      ;; search for a href="blah.pdf" where blah is in auxfiles-base
      (while (re-search-forward (concat "href=\"\\(" auxfiles-regexp "\\.pdf\\)\"") nil t)
	(let* ((pdf-file (match-string 1))
	       (html-file (concat (file-name-sans-extension pdf-file) ".html")))
	  (replace-match html-file t t nil 1))))))

(defcustom giscus-comment-script
  "<script src=\"https://giscus.app/client.js\"
        data-repo=\"Ultronozm/math\"
        data-repo-id=\"R_kgDOJlhjqQ\"
        data-category=\"Announcements\"
        data-category-id=\"DIC_kwDOJlhjqc4CWo21\"
        data-mapping=\"pathname\"
        data-strict=\"0\"
        data-reactions-enabled=\"1\"
        data-emit-metadata=\"0\"
        data-input-position=\"bottom\"
        data-theme=\"preferred_color_scheme\"
        data-lang=\"en\"
        crossorigin=\"anonymous\"
        async>
</script>"
  "Script to add comments to HTML files (specific to my repo)."
  :type 'string)

(defun tex2html-add-comment-script ()
  "Add script to HTML buffer that allows users to add comments."
  (interactive)
  (goto-char (point-min))
  (when (re-search-forward "</body>" nil t)
    (replace-match (concat giscus-comment-script "\n</body>"))))

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
  (when-let ((style-beg (search-forward "<style>" nil t))
	     (style-end (search-forward "</style>" nil t))
	     (html-beg
	      (progn
		(goto-char style-beg)
		(search-forward "html {" nil t)))
	     (html-end
	      (search-forward "}" nil t)))
    ;; replace the contents of html{} with the above
    (delete-region html-beg html-end)
    (goto-char html-beg)
    (insert
     "\n"
     (mapconcat
      #'identity
      '(
	"line-height: 1.5;"
	"font-family: Georgia, serif;"
	"font-size: 20px;"
	"color: #1a1a1a;"
	"background-color: #fdfdfd;"
	)
      "\n")
     "\n}")))

(defun tex2html-add-tex-pdf-links ()
  (interactive)
  (goto-char (point-min))
  (when-let ((style-beg (search-forward "<style>" nil t))
	     (body-beg (search-forward "<body>" nil t))
	     (base-filename (file-name-nondirectory (file-name-sans-extension (buffer-file-name)))))
    (goto-char body-beg)
    (insert
     (format "
    <div class=\"my-links-container\">
%s
      <a href=\"%s.tex\" class=\"my-link\">tex</a>
      <a href=\"%s.pdf\" class=\"my-link\">pdf</a>
      <a href=\"https://github.com/ultronozm/math/commits/main/%s.tex\" class=\"my-link\">history</a>
    </div>
"
	     (czm/format-git-time-string
	      (shell-command-to-string
	       (concat "git log -1 --format=%aI -- " (concat base-filename ".tex"))))
	     base-filename
	     base-filename
	     base-filename
	     )
     )
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
      }")))

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
	  (tex2html-collect-external-auxfiles-in-tex-buffer)
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
	   (pandoc-status (call-process "pandoc" nil "*pandoc output*" t "--standalone" filename "-o"
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
	  (tex2html-postprocess-html-buffer auxfile external-auxfiles)
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
      ;; 	(revert-buffer t t) ;; run tex2html-postprocess-html-buffer function
      ;; 	(tex2html-postprocess-html-buffer auxfile) ;; save changes (save-buffer))
      )))


(defun tex2html-exclude-from-config (directory)
  "Read and parse the 'exclude' field from 'config.json'."
  (interactive "D")
  (let* ((json-object-type 'hash-table)
         (json-array-type 'list)
         (json-key-type 'string)
         (json-data (json-read-file
		     (expand-file-name
		      (concat (file-name-as-directory directory)
			      "config.json")))))
    (gethash "exclude" json-data)))


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
	    (cl-remove-if (lambda (file)
			    (or (member (file-name-nondirectory file) exclude-list)
				(member (file-name-sans-extension
					 (file-name-nondirectory file))
					exclude-list)))
			  files))
	   (files-with-old-pdf
	    (cl-remove-if (lambda (file)
			    (let ((pdffile
				   (concat (file-name-sans-extension file) ".pdf")))
			      (and (file-exists-p pdffile)
				   (time-less-p (nth 5 (file-attributes file))
						(nth 5 (file-attributes pdffile))))))
			  files))
	   (files-with-old-html
	    (cl-remove-if (lambda (file)
			    (let ((htmlfile
				   (concat (file-name-sans-extension file) ".html")))
			      (and (file-exists-p htmlfile)
				   (time-less-p (nth 5 (file-attributes file))
						(nth 5 (file-attributes htmlfile))))))
			  files)))
     (dolist (file files-with-old-pdf)
	(let (
	      (latexmk-status
	       (progn
		 (message "Compiling %s" file)
		 (call-process
		  "latexmk" nil
		  "*latexmk output*" t
		  "-shell-escape"
		  "-view=none"
		  "-pdf" file))))
	  (unless (equal 0 latexmk-status)
	    (error (format "latexmk failed with exit status %d" latexmk-status)))))
      (dolist (file files-with-old-html)
	(message "Converting %s to html" file)
	(tex2html-convert-file file))
      (message "Done processing %d files" (length files)))))


(require 'json)
(require 'seq)

(defun czm/format-git-time-string (str)
  (let ((substr (substring str 0 19)))
    ;; replace T by space in substr
    (replace-regexp-in-string "T" " " substr))
  )

(defun populate-listing-json ()
  (interactive)
  (let* ((exclude-file "config.json")
	 (data-file "listing.json")
	 (exclude-list (with-temp-buffer
			 (insert-file-contents exclude-file)
			 (cdr (assoc 'exclude (json-read)))))
	 (tex-files (seq-filter (lambda (filename)
				  (and (string-suffix-p ".tex" filename)
				       (not (seq-contains-p exclude-list filename))))
				(split-string
				 (shell-command-to-string "git ls-files *.tex") "\n")))
	 (data-list (mapcar (lambda (filename)
			      (let* ((title (with-temp-buffer
					      (insert-file-contents filename)
					      (goto-char (point-min))
					      (if (re-search-forward "\\\\title\\(\\[.*?\\]\\)?{\\(.*?\\)}" nil t)
						  (match-string 2)
						filename)))
				     (abstract (with-temp-buffer
						 (insert-file-contents filename)
						 (goto-char (point-min))
						 (when (re-search-forward "\\\\begin{abstract}[[:space:]]+" nil t)
						   (let ((beg (match-end 0)))
						     (when (re-search-forward "[[:space:]]+\\\\end{abstract}" nil t)
						       (buffer-substring-no-properties beg (match-beginning 0)))))))
				     (created (czm/format-git-time-string (shell-command-to-string
								       (concat "git log --format=%aI -- " filename
									       " | tail -1"))))
				     (modified (czm/format-git-time-string
						(shell-command-to-string
						 (concat "git log -1 --format=%aI -- " filename)))))
				`((title . ,title)
				  (abstract . ,abstract)
				  (dateCreated . ,created)
				  (dateModified . ,modified)
				  (file . ,(file-name-sans-extension filename)))))
			    tex-files)))
    (with-temp-file data-file
      (insert (json-encode data-list)))))

(defun tex2html-make-index ()
  (interactive)
  (populate-listing-json)
  (find-file "index.org")
  (setq org-html-validation-link nil)
  (org-html-export-to-html))


(provide 'tex2html)
;;; tex2html.el ends here
