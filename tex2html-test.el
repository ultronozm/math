;;; tex2html-test.el --- tests for tex2html.el  -*- lexical-binding: t; -*-

;; Run from this directory with
;;
;;   emacs -Q --batch -l tex2html.el -l tex2html-test.el -f ert-run-tests-batch-and-exit
;;
;; They need neither pandoc nor LaTeX: the HTML is written out by hand,
;; in the shape that pandoc gives it.

;;; Code:

(require 'ert)
(require 'tex2html)

(defun tex2html-test--numbers (&rest pairs)
  "Return a hash table of labels to numbers from the list PAIRS."
  (let ((hash (make-hash-table :test 'equal)))
    (while pairs
      (puthash (pop pairs) (pop pairs) hash))
    hash))

(defmacro tex2html-test--edit (html &rest body)
  "Run BODY in a buffer holding HTML, point at the start; return the buffer."
  (declare (indent 1))
  `(with-temp-buffer
     (insert ,html)
     (goto-char (point-min))
     ,@body
     (buffer-string)))

(ert-deftest tex2html-test-labels-of-one-display ()
  "Both labels of an align lead to it, and each row has its own number."
  (let ((html (tex2html-test--edit
                  (concat "<p><span class=\"math display\">\\[x\\]</span></p>\n"
                          "<p><span class=\"math display\">\\[\\begin{align}\n"
                          "x &amp;= 1 \\label{eq:a}\\\\\n"
                          "y &amp;= 2 \\label{eq:b}\n"
                          "\\end{align}\\]</span></p>")
                (tex2html--label-displays
                 (tex2html-test--numbers "eq:a" "2" "eq:b" "3")))))
    (should (string-search "<span id=\"eq:b\"></span><span id=\"eq:a\" class=\"math display\">" html))
    (should (string-search "\\label{eq:a}\\tag{2}" html))
    (should (string-search "\\label{eq:b}\\tag{3}" html))
    ;; the display before is not touched
    (should (string-search "<p><span class=\"math display\">\\[x\\]</span></p>" html))))

(ert-deftest tex2html-test-label-in-a-comment ()
  "A commented-out label gets neither an id nor a number."
  (let ((html (tex2html-test--edit
                  "<span class=\"math display\">\\[\n%\\label{eq:old}\nx\n\\]</span>"
                (tex2html--label-displays (tex2html-test--numbers "eq:old" "9")))))
    (should-not (string-search "id=\"eq:old\"" html))
    (should-not (string-search "\\tag" html))))

(ert-deftest tex2html-test-one-tag-per-row ()
  "Two labels on one row make one \\tag, as MathJax allows no more."
  (let ((html (tex2html-test--edit
                  (concat "<span class=\"math display\">"
                          "\\[\\begin{equation}\\label{eq:a}\\label{eq:b}x\\end{equation}\\]"
                          "</span>")
                (tex2html--label-displays
                 (tex2html-test--numbers "eq:a" "1" "eq:b" "1")))))
    (should (= 1 (with-temp-buffer
                   (insert html)
                   (how-many "\\\\tag" (point-min) (point-max)))))
    (should (string-search "<span id=\"eq:b\"></span><span id=\"eq:a\"" html))))

(ert-deftest tex2html-test-statement-numbers ()
  "A labeled statement gets LaTeX's number; the others keep pandoc's."
  (let ((html (tex2html-test--edit
                  (concat "<div id=\"lem:a\" class=\"lemma\">\n"
                          "<p><strong>Lemma 4</strong>. Text.</p>\n</div>\n"
                          "<div class=\"lemma\">\n"
                          "<p><strong>Lemma 5</strong>. No label.</p>\n</div>\n"
                          "<div id=\"sec:x\" class=\"remark\">\n"
                          "<p><strong>Remark 6</strong>. Not in the aux file.</p>\n</div>")
                (tex2html--number-statements
                 (tex2html-test--numbers "lem:a" "2.3")))))
    (should (string-search "<strong>Lemma 2.3</strong>" html))
    (should (string-search "<strong>Lemma 5</strong>" html))
    (should (string-search "<strong>Remark 6</strong>" html))))

(ert-deftest tex2html-test-proof-markup ()
  "A proof folds as a whole, and its caption may hold a link with a backslash."
  (let ((html (tex2html-test--edit
                  (concat "<html><head></head><body>\n"
                          "<div class=\"proof\">\n"
                          "<p><em>Proof of <a href=\"#eq:a\" data-reference-type=\"eqref\""
                          " data-reference=\"eq:a\">\\((9)\\)</a>.</em> First\nparagraph.</p>\n"
                          "<p>Second paragraph.</p>\n</div>\n</body></html>")
                (tex2html-postprocess-make-proof-links-toggleable))))
    (should (string-search
             (concat "<em class=\"toggle-proof\" role=\"button\" tabindex=\"0\""
                     " aria-expanded=\"true\">Proof of <a href=\"#eq:a\""
                     " data-reference-type=\"eqref\" data-reference=\"eq:a\">\\((9)\\)</a>.</em>"
                     "<span class=\"proof-rest\"> First\nparagraph.</span></p>\n"
                     "<p>Second paragraph.</p>\n</div>")
             html))
    (should (string-search ".proof.folded" html))
    (should (string-search "toggle-proof" (substring html (string-search "<script>" html))))
    (should-not (string-search "data-default-text" html))))

(ert-deftest tex2html-test-postprocess ()
  "The whole post-processing, on a note with an aux file."
  (let* ((dir (make-temp-file "tex2html-test" t))
         (html-file (expand-file-name "n.html" dir)))
    (unwind-protect
        (progn
          (with-temp-file (expand-file-name "n.tex" dir)
            (insert "\\documentclass{amsart}\n"))
          (with-temp-file (expand-file-name "n.aux" dir)
            (insert "\\newlabel{sec:one}{{1}{1}{}{}{}}\n"
                    "\\newlabel{thm:a}{{2.3}{4}{}{}{}}\n"
                    "\\newlabel{eq:a}{{7}{5}{}{}{}}\n"))
          (with-temp-file html-file
            (insert "<html><head><style>x{}</style></head><body>\n"
                    "<h1 id=\"sec:one\">One</h1>\n"
                    "<div id=\"thm:a\" class=\"theorem\">\n"
                    "<p><strong>Theorem 4</strong>. See <a href=\"#eq:a\""
                    " data-reference-type=\"eqref\" data-reference=\"eq:a\">[eq:a]</a>"
                    " and <a href=\"#thm:a\" data-reference-type=\"ref\""
                    " data-reference=\"thm:a\">4</a>"
                    " <span class=\"citation\" data-cites=\"X Y\">(X 1999; Y 2000)</span>.</p>\n"
                    "</div>\n"
                    "<span class=\"math display\">\\[\\label{eq:a} e=mc^2\\]</span>\n"
                    "</body></html>"))
          (with-current-buffer (find-file-noselect html-file)
            (unwind-protect
                (progn
                  (tex2html-postprocess-html-buffer)
                  (let ((s (buffer-string)))
                    (should (string-search "§1. One" s))
                    (should (string-search "<strong>Theorem 2.3</strong>" s))
                    (should (string-search "data-reference=\"eq:a\">\\((7)\\)</a>" s))
                    (should (string-search "data-reference=\"thm:a\">2.3</a>" s))
                    (should (string-search "<a href=\"#ref-X\"" s))
                    (should (string-search "<span id=\"eq:a\" class=\"math display\">" s))
                    (should (string-search "\\label{eq:a}\\tag{7}" s))))
              (set-buffer-modified-p nil)
              (kill-buffer))))
      (delete-directory dir t))))

(provide 'tex2html-test)
;;; tex2html-test.el ends here
