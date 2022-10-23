(load "~/quicklisp/setup.lisp")
(load "~/site/src/util.lisp")
(load "~/site/src/path.lisp")

(ql:quickload :spinneret)
(ql:quickload 'css-lite)
(ql:quickload :parenscript)

(defpackage components
  (:use :cl))

(in-package :components)

(defun checkbox-menu ()
  "An interactive menu that allows the user to optionally enable js features."
  (spinneret::with-html
      (:div :class "checkbox-menu"
            (:input
             :type "checkbox"
             :id "hypothesis-checkbox"
             :checked "false"
             :onclick (parenscript:ps (toggle-hypothesis))
             "hypothes.is"))))

;; TODO
;; (defun links ()
;;   "A table of links about me."
;;   (:table
;;    (:thead
;;     :tr)
;;    )

;;   )


;; i want this site to feel like the navigation of a file system;
;; view version control, depth, what's in current dir;
;; support files of all types;
;; ideally this just doubles as a source code viewer, honestly
;; should mousing over reveal alternative things for this path? is that right?

;; can mouse over to view three options; can click '...' (or scroll?) to view more?

;; idea: route editor!
;; sidebar allows you to interactively add parts to your url
;; press some '+' button
;; adds a slash and a box
;; shows a list of all the possible things taht could go there
;; you type and search or just click on one of them and you're taken to that route,

;; i would love to actually have this in the browser,
;; but for now this is fine

;; also, for the parser to work properly with this page,
;; we might want to group some things into paragraphs
;; we might also want to see what we can do about that url thing on mobile;
;; maybe it's a good idea to move it to the top of the screen and fix it there

;; also: the name of the website should reflect the path. how?
;; it shouldn't be literal, but it should be aesthetically similar.

(defun collect-folder-paths-help (root ls title)
  "
   Convert a list of folder paths into a list of links.

   Examples:
    () -> (:a :href jake/indexx)
    (\"filename\") -> (:a :href jake/filename)
    (\"p\" \"filename\") -> (:a :href jake/p/index) (:a :href jake/p/filename)
    (\"a\" \"p\" \"filename\")  -> (:a :href jake/a/index) (:a :href jake/a/p/index) (:a :href jake/a/p/filename)
  "
  (cond
    ((endp ls) nil)

    ;; if we know we're in a folder (we have no file title),
    ;; we return the last one as a :b
    ((and (endp (cdr ls)) (not title))
     (spinneret::with-html
       (:span " / ")
       (:b (car ls))))

    ((consp ls)
     (let* ((fst (car ls))
            (next-root (concatenate 'string root "/" fst)))
       (spinneret::with-html
         (:span " / ")
         (:a :href (concatenate 'string next-root "/" "index.html") fst)
         (collect-folder-paths-help next-root (cdr ls) title))))))

(defun collect-folder-paths (path title)
  (collect-folder-paths-help "" (path::pathdir path) title))

(defun concat-around (ls around-char)
  "Concatenate the elements of a list of strings around a character."
  (cond
    ((endp ls) nil)
    ((endp (cdr ls)) (car ls))
    (t (concatenate
        'string
        (car ls)
        around-char
        (concat-around (cdr ls) around-char)))))

(defun final-path (path)
  "Construct a final path link for the page."
  (concatenate
   'string
   (concat-around (path::pathdir path) "/")
   "/"
   (pathname-name path)
   "."
   (pathname-type path)))

;; the page shows future paths in some way - how?
;; i want to be able to get to the other pages from this page.

(defun sidebar (path root title)
  "Display a sidebar for a page, given its root path."
  (let ((path (path::remove-root path root)))
    (spinneret::with-html
      (:div
       :class "sidebar"
       ;; Bold site title instead of making it a clickable link if at root (no path).
       (if (endp (path::pathdir path))
           (:b "jake.")
           (:a :href "/index.html" " jake."))
       (:a :href "https://isnt.online" " ~ ")
       (concatenate
        'list
        (collect-folder-paths path title)
        (list
         (:span " / ")
         ;; If we have a file title, add and bold it
         (when title (:b title))))))))


;; TODO: Should I make one of these for employment as well?
;; TODO: Should I generate this from a file instead?
;; -> I think yes; then I can also render it in other ways. Single source of truth.
(defun link-info ()
  "A pane to display personal links."
  (spinneret::with-html
    (:div :class "link-info-table"
         (:table (:tr
                  (:td "Twitter")
                  (:td (:a :href "https://twitter.com/jakeissnt" "jakeissnt")))
                 (:tr
                  (:td "GitHub")
                  (:td (:a :href "https://github.com/jakeisnt" "jakeisnt")))
                 (:tr
                  (:td "Email")
                  (:td (:a :href "mailto://jake+website@isnt.online" "jake @")))))))

(defun lastfm-now-playing ()
  "
   Displays the track currently playing, or the track that last played, through lastfm.
   NOTE: Currently this is just a hack that strings together some JS files.
         We should make this a proper component and develop an abstraction for interactive components.
  "
  (spinneret::with-html
    (:script :src "/lfmNowPlaying.js")
    (:script "getNowPlaying()")))

(defun employment-info ()
  "A panel displaying my employment history.")


(defun make-history-link (long-hash file-path)
  "Get a github permalink to a page in the wiki at a particular path and hash"
  (concatenate 'string "https://github.com/jakeisnt/wiki/blob/" long-hash "/" file-path))

(defun git-history (hist file-path)
  "A pane that renders the given git history of a given file, with github links to history"
  (spinneret::with-html
    (:div :class "git-hist-table"
          (:table
           (:tr (:th "History"))
           (loop for (short-hash long-hash date) in hist
                 collect
                 (:tr
                  (:td date)
                  (:td (:a :href (make-history-link long-hash file-path) short-hash))))))))
