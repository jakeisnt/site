(ns script)

(def parser
  (insta/parser "
<S> = TITLE | ALIAS | SUBTEXT | LINE
TITLE=<S>,STR,NEWLINE
ALIAS=ALIAS_NAME,\"= \",STR
SUBTEXT=\"(\"STR\")\",NEWLINE
LINE=ALIAS_NAME,#\": \",STR,NEWLINE

ALIAS_NAME=#\"\S\
NEWLINE=#\"\\n\"
STR=\"^#.\"
"))

;; parser for script files
(defn parse-script [str]
  (parser/parse parser str))

(defn to-ast [parsed]
  ;; switch(node):
  ;; - title -> add to title of doc
  ;; - alias -> add to alias map
  ;; - subtext -> add inline to document as subtext
  ;; - line: add linline as a line.
  )

;; NOTE:
;; The regexes are not perfect
;; Disallow `:`, `=`, `(`, `)` in str and alias names to avoid ambiguity.
