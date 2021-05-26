(with-markdown-to-file ("./README.md" :if-exists :supersede)
  `((:h1 "Hyperborean Markdown")
    "This is a library that has taken a stab at creating a *Lisp* -> *MD* library. So that you can translate Lisp objects into Markdown, perhaps you wish to create MD templates based off of some lisp objects you have, idk its up to you."
    (:h2 "Current Problems")
    (:unordered-list 
     "Currently references aren't implemented."
     "There is a bug where extra - get added when you start trying to make fancy nested unordered lists. If you simply nest with simple strings there are no problems")
    (:h2 "Usage")
    (:h3 "There are 4 main methods to invoke the parser:")
    (:code-block :lang "lisp" "(with-markdown STREAM BODY)")
    (:code-block :lang "lisp" "(with-markdown-to-string BODY)")
    (:code-block :lang "lisp" "(with-markdown-to-file (PATHNAME KEYS) BODY)")
    (:code-block :lang "lisp" "(parse-to-md STREAM LIST)")
    ""
    (:h2 "List of keys")
    (:ordered-list
     "h1" "h2" "h3" "h4" "h5" "h6" "br" "bold"
     "italic" "bold-and-italic" "blockquote" "blockquote1"
     "ordered-list" "unordered-list" "code-block" "image" "link"
     "hrule" "linked-image")
    ""
    "Keys are CL Keywords."
    (:h3 "Key options")
    "A few keys have special options:"
    (:ordered-list
     "**image**"
     (:ordered-list  "href")
     "**link**"
     (:ordered-list "href" "title")
     "**linked-image**"
     (:ordered-list "path" "description" "href")
     "**code-block**"
     (:ordered-list "lang"))
    ""
    "To provide arguments to these keys you have to use keyword arguments like so"
    (:code-block :lang "lisp"
                 " (:linked-image
     :path '~/oof.jpg'
     :description 'an oof image'
     :href 'imgur.com/oof'
     'an oof image')")
    (:h2 "Extra")
    "If you put \"\" at your top level a newline will be added"
    ""
    (:h2 "The code used to generate this README.md")
    ""
    ""
    (:code-block :lang "lisp"
                 "(with-markdown-to-file (\"./README.md\" :if-exists :supersede)
      `((:h1 \"Hyperborean Markdown\")
       \"This is a library that has taken a stab at creating a *Lisp* -> *MD* library. So that you can translate Lisp objects into Markdown, perhaps you wish to create MD templates based off of some lisp objects you have, idk its up to you.\"
       (:h2 \"Current Problems\")
       (:unordered-list 
        \"Currently references aren't implemented.\"
        \"There is a bug where extra - get added when you start trying to make fancy nested unordered lists. If you simply nest with simple strings there are no problems\")
       (:h2 \"Usage\")
       (:h3 \"There are 4 main methods to invoke the parser:\")
       (:code-block :lang \"lisp\" \"(with-markdown STREAM BODY)\")
       (:code-block :lang \"lisp\" \"(with-markdown-to-string BODY)\")
       (:code-block :lang \"lisp\" \"(with-markdown-to-file (PATHNAME KEYS) BODY)\")
       (:code-block :lang \"lisp\" \"(parse-to-md STREAM LIST)\")
       \"\"
       (:h2 \"List of keys\")
       (:ordered-list
        \"h1\" \"h2\" \"h3\" \"h4\" \"h5\" \"h6\" \"br\" \"bold\"
        \"italic\" \"bold-and-italic\" \"blockquote\" \"blockquote1\"
        \"ordered-list\" \"unordered-list\" \"code-block\" \"image\" \"link\"
        \"hrule\" \"linked-image\")
       \"\"
       \"Keys are CL Keywords.\"
       (:h3 \"Key options\")
       \"A few keys have special options:\"
       (:ordered-list
        \"**image**\"
        (:ordered-list  \"href\")
        \"**link**\"
        (:ordered-list \"href\" \"title\")
        \"**linked-image**\"
        (:ordered-list \"path\" \"description\" \"href\")
        \"**code-block**\"
        (:ordered-list \"lang\"))
       \"\"
       \"To provide arguments to these keys you have to use keyword arguments like so\"
       (:code-block :lang \"lisp\"
                    \" (:linked-image
                       :path '~/oof.jpg'
                       :description 'an oof image'
                       :href 'imgur.com/oof'
                       'an oof image')\")
       \"If you put \"\" at your top level a newline will be added\"
       \"\))\"
")))
