# Hyperborean Markdown
This is a library that has taken a stab at creating a *Lisp* -> *MD* library. So that you can translate Lisp objects into Markdown, perhaps you wish to create MD templates based off of some lisp objects you have, idk its up to you.
## Current Problems
- Currently references aren't implemented.
- There is a bug where extra - get added when you start trying to make fancy nested unordered lists. If you simply nest with simple strings there are no problems
## Usage
### There are 4 main methods to invoke the parser:
```lisp
 (with-markdown STREAM BODY)
```
```lisp
 (with-markdown-to-string BODY)
```
```lisp
 (with-markdown-to-file (PATHNAME KEYS) BODY)
```
```lisp
 (parse-to-md STREAM LIST)
```

## List of keys
1. h1
2. h2
3. h3
4. h4
5. h5
6. h6
7. br
8. bold
9. italic
10. bold-and-italic
11. blockquote
12. blockquote1
13. ordered-list
14. unordered-list
15. code-block
16. image
17. link
18. hrule
19. linked-image

Keys are CL Keywords.
### Key options
A few keys have special options:
1. **image**
    1. href
2. **link**
    1. href
    2. title
3. **linked-image**
    1. path
    2. description
    3. href
4. **code-block**
    1. lang

To provide arguments to these keys you have to use keyword arguments like so
```lisp
  (:linked-image
     :path '~/oof.jpg'
     :description 'an oof image'
     :href 'imgur.com/oof'
     'an oof image')
```
## Extra
If you put "" at your top level a newline will be added

## The code used to generate this README.md


```lisp
 (with-markdown-to-file ("./README.md" :if-exists :supersede)
      ((:h1 "Hyperborean Markdown")
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
       "If you put "" at your top level a newline will be added"
       ""

```
