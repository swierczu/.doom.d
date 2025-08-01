;;; init.el -*- lexical-binding: t; -*-

;; Install Emacs on MacOS:
;; brew install emacs-plus@30 --with-ctags --with-mailutils --with-xwidgets --with-imagemagick --with-modern-papirus-icon
;; osascript -e 'tell application "Finder" to make alias file to posix file "/usr/local/opt/emacs-plus@30/Emacs.app" at posix file "/Applications" with properties {name:"Emacs.app"}'
;; codesign --force --deep --sign - -v --strip-disallowed-xattrs /usr/local/Cellar/emacs-plus@30/30.1/Emacs.app

(when (string-equal system-type "android")
  (let ((termuxpath "/data/data/com.termux/files/usr/bin"))
    (setenv "PATH" (format "%s:%s" termuxpath
                           (getenv "PATH")))
    (push termuxpath exec-path)
    (push "~/.config/emacs/bin" exec-path)))

(doom! :input
       ;;chinese
       ;;japanese
       ;;layout            ; auie,ctsrnm is the superior home row

       :completion
       (corfu +orderless)
       ;; (company
       ;;  +auto            ; as-you-type code completion
       ;;  +childframe)           ; the ultimate code completion backend
       ;;(helm +fuzzy +icons)              ; the *other* search engine for love and life
       ;;ido               ; the other *other* search engine...
       ;; (ivy
       ;;  +fuzzy
       ;;  +prescient
       ;;  +icons)
       (vertico +icons)          ; the search engine of the future

       :ui
       ;;deft              ; notational velocity for Emacs
       doom              ; what makes DOOM look the way it does
       doom-dashboard    ; a nifty splash screen for Emacs
       doom-quit         ; DOOM quit-message prompts when you quit Emacs
       (emoji +unicode)  ; 🙂
       ;;fill-column       ; a `fill-column' indicator
       hl-todo           ; highlight TODO/FIXME/NOTE/DEPRECATED/HACK/REVIEW
       ;; hydra
       indent-guides     ; highlighted indent columns
       ;;(ligatures +extra)      ; ligatures and symbols to make your code pretty again
       ;;minimap           ; show a map of the code on the side
       modeline          ; snazzy, Atom-inspired modeline, plus API
       nav-flash         ; blink cursor line after big motions
       ;;neotree           ; a project drawer, like NERDTree for vim
       ophints           ; highlight the region an operation acts on
       (popup +defaults)   ; tame sudden yet inevitable temporary windows
       ;;tabs              ; a tab bar for Emacs
       treemacs          ; a project drawer, like neotree but cooler
       ;;unicode           ; extended unicode support for various languages
       vc-gutter         ; vcs diff in the fringe
       vi-tilde-fringe   ; fringe tildes to mark beyond EOB
       (window-select +numbers)     ; visually switch windows
       workspaces        ; tab emulation, persistence & separate workspaces
       zen               ; distraction-free coding or writing

       :editor
       (evil +everywhere); come to the dark side, we have cookies
       file-templates    ; auto-snippets for empty files
       fold              ; (nigh) universal code folding
       (format +onsave)  ; automated prettiness
       ;;god               ; run Emacs commands without modifier keys
       ;;lispy             ; vim for lisp, for people who don't like vim
       multiple-cursors  ; editing in many places at once
       ;;objed             ; text object editing for the innocent
       ;;parinfer          ; turn lisp into python, sort of
       rotate-text       ; cycle region at point between text candidates
       snippets          ; my elves. They type so I don't have to
       word-wrap         ; soft wrapping with language-aware indent

       :emacs
       (dired +icons)             ; making dired pretty [functional]
       electric          ; smarter, keyword-based electric-indent
       (ibuffer +icons)         ; interactive buffer management
       (undo +tree)             ; persistent, smarter undo for your inevitable mistakes
       vc                ; version-control and Emacs, sitting in a tree

       :term
       eshell            ; the elisp shell that works everywhere
       ;;shell             ; simple shell REPL for Emacs
       ;;term              ; basic terminal emulator for Emacs
       vterm             ; the best terminal emulation in Emacs

       :checkers
       syntax              ; tasing you for every semicolon you forget
       (spell            ; tasing you for misspelling misspelling
        +flyspell
        +hunspell)       ;+everywhere)
       (:if (featurep :system 'macos) grammar) ; tasing grammar mistake every you make

       :tools
       tree-sitter
       ansible
       biblio
       (debugger +lsp)         ; FIXME stepping through code, to help you add bugs
       direnv
       (docker +lsp)
       editorconfig      ; let someone else argue about tabs vs spaces
       ;;ein               ; tame Jupyter notebooks with emacs
       (eval +overlay)     ; run code, run (also, repls)
       (lookup              ; navigate your code and its documentation
        +devdocs         ; ...on devdocs.io online
        +dictionary
        +offline
        +docsets)        ; ...or in Dash docsets locally
       (lsp +peek)
       (magit +forge)             ; a git porcelain for Emacs
       make              ; run make tasks from Emacs
       pass              ; password manager for nerds
       pdf               ; pdf enhancements
       ;;prodigy           ; FIXME managing external services & code builders
       rgb               ; creating color strings
       ;;taskrunner        ; taskrunner for all your projects
       ;;terraform         ; infrastructure as code
       tmux              ; an API for interacting with tmux
       ;;upload            ; map local to remote projects via ssh/ftp

       :os
       (:if (featurep :system 'macos) macos)  ; improve compatibility with macOS
       tty               ; improve the terminal Emacs experience

       :lang
       ;;agda              ; types of types of types of types...
       (cc +lsp +tree-sitter)               ; C/C++/Obj-C madness
       ;;clojure           ; java with a lisp
       common-lisp       ; if you've seen one lisp, you've seen them all
       ;;coq               ; proofs-as-programs
       ;;crystal           ; ruby at the speed of c
       ;;csharp            ; unity, .NET, and mono shenanigans
       data              ; config/data formats
       (dart +lsp +flutter)   ; paint ui and not much else
       ;;elixir            ; erlang done right
       ;;elm               ; care for a cup of TEA?
       emacs-lisp        ; drown in parentheses
       ;;erlang            ; an elegant language for a more civilized age
       ;;ess               ; emacs speaks statistics
       ;;faust             ; dsp, but you get to keep your soul
       ;;fsharp            ; ML stands for Microsoft's Language
       ;;fstar             ; (dependent) types and (monadic) effects and Z3
       ;;gdscript          ; the language you waited for
       (go +lsp +tree-sitter)         ; the hipster dialect
       ;;(haskell +dante)  ; a language that's lazier than I am
       ;;hy                ; readability of scheme w/ speed of python
       ;;idris             ; a language you can depend on
       (json +lsp +tree-sitter)              ; At least it ain't XML
       (java +meghanada) ; the poster child for carpal tunnel syndrome
       (javascript +lsp +tree-sitter)
       ;;(julia +lsp)            ; a better, faster MATLAB
       ;;kotlin            ; a better, slicker Java(Script)
       (latex +fold)             ; writing papers in Emacs has never been so fun
       ;;lean
       ;;factor
       ledger            ; an accounting system in Emacs
       (lua +lsp +tree-sitter +fennel) ; one-based indices? one-based indices
       markdown          ; writing docs for people to ignore
       ;;nim               ; python + lisp at the speed of c
       ;;nix               ; I hereby declare "nix geht mehr!"
       ;;ocaml             ; an objective camel
       (org
        +attach
        +babel
        +capture
        +crypt
        +journal
        +export
        +present
        +roam2
        +pretty
        +noter
        +dragndrop
        +gnuplot
        +pandoc)
       (php +lsp +tree-sitter) ; perl's insecure younger brother
       plantuml          ; diagrams for confusing people more
       ;;purescript        ; javascript, but functional
       (python +lsp +tree-sitter +pyright +pyenv)
       ;;qt                ; the 'cutest' gui framework ever
       ;;racket            ; a DSL for DSLs
       ;;raku              ; the artist formerly known as perl6
       (rest +jq)          ; Emacs as a REST client
       ;;rst               ; ReST in peace
       (ruby +rails +lsp +tree-sitter) ; 1.step {|i| p "Ruby is #{i.even? ? 'love' : 'life'}"}
       (rust +lsp +tree-sitter) ; Fe2O3.unwrap().unwrap().unwrap().unwrap()
       ;;scala             ; java, but good
       ;;scheme            ; a fully conniving family of lisps
       (sh +lsp +tree-sitter) ; she sells {ba,z,fi}sh shells on the C xor
       ;;sml
       ;;solidity          ; do you need a blockchain? No.
       (swift +lsp +tree-sitter) ; who asked for emoji variables?
       ;;terra             ; Earth and Moon in alignment for performance.
       (web +html +lsp +tree-sitter)               ; the tubes
       (yaml +lsp +tree-sitter) ; JSON, but readable

       :email
       (mu4e +gmail +org)
       ;;notmuch
       ;;(wanderlust +gmail)

       :app
       calendar
       everywhere        ; *leave* Emacs!? You must be joking
       irc               ; how neckbeards socialize
       (rss +org)        ; emacs as an RSS reader
       ;;twitter           ; twitter client https://twitter.com/vnought

       :config
       ;;literate
       (default +bindings +smartparens +evil-commands))
