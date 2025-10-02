---
layout: default
title: "Chapter 8: Development Environment"
---

# Chapter 8: Setting Up Your Development Environment

> "A language that doesn't affect the way you think about programming is not worth knowing." - Alan Perlis

But a language without proper tooling is not worth using. The difference between fighting Lisp and flowing with it is your development environment. Set up correctly, Lisp code doesn't just write itself—it refactors itself, debugs itself, and occasionally seems to understand what you meant better than you did.

This chapter is about achieving that flow state where the boundary between thought and code disappears.

## The Ultimate Lisp Development Setup

No matter which Lisp you choose, certain tools are universal. Here's your essential toolkit for 2025:

### 1. Structural Editing (Paredit/Parinfer/Smartparens)

Forget counting parentheses. Forget manual balancing. Structural editing means you manipulate code as a tree, not as text:

```lisp
;; Before: cursor at |
(defn process [data|])

;; After slurp: bring next expression inside
(defn process [data])|

;; After wrap: surround with new parens
(defn process [(data)])

;; After barf: push last expression outside
(defn process [](data))

;; After splice: remove surrounding parens
(defn process [] data)
```

### 2. REPL Integration

The REPL isn't a separate tool—it's integrated into your editor:

```lisp
;; Write code in your editor
(defn calculate-interest [principal rate time]
  (* principal rate time))

;; Evaluate it without leaving editor (C-c C-c in most setups)
;; Function is now available in REPL

;; Test it immediately (C-c C-e evaluates at point)
(calculate-interest 1000 0.05 2)  ; => 100

;; Redefine on the fly
(defn calculate-interest [principal rate time]
  (let [simple (* principal rate time)]
    {:simple simple
     :compound (* principal (Math/pow (+ 1 rate) time))}))

;; Test new version immediately
(calculate-interest 1000 0.05 2)
;; => {:simple 100, :compound 1102.5}
```

### 3. Inline Documentation

Documentation at your fingertips:

```lisp
;; Place cursor on any function and press key combo
;; (Usually C-c C-d d or similar)

map| ; Cursor here
;; Shows:
;; map
;; ([f] [f coll] [f c1 c2] [f c1 c2 c3] [f c1 c2 c3 & colls])
;; Returns a lazy sequence of the results of applying f to
;; the items of coll(s)...
```

### 4. Jump to Definition

Navigate code like thought:

```lisp
;; Cursor on function name, press M-. (or equivalent)
(process-data input)
;;  ^--- Jump directly to where process-data is defined

;; Jump to library source
(map inc [1 2 3])
;; ^--- See how map is implemented in the core library
```

## Editor Configurations

### Emacs: The Classical Choice

Emacs remains the gold standard for Lisp development. Here's a modern Emacs setup for all Lisps:

```elisp
;;; init.el --- Universal Lisp Development Setup

;; Package management
(require 'package)
(setq package-archives
      '(("melpa" . "https://melpa.org/packages/")
        ("gnu" . "https://elpa.gnu.org/packages/")))
(package-initialize)

;; Bootstrap use-package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; Essential packages for Lisp development
(use-package paredit
  :ensure t
  :hook ((emacs-lisp-mode . paredit-mode)
         (lisp-mode . paredit-mode)
         (scheme-mode . paredit-mode)
         (clojure-mode . paredit-mode))
  :config
  ;; Custom keybindings for paredit
  (define-key paredit-mode-map (kbd "M-[") 'paredit-wrap-square)
  (define-key paredit-mode-map (kbd "M-{") 'paredit-wrap-curly))

;; Rainbow delimiters for visual matching
(use-package rainbow-delimiters
  :ensure t
  :hook (prog-mode . rainbow-delimiters-mode))

;; Common Lisp development with SLIME
(use-package slime
  :ensure t
  :config
  (setq inferior-lisp-program "sbcl")
  (setq slime-contribs '(slime-fancy slime-company))
  (slime-setup))

;; Clojure development with CIDER
(use-package cider
  :ensure t
  :config
  (setq cider-repl-display-help-banner nil)
  (setq cider-repl-pop-to-buffer-on-connect 'display-only)
  (setq cider-show-error-buffer nil)
  (setq cider-auto-select-error-buffer nil)
  (setq cider-repl-history-file "~/.cider-repl-history")
  (setq cider-repl-wrap-history t))

;; Scheme development with Geiser
(use-package geiser
  :ensure t
  :config
  (setq geiser-active-implementations '(guile racket)))

;; Auto-completion for all Lisps
(use-package company
  :ensure t
  :hook (after-init . global-company-mode)
  :config
  (setq company-idle-delay 0.2)
  (setq company-minimum-prefix-length 1))

;; Aggressive indentation for Lisp
(use-package aggressive-indent
  :ensure t
  :hook ((emacs-lisp-mode . aggressive-indent-mode)
         (lisp-mode . aggressive-indent-mode)
         (clojure-mode . aggressive-indent-mode)))

;; Which-key for discovering commands
(use-package which-key
  :ensure t
  :config
  (which-key-mode))

;; Projectile for project management
(use-package projectile
  :ensure t
  :config
  (projectile-mode +1)
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map))

;; Custom functions for Lisp development
(defun eval-and-replace ()
  "Evaluate sexp at point and replace it with the result."
  (interactive)
  (backward-kill-sexp)
  (condition-case nil
      (prin1 (eval (read (current-kill 0)))
             (current-buffer))
    (error (message "Invalid expression")
           (insert (current-kill 0)))))

(global-set-key (kbd "C-c e") 'eval-and-replace)

;; Lisp-specific configuration
(defun my-lisp-mode-setup ()
  "Custom setup for Lisp modes."
  (setq-local show-paren-style 'expression)
  (setq-local electric-pair-inhibit-predicate 'electric-pair-conservative-inhibit))

(add-hook 'lisp-mode-hook 'my-lisp-mode-setup)
(add-hook 'emacs-lisp-mode-hook 'my-lisp-mode-setup)
(add-hook 'clojure-mode-hook 'my-lisp-mode-setup)
(add-hook 'scheme-mode-hook 'my-lisp-mode-setup)
```

### VS Code: The Modern Choice

VS Code has excellent Lisp support through extensions:

```json
// .vscode/settings.json
{
  // Calva settings for Clojure
  "calva.prettyPrintingOptions": {
    "printLevel": 50,
    "lengthLimit": 80
  },
  "calva.evalOnSave": true,
  "calva.fmt.formatAsYouType": true,

  // Bracket pair colorization
  "editor.bracketPairColorization.enabled": true,
  "editor.guides.bracketPairs": "active",

  // Common Lisp settings
  "alive.format.indentWidth": 2,
  "alive.enableAutoIndent": true,

  // General settings for Lisp
  "editor.autoClosingBrackets": "never",
  "editor.autoClosingQuotes": "never",
  "editor.formatOnSave": true,
  "editor.wordWrap": "on",

  // File associations
  "files.associations": {
    "*.lisp": "lisp",
    "*.cl": "lisp",
    "*.clj": "clojure",
    "*.cljs": "clojurescript",
    "*.cljc": "clojure",
    "*.scm": "scheme",
    "*.rkt": "racket"
  }
}
```

Essential VS Code extensions:
- **Calva**: Clojure & ClojureScript
- **Alive**: Common Lisp
- **Racket**: Racket/Scheme
- **Parinfer**: Structural editing
- **Rainbow Brackets**: Visual matching

### Vim/Neovim: The Power User's Choice

For those who've taken both pills (Vim and Lisp):

```vim
" init.vim or .vimrc

" Plugin management with vim-plug
call plug#begin()

" Lisp development plugins
Plug 'guns/vim-sexp'                          " S-expression manipulation
Plug 'tpope/vim-sexp-mappings-for-regular-people'
Plug 'junegunn/rainbow_parentheses.vim'       " Rainbow parens
Plug 'Olical/conjure'                         " REPL integration
Plug 'tpope/vim-fireplace'                    " Clojure support
Plug 'vlime/vlime'                           " Common Lisp support
Plug 'kovisoft/slimv'                        " Alternative Lisp support
Plug 'eraserhd/parinfer-rust'                " Parinfer mode

" General development
Plug 'neoclide/coc.nvim', {'branch': 'release'}
Plug 'tpope/vim-fugitive'                    " Git integration
Plug 'junegunn/fzf', { 'do': { -> fzf#install() } }
Plug 'junegunn/fzf.vim'

call plug#end()

" Lisp-specific settings
autocmd FileType lisp,clojure,scheme setlocal expandtab shiftwidth=2 softtabstop=2

" Enable rainbow parentheses
autocmd VimEnter * RainbowParentheses

" Conjure mappings for REPL interaction
let g:conjure#mapping#doc_word = "K"
let g:conjure#mapping#def_word = "gd"
let g:conjure#mapping#eval_current_form = "<localleader>ee"
let g:conjure#mapping#eval_root_form = "<localleader>er"
let g:conjure#mapping#eval_file = "<localleader>ef"

" Parinfer configuration
let g:parinfer_mode = 'smart'
let g:parinfer_enabled = 1

" Custom functions
function! EvalAndReplace()
  " Evaluate expression and replace with result
  normal! yab
  let result = system('clojure -e "' . escape(@", '"') . '"')
  normal! vab"_c
  execute "normal! i" . substitute(result, '\n$', '', '')

nnoremap <leader>er :call EvalAndReplace()<CR>
```

## REPL Integration Across Lisps

Each Lisp has its own REPL story, but the workflow is similar:

### Common Lisp with SLIME

```lisp
;; Start SLIME: M-x slime

;; Compile and load file: C-c C-k
;; Compile defun: C-c C-c
;; Evaluate last expression: C-x C-e
;; Macroexpand: C-c C-m

;; Interactive debugging
(defun buggy-function (x)
  (/ 1 x))  ; Will error when x = 0

(buggy-function 0)
;; Debugger opens with restart options:
;; 0: [RETRY] Retry SLIME REPL evaluation request.
;; 1: [*ABORT] Return to SLIME's top level.
;; 2: [ABORT] abort thread

;; Inspect values in debugger
;; Navigate stack frames
;; Fix and continue!
```

### Clojure with CIDER

```clojure
;; Start REPL: M-x cider-jack-in

;; Evaluate buffer: C-c C-k
;; Evaluate form: C-c C-e
;; Evaluate and print: C-c C-p
;; Evaluate and pretty-print: C-c C-f

;; Interactive debugging with #dbg
#dbg
(defn process-data [data]
  (-> data
      (filter valid?)
      (map transform)
      (reduce accumulate)))

;; Step through each transformation
;; See intermediate values
;; Modify and continue
```

### Scheme with Geiser

```scheme
;; Start Geiser: M-x run-geiser

;; Evaluate definition: C-c C-d C-d
;; Evaluate region: C-c C-r
;; Evaluate buffer: C-c C-b

;; Module navigation
;; Jump to module: C-c C-d C-m
;; Show module docs: C-c C-d C-i
```

## Debugging and Profiling Tools

### Time and Space Profiling

```lisp
;; Common Lisp
(time (factorial 10000))
; Evaluation took:
;   0.003 seconds of real time
;   0.003125 seconds of total run time
;   2,048 bytes consed

;; Clojure
(time (reduce * (range 1 10001)))
; "Elapsed time: 3.245 msecs"

;; Profile with criterium (Clojure)
(use 'criterium.core)
(bench (process-large-dataset data))
; Execution time mean : 1.2 ms
; Execution time std-deviation : 0.05 ms
```

### Interactive Debugging

```lisp
;; Set breakpoints in Common Lisp
(defun calculate-complex [x y]
  (break "Debugging calculate-complex with x=~A y=~A" x y)
  (* x y))

;; Clojure with debugging macros
(defn process-pipeline [data]
  (doto data
    (println "Initial:")
    (->> (map transform)
         (println "After transform:"))
    (->> (filter valid?)
         (println "After filter:"))))
```

## Project Structure Best Practices

### Common Lisp Project

```
my-cl-project/
├── my-project.asd          # ASDF system definition
├── package.lisp            # Package definitions
├── src/
│   ├── main.lisp
│   └── utils.lisp
├── tests/
│   └── test-main.lisp
└── README.md

;; my-project.asd
(defsystem "my-project"
  :version "0.1.0"
  :author "Your Name"
  :license "MIT"
  :depends-on ("alexandria" "cl-ppcre")
  :components ((:module "src"
                :components
                ((:file "package")
                 (:file "utils")
                 (:file "main"))))
  :in-order-to ((test-op (test-op "my-project/tests"))))
```

### Clojure Project

```
my-clj-project/
├── deps.edn               # Or project.clj for Leiningen
├── src/
│   └── my_project/
│       ├── core.clj
│       └── utils.clj
├── test/
│   └── my_project/
│       └── core_test.clj
├── resources/
└── README.md

;; deps.edn
{:paths ["src" "resources"]
 :deps {org.clojure/clojure {:mvn/version "1.11.1"}}
 :aliases
 {:test {:extra-paths ["test"]
         :extra-deps {lambdaisland/kaocha {:mvn/version "1.0.861"}}}
  :dev {:extra-deps {cider/cider-nrepl {:mvn/version "0.28.5"}}}}}
```

## Essential Keyboard Shortcuts

Master these universal Lisp editing commands:

### Structural Navigation
- **Forward s-expression**: `C-M-f`
- **Backward s-expression**: `C-M-b`
- **Up s-expression**: `C-M-u`
- **Down s-expression**: `C-M-d`
- **Beginning of defun**: `C-M-a`
- **End of defun**: `C-M-e`

### Structural Editing
- **Slurp forward**: `C-)`
- **Barf forward**: `C-}`
- **Slurp backward**: `C-(`
- **Barf backward**: `C-{`
- **Splice**: `M-s`
- **Split**: `M-S`
- **Join**: `M-J`
- **Wrap with parens**: `M-(`
- **Raise expression**: `M-r`

### REPL Interaction
- **Eval last expression**: `C-x C-e`
- **Eval defun**: `C-M-x`
- **Eval region**: `C-c C-r`
- **Load file**: `C-c C-k`
- **Switch to REPL**: `C-c C-z`
- **Interrupt evaluation**: `C-c C-c`

## Advanced Tooling

### Language Servers

Modern Lisp development increasingly uses Language Server Protocol:

```bash
# Clojure LSP
brew install clojure-lsp

# Common Lisp (experimental)
ros install lem-project/lem

# Scheme LSP
npm install -g scheme-language-server
```

### Build Tools and Task Runners

```clojure
;; Babashka for scripting (bb.edn)
{:tasks
 {:init (println "Running tasks...")

  test {:doc "Run tests"
        :task (shell "clojure -M:test")}

  build {:doc "Build uberjar"
         :task (shell "clojure -T:build uber")}

  deploy {:doc "Deploy to production"
          :depends [test build]
          :task (shell "deploy.sh")}}}

;; Run with: bb deploy
```

### Documentation Generation

```lisp
;; Common Lisp with documentation strings
(defun calculate-interest (principal rate time)
  "Calculate simple interest.
   PRINCIPAL: Initial amount
   RATE: Interest rate (as decimal)
   TIME: Time period
   Returns: Interest amount"
  (* principal rate time))

;; Generate docs with: (document-package :my-package)

;; Clojure with codox
{:codox {:output-path "docs"
         :source-uri "https://github.com/user/project/blob/{version}/{filepath}#L{line}"}}
```

## Performance Monitoring

```clojure
;; Visual profiling with flame graphs
(require '[clj-async-profiler.core :as prof])

(prof/profile
  (dotimes [_ 10000]
    (process-data large-dataset)))

(prof/serve-files 8080) ; View flame graph at localhost:8080
```

## The Development Flow

The perfect Lisp development flow looks like this:

1. **Write a test** in your REPL
2. **Implement the function** incrementally
3. **Evaluate each change** immediately
4. **Refactor structurally** with paredit
5. **Debug interactively** when issues arise
6. **Profile if needed** without leaving environment
7. **Commit from editor** with magit or fugitive

This isn't a compile-run-debug cycle. It's a conversation with your code.

## Environment Variables and Configuration

```bash
# ~/.bashrc or ~/.zshrc

# Common Lisp
export SBCL_HOME=/usr/local/lib/sbcl
export PATH=$PATH:~/.roswell/bin

# Clojure
export CLJ_CONFIG=~/.clojure
export JAVA_OPTS="-Xmx4g -XX:+UseG1GC"

# Scheme
export GUILE_LOAD_PATH=~/.guile/lib

# Universal
export EDITOR=emacs
alias repl='rlwrap sbcl'  # Better REPL experience
```

## The Zen of Lisp Development

The best Lisp development environment is invisible. You don't think about parentheses because paredit handles them. You don't think about compilation because evaluation is instant. You don't think about debugging because you test as you write.

When your environment is properly configured:
- Code flows from thought to screen
- Refactoring is structural, not textual
- Testing happens continuously, not afterwards
- Debugging is exploration, not archaeology
- Documentation is inline, not separate

This is why Lispers are so particular about their setups. It's not about the tools—it's about achieving flow state. When you get there, programming becomes meditation with parentheses.

---

*"The best tool is the one that becomes invisible through mastery." - Unknown*

Next: Practical projects that work across Lisps. We'll build the same application in different dialects, share code between them, and see how the same ideas express themselves in different flavors of the One True Language.