# Chapter 5: Emacs Lisp - Your Editor is a Lisp Machine

> "Emacs is a great operating system, lacking only a decent editor." - Ancient Programmer Proverb

This joke misses the point entirely. Emacs isn't an editor that happens to be programmable—it's a Lisp machine that happens to edit text. You're not using an application; you're living inside a running Lisp program that you can modify, extend, and debug while you use it. It's the closest thing we have to the Lisp machines of legend, and it's been hiding in plain sight for over 40 years.

## Living Inside a Lisp REPL

When you start Emacs, you're not launching an editor. You're booting a Lisp environment. Every keystroke you type is evaluated by a Lisp interpreter. Every character you see was placed there by a Lisp function. That menu bar? Lisp. Syntax highlighting? Lisp. The blinking cursor? You guessed it—Lisp.

Let's prove it. Press `M-x` (Alt-x or Esc-x) and type `ielm` (Interactive Emacs Lisp Mode). You now have a REPL for the very environment you're using:

```elisp
*** Welcome to IELM ***  Type (describe-mode) for help.
ELISP> (+ 1 2)
3
ELISP> (buffer-name)
"*ielm*"
ELISP> (emacs-version)
"29.1"
ELISP> (message "Hello from inside Emacs!")
"Hello from inside Emacs!"  ; Also appears in minibuffer
```

But here's where it gets interesting. You can interrogate and modify your editor while you're using it:

```elisp
;; What buffers are open?
ELISP> (buffer-list)
(#<buffer *ielm*> #<buffer *scratch*> #<buffer *Messages*> ...)

;; What's the current cursor position?
ELISP> (point)
1423

;; Move the cursor
ELISP> (goto-char 1)
1  ; Cursor jumps to beginning of buffer!

;; Insert text at cursor
ELISP> (insert "I just programmed my editor to type this!")
nil  ; Text appears in the buffer!

;; Change the color scheme on the fly
ELISP> (load-theme 'modus-vivendi t)
t  ; Editor immediately switches to dark theme
```

You're not sending commands to an editor. You're calling functions in a live Lisp environment. There's no boundary between "the editor" and "the programming language"—they're the same thing.

## Configuring Emacs from Scratch

Your Emacs configuration file (`~/.emacs.d/init.el` or `~/.emacs`) isn't a config file—it's a program that runs every time Emacs starts. Let's build a modern configuration from scratch:

```elisp
;;; init.el --- My Emacs Configuration -*- lexical-binding: t -*-

;; Performance optimizations for startup
(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.6)

(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold (* 16 1024 1024)
                  gc-cons-percentage 0.1)))

;; Set up package management
(require 'package)
(setq package-archives
      '(("melpa" . "https://melpa.org/packages/")
        ("gnu" . "https://elpa.gnu.org/packages/")
        ("nongnu" . "https://elpa.nongnu.org/nongnu/")))
(package-initialize)

;; Bootstrap use-package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

;; Better defaults
(setq-default
 indent-tabs-mode nil                ; Spaces, not tabs
 tab-width 4                         ; 4 spaces per tab
 cursor-type 'bar                    ; Thin cursor
 show-paren-mode t                   ; Highlight matching parens
 column-number-mode t                ; Show column in modeline
 save-interprogram-paste-before-kill t  ; Save clipboard before replacing
 apropos-do-all t                    ; More extensive searches
 mouse-yank-at-point t               ; Paste at cursor, not mouse
 require-final-newline t             ; End files with newline
 visible-bell t                      ; Flash instead of beep
 load-prefer-newer t                 ; Load newer files
 backup-by-copying t                 ; Don't clobber symlinks
 frame-inhibit-implied-resize t      ; Don't resize frame
 custom-file (expand-file-name "custom.el" user-emacs-directory))

;; UI improvements
(when (fboundp 'tool-bar-mode) (tool-bar-mode -1))    ; No toolbar
(when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1)) ; No scrollbar
(when (fboundp 'menu-bar-mode) (menu-bar-mode -1))    ; No menu bar
(fset 'yes-or-no-p 'y-or-n-p)                          ; y/n instead of yes/no

;; Essential packages
(use-package vertico           ; Better minibuffer completion
  :init (vertico-mode))

(use-package marginalia        ; Annotations in minibuffer
  :init (marginalia-mode))

(use-package which-key         ; Show available keys
  :diminish which-key-mode
  :config
  (which-key-mode)
  (setq which-key-idle-delay 0.3))

(use-package magit             ; Git interface
  :bind ("C-x g" . magit-status))

(use-package company           ; Auto-completion
  :hook (after-init . global-company-mode)
  :config
  (setq company-idle-delay 0.2
        company-minimum-prefix-length 2))

;; Theme
(use-package doom-themes
  :config
  (load-theme 'doom-one t))

;; Modeline
(use-package doom-modeline
  :init (doom-modeline-mode))

;; The power of Lisp: define your own functionality
(defun my/open-config ()
  "Open my Emacs configuration file."
  (interactive)
  (find-file (expand-file-name "init.el" user-emacs-directory)))

(global-set-key (kbd "C-c c") 'my/open-config)

;; A more complex example: smart beginning of line
(defun my/smarter-move-beginning-of-line (arg)
  "Move point to first non-whitespace character or beginning of line.
Move point to the first non-whitespace character on this line.
If point is already there, move to the beginning of the line.
Effectively toggle between the first non-whitespace character and
the beginning of the line."
  (interactive "^p")
  (setq arg (or arg 1))
  (when (/= arg 1)
    (let ((line-move-visual nil))
      (forward-line (1- arg))))
  (let ((orig-point (point)))
    (back-to-indentation)
    (when (= orig-point (point))
      (move-beginning-of-line 1))))

(global-set-key [remap move-beginning-of-line]
                'my/smarter-move-beginning-of-line)

;; Load custom file if it exists
(when (file-exists-p custom-file)
  (load custom-file))

(provide 'init)
;;; init.el ends here
```

This configuration is code. Every line is evaluated. You can use loops, conditionals, define functions—anything you can do in Lisp, you can do in your config.

## Writing Your Own Modes and Packages

Emacs modes aren't plugins—they're Lisp programs that define how Emacs behaves for specific tasks. Let's write a simple mode for taking notes:

```elisp
;;; my-notes-mode.el --- A simple note-taking mode -*- lexical-binding: t -*-

(defgroup my-notes nil
  "Simple note-taking mode."
  :group 'text)

(defcustom my-notes-directory "~/notes/"
  "Directory where notes are stored."
  :type 'directory
  :group 'my-notes)

(defvar my-notes-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-n") 'my-notes-new-note)
    (define-key map (kbd "C-c C-t") 'my-notes-insert-timestamp)
    (define-key map (kbd "C-c C-l") 'my-notes-insert-link)
    (define-key map (kbd "C-c C-s") 'my-notes-search)
    map)
  "Keymap for my-notes-mode.")

(defface my-notes-timestamp
  '((t :foreground "cyan" :weight bold))
  "Face for timestamps in notes."
  :group 'my-notes)

(defface my-notes-link
  '((t :foreground "deep sky blue" :underline t))
  "Face for links in notes."
  :group 'my-notes)

(defvar my-notes-font-lock-keywords
  '(("\\[\\[\\([^]]+\\)\\]\\]" . 'my-notes-link)           ; [[links]]
    ("^\\*+ .*$" . 'font-lock-function-name-face)          ; Headers
    ("<[0-9-]+ [0-9:]+>" . 'my-notes-timestamp)            ; Timestamps
    ("TODO\\|DONE" . 'font-lock-warning-face))             ; Keywords
  "Font lock keywords for my-notes-mode.")

(defun my-notes-new-note (title)
  "Create a new note with TITLE."
  (interactive "sNote title: ")
  (let* ((filename (concat (format-time-string "%Y%m%d-%H%M%S-")
                           (replace-regexp-in-string "[^a-zA-Z0-9]" "-" title)
                           ".txt"))
         (filepath (expand-file-name filename my-notes-directory)))
    (unless (file-exists-p my-notes-directory)
      (make-directory my-notes-directory t))
    (find-file filepath)
    (insert (format "* %s\n" title))
    (my-notes-insert-timestamp)
    (insert "\n\n")
    (my-notes-mode)))

(defun my-notes-insert-timestamp ()
  "Insert current timestamp."
  (interactive)
  (insert (format "<%s>" (format-time-string "%Y-%m-%d %H:%M:%S"))))

(defun my-notes-insert-link ()
  "Insert a link to another note."
  (interactive)
  (let* ((notes (directory-files my-notes-directory nil "\\.txt$"))
         (note (completing-read "Link to note: " notes)))
    (insert (format "[[%s]]" note))))

(defun my-notes-search (query)
  "Search all notes for QUERY."
  (interactive "sSearch notes for: ")
  (let ((grep-command (format "grep -r '%s' %s" query my-notes-directory)))
    (grep grep-command)))

(defun my-notes-follow-link ()
  "Follow link at point."
  (interactive)
  (let ((link-regex "\\[\\[\\([^]]+\\)\\]\\]"))
    (when (thing-at-point-looking-at link-regex)
      (let ((filename (match-string 1)))
        (find-file (expand-file-name filename my-notes-directory))))))

;; Define the mode
(define-derived-mode my-notes-mode text-mode "MyNotes"
  "Major mode for simple note-taking.
\\{my-notes-mode-map}"
  (setq font-lock-defaults '(my-notes-font-lock-keywords))
  (setq-local comment-start "# ")
  (setq-local comment-end ""))

;; Auto-enable for .txt files in notes directory
(add-to-list 'auto-mode-alist
             (cons (concat (regexp-quote (expand-file-name my-notes-directory))
                           ".*\\.txt\\'")
                   'my-notes-mode))

;; Interactive command to start note-taking
(defun my-notes ()
  "Open notes directory in Dired."
  (interactive)
  (dired my-notes-directory))

(provide 'my-notes-mode)
;;; my-notes-mode.el ends here
```

Load this mode, and you have a complete note-taking system. But here's the key: you can modify it while you're using it. Don't like how timestamps look? Redefine the function. Want a new feature? Add it and evaluate it immediately.

## The Joy of Interactive Development

Emacs Lisp development is uniquely interactive. You don't write code and then run it—you evaluate expressions as you write them. Let's explore this workflow:

```elisp
;; Place cursor after any expression and press C-x C-e to evaluate

;; Start simple
(+ 1 2)  ; C-x C-e => 3

;; Define a function
(defun greet (name)
  (format "Hello, %s!" name))  ; C-x C-e

;; Test it immediately
(greet "World")  ; C-x C-e => "Hello, World!"

;; Not quite right? Redefine it
(defun greet (name)
  (format "Greetings, %s! Welcome to Emacs." name))  ; C-x C-e

;; Test again
(greet "World")  ; C-x C-e => "Greetings, World! Welcome to Emacs."

;; Create an interactive command
(defun greet-user ()
  (interactive)
  (let ((name (read-string "Your name: ")))
    (message (greet name))))  ; C-x C-e

;; Now you can run it with M-x greet-user

;; Bind it to a key immediately
(global-set-key (kbd "C-c h") 'greet-user)  ; C-x C-e

;; Press C-c h and your new command runs!
```

This isn't a development environment—it's a conversation with your editor. You're teaching it new tricks as you work.

## Advanced Emacs Lisp: Advice and Hooks

Emacs Lisp has powerful features for modifying existing behavior without changing the original code:

### Advice System

```elisp
;; Add behavior to existing functions
(defun my-save-message (orig-fun &rest args)
  "Notify when saving files."
  (message "Saving %s..." (buffer-name))
  (apply orig-fun args)
  (message "Saved %s!" (buffer-name)))

(advice-add 'save-buffer :around #'my-save-message)

;; Now every save shows messages!

;; Remove advice when done
(advice-remove 'save-buffer #'my-save-message)

;; More sophisticated: log all function calls
(defun log-function-call (orig-fun &rest args)
  "Log function calls for debugging."
  (let ((result (apply orig-fun args)))
    (with-current-buffer (get-buffer-create "*function-log*")
      (goto-char (point-max))
      (insert (format "%s: %S => %S\n"
                      (format-time-string "%H:%M:%S")
                      args
                      result)))
    result))

;; Debug any function
(advice-add 'some-problematic-function :around #'log-function-call)
```

### Hooks

```elisp
;; Hooks run at specific times
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Mode-specific hooks
(add-hook 'python-mode-hook
          (lambda ()
            (setq-local indent-tabs-mode nil)
            (setq-local tab-width 4)))

;; Create your own hooks
(defvar my-morning-hook nil
  "Hook run when I start Emacs in the morning.")

(defun my-morning-routine ()
  "My morning startup routine."
  (when (< (string-to-number (format-time-string "%H")) 12)
    (run-hooks 'my-morning-hook)))

(add-hook 'my-morning-hook
          (lambda ()
            (message "Good morning! Here's your agenda:")
            (org-agenda-list)))

(add-hook 'after-init-hook 'my-morning-routine)
```

## Real Power: Emacs as a Platform

Emacs isn't just for editing text. It's a platform for building text-based applications:

### Email Client (mu4e)
```elisp
(use-package mu4e
  :config
  (setq mu4e-maildir "~/Maildir"
        mu4e-get-mail-command "mbsync -a"
        mu4e-update-interval 300))
```

### RSS Reader (elfeed)
```elisp
(use-package elfeed
  :config
  (setq elfeed-feeds
        '("https://news.ycombinator.com/rss"
          "https://planet.lisp.org/rss20.xml")))
```

### Git Interface (magit)
```elisp
(use-package magit
  :bind ("C-x g" . magit-status)
  :config
  (setq magit-display-buffer-function
        #'magit-display-buffer-same-window-except-diff-v1))
```

### IRC Client (erc)
```elisp
(setq erc-server "irc.libera.chat"
      erc-nick "lisper"
      erc-autojoin-channels-alist
      '(("libera.chat" "#emacs" "#lisp")))
```

### Web Browser (eww)
```elisp
(setq eww-search-prefix "https://duckduckgo.com/html?q=")
(global-set-key (kbd "C-c w") 'eww)
```

Each of these isn't a plugin—it's Emacs Lisp code that extends Emacs into new domains. You can read email, browse the web, chat on IRC, manage git repositories, all without leaving your Lisp environment.

## The Org-Mode Phenomenon

Org-mode deserves special mention. It started as an outlining tool and evolved into a complete personal information manager, all in Emacs Lisp:

```org
;; Literate programming in Org-mode
#+BEGIN_SRC emacs-lisp
(defun fibonacci (n)
  (if (<= n 1)
      n
    (+ (fibonacci (- n 1))
       (fibonacci (- n 2)))))
#+END_SRC

;; Execute with C-c C-c, results appear below:
#+RESULTS:
: fibonacci

;; Create tables that calculate
| Item   | Price | Quantity | Total |
|--------+-------+----------+-------|
| Apples |  2.50 |        3 |  7.50 |
| Bread  |  3.00 |        2 |  6.00 |
|--------+-------+----------+-------|
| Total  |       |          | 13.50 |
#+TBLFM: $4=$2*$3::@5$4=vsum(@2..@4)

;; Export to HTML, LaTeX, Markdown, etc.
(org-export-dispatch)
```

Org-mode is 150,000+ lines of Emacs Lisp that turns plain text into a complete productivity system. It's a testament to what's possible when your editor is a Lisp machine.

## Debugging and Profiling

When your editor is a Lisp environment, debugging is a first-class experience:

```elisp
;; Enable debugging on error
(setq debug-on-error t)

;; Instrument a function for debugging
(defun buggy-function (x)
  (let ((result (* x 2)))
    (if (> result 10)
        (error "Result too large!")
      result)))

;; M-x edebug-defun on the function
;; Now step through with:
;; n - next
;; c - continue
;; i - step into
;; o - step out
;; e - eval expression
;; q - quit

;; Profile your code
(profiler-start 'cpu)
;; ... do something slow ...
(profiler-report)
(profiler-stop)

;; Benchmark expressions
(benchmark-run 1000
  (mapcar #'1+ (number-sequence 1 1000)))
;; => (0.004256 0 0.0)  ; elapsed time, GC count, GC time
```

## The Emacs Lisp Mindset

Programming in Emacs Lisp changes how you think about software:

**Everything is malleable**: There's no distinction between "system" and "user" code. You can redefine anything.

**Live programming**: Code changes take effect immediately. No compile-run-debug cycle.

**Text is the universal interface**: Everything is text, and text is programmable.

**Extensibility over features**: Instead of requesting features, you write them.

**Documentation is code**: Self-documenting code isn't a goal—it's built into the language.

```elisp
(defun my-function (arg)
  "This is the documentation string.
It appears when you ask for help on this function.
ARG is the argument we process."
  (interactive "p")  ; This makes it a command
  ;; Implementation here
  )

;; C-h f my-function shows the documentation
```

## Modern Emacs in 2025

Emacs in 2025 has evolved significantly:

- **Native Compilation**: Emacs 28+ compiles Elisp to native code
- **LSP Support**: Language Server Protocol integration
- **Tree-sitter**: Incremental parsing for better syntax highlighting
- **Pure GTK**: Native Wayland support
- **Emoji Support**: 🎉 Yes, really!

```elisp
;; Modern Emacs configuration
(when (and (fboundp 'native-comp-available-p)
           (native-comp-available-p))
  (message "Native compilation is available"))

;; Tree-sitter modes
(use-package treesit-auto
  :config
  (global-treesit-auto-mode))

;; LSP for multiple languages
(use-package lsp-mode
  :hook ((python-mode . lsp)
         (rust-mode . lsp)
         (go-mode . lsp))
  :commands lsp)
```

## Why Emacs Lisp Matters

Emacs Lisp isn't the most elegant Lisp (that's Scheme) or the most powerful (that's Common Lisp) or the most modern (that's Clojure). But it's the most successful Lisp in terms of daily users. Millions of people use Emacs Lisp every day, most without realizing it.

It matters because:

1. **It's practical**: Emacs Lisp solves real problems for real users every day.

2. **It's accessible**: You don't need to be a Lisp expert to customize Emacs.

3. **It's powerful**: When you need it, the full power of Lisp is there.

4. **It's enduring**: Emacs has been continuously developed since 1976. Your investment in learning it pays dividends for decades.

5. **It's a gateway drug**: Many programmers discover Lisp through Emacs and never look back.

## The Emacs Paradox

Emacs is simultaneously too much and never enough. It's an editor that's also an email client, a web browser, a psychiatrist (M-x doctor), and a tower of Hanoi game (M-x hanoi). It's criticized for doing too much, yet users always want it to do more.

This isn't feature creep—it's the natural result of giving users a programmable environment. When your editor is a Lisp machine, everything becomes possible, so everything gets built.

The paradox resolves when you realize Emacs isn't trying to be everything. It's trying to be nothing—a blank canvas that becomes whatever you need. The editor you use is the one you've programmed, consciously or not, through your configuration and usage.

## Your Journey with Emacs Lisp

Start small. Add one function to your config. Bind one key. Write one simple mode. Before you know it, you'll have crafted an environment perfectly suited to how you think and work.

The endgame isn't to master Emacs—it's to make Emacs an extension of your mind. When the boundary between thought and action disappears, when you can express ideas as fast as you can think them, when your tools adapt to you rather than the reverse—that's when you understand why people never leave Emacs.

Welcome to the church of Emacs. May your parentheses be balanced and your buffers never lost.

---

*"Emacs outshines all other editing software in approximately the same way that the noonday sun does the stars." - Neal Stephenson*

Next: Clojure, the Lisp that conquered the enterprise. We'll explore how Rich Hickey reimagined Lisp for the JVM, why immutability is liberation, and how to build systems that scale to millions of users while maintaining your sanity.