# Chapter 3: Common Lisp - The Industrial Strength Lisp

> "Common Lisp is politics, not art." - Scott Fahlman

Common Lisp is the B-52 bomber of programming languages—designed by committee in the 1980s, looks dated, and yet it's still flying missions that would ground newer aircraft. It's the language that runs credit card processing systems, airline reservation systems, and quantum computer compilers. It's ugly, it's beautiful, it's practical, and it's still ahead of its time.

If Scheme is a haiku and Clojure is a startup pitch, Common Lisp is the Encyclopædia Britannica—comprehensive, authoritative, and surprisingly fun to browse. It's the Lisp that said "yes" to every feature request and somehow made it all work together.

## Setting Up SBCL and Quicklisp

Let's start by getting a modern Common Lisp environment running. In 2025, the ecosystem has matured beautifully. We'll use SBCL (Steel Bank Common Lisp), the fastest and most actively developed open-source implementation, and Quicklisp, the package manager that finally made Common Lisp libraries easy to use.

### Installing SBCL

On macOS:
```bash
brew install sbcl
```

On Ubuntu/Debian:
```bash
sudo apt-get install sbcl
```

On Windows (yes, Common Lisp runs on Windows):
```bash
# Download the installer from sbcl.org
# Or use WSL2 and follow the Linux instructions
```

From source (for the brave):
```bash
git clone git://git.code.sf.net/p/sbcl/sbcl
cd sbcl
sh make.sh --fancy
sudo sh install.sh
```

The `--fancy` flag enables all the fun experimental features. Common Lisp programmers like their features fancy.

### Installing Quicklisp

Quicklisp is to Common Lisp what npm is to Node.js, except it actually works reliably. Here's the modern installation ritual:

```bash
cd ~/
curl -O https://beta.quicklisp.org/quicklisp.lisp
sbcl --load quicklisp.lisp
```

Then in the SBCL REPL:
```common-lisp
(quicklisp-quickstart:install)
(ql:add-to-init-file)
(quit)
```

Congratulations! You now have access to over 2,000 Common Lisp libraries. Let's install some essentials:

```common-lisp
;; Start SBCL again
sbcl

;; Install some powerful libraries
(ql:quickload :alexandria)      ; Utility functions
(ql:quickload :cl-ppcre)        ; Regular expressions that don't suck
(ql:quickload :hunchentoot)     ; Web server
(ql:quickload :cl-who)          ; HTML generation
(ql:quickload :postmodern)      ; PostgreSQL interface
(ql:quickload :bordeaux-threads) ; Portable threading
```

### Your First Common Lisp REPL Session

Let's explore what makes Common Lisp special:

```common-lisp
;; The REPL remembers everything
CL-USER> (defparameter *my-data* '(1 2 3 4 5))
*MY-DATA*

CL-USER> (mapcar #'(lambda (x) (* x x)) *my-data*)
(1 4 9 16 25)

;; Multiple return values - because sometimes one isn't enough
CL-USER> (floor 10 3)
3
1

;; Capture both values
CL-USER> (multiple-value-bind (quotient remainder)
           (floor 10 3)
           (format t "10 ÷ 3 = ~A remainder ~A~%" quotient remainder))
10 ÷ 3 = 3 remainder 1
NIL

;; The FORMAT function - printf on steroids
CL-USER> (format t "~R cats, ~:R place, ~@R emperor~%"
                 42 2 4)
forty-two cats, second place, IV emperor
NIL
```

That FORMAT function? It has over 30 different directives. You can output numbers in Roman numerals, English words, binary, octal, hexadecimal, or as monetary amounts. It can pluralize words, justify text, and iterate over lists. It's Turing-complete. Yes, you can write programs in FORMAT strings. No, you shouldn't. But you could.

## CLOS: The Most Powerful Object System You've Never Used

Common Lisp Object System (CLOS) isn't just an object system—it's the object system that makes others look like toys. Multiple inheritance? Check. Multiple dispatch? Check. Method combinations? Check. Meta-object protocol? Check. The ability to change an object's class at runtime? Why not?

Let's build something real to see CLOS in action:

```common-lisp
;; Define classes - notice no methods inside!
(defclass account ()
  ((id :initarg :id :reader account-id)
   (balance :initarg :balance :accessor account-balance)
   (holder :initarg :holder :accessor account-holder)))

(defclass savings-account (account)
  ((interest-rate :initarg :interest-rate
                  :accessor interest-rate
                  :initform 0.02)))

(defclass checking-account (account)
  ((overdraft-limit :initarg :overdraft-limit
                    :accessor overdraft-limit
                    :initform 100)))

;; Methods are separate from classes - and can dispatch on multiple arguments
(defgeneric withdraw (account amount))

(defmethod withdraw ((account account) amount)
  (if (>= (account-balance account) amount)
      (progn
        (decf (account-balance account) amount)
        (format t "Withdrew $~,2F. New balance: $~,2F~%"
                amount (account-balance account))
        amount)
      (error "Insufficient funds!")))

(defmethod withdraw ((account checking-account) amount)
  ;; Checking accounts can overdraft
  (let ((available (+ (account-balance account)
                      (overdraft-limit account))))
    (if (>= available amount)
        (progn
          (decf (account-balance account) amount)
          (format t "Withdrew $~,2F. New balance: $~,2F~%"
                  amount (account-balance account))
          amount)
        (error "Exceeds overdraft limit!"))))

;; Method combinations - :before, :after, and :around methods
(defmethod withdraw :before ((account account) amount)
  (format t "~%Attempting to withdraw $~,2F from account ~A~%"
          amount (account-id account)))

(defmethod withdraw :after ((account account) amount)
  (when (< (account-balance account) 100)
    (format t "WARNING: Low balance!~%")))

;; Multiple dispatch - the method depends on ALL arguments
(defgeneric transfer (from to amount))

(defmethod transfer ((from account) (to account) amount)
  (withdraw from amount)
  (incf (account-balance to) amount)
  (format t "Transferred $~,2F~%" amount))

(defmethod transfer ((from savings-account) (to checking-account) amount)
  ;; Special handling for savings-to-checking transfers
  (when (> amount 10000)
    (format t "Large transfer flagged for review.~%"))
  (call-next-method))  ; Then do the normal transfer
```

Let's see it in action:

```common-lisp
;; Create some accounts
(defparameter *savings*
  (make-instance 'savings-account
                 :id "SAV-001"
                 :balance 5000
                 :holder "Alice"))

(defparameter *checking*
  (make-instance 'checking-account
                 :id "CHK-001"
                 :balance 500
                 :holder "Bob"
                 :overdraft-limit 200))

;; Use them
CL-USER> (transfer *savings* *checking* 15000)
Attempting to withdraw $15000.00 from account SAV-001
Large transfer flagged for review.
Withdrew $15000.00. New balance: $-10000.00
WARNING: Low balance!
Transferred $15000.00
```

But wait, there's more! CLOS has a Meta-Object Protocol (MOP) that lets you change how the object system itself works:

```common-lisp
;; Define a metaclass that logs all slot access
(defclass logged-class (standard-class) ())

(defmethod sb-mop:slot-value-using-class :around
    ((class logged-class) object slot)
  (let ((value (call-next-method)))
    (format t "~&Reading slot ~A: ~A~%"
            (sb-mop:slot-definition-name slot) value)
    value))

;; Use it
(defclass logged-account (account)
  ()
  (:metaclass logged-class))

;; Now all slot access is logged automatically
```

This is metaprogramming at a level most languages can't even conceive of. You're not just writing programs; you're modifying the programming language itself.

## The Condition System: Exception Handling for Adults

Common Lisp's condition system makes try/catch look like finger painting. Instead of just throwing and catching, you can:

1. Signal a condition
2. Let handlers up the stack decide what to do
3. Provide multiple recovery strategies (restarts)
4. Continue execution from where the error occurred

Watch this:

```common-lisp
(define-condition insufficient-funds (error)
  ((account :initarg :account :reader account)
   (amount :initarg :amount :reader amount)
   (balance :initarg :balance :reader balance))
  (:report (lambda (condition stream)
             (format stream "Cannot withdraw ~A from account with balance ~A"
                     (amount condition) (balance condition)))))

(defun safe-withdraw (account amount)
  (restart-case
      (if (>= (account-balance account) amount)
          (progn
            (decf (account-balance account) amount)
            amount)
          (error 'insufficient-funds
                 :account account
                 :amount amount
                 :balance (account-balance account)))
    (use-overdraft (overdraft-amount)
      :report "Use overdraft protection"
      :interactive (lambda ()
                     (format t "Enter overdraft amount: ")
                     (list (read)))
      (decf (account-balance account) (+ amount overdraft-amount))
      amount)
    (reduce-amount (new-amount)
      :report "Withdraw a smaller amount"
      :interactive (lambda ()
                     (format t "Enter new amount: ")
                     (list (read)))
      (safe-withdraw account new-amount))
    (abort ()
      :report "Cancel the withdrawal"
      nil)))

;; Now you can handle errors interactively
(handler-bind ((insufficient-funds
                 (lambda (c)
                   (format t "~&Not enough money! Balance: ~A~%"
                           (balance c))
                   ;; Choose a restart programmatically
                   (invoke-restart 'reduce-amount
                                   (balance c)))))
  (safe-withdraw *checking* 1000))
```

This isn't just error handling; it's a negotiation between the code that detects the problem and the code that knows how to fix it. The error site provides options, the handler chooses solutions. It's like having a conversation with your errors instead of just getting punched in the face by them.

## Practical Common Lisp Projects

Let's build something real: a simple web service with Hunchentoot. This will show Common Lisp in its natural habitat—building robust, long-running services:

```common-lisp
(ql:quickload '(:hunchentoot :cl-who :cl-json :local-time))

;; Define our web application
(defpackage :my-web-app
  (:use :cl :hunchentoot :cl-who))

(in-package :my-web-app)

;; Some "database" (in production, use Postmodern or similar)
(defparameter *todos*
  (list (list :id 1 :task "Learn Common Lisp" :done nil)
        (list :id 2 :task "Build something cool" :done nil)
        (list :id 3 :task "Question reality" :done t)))

(defparameter *next-id* 4)

;; HTML generation with CL-WHO
(defmacro with-html-output-to-string (&body body)
  `(with-html-output-to-string (*standard-output* nil :prologue t)
     ,@body))

;; Define routes
(define-easy-handler (index :uri "/") ()
  (with-html-output-to-string
    (:html
     (:head
      (:title "Common Lisp TODO")
      (:style "body { font-family: sans-serif; max-width: 600px; margin: 0 auto; }
               .done { text-decoration: line-through; opacity: 0.5; }
               .todo-item { padding: 10px; border: 1px solid #ddd; margin: 5px 0; }"))
     (:body
      (:h1 "Common Lisp TODO App")
      (:div :id "todos"
       (dolist (todo *todos*)
         (htm
          (:div :class (format nil "todo-item ~A"
                               (if (getf todo :done) "done" ""))
           (:span (str (getf todo :task)))
           (:button :onclick (format nil "toggleTodo(~A)"
                                     (getf todo :id))
                    (str (if (getf todo :done) "Undo" "Done")))))))
      (:form :method "post" :action "/add"
       (:input :type "text" :name "task" :placeholder "New task...")
       (:button :type "submit" "Add"))
      (:script
       "function toggleTodo(id) {
          fetch('/toggle/' + id, {method: 'POST'})
            .then(() => location.reload());
        }")))))

(define-easy-handler (add-todo :uri "/add") (task)
  (when task
    (push (list :id *next-id* :task task :done nil) *todos*)
    (incf *next-id*))
  (redirect "/"))

(define-easy-handler (toggle-todo :uri "/toggle/:id") ()
  (let* ((id (parse-integer (nth 1 (split-sequence:split-sequence
                                     #\/ (request-uri*)))))
         (todo (find id *todos* :key (lambda (x) (getf x :id)))))
    (when todo
      (setf (getf todo :done) (not (getf todo :done)))))
  "OK")

;; API endpoint returning JSON
(define-easy-handler (api-todos :uri "/api/todos") ()
  (setf (content-type*) "application/json")
  (cl-json:encode-json-to-string *todos*))

;; Start the server
(defparameter *server* (start (make-instance 'easy-acceptor :port 8080)))

;; Visit http://localhost:8080
```

This is a complete web application in about 70 lines of code. It serves HTML, handles forms, provides a JSON API, and manages state. In production, you'd add a real database, authentication, and more sophisticated routing, but the structure would remain this clean.

## When to Choose Common Lisp

Common Lisp shines when you need:

**Exploratory Programming**: When you don't know what you're building yet. Common Lisp's interactive development and ability to redefine anything makes it perfect for research and prototyping.

**Long-Running Services**: Common Lisp processes can run for years. You can update them while they run, debug them while they serve traffic, and fix bugs without restarting. NASA uses Common Lisp for Deep Space 1 mission planning. When your software is millions of miles away, you can't turn it off and on again.

**Domain-Specific Languages**: Common Lisp's macro system is unmatched for building DSLs. Whether it's a query language, a rule system, or a specialized notation for your problem domain, Common Lisp makes it easy.

**Complex Systems**: When your problem is genuinely hard—theorem proving, symbolic computation, planning and scheduling, natural language processing—Common Lisp's expressiveness pays dividends.

**Performance-Critical Code**: With type declarations and a good compiler (SBCL), Common Lisp can approach C speeds while maintaining high-level abstractions:

```common-lisp
(declaim (optimize (speed 3) (safety 0)))

(defun dot-product (a b)
  (declare (type (simple-array double-float (*)) a b))
  (loop for x across a
        for y across b
        sum (* x y) double-float))
```

## The Common Lisp Workflow

Working in Common Lisp isn't like other languages. You don't write, compile, run, debug. You converse with a living system:

1. **Start your image**: Your Common Lisp image is your workshop. It contains all your code, data, and state.

2. **Build incrementally**: Write a function, test it immediately. Write another, test it with the first. Your program grows organically.

3. **Debug interactively**: When something breaks, you don't get a stack trace and exit. You get dropped into the debugger where you can inspect variables, change values, redefine functions, and continue execution.

4. **Save your image**: When everything works, save your entire Lisp image—code, data, state, everything—as an executable:

```common-lisp
(sb-ext:save-lisp-and-die "my-app"
  :toplevel #'my-main-function
  :executable t)
```

## The Dark Arts: Reader Macros and Compiler Macros

Common Lisp doesn't just let you extend the language semantically; you can extend its syntax too. Reader macros let you define new literal notations:

```common-lisp
;; Define a JSON-like syntax for hash tables
(set-dispatch-macro-character #\# #\{
  (lambda (stream char arg)
    (declare (ignore char arg))
    (let ((list (read-delimited-list #\} stream t)))
      `(alexandria:alist-hash-table
        (list ,@(loop for (key value) on list by #'cddr
                      collect `(cons ',key ,value)))))))

;; Now you can write:
(defparameter *data* #{name "Alice" age 30 city "Boston"})

;; Instead of:
(defparameter *data*
  (alexandria:alist-hash-table
    '((name . "Alice") (age . 30) (city . "Boston"))))
```

Compiler macros optimize specific function calls:

```common-lisp
(defun slow-sum (&rest numbers)
  (apply #'+ numbers))

(define-compiler-macro slow-sum (&rest numbers)
  ;; At compile time, if we know the arguments, optimize
  (if (every #'constantp numbers)
      (apply #'+ (mapcar #'eval numbers))
      `(+ ,@numbers)))

;; This call is optimized at compile time:
(slow-sum 1 2 3 4 5)  ; Becomes 15 at compile time
```

## The Common Lisp Community

The Common Lisp community is small but incredibly knowledgeable. These are people who've been writing Lisp for decades, who remember the Lisp Machines, who've seen every programming fad come and go.

Resources to know:

- **Common-Lisp.net**: The hub for Common Lisp projects
- **Planet Lisp**: Aggregator of Common Lisp blogs
- **Quicklisp**: Your gateway to libraries
- **r/Common_Lisp**: Active and helpful
- **#commonlisp on Libera.Chat**: Where the wizards hang out

Books to read:

- **Practical Common Lisp** by Peter Seibel (free online)
- **On Lisp** by Paul Graham (advanced macros)
- **ANSI Common Lisp** by Paul Graham (comprehensive introduction)
- **Let Over Lambda** by Doug Hoyte (mind-bending macro techniques)
- **The Art of the Metaobject Protocol** (if you want to truly understand CLOS)

## The Common Lisp Paradox

Here's the thing about Common Lisp: it's simultaneously ancient and futuristic. It has features that modern languages are just discovering (pattern matching, multiple dispatch, restartable exceptions) and features they haven't discovered yet (reader macros, the MOP, compiler macros).

It's the language that was standardized in 1994 and hasn't needed a major revision since. Not because it's dead, but because it got so much right the first time. The standard library might feel dated (no built-in JSON parsing!), but the language itself is timeless.

Common Lisp is the language you reach for when:
- The problem is harder than the tools
- You need to invent new abstractions
- The system needs to run forever
- You want to explore the limits of what's possible

It's not the easiest Lisp to learn (that's Scheme), or the most modern (that's Clojure), or the most widely deployed (that's Emacs Lisp). But it's the most powerful, the most complete, and in many ways, the most Lisp-y Lisp.

## A Living Fossil That Refuses to Fossilize

Common Lisp is often called a "dead language" by people who've never used it. Meanwhile, it's quietly running financial systems, airline reservations, CAD software, and yes, even modern web services. It's in quantum computer compilers (Rigetti's Quilc), computational biology (BioBike), and music composition (OpenMusic).

The secret is that Common Lisp isn't competing with other languages on their terms. It's not trying to be the next JavaScript or Python. It's the language for problems that are too hard for JavaScript or Python. It's the language you graduate to, not the one you start with.

And that's why it endures. Every generation of programmers rediscovers it, usually after beating their heads against the limitations of more "modern" languages. They come for the macros, stay for the interactive development, and become evangelists for the condition system.

Common Lisp is dead the same way Latin is dead—still used daily by professionals, still teaching us about the fundamentals of its domain, still influencing every descendant, and still the best choice for certain tasks.

Long live Common Lisp. May it outlive us all.

---

*"Lisp is the red pill." - John Fremlin*

Next up: Scheme, the minimalist masterpiece. Where Common Lisp said "yes" to everything, Scheme said "no" to everything except mathematical elegance. We'll explore how less can be more, why tail recursion matters, and how a 50-page specification can define a complete programming language. Prepare for purity.