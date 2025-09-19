# Chapter 2: The Lisp Family Tree

> "The greatest single programming language ever designed." - Alan Kay on Lisp

Imagine a programming language so fundamental that it spawned an entire family tree of descendants, each carrying forward its DNA while adapting to new environments. That's the story of Lisp—not just a language, but a genus, with species evolved for every computational niche imaginable.

In this chapter, we'll trace the genealogy of Lisp from John McCarthy's original flash of insight to the diverse ecosystem thriving in 2025. Along the way, we'll meet the major players, understand the philosophical splits that created different branches, and learn why, after nearly seven decades, new Lisps are still being born.

## From McCarthy's Original Vision (1958)

The year is 1958. Eisenhower is president. NASA has just been established. And at MIT, a mathematician named John McCarthy is trying to create a practical programming language based on lambda calculus. He doesn't realize he's about to discover something fundamental.

McCarthy's insight was beautifully simple: What if we could represent programs using the same data structures the programs manipulate? His notation used lists (denoted by parentheses) and atoms (symbols and numbers). This S-expression (symbolic expression) notation wasn't meant to be the actual syntax—it was supposed to be replaced with something more conventional, M-expressions. But then something remarkable happened.

Steve Russell, one of McCarthy's grad students, looked at the eval function McCarthy had defined mathematically and said, essentially, "Hey, I can implement this." McCarthy's response was allegedly that it would be too much work. Russell did it anyway, and suddenly they had a Lisp interpreter written in Lisp itself. The theoretical had become practical, and M-expressions were forgotten. The parentheses were here to stay.

Here's McCarthy's original eval function, the heart of Lisp, in its mathematical glory:

```lisp
;; The original eval - the universal function
;; Note: This is McCarthy's original mathematical definition
(defun eval (e a)
  (cond
    ((atom e) (assoc e a))
    ((atom (car e))
     (cond
       ((eq (car e) 'quote) (cadr e))
       ((eq (car e) 'atom) (atom (eval (cadr e) a)))
       ((eq (car e) 'eq) (eq (eval (cadr e) a) (eval (caddr e) a)))
       ((eq (car e) 'car) (car (eval (cadr e) a)))
       ((eq (car e) 'cdr) (cdr (eval (cadr e) a)))
       ((eq (car e) 'cons) (cons (eval (cadr e) a) (eval (caddr e) a)))
       ((eq (car e) 'cond) (evcon (cdr e) a))
       (t (eval (cons (assoc (car e) a) (cdr e)) a))))
    ((eq (caar e) 'lambda)
     (eval (caddar e) (append (pair (cadar e) (evlis (cdr e) a)) a)))))

;; Required helper functions for the original eval
(defun evcon (c a)
  (cond ((eval (caar c) a) (eval (cadar c) a))
        (t (evcon (cdr c) a))))

(defun evlis (m a)
  (cond ((null m) nil)
        (t (cons (eval (car m) a) (evlis (cdr m) a)))))

(defun pair (x y)
  (cond ((null x) nil)
        (t (cons (cons (car x) (car y)) (pair (cdr x) (cdr y))))))
```

That's it. That's the seed from which the entire Lisp family tree grew. Seven primitive operations (quote, atom, eq, car, cdr, cons, cond), the concept of lambda, and suddenly you have a Turing-complete language.

Fun historical fact: The names CAR and CDR come from the IBM 704 computer's assembly language instructions: "Contents of the Address part of Register" and "Contents of the Decrement part of Register." These were the instructions used to access the head and tail of a list in the original implementation. Nearly 70 years later, we're still using these names. That's either tradition or stubbornness—in Lisp, they're often the same thing.

## The First Branches: LISP 1.5 and Beyond

LISP 1.5, released in 1962, was the first widely distributed version. It ran on the IBM 7090 and introduced features that wouldn't appear in other languages for decades:

- Garbage collection (automatic memory management)
- Conditional expressions
- Recursion as the primary control structure
- Functions as first-class values
- Dynamic typing
- Interactive development (the REPL)

The manual for LISP 1.5 is a masterpiece of technical writing—clear, concise, and complete. You can still read it today and implement your own Lisp from it. Try doing that with the Java Language Specification.

From LISP 1.5, the tree began to branch:

**MacLisp** (MIT, 1960s-1980s): Despite the name, nothing to do with Apple. This was Project MAC's Lisp, and it introduced many features that would become standard: arrays, strings, bit operations, and the DEFUN syntax we still use today.

**Interlisp** (BBN/Xerox, 1960s-1980s): The West Coast answer to MacLisp. Interlisp pioneered programming environment features—structure editors, debuggers, and the idea of "residential" programming where your entire development environment was a live Lisp image. Sound familiar, Smalltalk fans?

**Lisp Machine Lisp** (MIT/Symbolics/LMI, 1970s-1980s): Imagine an entire computer designed to run Lisp, from the hardware up. That's what MIT built, and companies like Symbolics commercialized. These machines had hardware support for tagged data types, garbage collection, and function calls. The operating system, the applications, even the microcode—all Lisp. It was paradise. It was also expensive, which is why you're not reading this on a Lisp Machine.

## The Great Schism: Common Lisp vs Scheme

By the early 1980s, the Lisp world had a problem: too many dialects. If you wrote a program in MacLisp, it wouldn't run in Interlisp. PSL (Portable Standard Lisp) programs wouldn't run in Franz Lisp. It was chaos—creative, productive chaos, but chaos nonetheless.

Two standardization efforts emerged, representing fundamentally different philosophies:

### Common Lisp: The Big Tent

Common Lisp (standardized in 1984, ANSI standard in 1994) took the "everything and the kitchen sink" approach. The goal was to unify all the major Lisp dialects into one language that could do everything any of them could do. The result was huge—the Common Lisp standard is over 1,000 pages—but incredibly powerful.

Common Lisp's philosophy: "If someone's using it in production, it should be in the standard." This gave us:

- Multiple inheritance object system (CLOS)
- Condition system (exception handling that makes try/catch look primitive)
- Multiple return values
- Optional, keyword, and rest parameters
- Compiler macros
- Reader macros
- Packages (namespaces)
- Comprehensive type system (yes, Common Lisp has types!)

### Scheme: The Minimalist Masterpiece

Meanwhile, at MIT, Guy Steele and Gerald Sussman were taking the opposite approach. Scheme (1975) asked: "What's the smallest set of features that gives us a complete, elegant programming language?"

The entire Scheme R5RS standard is 50 pages. The language has about 20 primitive forms. Yet from this minimalist base, you can build anything. Scheme introduced:

- Lexical scoping (controversial at the time!)
- Proper tail recursion (loops are just recursive functions)
- Continuations (control flow as a first-class value)
- Hygienic macros (macros that can't accidentally capture variables)
- Uniform treatment of functions and other values

The philosophical difference is stark. Common Lisp says, "Here's everything you might need." Scheme says, "Here's everything you need to build everything you might need."

This split wasn't acrimonious—many people worked on both languages. But it represented a fundamental tension in language design that persists today: cathedral versus bazaar, completeness versus minimalism, practical versus pure.

## The Modern Branches: Clojure, Racket, and Beyond

Fast forward to the 21st century. The Lisp family tree hasn't just survived; it's flourishing with new growth. Let's meet the modern branches:

### Clojure (2007): The Practical Revolutionary

Rich Hickey didn't just create another Lisp; he reconsidered what Lisp should be in the age of multicore processors and web services. Clojure runs on the JVM (and JavaScript VMs, and .NET), embraces functional programming, and makes immutability the default.

Clojure's innovations:
- Persistent data structures (immutable but efficient)
- Software Transactional Memory (concurrency without locks)
- Protocols (polymorphism without inheritance)
- Literal syntax for maps, vectors, and sets
- Destructuring everywhere
- Seamless Java interop

```clojure
;; Modern Clojure - handling concurrent updates elegantly
(def account (ref {:balance 1000}))

(defn transfer [from to amount]
  (dosync
    (alter from update :balance - amount)
    (alter to update :balance + amount)))

;; Multiple threads can call transfer safely - STM handles it all
```

### Racket (2010): The Language Laboratory

Racket started as PLT Scheme but evolved into something more ambitious: a language for creating languages. Racket's macro system is so powerful that different Racket programs can literally be written in different languages.

```racket
;; Define a new language in Racket
#lang racket
(provide (all-from-out racket)
         #%module-begin)

;; Now someone can write:
;; #lang mylang
;; And get a customized language experience
```

Racket is where language experimentation happens. Typed Racket adds static typing. Scribble is a documentation language. There's even a language for writing languages for writing languages. It's very meta.

### Janet (2018): The Embedded Scripting Lisp

Janet represents a new generation of Lisps designed for embedding. It's small, fast, and has zero dependencies. Think Lua, but with S-expressions.

### Fennel (2016): Lisp Compiling to Lua

Why should JavaScript have all the compile-to-language fun? Fennel brings Lisp to anywhere Lua runs—game engines, embedded systems, network equipment.

### Hy (2013): Lisp on Python

Hy transforms Python's runtime into a Lisp machine. You get access to the entire Python ecosystem with Lisp syntax. It's delightfully subversive.

```hy
;; Hy - Python libraries with Lisp syntax
(import numpy :as np)
(import matplotlib.pyplot :as plt)

(defn plot-sine []
  (let [x (.linspace np 0 (* 2 np.pi) 100)
        y (.sin np x)]
    (.plot plt x y)
    (.show plt)))
```

## Understanding S-expressions

At the heart of every Lisp is the S-expression. Understanding S-expressions is understanding Lisp. They're simpler than you think and more powerful than you can imagine.

An S-expression is either:
1. An atom (symbol, number, string, etc.)
2. A list of S-expressions

That's it. That's the entire syntax of Lisp. Everything else is semantics.

```lisp
;; Atoms
42
"hello"
symbol    ; or 'symbol with quote
t         ; true in traditional Lisp (nil for false)

;; Lists (S-expressions)
(+ 1 2)
(defn square [x] (* x x))
(if (> x 0) "positive" "non-positive")

;; Nested lists
(map (fn [x] (* x 2)) (range 10))
```

The beauty is that code and data have the same structure. This is homoiconicity, and it's what makes macros possible:

```lisp
;; Data - a list we can manipulate
'(+ 1 2 3)

;; Code - the same list, evaluated
(+ 1 2 3) ; => 6

;; A function that manipulates code as data (Clojure syntax)
(defmacro infix [expr]
  (list (second expr) (first expr) (nth expr 2)))

;; Use our macro
(infix (1 + 2)) ; => 3

;; Or in Common Lisp syntax:
(defmacro infix (expr)
  (list (second expr) (first expr) (third expr)))
```

The power of S-expressions extends beyond macros. They're:

- **Self-documenting**: The structure shows the program's structure
- **Tool-friendly**: Editors can manipulate code structurally, not textually
- **Unambiguous**: No operator precedence to remember
- **Universal**: The same syntax works for data serialization, configuration, DSLs

## The Family Resemblance

Despite their differences, all Lisps share core traits, like family members with the same nose:

**Lists and Symbols**: The fundamental data structures remain lists and symbols. Whether you're writing Common Lisp or Clojure, (+ 1 2) means the same thing.

**REPL-Driven Development**: Every Lisp has a REPL (Read-Eval-Print Loop). But calling it a REPL undersells it—it's a conversation with your program. You don't run your program; you grow it, function by function, testing as you go.

**Macros**: The ability to extend the language never left. Each Lisp might implement macros differently (hygienic in Scheme, reader macros in Common Lisp, syntax-quote in Clojure), but they all have them.

**Functional Flavor**: While not all Lisps enforce functional programming, they all encourage it. Functions are first-class, recursion is natural, and side effects are controlled (if not eliminated).

**Interactive Development**: Lisps assume you'll be changing your program while it runs. Hot-reloading isn't a feature; it's the default state of existence.

**Simplicity at the Core**: Despite surface differences, every Lisp can be implemented with a small core. You could write a basic Lisp interpreter in any of them in a few hundred lines.

## The Evolutionary Pressure

Why do new Lisps keep appearing? Because Lisp isn't just a language—it's a pattern for language creation. When you need:

- A configuration language (Guile in GNU projects)
- A scripting language (Emacs Lisp, AutoLISP in AutoCAD)
- A teaching language (Racket in education)
- A systems language (Ferret compiling to Rust)
- A web language (ClojureScript, Wisp)

You reach for Lisp. Not because of tradition, but because S-expressions are the simplest possible syntax, macros give you unlimited extensibility, and the REPL gives you immediate feedback.

Each new environment creates evolutionary pressure for a new Lisp adapted to that niche. The JVM needed a Lisp that could use Java libraries—Clojure was born. The browser needed a Lisp—ClojureScript appeared. Embedded systems needed a small, fast Lisp—Janet evolved.

This isn't language fragmentation; it's specialization. Each Lisp is optimized for its environment while maintaining the family traits that make it a Lisp.

## The Lisp Enlightenment

There's a phenomenon in the Lisp community called "the Lisp enlightenment" or "the Lisp curse." Once you truly understand Lisp—not just the syntax, but the philosophy—every other language feels constraining. You see missing features everywhere:

- "Why can't I extend the language's syntax?"
- "Why is the compiler a black box?"
- "Why can't I inspect and modify running code?"
- "Why do I have to restart to see changes?"
- "Why is metaprogramming so hard?"

This isn't elitism (okay, maybe a little). It's the frustration of knowing there's a better way. It's why Lisp programmers often seem smug—they're not trying to be superior; they're genuinely puzzled why everyone doesn't see what they see.

But here's the thing: the Lisp family tree keeps growing because the ideas are too powerful to contain. Every modern language is slowly becoming more Lisp-like:

- JavaScript got closures and higher-order functions
- Python added list comprehensions and generators
- Java got lambdas and streams
- Rust has powerful macros
- Swift has trailing closures and optional chaining

Paul Graham calls this "The Lisp Paradox"—Lisp is so powerful that it makes other languages better by providing features to copy, but those languages can never quite catch up because Lisp's power comes from its simplicity, not its features.

## Choosing Your Branch

So which Lisp should you learn? Here's a decision tree (pun intended):

**Want to build production web services?** → Clojure
**Need to script your editor?** → Emacs Lisp
**Building DSLs or language experiments?** → Racket
**Need industrial-strength everything?** → Common Lisp
**Teaching or learning fundamentals?** → Scheme (particularly Racket or Guile)
**Embedding in another system?** → Janet or Fennel
**Want to confuse your Python colleagues?** → Hy

But here's a secret: it doesn't matter which one you start with. The family resemblance is so strong that once you learn one Lisp, learning another is trivial. The parentheses are the same, the philosophy is the same, and the enlightenment is the same.

## The Tree Continues to Grow

As I write this in 2025, new Lisps are being created. Someone, somewhere, is designing a Lisp for quantum computers. Another is creating a Lisp that compiles to WebAssembly. There's probably a Lisp for smart contracts, for machine learning, for game development.

This isn't redundancy—it's evolution. Each new Lisp learns from its ancestors, adapts to its environment, and adds its innovations back to the family knowledge. Clojure's persistent data structures are being ported to other Lisps. Racket's language-oriented programming influences new language designs. Common Lisp's condition system inspires error handling in new languages.

The Lisp family tree isn't just growing; it's thriving. And that's because Lisp isn't really a language—it's an idea. The idea that programs are data, that simplicity enables power, that languages should be extensible, that programming should be interactive.

As long as people are writing programs, they'll be inventing new Lisps. The tree that John McCarthy planted in 1958 has become a forest, and we're all living in its shade.

---

*"Lisp is a programmable programming language." - John Foderaro*

In the next chapter, we'll dive deep into Common Lisp—the Swiss Army knife of Lisps. We'll set up a modern Common Lisp environment, explore CLOS (and discover why object-oriented programming in Lisp makes other languages look primitive), and build something real. Prepare yourself for power tools.