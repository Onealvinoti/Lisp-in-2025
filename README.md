# Lisp in 2025: A Practical Guide to the One True Programming Language

A comprehensive, modern guide to the Lisp family of programming languages, covering everything from historical context to cutting-edge applications in 2025.

## About This Book

This book is for programmers who want to understand and use Lisp in 2025. Whether you're curious about why Lisp has endured for nearly 70 years, want to learn functional programming, or need to build robust systems with minimal complexity, this book will guide you through the diverse and powerful world of Lisp.

## What You'll Learn

- **The Power of Homoiconicity**: Understand why "code as data" is more than just a clever trick
- **Multiple Lisp Dialects**: Master Common Lisp, Scheme, Clojure, ClojureScript, and Emacs Lisp
- **Practical Development**: Set up modern development environments and build real applications
- **Advanced Techniques**: Macros, DSLs, concurrent programming, and metaprogramming
- **Real-World Applications**: Web services, data processing, browser applications, and more

## Table of Contents

1. **[Introduction - The Eternal Language](01-introduction.md)**
   - Why Lisp still matters in 2025
   - The power of S-expressions
   - Your journey into the One True Programming Language

2. **[The Lisp Family Tree](02-lisp-family-tree.md)**
   - From McCarthy's original vision (1958) to modern dialects
   - The great schism: Common Lisp vs Scheme
   - Modern branches: Clojure, Racket, and beyond

3. **[Common Lisp - The Industrial Strength Lisp](03-common-lisp.md)**
   - Setting up SBCL and Quicklisp
   - CLOS: The most powerful object system ever designed
   - Building production systems

4. **[Scheme - The Academic Beauty](04-scheme.md)**
   - Minimalism as a feature
   - Continuations and tail call optimization
   - Teaching computer science fundamentals

5. **[Emacs Lisp - Your Editor is a Lisp Machine](05-emacs-lisp.md)**
   - Living inside a REPL
   - Writing your own modes and packages
   - Extending your environment while using it

6. **[Clojure - The Modern Lisp](06-clojure.md)**
   - Leveraging the JVM ecosystem
   - Immutability by default
   - Building concurrent systems with ease

7. **[ClojureScript - Lisp in the Browser](07-clojurescript.md)**
   - One language, multiple platforms
   - React development with Reagent
   - Full-stack Lisp applications

8. **[Setting Up Your Development Environment](08-development-environment.md)**
   - Structural editing with Paredit
   - REPL-driven development workflow
   - Editor configurations for Emacs, VS Code, and Vim

9. **[Practical Projects Across Lisps](09-practical-projects.md)**
   - Building DSLs in each dialect
   - Web services from Common Lisp to Clojure
   - Cross-Lisp code sharing strategies

10. **[The Future of Lisp](10-future-of-lisp.md)**
    - New Lisps on the horizon
    - Why every language is slowly becoming Lisp
    - Your journey forward

## Who This Book Is For

- **Curious Programmers**: You've heard about Lisp and want to understand what makes it special
- **Functional Programming Enthusiasts**: You want to learn FP from the source
- **Professional Developers**: You need powerful abstractions for complex problems
- **Language Designers**: You want to understand language design fundamentals
- **Students**: You're learning computer science and want deep understanding

## Prerequisites

- Basic programming experience in any language
- Comfort with command-line tools
- Curiosity about different ways of thinking about code
- No prior Lisp experience required!

## How to Read This Book

- **Sequential Path**: Read chapters 1-10 in order for a complete journey
- **Dialect-Specific**: Jump to chapters 3-7 for specific Lisp dialects
- **Practical Focus**: Start with chapter 8 for environment setup, then explore projects in chapter 9
- **Quick Start**: Read chapters 1, 6, and 8 to get started with modern Clojure development

## Code Examples

All code examples in this book are tested and working as of 2025. Each example includes:
- Clear explanations of concepts
- Runnable code you can experiment with
- Real-world applications, not just toy examples

## Setting Up Your Environment

Quick start for each dialect:

```bash
# Common Lisp
brew install sbcl
curl -O https://beta.quicklisp.org/quicklisp.lisp
sbcl --load quicklisp.lisp

# Clojure
brew install clojure/tools/clojure

# Scheme (Racket)
brew install --cask racket

# Emacs (for Emacs Lisp)
brew install --cask emacs
```

## Resources and Communities

- **Online Communities**
  - [r/lisp](https://reddit.com/r/lisp) - General Lisp discussion
  - [Clojurians Slack](https://clojurians.net/) - 20,000+ Clojure developers
  - [Planet Lisp](http://planet.lisp.org/) - Lisp blog aggregator

- **Essential Books**
  - *Structure and Interpretation of Computer Programs* (SICP)
  - *Practical Common Lisp* by Peter Seibel
  - *The Little Schemer* by Friedman and Felleisen
  - *Clojure for the Brave and True* by Daniel Higginbotham

- **Online REPLs**
  - [Try Clojure](https://tryclojure.org/)
  - [Racket in Browser](https://try-racket.defn.io/)
  - [Common Lisp REPL](https://tio.run/#common-lisp)

## Contributing

Found an error or want to suggest improvements? Feel free to:
- Open an issue
- Submit a pull request
- Share your Lisp journey

## About the Author

This book was written by someone who believes that Lisp is not just a programming language but a notation for expressing thought, and that parentheses are not obstacles but wings that let your code fly.

## License

This book is shared with the Lisp community in the spirit of open knowledge.

---

*"Lisp isn't a language, it's a building material."* - Alan Kay

*"Learn Lisp, and you will be enlightened. Use Lisp, and you will be free."*

## Start Reading

Ready to begin your journey into the One True Programming Language?

**[→ Start with Chapter 1: Introduction - The Eternal Language](01-introduction.md)**

May your parentheses always balance and your recursion always terminate.

`(farewell reader)`