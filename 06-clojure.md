# Chapter 6: Clojure - The Modern Lisp

> "It is better to have 100 functions operate on one data structure than 10 functions on 10 data structures." - Alan Perlis

Rich Hickey took this aphorism seriously. In 2007, he unveiled Clojure—a Lisp that didn't just run on the JVM, but embraced it. A Lisp that made immutability the default. A Lisp designed for the multicore age. A Lisp that proved you could sneak into the enterprise through the Java door and revolutionize how people think about programming.

Clojure isn't just a modern Lisp; it's a Lisp that learned from 50 years of history and said, "What if we got the defaults right this time?"

## The JVM Advantage

Clojure's masterstroke was choosing the JVM as its host platform. While Lisp purists scoffed, Hickey understood something crucial: the platform matters more than purity.

By targeting the JVM, Clojure got:
- Battle-tested garbage collection
- Mature JIT compilation
- Thousands of libraries
- Enterprise acceptance
- Cross-platform deployment
- Industry-standard tooling

But Clojure doesn't just run on the JVM—it embraces it:

```clojure
;; Seamless Java interop
(import java.time.LocalDateTime)

(defn current-time []
  (.toString (LocalDateTime/now)))

;; Call Java methods with dot notation
(.toUpperCase "hello")  ; => "HELLO"

;; Create Java objects
(java.util.ArrayList. [1 2 3])

;; Implement Java interfaces
(reify java.lang.Runnable
  (run [this]
    (println "Running in a thread!")))

;; Use Java libraries directly
(import '[org.apache.commons.codec.digest DigestUtils])
(DigestUtils/sha256Hex "secret")
```

This isn't grudging compatibility—it's a first-class feature. Clojure code can call Java, Java can call Clojure, and it all just works.

## Immutability by Default

Clojure's most radical decision: making immutability the default. Not optional, not recommended—default.

```clojure
;; All core data structures are immutable
(def my-vector [1 2 3])
(conj my-vector 4)  ; Returns [1 2 3 4]
; my-vector is still [1 2 3]

(def my-map {:name "Alice" :age 30})
(assoc my-map :city "Boston")  ; Returns new map
; my-map is unchanged

;; But it's efficient! Structural sharing means near-constant time
(def big-vector (vec (range 1000000)))
(time (conj big-vector 1000001))
; "Elapsed time: 0.031 msecs"
```

This isn't just about preventing bugs (though it does). It's about enabling fearless concurrency:

```clojure
;; Share data between threads without fear
(def shared-data {:count 0 :items []})

;; Multiple threads can read simultaneously
(future (println (:count shared-data)))
(future (println (count (:items shared-data))))
(future (println (keys shared-data)))

;; No locks needed - the data can't change!
```

When you need mutability, Clojure provides it—but with controls:

```clojure
;; Atoms for independent, synchronous updates
(def counter (atom 0))
(swap! counter inc)  ; => 1
(swap! counter + 10) ; => 11

;; Refs for coordinated, synchronous updates (STM)
(def account1 (ref {:balance 1000}))
(def account2 (ref {:balance 500}))

(defn transfer [from to amount]
  (dosync  ; Transaction
    (alter from update :balance - amount)
    (alter to update :balance + amount)))

;; Agents for independent, asynchronous updates
(def logger (agent []))
(send logger conj "Event happened")

;; Vars for thread-local mutability
(def ^:dynamic *context* nil)
(binding [*context* "production"]
  (println *context*))  ; => "production"
```

Each reference type has specific semantics for specific use cases. You can't accidentally use the wrong one.

## Rich Hickey's Philosophy

Understanding Clojure means understanding Rich Hickey's philosophy. His talks are legendary in the programming community:

### Simple Made Easy
Hickey distinguishes between "simple" (not compound) and "easy" (familiar). Clojure chooses simple over easy:

```clojure
;; Simple: Functions and data
(defn process-user [user]
  (-> user
      (assoc :processed true)
      (update :score inc)))

;; Complex: Objects with hidden state
; class User {
;   private int score;
;   public void process() {
;     this.processed = true;
;     this.score++;
;   }
; }
```

### The Value of Values
Hickey argues that we should program with values, not places:

```clojure
;; Values don't change
(def then {:time "2023-01-01" :temperature 32})
(def now {:time "2025-01-01" :temperature 28})

;; then is still then, even though time has passed

;; Compare with places (variables)
; int temperature = 32;
; temperature = 28;  // We lost the old value!
```

### Spec: Specification as Values
Clojure's spec library embodies this philosophy:

```clojure
(require '[clojure.spec.alpha :as s])

;; Describe data with data
(s/def ::name string?)
(s/def ::age (s/and int? #(>= % 0)))
(s/def ::user (s/keys :req [::name ::age]))

;; Validate
(s/valid? ::user {::name "Alice" ::age 30})  ; => true

;; Generate test data
(s/exercise ::user 5)  ; Generate 5 random users

;; Instrument functions
(s/fdef process-user
  :args (s/cat :user ::user)
  :ret ::user)
```

## Building Production Systems

Let's build a real web service to see Clojure in production:

```clojure
;; project.clj or deps.edn
{:deps {org.clojure/clojure {:mvn/version "1.11.1"}
        ring/ring-core {:mvn/version "1.10.0"}
        ring/ring-jetty-adapter {:mvn/version "1.10.0"}
        compojure {:mvn/version "1.7.0"}
        org.clojure/data.json {:mvn/version "2.4.0"}
        com.github.seancorfield/next.jdbc {:mvn/version "1.3.883"}
        org.postgresql/postgresql {:mvn/version "42.5.4"}}}

;; src/myapp/core.clj
(ns myapp.core
  (:require [ring.adapter.jetty :as jetty]
            [ring.middleware.params :refer [wrap-params]]
            [ring.middleware.json :refer [wrap-json-response wrap-json-body]]
            [compojure.core :refer [defroutes GET POST PUT DELETE]]
            [compojure.route :as route]
            [next.jdbc :as jdbc]
            [clojure.data.json :as json]))

;; Database configuration
(def db-spec
  {:dbtype "postgresql"
   :dbname "myapp"
   :host "localhost"
   :user "postgres"
   :password "secret"})

(def datasource (jdbc/get-datasource db-spec))

;; Database functions
(defn create-tables []
  (jdbc/execute! datasource
    ["CREATE TABLE IF NOT EXISTS todos (
       id SERIAL PRIMARY KEY,
       title VARCHAR(255) NOT NULL,
       completed BOOLEAN DEFAULT false,
       created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
     )"]))

(defn get-todos []
  (jdbc/execute! datasource ["SELECT * FROM todos ORDER BY created_at DESC"]))

(defn create-todo [title]
  (jdbc/execute-one! datasource
    ["INSERT INTO todos (title) VALUES (?) RETURNING *" title]))

(defn update-todo [id updates]
  (jdbc/execute-one! datasource
    ["UPDATE todos SET completed = ? WHERE id = ? RETURNING *"
     (:completed updates) id]))

(defn delete-todo [id]
  (jdbc/execute-one! datasource ["DELETE FROM todos WHERE id = ?" id]))

;; Request handlers
(defn handle-get-todos [req]
  {:status 200
   :body (get-todos)})

(defn handle-create-todo [req]
  (let [title (get-in req [:body "title"])]
    {:status 201
     :body (create-todo title)}))

(defn handle-update-todo [req]
  (let [id (Integer/parseInt (get-in req [:params :id]))
        completed (get-in req [:body "completed"])]
    {:status 200
     :body (update-todo id {:completed completed})}))

(defn handle-delete-todo [req]
  (let [id (Integer/parseInt (get-in req [:params :id]))]
    (delete-todo id)
    {:status 204}))

;; Routes
(defroutes app-routes
  (GET "/todos" [] handle-get-todos)
  (POST "/todos" [] handle-create-todo)
  (PUT "/todos/:id" [] handle-update-todo)
  (DELETE "/todos/:id" [] handle-delete-todo)
  (route/not-found "Not Found"))

;; Middleware stack
(def app
  (-> app-routes
      wrap-json-body
      wrap-json-response
      wrap-params))

;; Server
(defn -main [& args]
  (create-tables)
  (jetty/run-jetty app {:port 3000 :join? false})
  (println "Server running on http://localhost:3000"))
```

This is a complete REST API with database persistence. Notice:
- No classes, no inheritance
- Pure functions everywhere possible
- Data flows through transformations
- Minimal ceremony

## The REPL-Driven Development Workflow

Clojure development isn't about writing and running programs—it's about growing them interactively:

```clojure
;; Start a REPL in your project
; lein repl or clj

;; Load your namespace
user=> (require '[myapp.core :as core])

;; Test functions immediately
user=> (core/create-todo "Learn Clojure")
{:id 1 :title "Learn Clojure" :completed false ...}

;; Redefine functions on the fly
user=> (defn core/create-todo [title]
         (println "Creating:" title)
         (jdbc/execute-one! ...))

;; Test the new version
user=> (core/create-todo "Master Clojure")
Creating: Master Clojure
{:id 2 :title "Master Clojure" ...}

;; Start your server from the REPL
user=> (def server (jetty/run-jetty #'core/app {:port 3000 :join? false}))

;; Change your handler
user=> (defn core/handle-get-todos [req]
         {:status 200
          :body (assoc {:todos (get-todos)} :timestamp (System/currentTimeMillis))})

;; The running server uses the new version immediately!

;; Stop the server
user=> (.stop server)
```

This workflow is transformative. You're not restarting your application to see changes—you're evolving it while it runs.

## Clojure's Killer Features

### Destructuring Everywhere

```clojure
;; Destructure in function arguments
(defn greet [{:keys [name age city] :or {city "Unknown"}}]
  (str "Hello " name " (" age " years old) from " city))

(greet {:name "Alice" :age 30 :city "Boston"})
; => "Hello Alice (30 years old) from Boston"

;; Destructure in let bindings
(let [[first second & rest] [1 2 3 4 5]]
  (println first second rest))  ; 1 2 (3 4 5)

;; Nested destructuring
(let [{:keys [user]
       {:keys [name email]} :user} {:user {:name "Bob" :email "bob@example.com"}}]
  (println name email))  ; Bob bob@example.com
```

### Threading Macros

```clojure
;; Thread-first (->)
(-> "hello"
    str/upper-case
    (str/replace "L" "X")
    str/reverse)
; => "OXXEH"

;; Thread-last (->>)
(->> (range 10)
     (map inc)
     (filter even?)
     (reduce +))
; => 30

;; Thread-as (as->)
(as-> {:a 1} x
  (assoc x :b 2)
  (update x :a inc)
  (select-keys x [:a]))
; => {:a 2}

;; Some-thread (some->)
(some-> {:user {:name "Alice"}}
        :user
        :name
        str/upper-case)
; => "ALICE"

(some-> {:user nil}
        :user
        :name
        str/upper-case)
; => nil (doesn't throw!)
```

### Transducers

```clojure
;; Composable algorithmic transformations
(def xf
  (comp
    (map inc)
    (filter even?)
    (take 5)))

;; Use with different contexts
(into [] xf (range 100))      ; => [2 4 6 8 10]
(sequence xf (range 100))     ; Lazy sequence
(transduce xf + (range 100))   ; => 30

;; No intermediate collections!
```

### Core.async

```clojure
(require '[clojure.core.async :as async])

;; CSP-style concurrency
(let [ch (async/chan)]
  ;; Producer
  (async/go
    (dotimes [i 5]
      (async/>! ch i)
      (async/<! (async/timeout 1000))))

  ;; Consumer
  (async/go
    (loop []
      (when-let [val (async/<! ch)]
        (println "Got:" val)
        (recur)))))

;; Parallel processing pipeline
(def input (async/chan 100))
(def output (async/chan 100))

(async/pipeline 4  ; 4 parallel processors
                output
                (map #(* % %))  ; Square each number
                input)
```

## The Clojure Ecosystem

Clojure's ecosystem is remarkably cohesive:

### Web Development
- **Ring**: HTTP abstraction (like Ruby's Rack)
- **Compojure**: Routing DSL
- **Pedestal**: High-performance async web framework
- **Luminus**: Full-stack framework

### Data Processing
- **Onyx**: Distributed computation
- **Apache Storm**: Stream processing (Clojure inside!)
- **Cascalog**: Hadoop queries in Clojure

### Development Tools
- **Leiningen**: Build tool ("lein" for short)
- **deps.edn**: Official dependency management
- **CIDER**: Emacs integration
- **Cursive**: IntelliJ integration
- **Calva**: VS Code integration

### Libraries That Changed the Game
- **Datomic**: Immutable database by Rich Hickey
- **Clara Rules**: Rules engine
- **Reagent**: React wrapper for ClojureScript
- **Re-frame**: State management for SPAs

## Real-World Clojure

Companies using Clojure in production:
- **Walmart**: Receipts processing system
- **Netflix**: Internal tools and services
- **Nubank**: Entire banking platform
- **CircleCI**: Continuous integration platform
- **Metabase**: Business intelligence tool

Why they chose Clojure:
1. **Productivity**: Small teams building large systems
2. **Reliability**: Immutability prevents entire classes of bugs
3. **Performance**: JVM performance with functional elegance
4. **Maintainability**: Code that's easy to reason about
5. **Hiring**: Clojure attracts thoughtful developers

## The Clojure Mindset

Programming in Clojure changes how you think:

### Data-Oriented Programming
Instead of objects with methods, you have data and functions:

```clojure
;; Not this:
; user.updateAge(31)
; user.addRole("admin")
; user.save()

;; But this:
(-> user
    (assoc :age 31)
    (update :roles conj "admin")
    save-to-db)
```

### REPL as Primary Interface
The REPL isn't for debugging—it's for development:

```clojure
;; Explore your data
(def response (fetch-api-data))
(keys response)
(-> response :data first)
(map :id (:data response))

;; Build your function interactively
(defn process-response [resp]
  (map :id (:data resp)))  ; Start simple

(defn process-response [resp]
  (->> (:data resp)
       (map :id)
       (filter pos?)))  ; Add filtering

(defn process-response [resp]
  (->> (:data resp)
       (map :id)
       (filter pos?)
       distinct))  ; Add deduplication
```

### Libraries Over Frameworks
Clojure favors composable libraries over monolithic frameworks. You build your architecture from simple pieces:

```clojure
;; Compose your stack
(def app
  (-> routes
      (wrap-json-response)
      (wrap-cors)
      (wrap-authentication)
      (wrap-logging)))

;; Each wrapper is independent and composable
```

## Advanced Clojure

### Macros
Clojure macros use syntax-quote for hygiene:

```clojure
(defmacro when-let* [bindings & body]
  (if (empty? bindings)
    `(do ~@body)
    `(when-let [~(first bindings) ~(second bindings)]
       (when-let* ~(drop 2 bindings) ~@body))))

;; Use it
(when-let* [a (get {:a 1} :a)
            b (get {:b 2} :b)
            c (get {:c 3} :c)]
  (+ a b c))  ; => 6
```

### Protocols and Multimethods

```clojure
;; Protocols for type-based polymorphism
(defprotocol Drawable
  (draw [this]))

(defrecord Circle [radius]
  Drawable
  (draw [this]
    (str "Drawing circle with radius " radius)))

(defrecord Square [side]
  Drawable
  (draw [this]
    (str "Drawing square with side " side)))

;; Multimethods for arbitrary dispatch
(defmulti area :shape)

(defmethod area :circle [{:keys [radius]}]
  (* Math/PI radius radius))

(defmethod area :square [{:keys [side]}]
  (* side side))

(area {:shape :circle :radius 5})  ; => 78.53...
```

### Meta-programming with Spec

```clojure
(require '[clojure.spec.alpha :as s])
(require '[clojure.spec.gen.alpha :as gen])

;; Generate test data from specs
(s/def ::id uuid?)
(s/def ::email (s/and string? #(re-matches #".+@.+\..+" %)))
(s/def ::age (s/int-in 0 150))
(s/def ::user (s/keys :req [::id ::email ::age]))

;; Generate sample users
(gen/sample (s/gen ::user) 3)

;; Property-based testing
(require '[clojure.test.check.properties :as prop])

(def sort-idempotent
  (prop/for-all [v (gen/vector gen/int)]
    (= (sort v) (sort (sort v)))))
```

## The Future is Functional

Clojure proved that functional programming isn't academic—it's practical. That immutability isn't a constraint—it's liberation. That the JVM isn't a compromise—it's a strength.

More importantly, Clojure proved that a modern Lisp could succeed in the enterprise. It snuck in through the Java door and showed developers a better way to build systems.

The influence is everywhere:
- Java added lambdas and streams
- JavaScript libraries embrace immutability
- Rust's ownership system enforces similar guarantees
- Kotlin borrowed many Clojure ideas

But Clojure isn't resting. Recent developments:
- **Babashka**: Native Clojure scripting
- **SCI**: Small Clojure Interpreter for embedding
- **Holy-lambda**: Clojure on AWS Lambda
- **Jank**: Clojure on LLVM (experimental)

## Why Choose Clojure

Choose Clojure when you need:
- **Concurrent systems**: STM and immutability make concurrency tractable
- **Data processing**: Perfect for ETL, analytics, and transformation
- **Web services**: Small teams building robust APIs
- **Interactive development**: REPL-driven development at its finest
- **Java interop**: When you need the JVM ecosystem

But really, choose Clojure when you're tired of incidental complexity. When you want to focus on your problem, not your language. When you want code that's a joy to write and a joy to maintain.

---

*"Programming is not about typing, it's about thinking." - Rich Hickey*

Next: ClojureScript, where we take everything we love about Clojure and run it in the browser. We'll build reactive UIs, share code between client and server, and discover why ClojureScript developers are the happiest in the JavaScript ecosystem.