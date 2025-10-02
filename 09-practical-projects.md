---
layout: default
title: "Chapter 9: Practical Projects"
---

# Chapter 9: Practical Projects Across Lisps

> "Show me your flowcharts and conceal your tables, and I shall continue to be mystified. Show me your tables, and I won't usually need your flowcharts; they'll be obvious." - Fred Brooks

Let's put theory into practice. In this chapter, we'll build the same projects in different Lisps, demonstrating both the family resemblance and the unique strengths of each dialect. You'll see how the same ideas express themselves differently, and learn when to reach for each tool.

## Project 1: Building a DSL in Each Lisp

Domain-Specific Languages are where Lisp shines brightest. Let's build a simple query DSL that looks like SQL but is pure Lisp.

### Common Lisp Version

```common-lisp
;;; query-dsl.lisp - SQL-like DSL in Common Lisp

(defpackage :query-dsl
  (:use :cl)
  (:export #:select #:from #:where #:order-by #:execute-query))

(in-package :query-dsl)

;; Data structure for queries
(defstruct query
  fields
  table
  conditions
  ordering)

;; Macros for the DSL
(defmacro select (&rest fields)
  `(make-query :fields ',fields))

(defmacro from (query table)
  `(progn
     (setf (query-table ,query) ',table)
     ,query))

(defmacro where (query &rest conditions)
  `(progn
     (setf (query-conditions ,query) ',conditions)
     ,query))

(defmacro order-by (query &rest fields)
  `(progn
     (setf (query-ordering ,query) ',fields)
     ,query))

;; Sample data
(defparameter *users*
  '(((:id . 1) (:name . "Alice") (:age . 30) (:city . "Boston"))
    ((:id . 2) (:name . "Bob") (:age . 25) (:city . "Chicago"))
    ((:id . 3) (:name . "Charlie") (:age . 35) (:city . "Boston"))
    ((:id . 4) (:name . "Diana") (:age . 28) (:city . "Denver"))))

;; Query execution
(defun execute-query (query)
  (let ((results (copy-list (symbol-value (query-table query)))))
    ;; Apply WHERE conditions
    (when (query-conditions query)
      (setf results (filter-records results (query-conditions query))))
    ;; Apply ORDER BY
    (when (query-ordering query)
      (setf results (sort-records results (query-ordering query))))
    ;; Select fields
    (if (equal (query-fields query) '(*))
        results
        (mapcar (lambda (record)
                  (select-fields record (query-fields query)))
                results))))

(defun filter-records (records conditions)
  (remove-if-not
   (lambda (record)
     (eval-condition record conditions))
   records))

(defun eval-condition (record conditions)
  (destructuring-bind (field op value) conditions
    (let ((field-value (cdr (assoc field record))))
      (case op
        (= (equal field-value value))
        (> (> field-value value))
        (< (< field-value value))
        (like (search value field-value))))))

(defun sort-records (records fields)
  (sort records
        (lambda (a b)
          (< (cdr (assoc (first fields) a))
             (cdr (assoc (first fields) b))))))

(defun select-fields (record fields)
  (mapcar (lambda (field)
            (assoc field record))
          fields))

;; Usage example
;; Note: -> is not standard Common Lisp, using nested calls instead
(defparameter *query1*
  (order-by
    (where
      (from
        (select *)
        *users*)
      :age > 25)
    :age))

(execute-query *query1*)
;; Returns users older than 25, sorted by age
```

### Clojure Version

```clojure
;;; query_dsl.clj - SQL-like DSL in Clojure

(ns query-dsl.core)

;; Query representation
(defn select [& fields]
  {:select (vec fields)})

(defn from [query table]
  (assoc query :from table))

(defn where [query & conditions]
  (assoc query :where conditions))

(defn order-by [query & fields]
  (assoc query :order-by (vec fields)))

;; Sample data
(def users
  [{:id 1 :name "Alice" :age 30 :city "Boston"}
   {:id 2 :name "Bob" :age 25 :city "Chicago"}
   {:id 3 :name "Charlie" :age 35 :city "Boston"}
   {:id 4 :name "Diana" :age 28 :city "Denver"}])

;; Query execution
(defn execute-query [{:keys [select from where order-by]}]
  (cond-> from
    ;; Apply WHERE
    where (filter (fn [record]
                   (let [[field op value] where]
                     (case op
                       :> (> (get record field) value)
                       :< (< (get record field) value)
                       := (= (get record field) value)
                       :like (re-find (re-pattern value)
                                     (str (get record field)))))))
    ;; Apply ORDER-BY
    order-by (sort-by (first order-by))

    ;; Apply SELECT
    (not= select [:*]) (map #(select-keys % select))))

;; Thread-first macro for readable queries
(def query1
  (-> (select :*)
      (from users)
      (where :age :> 25)
      (order-by :age)))

(execute-query query1)
;; Returns users older than 25, sorted by age

;; More advanced: macro version for even better syntax
(defmacro sql [& body]
  `(-> ~@body))

(defmacro SELECT [& fields]
  `(select ~@(map keyword fields)))

(defmacro FROM [table]
  `(from ~table))

(defmacro WHERE [field op value]
  `(where ~(keyword field) ~(keyword op) ~value))

;; Now we can write:
(def query2
  (sql (SELECT name age city)
       (FROM users)
       (WHERE age > 25)))
```

### Scheme Version

```scheme
;;; query-dsl.scm - SQL-like DSL in Scheme

;; Query structure
(define (make-query)
  '((select . ())
    (from . ())
    (where . ())
    (order-by . ())))

(define (select query . fields)
  (cons (cons 'select fields)
        (cdr query)))

(define (from query table)
  (cons (car query)
        (cons (cons 'from table)
              (cddr query))))

(define (where query . conditions)
  (append (list (car query)
                (cadr query)
                (cons 'where conditions))
          (cdddr query)))

;; Sample data
(define users
  '(((id . 1) (name . "Alice") (age . 30) (city . "Boston"))
    ((id . 2) (name . "Bob") (age . 25) (city . "Chicago"))
    ((id . 3) (name . "Charlie") (age . 35) (city . "Boston"))
    ((id . 4) (name . "Diana") (age . 28) (city . "Denver"))))

;; Query execution
(define (execute-query query data)
  (let ((select-fields (cdr (assoc 'select query)))
        (where-clause (cdr (assoc 'where query)))
        (order-field (cdr (assoc 'order-by query))))

    ;; Apply filters
    (let ((filtered (if where-clause
                       (filter (lambda (record)
                                (apply-condition record where-clause))
                              data)
                       data)))
      ;; Apply sorting
      (let ((sorted (if order-field
                       (sort filtered
                            (lambda (a b)
                              (< (cdr (assoc (car order-field) a))
                                 (cdr (assoc (car order-field) b)))))
                       filtered)))
        ;; Select fields
        (if (equal? select-fields '(*))
            sorted
            (map (lambda (record)
                   (filter (lambda (field)
                            (member (car field) select-fields))
                          record))
                 sorted))))))

;; Macro for nice syntax
(define-syntax sql-query
  (syntax-rules (select from where order-by)
    ((sql-query (select fields ...) rest ...)
     (select (sql-query rest ...) 'fields ...))
    ((sql-query (from table) rest ...)
     (from (sql-query rest ...) 'table))
    ((sql-query (where field op value) rest ...)
     (where (sql-query rest ...) 'field 'op value))
    ((sql-query (order-by field) rest ...)
     (order-by (sql-query rest ...) 'field))
    ((sql-query)
     (make-query))))

;; Usage
(define my-query
  (sql-query
   (select name age)
   (from users)
   (where age > 25)
   (order-by age)))
```

## Project 2: Web Services - From Common Lisp to Clojure

Let's build the same REST API in different Lisps to see how web development varies across the family.

### Common Lisp Web Service

```common-lisp
;;; todo-api.lisp - REST API in Common Lisp with Hunchentoot

(ql:quickload '(:hunchentoot :cl-json :local-time :ironclad :cl-ppcre))

(defpackage :todo-api
  (:use :cl :hunchentoot))

(in-package :todo-api)

;; Data store (in production, use a database)
(defparameter *todos* (make-hash-table :test 'equal))
(defparameter *next-id* 1)

;; JSON helpers
(defun json-response (data &optional (status 200))
  (setf (return-code*) status)
  (setf (content-type*) "application/json")
  (cl-json:encode-json-to-string data))

(defun parse-json-body ()
  (cl-json:decode-json-from-string
   (raw-post-data :force-text t)))

;; CORS middleware
(defun enable-cors ()
  (setf (header-out "Access-Control-Allow-Origin") "*")
  (setf (header-out "Access-Control-Allow-Methods") "GET, POST, PUT, DELETE, OPTIONS")
  (setf (header-out "Access-Control-Allow-Headers") "Content-Type"))

;; Routes
(define-easy-handler (get-todos :uri "/api/todos" :default-request-type :get) ()
  (enable-cors)
  (json-response
   (loop for todo being the hash-values of *todos*
         collect todo)))

(define-easy-handler (create-todo :uri "/api/todos" :default-request-type :post) ()
  (enable-cors)
  (let* ((data (parse-json-body))
         (todo (list :id *next-id*
                    :title (cdr (assoc :title data))
                    :completed nil
                    :created-at (local-time:now))))
    (setf (gethash *next-id* *todos*) todo)
    (incf *next-id*)
    (json-response todo 201)))

(define-easy-handler (update-todo :uri "/api/todos/:id" :default-request-type :put)
    (id)
  (enable-cors)
  (let* ((todo-id (parse-integer id))
         (todo (gethash todo-id *todos*))
         (data (parse-json-body)))
    (if todo
        (progn
          (setf (getf todo :completed) (cdr (assoc :completed data)))
          (setf (gethash todo-id *todos*) todo)
          (json-response todo))
        (json-response '(:error "Not found") 404))))

(define-easy-handler (delete-todo :uri "/api/todos/:id" :default-request-type :delete)
    (id)
  (enable-cors)
  (let ((todo-id (parse-integer id)))
    (if (gethash todo-id *todos*)
        (progn
          (remhash todo-id *todos*)
          (json-response '(:message "Deleted") 204))
        (json-response '(:error "Not found") 404))))

;; Start server
(defparameter *server*
  (start (make-instance 'easy-acceptor :port 3000)))
```

### Clojure Web Service

```clojure
;;; todo-api.clj - REST API in Clojure with Ring/Compojure

(ns todo-api.core
  (:require [ring.adapter.jetty :as jetty]
            [ring.middleware.json :refer [wrap-json-response wrap-json-body]]
            [ring.middleware.cors :refer [wrap-cors]]
            [compojure.core :refer [defroutes GET POST PUT DELETE]]
            [compojure.route :as route]))

;; Data store
(def todos (atom {}))
(def next-id (atom 1))

;; Helper functions
(defn create-todo [title]
  (let [id @next-id
        todo {:id id
              :title title
              :completed false
              :created-at (java.util.Date.)}]
    (swap! next-id inc)
    (swap! todos assoc id todo)
    todo))

(defn update-todo [id updates]
  (when-let [todo (get @todos id)]
    (let [updated (merge todo updates)]
      (swap! todos assoc id updated)
      updated)))

(defn delete-todo [id]
  (when (get @todos id)
    (swap! todos dissoc id)
    true))

;; Routes
(defroutes app-routes
  (GET "/api/todos" []
    {:status 200
     :body (vals @todos)})

  (POST "/api/todos" request
    (let [title (get-in request [:body "title"])
          todo (create-todo title)]
      {:status 201
       :body todo}))

  (PUT "/api/todos/:id" [id :as request]
    (let [todo-id (Integer/parseInt id)
          completed (get-in request [:body "completed"])]
      (if-let [updated (update-todo todo-id {:completed completed})]
        {:status 200
         :body updated}
        {:status 404
         :body {:error "Not found"}})))

  (DELETE "/api/todos/:id" [id]
    (let [todo-id (Integer/parseInt id)]
      (if (delete-todo todo-id)
        {:status 204}
        {:status 404
         :body {:error "Not found"}})))

  (route/not-found
    {:status 404
     :body {:error "Route not found"}}))

;; Middleware stack
(def app
  (-> app-routes
      wrap-json-body
      wrap-json-response
      (wrap-cors :access-control-allow-origin [#".*"]
                 :access-control-allow-methods [:get :post :put :delete])))

;; Start server
(defn -main []
  (jetty/run-jetty app {:port 3000 :join? false}))
```

## Project 3: Data Processing Pipelines

Data transformation is a sweet spot for Lisp. Let's build a log processing pipeline.

### Clojure Data Pipeline

```clojure
(ns data-pipeline.core
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.data.json :as json]))

;; Parse log lines
(defn parse-log-line [line]
  (when-let [[_ timestamp level message]
             (re-matches #"(\S+\s+\S+)\s+\[(\w+)\]\s+(.*)" line)]
    {:timestamp timestamp
     :level (keyword (str/lower-case level))
     :message message}))

;; Processing pipeline with transducers
(def processing-pipeline
  (comp
    ;; Parse lines
    (map parse-log-line)
    ;; Remove nils
    (filter some?)
    ;; Filter by level
    (filter #(#{:error :warn} (:level %)))
    ;; Add metadata
    (map #(assoc % :processed-at (System/currentTimeMillis)))
    ;; Group by level
    (partition-by :level)))

;; Process file
(defn process-log-file [filename]
  (with-open [reader (io/reader filename)]
    (into []
          processing-pipeline
          (line-seq reader))))

;; Parallel processing with core.async
(require '[clojure.core.async :as async])

(defn parallel-process-logs [files]
  (let [input-ch (async/chan 100)
        output-ch (async/chan 100)]

    ;; Start workers
    (dotimes [_ 4]
      (async/go-loop []
        (when-let [file (async/<! input-ch)]
          (let [results (process-log-file file)]
            (async/>! output-ch results))
          (recur))))

    ;; Feed files
    (async/go
      (doseq [file files]
        (async/>! input-ch file))
      (async/close! input-ch))

    ;; Collect results
    (async/go-loop [all-results []]
      (if-let [results (async/<! output-ch)]
        (recur (concat all-results results))
        all-results))))

;; Statistical analysis
(defn analyze-logs [logs]
  {:total-count (count logs)
   :by-level (frequencies (map :level logs))
   :error-messages (map :message (filter #(= :error (:level %)) logs))
   :time-range {:start (apply min (map :timestamp logs))
                :end (apply max (map :timestamp logs))}})
```

### Common Lisp Data Pipeline

```common-lisp
(ql:quickload '(:cl-ppcre :local-time :lparallel))

(defpackage :data-pipeline
  (:use :cl))

(in-package :data-pipeline)

;; Parse log line
(defun parse-log-line (line)
  (multiple-value-bind (match groups)
      (cl-ppcre:scan-to-strings
       "(\\S+\\s+\\S+)\\s+\\[(\\w+)\\]\\s+(.*)" line)
    (when match
      (list :timestamp (aref groups 0)
            :level (intern (string-upcase (aref groups 1)) :keyword)
            :message (aref groups 2)))))

;; Processing functions
(defun filter-by-level (logs &rest levels)
  (remove-if-not (lambda (log)
                   (member (getf log :level) levels))
                 logs))

(defun add-metadata (log)
  (append log (list :processed-at (get-universal-time))))

;; Main pipeline
(defun process-log-file (filename)
  (with-open-file (stream filename)
    (loop for line = (read-line stream nil)
          while line
          for parsed = (parse-log-line line)
          when parsed
          when (member (getf parsed :level) '(:error :warn))
          collect (add-metadata parsed))))

;; Parallel processing
(defun parallel-process-logs (files)
  (lparallel:pmapcar #'process-log-file files))

;; Analysis
(defun analyze-logs (logs)
  (list :total-count (length logs)
        :by-level (count-by-level logs)
        :error-messages (mapcar (lambda (log) (getf log :message))
                               (remove-if-not (lambda (log)
                                               (eq (getf log :level) :error))
                                            logs))))
```

## Project 4: Cross-Lisp Code Sharing

One dream of Lisp is portable code. Let's write a library that works across multiple Lisps.

### Portable Math Library

```lisp
;;; portable-math.lisp - Works in multiple Lisps

;; Feature detection
#+common-lisp (defpackage :portable-math (:use :cl))
#+scheme (define-module (portable-math))
#+clojure (ns portable-math.core)

;; Portable function definition
#+common-lisp
(defun factorial (n)
  (if (<= n 1)
      1
      (* n (factorial (- n 1)))))

#+scheme
(define (factorial n)
  (if (<= n 1)
      1
      (* n (factorial (- n 1)))))

#+clojure
(defn factorial [n]
  (if (<= n 1)
      1
      (* n (factorial (dec n)))))

;; Portable macro
#+common-lisp
(defmacro time-it (expr)
  `(let ((start (get-internal-real-time)))
     (prog1 ,expr
       (format t "Time: ~F seconds~%"
               (/ (- (get-internal-real-time) start)
                  internal-time-units-per-second)))))

#+clojure
(defmacro time-it [expr]
  `(let [start# (System/nanoTime)
         result# ~expr]
     (println "Time:" (/ (- (System/nanoTime) start#) 1e9) "seconds")
     result#))

;; Statistical functions portable across Lisps
(defun mean (numbers)
  #+common-lisp (/ (reduce #'+ numbers) (length numbers))
  #+clojure (/ (reduce + numbers) (count numbers))
  #+scheme (/ (apply + numbers) (length numbers)))

(defun standard-deviation (numbers)
  (let ((m (mean numbers)))
    #+common-lisp
    (sqrt (mean (mapcar (lambda (x) (expt (- x m) 2)) numbers)))

    #+clojure
    (Math/sqrt (mean (map #(Math/pow (- % m) 2) numbers)))

    #+scheme
    (sqrt (mean (map (lambda (x) (expt (- x m) 2)) numbers)))))
```

## Project 5: Building a Complete Application

Let's build a markdown blog engine that demonstrates real-world Lisp usage.

### Clojure Blog Engine

```clojure
(ns blog-engine.core
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [markdown.core :as md]
            [hiccup.page :as page]
            [ring.adapter.jetty :as jetty]
            [compojure.core :refer [defroutes GET]]
            [compojure.route :as route]))

;; Data model
(defrecord Post [id title slug content date tags])

;; Parse front matter
(defn parse-front-matter [content]
  (if (str/starts-with? content "---")
    (let [parts (str/split content #"---" 3)]
      (when (>= (count parts) 3)
        (let [metadata (second parts)
              content (nth parts 2)]
          {:metadata (into {}
                          (map #(let [[k v] (str/split % #":" 2)]
                                 [(keyword (str/trim k))
                                  (str/trim v)])
                               (str/split-lines metadata)))
           :content content})))
    {:metadata {} :content content}))

;; Load posts from filesystem
(defn load-posts [dir]
  (->> (file-seq (io/file dir))
       (filter #(.endsWith (.getName %) ".md"))
       (map (fn [file]
              (let [content (slurp file)
                    {:keys [metadata content]} (parse-front-matter content)]
                (->Post
                 (str (hash file))
                 (:title metadata)
                 (:slug metadata (str/replace (:title metadata) #"\s+" "-"))
                 (md/md-to-html-string content)
                 (:date metadata)
                 (str/split (:tags metadata "") #",")))))
       (sort-by :date)
       reverse))

;; HTML generation
(defn layout [title & content]
  (page/html5
   [:head
    [:title title]
    [:meta {:charset "utf-8"}]
    [:style "body { font-family: sans-serif; max-width: 800px; margin: 0 auto; }"]]
   [:body
    [:header
     [:h1 "My Lisp Blog"]
     [:nav
      [:a {:href "/"} "Home"] " | "
      [:a {:href "/about"} "About"] " | "
      [:a {:href "/archive"} "Archive"]]]
    [:main content]
    [:footer
     [:p "Powered by Clojure"]]]))

(defn index-page [posts]
  (layout "Home"
          (for [post (take 5 posts)]
            [:article
             [:h2 [:a {:href (str "/post/" (:slug post))} (:title post)]]
             [:p.date (:date post)]
             [:div.content (:content post)]
             [:p.tags (str/join ", " (:tags post))]])))

(defn post-page [post]
  (layout (:title post)
          [:article
           [:h1 (:title post)]
           [:p.date (:date post)]
           [:div.content (:content post)]
           [:p.tags (str/join ", " (:tags post))]
           [:a {:href "/"} "← Back to home"]]))

;; Routes
(defn create-app [posts]
  (defroutes app-routes
    (GET "/" []
      (index-page posts))

    (GET "/post/:slug" [slug]
      (if-let [post (first (filter #(= (:slug %) slug) posts))]
        (post-page post)
        {:status 404 :body "Post not found"}))

    (route/not-found "Not Found")))

;; Main
(defn -main []
  (let [posts (load-posts "posts/")
        app (create-app posts)]
    (jetty/run-jetty app {:port 3000 :join? false})))
```

## Lessons Learned Across Lisps

Building the same projects in different Lisps reveals patterns:

### Common Patterns

1. **Data-first design**: All Lisps encourage thinking about data structures first
2. **REPL-driven development**: Build incrementally, test immediately
3. **Functional core**: Even in imperative operations, functional thinking dominates
4. **Macros for DSLs**: When you need a new language, macros deliver

### Dialect Strengths

**Common Lisp**: When you need everything built-in and maximum control
**Scheme**: When elegance and teaching clarity matter most
**Clojure**: When you need JVM libraries and modern concurrency
**ClojureScript**: When the browser is your target
**Emacs Lisp**: When extending your editor is the goal

### Portability Strategies

1. **Core algorithms**: Mathematical and algorithmic code ports easily
2. **Data structures**: Lists and hash tables are universal
3. **Conditional compilation**: Use feature flags for dialect-specific code
4. **Abstraction layers**: Hide platform specifics behind common interfaces

## The Polyglot Lisper

Mastering multiple Lisps makes you a better programmer in all of them. You learn:

- Common Lisp's pragmatism informs your Clojure architecture
- Scheme's minimalism clarifies your Common Lisp code
- Clojure's immutability improves your Emacs Lisp
- ClojureScript's constraints teach efficient design

The projects in this chapter aren't just exercises—they're templates for real applications. Modify them, extend them, combine them. The beauty of Lisp is that the distance from toy to production is shorter than in any other language family.

---

*"The programmer is omnipotent not as an individual but as an intellect augmented with a programming environment and language that brings out maximum leverage." - Patrick Winston*

Final chapter ahead: The future of Lisp. Where is the eternal language heading? What new domains will it conquer? And why, nearly 70 years after its creation, is Lisp more relevant than ever?