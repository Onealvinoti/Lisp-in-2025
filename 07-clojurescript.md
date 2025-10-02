---
layout: default
title: "Chapter 7: ClojureScript - Lisp in the Browser"
---

# Chapter 7: ClojureScript - Lisp in the Browser

> "ClojureScript is the most fun I've had programming in 25 years." - David Nolen

Remember when JavaScript was the only option for browser programming? When you had to leave your nice functional language at the server boundary and descend into callback hell? ClojureScript changed that. It's not just Clojure compiled to JavaScript—it's a better way to think about front-end development.

ClojureScript brings immutability, functional programming, and the full power of Lisp to the browser. And thanks to Google's Closure compiler (yes, Closure with an 's'), it often produces smaller, faster JavaScript than hand-written code.

## One Language, Multiple Platforms

The dream of ClojureScript is simple: write Clojure everywhere. Same syntax, same data structures, same abstractions, whether you're writing server code, browser code, or native mobile apps.

```clojure
;; This code works in both Clojure and ClojureScript
(defn calculate-tax [amount rate]
  (* amount rate))

(defn format-currency [amount]
  (str "$" (format "%.2f" amount)))  ; Works in both!

;; Share validation logic
(defn valid-email? [email]
  (re-matches #".+@.+\..+" email))

;; Share data specifications
(def user-schema
  [:map
   [:id uuid?]
   [:email string?]
   [:age pos-int?]])
```

But ClojureScript isn't just Clojure with a different compilation target. It's optimized for the browser environment:

```clojure
;; ClojureScript-specific browser interaction
(ns my-app.core
  (:require [goog.dom :as gdom]
            [goog.events :as events]))

;; Direct DOM manipulation
(def button (gdom/getElement "my-button"))
(events/listen button "click"
  (fn [e]
    (js/alert "Clicked!")))

;; JavaScript interop is seamless
(js/console.log "Hello from ClojureScript!")
(js/setTimeout #(println "Delayed") 1000)

;; Access JavaScript objects naturally
(.-length (js/Array 1 2 3))  ; => 3
(.toUpperCase "hello")        ; => "HELLO"

;; Use JavaScript libraries
(def moment (js/require "moment"))
(.format (moment) "YYYY-MM-DD")
```

## React and Reagent

ClojureScript's killer app is React development. Reagent wraps React in a Clojurey embrace, making UI development a joy:

```clojure
(ns my-app.views
  (:require [reagent.core :as r]))

;; Components are just functions returning hiccup
(defn hello-component [name]
  [:div
   [:h1 "Hello, " name "!"]
   [:p "Welcome to ClojureScript"]])

;; State is managed with atoms
(def app-state (r/atom {:counter 0
                        :user nil}))

;; Reactive components re-render when atoms change
(defn counter-component []
  [:div
   [:h2 "Counter: " (:counter @app-state)]
   [:button {:on-click #(swap! app-state update :counter inc)}
    "Increment"]
   [:button {:on-click #(swap! app-state update :counter dec)}
    "Decrement"]])

;; Compose components naturally
(defn app []
  [:div.container
   [hello-component "World"]
   [counter-component]
   [:p "Counter value squared: " (* (:counter @app-state)
                                     (:counter @app-state))]])

;; Mount the app
(r/render [app]
  (js/document.getElementById "app"))
```

This is React, but better. No JSX, no class components, no `this` binding issues. Just functions and data.

## Shadow-cljs and Modern Tooling

In 2025, shadow-cljs has become the de facto build tool for ClojureScript. It understands npm, provides hot reloading, and makes ClojureScript feel like a first-class citizen in the JavaScript ecosystem:

```clojure
;; shadow-cljs.edn
{:source-paths ["src"]
 :dependencies [[reagent "1.2.0"]
                [re-frame "1.3.0"]]

 :builds
 {:app {:target :browser
        :output-dir "public/js"
        :asset-path "/js"
        :modules {:main {:init-fn my-app.core/init!}}
        :devtools {:http-root "public"
                   :http-port 8080
                   :preloads [devtools.preload]}}

  :test {:target :node-test
         :output-to "out/test.js"}

  :npm {:target :npm-module
        :output-dir "dist"}}}
```

Start development with hot reloading:
```bash
npx shadow-cljs watch app
```

Now you can use any npm package:
```clojure
(ns my-app.core
  (:require ["react" :as react]
            ["lodash" :as _]
            ["axios" :as axios]))

;; Use npm packages directly
(defn fetch-data []
  (-> (axios/get "/api/data")
      (.then (fn [response]
               (println (.-data response))))))

;; Mix with ClojureScript libraries
(def debounced-save
  (js/_.debounce save-to-server 1000))
```

## Building Full-Stack Applications

The real power of ClojureScript shines in full-stack applications. Same language, shared code, seamless development:

```clojure
;; shared/validation.cljc - Works in both Clojure and ClojureScript
(ns myapp.shared.validation)

(defn valid-password? [password]
  (and (string? password)
       (>= (count password) 8)
       (re-find #"[0-9]" password)
       (re-find #"[A-Z]" password)))

(defn validate-user [user]
  (cond
    (not (valid-email? (:email user)))
    {:error "Invalid email"}

    (not (valid-password? (:password user)))
    {:error "Password must be 8+ chars with number and uppercase"}

    :else
    {:success true}))

;; backend/api.clj - Server-side Clojure
(ns myapp.backend.api
  (:require [myapp.shared.validation :as v]))

(defn register-handler [req]
  (let [user (:body req)
        validation (v/validate-user user)]
    (if (:success validation)
      (create-user! user)
      {:status 400 :body validation})))

;; frontend/views.cljs - Client-side ClojureScript
(ns myapp.frontend.views
  (:require [myapp.shared.validation :as v]
            [reagent.core :as r]))

(defn registration-form []
  (let [user (r/atom {:email "" :password ""})
        error (r/atom nil)]
    (fn []
      [:form
       [:input {:type "email"
                :value (:email @user)
                :on-change #(swap! user assoc :email (-> % .-target .-value))}]
       [:input {:type "password"
                :value (:password @user)
                :on-change #(swap! user assoc :password (-> % .-target .-value))}]

       ;; Client-side validation using shared code
       [:button {:on-click (fn [e]
                            (.preventDefault e)
                            (let [validation (v/validate-user @user)]
                              (if (:success validation)
                                (submit-registration @user)
                                (reset! error (:error validation)))))}
        "Register"]

       (when @error
         [:p.error @error])])))
```

## Re-frame: Architecture for SPAs

Re-frame is to ClojureScript what Redux is to JavaScript, but better. It provides a structured way to build single-page applications:

```clojure
(ns my-app.events
  (:require [re-frame.core :as rf]))

;; Define events (similar to Redux actions)
(rf/reg-event-fx
 ::initialize-db
 (fn [_ _]
   {:db {:todos []
         :filter :all}}))

(rf/reg-event-db
 ::add-todo
 (fn [db [_ text]]
   (update db :todos conj {:id (random-uuid)
                           :text text
                           :done false})))

(rf/reg-event-db
 ::toggle-todo
 (fn [db [_ id]]
   (update db :todos
           (fn [todos]
             (map #(if (= (:id %) id)
                     (update % :done not)
                     %)
                  todos)))))

;; Define subscriptions (computed values)
(rf/reg-sub
 ::todos
 (fn [db]
   (:todos db)))

(rf/reg-sub
 ::visible-todos
 :<- [::todos]  ; Depends on ::todos subscription
 :<- [::filter]
 (fn [[todos filter] _]
   (case filter
     :all todos
     :active (filter (complement :done) todos)
     :completed (filter :done todos))))

;; Define effects (side effects)
(rf/reg-fx
 ::save-to-local-storage
 (fn [todos]
   (js/localStorage.setItem "todos" (pr-str todos))))

;; Chain events with effects
(rf/reg-event-fx
 ::save-todos
 (fn [{:keys [db]} _]
   {:db db
    ::save-to-local-storage (:todos db)
    :dispatch-later [{:ms 1000
                     :dispatch [::show-saved-notification]}]}))

;; Views subscribe to data
(defn todo-list []
  (let [todos (rf/subscribe [::visible-todos])]
    [:ul
     (for [todo @todos]
       ^{:key (:id todo)}
       [:li {:class (when (:done todo) "completed")
             :on-click #(rf/dispatch [::toggle-todo (:id todo)])}
        (:text todo)])]))

(defn todo-input []
  (let [value (r/atom "")]
    (fn []
      [:input {:type "text"
               :value @value
               :on-change #(reset! value (-> % .-target .-value))
               :on-key-press (fn [e]
                              (when (= 13 (.-charCode e))
                                (rf/dispatch [::add-todo @value])
                                (reset! value "")))}])))

(defn app []
  [:div
   [:h1 "Todos"]
   [todo-input]
   [todo-list]])
```

This architecture scales to massive applications. The unidirectional data flow makes debugging easy, and time-travel debugging comes for free.

## Advanced ClojureScript

### Core.async in the Browser

```clojure
(ns my-app.async
  (:require-macros [cljs.core.async.macros :refer [go go-loop]])
  (:require [cljs.core.async :refer [<! >! chan timeout]]))

;; Async operations without callback hell
(defn fetch-with-timeout [url timeout-ms]
  (let [ch (chan)]
    (go
      (let [timeout-ch (timeout timeout-ms)
            response-ch (chan)]

        ;; Start fetch
        (-> (js/fetch url)
            (.then (fn [resp] (go (>! response-ch resp))))
            (.catch (fn [err] (go (>! response-ch :error)))))

        ;; Race between timeout and response
        (let [[result ch] (alts! [response-ch timeout-ch])]
          (>! ch (if (= ch timeout-ch)
                   :timeout
                   result))))
    ch))

;; Use it
(go
  (let [result (<! (fetch-with-timeout "/api/data" 5000))]
    (case result
      :timeout (println "Request timed out")
      :error (println "Request failed")
      (println "Got response:" result))))
```

### Spec in ClojureScript

```clojure
(ns my-app.spec
  (:require [cljs.spec.alpha :as s]
            [cljs.spec.gen.alpha :as gen]
            [cljs.spec.test.alpha :as stest]))

;; Define specs for runtime validation
(s/def ::email (s/and string? #(re-matches #".+@.+" %)))
(s/def ::age (s/int-in 0 130))
(s/def ::user (s/keys :req-un [::email ::age]))

;; Validate form data
(defn validate-form [data]
  (if (s/valid? ::user data)
    {:valid true}
    {:valid false
     :errors (s/explain-data ::user data)}))

;; Generate test data in development
(defn generate-test-users []
  (gen/sample (s/gen ::user) 10))

;; Instrument functions in development
(s/fdef process-user
  :args (s/cat :user ::user)
  :ret ::user)

(when ^boolean goog.DEBUG
  (stest/instrument))
```

### WebAssembly Integration

ClojureScript can even work with WebAssembly:

```clojure
(defn load-wasm-module []
  (-> (js/fetch "/my-module.wasm")
      (.then #(.arrayBuffer %))
      (.then #(js/WebAssembly.instantiate %))
      (.then (fn [result]
               (let [exports (.-exports (.-instance result))]
                 ;; Use WASM functions from ClojureScript
                 (println "Result:" (.myFunction exports 42)))))))
```

## Performance Optimization

ClojureScript with advanced compilation often outperforms hand-written JavaScript:

```clojure
;; Use type hints for performance
(defn ^number sum-array [^array arr]
  (let [len (.-length arr)]
    (loop [i 0
           sum 0]
      (if (< i len)
        (recur (inc i) (+ sum (aget arr i)))
        sum))))

;; Leverage Google Closure Compiler optimizations
(defn process-data [data]
  ;; Dead code elimination removes unused branches
  (if ^boolean goog.DEBUG
    (do (js/console.log "Processing:" data)
        (expensive-validation data))
    data))

;; Use transducers for efficient transformations
(def process-pipeline
  (comp
    (map parse-item)
    (filter valid?)
    (map transform)
    (take 1000)))

(into [] process-pipeline large-dataset)  ; No intermediate collections
```

## Testing ClojureScript

Testing ClojureScript is a first-class concern:

```clojure
(ns my-app.test
  (:require [cljs.test :refer-macros [deftest is testing run-tests]]
            [my-app.core :as core]))

(deftest test-arithmetic
  (testing "Basic math"
    (is (= 4 (+ 2 2)))
    (is (= 6 (* 2 3)))))

(deftest test-async
  (testing "Async operations"
    (async done
      (go
        (let [result (<! (fetch-data))]
          (is (= (:status result) 200))
          (done))))))

;; Property-based testing
(deftest test-properties
  (checking "Sorting is idempotent" 100
    [v (gen/vector gen/int)]
    (is (= (sort v) (sort (sort v))))))

;; Run tests in Node.js or browser
(run-tests)
```

## ClojureScript Success Stories

Companies building products with ClojureScript:

**Pitch**: Collaborative presentation software, entire frontend in ClojureScript
**Metabase**: Open-source BI tool, 100k+ lines of ClojureScript
**CircleCI**: CI/CD platform UI
**Attendify**: Event apps platform
**Adzerk**: Ad serving platform

Why they chose ClojureScript:
1. **Developer happiness**: Functional programming with live reloading
2. **Code sharing**: Same validation, logic, and specs on client and server
3. **Performance**: Smaller bundle sizes thanks to Closure Compiler
4. **Reliability**: Immutability prevents entire categories of bugs
5. **Productivity**: Small teams building complex applications

## The Mobile Story

ClojureScript isn't limited to browsers. React Native support means mobile apps too:

```clojure
(ns my-app.mobile
  (:require [reagent.core :as r]
            ["react-native" :as rn]))

(def <> r/as-element)

(defn app []
  (<> (.-View rn)
      #js {:style #js {:flex 1
                       :justifyContent "center"
                       :alignItems "center"}}
      (<> (.-Text rn)
          nil
          "Hello from ClojureScript!")
      (<> (.-Button rn)
          #js {:title "Press me"
               :onPress #(js/alert "Pressed!")})))

;; Register the app
(.registerComponent (.-AppRegistry rn) "MyApp" (constantly app))
```

## Development Workflow

The ClojureScript development experience is unmatched:

```bash
# Start shadow-cljs with hot reloading
npx shadow-cljs watch app

# Connect to REPL
npx shadow-cljs cljs-repl app

# Now you're connected to the browser!
cljs.user=> (js/alert "Hello from REPL!")
cljs.user=> (require '[my-app.core :as app])
cljs.user=> (app/get-current-state)
{:user {...} :todos [...]}

# Change code, see it instantly in browser
# No refresh needed!
```

## ClojureScript vs JavaScript Ecosystem

ClojureScript doesn't fight the JavaScript ecosystem—it embraces it:

```clojure
;; Use any React component
(def ReactDatePicker (r/adapt-react-class DatePicker))

(defn date-selector []
  [ReactDatePicker {:selected @selected-date
                    :onChange #(reset! selected-date %)}])

;; Use any JavaScript library
(def chart (js/Chart. ctx config))
(.update chart)

;; Export for JavaScript consumption
(defn ^:export calculateTax [amount rate]
  (* amount rate))

;; Now JavaScript can call: MyApp.calculateTax(100, 0.15)
```

## The Future of ClojureScript

ClojureScript continues to evolve:

- **Faster compilation**: Improvements to compiler speed
- **Better JavaScript interop**: Smoother integration with JS ecosystem
- **Smaller bundles**: Continued optimization of output size
- **Native ES modules**: Direct ES module output
- **WebAssembly backend**: Experimental WASM compilation

But the real future is in the ecosystem:

- **Babashka**: ClojureScript-like scripting everywhere
- **Scittle**: ClojureScript in script tags, no build step
- **Cherry**: Experimental ClojureScript compiler with better JS interop
- **Squint**: ClojureScript syntax that compiles to idiomatic JavaScript

## Why ClojureScript Wins

ClojureScript succeeds because it solves real problems:

1. **JavaScript fatigue**: One language, stable for years
2. **Complexity**: Immutability and functional programming reduce bugs
3. **Performance**: Google Closure Compiler produces optimized code
4. **Developer experience**: REPL-driven development with hot reloading
5. **Code sharing**: True full-stack development with shared code

But mostly, ClojureScript wins because it's fun. It makes frontend development enjoyable again. No more `this` binding issues, no more null pointer exceptions, no more "cannot read property of undefined". Just data and functions, the way programming should be.

## The ClojureScript Enlightenment

Learning ClojureScript changes how you think about frontend development. You stop thinking in terms of components and state management and start thinking in terms of data flow and transformations.

Your UIs become pure functions of your data. Your applications become predictable. Your bugs become shallow and easy to fix. And your code becomes a joy to work with.

Welcome to frontend development that doesn't hurt.

---

*"ClojureScript is not about replacing JavaScript. It's about replacing the pain of JavaScript with the joy of Clojure." - Mike Fikes*

Next: Setting up your ultimate Lisp development environment. We'll configure editors, set up REPLs, install structural editing, and create an environment where Lisp code seems to write itself.