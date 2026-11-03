(ns clordle
  (:require [clojure.string :as str]
            [reagent.core :as r]
            [reagent.dom :as rdom]))

(def max-guesses 6)
(def step-ms 260)
(def flip-ms 500)  

(def win-messages
  ["Genius!" "Magnificent!" "Impressive!" "Splendid!" "Great!" "Phew!"])

(def themes
  [{:id "system"    :label "System"    :bg "linear-gradient(135deg, #faf4ed 50%, #191724 50%)"
    :swatches ["#b4637a" "#286983" "#eb6f92" "#31748f"]}
   {:id "rose-pine" :label "Rosé Pine" :bg "#191724" :swatches ["#eb6f92" "#f6c177" "#31748f" "#c4a7e7"]}
   {:id "moon"      :label "Moon"      :bg "#232136" :swatches ["#eb6f92" "#f6c177" "#3e8fb0" "#c4a7e7"]}
   {:id "dawn"      :label "Dawn"      :bg "#faf4ed" :swatches ["#b4637a" "#ea9d34" "#286983" "#907aa9"]}])

(def keyboard-rows
  [["q" "w" "e" "r" "t" "y" "u" "i" "o" "p"]
   ["a" "s" "d" "f" "g" "h" "j" "k" "l"]
   [:enter "z" "x" "c" "v" "b" "n" "m" :backspace]])

(def reasons
  {:macro        "is a macro, not a function"
   :special-form "is a special form, not a function"
   :multimethod  "is a multimethod, not a function"
   :value        "is a var, not a function"
   :mismatch     "failed verification: clojuredocs and the JVM don't match :/"
   :unresolved   "is on clojuredocs but couldn't be verified"})

(def empty-stats {:played 0 :won 0 :streak 0 :max-streak 0 :distribution {}})

(defn- stored [k]
  (try (some-> (.getItem js/localStorage (str "clordle/" k)) read-string)
       (catch :default _ nil)))

(defn- store! [k v]
  (try (.setItem js/localStorage (str "clordle/" k) (pr-str v))
       (catch :default _ nil)))

(defn score 
  [guess target]
  (let [exact?    (mapv = guess target)
        leftovers (frequencies (keep-indexed (fn [i c] (when-not (exact? i) c)) target))]
    (first
     (reduce (fn [[res avail] [i c]]
               (cond (exact? i)             [(conj res :correct) avail]
                     (pos? (get avail c 0)) [(conj res :present) (update avail c dec)]
                     :else                  [(conj res :absent) avail]))
             [[] leftovers]
             (map-indexed vector guess)))))

(def ^:private rank {:absent 1 :present 2 :correct 3})

(defn key-colors
  "letter -> best state seen"
  [guesses target]
  (reduce (fn [m guess]
            (reduce (fn [m [c s]] (if (> (rank s) (rank (m c) 0)) (assoc m c s) m))
                    m
                    (map vector guess (score guess target))))
          {}
          guesses))

(defn- truncate [s n]
  (if (<= (count s) n)
    s
    (let [cut   (subs s 0 n)
          space (.lastIndexOf cut " ")]
      (str (subs cut 0 (if (pos? space) space n)) "..."))))

(defn- clean-doc
  "join docstrings into paragraphs so they work at any width"
  [doc]
  (->> (str/split (str/trim (or doc "")) #"\n\s*\n")
       (map #(str/replace % #"\s*\n\s*" " "))
       (str/join "\n\n")))

(defn- redact [text word]
  (.replace text (js/RegExp. (str "\\b" word "\\b") "gi") (apply str (repeat (count word) "_"))))

(defn- hint-text
  "the docstring with the name hidden"
  [{fn-name :name :keys [doc arglists]}]
  (if (str/blank? doc)
    (let [blank (apply str (repeat (count fn-name) "_"))]
      (str "No docstring for this one. Signature: "
           (str/join "  " (for [args arglists]
                            (str "(" blank (when (seq args) (str " " args)) ")")))))
    (truncate (redact (str/replace doc #"\s+" " ") fn-name) 220)))

(defonce !state (r/atom {:loading? true}))
(defonce !counter (atom 0))

(defn- next-id [] (swap! !counter inc))

(defn- answer-pool 
  [{:keys [dict settings]}]
  (cond->> (:words dict)
    (= :core (:answers settings)) (filter #(= "clojure.core" (:ns %)))))

(defn- busy? [st]
  (or (:flipping st) (not= :playing (get-in st [:game :phase]))))

(defn- hint-available? [st]
  (and (not (busy? st))
       (>= (count (get-in st [:game :guesses])) 2)))

(defn- save-game! []
  (store! "game" (update (:game @!state) :target :name)))

(defn toast!
  ([content] (toast! content 1800))
  ([content ms]
   (let [id (next-id)]
     (swap! !state update :toasts (fnil conj []) {:id id :content content})
     (js/setTimeout (fn [] (swap! !state update :toasts #(filterv (fn [t] (not= id (:id t))) %)))
                    ms))))

(defn- shake! []
  (let [id (next-id)]
    (swap! !state assoc :shake id)
    (js/setTimeout #(swap! !state (fn [st] (if (= id (:shake st)) (dissoc st :shake) st)))
                   600)))

(defn open! [modal] (swap! !state assoc :modal modal))
(defn close! [] (swap! !state dissoc :modal))

(defn new-game! []
  (swap! !state
         (fn [st]
           (let [previous (get-in st [:game :target :name])
                 options  (vec (remove #(= previous (:name %)) (answer-pool st)))]
             (-> st
                 (assoc :game {:id      (next-id)
                               :target  (rand-nth options)
                               :guesses []
                               :phase   :playing}
                        :current "" :revealed 0 :flipping nil
                        :celebrate? false :hint? false :hint-seen? false)
                 (dissoc :modal)))))
  (save-game!))

(defn- record-result! [won? n-guesses]
  (swap! !state update :stats
         (fn [s]
           (let [s (cond-> (update s :played inc)
                     won?       (-> (update :won inc)
                                    (update :streak inc)
                                    (update-in [:distribution n-guesses] (fnil inc 0)))
                     (not won?) (assoc :streak 0))]
             (assoc s :max-streak (max (:max-streak s) (:streak s))))))
  (store! "stats" (:stats @!state)))

(defn type-letter! [letter]
  (let [st @!state]
    (when (and (not (busy? st))
               (< (count (:current st)) (get-in st [:dict :length])))
      (swap! !state update :current str letter))))

(defn backspace! []
  (when-not (busy? @!state)
    (swap! !state update :current #(subs % 0 (max 0 (dec (count %)))))))

(defn- rejection
  "nil if word is a valid guess, otherwise the message to show"
  [st word]
  (let [{:keys [length non-functions]} (:dict st)]
    (cond
      (< (count word) length)      "Not enough letters"
      (contains? (:valid st) word) nil
      :else (if-let [why (get non-functions word)]
              [:span [:code word] " " (reasons why)]
              [:span [:code word] " is not a Clojure function"]))))

(defn submit! []
  (let [st @!state]
    (when-not (busy? st)
      (if-let [message (rejection st (:current st))]
        (do (shake!) (toast! message))
        (let [guess  (:current st)
              row    (count (get-in st [:game :guesses]))
              target (get-in st [:game :target :name])
              won?   (= guess target)
              phase  (cond won?                       :won
                           (= (inc row) max-guesses)  :lost
                           :else                      :playing)]
          (swap! !state #(-> %
                             (update-in [:game :guesses] conj guess)
                             (assoc-in [:game :phase] phase)
                             (assoc :current "" :flipping row)))
          (save-game!)
          (when (not= phase :playing)
            (record-result! won? (inc row)))
          ;; keyboard colors and the outcome arrive once the flip is over
          (js/setTimeout
           (fn []
             (swap! !state assoc :flipping nil :revealed (inc row))
             (case phase
               :won  (do (swap! !state assoc :celebrate? true)
                         (toast! (win-messages row) 2000)
                         (js/setTimeout #(open! :stats) 1700))
               :lost (do (toast! [:span "It was " [:code target]] 2400)
                         (js/setTimeout #(open! :stats) 1700))
               nil))
           (+ (* (dec (count guess)) step-ms) flip-ms)))))))

(defn give-up-or-new!
  "starting another game halfway through one counts as a loss"
  []
  (let [st @!state
        {:keys [phase guesses target]} (:game st)]
    (when-not (:flipping st)
      (when (and (= phase :playing) (seq guesses))
        (record-result! false nil)
        (toast! [:span "It was " [:code (:name target)]] 2400))
      (new-game!))))

(defonce ^:private light-query (js/matchMedia "(prefers-color-scheme: light)"))

(defn- effective-theme
  "\"system\" follows the device: Dawn in light mode, Rosé Pine in dark mode"
  [{:keys [theme system-light?]}]
  (if (= theme "system")
    (if system-light? "dawn" "rose-pine")
    theme))

(defn- apply-theme! []
  (when-let [t (effective-theme @!state)]
    (.setAttribute (.-documentElement js/document) "data-theme" t)))

(defn set-theme! [id]
  (swap! !state assoc :theme id)
  (store! "theme" id)
  (apply-theme!))

(defn set-answers!
  "changes the answer pool (from clojure.core to any func ns)"
  [mode]
  (when (not= mode (get-in @!state [:settings :answers]))
    (swap! !state assoc-in [:settings :answers] mode)
    (store! "settings" (:settings @!state))
    (let [{:keys [phase guesses]} (:game @!state)]
      (if (and (= phase :playing) (empty? guesses))
        (do (swap! !state #(assoc-in % [:game :target] (rand-nth (vec (answer-pool %)))))
            (save-game!))
        (toast! "Takes effect from the next game")))))

(def ^:private icon-paths
  {:help      [[:circle {:cx 12 :cy 12 :r 10}]
               [:path {:d "M9.09 9a3 3 0 0 1 5.83 1c0 2-3 3-3 3"}]
               [:path {:d "M12 17h.01"}]]
   :settings  [[:path {:d "M20 7h-9"}] [:path {:d "M14 17H5"}]
               [:circle {:cx 17 :cy 17 :r 3}] [:circle {:cx 7 :cy 7 :r 3}]]
   :stats     [[:path {:d "M3 3v18h18"}] [:path {:d "M18 17V9"}]
               [:path {:d "M13 17V5"}] [:path {:d "M8 17v-3"}]]
   :new-game  [[:path {:d "M3 12a9 9 0 1 0 9-9 9.75 9.75 0 0 0-6.74 2.74L3 8"}]
               [:path {:d "M3 3v5h5"}]]
   :enter     [[:polyline {:points "9 10 4 15 9 20"}] [:path {:d "M20 4v7a4 4 0 0 1-4 4H4"}]]
   :backspace [[:path {:d "M20 5H9l-7 7 7 7h11a2 2 0 0 0 2-2V7a2 2 0 0 0-2-2Z"}]
               [:path {:d "m18 9-6 6"}] [:path {:d "m12 9 6 6"}]]
   :hint      [[:path {:d "M15 14c.2-1 .7-1.7 1.5-2.5 1-.9 1.5-2.2 1.5-3.5A6 6 0 0 0 6 8c0 1 .2 2.2 1.5 3.5.7.7 1.3 1.5 1.5 2.5"}]
               [:path {:d "M9 18h6"}] [:path {:d "M10 22h4"}]]
   :close     [[:path {:d "M18 6 6 18"}] [:path {:d "m6 6 12 12"}]]
   :hide      [[:path {:d "M10.733 5.076a10.744 10.744 0 0 1 11.205 6.575 1 1 0 0 1 0 .696 10.747 10.747 0 0 1-1.444 2.49"}]
               [:path {:d "M14.084 14.158a3 3 0 0 1-4.242-4.242"}]
               [:path {:d "M17.479 17.499a10.75 10.75 0 0 1-15.417-5.151 1 1 0 0 1 0-.696 10.75 10.75 0 0 1 4.446-5.143"}]
               [:path {:d "m2 2 20 20"}]]
   :external  [[:path {:d "M15 3h6v6"}] [:path {:d "M10 14 21 3"}]
               [:path {:d "M18 13v6a2 2 0 0 1-2 2H5a2 2 0 0 1-2-2V8a2 2 0 0 1 2-2h6"}]]
   :heart     [[:path {:d "M19 14c1.49-1.46 3-3.21 3-5.5A5.5 5.5 0 0 0 16.5 3c-1.76 0-3 .5-4.5 2-1.5-1.5-2.74-2-4.5-2A5.5 5.5 0 0 0 2 8.5c0 2.3 1.5 4.05 3 5.5l7 7Z"}]]})

(defn icon [k]
  (into [:svg.icon {:view-box "0 0 24 24" :fill "none" :stroke "currentColor"
                    :stroke-width 2 :stroke-linecap "round" :stroke-linejoin "round"
                    :aria-hidden true}]
        (icon-paths k)))

(defn icon-button [k label on-click]
  [:button.icon-button {:type "button" :title label :aria-label label :on-click on-click}
   [icon k]])


(defn header []
  (let [{:keys [dict]} @!state]
    [:<>
     [:header.header
      [:div.group
       [icon-button :help "How to play" #(open! :help)]
       [icon-button :settings "Settings" #(open! :settings)]]
      [:h1.logo [:span.paren "("] "clordle" [:span.paren ")"]]
      [:div.group
       [icon-button :stats "Statistics" #(open! :stats)]
       [icon-button :new-game "New game" #(give-up-or-new!)]]]
     [:p.subtitle "guess the Clojure " [:strong "function"] " - "
      [:strong (:length dict) " letters"]]]))

(defn- cx [& xs] (str/join " " (remove nil? xs)))

(defn board []
  (let [{:keys [dict game current flipping shake celebrate?]} @!state
        {:keys [id target guesses phase]} game
        n (:length dict)]
    [:div.board {:key id :style {"--n" n} :role "grid" :aria-label "Board"}
     (for [i (range max-guesses)]
       (let [guess    (get guesses i)
             current? (and (= i (count guesses)) (= phase :playing))
             letters  (cond guess guess current? current :else "")
             colors   (when guess (score guess (:name target)))]
         [:div.row {:key   (str i (when current? (str "-" shake)))
                    :role  "row"
                    :class (cx (when current? "current")
                               (when (and current? shake) "shake")
                               (when (= i flipping) "flipping")
                               (when (and celebrate? (= i (dec (count guesses)))) "celebrate"))}
          (for [j (range n)]
            (let [c (get letters j)]
              [:div.tile {:key   j
                          :role  "gridcell"
                          :style {"--i" j}
                          :class (cx (when c "filled") (some-> colors (nth j) name))}
               c]))]))]))

(defn hint []
  (let [{:keys [game hint? hint-seen?]} @!state
        target (:target game)]
    (if hint?
      [:div.hint
       [:div.hint-header
        [icon :hint] "hint" [:code (:ns target)]
        [:button.hint-hide {:type "button" :title "Hide hint" :aria-label "Hide hint"
                            :on-click #(swap! !state assoc :hint? false)}
         [icon :hide]]]
       [:p (hint-text target)]]
      [:div.bar
       [:button.button.ghost {:type "button"
                              :on-click #(swap! !state assoc :hint? true :hint-seen? true)}
        [icon :hint] (if hint-seen? "Show hint" "Need a hint?")]])))

(defn bottom-bar []
  (let [st @!state
        phase (get-in st [:game :phase])]
    (cond
      (and (not= phase :playing) (not (:flipping st)))
      [:div.bar
       [:button.button.primary {:type "button" :on-click #(new-game!)} [icon :new-game] "New game"]
       [:button.button {:type "button" :on-click #(open! :stats)} [icon :stats] "Result"]]

      (hint-available? st) [hint]
      :else                [:div.bar])))

(defn keyboard []
  (let [{:keys [game revealed]} @!state
        colors (key-colors (take revealed (:guesses game))
                           (get-in game [:target :name]))]
    [:div.keyboard {:role "group" :aria-label "Keyboard"}
     (for [[i row] (map-indexed vector keyboard-rows)]
       [:div.keyboard-row {:key i}
        (for [k row]
          (case k
            :enter     [:button.key.wide {:key "enter" :type "button" :aria-label "Enter"
                                          :on-click #(submit!)}
                        [:span.key-label "enter"]
                        [:span.key-icon [icon :enter]]]
            :backspace [:button.key.wide {:key "backspace" :type "button" :aria-label "Backspace"
                                          :on-click #(backspace!)}
                        [icon :backspace]]
            [:button.key {:key k :type "button"
                          :class (some-> (colors k) name)
                          :on-click #(type-letter! k)}
             k]))])]))

(defn toasts []
  [:div.toasts {:aria-live "polite"}
   (for [{:keys [id content]} (:toasts @!state)]
     [:div.toast {:key id} content])])

(defn modal [title & children]
  [:div.modal-backdrop {:on-click #(when (= (.-target %) (.-currentTarget %)) (close!))}
   [:div.modal {:role "dialog" :aria-modal true :aria-label title}
    [:div.modal-header
     [:h2 title]
     [icon-button :close "Close" #(close!)]]
    (into [:div.modal-body] children)]])

(defn example [word idx state & text]
  [:div.example
   [:div.row
    (for [[j c] (map-indexed vector word)]
      [:div.tile {:key j :class (if (= j idx) (name state) "filled")} c])]
   (into [:p] text)])

(defn histogram [{:keys [histogram length]}]
  (let [maximum (apply max (vals histogram))
        rows    (sort histogram)]
    [:figure.histogram
     [:div.hist-columns {:style {"--cols" (count rows)} :aria-hidden true}
      (for [[len n] rows
            :let [pct     (* 100 (/ n maximum))
                  winner? (= len length)]]
        [:div.hist-col {:key      len
                        :class    (when winner? "winner")
                        :data-tip (str n (if (= n 1) " function" " functions") " with " len
                                       (if (= len 1) " letter" " letters"))}
         [:div.hist-area
          [:div.hist-bar {:style {:height (str pct "%")}}]
          (when winner?
            [:span.hist-value {:style {:bottom (str "calc(" pct "% + 4px)")}} n])]
         [:span.hist-label len]])]
     [:figcaption "functions by number of letters (hover over a column)"]
     [:table.sr-only
      [:caption "Functions by number of letters"]
      [:thead [:tr [:th "Letters"] [:th "Functions"]]]
      [:tbody (for [[len n] rows] [:tr {:key len} [:td len] [:td n]])]]]))

(defn- core-count [dict]
  (count (filter #(= "clojure.core" (:ns %)) (:words dict))))

(defn answers-picker []
  (let [{:keys [settings dict]} @!state
        answers (:answers settings)]
    [:div.segmented
     [:button {:type "button" :class (when (= :all answers) "active")
               :on-click #(set-answers! :all)}
      "All of clojuredocs" [:small (count (:words dict)) " functions"]]
     [:button {:type "button" :class (when (= :core answers) "active")
               :on-click #(set-answers! :core)}
      "clojure.core only" [:small (core-count dict) " functions"]]]))

(defn help-modal []
  (let [{:keys [dict]} @!state
        {:keys [length summary words]} dict]
    [modal "How to play"
     [:p "Guess the " [:strong "Clojure function"] " with " [:strong length " letters"]
      " in " max-guesses " tries. Every guess has to be a real function: "
      [:code "when"] " or " [:code "defn"] " don't count, as they are macros"]
     [example "conj" 0 :correct "The " [:code "c"] " is in the right spot."]
     [example "rest" 1 :present "The " [:code "e"] " is in the word, but in another spot."]
     [example "drop" 3 :absent "The " [:code "p"] " is not in the word."]
     [:div.divider]
     [:h3 "Prefer " [:code "clojure.core"] " only?"]
     [:p "By default the answer can be any of the " (count words) " functions on clojuredocs, "
      "including namespaces like " [:code "clojure.math"] ", " [:code "clojure.zip"] " or "
      [:code "clojure.core.async"] ". If you'd rather stick to the " (core-count dict) " in "
      [:code "clojure.core"] ", pick it here or later in Settings. Every function stays "
      "a valid guess either way."]
     [answers-picker]
     [:div.divider]
     [:h3 "Why " length " letters?"]
     [:p "The dictionary comes from the " [:strong (:clojuredocs-vars summary)] " vars on clojuredocs.org. "
      "Only the " [:strong (:verified-functions summary)] " that are functions according to clojuredocs "
      [:em "and"] " the JVM count: not a macro or special form, and " [:code "(fn? f)"] " returns "
      [:code "true"] ". Of the " (:letter-only-names summary) " names made only of letters, "
      "the most common length is " [:strong length] ", with " (get-in dict [:histogram length]) " functions."]
     [histogram dict]]))

(defn settings-modal []
  (let [{:keys [theme dict]} @!state
        total (count (:words dict))]
    [modal "Settings"
     [:h3 "Theme"]
     [:div.themes
      (for [{:keys [id label bg swatches]} themes]
        [:button.theme {:key id :type "button"
                        :class (when (= id theme) "active")
                        :on-click #(set-theme! id)}
         [:span.theme-preview {:style {"--swatch-bg" bg}}
          (for [c swatches] [:i {:key c :style {:background c}}])]
         label])]
     [:p "System follows your device: Dawn in light mode, Rosé Pine in dark mode."]
     [:h3 "Where the answer comes from"]
     [answers-picker]
     [:p "Any of the " total " functions is a valid guess; this only changes the pool "
      "the answer is drawn from."]
     [:div.divider]
     [:p.footnote "Dictionary built from "
      [:a {:href (:source dict) :target "_blank" :rel "noopener"} "clojuredocs.org"]
      " and verified with Clojure " (:clojure dict) "."]]))

(defn fn-card [{fn-name :name fn-ns :ns :keys [arglists doc href]}]
  [:div.fn-card
   [:div.fn-card-header
    [:span.fn-ns fn-ns "/"]
    [:span.fn-name fn-name]
    [:span.verified "(fn? " fn-name ") => " [:b "true"]]]
   [:div.fn-sigs
    (for [args arglists]
      [:code.sig {:key args} "(" [:b fn-name] (when (seq args) (str " " args)) ")"])]
   (when (seq doc) [:p.fn-doc (truncate (clean-doc doc) 420)])
   [:a.link {:href (str "https://clojuredocs.org" href) :target "_blank" :rel "noopener"}
    "View on clojuredocs" [icon :external]]])

(defn stat [value label]
  [:div.stat [:span.stat-value value] [:span.stat-label label]])

(defn statistics [{:keys [played won streak max-streak distribution]} highlight]
  (let [maximum (apply max 1 (vals distribution))]
    [:<>
     [:div.stats
      [stat played "Played"]
      [stat (str (if (pos? played) (.round js/Math (* 100 (/ won played))) 0) "%") "Win rate"]
      [stat streak "Current streak"]
      [stat max-streak "Max streak"]]
     [:h3 "Guess distribution"]
     [:div.distribution
      (for [i (range 1 (inc max-guesses))
            :let [n (get distribution i 0)]]
        [:div.dist-row {:key i}
         [:span.dist-num i]
         [:div.dist-track
          [:div.dist-bar {:class (when (= i highlight) "current")
                          :style {:width (str "calc((100% - 2.5rem) * " (/ n maximum) ")")}}]
          [:span.dist-value n]]])]]))

(defn stats-modal []
  (let [{:keys [game stats flipping]} @!state
        {:keys [target guesses phase]} game
        over? (and (not= phase :playing) (not flipping))
        won?  (= phase :won)]
    [modal (cond (not over?) "Statistics"
                 won?        (win-messages (dec (count guesses)))
                 :else       "Not this time")
     (when over?
       [:<>
        [:p.result-text
         (if won?
           [:<> "You solved it in " [:strong (count guesses) "/" max-guesses] ". The function was:"]
           "The answer was:")]
        [fn-card target]
        [:div.divider]])
     [statistics stats (when (and over? won?) (count guesses))]
     (when over?
       [:button.button.primary.block {:type "button" :on-click #(new-game!)}
        [icon :new-game] "New game"])]))

(defn footer []
  [:footer.footer
   "made with " [:span.heart {:aria-label "love" :role "img"} [icon :heart]] " by "
   [:a {:href "https://github.com/Jimmysit0" :target "_blank" :rel "noopener"} "jimmysit0"]
   [:span.sep "|"]
   [:a {:href "https://github.com/Jimmysit0/clordle" :target "_blank" :rel "noopener"} "star this project"]])

(defn app []
  (let [{:keys [loading? error modal]} @!state]
    (cond
      error    [:div.error
                [:h2 "Couldn't load the dictionary"]
                [:p error]
                [:p "Build it with " [:code "clojure -M:dictionary"] " and reload."]]
      loading? [:div.loading "loading dictionary..."]
      :else
      [:div.app
       [header]
       [:main [board] [bottom-bar]]
       [keyboard]
       [footer]
       [toasts]
       (case modal
         :help     [help-modal]
         :settings [settings-modal]
         :stats    [stats-modal]
         nil)])))

(defn- on-keydown [e]
  (let [k (.-key e)]
    (when-not (or (.-metaKey e) (.-ctrlKey e) (.-altKey e))
      (cond
        (= k "Escape")             (close!) 
        (:modal @!state)           (when (and (= k "Enter")
                                              (= :stats (:modal @!state))
                                              (not= :playing (get-in @!state [:game :phase])))
                                     (.preventDefault e)
                                     (new-game!))
        (= k "Enter")              (do (.preventDefault e) (submit!))
        (= k "Backspace")          (do (.preventDefault e) (backspace!))
        (re-matches #"[a-zA-Z]" k) (type-letter! (str/lower-case k))))))

(defn- init! [dict]
  (let [by-name  (into {} (map (juxt :name identity)) (:words dict))
        saved    (stored "game")
        stats    (merge empty-stats (stored "stats"))
        restore? (and (map? saved)
                      (by-name (:target saved))
                      (every? by-name (:guesses saved)))]
    (reset! !state {:dict          dict
                    :valid         (set (keys by-name))
                    :stats         stats
                    :settings      (merge {:answers :all} (stored "settings"))
                    :theme         (or (stored "theme") "system")
                    :system-light? (.-matches light-query)
                    :current       ""
                    :toasts        []})
    (apply-theme!)
    (if restore?
      (swap! !state assoc
             :game (assoc saved :target (by-name (:target saved)) :id (next-id))
             :revealed (count (:guesses saved)))
      (new-game!))
    (when (and (zero? (:played stats)) (not restore?))
      (open! :help))))

(defn- load-dictionary! []
  (-> (js/fetch "dictionary.edn" #js {:cache "no-cache"})
      (.then (fn [resp]
               (if (.-ok resp)
                 (.text resp)
                 (throw (js/Error. (str "HTTP " (.-status resp)))))))
      (.then (fn [text] (init! (read-string text))))
      (.catch (fn [err] (swap! !state assoc :error (.-message err))))))

(.addEventListener js/window "keydown" on-keydown)
;; follow the device live when it switches between light and dark
(.addEventListener light-query "change"
                   (fn [e]
                     (swap! !state assoc :system-light? (.-matches e))
                     (apply-theme!)))
(rdom/render [app] (.getElementById js/document "app"))
(load-dictionary!)
