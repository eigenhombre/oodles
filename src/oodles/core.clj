(ns ^{:author "John Jacobsen, adapted from Douglas R. Hofstadter"
      :doc "
In 1983, Douglas R. Hofstadter, author of \"Gödel, Escher, Bach: An
Eternal Golden Braid\" wrote a series of articles on Lisp in
Scientific American (later republished as the book *Metamagical Themas*).
The third and last in the series was an entertaining example
which is an instructive example on blurring the line between code and
data, which is typical for Lisp (though somewhat less common in
Clojure, except for the occasional macro).

In the example, a set of acronyms are presented; each acronym can be
\"expanded\" into a phrase which itself may contain other acronyms.
The program generates random expansions of these acronyms, \"bottoming
out\" (base case) at random, though with increasing probability of
termination for more deeply-nested expressions.
"}
  oodles.core)


(defmacro defpasta
  "
  First, a small macro helps us keep things tidy: `defpasta` makes a
  function that simply returns the list supplied as its argument,
  avoiding the need for the argument vector or the quote.  Note that
  just one small macro provides enough \"syntactic sugar\" to make the
  examples more readable.  We also attach metadata for convenience in
  testing (see below).
  "
  [name_ & expr]
  `(defn ~(with-meta name_ {:pasta true}) []
     '~@expr))


;; Next are the actual acronym definitions. These are the definitions
;; Hofstadter uses, with commas treated differently.  The commas are
;; important in his output, but commas are whitespace in Clojure... so
;; where the symbols are used with commas in compound statements, we
;; append `_CO` to tell the program to supply a comma in the output.
;; We cannot, unfortunately, add commas after lists in this way, but
;; his example is inconsistent in this regard (three examples have
;; commas after lists, but the example expansion later in the article
;; does not).
;;
;; Hostadter presents the acronyms in capital letters, though
;; Common Lisp atoms (analogous to Clojure symbols) are
;; case-insensitive; we follow that convention here and use it to
;; indicate terms which may be expanded... that is to say, the
;; `COFFEE` in the expansion of `SAUCE` can itself be expanded to a
;; phrase which includes `ESPRESSO`, and so on.
(defpasta TOMATOES (TOMATOES on MACARONI (and TOMATOES only)
                             exquisitely SPICED))
(defpasta MACARONI (MACARONI and CHEESE (a REPAST of Naples_CO Italy)))
(defpasta REPAST (rather extraordinary PASTA and SAUCE_CO typical))
(defpasta CHEESE (cheddar_CO havarti_CO, emmentaler
                                (especially SHARP emmenthaler)))
(defpasta SHARP (strong_CO hearty_CO and rather pungent))
(defpasta SPICED (sweetly pickled in CHEESE ENDIVE dressing))
(defpasta ENDIVE (egg NOODLES_CO dipped in vinegar eggnog))
(defpasta NOODLES (NOODLES (oodles of delicious LINGUINI) elegantly served))
(defpasta LINGUINI (LAMBCHOPS (including NOODLES)
                                  gotten usually in Northern Italy))
(defpasta PASTA (PASTA and SAUCE (that's ALL!)))
(defpasta ALL! (a lucious lunch))
(defpasta SAUCE (shad and unusual COFFEE (excellente!)))
(defpasta SHAD (SPAGHETTI_CO heated al dente))
(defpasta SPAGHETTI (standard PASTA_CO always good_CO hot particularly
                              (twist_CO then ingest)))
(defpasta COFFEE (choice of fine flavors, particularly ESPRESSO))
(defpasta ESPRESSO (excellent_CO strong_CO powerful_CO rich ESPRESSO_CO
                                 suppressing sleep outrageously))
(defpasta BASTA! (belly all stuffed (tummy ache!)))
(defpasta LAMBCHOPS (LASAGNE and meatballs_CO casually heaped onto PASTA SAUCE))
(defpasta LASAGNE (LINGUINI and SAUCE and GARLIC (NOODLES everywhere!)))
(defpasta RHUBARB (RAVIOLI_CO heated under butter and RHUBARB (BASTA!)))
(defpasta RAVIOLI (RIGATONI and vongole in oil_CO lavishly introduced))
(defpasta RIGATONI (rich Italian GNOCCHI and TOMATOES (or NOODLES instead)))
(defpasta GNOCCHI (GARLIC NOODLES over crisp CHEESE_CO heated immediately))
(defpasta GARLIC (green and red LASAGNE in CHEESE))


(defn acronym? [x] (and (symbol? x) (= (name x) (.toUpperCase (name x)))))


;; FIXME: This isn't really how he does it, get it closer or explain why...
(defn strip-comma-and-eval [sym]
  (let [base-symbol-name (-> sym
                             name
                             (clojure.string/replace #"_CO$" "")
                             symbol)
        pasta-var (-> *ns*
                      ns-map
                      (get (symbol base-symbol-name))
                      var-get)]
    (pasta-var)))


(strip-comma-and-eval 'SAUCE) ;;=> (shad and unusual COFFEE (excellente!))
(strip-comma-and-eval 'SAUCE_CO) ;;=> (shad and unusual COFFEE (excellente!))


(defn lower [x] (* x x))


(defn expand [phrase probability]
  (cond
    (symbol? phrase) phrase
    (empty? phrase) phrase

    (acronym? (first phrase))
    (if (< (rand) probability)
      (concat
       (expand (strip-comma-and-eval (first phrase)) (lower probability))
       (expand (rest phrase) probability))
      (cons (first phrase) (expand (rest phrase) probability)))

    :else (cons (expand (first phrase) (lower probability))
                (expand (rest phrase) (lower probability)))))


(defn lower-case-symbol [x]
  (if-not (symbol? x)
    x
    (-> x name .toLowerCase symbol)))


(defn get-commas-back [x]
  (if-not (symbol? x)
    x
    (-> x
        name
        (clojure.string/replace #"_CO$" ",")
        symbol)))


(defn normalize
  "
  When we're done expanding our lists of food (and our bellies), we
  prepare our data for output to the user: turn `_CO` suffixes into
  actual commas (using the fact that `(symbol \"x,\")` yields a valid
  symbol, if one that would be hard to read back at the REPL); and,
  lower-case all symbols to adhere to the style of the output shown in
  *Metamagical Themas.*

  This bit isn't how Hofstadter's code works, but we need it due to
  differences between Common Lisp and Clojure.
  "
  [expr]
  (->> expr
       (clojure.walk/postwalk get-commas-back)
       (clojure.walk/postwalk lower-case-symbol)))



(defn dinner [expr] (normalize (expand expr 0.99999)))


;; "unit tests"
(let [nsm (ns-map *ns*)]
  (doseq [[k v] nsm :when (:pasta (meta v))]
    (dotimes [_ 10]
      (dinner (list k)))))


(dinner '(GNOCCHI))
;;=>
'(green and red lasagne and meatballs, casually heaped onto pasta
sauce (including noodles) gotten usually in northern italy and shad
and unusual coffee (excellente!) and garlic (noodles everywhere!) and
meatballs, casually heaped onto pasta sauce (including noodles (oodles
of delicious linguini) elegantly served) gotten usually in northern
italy and shad and unusual coffee (excellente!) and green and red
lasagne in cheese (noodles (oodles of delicious linguini) elegantly
served (oodles of delicious linguini) elegantly served everywhere!)
and meatballs, casually heaped onto pasta shad and unusual coffee
(excellente!) (including noodles (oodles of delicious linguini)
elegantly served (oodles of delicious linguini) elegantly served
(oodles of delicious linguini) elegantly served (oodles of delicious
linguini) elegantly served (oodles of delicious linguini) elegantly
served) gotten usually in northern italy and sauce and green and red
linguini and sauce and garlic (noodles everywhere!) in cheese (noodles
(oodles of delicious linguini) elegantly served (oodles of delicious
linguini) elegantly served (oodles of delicious linguini) elegantly
served everywhere!) and meatballs, casually heaped onto pasta and
sauce (that's all!) and sauce (that's all!) and shad and ...)

