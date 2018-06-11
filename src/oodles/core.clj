(ns ^{:author "John Jacobsen, adapted from Douglas R. Hofstadter"
      :doc " In 1983, Douglas R. Hofstadter, author of \"Gödel,
Escher, Bach: An Eternal Golden Braid\" wrote a series of articles on
Lisp in Scientific American (later republished in the book
*Metamagical Themas*).  The third and last in the series was an
entertaining example in which lists of words (\"atoms,\" akin to
Clojure's *symbols*) are transformed recursively; the words in
question act as both function names and regular English words.

This technique, in which the line between code and data is blurred, is
somewhat more common in older, traditional Lisp programs than in
Clojure (where use of symbols for things other than vars is somewhat
frowned upon). Such programs can be powerful and expressive, but
harder to understand.  Examples of Lisp programs written in this way
can be found in Norvig's *Paradigms of Artificial Intelligence
Programming - Case Studies in Common Lisp*.
"}
  oodles.core)


;; In this program, a set of acronyms are presented; each acronym can be
;; "expanded" into a phrase which itself may contain other acronyms.
;; The program generates random expansions of these acronyms, "bottoming
;; out" (hitting the base case) at random, though with increasing
;; probability of termination for more deeply-nested expressions. The
;; example on the right includes three successive example expansions,
;; of `TOMATOES`, then `MACARONI`, then `CHEESE`.
(comment
  TOMATOES

  (TOMATOES on MACARONI (and TOMATOES only) exquisitely SPICED)

  (TOMATOES on MACARONI and CHEESE (a REPAST of Naples, Italy
   (and TOMATOES only) exquisitely SPICED))

  (TOMATOES on MACARONI and cheddar, havarti, emmentaler (especially
  SHARP emmenthaler) (a REPAST of Naples, Italy (and TOMATOES only)
  exquisitely SPICED))

)


;; The actual implementation follows.

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
;; (Example: `SAUCE,` becomes `SAUCE_CO`.)
;;
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
(defpasta SAUCE (SHAD and unusual COFFEE (excellente!)))
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


(defn acronym?
  "
  Hofstadter leaves the implementation of `acronym?` undefined,
  since it is so implementation-specific.  In our case, something is
  an acronym if it's a symbol and its name is equal to an
  upper case version of ifself.
  "
  [x]
  (and (symbol? x) (= (name x) (.toUpperCase (name x)))))


(defn strip-comma-and-eval
  "
  Remove commas from a symbol and evaluate it as a function. This is
  also very implementation-specific; in our case, it relies on
  Clojure's machinery for converting symbols to strings and
  vice-versa, and on the subtle differences between symbols, vars, and
  functions.
  "
  [sym]
  (let [base-symbol-name (-> sym
                             name
                             (clojure.string/replace #"_CO$" "")
                             symbol)]
    ((-> *ns*
          ns-map
          (get (symbol base-symbol-name))
          var-get))))


(defn lower
  "
  Reduce the probability by some amount to encourage our recursion to
  bottom out (and thereby avoid stack overflows).  Hofstadter
  multiplies probabilities by 0.8, but I simply square the probability
  so that higher probabilities decrease more slowly than lower ones as
  the recursion progresses.
  "
  [x]
  (* x x))


(defn expand
  "
  A significant portion of Hofstadter's article relates to the
  recursive `expand` function, so I won't go into details here.  This
  version is very similar to his; it adds a bottoming-out clause to
  accommodate the base case of an empty phrase (since `()` and `nil`
  are not the same in Clojure, as they are in Common Lisp).  Also we
  use `strip-comma-and-eval` instead of plain `eval` to handle the
  comma-ed symbols.
  "
  [phrase probability]
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
  This function accomodates the differences between Common Lisp and
  Clojure, to make the output similar to what Hofstadter shows in the
  book.

  When we're done expanding our lists of food (and our bellies), we
  prepare our data for output to the user: turn `_CO` suffixes into
  actual commas (using the fact that `(symbol \"x,\")` yields a valid
  symbol, if one that would be hard to read back at the REPL); and,
  lower-case all symbols to adhere to the style of the output shown in
  *Metamagical Themas.*

  "
  [expr]
  (->> expr
       (clojure.walk/postwalk get-commas-back)
       (clojure.walk/postwalk lower-case-symbol)))



(defn dinner
  "
  The actual function we'll use to generate our expansions.  Setting a
  large probability will ensure longer examples.
  "
  [expr]
  (normalize (expand expr 0.9999)))


;; "Unit tests": make sure it doesn't bomb out on any of our acronyms.
;; Find all the acronyms based on the metadata added by the `defpasta`
;; macro, ensuring each runs to completion.
(let [nsm (ns-map *ns*)]
  (doseq [[k v] nsm :when (:pasta (meta v))]
    (dotimes [_ 10]
      (dinner (list k)))))

;; If, like me, you use Emacs and CIDER and want long REPL examples,
;; you have to set `*print-length*` to something bigger than its
;; default.
(set! *print-length* 1000)

;; Finally, our example:
(dinner '(LINGUINI))
;;=>
'(lasagne and meatballs, casually heaped onto pasta sauce (including
noodles) gotten usually in northern italy and standard pasta, always
good, hot particularly (twist, then ingest) heated al dente and
unusual coffee (excellente!) and green and red lasagne in cheese
(noodles (oodles of delicious linguini) elegantly served (oodles of
delicious linguini) elegantly served everywhere!) and meatballs,
casually heaped onto pasta shad and unusual coffee (excellente!)
(including noodles (oodles of delicious linguini) elegantly served
(oodles of delicious linguini) elegantly served (oodles of delicious
linguini) elegantly served (oodles of delicious linguini) elegantly
served (oodles of delicious lasagne and meatballs, casually heaped
onto pasta sauce (including noodles) gotten usually in northern italy)
elegantly served) gotten usually in northern italy and standard pasta
and shad and unusual coffee (excellente!) (that's all!) and shad and
unusual coffee (excellente!) (that's all!) always good, hot
particularly (twist, then ingest) heated al dente and unusual choice
of fine flavors particularly espresso (excellente!) and green and red
lasagne in cheese (noodles (oodles of delicious linguini) elegantly
served (oodles of delicious linguini) elegantly served (oodles of
delicious linguini) elegantly served (oodles of delicious linguini)
elegantly served everywhere!) and meatballs, casually heaped onto
pasta and sauce (that's all!) and sauce (that's all!) and shad and
unusual coffee (excellente!) (that's all!) sauce (including noodles
(oodles of delicious linguini) elegantly served (oodles of delicious
lambchops (including noodles) gotten usually in northern italy)
elegantly served (oodles of delicious lambchops (including noodles)
gotten usually in northern italy) elegantly served) gotten usually in
northern italy and standard pasta and shad and unusual coffee
(excellente!) (that's all!) and standard pasta, always good, hot
particularly (twist, then ingest) heated al dente and unusual coffee
(excellente!) (that's all!) and spaghetti, heated al dente and unusual
coffee (excellente!) (that's a lucious lunch) and standard pasta,
always good, hot particularly (twist, then ingest) heated al dente and
unusual coffee (excellente!) (that's a lucious lunch) always good, hot
particularly (twist, then ingest) heated al dente and unusual choice
of fine flavors particularly espresso (excellente!) and green and red
linguini and sauce and garlic (noodles everywhere!) and meatballs,
casually heaped onto pasta sauce (including noodles) gotten usually in
northern italy and spaghetti, heated al dente and unusual coffee
(excellente!) and green and red lasagne in cheese (noodles (oodles of
delicious linguini) elegantly served everywhere!) in cheese (noodles
(oodles of delicious linguini) elegantly served (oodles of delicious
linguini) elegantly served (oodles of delicious linguini) elegantly
served (oodles of delicious linguini) elegantly served (oodles of
delicious linguini) elegantly served (oodles of delicious lambchops
(including noodles) gotten usually in northern italy) elegantly served
(oodles of delicious linguini) elegantly served (oodles of delicious
linguini) elegantly served everywhere!) and meatballs, casually heaped
onto pasta and shad and unusual coffee (excellente!) (that's a lucious
lunch) and sauce (that's all!) and spaghetti, heated al dente and
unusual coffee (excellente!) (that's a lucious lunch) and standard
pasta and sauce (that's all!) always good, hot particularly (twist,
then ingest) heated al dente and unusual coffee (excellente!) (that's
a lucious lunch) shad and unusual choice of fine flavors particularly
espresso (excellente!) (including noodles (oodles of delicious
linguini) elegantly served (oodles of delicious linguini) elegantly
served (oodles of delicious linguini) elegantly served (oodles of
delicious linguini) elegantly served (oodles of delicious lasagne and
meatballs, casually heaped onto pasta sauce (including noodles) gotten
usually in northern italy) elegantly served (oodles of delicious
lambchops (including noodles) gotten usually in northern italy)
elegantly served (oodles of delicious linguini and sauce and garlic
(noodles everywhere!) and meatballs, casually heaped onto pasta sauce
(including noodles (oodles of delicious linguini) elegantly served)
gotten usually in northern italy) elegantly served (oodles of
delicious lambchops (including noodles (oodles of delicious linguini)
elegantly served) gotten usually in northern italy) elegantly served
(oodles of delicious lasagne and meatballs, casually heaped onto pasta
sauce (including noodles) gotten usually in northern italy and shad
and unusual coffee (excellente!) and garlic (noodles everywhere!) and
meatballs, casually heaped onto pasta sauce (including noodles (oodles
of delicious linguini) elegantly served (oodles of delicious linguini)
elegantly served) gotten usually in northern italy) elegantly served
(oodles of delicious linguini and sauce and garlic (noodles
everywhere!) and meatballs, casually heaped onto pasta sauce
(including noodles) gotten usually in northern italy and spaghetti,
heated al dente and unusual coffee (excellente!) and garlic (noodles
(oodles of delicious linguini) elegantly served everywhere!) and
meatballs, casually heaped onto pasta sauce (including noodles (oodles
of delicious linguini) elegantly served (oodles of delicious linguini)
elegantly served) gotten usually in northern italy) elegantly served)
gotten usually in northern italy)

;; Man, I'm stuffed!
