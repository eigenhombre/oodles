(ns ^{:author "John Jacobsen, adapted from Douglas R. Hofstadter"
      :doc "
In 1983, Douglas R. Hofstadter, author of \"Gödel, Escher, Bach: An
Eternal Golden Braid\" wrote a series of articles on Lisp in
Scientific American (later republished as \"Metamagical Themas\"). The
third and last of these was an entertaining example which is an
instructive example on blurring the line between code and data, which
is typical for Lisp (though somewhat less common in Clojure, except
for the occasional macro).
"}
  oodles.core)


(defmacro defpasta
  "
  `defpasta` makes a function that return the supplied list, avoiding
  the need for the argument vector or the quote.  Note that just one
  small macro provides enough \"syntactic sugar\" to make the examples
  much more readable.
  "
  [name_ & expr]
  `(defn ~name_ []
     '~@expr))


;; These are the definitions Hofstadter uses, with commas treated
;; differently.  The commas are important in his output, but commas
;; are whitespace in Clojure... so where the symbols are used with
;; commas in compound statements, we append `_CO` to tell the program
;; to supply a comma in the output.
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
(defpasta RHUBARB (RAVIOLI_CO heated under butter and RHUBARB (BASTA!)))
(defpasta RAVIOLI (RIGATONI and vongole in oil_CO lavishly introduced))
(defpasta RIGATONI (rich Italian GNOCCHI and TOMATOES (or NOODLES instead)))
(defpasta GNOCCHI (GARLIC NOODLES over crisp CHEESE, heated immediately))
(defpasta GARLIC (green and red LASAGNE in CHEESE))


(defn expandable? [x] (and (symbol? x) (= (name x) (.toUpperCase (name x)))))


(defn get-commas-back [x]
  (if-not (symbol? x)
    x
    (-> x
        name
        (clojure.string/replace #"_CO$" ",")
        symbol)))


(defn strip-comma-and-eval [sym]
  (let [base-symbol-name (-> sym
                             name
                             (clojure.string/replace #"_CO$" "")
                             symbol)
        pasta-var (some-> *ns*
                          ns-map
                          (get (symbol base-symbol-name))
                          var-get)]
    (if pasta-var
      (pasta-var)
      sym)))

(strip-comma-and-eval 'SAUCI) ;;=> SAUCI
(strip-comma-and-eval 'SAUCE) ;;=> (shad and unusual COFFEE (excellente!))
(strip-comma-and-eval 'SAUCE_CO) ;;=> (shad and unusual COFFEE (excellente!))


(defn lower [x] (* x x))


(defn expand [phrase probability]
  (cond
    (symbol? phrase) phrase
    (empty? phrase) phrase

    (expandable? (first phrase))
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


(defn normalize [expr]
  (->> expr
       (clojure.walk/postwalk get-commas-back)
       (clojure.walk/postwalk lower-case-symbol)))




(normalize (expand '() 0.8))
(normalize (expand 'MACARONI 0.8))
(normalize (expand '(PASTA) 0.9999))
'(pasta and sauce (that's all!) and sauce (that's all!) and shad and unusual coffee (excellente!) (that's a lucious lunch) and shad and unusual coffee (excellente!) (that's a lucious lunch) and sauce (that's a lucious lunch) and shad and unusual choice of fine flavors particularly espresso (excellente!) (that's a lucious lunch) and shad and unusual choice of fine flavors particularly espresso (excellente!) (that's a lucious lunch) and shad and unusual choice of fine flavors particularly espresso (excellente!) (that's a lucious lunch) and shad and unusual choice of fine flavors particularly espresso (excellente!) (that's a lucious lunch) and shad and unusual choice of fine flavors particularly espresso (excellente!) (that's a lucious lunch) and shad and unusual choice of fine flavors particularly espresso (excellente!) (that's a lucious lunch) and shad and unusual choice of fine flavors particularly excellent, strong, powerful, rich espresso, suppressing sleep outrageously (excellente!) (that's a lucious lunch))



