(defn- maybe-group [& grp ]
  (do
    (if (= (length grp) 1)
      (in grp 0)
      grp)))

(defn- build-command [name args]
  (let [cmd @{:cmd name}]
    (when args
      (put cmd :args args))
    # I prefer structs ;)
    (table/to-struct cmd)))
 
(def latex-gram (peg/compile
  ~{:commandName (capture (sequence
                   # Initial must be ASCII letter or \ (for \\)
                   (choice :a "\\")
                   # After that we have can include digits and a number of other choices
                   (any (choice :d :a (set "_")))) :commandName)
    # A single argument to a latex command
    :commandArg (sequence "{" :latex "}")
    :command (replace (sequence
                "\\"
                :commandName
                (group (any :commandArg))) ,build-command)
    # Forbidden latex characters in "literals":
    # backslash, quote, open/closing bracket or paren (those have seperate meanings)
    #
    # NOTE: We permit newlines even though those are only valid in certian contexts
    :literalText (capture (some (if-not (set "\\{}()\"\"") 1)) :literal)
    # parantheeses can be used for grouping, but must balance
    :parenGroup (group (sequence (capture "(") :latex (capture ")")))
    :latex (replace (any (choice :command :literalText :parenGroup)) ,maybe-group)
    # Main is alias for latex
    #
    # Unfortunately this all results in an extra level of nesting...
    :main :latex
    }))

(defn latex [text] (peg/match latex-gram text))

(defn- group [code &opt needs-paren]
  # TODO: Expand the power of the "is simple" check
  (def is-simple
    (peg/match
      '{:magicChar (sequence "\\[" :w+ "\\]")
       :dec (sequence :d* "." :d+)
       # :parenGroup (sequence "(" (any (choice :main :s+)) ")")
       :main (sequence (some (choice :w+ :dec)) -1)}
      code))
  (cond
    # Escape using "precedence form"
    (not is-simple) (string/format "PrecedenceForm[%s,5]" code)
    (true? needs-paren) (string/format "(%s)" code)
    code))

(def literal-table 
  {:ne "\\[NotEquals\\]"
   :infty "\\[Infinity\\]"
   :pm "\\[PlusMinus\\]"
   :cdot "."
   :times "\\[Times\\]"
   # This is non-standard
   :cross "\\[Cross\\]"
   })

(def DEBUG false)

(defn- dbg [x &opt lbl]
  (if (not DEBUG) x (do
    (def val (string/format (dyn *pretty-format* "%q") x))
    (if (nil? lbl)
      (print val)
      (print (string/format "%s: %s" lbl val)))
    x)))

(defn- lower-func [name & args]
  (string/format "%s[%s]" name (string/join args ",")))

(defn- simple-func [name arity]
  (assert (int? arity))
  (fn [& args]
    (when (not= (length args) arity)
      (errorf "Expected %d args for %s: %q" arity name args))
    (lower-func name ;args)))

(def command-table
  (do
    (def tbl @{
      :frac (fn [a b] (string/join [(group a) "/" (group b)]))
      :sqrt (simple-func "Sqrt" 1)
      :vec (simple-func "OverVector" 1)
      :hat (simple-func "OverHat" 1)})
    (loop [[name text] :pairs literal-table]
      (put tbl name (fn [] text)))
    tbl))


(defn translate [latex]
  (defn translate-seq [latex]
    (cond
      (empty? latex) latex
      (= (first latex) "(") (do
        (assert (= (last latex) ")"))
        (group (translate (slice latex 1 -2)) true))
      (string/join (map translate latex) " ")))
  (case (dbg (type latex) "type(translate)")
    :string latex
    :number (string latex)
    :array (translate-seq latex)
    :tuple (translate-seq latex)
    :struct
      (let [{:cmd cmdName :args cmdArgs} latex]
       # Ensure non-nil
       (def cmdArgs (if (nil? cmdArgs) '() cmdArgs))
       # Dispatch to command-table
       (def cmd (get command-table (keyword cmdName)))
       (assert (not (nil? cmd)) (error (string/format "Unknown command: \\%s" cmdName)))
       (dbg (cmd ;(map translate cmdArgs)) cmdName))
    (errorf "Unexpected type: %j" latex)))

(let [parsed (latex (string/trimr (file/read stdin :all)))]
  (dbg "parsed latex")
  (print (translate parsed)))
