(ns zipper-encoding.core
  "Utilities for working with zippers."
  (:require [clojure.spec.alpha :as s]
            [clojure.zip :as z]))

(defn leaf?
  "Is the `zipper` node at the current location of a leaf?"
  {:inline (fn [zipper] `(not (z/branch? ~zipper)))}
  [zipper]
  (not (z/branch? zipper)))

(defn next-leaf
  "Move to the next location of a leaf node in the hierarchy, depth-first.
  Do nothing if the current location represents the end of a depth-first walk."
  [zipper]
  (when-not (z/end? zipper)
    (let [next-zipper (z/next zipper)]
      (if (z/branch? next-zipper)
        (recur next-zipper)
        next-zipper))))

(defn validate-tree-with
  "Breadth-first validation of a `zipper` tree,
  checking that each node is `valid?` under the predicate."
  ([valid? zipper]
   (let [zippers (into (clojure.lang.PersistentQueue/EMPTY) [zipper])]
     (validate-tree-with valid? zippers [])))
  ([valid? zippers visited]
   (if-let [zipper (peek zippers)]
     (let [node (z/node zipper)]
       (if (valid? node)
         (recur valid?
                (into (pop zippers)
                      (some->> zipper
                               (z/down)
                               (iterate z/right)
                               (take-while identity)))
                (conj visited node))
         {:valid? false
          :node node
          :visited visited}))
     {:valid? true
      :node nil
      :visited visited})))

(defn validate-tree-with-spec
    "Breadth-first validation of a `zipper` tree,
  checking that each node is valid for `spec`."
    [spec zipper]
    (validate-tree-with (fn valid? [node]
                          (s/valid? spec node))
                        zipper))

(defn all-children-valid?
  "Is the `zipper` node at the current location of a branch node,
  where all of the child nodes are deemed `valid?`?"
  [valid? zipper]
  (and (z/branch? zipper)
       (every? valid?
               (z/children zipper))))

(defn encode-tree-with
  "Depth-first encoding of a tree."
  [encoded? encoder zipper]
  (letfn [(f [zipper]
            (let [node (z/node zipper)]
              (if (encoded? node)
                ;; Node already fully encoded.
                node
                ;; If it is a leaf, or all of its children are encoded.
                (if (or (all-children-valid? encoded? zipper)
                        (leaf? zipper))
                  ;; Encode then go up a "level".
                  (let [encoded (z/edit zipper encoder)]
                    (recur (or (z/right encoded)
                               (z/up encoded)
                               encoded)))
                  ;; Keep moving downwards until you find a leaf.
                  (recur (next-leaf zipper))))))]
    (f zipper)))
