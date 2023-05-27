val simplify_cnf : Cnf.cnf -> Cnf.cnf
(** [simplify_cnf c] removes all duplicates within each disjunction, and then removes all
    duplicate disjunctions in the conjunction*)
