# generalizations

the main things that should be generalized in order to make more advanced type systems work well are:

- derivation of small-step rules: currently, _many_ special rules are needed even in just the monomorphic case
  - things that _should_ be derivable:
    - up/down rules for tuples (and other non-base types)
    - up/down rules for function type
    - substitution of let-bound polymorphic variables
