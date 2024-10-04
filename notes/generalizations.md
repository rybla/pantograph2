# generalizations

the main things that should be generalized in order to make more advanced type systems work well are:

- derivation of small-step rules: currently, _many_ special rules are needed even in just the monomorphic case
  - things that _should_ be derivable:
    - up/down rules for tuples (and other non-base types)
    - up/down rules for function type
    - substitution of let-bound polymorphic variables

## each DerivRule's kids have Changes to define their sorts in relation to the parent sort, rather than labeling those kid sorts directly

perhaps each kid in a DerivRule is labeled not just by _one_ Change, but by 2
Changes: one that will go down and one that will go up when a change is being
propagated from kid to parent or visa-versa.