# TODO

## Tasks

- [x] example EditRule for Slc
- [ ] basic interface for interactively performing edits
  - make sure to keep frontend and backend _entirely_ separate!! the backend should only need specify basic syntax rendering
- [ ] think about how to do _real-time updating_ mouse controls efficiently

## Laws

- do not rely on typeclasses for cumulative functionality like adding new forms to `dr` and `sr`
  - I've already gone down this rabbit hole, and it's just too difficult and makes it hard to fix later -- I wasted so much time. normal functions are better and easier to optimize anyway
