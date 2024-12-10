# Polymorphic Function Application

## Boolro

(This has been a sort of problem case for previous version of Pantograph.)

Suppose you have a polymorphic function and use it (note that explicit polymorphism is in the syntax; its required to be "known" by the syntax anyway due to intrinsic typing, so might as well include in surface syntax in some way, right?)

```
singleton : forall α. α → List α ↦ ...
_ : List Bool = singleton @Bool True
```

Then perform the edit that wraps `True` with a `⟨λ _ → ⟨⟩⟩`

```
1.     singleton @Bool                                                 {{ λ _ → True ↑ -⟨β → ⟨Bool⟩⟩ }} : List Bool
2.  {{ singleton @Bool        ↓ +⟨β → ⟨Bool⟩⟩ → List +⟨β → ⟨Bool⟩⟩ }}    (λ _ → True)                   : List (β → Bool)
3.  singleton    @(β → Bool)                                             (λ _ → True)                   : List (β → Bool)
```

```
2.  {{ singleton @Bool        ↓ +⟨β → ⟨Bool⟩⟩ → List +⟨β → ⟨Bool⟩⟩ }}     (λ _ → True)                   : List (β → Bool)
2.1 {{ singleton ↓ α → List α }} @{{ Bool ↓ +⟨β → ⟨Bool⟩⟩ }} (λ _ → True)
3.  singleton    @(β → Bool)                                              (λ _ → True)                   : List (β → Bool)
```

How do you go from step 2 to step 3?
Somehow, has to
- see `singleton @Bool` and know to extract the polymorphic type of `singleton` which is `forall α. α → List α`
- match `α → List α` with `+⟨β → ⟨Bool⟩⟩ → List +⟨β → ⟨Bool⟩⟩` in order to find the substitution of `α ↦ +⟨β → ⟨Bool⟩⟩`
  - such that applying that substitution to the polymorphic type of `singleton` yields a type change that can be applied to the type of `singleton @Bool` to yield a valid type
  - then apply the typechange for each of the typevars to each of the explicit type arguments of `singleton @Bool`, in this case applying `+⟨β → ⟨Bool⟩⟩` to the `Bool` in `@Bool`.

## Solution 1: All-At-Once Monomorphization

Same setup as in intro:

```
singleton : forall α. α → List α = ...
_ : List Bool = singleton {α ↦ Bool} True
```

Then perform this edit:
```
_ : List Bool = singleton {α ↦ Bool} {{ λ _ → True ↑ -⟨β → ⟨Bool⟩⟩ }}
```

```
singleton {α ↦ Bool} : Bool → List Bool
```

Then steps:

```
1. _ : List Bool = singleton {α ↦ Bool} {{ λ _ → True ↑ -⟨β → ⟨Bool⟩⟩ }}
2. _ : List Bool = {{ singleton {α ↦ Bool} ↓ +⟨β → ⟨Bool⟩⟩ → List Bool }} (λ _ → True)
3. _ : List (β → Bool) = {{ singleton {α ↦ β → Bool} ↑ (β → Bool) → List +⟨β → ⟨Bool⟩⟩ }} (λ _ → True)
```

Figures out in step 2->3 that can substitute `α` appropriately, and then propagates up that change.
Importantly, output type `List Bool` doesnt get updated until 3, rather than in 2 like suggested in intro.

## Solution 2: Monomorphization At Each Partial Application

```
f : forall α. α → α → α → α = ...
_ : Bool = (α, {α → Bool}, f {α ↦ Bool} ({α ↦ Bool}, True) ({α ↦ Bool}, False) ({α ↦ Bool}, True))
```

Edit:

```
_ : Bool = (α, {α → Bool}, f {α ↦ Bool} (α, {α ↦ Bool}, True) (α, {α ↦ Bool}, {{ λ _ → False ↑ -⟨β → ⟨Bool⟩⟩ }}) (α, {α ↦ Bool}, True))
```

Steps:

```
_ :     Bool =    (α, {α ↦ Bool    },       f {α ↦     Bool}                          (α, {α ↦ Bool    },       True)                             (α, {α ↦     Bool}, {{ λ _ → False ↑ -⟨β → ⟨Bool⟩⟩ }})                          (α, {α ↦     Bool},       True)                         )
_ :     Bool =    (α, {α ↦ Bool    },       f {α ↦     Bool}                          (α, {α ↦ Bool    },       True)                          {{ (α, {α ↦ β → Bool},    λ _ → False                   ) ↑ {α ↦ -⟨β → ⟨Bool⟩⟩} }} (α, {α ↦     Bool},       True)                         )
_ :     Bool =    (α, {α ↦ Bool    }, {{ {{ f {α ↦     Bool}                          (α, {α ↦ Bool    },       True) ↓ {α ↦ +⟨β → ⟨Bool⟩⟩} }}    (α, {α ↦ β → Bool},    λ _ → False                   ) ↑ {α ↦ -⟨β → ⟨Bool⟩⟩} }} (α, {α ↦     Bool},       True)                         )
_ :     Bool =    (α, {α ↦ Bool    }, {{ {{ f {α ↦     Bool} ↓ {α ↦ +⟨β → ⟨Bool⟩⟩} }} (α, {α ↦ β → Bool}, λ _ → True)                             (α, {α ↦ β → Bool},    λ _ → False                   ) ↑ {α ↦ -⟨β → ⟨Bool⟩⟩} }} (α, {α ↦     Bool},       True)                         )
_ :     Bool =    (α, {α ↦ Bool    }, {{    f {α ↦ β → Bool}                          (α, {α ↦ β → Bool}, λ _ → True)                             (α, {α ↦ β → Bool},    λ _ → False                   ) ↑ {α ↦ -⟨β → ⟨Bool⟩⟩} }} (α, {α ↦     Bool},       True)                         )
_ :     Bool =    (α, {α ↦ Bool    }, {{    f {α ↦ β → Bool}                          (α, {α ↦ β → Bool}, λ _ → True)                             (α, {α ↦ β → Bool},    λ _ → False                   )                          (α, {α ↦ β → Bool}, λ _ → True) ↑ {α ↦ -⟨β → ⟨Bool⟩⟩} }})
_ :     Bool = {{ (α, {α ↦ β → Bool},       f {α ↦ β → Bool}                          (α, {α ↦ β → Bool}, λ _ → True)                             (α, {α ↦ β → Bool},    λ _ → False                   )                          (α, {α ↦ β → Bool}, λ _ → True)                         ) ↑ -⟨β → ⟨Bool⟩⟩ }}
_ : β → Bool =    (α, {α ↦ β → Bool},       f {α ↦ β → Bool}                          (α, {α ↦ β → Bool}, λ _ → True)                             (α, {α ↦ β → Bool},    λ _ → False                   )                          (α, {α ↦ β → Bool}, λ _ → True)                         ) ↑ -⟨β → ⟨Bool⟩⟩
```
