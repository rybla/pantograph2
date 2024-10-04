-- | Untyped lambda calculus
module Pantograph.Example.Ulc where

-- import Pantograph.Grammar
-- import Pantograph.Tree
-- import Prelude

-- import Control.Plus (empty)
-- import Data.Either (Either(..))
-- import Data.Eq.Generic (genericEq)
-- import Data.Foldable (class Foldable)
-- import Data.Generic.Rep (class Generic)
-- import Data.List (List(..), (:))
-- import Data.List as List
-- import Data.Map as Map
-- import Data.Maybe (Maybe)
-- import Data.Show.Generic (genericShow)
-- import Pantograph.Pretty (class Pretty)
-- import Pantograph.Utility (bug)

-- --------------------------------------------------------------------------------
-- -- types
-- --------------------------------------------------------------------------------

-- data S
--   = String_S
--   | Term_S

-- derive instance Generic S _

-- instance Show S where
--   show x = genericShow x

-- instance Pretty S where
--   pretty String_S = "String"
--   pretty Term_S = "Term"

-- instance Eq S where
--   eq x = genericEq x

-- data D
--   = String_D String
--   | Var_D -- String
--   | Lam_D -- String, Term
--   | App_D -- Term, Term
--   | Hole_D

-- derive instance Generic D _

-- instance Show D where
--   show x = genericShow x

-- instance Pretty D where
--   pretty (String_D str) = "String " <> show str
--   pretty Var_D = "Var"
--   pretty Lam_D = "Lam"
--   pretty App_D = "App"
--   pretty Hole_D = "Hole"

-- instance Eq D where
--   eq x = genericEq x

-- --------------------------------------------------------------------------------
-- -- smart constructors
-- --------------------------------------------------------------------------------

-- mkString_S :: Sort S
-- mkString_S = (String_S ▵* [])

-- mkTerm_S :: Sort S
-- mkTerm_S = (Term_S ▵* [])

-- --------------------------------------------------------------------------------
-- -- semantics
-- --------------------------------------------------------------------------------

-- derivRules :: DerivRules D S
-- derivRules =
--   let
--     ruleString =
--       mkDerivRule "String"
--         []
--         (Right (Term_S) ▵* [ Right (String_S) ▵* [] ])

--     ruleVar =
--       mkDerivRule "Var"
--         [ Replace (pure (String_S) ▵* []) (pure (Term_S) ▵* []) ▵* [] ]
--         (Right (Term_S) ▵* [])

--     ruleLam =
--       mkDerivRule "Lam"
--         [ Replace (pure (String_S) ▵* []) (pure (Term_S) ▵* []) ▵* []
--         , Congruence (pure (Term_S)) ▵* []
--         ]
--         (Right (Term_S) ▵* [])

--     ruleApp =
--       mkDerivRule "App"
--         [ Congruence (pure (Term_S)) ▵* []
--         , Congruence (pure (Term_S)) ▵* []
--         ]
--         (Right (Term_S) ▵* [])

--     ruleHole =
--       mkDerivRule "Hole"
--         []
--         (Right (Term_S) ▵* [])

--   in
--     case _ of
--       String_D _ -> ruleString
--       Var_D -> ruleVar
--       Lam_D -> ruleLam
--       App_D -> ruleApp
--       Hole_D -> ruleHole

-- propagRules :: PropagRules D S
-- propagRules =
--   [ PropagRule "trivial" \_th pl -> case pl of
--       -- simply remove boundary
--       PropagBoundary _ _ ▵ (kid : Nil) -> pure kid
--       _ -> empty
--   ] # List.fromFoldable

-- canonicalDerivOfSort :: Sort S -> Maybe (Deriv D S)
-- canonicalDerivOfSort (String_S ▵ Nil) = pure (DerivLabel (String_D "") Map.empty ▵ Nil)
-- canonicalDerivOfSort (String_S ▵ _) = bug "invalid sort"
-- canonicalDerivOfSort (Term_S ▵ Nil) = pure (DerivLabel Hole_D Map.empty ▵ Nil)
-- canonicalDerivOfSort (Term_S ▵ _) = bug "invalid sort"

-- --------------------------------------------------------------------------------
-- -- utilities
-- --------------------------------------------------------------------------------

-- mkDerivRule
--   :: forall s f
--    . Foldable f
--   => String
--   -> f (RulialSortChange s)
--   -> RulialSort s
--   -> DerivRule s
-- mkDerivRule label args sort = DerivRule label (List.fromFoldable args) sort

