-- Tautology testing by partial evaluation and case analysis.
-- Colin Runciman, 2003.

module Taut where

data Prop = Lit Bool
          | Var Name
          | Not Prop
          | Prop :=> Prop
  deriving (Eq, Ord, Show) -- TODO: actually write semantic equality

data Name = P | Q | R deriving (Eq, Ord, Show)

infixr :=>

eval :: Prop -> Prop
eval (Lit b)   = Lit b
eval (Var v)   = Var v
eval (Not p)   = case eval p of
                 Lit b -> Lit (not b)
                 p'    -> Not p'
eval (p :=> q) = case (eval p, eval q) of
                 (Lit b, q') -> if b then q' else Lit True
                 (p', Lit b) -> if b then Lit True else Not p'
                 (p',    q') -> p' :=> q'

varOf :: Prop -> Name
varOf (Var v)   = v
varOf (Not p)   = varOf p
varOf (p :=> _) = varOf p

subst :: Name -> Bool -> Prop -> Prop
subst _ _ (Lit b)   = Lit b
subst v b (Var w)   = if v==w then Lit b else Var w
subst v b (Not p)   = Not (subst v b p)
subst v b (p :=> q) = subst v b p :=> subst v b q

taut :: Prop -> Bool
taut p = case eval p of
         Lit b -> b
         p'    -> let v = varOf p' in
                  taut (subst v True  p') &&
                  taut (subst v False p')

main :: Prop -> IO ()
main p = print (taut p)
-- eg. (Var 'p' :=> (Var 'p' :=> Var 'q') :=> Var 'q'))
