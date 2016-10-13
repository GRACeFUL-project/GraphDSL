{-# LANGUAGE ExistentialQuantification,
             MultiParamTypeClasses,
             FlexibleInstances,
             FlexibleContexts
 #-}
module DSL.ConstraintDSL where

-- The type of constraints we are modelling
data ConstraintType signs variables codomain = Equality    variables codomain
                                             | Implication (ConstraintType signs variables codomain)
                                                           (ConstraintType signs variables codomain)
                                             | Negation    (ConstraintType signs variables codomain)
                                             | Goal        variables [(signs, Int)]
                                             | Action      variables [(signs, Int)]
                                             deriving (Show)

-- The constraint of equality, to make syntax nice
data Equality name codomain = name := codomain deriving (Show)

-- The constraint of implication
data Implication constraint constraint' = constraint :=> constraint' deriving (Show)

-- The constraint of inequality
data InEquality name codomain = name :!= codomain deriving (Show)

-- Lifting an individual constraint type to the ConstraintType level
class IsConstraint signs name liftedCodomain c where
    toConstraint :: c -> ConstraintType signs name liftedCodomain 

-- Lifting an individual type
class Lifts a b where
    lift :: a -> b

-- If a type is a constraint, then the type lifts
instance (IsConstraint signs name codomain c) => Lifts c (ConstraintType signs name codomain) where
    lift = toConstraint

-- Of course we can lift a type to itself
instance Lifts a a where
    lift = id

-- Any type lifts to ()
instance Lifts a () where
    lift = const ()

-- Lifting equality to ConstraintType
instance (Lifts c c1, Lifts n n1) => IsConstraint s n1 c1 (Equality n c) where
    toConstraint (n := c) = Equality (lift n) (lift c)

-- Lifting implication to constraints
instance (Lifts c (ConstraintType signs name codomain),
          Lifts c' (ConstraintType signs name codomain)) => IsConstraint signs name codomain (Implication c c') where
    toConstraint (c :=> c') = Implication (lift c) (lift c')

-- Lifting inequalities to constraints
instance (Lifts c c1, Lifts n n1) => IsConstraint signs n1 c1 (InEquality n c) where
    toConstraint (n :!= c) = Negation $ toConstraint (n := c)

instance IsConstraint s n c (ConstraintType s n c) where
    toConstraint = id
