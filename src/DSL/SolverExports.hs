module DSL.SolverExports (
    -- Types
    CLD,
    Node,
    Sign (..),

    -- Utility functions
    getNodeContexts
) where
import Data.Graph.Inductive.Graph
import DSL.ConstraintDSL
import DSL.GraphDSL

-- | Get the observed sign for each node
getNodeContexts :: CLD -> [(Node, Maybe Sign, [(Node, Sign)], [(Node, Sign)])]
getNodeContexts cld = map (\n -> (n, getObservedSign (constraints cld) n, outs n, ins n)) $ nodes (graph cld)
    where
        g      = graph cld
        outs n = map (\(a, b, c) -> (b, c)) $ out g n
        ins  n = map (\(a, b, c) -> (a, c)) $ inn g n

-- Get the observed codomain
getObservedSign :: [Constraint] -> Node -> Maybe Sign
getObservedSign xs x = safeHead [c | Equality v (S c) <- xs, v == x]

-- Safe head of a list
safeHead :: [a] -> Maybe a
safeHead []     = Nothing
safeHead (x:xs) = Just x
