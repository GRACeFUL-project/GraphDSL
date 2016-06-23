{-# LANGUAGE FlexibleContexts,
             MultiParamTypeClasses
 #-}
module DSL.Generalized.GraphDSL (
    GraphSyntax,
    -- Graph syntax
    mkNode,
    (>-),
    link,
    
    -- Compilation
    compile,
    
    -- Printing and visualization
    prettify,
    prettyPrint,
    preview
) where

-- | Imports
import Control.Monad.State
import Data.Graph.Inductive as G hiding (mkNode, Graph)
import Data.GraphViz hiding (Graph)
import Data.Text.Lazy hiding (head)

-- | Gives an identifier for a node
type Name = Node 

-- | The monadic syntax of graphs
type GraphSyntax n e a = State (Gr n e) a

-- | Create a new node
mkNode :: n -> GraphSyntax n e Name
mkNode n = do
             gr <- get
             let i  = head $ newNodes 1 gr
             put $ insNode (i, n) gr
             return i

infixl 0 >-

-- | The general edge constructor
(>-) :: GraphSyntax n e Name -> e -> Name -> GraphSyntax n e Name
(>-) g e w = do
               v <- g
               gr <- get
               put $ insEdge (v, w, e) gr
               return w 

-- | Syntactic sugar 
link :: a -> GraphSyntax n e a
link = return

-- | Compile the graph
compile :: GraphSyntax n e a -> Gr n e 
compile gs = snd $ runState gs G.empty
