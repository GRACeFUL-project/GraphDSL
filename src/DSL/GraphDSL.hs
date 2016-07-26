{-# LANGUAGE FlexibleContexts,
             MultiParamTypeClasses
 #-}
module DSL.GraphDSL (
    -- Types
    Equality (..),
    Implication (..),
    InEquality (..),
    Constraint,
    Requirement (..),
    Graph,
    CLD (..),
    Sign (..),

    -- Graph syntax
    GraphSyntax,
    mkNode,
    (>+>),
    (>->),
    (>++>),
    (>-->),
    (>?>),
    link,
    constrain,

    -- Compilation
    compile,
    compileGraph,
    compileConstraints,

    -- Printing and visualization
    prettify,
    prettyPrint,
    preview
) where

-- | Imports
import Control.Monad.State
import DSL.ConstraintDSL
import Data.Graph.Inductive as G hiding (mkNode, Graph)
import Data.GraphViz hiding (Graph)
import Data.Text.Lazy hiding (head)

-- | Gives an identifier for a node
type Name = Node

-- | Gives the sign of influence {+, ++, +?, -, --, -?, 0, ?}
data Sign = P | PP | PQ | M | MM | MQ | Z | Q deriving (Ord, Eq)

-- | Convert a sign to a string
instance Show Sign where
    show P  = "+"
    show PP = "++"
    show PQ = "+?"
    show M  = "-"
    show MM = "--"
    show MQ = "-?"
    show Z  = "0"
    show Q  = "?"

-- | Label a sign
instance Labellable Sign where
    toLabelValue = textLabelValue . pack . show

-- | A type for requirements
data Requirement = S Sign deriving (Show)

-- | We need to be able to lift Signs into Requirements
instance Lifts Sign Requirement where
    lift = S

-- | A type for constraints
type Constraint = ConstraintType Name Requirement

-- | Graphs
type Graph = Gr String Sign

-- | A CLD is the graph, the signs associated with the edges and the constraints (and the names of the nodes)
data CLD = CLD {graph :: Gr String Sign, constraints :: [Constraint]} deriving (Show)

-- | The monadic syntax of graphs
type GraphSyntax a = State CLD a

-- fixity
infixl >+>
infixl >++>
infixl >->
infixl >-->
infixl >?>

-- | Create a new node
mkNode :: String -> GraphSyntax Name
mkNode s = do
             cld <- get
             let gr = graph cld
             let i  = head $ newNodes 1 gr
             put cld { graph = insNode (i, s) gr }
             return i

-- | Create a new edge
(>+>), (>->), (>++>), (>-->), (>?>) :: GraphSyntax Name -> Name -> GraphSyntax Name
(>+>)  = makeEdge P
(>++>) = makeEdge PP
(>->)  = makeEdge M
(>-->) = makeEdge MM
(>?>)  = makeEdge Q

-- | Factor out the commonality in >x>
makeEdge :: Sign -> GraphSyntax Name -> Name -> GraphSyntax Name
makeEdge s g w = do
                    v <- g
                    cld <- get
                    put $ cld { graph = insEdge (v, w, s) (graph cld) }
                    return w

-- | Add a constraint
constrain :: (IsConstraint Name Requirement c) => c -> GraphSyntax ()
constrain c = do
                cld <- get
                put $ cld { constraints = toConstraint c : constraints cld }

-- | Syntactic sugar
link :: a -> GraphSyntax a
link = return

-- | The initial state
initialState :: CLD
initialState = CLD G.empty []

-- | Compile the graph
compile :: GraphSyntax a -> CLD
compile gs = execState gs initialState

-- | Extract a graph from a syntax
compileGraph :: GraphSyntax a -> Graph
compileGraph = graph . compile

-- | Extract the constraints from a syntax
compileConstraints :: GraphSyntax a -> [Constraint]
compileConstraints = constraints . compile
