{-# LANGUAGE FlexibleContexts,
             MultiParamTypeClasses,
             FlexibleInstances
 #-}
module DSL.GraphDSL (
    -- Types
    Equality (..),
    Implication (..),
    InEquality (..),
    Constraint,
    Requirement (..),
    Graph,
    CLD,
    graph,
    constraints,
    Sign (..),

    -- Graph syntax
    GraphSyntax,
    mkNode,
    (>+>),
    (>->),
    (>++>),
    (>-->),
    (>?>),
    (>~+>),
    (>~->),
    (>~++>),
    (>~-->),
    (>~?>),
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

-- | Label a node
instance Labellable (String, Maybe Node) where
    toLabelValue (s, _) = textLabelValue . pack $ s

-- | A type for requirements
data Requirement = S Sign deriving (Show)

-- | We need to be able to lift Signs into Requirements
instance Lifts Sign Requirement where
    lift = S

-- | A type for constraints
type Constraint = ConstraintType Name Requirement

-- | A Time interval
data TimeFrame = Im | Future deriving (Ord, Eq, Show)

-- | Graphs
type Graph = Gr (String, Maybe Node) Sign

-- | A CLD is the graph, the signs associated with the edges and the constraints (and the names of the nodes)
data CLDG n e = CLDG {graph :: Gr n e, constraints :: [Constraint]} deriving (Show)

-- | A CLD is a specialiced general cld.......
type CLD = CLDG (String, Maybe Node) Sign

-- | The monadic syntax of graphs
type GraphSyntax a = State (CLDG String (Sign, TimeFrame)) a

-- fixity
infixl >+>
infixl >++>
infixl >->
infixl >-->
infixl >?>
infixl >~+>
infixl >~++>
infixl >~->
infixl >~-->
infixl >~?>

-- | Create a new node
mkNode :: String -> GraphSyntax Name
mkNode s = do
             cld <- get
             let gr = graph cld
             let i  = head $ newNodes 1 gr
             put cld { graph = insNode (i, s) gr }
             return i

-- | Create a new edge
(>+>)  = makeEdge P Im
(>++>) = makeEdge PP Im
(>->)  = makeEdge M Im
(>-->) = makeEdge MM Im
(>?>)  = makeEdge Q Im
(>~+>)  = makeEdge P Future 
(>~++>) = makeEdge PP Future 
(>~->)  = makeEdge M Future 
(>~-->) = makeEdge MM Future 
(>~?>)  = makeEdge Q Future 

-- | Factor out the commonality in >x>
makeEdge :: Sign -> TimeFrame -> GraphSyntax Name -> Name -> GraphSyntax Name
makeEdge s t g w = do
                    v <- g
                    cld <- get
                    put $ cld { graph = insEdge (v, w, (s, t)) (graph cld) }
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
initialState :: CLDG String (Sign, TimeFrame)
initialState = CLDG G.empty []

-- | Compile the graph
compile :: GraphSyntax a -> CLD
compile gs = CLDG g constr
    where
        cldg = execState gs initialState
        constr = constraints cldg
        gra = graph cldg
        gra' = nmap (\s -> (s, Nothing)) $ emap fst gra
        minNode = head $ newNodes 1 gra
        guardThing = Prelude.any (\(_, _, (_, t)) -> t /= Im) $ labEdges gra
        newNs = Prelude.map (\(n, a) -> (n+minNode, (a++"'", Just n))) (labNodes gra)
        newEs = Prelude.map (\(sr, si, (s, t)) -> if t == Im then
                                                    (sr+minNode, si+minNode, (s, Im))
                                                  else (sr, si+minNode, (s, Im))
                            ) (labEdges gra)
        weirdGraph = Prelude.foldl (flip insEdge) (Prelude.foldl (flip insNode) (nmap (\s -> (s, Nothing)) gra) newNs) newEs
        g = if guardThing then
                emap fst $ delEdges
                    (Prelude.map (\(a, b, _) -> (a, b))
                        (Prelude.filter (\(_, _, (x, y)) -> y /= Im)
                            (labEdges weirdGraph)
                        )
                    ) weirdGraph
            else
                gra'

-- | Extract a graph from a syntax
compileGraph :: GraphSyntax a -> Graph
compileGraph = graph . compile

-- | Extract the constraints from a syntax
compileConstraints :: GraphSyntax a -> [Constraint]
compileConstraints = constraints . compile
