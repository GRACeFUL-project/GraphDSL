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
    TimeFrame (..),

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
    ignoreTime,

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
import qualified Data.Text.Lazy as Txt hiding (head)

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
    toLabelValue = textLabelValue . Txt.pack . show

-- | Label a node
instance Labellable (String, Maybe Node) where
    toLabelValue (s, _) = textLabelValue . Txt.pack $ s

-- | A type for requirements
data Requirement = S Sign | ST (Sign, TimeFrame) deriving (Show)

-- | We need to be able to lift Signs into Requirements
instance Lifts Sign Requirement where
    lift = S

-- | We need to be able to lift pairs of Signs in to Requirements
instance Lifts (Sign, TimeFrame) Requirement where
    lift = ST

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
(>+>)   = makeEdge P Im
(>->)   = makeEdge M Im
(>?>)   = makeEdge Q Im
(>++>)  = makeEdge PP Im
(>-->)  = makeEdge MM Im
(>~+>)  = makeEdge P Future 
(>~->)  = makeEdge M Future 
(>~?>)  = makeEdge Q Future 
(>~++>) = makeEdge PP Future 
(>~-->) = makeEdge MM Future 

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
compile gs = CLDG (nfilter (\n -> 0 /= (length (neighbors g n))) g) constrs
    where
        -- General CLD
        cldg = execState gs initialState

        -- Posed constraints
        constr = constraints cldg
        
        -- compute temporal constraints
        listConstrTemporal = [ (n, (s, t)) | (Equality n (ST (s, t))) <- constr]
        constrs = filter removeTemporal constr ++
            [
                if futureEdge then
                    toConstraint $ (n+(if t == Future then minNode else 0)) := s
                else
                    toConstraint $ n := s
                | (n, (s, t)) <- listConstrTemporal
            ]
        removeTemporal (Equality _ (ST _)) = False
        removeTemporal _                   = True
        
        -- The raw CLDG graph
        gra = graph cldg
        -- The raw CLDG extended with parents, but without time
        gra' = nmap (\s -> (s, Nothing)) $ emap fst gra
        -- The minimum value for the new node
        minNode = head $ newNodes 1 gra

        -- Is there an edge to the near future
        futureEdge = any (\(_, _, (_, t)) -> t /= Im) $ labEdges gra


        newNs = map (\(n, a) -> (n+minNode, (a++"'", Just n))) (labNodes gra)
        newEs = map (\(sr, si, (s, t)) -> if t == Im then
                                                    (sr+minNode, si+minNode, (s, Im))
                                                  else (sr, si+minNode, (s, Im))
                    ) (labEdges gra)

        weirdGraph = foldl (flip insEdge) (foldl (flip insNode) (nmap (\s -> (s, Nothing)) gra) newNs) newEs
        g = if futureEdge then
                emap fst $ delEdges
                    (map (\(a, b, _) -> (a, b)) $
                     filter (\(_, _, (x, y)) -> y /= Im) $
                     labEdges weirdGraph
                    ) weirdGraph
            else
                gra'

-- | Extract a graph from a syntax
compileGraph :: GraphSyntax a -> Graph
compileGraph = graph . compile

-- | Extract the constraints from a syntax
compileConstraints :: GraphSyntax a -> [Constraint]
compileConstraints = constraints . compile

-- | Ignore time in the CLD
ignoreTime :: GraphSyntax a -> GraphSyntax ()
ignoreTime gs = do
                    gs
                    state <- get
                    put $ state {graph = emap (\(s, _) -> (s, Im)) (graph state)}
