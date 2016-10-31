{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric     #-}

module DSL.Export where

import           Control.Monad
import           Control.Monad.State
import           Data.Aeson
import           Data.Aeson.Types
import           Data.GraphViz
import qualified Data.Graph.Inductive.Graph as G
import qualified Data.Text                  as T
import           Data.Text                  (Text)
import qualified Data.Text.Lazy.IO          as T
import qualified Data.Text.Lazy.Encoding    as T
import           DSL.GraphDSL
import           GHC.Generics

-- * Intermediate JSON record
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

type Name = Text

-- | Representation of the JSON graph object. A graph consists of a list of
-- nodes (represented by names) and a list of directed edges annotated with a
-- @Sign@. A Graph object is formatted (in JSON) as
--
-- > { "nodes":[<String>]
-- > , "edges":[[<String>, <String>, +/++/+?/-/--/-?/0/?], ...]
-- > }
data GObj = GObj 
  { nodes :: [Name]
  , edges :: [(Name, Name, Sign)]
  } deriving (Generic, Show)

instance FromJSON GObj where

instance ToJSON GObj where

instance ToJSON Sign where
  toJSON = String . unSign

-- | An edge sign is one of @+@, @++@, @+?@, @-@, @--@, @-?@, @0@, @?@.
instance FromJSON Sign where
  parseJSON (String s) = mkSign s
  parseJSON _          = mzero

unSign :: Sign -> Text
unSign sgn = 
  case sgn of
    P  -> "+"
    PP -> "++"
    PQ -> "+?"
    M  -> "-"
    MM -> "--"
    MQ -> "-?"
    Z  -> "0"
    Q  -> "?"

mkSign :: Text -> Parser Sign
mkSign s = 
  case s of 
    "+"  -> return P
    "++" -> return PP 
    "+?" -> return PQ
    "-"  -> return M
    "--" -> return MM
    "-?" -> return MQ
    "0"  -> return Z
    "?"  -> return Q
    str  -> mzero

-- * Test
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

-- | Example graph.
exGraph :: GObj
exGraph = GObj
  ["node1", "node2", "node3"]
  [ ("node1", "node2", P)
  , ("node1", "node3", M)
  , ("node2", "node3", P)
  ]

-- | Parse test.
test :: IO (Maybe GObj)
test = (decode . T.encodeUtf8) <$> T.readFile "src/DSL/exgraph.json"

testAndDraw :: IO ()
testAndDraw = do
  obj <- (decode . T.encodeUtf8) <$> T.readFile "src/DSL/exgraph.json"
  case obj of
    Nothing -> putStrLn "Decoding failed."
    Just g  -> flip exportPDF "ex.pdf" $ compileGraph $ toGraphDSL g

-- | Write test.
testwrite = encode exGraph

-- | Patrik's function (put a bunch of these in here? i.e. preview, savePDF, 
-- etc).
exportPDF g name = ign $ runGraphviz dg Pdf name
  where
    dg = setDirectedness graphToDot params g
    params = nonClusteredParams { fmtNode = \ (_,l)     -> [toLabel l]
                                , fmtEdge = \ (_, _, l) -> [toLabel l]
                                }

    ign = (>> return ())

testInt :: GraphSyntax ()
testInt = do
  n1 <- mkNode "node1"
  n2 <- mkNode "node2"
  n3 <- mkNode "node3"
  link n1 >+> n2 >+> n3
  link n2 >-> n3
  return ()

-- * Encoding and decoding GraphDSL
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

-- | Convert a GraphSyntax representation to a JSON representation.
--
-- /No error checking; assuming well-formed graphs./
fromGraphDSL :: GraphSyntax () -> GObj
fromGraphDSL gs = GObj ns es
  where
    g   = graph $ execState gs initialState
    lns = G.labNodes g
    les = G.labEdges g

    ns  = map (T.pack . snd) lns
    es  = [ (T.pack a, T.pack b, s) 
          | (Just a, Just b, s) <- map (mkEdge lns) les 
          ]

    mkEdge m = rename m . deconstr

    deconstr (x, y, (s, _)) = (x, y, s)
    rename m (x, y, z)      = (x `lookup` m, y `lookup` m, z)

-- | Convert a JSON representation to GraphSyntax representation.
toGraphDSL :: GObj -> GraphSyntax ()
toGraphDSL (GObj ns es) = do
  let names  = map T.unpack ns
      edges  = map (\(x, y, z) -> (T.unpack x, T.unpack y, z)) es
  nodes <- mapM mkNode names
  mapM_ (mkEdge (zip names nodes)) edges
  where
    mkEdge m (a, b, s) = 
      case (a `lookup` m, b `lookup` m) of
        (Just c, Just d) -> makeEdge s Im (pure c) d
        (Nothing, _)     -> fail $ a ++ " is not a declared node."
        (_, Nothing)     -> fail $ b ++ " is not a declared node."
      
  
