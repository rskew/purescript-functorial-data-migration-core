module Signature where

import Prelude

import Data.Array (uncons, unsnoc)
import Data.Foldable (all)
import Data.Maybe (Maybe)
import Data.Set (Set)
import Data.Set as Set
import KnuthBendix (Equation(..))

-- | A Signature is a graph + path equivalences that give the presentation of
-- | a category.
-- |
-- | A Signature's members are labelled nodes and edges to reduce confusion with
-- | the corresponding category.
-- |
-- | For functorial data migration we require that schemas come from finite
-- | categories, i.e. the signature graph has no cycles (modulo path
-- | equivalances) that would lead to infinitely-many morphisms.
-- |
-- | For example, the signature with nodes ${a, b}$ and edges
-- | ${f:(a, b), g:(b, a)}$ generates to a category with objects ${a, b}$
-- | and morphisms
-- | ${a_identity, b_identity, f, g, fg, gf, fgf, gfg, fgfg, gfgf, ... }$
-- | However, including the path equation $fg = identity_a$ restricts the
-- | morphisms to ${a_identity, b_identity, f, g, gf}$, giving a finite category.
type Signature
  = { nodes         :: Set Node
    , edges         :: Set Edge
    , pathEquations :: Set (Equation Edge)
    }

type Node = String

data Edge
  = Edge { source :: Node, target :: Node }
  | Id Node
derive instance eqEdge :: Eq Edge
derive instance ordEdge :: Ord Edge
instance showEdge :: Show Edge where
   show = case _ of
     Edge {source, target} -> "{" <> show source <> ", " <> show target <> "}"
     Id node -> "{ id " <> show node <> "}"

edgeSource :: Edge -> Node
edgeSource = case _ of
  Edge {source, target} -> source
  Id node -> node

edgeTarget :: Edge -> Node
edgeTarget = case _ of
  Edge {source, target} -> target
  Id node -> node

edgeNodes :: Edge -> Set Node
edgeNodes = case _ of
  Edge {source, target} -> Set.fromFoldable [source, target]
  Id node -> Set.singleton node

-- | A Path is a set of edges that connect together into an unbroken graph walk.
type Path = Array Edge

pathSource :: Path -> Maybe Node
pathSource path = uncons path <#> \{head, tail} ->
  case head of
    Edge {source, target} -> source
    Id node -> node

pathTarget :: Path -> Maybe Node
pathTarget path = unsnoc path <#> \{init, last} ->
  case last of
    Edge {source, target} -> target
    Id node -> node

-- | Check that a path, given by a collection of nodes, is supported by edges
-- | in the graph.
pathIsSupported :: Signature -> Path -> Boolean
pathIsSupported sig pathEdges =
  all (case _ of
         Edge edge -> Set.member (Edge edge) sig.edges
         Id node -> Set.member node sig.nodes)
      pathEdges

-- | Check that the path equations describe paths that exist in the signature
-- | and that the nodes refered to by the edges are present in the node set.
signatureIsWellFormed :: Signature -> Boolean
signatureIsWellFormed sig =
  let
    pathEquationIsSupported (Equation leftPath rightPath) =
      pathIsSupported sig leftPath
      &&
      pathIsSupported sig rightPath
    allEdgeNodes = Set.unions $ Set.map edgeNodes sig.edges
  in
    all (flip Set.member sig.nodes) allEdgeNodes
    &&
    all pathEquationIsSupported sig.pathEquations

