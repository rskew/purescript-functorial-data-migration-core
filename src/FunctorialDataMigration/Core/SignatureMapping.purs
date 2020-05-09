module FunctorialDataMigration.Core.SignatureMapping where

import Prelude

import Data.Array (concat, fromFoldable, catMaybes)
import Data.Foldable (all)
import Data.Maybe (Maybe(..), isJust)
import Data.Set (Set)
import Data.Set as Set
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..), fst, snd)
import FunctorialDataMigration.Core.Signature (Edge, Node, Path, Signature, pathIsSupported, pathSource, pathTarget, signatureIsWellFormed, sortPath)
import StringRewriting.KnuthBendix (Equation(..), eqModuloTheory, knuthBendix)

-- | A signature mapping represents a functor between categories from which the
-- | data-migration functors are derived.
-- |
-- | As this is a mapping of signatures and not an explicit functor, we map each
-- | edge in the source signature to a *path* in the target signature.
-- | The path represents the composite morphisms in the category presented
-- | by the signature.
-- |
-- | A functor has a function from objects to objects and a function from
-- | morphisms to morphisms such that source and target of morphisms is preserved,
-- | and composition of morphisms is preserved, i.e. if $F$ is the functor and
-- | $f$, $g$ are morphisms in the source domain:
-- |
-- |     $F(f ; g) = F(f) ; F(g)$
-- |
-- | This gives functors the property of being 'structure preserving',
-- | at least concerning structure encoded into the morphisms of the
-- | source domain and the ways they can be composed. Preservation of this structure
-- | is what enables functorial data migration to describe migrations that are
-- | guaranteed not to violiate data-integrity constraints, when those
-- | constraints are given as path equations.
-- |
-- | Particular migrations can only be computed when their functors obeys
-- | additional restrictions:
-- |
-- | - $\Pi$ migrations require the functor's object function be a bijection
-- |   to ensure that the values used to fill in attributes (typed base-language
-- |   values) in the target tables are present in the source tables.
-- |
-- | - $\Sigma$ migrations require that the functor be a discrete op-fibration.
-- |   In other words, any table in the source schema that maps to a table
-- |   in the target schema must have an identical column structure.
-- |   This enables data from different tables to be unioned together
-- |   coherently.
type SignatureMapping
  = { source :: Signature
    , target :: Signature
    , nodeFunction :: Func Node Node
    , edgeFunction :: Func Edge Path
    }

-- | Represent functions as explicit mappings of elements from the domain
-- | to the codomain.
type Func a b = Set (Tuple a b)

-- | Lookup the pair with the corresponding value in the domain.
-- | O(n) complexity in the size of the domain :O
applyFunc :: forall a b. Ord a => Ord b => Func a b -> a -> Maybe b
applyFunc function a =
  function
  # Set.filter (\(Tuple from to) -> from == a)
  # Set.findMin
  <#> snd

-- | Maps a path in some category to a path in another category
-- | using a functor between them.
-- | The functor laws ensure that the target domain path will be well-formed
-- | when composed of the mapped constituent paths.
-- |
-- | This fails if the signature mapping is not well-formed.
mapPathAlongFunctor :: SignatureMapping -> Path -> Maybe Path
mapPathAlongFunctor sigMap path =
  path
  <#> applyFunc sigMap.edgeFunction
  # catMaybes
  # concat
  # sortPath <<< Set.fromFoldable

-- | Translate a path equation in one category to a path equation in another using
-- | a functor.
-- |
-- | Mappings from non-identity morphisms to identity morphisms are implicit in the
-- | mapping representation.
mapEquationAlongFunctor :: SignatureMapping -> Equation Edge -> Maybe (Equation Edge)
mapEquationAlongFunctor sigMap (Equation left right) =
  Equation <$> mapPathAlongFunctor sigMap left
           <*> mapPathAlongFunctor sigMap right

-- | Check that a function is well-formed by observing that it maps each
-- | element in its domain to an element in its codomain.
-- | A function argument is required to decide codomain membership, preventing
-- | the need to enumerate e.g. the transitive closure of all edges
-- | in a graph when checking the well-formedness of the morphism function of
-- | a functor.
functionIsWellFormed :: forall a b. Ord a => Func a b -> Set a -> (b -> Boolean) -> Boolean
functionIsWellFormed function domain inCodomain =
  -- The first of each pair is a member of the domain
  all (flip Set.member domain <<< fst) function
  &&
  -- The second of each pair is a member of the codomain
  all (inCodomain <<< snd) function
  &&
  -- Each element of the domain is included in the mapping
  all (flip Set.member (Set.map fst function)) domain
  &&
  -- Each element of the domain is mapped to exactly one element
  -- Cheekily relying on Set.map not being structure preserving (collapsing equal elements)
  Set.size function == Set.size (Set.map fst function)

-- | To check that a functor is well-formed, we check that its node and
-- | edge functions are valid, that the edge function preserves sources and
-- | targets of morphisms and that path equivalences are preserved.
mappingIsWellFormed :: SignatureMapping -> Boolean
mappingIsWellFormed sigMap =
  -- Source and target signatures are well-formed
  signatureIsWellFormed sigMap.source
  &&
  signatureIsWellFormed sigMap.target
  &&
  -- Node function is well-formed
  functionIsWellFormed sigMap.nodeFunction
                       sigMap.source.nodes
                       (flip Set.member sigMap.target.nodes)
  &&
  -- Edge function is well formed, considering un-mapped edges as
  -- implicitly mapping to identity morphisms in the codomain.
  let
    mappedEdges = sigMap.source.edges
                  # Set.filter (\edge -> Set.member edge (Set.map fst sigMap.edgeFunction))
    unmappedEdges = sigMap.source.edges
                    # Set.filter (\edge -> not $ Set.member edge mappedEdges)
  in
  functionIsWellFormed sigMap.edgeFunction
                       mappedEdges
                       (pathIsSupported sigMap.target)
  &&
  all (\unmappedEdge ->
        let
          mappedSourceNode = applyFunc sigMap.nodeFunction unmappedEdge.source
          mappedTargetNode = applyFunc sigMap.nodeFunction unmappedEdge.target
        in
          isJust mappedSourceNode
          &&
          mappedSourceNode == mappedTargetNode)
      unmappedEdges
  &&
  -- Sources and targets of morphisms are preserved
  all (\(Tuple sourceEdge targetPath) ->
        applyFunc sigMap.nodeFunction sourceEdge.source == pathSource targetPath
        &&
        applyFunc sigMap.nodeFunction sourceEdge.target == pathTarget targetPath)
      sigMap.edgeFunction
  &&
  -- Path equivalences are preserved, that is, the equations in the source domain hold
  -- after mapping their paths to the target domain.
  case traverse (mapEquationAlongFunctor sigMap)
                (fromFoldable sigMap.source.pathEquations)
  of
    Nothing -> false
    Just mappedPathEquations ->
      let
        -- TODO(purescript-string-rewriting) don't run forever when undecidable,
        -- return failure after x iterations.
        reWriteSystem = knuthBendix $ fromFoldable sigMap.target.pathEquations
      in
        all (\(Equation left right) -> eqModuloTheory reWriteSystem left right) mappedPathEquations
