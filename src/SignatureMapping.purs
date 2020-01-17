module SignatureMapping where

import Prelude

import Data.Array (concatMap, fromFoldable)
import Data.Foldable (all)
import Data.Maybe (Maybe(..))
import Data.Set (Set)
import Data.Set as Set
import Data.Tuple (Tuple(..), fst, snd)
import Effect.Exception (error, throwException)
import Effect.Unsafe (unsafePerformEffect)
import KnuthBendix (Equation(..), eqModuloTheory, knuthBendix)
import Signature (Edge, Node, Path, Signature, pathIsSupported, pathSource, pathTarget, signatureIsWellFormed)
import Unsafe.Coerce (unsafeCoerce)

-- | A signature mapping represents a functor between categories from which the
-- | data-migration functors are derived.
-- |
-- | As this is a mapping of signatures and not an explicit functor, we map each
-- | edge in the source signature to a *path* in the target signature.
-- | The path represents the composite morphisms in the category presented
-- | by the signature.
-- |
-- | A functor has a function from objects to objects and a function from from
-- | morphisms to morphisms such that source and target of morphisms is preserved,
-- | and composition of morphisms is preserved, i.e. if $F$ is the functor and
-- | $f$, $g$ are morphisms in the source domain:
-- |
-- |     $F(f ; g) = F(f) ; F(g)$
-- |
-- | This gives functors the property of being 'structure preserving',
-- | at least concerning structure encoded into the morphisms of the
-- | source domain and their composition. Preservation of this structure
-- | is what enables functorial data migration to describe migrations that are
-- | guaranteed not to violiate data-integrity constraints, when those
-- | constraints are given as path equations.
-- |
-- | Particular migrations can only be computed when their functos obeys
-- | additional restrictions:
-- |
-- | - $\Pi$ migrations require the functor's object function to be a bijection
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

-- | Represent functions as exlicit mappings of elements from the domain
-- | to the codomain.
type Func a b = Set (Tuple a b)

-- | Lookup the pair with the corresponding value of the domain.
-- | O(n) complexity in the size of the domain :O
applyFunc :: forall a b. Ord a => Ord b => Func a b -> a -> b
applyFunc function a =
  case function # Set.filter (\(Tuple from to) -> from == a) # Set.findMin of
    Nothing ->
      unsafePerformEffect $ throwException $ error "Tried to apply mapping function and failed"
      # unsafeCoerce
    Just (Tuple from to) -> to

-- | Maps a path in some category to a path in another category
-- | using a functor between them.
-- | The functor laws ensure that the target domain path will be well-formed
-- | when composed of the mapped constituent paths.
-- |
-- | This function does not check that the signature mapping is well-formed
-- | (i.e. represents a valid functor) or that the argument path is a valid path
-- | in the source domain of the mapping.
mapPathAlongFunctor :: SignatureMapping -> Path -> Path
mapPathAlongFunctor sigMap path = concatMap (applyFunc sigMap.edgeFunction) path

-- | Check that a function is well-formed by observing that it maps each
-- | element in its domain to an element in its codomain.
-- | A function argument is required to decide codomain membership, preventing
-- | the need to explicitly represent e.g. the transitive closure of all edges
-- | in a graph when specifying the morphism function for a functor.
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
-- | edge functions are valid, that the edge function preserves source and
-- | target of morphisms and that path equivalences are respected.
-- |
-- | Checking source and target of morphisms amounts to checking that
-- | given a morhpisms in the domain $f : a -> b$, $F(f) : F(a) -> F(b)$.
-- |
-- | TODO: Check that path equivalences are preserved
mappingIsWellFormed :: SignatureMapping -> Boolean
mappingIsWellFormed sigMap =
  -- Source and target signatures are well-formed
  signatureIsWellFormed sigMap.source
  &&
  signatureIsWellFormed sigMap.target
  &&
  -- Node and edge functions are well-formed
  functionIsWellFormed sigMap.nodeFunction sigMap.source.nodes
                                           (flip Set.member sigMap.target.nodes)
  &&
  functionIsWellFormed sigMap.edgeFunction sigMap.source.edges
                                           (pathIsSupported sigMap.target)
  &&
  -- Source and target of morphisms are preserved
  all (\(Tuple sourceEdge targetPath) ->
        Just (applyFunc sigMap.nodeFunction sourceEdge.source) == pathSource targetPath
        &&
        Just (applyFunc sigMap.nodeFunction sourceEdge.target) == pathTarget targetPath)
      sigMap.edgeFunction
  &&
  -- Path equivalences are preserved, that is, the equations in the source domain hold
  -- after mapping their paths to the target domain.
  let
    mappedPathEquations =
      Set.map (\(Equation left right) ->
                Equation (mapPathAlongFunctor sigMap left)
                         (mapPathAlongFunctor sigMap right))
              sigMap.source.pathEquations
    -- TODO(purescript-string-rewriting) don't run forever, return failure after x iterations
    reWriteSystem = knuthBendix $ fromFoldable sigMap.target.pathEquations
  in
    all (\(Equation left right) -> eqModuloTheory reWriteSystem left right) mappedPathEquations
