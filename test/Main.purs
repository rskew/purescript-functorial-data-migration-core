module Test.Main where

import Prelude

import Data.Set as Set
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Class.Console as Console
import KnuthBendix (Equation(..))
import Signature (Edge(..), Signature, signatureIsWellFormed)
import SignatureMapping (SignatureMapping, mappingIsWellFormed)
import Test.Assert (assert', assertFalse')

emptySignature :: Signature
emptySignature = { nodes         : Set.empty
                 , edges         : Set.empty
                 , pathEquations : Set.empty
                 }

-- |      f
-- |   a ---> b
-- |   |      |
-- | g |      | h
-- |   \/     \/
-- |   c ---> d
-- |      i
-- |
freeSquare :: Signature
freeSquare =
  { nodes : Set.fromFoldable ["a", "b", "c", "d"]
  , edges : Set.fromFoldable [ Edge {source: "a", target: "b"}
                             , Edge {source: "b", target: "d"}
                             , Edge {source: "a", target: "c"}
                             , Edge {source: "c", target: "d"}
                             ]
  , pathEquations : Set.empty
  }

-- |       f'
-- |    a' ---> b'
-- |    |       |
-- | g' |   =   | h'
-- |    \/      \/
-- |    c' ---> d'
-- |        i'
-- |
-- | with f' ; h' = g' ; i'
-- |
commutativeSquare :: Signature
commutativeSquare =
  { nodes : Set.fromFoldable ["a'", "b'", "c'", "d'"]
  , edges : Set.fromFoldable [ Edge {source: "a'", target: "b'"}
                             , Edge {source: "b'", target: "d'"}
                             , Edge {source: "a'", target: "c'"}
                             , Edge {source: "c'", target: "d'"}
                             ]
  , pathEquations : Set.singleton
      $ Equation [ Edge {source: "a'", target: "b'"}, Edge {source: "b'", target: "d'"} ]
                 [ Edge {source: "a'", target: "c'"}, Edge {source: "c'", target: "d'"} ]
  }

-- | The bogus square has a path equation including an edge
-- | from a to d which does not exist in the signature's edge set
bogusSquare :: Signature
bogusSquare =
  { nodes : Set.fromFoldable ["a", "b", "c", "d"]
  , edges : Set.fromFoldable [ Edge {source: "a", target: "b"}
                             , Edge {source: "b", target: "d"}
                             , Edge {source: "a", target: "c"}
                             , Edge {source: "c", target: "d"}
                             ]
  , pathEquations : Set.singleton
    (Equation [ Edge {source: "a", target: "b"}, Edge {source: "b", target: "d"} ]
              [ Edge {source: "a", target: "d"} ])
  }

-- | A signature with an edge that doesn't have corresponding nodes
invalidEdgeSig :: Signature
invalidEdgeSig =
  { nodes : Set.singleton "a"
  , edges : Set.singleton $ Edge { source : "a", target : "b" }
  , pathEquations : Set.empty
  }

-- | This mapping from the free square to the commutative square is
-- | well-formed as the node- and edge-functions are well-formed
-- | and the mapping preserves path equivalences.
-- |
-- | As the free square has no path equivalences, they are trivially preserved.
freeToCommMapping :: SignatureMapping
freeToCommMapping =
  { source : freeSquare
  , target : commutativeSquare
  , nodeFunction : Set.fromFoldable
    [ Tuple "a" "a'"
    , Tuple "b" "b'"
    , Tuple "c" "c'"
    , Tuple "d" "d'"
    ]
  , edgeFunction : Set.fromFoldable
    [ Tuple (Edge {source: "a", target: "b"})
            [ Edge {source: "a'", target: "b'"} ]
    , Tuple (Edge {source: "b", target: "d"})
            [ Edge {source: "b'", target: "d'"} ]
    , Tuple (Edge {source: "a", target: "c"})
            [ Edge {source: "a'", target: "c'"} ]
    , Tuple (Edge {source: "c", target: "d"})
            [ Edge {source: "c'", target: "d'"} ]
    ]
  }

-- | This mapping violates the functor law of source-target preservation for
-- | morphisms. For a morphism in the domain $f: a -> b$ that is mapped to a
-- | morphisms in the codomain $F(f)$ by a functor $F$, then $F(f) must have
-- | source and target given by $F(a) -> F(b)$.
freeToCommMappingViolatingSourceTargetPreservation :: SignatureMapping
freeToCommMappingViolatingSourceTargetPreservation =
  { source : freeSquare
  , target : commutativeSquare
  , nodeFunction : Set.fromFoldable
    [ Tuple "a" "a'"
    , Tuple "b" "b'"
    , Tuple "c" "a'"
    , Tuple "d" "b'"
    ]
  , edgeFunction : Set.fromFoldable
    [ Tuple (Edge {source: "a", target: "b"})
            [ Edge {source: "a'", target: "b'"} ]
    , Tuple (Edge {source: "b", target: "d"})
            [ Edge {source: "b'", target: "d'"} ]
    , Tuple (Edge {source: "a", target: "c"})
            [ Edge {source: "a'", target: "c'"} ]
    , Tuple (Edge {source: "c", target: "d"})
            [ Edge {source: "c'", target: "d'"} ]
    ]
  }

-- | This mapping from the commutative square to the free square is not well-formed
-- | as the morphisms of the commutative square do not obey their equation after
-- | being mapped to the free square.
-- |
-- | For this mapping to correspond to a valid functor, the following equation
-- | has to hold:
-- |
-- |     $f ; h = F(f') ; F(h') = F(f' ; h') = F(g' ; i') = F(g') ; F(i') = g ; h$
-- |
-- | But this does not for the free square, where $f ; h \neq g ; h$
commToFreeMapping :: SignatureMapping
commToFreeMapping =
  { source : commutativeSquare
  , target : freeSquare
  , nodeFunction : Set.fromFoldable
    [ Tuple "a'" "a"
    , Tuple "b'" "b"
    , Tuple "c'" "c"
    , Tuple "d'" "d"
    ]
  , edgeFunction : Set.fromFoldable
    [ Tuple (Edge {source: "a'", target: "b'"})
            [ Edge {source: "a", target: "b"} ]
    , Tuple (Edge {source: "b'", target: "d'"})
            [ Edge {source: "b", target: "d"} ]
    , Tuple (Edge {source: "a'", target: "c'"})
            [ Edge {source: "a", target: "c"} ]
    , Tuple (Edge {source: "c'", target: "d'"})
            [ Edge {source: "c", target: "d"} ]
    ]
  }

-- | This mapping from the commutative square to the free square *is* well-formed
-- | as by mapping all commutativeSquare objects to a single freeSquare object and
-- | all commutativeSquare edges to the identity morphism of the freeSquare object,
-- | the path equations are trivially preserved.
commToFreeMappingTrivial :: SignatureMapping
commToFreeMappingTrivial =
  { source : commutativeSquare
  , target : freeSquare
  , nodeFunction : Set.fromFoldable
    [ Tuple "a'" "a"
    , Tuple "b'" "a"
    , Tuple "c'" "a"
    , Tuple "d'" "a"
    ]
  , edgeFunction : Set.fromFoldable
    [ Tuple (Edge {source: "a'", target: "b'"})
            [ Id "a" ]
    , Tuple (Edge {source: "b'", target: "d'"})
            [ Id "a" ]
    , Tuple (Edge {source: "a'", target: "c'"})
            [ Id "a" ]
    , Tuple (Edge {source: "c'", target: "d'"})
            [ Id "a" ]
    ]
  }

main :: Effect Unit
main = do
  assert' "emptySignature should we well-formed"
    $ signatureIsWellFormed emptySignature

  assert' "freeSquare should we well-formed"
    $ signatureIsWellFormed freeSquare

  assert' "commutativeSquare should we well-formed"
    $ signatureIsWellFormed commutativeSquare

  assertFalse' "bogusSquare should not be well-formed"
    $ signatureIsWellFormed bogusSquare

  assertFalse' "invalidEdgeSig should not be well-formed"
    $ signatureIsWellFormed invalidEdgeSig

  assert' "freeToCommMapping should be well-formed"
    $ mappingIsWellFormed freeToCommMapping

  assertFalse' "freeToCommMappingViolatingSourceTargetPreservation should not be well-formed"
    $ mappingIsWellFormed freeToCommMappingViolatingSourceTargetPreservation

  assertFalse' "commToFreeMapping should not be well-formed"
    $ mappingIsWellFormed commToFreeMapping

  assert' "commToFreeMappingTrivial should be well-formed"
    $ mappingIsWellFormed commToFreeMappingTrivial

  Console.log ":D"
