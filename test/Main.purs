module Test.Main where

import Prelude

import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import FunctorialDataMigration.Core.Examples (emptySignature, freeSquare, commutativeSquare, bogusSquare, invalidEdgeSig, freeToCommMapping, freeToCommMappingViolatingSourceTargetPreservation, commToFreeMapping, commToFreeMappingTrivial)
import FunctorialDataMigration.Core.Signature (signatureIsWellFormed)
import FunctorialDataMigration.Core.SignatureMapping (mappingIsWellFormed)
import Test.Assert (assert, assertFalse)
import Test.Spec (it, describe)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (runSpec)

main :: Effect Unit
main = do
  launchAff_ do
    runSpec [ consoleReporter ] do
      describe "Well-formedness of Signatures" do
        it "emptySignature is well-formed" $ liftEffect $
          assert $ signatureIsWellFormed emptySignature
        it "freeSquare is well-formed" $ liftEffect $
          assert $ signatureIsWellFormed freeSquare
        it "commutativeSquare is well-formed" $ liftEffect $
          assert $ signatureIsWellFormed commutativeSquare
        it "bogusSquare is not well-formed" $ liftEffect $
          assertFalse $ signatureIsWellFormed bogusSquare
        it "invalidEdgeSig is not well-formed" $ liftEffect $
          assertFalse $ signatureIsWellFormed invalidEdgeSig
      describe "Well-formedness of SignatureMappings" do
        it "freeToCommMapping is well-formed" $ liftEffect $
          assert $ mappingIsWellFormed freeToCommMapping
        it "freeToCommMappingViolatingSourceTargetPreservation is not well-formed" $ liftEffect $
          assertFalse $ mappingIsWellFormed freeToCommMappingViolatingSourceTargetPreservation
        it "commToFreeMapping is not well-formed" $ liftEffect $
          assertFalse $ mappingIsWellFormed commToFreeMapping
        it "commToFreeMappingTrivial is well-formed" $ liftEffect $
          assert $ mappingIsWellFormed commToFreeMappingTrivial
