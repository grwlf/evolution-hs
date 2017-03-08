module Test where

import Test.Tasty
import Test.Tasty.QuickCheck

import Imports
import Types


genDeck = shuffle deck

genEmptyPlayer = Player <$> (PlayerName <$> elements [pack [l] | l<-['A'..'Z']]) <*> pure mempty <*> pure emptySetting

genEmptyPlayers = listOf1 genEmptyPlayer

genTable = Table <$> (fromPlayers <$> genEmptyPlayers) <*> genDeck <*> pure mempty

data Move = Move { m_desc :: String, m_fn :: (Player->Player)}

instance Show Move where
  show (Move s _) = s

genMove :: Gen Move
genMove = elements [
    Move "add-a" $ playTopHand addAnimal
  , Move "add-p" $ \p ->
      case playerAnimals p of
        (a1:a2:_) -> playTopHand (addPairingFeature (animalBaseID a1) (animalBaseID a2)) p
        _ -> p
  , Move "add-f" $ \p ->
      case playerAnimals p of
        (a1:_) -> playTopHand (addAnimalFeature (animalBaseID a1)) p
        _ -> p
  ]

data TMove = TMove String (Table->Table)

instance Show TMove where
  show (TMove s _) = s

genMoves :: Gen TMove
genMoves = do
  n <- elements [1..100]
  ms <- listOf genMove
  elements [
      TMove "spread" spreadCards
    , TMove (show n <> ":" <> concat (intersperse "," (map m_desc ms))) $ \t ->
        forPlayers (playerTurns n t) (map m_fn ms) t
    ]

main = defaultMain $ testGroup "all" [
    testGroup "phase1" [
      testProperty "1" $
        forAll genTable $ \t ->
          forAll genMoves $ \(TMove _ fn) ->
            tableCards t == tableCards (fn t)
    ]
  ]
