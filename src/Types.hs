{-# LANGUAGE LambdaCase #-}
module Types where

import Imports

data Feature =
    Running
  | Swimming
  | TailLoss
  | Piracy
  | Mimicry
  | Scavanger
  | Poisonous
  | Carnivouros
  | HighBodyWeight
  | FatTissue
  | Burrowing
  | Hibernation
  | SharpVision
  | Camouflage
  | Grazing
  | Parasite
  | Symbiosus
  | Cooperation
  | Communication
  deriving(Eq,Show,Ord,Enum,Bounded)


featureArity :: Feature -> Integer
featureArity = \case
  Symbiosus -> 2
  Cooperation -> 2
  Communication -> 2
  _ -> 1

data CardID = CardID Integer
  deriving(Show,Eq,Ord,Num)

data Card = Card1 CardID Feature | Card2 CardID Feature Feature
  deriving(Show)

deck :: [Card]
deck =
  let
    nextid = do { i <- get ; modify (+1) ; return i}
    add1 p = nextid >>= \i -> return (Card1 i p)
    add2 p1 p2 = nextid >>= \i -> return (Card2 i p1 p2)
  in do
  execState (CardID 0) $ do
    sequence $
       replicate 4 (add1 Running)
    <> replicate 8 (add1 Swimming)
    <> replicate 4 (add1 TailLoss)
    <> replicate 4 (add1 Piracy)
    <> replicate 4 (add1 Mimicry)
    <> replicate 4 (add1 Scavanger)
    <> replicate 4 (add2 Poisonous Carnivouros)
    <> replicate 4 (add2 HighBodyWeight Carnivouros)
    <> replicate 4 (add2 HighBodyWeight FatTissue)
    <> replicate 4 (add2 Burrowing FatTissue)
    <> replicate 4 (add2 Hibernation Carnivouros)
    <> replicate 4 (add2 SharpVision FatTissue)
    <> replicate 4 (add2 Camouflage FatTissue)
    <> replicate 4 (add2 Grazing FatTissue)

    <> replicate 4 (add2 Parasite Carnivouros)
    <> replicate 4 (add2 Parasite FatTissue)

    <> replicate 4 (add1 Symbiosus)
    <> replicate 4 (add2 Cooperation Carnivouros)
    <> replicate 4 (add2 Cooperation FatTissue)
    <> replicate 4 (add2 Communication Carnivouros)



