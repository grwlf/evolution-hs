{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveDataTypeable #-}
module Types where

import Imports

import qualified Data.Map as Map
import qualified Data.List as List

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
  deriving(Eq,Show,Ord,Enum,Bounded,Data,Typeable)


featureArity :: Feature -> Integer
featureArity = \case
  Symbiosus -> 2
  Cooperation -> 2
  Communication -> 2
  _ -> 1

featureFood :: Feature -> Integer
featureFood = \case
  Parasite -> 2
  Carnivouros -> 1
  HighBodyWeight -> 1
  _ -> 0

newtype CardID = CardID Integer
  deriving(Show,Eq,Ord,Num,Data,Typeable)

data Card = Card {c_id :: CardID, c_features :: [Feature] }
  deriving(Show, Eq, Ord,Data,Typeable)

cardID = c_id
cardFeatures = c_features

deck :: [Card]
deck =
  let
    nextid = do { i <- get ; modify (+1) ; return i}
    add1 p = nextid >>= \i -> return (Card i [p])
    add2 p1 p2 = nextid >>= \i -> return (Card i [p1,p2])
  in do
  flip evalState (CardID 0) $ do
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

data CardFeature = CardFeature {
    cf_card :: Card
  , cf_feature :: Feature
  } deriving(Show,Eq,Ord,Data,Typeable)

cardFeature :: Card -> Feature -> CardFeature
cardFeature c@(Card _ fs) f =
  case f`elem`fs of
    True -> CardFeature c f
    False -> error $ "cardFeature: bad feature"

cardFeatureArity (CardFeature _ f) = featureArity f


data Animal = Animal { a_base :: Card, a_features :: [CardFeature] }
  deriving(Show,Eq,Ord,Data,Typeable)

initialAnimal :: Card -> Animal
initialAnimal c = Animal c mempty

animalBaseID :: Animal -> CardID
animalBaseID a = cardID $ a_base a

data Setting = Setting {
    set_animals :: Map CardID Animal
  , set_pairs :: Map CardFeature (CardID, CardID)
  } deriving(Show,Eq,Ord,Data,Typeable)

emptySetting :: Setting
emptySetting = Setting mempty mempty

removeAnimal :: CardID -> Setting -> Setting
removeAnimal aid s =
  case Map.lookup aid (set_animals s) of
    Just a -> s{set_animals = Map.delete aid (set_animals s)}
    Nothing -> error $ "removeAnimal: no such animal"

getAnimal :: CardID -> Setting -> Animal
getAnimal c s =
  case Map.lookup c (set_animals s) of
    Just a -> a
    Nothing -> error $ "getAnimal: no such animal"

modAnimal :: CardID -> Setting -> (Animal -> Animal) -> Setting
modAnimal c s f =
  case Map.lookup c (set_animals s) of
    Just a -> s{set_animals = Map.insert c (f a) (set_animals s)}
    Nothing -> error $ "modAnimal: no such animal"


addAnimal :: CardFeature -> Setting -> Setting
addAnimal cf s = s{set_animals = Map.insert (cardID (cf_card cf)) (initialAnimal (cf_card cf)) (set_animals s)}

addAnimalFeature :: CardID -> CardFeature -> Setting -> Setting
addAnimalFeature aid prop s = modAnimal aid s (\(Animal aid ps) -> Animal aid (prop:ps))

addPairingFeature :: CardID -> CardID -> CardFeature -> Setting -> Setting
addPairingFeature a1 a2 c s =
  case (cardFeatureArity c, Map.lookup a1 (set_animals s), Map.lookup a2 (set_animals s)) of
    (2, Just _, Just _) -> s {set_pairs = Map.insert c (a1,a2) (set_pairs s)}
    _ -> error $ "addPairingFeature: bad arguments"

newtype PlayerName = PlayerName Text
  deriving(Show,Eq,Ord,Data,Typeable)

data Player = Player {
    pla_name :: PlayerName
  , pla_hand :: [Card]
  , pla_setting :: Setting
  } deriving(Show,Eq,Ord,Data,Typeable)

giveCard :: Card -> Player -> Player
giveCard c p = p { pla_hand = c:(pla_hand p)}

playFromHand :: CardFeature -> (CardFeature -> Setting -> Setting) -> Player -> Player
playFromHand c f p =
  case (cf_card c)`elem`(pla_hand p) of
    True -> p{pla_setting = f c (pla_setting p), pla_hand = List.delete (cf_card c) (pla_hand p)}
    False -> error $ "playFromHand: no such card in hand"

playTopHand :: (CardFeature -> Setting -> Setting) -> Player -> Player
playTopHand f p =
  case pla_hand p of
    (c:cs) -> playFromHand (cardFeature c (head $ cardFeatures c)) f p
    [] -> p

playerAnimals :: Player -> [Animal]
playerAnimals p = Map.elems $ set_animals $ pla_setting p

playerNAnimals :: Player -> Integer
playerNAnimals p = toInteger $ length $ playerAnimals p

playerHand = pla_hand

playerEmpty :: Player -> Bool
playerEmpty p = null $ playerCards p

playerCards :: Player -> [Card]
playerCards = List.sort . listify (\c@(Card _ _) -> True)

data Table = Table {
    t_players :: Map PlayerName Player
  , t_deck :: [Card]
  , t_trash :: [Card]
  } deriving(Show,Eq,Ord,Data,Typeable)

wipeAnimal :: CardID -> Table -> Table
wipeAnimal = undefined

tableCards :: Table -> [Card]
tableCards = List.sort . listify (\c@(Card _ _) -> True)

fromPlayers :: [Player] -> Map PlayerName Player
fromPlayers ps = Map.fromList $ map (\p -> (pla_name p, p)) ps

withPlayer' :: PlayerName -> (Player -> (Player,a)) -> Table -> (Table,a)
withPlayer' pn f t =
  case Map.lookup pn (t_players t) of
    Just p -> let (p',r) = f p in (t{t_players = Map.insert pn p' (t_players t)},r)
    Nothing -> error $ "takeCards: no such player"

withPlayer :: PlayerName -> (Player -> Player) -> Table -> Table
withPlayer pn f t = fst $ withPlayer' pn (\p -> (f p,())) t

forPlayers :: [PlayerName] -> [Player -> Player] -> Table -> Table
forPlayers pn pm =
  foldl (.) id $ map (\(pn,pm) -> withPlayer pn pm) (pn`zip`pm)

forAllPlayers :: (Player -> Player) -> Table -> Table
forAllPlayers f t = t {t_players = Map.map f (t_players t)}

forAllPlayersM :: (Monad m) => (Player -> m Player) -> Table -> m Table
forAllPlayersM f t = do
  ps' <- mapM f (Map.elems (t_players t))
  return $ t {t_players = fromPlayers ps'}

setDeck :: [Card] -> Table -> Table
setDeck d t = t{ t_deck = d }

takeNCards :: PlayerName -> Integer -> Table -> Table
takeNCards pn n t =
  let
    (cs,rest) = splitAt (fromInteger n) (t_deck t)
  in
  setDeck rest $
  withPlayer pn (\p -> foldl (\p c -> giveCard c p) p cs) t

spreadCards :: Table -> Table
spreadCards t =
  flip execState t $ do
    forM_ (Map.toList $ t_players t) $ \(pn,p) -> do
      case playerEmpty p of
        True -> modify $ takeNCards pn 6
        False -> modify $ takeNCards pn (playerNAnimals p + 1)

playerTurns :: Integer -> Table -> [PlayerName]
playerTurns nturn t =
  let
    off = ((fromInteger nturn) `mod` (length (t_players t)))
    p = map pla_name $ Map.elems $ t_players t
  in
  (drop off p) <> (take off p)

