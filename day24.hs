import Data.Ord
import Data.Either
import Data.List
import qualified Data.Map as M
import Text.Parsec
import Control.Arrow

import Debug.Trace

number :: Parsec String u Int
number = read <$> many1 digit
feature :: Bool -> String -> Parsec String u (Bool,[String])
feature b x = do
  string x
  string " to "
  (,) b <$> many1 letter `sepBy` string ", "

data Group = G { gFaction :: Bool
               , gSize :: Int
               , gHp :: Int
               , gPow :: Int
               , gType :: String
               , gInit :: Int
               , gImm :: [String]
               , gWeak :: [String]
               } deriving (Show,Eq,Ord)

gEffPow = (*) <$> gSize <*> gPow

input = do
  g1 <- army True "Immune System"
  char '\n'
  g2 <- army False "Infection"
  return (g1++g2)
army f name = do
  string name
  string ":\n"
  many (armyGroup f)
armyGroup f = do
  count <- number
  string " units each with "
  hp <- number
  string " hit points "
  (immunities,weaknesses) <- (concatMap snd *** concatMap snd) . partition fst <$> option [] (string "(" *> (immunity <|> weakness) `sepBy` string "; " <* string ") ")
  string "with an attack that does "
  attack <- number
  string " "
  attackType <- many letter
  string " damage at initiative "
  initiative <- number
  string "\n"
  return $ G f count hp attack attackType initiative immunities weaknesses
immunity = feature True "immune"
weakness = feature False "weak"

targetSelect :: [Group] -> [(Group,Maybe Group)]
targetSelect g0 = go gs0 gs0 where
  gs0 = sortBy (comparing (Down . gEffPow) <>
                comparing (Down . gInit)) g0 :: [Group]
  go :: [Group] -> [Group] -> [(Group,Maybe Group)]
  go (g:gs) targets = (g,target) : go gs targets' where
    choice = map (snd . snd) $ sort $ c0
    c0 =     map (Down *** ((Down . gEffPow &&& Down . gInit) &&& id)) $ c1
    c1 =     filter ((> 0) . fst) $ c2
    c2 =     map (damage g &&& id) $
             filter ((/= gFaction g) . gFaction)targets :: [(Int,Group)]
    (target,targets') = case choice of
      (t:_) -> (Just t, delete t targets)
      _ -> (Nothing,targets)
  go [] _ = []

damage attacker defender | immune = 0
                         | weak = 2 * raw
                         | otherwise = raw
  where kind = gType attacker
        immune = kind `elem` gImm defender
        weak = kind `elem` gWeak defender
        raw = gEffPow attacker

attack attacker defender = defender' where
  losses = damage attacker defender `div` gHp defender
  defender' = defender { gSize = gSize defender - losses }

fight :: [Group] -> [Group]
fight gs = filter ((> 0) . gSize) $ go [] match where
  match = sortOn (Down . gInit . fst) (targetSelect gs)
  go done ((a,md):ms) | Nothing <- md = go (a:done) ms
                      | gSize a <= 0 = go (a:done) ms
                      | Just d <- md = 
                        let update g | g == d = d'
                                     | otherwise = g
                            d'= attack a d
                        in go (a:map update done) (map (first update) ms)
  go done [] = done

isDone (gs:gss) | null a = Left $ size b
                | null b = Right $ size a
                | gs == head gss = Left 0
                | otherwise = isDone gss
  where (a,b) = partition gFaction gs
        size = sum . map gSize

combat = isDone . iterate fight

boost b g | gFaction g = g { gPow = gPow g + b }
          | otherwise = g

bsearch f a b | a + 1 == b = v
              | isRight v = bsearch f a m
              | isLeft v = bsearch f m b
  where m = (a + b + 1) `div` 2
        v = traceShow m $ f m

main = do
  Right input <- parse input "in" <$> getContents
  print $ combat input
  let input' = map (boost 1570) input
  print $ combat input'
  let f b = combat (map (boost b) input)
  print $ bsearch f 0 1750
