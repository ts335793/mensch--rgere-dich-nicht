{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiWayIf #-}

module Main where

import Data.Foldable as Foldable
import Data.List as List
import Data.Lists as Lists
import Data.Map as Map
import Data.Maybe as Maybe
import Control.Monad
import Control.Monad.Random
import Debug.Trace
import Data.List.Extras
import Text.ParserCombinators.Parsec as Parsec
import Text.ParserCombinators.Parsec.Number
import System.IO

data Player = Red
            | Green
            | Blue
            | Yellow
            | Nature
            | UnexistingPlayer
    deriving (Show, Eq, Ord, Enum)

data Position = Base Player
              | Ring Player Int
              | Home Player Int
    deriving (Show, Eq, Ord)

type PlayerState = [Position]

data GameState = GameState {
    queue :: [Player],
    players :: Map Player PlayerState,
    die :: Maybe Int,
    trials :: Int
} deriving Show

type Move = (Position, Position)

type GameStatistics = Map Player Double

replaceOne :: Eq a => a -> a -> [a] -> [a]
replaceOne from to = replaceOne'
  where
    replaceOne' (x:xs)
        | x == from = to : xs
        | otherwise = x : replaceOne' xs
    replaceOne' [] = []

base :: Position -> Bool
base (Base _) = True
base _ = False

ring :: Position -> Bool
ring (Ring _ _) = True
ring _ = False

home :: Position -> Bool
home (Home _ _) = True
home _ = False

pawns :: GameState -> Player -> [Position]
pawns (GameState _ ps _ _) p =
    let Just pwns = Map.lookup p ps in pwns

basePawns :: GameState -> Player -> [Position]
basePawns g p = List.filter base $ pawns g p

ringPawns :: GameState -> Player -> [Position]
ringPawns g p = List.filter ring $ pawns g p

homePawns :: GameState -> Player -> [Position]
homePawns g p = List.filter home $ pawns g p

intToPosition :: Player -> Int -> Maybe Position
intToPosition p pos
    | pos == 0              = Just (Base p)
    | 0 < pos && pos <= 40  = Just (Ring p pos)
    | 40 < pos && pos <= 44 = Just (Home p pos)
    | otherwise             = Nothing

positionToInt :: Position -> Int
positionToInt (Base _) = 0
positionToInt (Ring _ pos) = pos
positionToInt (Home _ pos) = pos

translatedPosition :: Position -> Player -> Position
translatedPosition (Ring p1 posP1) p2 = 
    let posP2 = (posP1 + fromEnum p1 * 10 - fromEnum p2 * 10) `mod` 40
    in Ring p2 (if posP2 == 0 then 40 else posP2)
translatedPosition pos _ = pos

positionCanBeTaken :: GameState -> Position -> Bool
positionCanBeTaken _ (Base _) = True
positionCanBeTaken g (Ring p pos) = Ring p pos `List.notElem` pawns g p
positionCanBeTaken g (Home p pos) = Home p pos `List.notElem` pawns g p

winner :: GameState -> Maybe Player
winner (GameState _ ps _ _) = 
    case List.find (\(_, poss) -> List.all home poss) $ Map.toList ps of
        Nothing -> Nothing
        Just (p, _) -> Just p

player :: GameState -> Player
player (GameState q _ _ _) = head q

nextPlayer :: GameState -> Player
nextPlayer (GameState q _ _ _) = head (tail q)

voidMove :: Player -> Move
voidMove p = (Base p, Base p)

playerMoves :: GameState -> [Move]
playerMoves g = if List.null moves then [voidMove p] else moves
  where
    p = player g

    baseMoves :: GameState -> [Move]
    baseMoves (GameState _ _ (Just 6) _) =
        if (not $ List.null $ basePawns g p) && (positionCanBeTaken g $ Ring p 1)
            then [(Base p, Ring p 1)]
            else []
    baseMoves _ = []

    ringMoves :: GameState -> [Move]
    ringMoves (GameState _ _ (Just d) _) = Maybe.mapMaybe help $ ringPawns g p
      where
        help :: Position -> Maybe Move
        help (Ring p pos) =
            case intToPosition p $ pos + d of
                Just newPos ->
                    if positionCanBeTaken g newPos
                    then Just (Ring p pos, newPos)
                    else Nothing
                Nothing -> Nothing

    homeMoves :: GameState -> [Move]
    homeMoves (GameState _ _ (Just d) _) = Maybe.mapMaybe help $ homePawns g p
      where
        help :: Position -> Maybe Move
        help (Home p pos) =
            case intToPosition p $ pos + d of
                Just newPos ->
                    if positionCanBeTaken g newPos
                    then Just (Home p pos, newPos)
                    else Nothing
                Nothing -> Nothing

    moves = baseMoves g ++ ringMoves g ++ homeMoves g

playerHasAllPawnsInBase :: GameState -> Player -> Bool
playerHasAllPawnsInBase g p = List.all (== Base p) $ pawns g p

natureMoves :: GameState -> [Int]
natureMoves g =
    if playerHasAllPawnsInBase g $ nextPlayer g
    then [1,1,1,6,6] {- almost 91 / 216 -}
    else [1,2,3,4,5,6]

gameStateAppliedToPlayerMove :: GameState -> Move -> GameState
gameStateAppliedToPlayerMove g (src, dst) =
    GameState {
        queue = tail q,
        die = Nothing,
        trials = t - 1,
        players = Map.mapWithKey help ps
    }
      where
        p = player g
        
        GameState q ps (Just _) t = g

        help :: Player -> PlayerState -> PlayerState
        help p1 pwns1 =
            if p == p1
            then replaceOne src dst pwns1
            else replaceOne (translatedPosition dst p1) (Base p1) pwns1

gameStateAppliedToNatureMove :: GameState -> Int -> GameState {- OK -}
gameStateAppliedToNatureMove g 6 =
    case g of
        GameState q _ Nothing 0 ->
            g {
                queue = nextPlayer g : q,
                die = Just 6,
                trials = 3
            }
        GameState q _ Nothing 1 ->
            g {
                queue = tail q,
                die = Just 6,
                trials = 1
            }
        GameState q _ Nothing 2 ->
            g {
                queue = nextPlayer g : q,
                die = Just 6,
                trials = 2
            }
gameStateAppliedToNatureMove g d =
    g {
        queue = tail $ queue g,
        die = Just d,
        trials = 1
    }

terminal :: GameState -> Bool
terminal g =
    case winner g of
        Just _ -> True
        Nothing -> False

evaluate :: GameState -> Player
evaluate g = let Just w = winner g in w

randomPlayout :: MonadRandom m => GameState -> m Player
randomPlayout g =
    if terminal g
        then return $ evaluate g
        else case player g of
            Nature -> do
                let ms = natureMoves g
                mv <- uniform ms
                randomPlayout $ gameStateAppliedToNatureMove g mv
            _ -> do
                let ms = playerMoves g
                mv <- uniform ms
                randomPlayout $ gameStateAppliedToPlayerMove g mv

positionIsTakenByEnemy :: GameState -> Position -> Bool
positionIsTakenByEnemy g (Base p) = False
positionIsTakenByEnemy g (Ring p1 pos1) =
    let poss = List.concat $ Map.elems $ players g
        help :: Position -> Bool
        help (Ring p2 pos2) = p1 /= p2 && pos1 == pos2
        help _ = False
    in List.any help poss
positionIsTakenByEnemy g (Home p pos) = False

firstForwardMove :: [Move] -> Move
firstForwardMove ms = List.foldl (\a b -> if snd a  < snd b then b else a) (head ms) ms

antiFirstForwardPlayout :: forall m. MonadRandom m => Player -> GameState -> m Player
antiFirstForwardPlayout me g =
    if terminal g
        then return $ evaluate g
        else case player g of
            Nature -> do
                let ms = natureMoves g
                mv <- uniform ms
                antiFirstForwardPlayout me $ gameStateAppliedToNatureMove g mv
            p -> do
                let ms = playerMoves g
                if p == me
                    then do
                        let bms = List.filter (\(_, dst) -> positionIsTakenByEnemy g dst) ms
                        if not $ List.null bms
                            then do
                                mv <- uniform bms
                                antiFirstForwardPlayout me $ gameStateAppliedToPlayerMove g mv
                            else do
                                mv <- uniform ms
                                antiFirstForwardPlayout me $ gameStateAppliedToPlayerMove g mv
                    else antiFirstForwardPlayout me $ gameStateAppliedToPlayerMove g $ firstForwardMove ms

firstForwardPlayout :: forall m. MonadRandom m => GameState -> m Player
firstForwardPlayout g =
    if terminal g
        then return $ evaluate g
        else case player g of
            Nature -> do
                let ms = natureMoves g
                mv <- uniform ms
                firstForwardPlayout $ gameStateAppliedToNatureMove g mv
            p -> do
                let ms = playerMoves g
                firstForwardPlayout $ gameStateAppliedToPlayerMove g $ firstForwardMove ms

prefereBeatPlayout :: forall m. MonadRandom m => GameState -> m Player
prefereBeatPlayout g =
    if terminal g
        then return $ evaluate g
        else case player g of
            Nature -> do
                let ms = natureMoves g
                mv <- uniform ms
                prefereBeatPlayout $ gameStateAppliedToNatureMove g mv
            p -> do
                let ms = playerMoves g
                    bms = List.filter (\(_, dst) -> positionIsTakenByEnemy g dst) ms
                if not $ List.null bms
                    then do
                        mv <- uniform bms
                        prefereBeatPlayout $ gameStateAppliedToPlayerMove g mv
                    else do
                        mv <- uniform ms
                        prefereBeatPlayout $ gameStateAppliedToPlayerMove g mv

emptyStatistics :: GameState -> GameStatistics
emptyStatistics g =
    Map.map (const 0) $ players g

statisticsFromPlayers :: GameState -> [Player] -> GameStatistics
statisticsFromPlayers g ps = Map.mapWithKey (\p _ -> fromIntegral $ Lists.countElem p ps) $ players g

statisticsFromPlayer :: GameState -> Player -> Double -> GameStatistics
statisticsFromPlayer g w i = Map.mapWithKey (\p _ -> if p == w then i else 0) $ players g

updatedStatistics :: GameStatistics -> Player -> GameStatistics
updatedStatistics sts w = Map.insert w ((sts ! w) + 1) sts

{- must be run in player turn -}
monteCarlo :: forall m. MonadRandom m => (GameState -> m Player) -> GameState -> Int -> m (Move, GameStatistics)
monteCarlo f g i = do
    let p = player g

        help :: (Move, GameStatistics) -> Move -> m (Move, GameStatistics)
        help (bmv, bsts) cmv = do
            rs <- replicateM i $ f $ gameStateAppliedToPlayerMove g cmv
            let csts = statisticsFromPlayers g rs
            if csts ! p > bsts ! p
                then return (cmv, csts)
                else return (bmv, bsts)
    
    Foldable.foldlM help (voidMove p, emptyStatistics g) $ playerMoves g

ucb :: Double -> Double -> Double -> Double
ucb _ 0 _ = 1000000
ucb nv ns ws = ws / ns + sqrt (log nv / ns)

{- must be run in player turn -}
monteCarloUCB :: forall m. MonadRandom m => (GameState -> m Player) -> GameState -> Int -> m (Move, GameStatistics)
monteCarloUCB f g i = loop i 0 mvs
  where
    p = player g
    
    mvs :: Map Move (Int, Int, GameStatistics) -- s -> ns, ws, sts
    mvs = Map.fromList [(mv, (0, 0,emptyStatistics g)) | mv <- playerMoves g]

    help :: Int -> (Move, (Int, Int, GameStatistics)) -> Double
    help nv (_, (ns, ws, _)) = ucb (fromIntegral nv) (fromIntegral ns) (fromIntegral ws)

    loop :: Int -> Int -> Map Move (Int, Int, GameStatistics) -> m (Move, GameStatistics)
    loop 0 nv mvs = 
        let (mv, (_, _, sts)) = Lists.argmax (help nv) $ Map.toList mvs
        in return (mv, sts)
    loop i nv mvs = do
        let (mv, (ns, ws, sts)) = Lists.argmax (help nv) $ Map.toList mvs
        w <- f $ gameStateAppliedToPlayerMove g mv
        loop (i - 1) (nv + 1) $ Map.insert mv (ns + 1, ws + if p == w then 1 else 0, updatedStatistics sts w) mvs

{- must be run in player turn; depth must be odd;  -}
expectimaxn :: forall m. MonadRandom m => (GameState -> m (Move, GameStatistics)) -> (GameState -> GameStatistics) -> GameState -> Int -> m (Move, GameStatistics)
expectimaxn f1 f2 g d = do
    let expectimaxn' :: GameState -> Int -> m (Maybe Move, GameStatistics)
        expectimaxn' g d =
            if terminal g
                then return (Nothing, f2 g)
                else case player g of
                    Nature -> do
                        let mvs = natureMoves g
                            l = fromIntegral $ length mvs

                            help :: GameStatistics -> Int -> m GameStatistics
                            help sts mv =
                                if d == 0
                                    then do
                                        (_, mvsts) <- f1 $ gameStateAppliedToNatureMove g mv
                                        return $ Map.unionWith (\v1 v2 -> v1 + (v2 / l)) sts mvsts
                                    else do
                                        (_, mvsts) <- expectimaxn' (gameStateAppliedToNatureMove g mv) (d - 1)
                                        return $ Map.unionWith (\v1 v2 -> v1 + (v2 / l)) sts mvsts

                        sts <- Foldable.foldlM help (emptyStatistics g) mvs
                        return (Nothing, sts)
                    p -> do
                        let mvs = playerMoves g
                            
                            help :: (Move, GameStatistics) -> Move -> m (Move, GameStatistics)
                            help (bmv, bsts) mv = do
                                (_, mvsts) <- expectimaxn' (gameStateAppliedToPlayerMove g mv) (d - 1)
                                if mvsts ! p > bsts ! p
                                    then return (mv, mvsts)
                                    else return (bmv, bsts)
                        
                        (mv, sts) <- Foldable.foldlM help (voidMove p, emptyStatistics g) mvs
                        return (Just mv, sts)
    (Just mv, sts) <- expectimaxn' g d
    return (mv, sts)

nestedMonteCarlo :: forall m. MonadRandom m => (GameState -> m Player) -> GameState -> Int -> Int -> m (Move, GameStatistics)
nestedMonteCarlo f g d i =
  expectimaxn help1 help2 g d
    where
        help1 :: GameState -> m (Move, GameStatistics)
        help1 g = monteCarlo f g i

        help2 :: GameState -> GameStatistics
        help2 g = let Just w = winner g
                  in statisticsFromPlayer g w $ fromIntegral i

data Tree = PlayerNode Double Double [(Move, Tree)]
          | NatureNode Double Double [(Int, Tree)]
          | Leaf Double Double
    deriving (Eq, Show)

emptyNode :: Tree
emptyNode =
    Leaf 0 0

expandBound :: Double
expandBound = 10

expandedNatureNode :: GameState -> Double -> Double -> Tree
expandedNatureNode g nv wv=
    NatureNode nv wv [(m, Leaf 0 0) | m <- natureMoves g]

expandedPlayerNode :: GameState -> Double -> Double -> Tree
expandedPlayerNode g nv wv =
    PlayerNode nv wv [(m, Leaf 0 0) | m <- playerMoves g]

monteCarloTreeSearch :: forall m. MonadRandom m => (GameState -> m Player) -> GameState -> Int -> m Move
monteCarloTreeSearch f g i = do
    let help :: Double -> (Move, Tree) -> Double
        help nv (_, Leaf ns ws) = ucb nv ns ws
        help nv (_, NatureNode ns ws _) = ucb nv ns ws
        
        monteCarloTreeSearch' :: GameState -> Tree -> Player -> m (Player, Tree)
        monteCarloTreeSearch' g t p =
            if terminal g
                then let Just w = winner g
                     in return (w, t)
                else case player g of
                    Nature ->
                        case t of
                            NatureNode nv wv cdn -> do
                                (mv, ocdt) <- uniform cdn
                                (w, ncdt) <- monteCarloTreeSearch' (gameStateAppliedToNatureMove g mv) ocdt p
                                let nnv = nv + 1
                                    nwv = wv + if p == w then 1 else 0
                                    ncdn = replaceOne (mv, ocdt) (mv, ncdt) cdn
                                return (w, NatureNode nnv nwv ncdn)
                            Leaf nv wv -> do
                                w <- f g
                                let nnv = nv + 1
                                    nwv = wv + if p == w then 1 else 0
                                if nnv > expandBound
                                    then return (w, expandedNatureNode g nnv nwv)
                                    else return (w, Leaf nnv nwv)
                    cp ->
                        case t of
                            PlayerNode nv wv cdn -> do
                                let (mv, ocdt) = argmax (help nv) cdn
                                (w, ncdt) <- monteCarloTreeSearch' (gameStateAppliedToPlayerMove g mv) ocdt cp
                                let nnv = nv + 1
                                    nwv = wv + if cp == w then 1 else 0
                                    ncdn = replaceOne (mv, ocdt) (mv, ncdt) cdn
                                return (w, PlayerNode nnv nwv ncdn)
                            Leaf nv wv -> do
                                w <- f g
                                let nnv = nv + 1
                                    nwv = wv + if cp == w then 1 else 0
                                if nnv > expandBound
                                    then return (w, expandedPlayerNode g nnv nwv)
                                    else return (w, Leaf nnv nwv)

        loop :: Int -> Tree -> m Tree
        loop 0 t = return t
        loop i t = do
            (_, nt) <- monteCarloTreeSearch' g t $ player g
            loop (i - 1) nt

    (PlayerNode nv _ cdn) <- loop i emptyNode {- ! i > expandBound -}
    let (mv, _) = argmax (help nv) cdn
    return mv

data Command = Setup Int [Player]
             | TimeLeft Int
             | Play Player Int Move
             | GenerateMove Player Int
             | Skip Player
             | WTF
    deriving (Show)

command :: String -> Either ParseError Command
command = parse command' ""
  where
    setup = do
        many $ char ' '
        string "setup"
        many $ char ' '
        t <- int -- time
        many $ char ' '
        int -- number of players
        many $ char ' '
        p <- int `Parsec.sepBy` many (char ' ') -- players
        many $ char ' '
        return $ Setup t (List.map toEnum p)

    timeLeft = do
        many $ char ' '
        string "timeleft"
        many $ char ' '
        t <- int
        many $ char ' '
        return $ TimeLeft t

    play = do
        many $ char ' '
        string "play"
        many $ char ' '
        p <- int
        many $ char ' '
        d <- int
        many $ char ' '
        pos <- int
        many $ char ' '
        let (Just src, Just dst) = if pos == 0
                                       then (intToPosition (toEnum p) 0, intToPosition (toEnum p) 1)
                                       else (intToPosition (toEnum p) pos, intToPosition (toEnum p) (pos + d))
        return $ Play (toEnum p) d (src, dst)

    skip = do
        many $ char ' '
        string "skip"
        many $ char ' '
        p <- int
        return $ Skip (toEnum p)

    generateMove = do
        many $ char ' '
        string "genmove"
        many $ char ' '
        p <- int
        many $ char ' '
        d <- int
        many $ char ' '
        return $ GenerateMove (toEnum p) d

    wtf = do
        many anyChar
        return WTF

    command' =   try setup
             <|> try timeLeft
             <|> try play
             <|> try skip
             <|> try generateMove
             <|> try wtf

respond :: Maybe Int -> IO ()
respond Nothing = do
    putStrLn "="
    putStrLn ""
respond (Just i) = do
    putStrLn $ "= " ++ show i
    putStrLn ""


scrolledToPlayer :: GameState -> Player -> GameState
scrolledToPlayer g p =
    if p == player g
        then g
        else scrolledToPlayer (g { queue = tail $ queue g }) p

startGameState :: [Player] -> GameState
startGameState ps = GameState {
    queue = cycle $ Nature : List.intersperse Nature ps,
    players = Map.fromList [(p, replicate 4 (Base p)) | p <- ps],
    die = Nothing,
    trials = 0
}

mainLoop :: GameState -> Player -> Int -> IO ()
mainLoop g prev t = do
    line <- getLine
    let Right c = command line
    case c of
        TimeLeft _ -> do
            respond Nothing
            mainLoop g prev t
        Skip p -> do
            respond Nothing
            mainLoop g p 0
        Play p d mv -> do
            let g1 = scrolledToPlayer g p
                g2 = g1 { die = Just d, trials = 1 }
                g3 = gameStateAppliedToPlayerMove g2 mv
                nt = if p == prev
                         then t - 1
                         else if d == 6 then 2 else 0
            respond Nothing
            mainLoop g3 p nt
        GenerateMove p d -> do
            let g1 = scrolledToPlayer g p
                g2 = g1 { die = Just d, trials = (if prev == p
                                                      then t
                                                      else if d == 6 then 3 else 1) }
            (src, dst) <- monteCarloTreeSearch prefereBeatPlayout g2 6000
            respond $ Just (positionToInt src)
            mainLoop g prev t
        WTF -> do
            respond Nothing
            mainLoop g prev t

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering
    line <- getLine
    let Right (Setup _ ps) = command line
        g = startGameState ps
    respond Nothing
    mainLoop g UnexistingPlayer 1