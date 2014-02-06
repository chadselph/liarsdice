import System.Random
import Control.Monad.State
import Data.Maybe (fromJust)
import Data.List (elemIndex)

--data Face = Ones | Twos | Threes | Fours | Fives | Sixes deriving (Show, Read, Eq, Ord)
data Move = Call | Bid { count :: Int, face :: Int } deriving (Show, Read, Eq)

type Hand = [Int]

data Player = Player {
  hand :: Hand,
  name :: String
  } deriving (Show, Eq)

newtype PlayerCollection = PlayerCollection [Player]

data GameState = GameState { lastMove :: Maybe Move, onesWild :: Bool, players :: PlayerCollection, stdGen :: StdGen } deriving (Show)
data MoveResult = Ok | InvalidMove String | NextRound String | GameOver String deriving (Show)
data CallResult = InsufficientDice | SufficientDice

instance Ord Move where
  (Bid countLeft faceLeft) `compare` (Bid countRight faceRight)
    | countLeft > countRight = GT
    | countRight > countLeft = LT
    | otherwise = faceLeft `compare` faceRight
  -- probably never used...
  Call `compare` Call = EQ
  Call `compare` _ = GT
  _ `compare` Call = LT

instance Show PlayerCollection  where
  show players =  "{Up=" ++ show (up players) ++ " Prev=" ++ show (upPrev players) ++ " Next=" ++ show (upNext players) ++ "}"

upPrev :: PlayerCollection -> Player
upPrev (PlayerCollection (p:_)) = p

up :: PlayerCollection -> Player
up (PlayerCollection (_:t:_)) = t

upNext :: PlayerCollection -> Player
upNext (PlayerCollection (_:_:n:_)) = n

succ :: PlayerCollection -> PlayerCollection
succ (PlayerCollection players) = PlayerCollection (tail players)

playersRemaining :: PlayerCollection -> Int
playersRemaining (PlayerCollection (p:ps)) = fromJust . elemIndex p $ ps

-- constants
maxDie = 6
minDie = 1
startHandSize = 5

newHand :: Hand
newHand = [1,2,3,4,5]

type GeneratorState = State StdGen

rollDie :: GeneratorState Int
rollDie = do generator <- get
             let (value, newGenerator) = randomR (minDie, maxDie) generator
             put newGenerator
             return value

rollDice :: Int -> GeneratorState Hand
rollDice 0 = return []
rollDice n = liftM2 (:) rollDie (rollDice (n - 1))



--rollDice :: Hand -> Int -> StdGen -> (Hand, StdGen)
--rollDice accum 0 g = (accum, g)
--rollDice accum count g = let (num, g') = randomR (minDie ,maxDie) g
--                             in rollDice (num:accum) (pred count) g'

--rollDicePlayer :: Player -> StdGen -> (Player, StdGen)
--rollDicePlayer player g = let (hand', g') = rollDice [] handsize g
--                              in (player{hand = hand'}, g')
--                              where handsize = length (hand player)
                                      --
rollDicePlayer :: Player -> GeneratorState Player
rollDicePlayer player = do
  hand' <- rollDice(length (hand player))
  return $ player {hand = hand'}

--rollAllPlayersDice' :: [Player] -> StdGen -> ([Player], StdGen)
--rollAllPlayersDice' [] g = ([], g)
--rollAllPlayersDice' (p:ps) g = let (p', g') = rollDicePlayer p g
--                                  in let (ps', g'') = rollAllPlayersDice' ps g'
--                                         in (p':ps', g'')

rollAllPlayersDice :: PlayerCollection -> StdGen -> (PlayerCollection, StdGen)
rollAllPlayersDice = undefined



newGame :: [Player] -> StdGen -> GameState
newGame players stdGen = nextRound GameState {lastMove = Nothing, onesWild = True, players = PlayerCollection (cycle players), stdGen = stdGen}

newPlayer :: String -> Player
newPlayer name = Player { hand = newHand, name = name}

atLeastNOf 0 _ _ = True
atLeastNOf _ _ [] = False
atLeastNOf n predicate (head:tail)
  | predicate head = atLeastNOf (pred n) predicate tail
  | otherwise = atLeastNOf n predicate tail

evaluateLastBid game @ GameState{lastMove = Just guess @ Bid{}, onesWild=onesWild, players=(PlayerCollection players)} =
  case evaluate guess onesWild (players >>= hand) of
    InsufficientDice -> (NextRound "insufficient dice. bidder loses", nextRound game)
    SufficientDice -> (NextRound "enough dice for bid", nextRound game)
    where 
      evaluate Bid {count=count, face=face} onesWild dice
          | atLeastNOf count predicate dice = SufficientDice
          | otherwise = InsufficientDice
          where predicate x = x == face || (onesWild && x == 1)

-- XXX: placeholder for where loser will drop 1 die and be filtered out.
nextRound :: GameState -> GameState
nextRound game @ GameState{stdGen = stdGen, players = players} =
  let (newPlayers, newRand) = rollAllPlayersDice players stdGen
      in game {players = newPlayers, onesWild = True, lastMove = Nothing, stdGen = newRand}

nextTurn :: GameState -> GameState
nextTurn game =
  checkOnesWild . turn $ game
  where
    checkOnesWild g @ GameState{lastMove = Just Bid{face=1}, onesWild = True} = g{onesWild = False}
    checkOnesWild g = g
-- XXX: placeholder for where we decide whose turn it is
    turn game = game

makeMove :: GameState -> Move -> (MoveResult, GameState)
makeMove game @ GameState {lastMove = Nothing} Call = (InvalidMove "Cannot call before a move has been played.", game)
makeMove game Call = evaluateLastBid game
makeMove game @ GameState {lastMove = Nothing} guess = (Ok, nextTurn game{lastMove=Just guess})
makeMove game @ GameState {lastMove = Just lastBid @ Bid{}} guess 
  | guess > lastBid = (Ok, nextTurn game{lastMove=Just guess})
  | otherwise = (InvalidMove "Bid must be higher than last bid", game)


-- Example usage: Command line interface

main :: IO ()
main = do g <- newStdGen
          let game = newGame [newPlayer "chad", newPlayer "elaine", newPlayer "kelvin"] g
              in playGame game

getMoveFromStdin :: IO Move
getMoveFromStdin = do
  putStr "> "
  inputStr <- getLine
  return (read inputStr :: Move)

playGame game = do print game
                   move <- getMoveFromStdin
                   let (result, nGame) = makeMove game move
                       in do
                         print result
                         playGame nGame

