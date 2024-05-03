-- |
-- This module defines the logic of the game and the communication with the `Board.RenderState`
module GameState where

-- These are all the imports. Feel free to use more if needed.

import Data.Foldable (toList)
import Data.Sequence (Seq (..))
import qualified Data.Sequence as S
import RenderState (BoardInfo (..), Point)
import qualified RenderState as Board
import System.Random (RandomGen (split), StdGen, uniformR)

-- The movement is one of this.
data Movement = North | South | East | West deriving (Show, Eq)

-- | The snakeSeq is a non-empty sequence. It is important to use precise types in Haskell
--   In first sight we'd define the snake as a sequence, but If you think carefully, an empty
--   sequence can't represent a valid Snake, therefore we must use a non empty one.
--   You should investigate about Seq type in haskell and we it is a good option for our purpose.
data SnakeSeq = SnakeSeq {snakeHead :: Point, snakeBody :: Seq Point} deriving (Show, Eq)

-- | The GameState represents all important bits in the game. The Snake, The apple, the current direction of movement and
--   a random seed to calculate the next random apple.
data GameState = GameState
  { snakeSeq :: SnakeSeq,
    applePosition :: Point,
    movement :: Movement,
    randomGen :: StdGen
  }
  deriving (Show, Eq)

-- | This function should calculate the opposite movement.
oppositeMovement :: Movement -> Movement
oppositeMovement North = South
oppositeMovement South = North
oppositeMovement East = West
oppositeMovement West = East

-- >>> oppositeMovement North == South
-- >>> oppositeMovement South == North
-- >>> oppositeMovement East == West
-- >>> oppositeMovement West == East

-- | Purely creates a random point within the board limits
--   You should take a look to System.Random documentation.
--   Also, in the import list you have all relevant functions.
makeRandomPoint :: BoardInfo -> StdGen -> (Point, StdGen)
-- makeRandomPoint boardInfo rand = ((randomR 1  . height boardInfo, randomR 1 width boardInfo), rand)
makeRandomPoint (BoardInfo h w) rand = (generatedPoint, updatedGenerator)
  where
    (randGen1, randGen2) = split rand
    (randomHeight, randGen1') = uniformR (1, h) randGen1
    (randomWidth, _) = uniformR (1, w) randGen2
    generatedPoint = (randomHeight, randomWidth)
    updatedGenerator = randGen1'

{-
We can't test makeRandomPoint, because different implementation may lead to different valid result.
-}

-- | Check if a point is in the snake
-- recurse over the snake to see if
-- the point exists in that snake
inSnake :: Point -> SnakeSeq -> Bool
inSnake _ (SnakeSeq {snakeBody = Empty}) = False
inSnake point (SnakeSeq {snakeHead = x, snakeBody = (y :<| ys)})
  | point == x = True
  | point == y = True
  | otherwise = inSnake point SnakeSeq {snakeHead = x, snakeBody = ys}

{-
This is a test for inSnake. It should return
True
True
False
-}
-- >>> snake_seq = SnakeSeq (1,1) (Data.Sequence.fromList [(1,2), (1,3)])
-- >>> inSnake (1,1) snake_seq
-- >>> inSnake (1,2) snake_seq
-- >>> inSnake (1,4) snake_seq
-- True
-- True
-- False

tryMovement :: Movement -> Point -> Point
tryMovement
  m
  (y, x) = case m of
    North -> (y - 1, x)
    South -> (y + 1, x)
    West -> (y, x - 1)
    East -> (y, x + 1)

data Side = TopSide | BottomSide | LeftSide | RightSide

data ProposedMove = Inner | WrapsTo Side

handleMovement :: BoardInfo -> Point -> Point -> ProposedMove
handleMovement (BoardInfo {height = boardH, width = boardW}) (minH, minW) (y, x)
  | y > boardH = WrapsTo TopSide
  | x > boardW = WrapsTo LeftSide
  | y < minH = WrapsTo BottomSide
  | x < minW = WrapsTo RightSide
  | otherwise = Inner

moveSnake :: BoardInfo -> Movement -> Point -> Point
moveSnake boardInfo currentMovement snakePoint =
  let (newY, newX) = tryMovement currentMovement snakePoint
   in case handleMovement boardInfo (1, 1) (newY, newX) of
        Inner -> (newY, newX)
        WrapsTo TopSide -> (1, newX)
        WrapsTo BottomSide -> (height boardInfo, newX)
        WrapsTo LeftSide -> (newY, 1)
        WrapsTo RightSide -> (newY, width boardInfo)

-- | Calculates de new head of the snake. Considering it is moving in the current direction
--   Take into account the edges of the board
nextHead :: BoardInfo -> GameState -> Point
nextHead boardInfo gameState =
  -- let currentMovement = movement gameState
  --     currentHead = snakeHead $ snakeSeq gameState
  --     (newY, newX) = tryMovement currentMovement currentHead
  moveSnake boardInfo (movement gameState) (snakeHead (snakeSeq gameState))

{-
This is a test for nextHead. It should return
True
True
True
-}
-- >>> snake_seq = SnakeSeq (1,1) (Data.Sequence.fromList [(1,2), (1,3)])
-- >>> apple_pos = (2,2)
-- >>> board_info = BoardInfo 4 4
-- >>> game_state1 = GameState snake_seq apple_pos West (System.Random.mkStdGen 1)
-- >>> game_state2 = GameState snake_seq apple_pos South (System.Random.mkStdGen 1)
-- >>> game_state3 = GameState snake_seq apple_pos North (System.Random.mkStdGen 1)
-- >>> nextHead board_info game_state1 == (1,4)
-- >>> nextHead board_info game_state2 == (2,1)
-- >>> nextHead board_info game_state3 == (4,1)
-- True
-- True
-- True

-- | Calculates a new random apple, avoiding creating the apple in the same place, or in the snake body
newApple :: BoardInfo -> GameState -> (Point, StdGen)
-- app@ is what's called a "named pattern" and allows us to pattern match/destructure and give a name that we can use later, like in the `then`
newApple boardInfo app@(GameState snakeSequence currentApple _move randomGenerator) =
  if proposedApple == currentApple || proposedApple `inSnake` snakeSequence
    then newApple boardInfo app {randomGen = updatedRand}
    else (proposedApple, updatedRand)
  where
    (proposedApple, updatedRand) = makeRandomPoint boardInfo randomGenerator

{- We can't test this function because it depends on makeRandomPoint -}

-- >>> snake_seq = SnakeSeq (1,1) (Data.Sequence.fromList [(1,2), (1,3)])
-- >>> apple_pos = (2,2)
-- >>> board_info = BoardInfo 4 4
-- >>> game_state1 = GameState snake_seq apple_pos West (System.Random.mkStdGen 1)
-- >>> isMoveIntoSnake (1, 2) game_state1
-- >>> isMoveIntoSnake (4, 2) game_state1
-- GameOver
-- RenderBoard [((1,1),Empty)]

toSnakeBody :: Point -> (Point, Board.CellType)
toSnakeBody point = (point, Board.Snake)

moveSnakeBody :: BoardInfo -> GameState -> Board.DeltaBoard
moveSnakeBody boardInfo gameState = toList . fmap (toSnakeBody . moveSnake boardInfo (movement gameState)) $ snakeBody $ snakeSeq gameState

-- | Moves the snake based on the current direction. It sends the adequate RenderMessage
-- Notice that a delta board must include all modified cells in the movement.
-- For example, if we move between this two steps
--        - - - -          - - - -
--        - 0 $ -    =>    - - 0 $
--        - - - -    =>    - - - -
--        - - - X          - - - X
-- We need to send the following delta: [((2,2), Empty), ((2,3), Snake), ((2,4), SnakeHead)]
--
-- Another example, if we move between this two steps
--        - - - -          - - - -
--        - - - -    =>    - X - -
--        - - - -    =>    - - - -
--        - 0 $ X          - 0 0 $
-- We need to send the following delta: [((2,2), Apple), ((4,3), Snake), ((4,4), SnakeHead)]
move :: BoardInfo -> GameState -> (Board.RenderMessage, GameState)
move bi s@(GameState (SnakeSeq oldHead sb) applePos _ _g) =
  if isCollision
    then (Board.GameOver, s)
    else case isEatingApple of
      True ->
        case sb of
          S.Empty ->
            let newSnake = SnakeSeq newHead (S.singleton oldHead)
                newState = s {snakeSeq = newSnake, applePosition = newApplePos, randomGen = g'}
                delta = [(newHead, Board.SnakeHead), (oldHead, Board.Snake), (newApplePos, Board.Apple)]
             in (Board.RenderBoard delta, newState)
          xs ->
            let newSnake = SnakeSeq newHead (oldHead :<| xs)
                newState = s {snakeSeq = newSnake, applePosition = newApplePos, randomGen = g'}
                delta = [(newHead, Board.SnakeHead), (oldHead, Board.Snake), (newApplePos, Board.Apple)]
             in (Board.RenderBoard delta, newState)
      False ->
        case sb of
          S.Empty ->
            let newSnake = SnakeSeq newHead S.empty
                newState = s {snakeSeq = newSnake}
                delta = [(newHead, Board.SnakeHead), (oldHead, Board.Empty)]
             in (Board.RenderBoard delta, newState)
          x :<| S.Empty ->
            let newSnake = SnakeSeq newHead (S.singleton oldHead)
                newState = s {snakeSeq = newSnake}
                delta = [(newHead, Board.SnakeHead), (oldHead, Board.Snake), (x, Board.Empty)]
             in (Board.RenderBoard delta, newState)
          x :<| (xs :|> t) ->
            let newSnake = SnakeSeq newHead (oldHead :<| x :<| xs)
                newState = s {snakeSeq = newSnake}
                delta = [(newHead, Board.SnakeHead), (oldHead, Board.Snake), (t, Board.Empty)]
             in (Board.RenderBoard delta, newState)
  where
    newHead = nextHead bi s
    isCollision = newHead `elem` sb
    isEatingApple = newHead == applePos
    (newApplePos, g') = newApple bi s
{- This is a test for move. It should return

RenderBoard [((1,4),SnakeHead),((1,1),Snake),((1,3),Empty)]
RenderBoard [((2,1),SnakeHead),((1,1),Snake),((3,1),Apple)] ** your Apple might be different from mine
RenderBoard [((4,1),SnakeHead),((1,1),Snake),((1,3),Empty)]

-}

-- >>> snake_seq = SnakeSeq (1,1) (Data.Sequence.fromList [(1,2), (1,3)])
-- >>> apple_pos = (2,1)
-- >>> board_info = BoardInfo 4 4
-- >>> game_state1 = GameState snake_seq apple_pos West (System.Random.mkStdGen 1)
-- >>> game_state2 = GameState snake_seq apple_pos South (System.Random.mkStdGen 1)
-- >>> game_state3 = GameState snake_seq apple_pos North (System.Random.mkStdGen 1)
-- >>> fst $ move board_info game_state1
-- >>> fst $ move board_info game_state2
-- >>> fst $ move board_info game_state3
-- RenderBoard [((1,4),SnakeHead),((1,1),Snake),((1,3),Empty)]
-- RenderBoard [((2,1),SnakeHead),((1,1),Snake),((3,1),Apple)]
-- RenderBoard [((4,1),SnakeHead),((1,1),Snake),((1,3),Empty)]
