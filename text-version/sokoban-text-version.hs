{-# LANGUAGE OverloadedStrings #-}

import System.IO
type Program = IO ()


-------------
-- data types
-------------

data Tile = Wall | Ground | Storage | Box | Blank deriving (Eq, Show)

data Direction = R | U | L | D deriving Eq

data Coord = C Int Int deriving (Eq, Show)

type MazeMap = Coord -> Tile

data Maze = Maze {
  mInitialCoord :: Coord,
  mMazeMap      :: MazeMap
}

data State = S {
  stPlayer          :: Coord,
  stDir             :: Direction,
  stBoxes           :: [Coord],
  stMaze            :: MazeMap,
  stLevel           :: Integer,
  stMove            :: Integer,
  stReachableCoords :: [Coord],
  stMazeSize        :: Int,
  stGameEnded       :: Bool
}

-- not comparing maze map
instance Eq State where
  s1 == s2 = stPlayer s1             == stPlayer s2
             && stDir s1             == stDir s2
             && stBoxes s1           == stBoxes s2
             && stLevel s1           == stLevel s2
             && stMove s1            == stMove s2
             && stReachableCoords s1 == stReachableCoords s2
             && stMazeSize s1        == stMazeSize s2
             && stGameEnded s1       == stGameEnded s2

data Activity world = Activity
    world
    (Event -> world -> world)
    (world -> Screen)
    
data Event = KeyPress String

-- terminal window: 23 lines, 79 characters each
type Screen = String
    
data SSState world = StartScreen | Running world deriving Eq

data WithUndo a = WithUndo a [a]

type DrawFun = Integer -> Integer -> Char

type Picture = DrawFun -> DrawFun


---------------
-- main program
---------------

main :: Program
main = program

program :: Program
program = etap5

etap5 :: IO ()
etap5 = runActivity ((resettable . withUndo . withStartScreen) defaultActivity)

runActivity :: Activity s -> IO ()
runActivity (Activity state0 handle draw) = do
  hSetBuffering stdin NoBuffering
  hSetBuffering stdout NoBuffering
  clearScreen
  putStrLn (draw state0)
  input <- getContents
  go state0 input
  where
    -- quit command
    go _ ('q':_) = pure()
    go _ ('Q':_) = pure()
    -- arrows
    go state ('\ESC':'[':'A':xs) = go' state "Up" xs
    go state ('\ESC':'[':'B':xs) = go' state "Down" xs
    go state ('\ESC':'[':'D':xs) = go' state "Left" xs
    go state ('\ESC':'[':'C':xs) = go' state "Right" xs
    -- other keys
    go state (x:xs) = go' state [x] xs
    go' state key xs = do
      let updatedState = handle (KeyPress key) state
      clearScreen
      putStrLn (draw updatedState)
      go updatedState xs

-- clears screen and makes cursor invisible
clearScreen :: IO ()
clearScreen = putStr "\ESCc" >> putStr "\ESC[?25l"

defaultActivity :: Activity State
defaultActivity = Activity initial handleEvent draw

startScreen :: Screen
startScreen = "Sokoban!\n" ++ "Wciśnij spację, żeby rozpocząć\n"
              ++ "Wciśnij 'Q', żeby wyjść z gry"
    
withStartScreen :: Activity s -> Activity (SSState s)
withStartScreen (Activity state0 handle draw)
  = Activity state0' handle' draw'
  where
    state0' = StartScreen

    handle' (KeyPress key) StartScreen
         | key == " "                  = Running state0
    handle' _              StartScreen = StartScreen
    handle' e              (Running s) = Running (handle e s)

    draw' StartScreen = startScreen
    draw' (Running s) = draw s
    
-- ESC has to be pressed twice :(
resettable :: Activity s -> Activity s
resettable (Activity state0 handle draw)
  = Activity state0 handle' draw
  where handle' (KeyPress key) _ | key == "\ESC" = state0
        handle' e s = handle e s

withUndo :: Eq a => Activity a -> Activity (WithUndo a)
withUndo (Activity state0 handle draw) = Activity state0' handle' draw' where
    state0' = WithUndo state0 []
    handle' (KeyPress key) (WithUndo s stack) | key == "U" || key == "u"
      = case stack of s':stack' -> WithUndo s' stack'
                      []        -> WithUndo s []
    handle' e              (WithUndo s stack)
       | s' == s = WithUndo s stack
       | otherwise = WithUndo (handle e s) (s:stack)
      where s' = handle e s
    draw' (WithUndo s _) = draw s


----------------
-- initial state
----------------

initial :: State
initial = initialState

initialState :: State
initialState = S {
    stPlayer          = mInitialCoord initialMaze,
    stDir             = initialDirection,
    stBoxes           = initialReachableBoxes initialMaze,
    stMaze            = mMazeMap initialMaze,
    stLevel           = 1,
    stMove            = 0,
    stReachableCoords = reachableCoords initialMaze,
    stMazeSize        = findMazeSize initialMaze,
    stGameEnded       = False
  }
  where initialMaze = head mazes

initialCoord :: Coord
initialCoord = C (-3) 3

initialDirection :: Direction
initialDirection = D

initialReachableBoxes :: Maze -> [Coord]
initialReachableBoxes maze = reachableTiles maze Box

-- find size of given maze
findMazeSize :: Maze -> Int
findMazeSize maze = findSize 2
  where
    {- check if outline of square determined by coords
    (-size, -size), (-size, size), (size, -size), (size, size)
    is all blanks - if so, that means the whole maze is inside it
    and so the size is correct -}
    findSize size
      | allList isBlank coords = size
      | otherwise              = findSize (2 * size)
        where
          coords = [ C x y | x <- [-size .. size], y <- [-size .. size],
                             abs x == size || abs y == size ]
          isBlank :: Coord -> Bool
          isBlank = (== Blank) . (mMazeMap maze)


---------
-- handle
---------

handleEvent :: Event -> State -> State
handleEvent (KeyPress key) state
  | key == "N" || key == "n" = goToNextLevelIfPossible state
  | isWinning state = state
  | key == "Right" || key == "D" || key == "d" = go R 
  | key == "Up"    || key == "W" || key == "w" = go U
  | key == "Left"  || key == "A" || key == "a" = go L
  | key == "Down"  || key == "S" || key == "s" = go D
  where
    go direction = tryToMove direction state
handleEvent _ state = state

isWinning :: State -> Bool
isWinning state = andList (map isStorage (stBoxes state))
  where
    isStorage :: Coord -> Bool
    isStorage = (== Storage) . stMaze state

goToNextLevelIfPossible :: State -> State
goToNextLevelIfPossible state
  | stLevel state < listLength mazes = goToNextLevel state
  | otherwise                        = state { stGameEnded = True }

goToNextLevel :: State -> State
goToNextLevel state = state {
    stPlayer          = mInitialCoord nextMaze,
    stDir             = initialDirection,
    stBoxes           = initialReachableBoxes nextMaze,
    stMaze            = mMazeMap nextMaze,
    stLevel           = nextLevel,
    stMove            = 0,
    stReachableCoords = reachableCoords nextMaze,
    stMazeSize        = findMazeSize nextMaze
  }
  where
    nextLevel = (stLevel state) + 1
    nextMaze = nth mazes nextLevel
    
tryToMove :: Direction -> State -> State
tryToMove direction state
  | isABox targetPosition && isSteppable behindTargetPosition updatedMaze =
      -- move player and box
      state { stPlayer = targetPosition,
              stDir = direction,
              stBoxes = map moveBox (stBoxes state),
              stMove = stMove state + 1 }
  | isSteppable targetPosition updatedMaze =
      -- move player
      state { stPlayer = targetPosition,
              stDir = direction,
              stMove = stMove state + 1 }
  | otherwise =
      -- incorrect move, only player direction changes
      state { stDir = direction }
  where
    updatedMaze = updateBoxes (stBoxes state) (stMaze state)
    
    targetPosition = adjacentCoord direction (stPlayer state)
    behindTargetPosition = adjacentCoord direction targetPosition
    
    isABox :: Coord -> Bool
    isABox = (== Box) . updatedMaze
    
    moveBox :: Coord -> Coord
    moveBox = \c -> if c == targetPosition then behindTargetPosition else c
    
adjacentCoord :: Direction -> Coord -> Coord
adjacentCoord direction (C x y) =
  case direction of
    R -> C (x + 1) y
    U -> C x (y + 1)
    L -> C (x - 1) y
    D -> C x (y - 1)

-- returns true if player is allowed to enter tile at given coordinate
isSteppable :: Coord -> MazeMap -> Bool
isSteppable coord maze = (\t -> t == Ground || t == Storage) $ maze coord

updateBoxes :: [Coord] -> MazeMap -> MazeMap
updateBoxes boxes maze = addBoxes boxes (removeBoxes maze)

removeBoxes :: MazeMap -> MazeMap
removeBoxes maze = boxToGround . maze
  where
    boxToGround :: Tile -> Tile
    boxToGround tile = if tile == Box then Ground else tile
    
addBoxes :: [Coord] -> MazeMap -> MazeMap
addBoxes coords maze = maze'
  where
    maze' coord
      | elemList coord coords = Box
      | otherwise             = maze coord
      

-------
-- draw
-------

draw :: State -> Screen
draw state
  | stGameEnded state = endScreen
  | isWinning state   = winScreen state
  | otherwise         = drawPicture (drawBoard state)

endScreen :: Screen
endScreen = "Wszystkie poziomy ukończone!\n"
            ++ "Wciśnij dwukrotnie ESC, żeby zagrać ponownie"

winScreen :: State -> Screen
winScreen state = "Poziom ukończony, liczba ruchów: " ++ show (stMove state)
                  ++ "\n" ++ "Wciśnij 'N', żeby przejść do kolejnego poziomu"
    
drawPicture :: Picture -> Screen
drawPicture picture = [ render x y | y <- [11,10..(-11)], x <- [-39..39] ]
  where
    render :: Integer -> Integer -> Char
    -- last character in each line is newline
    render 39 _ = '\n'
    render x y = picture (\_ _ -> ' ') x y

-- draws player, reachable boxes and the rest of maze separately
drawBoard :: State -> Picture
drawBoard state = atCoord (stPlayer state) player
                  & pictureOfBoxes (stBoxes state) mazeWithoutBoxes
                  & pictureOfMaze mazeWithoutBoxes (stMazeSize state)
  where mazeWithoutBoxes =
          removeReachableBoxes (stReachableCoords state) (stMaze state)

{- i assume we still want to draw the unreachable boxes
if we just use the removeBoxes function, they will just become blanks
(as they don't have equivalents in stBoxes), see: oneBoxUnreachable level -}
removeReachableBoxes :: [Coord] -> MazeMap -> MazeMap
removeReachableBoxes reachableCoords maze = maze'
  where
    maze' coord
      | maze coord == Box && elemList coord reachableCoords = Ground
      | otherwise                                           = maze coord


-----------
-- pictures
-----------

translated :: Integer -> Integer -> Picture -> Picture
translated dx dy picture = revertCoords & picture & changeCoords
  where
    changeCoords :: Picture
    changeCoords drawFun = \x y -> drawFun (x + dx) (y + dy)
    
    revertCoords :: Picture
    revertCoords drawFun = \x y -> drawFun (x - dx) (y - dy)

atCoord :: Coord -> Picture -> Picture
atCoord (C x y) pic = translated (fromIntegral x) (fromIntegral y) pic

player :: Picture
player = pictureGenerator '@'

pictures :: [Picture] -> Picture
pictures list = foldList (&) blank list

blank :: Picture
blank = id

(&) :: Picture -> Picture -> Picture
(&) = (.)
 
pictureOfBoxes :: [Coord] -> MazeMap -> Picture
pictureOfBoxes coords maze =
  pictures([ atCoord coord (boxType coord) | coord <- coords ])
  where
    {- returns type of box that should be drawn at given coordinate based on
    whether it is in the correct place (storage) -}
    boxType :: Coord -> Picture
    boxType coord
      | maze coord == Storage = correctBox
      | otherwise             = box

pictureOfMaze :: MazeMap -> Int -> Picture
pictureOfMaze maze mazeSize =
  pictures([ let
               coord = C x y
             in atCoord coord (drawTile (maze coord))
             | x <- [-mazeSize .. mazeSize], y <- [-mazeSize .. mazeSize] ])

drawTile :: Tile -> Picture
drawTile Wall    = wall
drawTile Ground  = ground
drawTile Storage = storage
drawTile Box     = box
drawTile Blank   = blank

-- characters according to http://www.sokobano.de/wiki/index.php?title=Level_format
wall, ground, storage, box, correctBox :: Picture

wall = pictureGenerator '#'

ground = pictureGenerator ' '

storage = pictureGenerator '.'

box = pictureGenerator '$'

correctBox = pictureGenerator '*'

pictureGenerator :: Char -> Picture
pictureGenerator char _ 0 0       = char
pictureGenerator char drawFun x y = drawFun x y


------------------
-- graph functions
------------------

{- bfs algorithm, returns list of visited vertices
the arguments are: neighbours function, list of vertices to be visited,
list of visited vertices -}
bfs :: Eq a => (a -> [a]) -> [a] -> [a] -> [a]
bfs neighbours [] visited = visited
bfs neighbours (x:xs) visited
  | elemList x visited = bfs neighbours xs visited
  | otherwise          = bfs neighbours (appendList xs (neighbours x)) (x:visited)

-- returns list of reachable vertices from initial vertex, given neighbours function
reachableVertices :: Eq a => a -> (a -> [a]) -> [a]
reachableVertices initial neighbours = bfs neighbours [initial] []

isGraphClosed :: Eq a => a -> (a -> [a]) -> (a -> Bool) -> Bool
isGraphClosed initial neighbours isOk =
  allList isOk (reachableVertices initial neighbours)

reachable :: Eq a => a -> a -> (a -> [a]) -> Bool
reachable v initial neighbours = elemList v (reachableVertices v neighbours)

allReachable :: Eq a => [a] -> a -> (a -> [a]) -> Bool
allReachable vs initial neighbours = allList (\v -> reachable v initial neighbours) vs

{- if coord is blank we assume it has no neighbours
this way if a maze is open (see: openMaze level) isClosed will not loop
otherwise bfs will never stop as the list of vertices to be visited will
be infinite - filled with blanks reachable from the "hole" in the maze -}
neighbours :: MazeMap -> Coord -> [Coord]
neighbours maze coord
  | isBlank coord = []
  | otherwise     = filter isNotWall (allAdjacentCoords coord)
  where
    isBlank :: Coord -> Bool
    isBlank = (== Blank) . maze
    
    isNotWall :: Coord -> Bool
    isNotWall = (/= Wall) . maze

allAdjacentCoords :: Coord -> [Coord]
allAdjacentCoords coord = [ adjacentCoord direction coord
                            | direction <- [R, U, L, D] ]

isClosed :: Maze -> Bool
isClosed (Maze initialCoord maze) =
  isSteppable initialCoord maze
  && isGraphClosed initialCoord (neighbours maze) isNotBlank
  where
    isNotBlank :: Coord -> Bool
    isNotBlank = (/= Blank) . maze

-- returns list of reachable coords from initial coord in given maze
reachableCoords :: Maze -> [Coord]
reachableCoords (Maze initialCoord maze) =
  reachableVertices initialCoord (neighbours maze)

-- returns a list of reachable coords with specific tiles in given maze
reachableTiles :: Maze -> Tile -> [Coord]
reachableTiles maze tile = filterList isTile (reachableCoords maze)
  where
    isTile :: Coord -> Bool
    isTile = (== tile) . (mMazeMap maze)

isSane :: Maze -> Bool
isSane maze = numberOfReachableStorages >= numberOfReachableBoxes
  where
    numberOfReachableStorages = listLength (reachableTiles maze Storage)
    numberOfReachableBoxes = listLength (reachableTiles maze Box)


-----------------
-- list functions
-----------------

elemList :: Eq a => a -> [a] -> Bool
elemList element list = foldList elementFound False list
  where elementFound currentElement foundBefore =
          currentElement == element || foundBefore

appendList :: [a] -> [a] -> [a]
appendList firstList secondList = foldList append firstList secondList
  where append element list = list ++ [element]

listLength :: [a] -> Integer
listLength list = foldList calculateLength 0 list
  where calculateLength _ previousLength = previousLength + 1
  
filterList :: (a -> Bool) -> [a] -> [a]
filterList condition list = foldList appendIf [] list
  where appendIf element previousList
          | condition element = previousList ++ [element]
          | otherwise         = previousList

nth :: [a] -> Integer -> a
nth [] _ = error "nth: empty list"
nth (x:xs) n
  | n <= 0                = error "nth: index too small"
  | n > listLength (x:xs) = error "nth: index too large"
  | otherwise             = snd (foldList findNth (1, x) (x:xs))
    where
      {- pair is (current iterator i, nth or previous element if i <= n) 
      if i <= n then the second element is previous element, so we should
      overwrite it using the current element
      if i > n then the second element is the nth element, so we should
      pass it on -}
      findNth currentElement (i, nthOrPrevious)
        | i <= n    = (i + 1, currentElement)
        | otherwise = (i + 1, nthOrPrevious)

mapList :: (a -> b) -> [a] -> [b]
mapList mapping list = foldList mapAndAppend [] list
  where mapAndAppend element previousList = previousList ++ [mapping element]

andList :: [Bool] -> Bool
andList list = foldList (&&) True list
  
allList :: (a -> Bool) -> [a] -> Bool
allList condition list = foldList allMeetTheCondition True list
  where allMeetTheCondition currentElement metBefore =
          condition currentElement && metBefore

-- fold left because it makes nth implementation easier
foldList :: (a -> b -> b) -> b -> [a] -> b
foldList function accumulator [] = accumulator
foldList function accumulator (x:xs) = foldList function (function x accumulator) xs


---------
-- levels
---------

mazes :: [Maze]
mazes = [sampleMaze, microcosmos01, oneBoxUnreachable, easy_spiralMaze_DM,
         medium_maze3_GN, medium_maze4_GN, hard_maze2_GN]

badMazes :: [Maze]
badMazes = [openMaze, badTestMaze_BS, cutOffStorageMaze_DM]

-- sample maze from repo
sampleMaze :: Maze
sampleMaze = Maze start maze
  where
    start = C (-3) 3
    maze (C x y)
      | abs x > 4  || abs y > 4  = Blank
      | abs x == 4 || abs y == 4 = Wall
      | x ==  2 && y <= 0        = Wall
      | x ==  3 && y <= 0        = Storage
      | x >= -2 && y == 0        = Box
      | otherwise                = Ground

-- mazes implemented by me

{- Microcosmos 01 level, solvable
source: http://www.sneezingtiger.com/sokoban/levelpics/microcosmosImages.html -}
microcosmos01 :: Maze
microcosmos01 = Maze start maze
  where
    start = C 0 0
    maze (C x y)
      | abs x > 4  || abs y > 3  = Blank
      | abs x > 2  && y < -1     = Blank
      | x == 0     && y == 3     = Blank
      | abs x == 4 || abs y == 3 = Wall
      | abs x <= 1 && y == 2     = Wall
      | abs x >= 2 && y == -1    = Wall
      | abs x == 2 && y == -2    = Wall
      | x == 0     && abs y >= 1 = Storage
      | abs x == 2 && y == 1     = Box
      | x == 1     && y == -1    = Box
      | otherwise                = Ground

-- sample maze with additional unreachable box, solvable
oneBoxUnreachable :: Maze
oneBoxUnreachable = Maze start maze
  where
    start = C (-3) 3
    maze (C x y)
      | abs x > 4  || abs y > 4  = Blank
      | abs x == 4 || abs y == 4 = Wall
      | x ==  2 && y <=  0       = Wall
      | x == -2 && y <= -2       = Wall
      | x <= -2 && y == -2       = Wall
      | x ==  3 && y <=  0       = Storage
      | x >= -2 && y ==  0       = Box
      | x == -3 && y == -3       = Box
      | otherwise                = Ground

-- sample maze with hole (blank) in wall frame, unsolvable
openMaze :: Maze
openMaze = Maze start maze
  where
    start = C (-3) 3
    maze (C x y)
      | abs x > 4  || abs y > 4  = Blank
      | abs x == 0 &&     y == 4 = Blank
      | abs x == 4 || abs y == 4 = Wall
      | x ==  2 && y <= 0        = Wall
      | x ==  3 && y <= 0        = Storage
      | x >= -2 && y == 0        = Box
      | otherwise                = Ground

-- mazes made by other students, source: https://bit.ly/SokobanMazesMIMUW

easy_spiralMaze_DM :: Maze
easy_spiralMaze_DM = Maze start maze
  where
    start = C 1 (-1)
    maze (C x y)
      | abs x >  4 || abs y > 4      = Blank
      | abs x == 4                   = Wall
      | abs y == 4                   = Wall
      | x ==  2 && y <=  1           = Wall
      | x >= -1 && x <=  2 && y == 1 = Wall
      | x == -1 && y >= -1 && y <= 1 = Wall
      | x ==  0 && y == -1           = Box
      | x ==  3 && y == -3           = Storage
      | otherwise                    = Ground

medium_maze3_GN :: Maze
medium_maze3_GN = Maze start maze
  where
    start = C 0 1
    maze (C x y)
      | abs x >  4 || abs y >  4           = Blank
      | abs x == 4 || abs y == 4           = Wall
      | x ==     1 && y <      0           = Wall
      | x ==    -3 && y ==    -2           = Wall
      | x <=     1 && x >     -2 && y == 0 = Wall
      | x >     -3 && x <      3 && y == 2 = Wall
      | x ==     3 && y >      1           = Storage
      | y ==    -2 && x <      0           = Box
      | y ==    -2 && x ==     2           = Box
      | y ==    0  && x ==     3           = Box
      | y == -1    && x > 1      && x < 4  = Storage
      | otherwise                          = Ground

medium_maze4_GN :: Maze
medium_maze4_GN = Maze start maze
  where
    start = C 1 1
    maze (C x y)
      | abs x > 4  || abs y > 4                  = Blank
      | abs x == 4 || abs y == 4 || x == -3      = Wall
      | x == -2 && (y == 3 || y == 0)            = Wall
      | x == -1 &&  y == -1                      = Wall
      | x == -0 &&  y == 1                       = Wall
      | x ==  3 &&  y == 0                       = Wall
      | x <   0 && (y == 2 || y == -3)           = Storage
      | x == -1 &&  y == 1                       = Storage
      | x ==  0 && (y == 2 || y == 0 || y == -1) = Box
      | x ==  1 &&  y == -2                      = Box
      | x ==  2 &&  y == -3                      = Box
      | otherwise                                = Ground

hard_maze2_GN :: Maze
hard_maze2_GN = Maze start maze
  where
    start = C 1 (-3)
    maze (C x y)
      | abs x > 4  || abs y > 4      = Blank
      | abs x == 4 || abs y == 4     = Wall
      | x ==  2 && y <   0           = Wall
      | x >= -1 && y ==  1 && x <= 2 = Wall
      | x == -3 && y ==  1           = Wall
      | x ==  0 && y ==  3           = Wall
      | x ==  0 && y ==  0           = Wall
      | x ==  3 && y == -3           = Storage
      | x ==  1 && y ==  2           = Storage
      | x == -3 && y ==  2           = Storage
      | x ==  1 && y == -1           = Storage
      | x == -2 && y ==  1           = Box
      | x ==  2 && y ==  2           = Box
      | x <=  1 && y == -2 && x >= 0 = Box
      | otherwise                    = Ground
      
badTestMaze_BS :: Maze
badTestMaze_BS = Maze start maze
  where
    start = C (-2) (-2)
    maze (C x y)
      | abs x > 4  || abs y > 4  = Blank
      | abs x == 4 || abs y == 4 = Wall
      | x ==  2                  = Wall
      | x ==  3 && y <= 0        = Storage
      | x >= -2 && y == 0        = Box
      | otherwise                = Ground

cutOffStorageMaze_DM :: Maze
cutOffStorageMaze_DM = Maze start maze
  where
    start = C 1 (-1)
    maze (C x y)
      | abs x > 7 || abs y > 7                            = Blank
      | abs x == 7                                        = Wall
      | abs y == 7                                        = Wall
      | x >= 4 && y == 4                                  = Wall
      | x == 4 && y >= 4                                  = Wall
      | x >= 5 && y >= 5                                  = Storage
      | elem (x, y) [(-6, 6), (-6, -6), (6, -6), (6, -5)] = Storage
      | x == 0 && elem y [-4 .. 2]                        = Box
      | otherwise                                         = Ground

