-- This is a sugarless rewrite. Will not be maintained.
-- Todo:
-- writefile' & readfile' failure resistent and read "f" in case of file read failure.
-- fix bug: Pot is too small with 2 raising bots ,1 call bot 1 fold bot pot stays at 9
-- rest seems to work.

import System.IO
import Data.List
import System.Random
import Data.Array.IO
import Control.Monad
import Control.Monad.State
import Data.Poker
import System.Posix.Unistd


-- magic numbers
kPlayers = 4 :: Int     -- no of bots playing
kMaxRaises = 3 :: Int    -- max number of raises per street
kPause = 10000  -- time given to bots for thinking in 10^{-6} seconds
-- unshuffled deck
kDeck = [mkCard a b | a <- [Two .. Ace], b <- [Hearts .. Spades ] ] 
-- create a table of kPlayers
kTable = replicate kPlayers (Player {pStack = 0, pVpip =0})  -- start table with kPlayersk

-- create initial gamestate
kGameState = GameState {gTable = kTable, gActive = [0..kPlayers -1], gDeck = kDeck, gButton = 0, gHandNo = 0, gHandDesc = "", gPot = 0, gRaises = 0, gCalls = 0, gWinners = []}



-- players and tables
data Player = Player {
        pStack  :: Int      -- stack
    ,   pVpip :: Int     -- vpip, number of bets called on current street
} deriving (Show)


data GameState = GameState {
        gTable :: Table  -- all players in the game
    ,   gActive :: [Int]    -- seats of all players still in the hand
    ,   gDeck    :: [Card]      -- deck
    ,   gButton  :: Int          -- button
    ,   gHandNo  :: Int          -- hand number
    ,   gHandDesc :: String        -- hand description to communicate to bots
    ,   gPot :: Int           -- pot
    ,   gRaises :: Int           -- no of raises
    ,   gCalls :: Int           -- no of calls made
    ,   gWinners :: [Int]   -- seat of winners
} deriving (Show)

type Deck = [Card]
type Table = [Player]


-- | Randomly shuffle a list
--   /O(N)/
shuffle :: [a] -> IO [a]
shuffle xs = do
        ar <- newArray n xs
        forM [1..n] $ \i -> do
            j <- randomRIO (i,n)
            vi <- readArray ar i
            vj <- readArray ar j
            writeArray ar j vi
            return vj
  where
    n = length xs
    newArray :: Int -> [a] -> IO (IOArray Int a)
    newArray n xs =  newListArray (1,n) xs


-- rotate a table k times, ie move head player to tail k times
rotate :: Int -> [Int] -> [Int]
rotate k tb
    | k == 0 = tb
    | otherwise = rotate (k - 1) (tail tb ++ head tb : [])


-- write starting hands to bot files Note: file of player in seat i gets card 2*i and 2*i+1 in deck
dealCardsFile :: Int -> Int -> StateT GameState IO ()
dealCardsFile  handNo button = 
        gets gDeck >>= \gs ->
           forM_ [0..kPlayers - 1] (\i -> 
               let sh = show handNo ++ "D" ++ show button ++ "D" ++ show card1 ++ show card2 
                   card1 =  (gs) !! (2 * i)     
                   card2 =  (gs) !! (2 * i + 1)
               in  liftIO ( writeFile  ("./botfiles/casinoToBot" ++ show i) (sh)))

-- start of new hand: reinitialise GameState

resetGameState :: Deck -> GameState -> GameState
resetGameState deck gs =  GameState {
     gTable = map (collectBlinds) (zip [0 .. kPlayers -1] tb1)    -- collect blinds, set pVpip to 1
   , gActive = rotate ((bn' + 3) `mod` kPlayers) [0 .. kPlayers - 1]     -- now Utg is first
   , gDeck = deck   
   , gButton = bn' 
   , gHandNo = hn' 
   , gHandDesc = show hn' ++ "D" ++ show bn' ++ "P" 
   , gPot = 2 -- 2 blinds
   , gRaises = 1 -- blinds = 1 raise
   , gCalls = 0
   , gWinners = []
}

   where
      bn' = (gButton gs + 1) `mod` kPlayers   -- button moves
      hn' = gHandNo gs + 1    -- hand no +1
      tb1 = map (\p -> p {pVpip = 0}) (gTable gs)   -- set all vpip to 0
      collectBlinds = \a -> if fst a == mod (bn' + 1) kPlayers || fst a == mod (bn' + 2) kPlayers then (snd a) {pStack = pStack (snd a) - 1, pVpip = 1} else snd a



rotate' :: Int -> [Int] -> [Int]    -- rotates till first to act is first elem in list
rotate' bn tb 
    | bn `elem` tb = rot1 bn tb  -- btn is on table
    | bn < maximum tb = rot2 bn (sort tb)  -- some seats number are >= than btn
    | otherwise = sort tb   -- all seats < btn, ie next to act is min
    
        where
    rot1 :: Int -> [Int] -> [Int]    -- first input is btn seat, rot1 rotates till btn + 1, 
    rot1 b t 
        | t !! 0 == b = rotate 1 t
        | otherwise = rot1 b (rotate 1 t)

    rot2 :: Int -> [Int] -> [Int]    -- rotates till first seat > btn is placed begin of list.
    rot2 b t 
        | t !! 0 > b = t
        | otherwise = rot2 b (rotate 1 t)


-- get bets: Int is street 0,1,2,3,4: pre, flop, turn, river, handover
getBets :: Int -> StateT GameState IO ()
getBets st = 
   get >>= \gs ->
      let
      -- call
          due2 = (if st < 2 then 1 else 2) * (gRaises gs - pVpip currP)  -- ammount owed by caller multiplied by price of current round
          currP2 = currP {pStack = pStack currP - due2, pVpip = gRaises gs}     -- update stack, and vpip of current player
          tb2 = take currS (gTable gs) ++ currP2 : (reverse . take (kPlayers - currS - 1) . reverse $ (gTable gs))   -- upate table by adding updated current player
          active2 = rotate 1 (gActive gs)     -- mv current player at end of table
      -- raise                                
          due3 = (if st < 2 then 1 else 2) * (gRaises gs + 1 - pVpip currP)  -- ammount owed by raiser, + 1 cos gs not updated yet
          currP3 = currP {pStack = pStack currP - due3, pVpip = 1 + gRaises gs}     -- update stack, and vpip
          tb3 = take currS (gTable gs) ++ currP3 : (reverse . take (kPlayers - currS - 1) . reverse $ gTable gs)  -- upate table by adding updated current player
      -- fold
          currS = head $ gActive gs -- seat of current Player to act
          currP = gTable gs !! currS -- current player
          tb1 = tail $ gActive gs  in -- remove folding player 

           -- all but one folded or hand is over
            if ((length $ gActive gs) == 1 || st == 4) then return ()
                 -- everyone called already, end the street.
            else if (gCalls gs == length (gActive gs)) then 
               modify (resetPostStreet st) >>
               getBets (st + 1) 
            else  
                 (liftIO $ writeFile ("./botfiles/casinoToBot" ++ (show  currS)) (show $ gHandDesc gs)) >>
                 (liftIO $ usleep kPause) >>  -- sleep 
                 (liftIO $ readFile ("./botfiles/botToCasino" ++ (show  currS))) >>= \b ->
                    case (b !! 0)   of  'c' ->     (put $ gs {gActive = active2, gTable = tb2, gHandDesc = gHandDesc gs ++ "c", gPot = due2 + gPot gs, gCalls = gCalls gs + 1}) >>
                                                   getBets st 
                                        'r' -> if gRaises gs == kMaxRaises then -- a call 
                                                   (put $ gs {gActive = active2, gTable = tb2, gHandDesc = gHandDesc gs ++ "c", gPot = due2 + gPot gs, gCalls = gCalls gs + 1}) >>
                                                   getBets st 
                                               else 
                                                   (put $ gs {gActive = active2, gTable = tb3, gHandDesc = gHandDesc gs ++ "r", gPot = due3 + gPot gs  , gRaises = gRaises gs + 1, gCalls = 0}) >>
                                                   getBets st 
                                        _   -> 
                                                   (put $ gs {gActive = tb1, gHandDesc = gHandDesc gs ++ "f"}) >>     -- updated hd desc with info latest move is a fold
                                                   getBets st 

-- prepare for next street, Int codes which street just finished, 0 is pre, 1 flop, 2 turn, 
resetPostStreet :: Int -> GameState -> GameState
resetPostStreet st gs = gs'
        where
    tb' = map (\p -> p {pVpip = 0}) (gTable gs)   -- set all vpip to 0
    active = rotate' (gButton gs) (gActive gs)  -- table of active players, with utg placed begin of list
    gs' = gs {gTable = tb', gActive = active, gHandDesc = gHandDesc gs ++ street, gRaises = 0, gCalls = 0}  -- update hand desc, reset raises and calls to 0
    street = case st of     0 -> "F"        
                            1 -> "T"
                            2 -> "R"
                            _ -> ""



-- stores winners in gWinners, writes showdown in gHandDesc 
showdown :: GameState -> GameState
showdown gs = gs'
        where
-- compute winners
    surv = map (\s -> (s, let   board = take 5 (reverse $gDeck gs)
                          in    numericalHandValue_n 7 (fromList $ (gDeck gs !! (2 * s)) : (gDeck gs !! (1 + 2 * s)) : board))) 
               (sort $ gActive gs)  -- yields list of pairs (seat, handstrength) for all survivors

    bestHand = maximum [snd h | h <- surv]    -- NumericalHandValue of best made hand
    winners = foldl (\acc x ->  if snd x == bestHand then fst x : acc else acc) [] surv    -- list of seats of winners

-- write showdown to gHandDesc
    sdString = (gHandDesc gs) ++ "S" ++ (show $ take (2 * kPlayers) (gDeck gs)) ++ "B" ++ (show $ take 5 (reverse $ gDeck gs)) ++ "W" ++ show winners
-- updated gs
    gs' = gs {gWinners = winners, gHandDesc = sdString}  -- updated winnerseats, and hand description

-- writes gHandDesc to file, call it after showdown
showdownToFile ::  StateT GameState IO ()
showdownToFile = 
    gets gHandDesc >>= \gs ->
       liftIO $ appendFile ("./botfiles/showdowns") (show  gs ++ "\n")

-- payoffs the winners, use it when there was and was not a showdown
payoffs :: GameState ->  GameState 
payoffs gs = gs'
        where
    won = (gPot gs) `div` (length $ gWinners gs)    -- amount to add to stack of each winner
    tb = map (\p -> if fst p `elem` gWinners gs then (snd p) {pStack = won + pStack (snd p)} else snd p) (zip [0 .. kPlayers - 1] (gTable gs))
    gs' = gs {gTable = tb} 

-- main function: loops and plays hand
oneHand :: StateT GameState IO ()
oneHand = 
    (liftIO $ shuffle kDeck) >>= \sDeck ->       -- newly shuffled deck
-- reset table and state
       modify (resetGameState sDeck) >> get >>= \gs0 -> -- reboot gs
   -- deal cards
          dealCardsFile (gHandNo gs0) (gButton gs0) >> -- writes hands, handno, button to files of each bot
       
          getBets 0 >> -- launches the betting
          modify showdown >>
          showdownToFile >>
          (liftIO $ putStrLn "End of hand, gs is:") >>
          modify payoffs >> get >>= \x -> liftIO  (print x) >>
          (liftIO $ putStrLn "") >>
          oneHand 

main =  
    evalStateT oneHand kGameState
