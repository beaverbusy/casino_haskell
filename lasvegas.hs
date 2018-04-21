-- Todo:
-- writefile' & readfile' failure resistent and read "f" in case of file read failure.
-- fix bug: Pot is too small with 2 raising bots ,1 call bot 1 fold bot pot stays at 9
-- rest seems to work.

import System.IO
import Data.List
import System.Random
import Data.Array.IO
import Control.Monad
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
dealCardsFile :: Deck -> Int -> Int -> IO ()
dealCardsFile deck handNo button = 
        forM_ [0..kPlayers - 1] (\i -> 
            let sh = show handNo ++ "D" ++ show button ++ "D" ++ show card1 ++ show card2
                card1 =  deck !! (2 * i)     
                card2 =  deck !! (2 * i + 1)
            in  do writeFile ("./botfiles/casinoToBot" ++ show i) (sh))

-- start of new hand: reinitialise GameState
resetGameState :: Deck -> GameState -> GameState
resetGameState deck (GameState tb ac _ bn hn hd pt rs cl wn) =  GameState tb' ac' deck bn' hn' hd' pt' rs' cl' wn'
        where
    bn' = (bn + 1) `mod` kPlayers   -- button moves
    hn' = hn + 1    -- hand no +1
    hd' = show hn' ++ "D" ++ show bn' ++ "P" 
    pt' = 2     -- 2 blinds
    rs' = 1     -- blinds are one raise
    cl' = 0     -- nobody called yet
    wn' = []    -- no winners yet
    ac' = rotate ((bn' + 3) `mod` kPlayers) [0 .. kPlayers - 1]     -- now Utg is first
    tb1 = map (\p -> p {pVpip = 0}) tb   -- set all vpip to 0
    tb' = map (collectBlinds) (zip [0 .. kPlayers -1] tb1)    -- collect blinds, set pVpip to 1
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
getBets :: Int -> GameState -> IO GameState
getBets st gs
        -- all but one folded or hand is over
    | ((length $ gActive gs) == 1 || st == 4) = return gs
        -- everyone called already, end the street.
    | (gCalls gs == length (gActive gs)) = do
        let  gs' = resetPostStreet st gs
        getBets (st + 1) gs'
    | otherwise = do
        writeFile ("./botfiles/casinoToBot" ++ (show  currS)) (show $ gHandDesc gs)
        usleep kPause   -- sleep 
        b <- readFile ("./botfiles/botToCasino" ++ (show  currS))
        case (b !! 0)   of  'f' -> getBets st gs1                    
                            'c' -> getBets st gs2                    
                            'r' -> if gRaises gs == kMaxRaises then getBets st gs2 else getBets st gs3    -- a raise past kMaxRaises is a call
                            _   -> getBets st gs1 -- error is a fold
            where   
-- fold
    currS = head $ gActive gs -- seat of current Player to act
    currP = gTable gs !! currS -- current player
    tb1 = tail $ gActive gs -- remove folding player
    gs1  = gs {gActive = tb1, gHandDesc = gHandDesc gs ++ "f"}     -- updated hd desc with info latest move is a fold
-- call
    due2 = (if st < 2 then 1 else 2) * (gRaises gs - pVpip currP)  -- ammount owed by caller multiplied by price of current round
    currP2 = currP {pStack = pStack currP - due2, pVpip = gRaises gs}     -- update stack, and vpip of current player
    tb2 = take currS (gTable gs) ++ currP2 : (reverse . take (kPlayers - currS - 1) . reverse $ (gTable gs))   -- upate table by adding updated current player
    active2 = rotate 1 (gActive gs)     -- mv current player at end of table
    gs2  = gs {gActive = active2, gTable = tb2, gHandDesc = gHandDesc gs ++ "c", gPot = due2 + gPot gs, gCalls = gCalls gs + 1}
-- raise                                
    due3 = (if st < 2 then 1 else 2) * (gRaises gs + 1 - pVpip currP)  -- ammount owed by raiser, + 1 cos gs not updated yet
    currP3 = currP {pStack = pStack currP - due3, pVpip = 1 + gRaises gs}     -- update stack, and vpip
    tb3 = take currS (gTable gs) ++ currP3 : (reverse . take (kPlayers - currS - 1) . reverse $ gTable gs)  -- upate table by adding updated current player
    gs3  = gs {gActive = active2, gTable = tb3, gHandDesc = gHandDesc gs ++ "r", gPot = due3 + gPot gs  , gRaises = gRaises gs + 1, gCalls = 0}



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
showdownToFile ::  GameState -> IO ()
showdownToFile gs = do
    appendFile ("./botfiles/showdowns") (show  (gHandDesc gs) ++ "\n")
    return ()

-- payoffs the winners, use it when there was and was not a showdown
payoffs :: GameState ->  GameState 
payoffs gs = gs'
        where
    won = (gPot gs) `div` (length $ gWinners gs)    -- amount to add to stack of each winner
    tb = map (\p -> if fst p `elem` gWinners gs then (snd p) {pStack = won + pStack (snd p)} else snd p) (zip [0 .. kPlayers - 1] (gTable gs))
    gs' = gs {gTable = tb} 

-- main function: loops and plays hand
oneHand :: GameState -> IO ()
oneHand gs = do
    sDeck <- shuffle kDeck     -- newly shuffled deck
-- reset table and state
    let gs0 = resetGameState sDeck gs     -- reboot gs
-- deal cards
    dealCardsFile sDeck (gHandNo gs0) (gButton gs0) -- writes hands, handno, button to files of each bot
 
    gs1 <- getBets 0 gs0     -- launches the betting returns end gs
    let gs2 = showdown gs1
    showdownToFile gs2
    let gs3 = payoffs gs2
    putStrLn "End of hand, gs is:"
    print gs3
    putStrLn ""
    oneHand gs3

main =  do
    oneHand kGameState
