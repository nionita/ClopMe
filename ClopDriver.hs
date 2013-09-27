module Main (main) where

-- This is the CLOP driver, i.e. this program is called by CLOP (Remi Coulom)
-- in order to play a game and announce the result
-- The program is called with following parameters
-- #1: processor id - a symbolic name, Clop could run the driver with different values (in parallel)
-- #2: seed (integer) - game number
-- #3: parameter id of first parameter (name)
-- #4: value of first parameter (float)
-- #5: parameter id of second parameter (optional)
-- #6: value of second parameter (optional)
-- ...
-- The driver should write the game outcome to its output:
-- W = win
-- L = loss
-- D = draw

import Prelude hiding (catch)
import Control.Applicative ((<$>))
import Control.Concurrent
import Control.Concurrent.Async
import Control.Exception
import Control.Monad (forM, forM_, liftM, when)
import Control.Monad.Trans.Class
import Control.Monad.Trans.State.Lazy
import Data.Char (isSpace)
import Data.List (intersperse, isPrefixOf, sortBy, groupBy, delete)
import qualified Data.Map as M
import Data.Maybe
import Data.Ratio
import Data.Typeable
import System.Directory
import System.Environment (getArgs)
import System.FilePath
import System.IO
import System.IO.Error hiding (catch)
import System.Process
import System.Time
import System.Timeout (timeout)
import System.Locale (defaultTimeLocale)
import System.Random
import Text.Printf

data DriverConfig = DC {
        dcRefEngine :: String,		-- reference engine
        dcRefArgs   :: String,	-- reference engine arguments
        dcChaEngine :: String,		-- learning engine
        dcChaArgs   :: String,	-- learning engine arguments
        dcRefMoves, dcRefFixTm, dcRefSecPerMv :: String,	-- time for reference
        dcChaMoves, dcChaFixTm, dcChaSecPerMv :: String,	-- time for challenger
        -- dcRefUci :: String,	-- uci command for reference engine
        -- dcChaUci :: String,	-- uci command for learning engine
        dcRefProto, dcChaProto :: String	-- protocols (uci/)
     }

-- Some constants for the evolving environment
learnDir   = "J:\\Learn\\CLOP"
gamesDir   = "Games"
currentDir = "Current"

-- Some constants for playing one match
cuteChessDir = "J:\\Chess\\cutechess-cli-0.6.0-win32"
cuteChessCom = cuteChessDir ++ "\\cutechess-cli.exe"

-- We have to use the first parameter from CLOP twofold:
-- first part will be a subdirectory in which the CLOP learn session will run
-- second part will a unique identifier for the parallel process
-- The 2 parts are delimitted by an underscore "_"
main = do
    (proc : seed : params)  <- getArgs
    let dict = makeDict params
        (session, thread) = break (== '_') proc
    res <- runGame session thread seed dict
    putStrLn res

-- Take a list of param/values sequence (as strings) and structure it
-- in form [(param, value)]
makeDict :: [String] -> [(String, Double)]
makeDict ss = go Nothing ss []
    where go Nothing [] acc = acc
          go _       [] _   = error "Unmatched param/value sequence"
          go Nothing  (p:ps) acc = go (Just p) ps acc
          go (Just p) (v:ps) acc = go Nothing  ps ((p, read v):acc)

-- Read a configuration file for the CLOP learn session
-- prepare the environment (i.e. config file, log files, pgn file names)
-- and start a game using cutechess-cli
-- The leaning engine must understand at least the option -p par=val,...
-- Other options can be given in the session config
runGame :: String -> String -> String -> [(String, Double)] -> IO String
runGame session thread seed dict = do
    setCurrentDirectory $ baseDir session
    cdconf <- readDriverConfig
    let white = last seed `elem` "13579"
        (args, refdir, chadir) = mkCutechessCommand cdconf session thread white dict
    createDirectoryIfMissing True refdir
    createDirectoryIfMissing True chadir
    wbd <- oneMatch args
    case wbd of
        (1, 0, 0) -> if white then return "W" else return "L"
        (0, 1, 0) -> if white then return "L" else return "W"
        _         -> return "D"

baseDir :: String -> FilePath
baseDir session = learnDir </> session

-- Give names to the candidates
nameCandidates evn cycle = zip (map label [1..])
    where label i = evn ++ "-" ++ show cycle ++ "-" ++ show i ++ ".txt"

-- Return a list of parameters for the cutechess-cli command
-- and the 2 directories in which the engines run
mkCutechessCommand :: DriverConfig -> String -> String -> Bool -> [(String,Double)]
                   -> ([String], FilePath, FilePath)
mkCutechessCommand dcf session thread white dict
    = (args, refcurr, chacurr)
    where common = [
              "-site", "Sixpack",
              "-event", session,
              "-draw", "movenumber=25", "movecount=10", "score=8",	-- draw conditions
              "-resign", "movecount=4", "score=800",			-- resign conditions
              "-pgnout", pgnout
              ]
          eng1 = [	-- the learning engine
              "-engine",
              "name=" ++ takeFileName (dcChaEngine dcf),
              "cmd=" ++ dcChaEngine dcf,
              "dir=" ++ chacurr,
              "proto=" ++ dcChaProto dcf,
              "tc=" ++ chatime
              ] ++ map (\(n,v) -> "arg=-p" ++ n ++ "=" ++ show v) dict
                ++ optArgs dcChaArgs
          eng2 = [	-- the reference engine
              "-engine",
              "name=" ++ takeFileName (dcRefEngine dcf),
              "cmd=" ++ dcRefEngine dcf,
              "dir=" ++ refcurr,
              "proto=" ++ dcRefProto dcf,
              "tc=" ++ reftime
              ] ++ optArgs dcRefArgs
          args = if white then common ++ eng1 ++ eng2 else common ++ eng2 ++ eng1
          pgnout = base </> ("thr" ++ thread ++ ".pgn")
          refcurr = base </> ("ref" ++ thread)
          reftime = "tc=" ++ dcRefMoves dcf ++ "/" ++ dcRefFixTm dcf ++ "+" ++ dcRefSecPerMv dcf
          chacurr = base </> ("cha" ++ thread)
          chatime = "tc=" ++ dcChaMoves dcf ++ "/" ++ dcChaFixTm dcf ++ "+" ++ dcChaSecPerMv dcf
          base = baseDir session
          optArgs f = if null (f dcf) then [] else map (\w -> "arg=" ++ w) (words $ f dcf)

oneMatch :: [String] -> IO (Int, Int, Int)
oneMatch args = do
    (_, Just hout, _, ph)
            <- createProcess (proc cuteChessCom args) { std_out = CreatePipe }
    catch (everyLine hout (0, 0, 0) 1) $ \e -> do
        let es = ioeGetErrorString e
        putStrLn $ "Error in everyLine: " ++ es
        terminateProcess ph
        throwIO e

everyLine _ r 0 = return r
everyLine h r g = do
    lin <- hGetLine h
    -- when debug $ putStrLn $ "Got: " ++ lin
    let (r1, g1) = if "Score of" `isPrefixOf` lin
                      then (getScore lin, g-1)
                      else (r, g)
    everyLine h r1 g1

-- The line has the following structure:
-- Score of x x1 ... vs y y1 ...: a - b - c [prc] n
-- where x and y are the opponents, a = wins of x, b = wins of y, c = draws
getScore :: String -> (Int, Int, Int)
getScore
    = listToTrio
    . map (read . snd)
    . filter (even . fst)
    . zip [0..]
    . take 5
    . words
    . drop 2
    . dropWhile (/= ':')

listToTrio (x:y:z:_) = (x, y, z)

readDriverConfig = stringToConfig <$> readFile "ClopDriver.txt"

stringToConfig :: String -> DriverConfig
stringToConfig = foldr (\(n, s) dc -> lookApply n s dc funlist) defDC
                       . catMaybes . map readParam . noComments . lines
    where defDC = DC {
              dcRefEngine = "J:\\Barbarossa\\dist\\build\\Barbarossa\\Barbarossa_0_01_k3nmd.exe",
              dcRefArgs   = "-l5",
              dcChaEngine = "J:\\Barbarossa\\dist\\build\\Barbarossa\\Barbarossa_0_01_castp.exe",
              dcChaArgs   = "-l2",
              -- dcChaConfig = "J:\\AbaAba\\dist\\build\\Abulafia\\test1-51-6.txt",
              -- dcRefMoves = "40", dcRefFixTm = "20", dcRefSecPerMv = "0.2",
              -- dcChaMoves = "40", dcChaFixTm = "20", dcChaSecPerMv = "0.2",
              dcRefMoves = "40", dcRefFixTm = "120", dcRefSecPerMv = "1",
              dcChaMoves = "40", dcChaFixTm = "120", dcChaSecPerMv = "1",
              dcRefProto = "uci", dcChaProto = "uci"
          }
          setRefEngine   s dc = dc { dcRefEngine   = s }
          setRefArgs     s dc = dc { dcRefArgs     = s }
          setRefMoves    s dc = dc { dcRefMoves    = s }
          setRefFixTm    s dc = dc { dcRefFixTm    = s }
          setRefSecPerMv s dc = dc { dcRefSecPerMv = s }
          setRefProto    s dc = dc { dcRefProto    = s }
          setChaEngine   s dc = dc { dcChaEngine   = s }
          setChaArgs     s dc = dc { dcChaArgs     = s }
          setChaMoves    s dc = dc { dcChaMoves    = s }
          setChaFixTm    s dc = dc { dcChaFixTm    = s }
          setChaSecPerMv s dc = dc { dcChaSecPerMv = s }
          setChaProto    s dc = dc { dcChaProto    = s }
          funlist = [ ("RefEngine",   setRefEngine),
                      ("RefArgs",     setRefArgs),
                      ("RefMoves",    setRefMoves),
                      ("RefFixTm",    setRefFixTm),
                      ("RefSecPerMv", setRefSecPerMv),
                      ("RefProto",    setRefProto),
                      ("ChaEngine",   setChaEngine),
                      ("ChaArgs",     setChaArgs),
                      ("ChaMoves",    setChaMoves),
                      ("ChaFixTm",    setChaFixTm),
                      ("ChaSecPerMv", setChaSecPerMv),
                      ("ChaProto",    setChaProto) ]
          noComments = filter (not . isComment)
          isComment ""                 = True
          isComment ('-':'-':_)        = True
          isComment (c:cs) | isSpace c = isComment cs
          isComment _                  = False

type Setter a = String -> a -> a

lookApply :: String -> String -> a -> [(String, Setter a)] -> a
lookApply s v a = maybe a (($ a) . ($ v)) . lookup s

readParam :: String -> Maybe (String, String)
readParam s = let (ns, vs) = span (/= '=') s
              in case vs of
                     ('=' : rs) -> Just (strip ns, strip rs)
                     _          -> Nothing	-- did not contain '='
    where strip = filter (not . isSpace)
