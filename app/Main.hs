module Main ( main ) where

import           Lib

import           System.Console.GetOpt
import           System.Environment
import           System.Exit
import           System.IO
import           Data.Word

import qualified Data.ByteString       as BS
import qualified Data.ByteString.Lazy  as BSL

main :: IO ()
main = do
    args <- getArgs

    let (actions, nonOptions, errors) =
            getOpt RequireOrder options args
    opts <- foldl (>>=) (return startOptions) actions

    let Options{optInput = input,optOutput = output} =
            opts

    convert <$> input >>= output

data Options = Options { optInput  :: IO BSL.ByteString
                       , optOutput :: BSL.ByteString -> IO ()
                       }

startOptions :: Options
startOptions = Options { optInput = BSL.getContents, optOutput = BSL.putStr }

options :: [OptDescr (Options -> IO Options)]
options = [ Option "u"
                   [ "unicode" ]
                   (ReqArg (\arg _ -> do
                                let utf8 = convertSingle (read arg :: Word)

                                putStr "UTF-8 bytes: "
                                print $ asHex utf8
                                putStrLn ""

                                putStr "Character: "
                                BS.putStr utf8
                                putStrLn ""
                                exitSuccess)
                           "INTEGER")
                   "Input unicode character code"
          , Option "i"
                   [ "input" ]
                   (ReqArg (\arg opt -> return opt { optInput = BSL.readFile arg })
                           "FILE")
                   "Input file"
          , Option "o"
                   [ "output" ]
                   (ReqArg (\arg opt -> return opt { optOutput = BSL.writeFile arg })
                           "FILE")
                   "Output file"
          , Option "h"
                   [ "help" ]
                   (NoArg (\_ -> do
                               prg <- getProgName
                               hPutStrLn stderr (usageInfo prg options)
                               exitSuccess))
                   "Show help"
          ]