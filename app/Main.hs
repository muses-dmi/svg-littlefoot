{-# LANGUAGE OverloadedStrings #-}
module Main where
    
import qualified Data.ByteString.Lazy as B

import Data.Text (Text, pack)
import qualified Data.Text as T
import qualified Data.Text.IO as T

-- command line arguments
-- https://github.com/pcapriotti/optparse-applicative
import Options.Applicative
import Data.Semigroup ((<>))

import Lib

-----------------------------------------------------------------------------------
-- Command line stuff
-----------------------------------------------------------------------------------

data Input
    = JSON String
  
input :: Parser Input
input = JSON <$> strOption
                    ( long "json"
                    <> metavar "JSON"
                    <> help "JSON interface" )

opts :: ParserInfo Input
opts = info (input <**> helper)
            ( fullDesc
            <> progDesc "json lightblock compiler"
            <> header "Muses\n\tjson lightpad compiler\nContact: benedict.gaster@uwe.ac.uk" )
  
handleArgs :: Input -> IO String
handleArgs (JSON path) = return path
handleArgs _          = error "incorrect line options"


main :: IO ()
main = do
    file <- handleArgs =<< execParser opts
    s <- go file
    T.putStrLn s