{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DeriveAnyClass         #-}
{-# LANGUAGE TemplateHaskell         #-}

module Main where

import GHC.Generics
import Data.Aeson as Aeson
import Data.Aeson.TH
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Lazy as BSL
import Web.UAParser
import Data.Maybe
import Data.Char ( toUpper )
import Data.Text (Text)
import Data.List
import qualified Data.Map.Strict as Map

data SearchResult = SearchResult { ua :: String }
  deriving (Generic, Show)

$(deriveJSON defaultOptions{fieldLabelModifier=map toUpper} 'SearchResult)

data SearchEntry = SearchEntry { preview :: Bool, result :: SearchResult }
  deriving (Generic, Aeson.FromJSON, Show)

test1 :: ByteString
test1 = "Mozilla/4.0 (compatible; MSIE 7.0; Windows NT 6.2; Win64; x64; Trident/7.0; .NET4.0C; .NET4.0E; .NET CLR 2.0.50727; .NET CLR 3.0.30729; .NET CLR 3.5.30729; Zoom 3.6.0)"
test2 :: ByteString
test2 = "Mozilla/4.0 (compatible; MSIE 10.0; Windows NT 5.1; Trident/4.0; (R1 1.5); .NET CLR 1.1.4322; .NET CLR 2.0.50727)"


files =
  [ "/Users/haishengwu/Downloads/ok1-widget-uas.json"
  , "/Users/haishengwu/Downloads/ok3-widget-uas.json"
  , "/Users/haishengwu/Downloads/ok12-widget-uas.json"
  ]

main :: IO ()
main = mainFile (head $ tail $ files)

mainFile file = do
  putStr "=================== " >> putStrLn file
  file <- BSL.readFile file
  case (Aeson.eitherDecode file :: Either String [SearchEntry]) of
    Left e -> print e
    Right xs -> do
      let uas = map (C.pack . ua . result) xs
      let parsed = map (\str -> (parseUA str, parseOS str, str)) uas
      let result = [osrFamily (fromJust mb) <> ":" <> uarFamily (fromJust ma) <> "-" <> (fromMaybe "" $ uarV1 (fromJust ma)) | (ma, mb, raw) <- parsed, isJust ma, isJust mb, isJust (uarV1 (fromJust ma)) ] :: [Text]
      -- mapM_ print (nub $ sort result)
      mapM_ (\(_,_,raw) -> print raw) $ filter (\(a,b, raw) -> isNothing a || isNothing b || isNothing (uarV1 (fromJust a))) parsed

cannotBeParsed :: ByteString -> Bool
cannotBeParsed str = isNothing (parseUA str) || isNothing (parseOS str)

mainTest :: ByteString -> IO ()
mainTest str = do
  print (parseUA str)
  print (parseOS str)
  print (parseDev str)
