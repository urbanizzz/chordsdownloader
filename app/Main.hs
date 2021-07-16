module Main where

import Lib
import Network.HTTP.Simple

import qualified Data.ByteString.Lazy.Char8           as BL

urlPrefix = "https://muzland.ru/pic/chords/"
urlExtension = ".gif"

getFile :: IO ()
getFile = do
  request <- parseRequest $ mconcat
                              [ urlPrefix
                              , "Bbmaj9"
                              , urlExtension
                              ]
  res <- httpLBS request
  let contents = getResponseBody res
  BL.writeFile "Bbmaj9.gif" contents


main :: IO ()
main = do
  getFile
  print "OK"

