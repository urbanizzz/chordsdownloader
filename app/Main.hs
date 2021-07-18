module Main where

import Network.HTTP.Simple
import System.Directory           (createDirectoryIfMissing)

import qualified Data.ByteString.Lazy.Char8           as BL


type Content = BL.ByteString

urlPrefix = "https://muzland.ru/pic/chords/"
fileExtension = ".gif"
savePath = "~/chords/"

chordNotes = ["C", "Cw", "D", "Dw", "E", "F", "Fw", "G", "Gw", "A", "Bb", "H"]
chordSuffixes =
  [ ("", "Основной мажорный аккорд")
  , ("7", "Мажорный септаккорд")
  , ("maj", "Большой мажорный септаккорд")
  , ("maj9", "Мажорный нонаккорд с большой септимой")
  , ("add11", "Мажорный аккорд с добавленной ундецимой")
  , ("7add11", "Мажорный септаккорд с добавленной ундецимой")
  , ("add11p", "Мажорный аккорд с добавленной увеличенной ундецимой")
  , ("11", "Мажорный ундецимаккорд")
  , ("6", "Мажорный секстаккорд")
  , ("7s6", "Мажорный септаккорд с секстой")
  , ("6add9", "Мажорный секстаккорд с добавленной ноной")
  , ("6add11", "Мажорный секстаккорд с добавленной ундецимой")
  , ("add13z", "Мажорный аккорд с добавленной уменьшённой терцдецимой")
  , ("7add13z", "Мажорный септаккорд с добавленной уменьшённой терцдецимой")
  , ("p", "Мажорный аккорд с увеличенной квинтой")
  , ("75p", "Мажорный септаккорд с увеличенной квинтой")
  , ("5z", "Мажорный аккорд с уменьшённой квинтой")
  , ("75z", "Мажорный септаккорд с уменьшённой квинтой")
  , ("add9", "Мажорный аккорд с добавленной ноной")
  , ("9", "Мажорный нонаккорд")
  , ("add9z", "Мажорный аккорд с добавленной уменьшённой ноной")
  , ("9z", "Мажорный нонаккорд с уменьшённой ноной")
  , ("add9p", "Мажорный аккорд с добавленной увеличенной ноной")
  , ("9p", "Мажорный нонаккорд с увеличенной ноной")
  , ("7add11p", "Мажорный септаккорд с добавленной увеличенной ундецимой")
  , ("maj5z", "Большой мажорный септаккорд с уменьшённой квинтой")
  , ("sus2", "Аккорд с задержанием на секунде")
  , ("6sus2", "Секстаккорд с задержанием на секунде")
  , ("7sus2", "Септаккорд с задержанием на секунде")
  , ("sus4", "Аккорд с задержанием на кварте")
  , ("6sus4", "Секстаккорд с задержанием на кварте")
  , ("7sus4", "Септаккорд с задержанием на кварте")
  , ("dim7", "Уменьшённый септаккорд")
  , ("5", "Квинтаккорд (пауэр аккорд)")
  , ("m", "Основной минорный аккорд")
  , ("m7", "Минорный септаккорд")
  , ("m7p", "Большой минорный септаккорд")
  , ("mmaj9", "Минорный нонаккорд с большой септимой")
  , ("madd11", "Минорный аккорд с добавленной ундецимой")
  , ("m7add11", "Минорный септаккорд с добавленной ундецимой")
  , ("madd11p", "Минорный аккорд с добавленной увеличенной ундецимой")
  , ("m11", "Минорный ундецимаккорд")
  , ("m6", "Минорный секстаккорд")
  , ("m7s6", "Минорный септаккорд с секстой")
  , ("m6add9", "Минорный секстаккорд с добавленной ноной")
  , ("m6add11", "Минорный секстаккорд с добавленной ундецимой")
  , ("madd13z", "Минорный аккорд с добавленной уменьшённой терцдецимой")
  , ("m7add13z", "Минорный септаккорд с добавленной уменьшённой терцдецимой")
  , ("mp", "Минорный аккорд с увеличенной квинтой")
  , ("m75p", "Минорный септаккорд с увеличенной квинтой")
  , ("m5z", "Минорный аккорд с уменьшённой квинтой")
  , ("m75z", "Минорный септаккорд с уменьшённой квинтой")
  , ("madd9", "Минорный аккорд с добавленной ноной")
  , ("m9", "Минорный нонаккорд")
  , ("madd9z", "Минорный аккорд с добавленной уменьшённой ноной")
  , ("m9z", "Минорный нонаккорд с уменьшённой ноной")
  ]
chordVariations = "" : withDash
  where withDash = map (('-' :) . show) [1 ..]

chordsList :: [(String, String)]
chordsList = [ (mconcat [note, suffix], mconcat [ "C", suffix ]) | note <- chordNotes, suffix <- map fst chordSuffixes ]

getFileContents :: FilePath -> IO (Maybe Content)
getFileContents filename = do
  request <- parseRequest $ mconcat
        [ urlPrefix
        , filename
        , fileExtension
        ]
  res <- httpLBS request
  let contents = getResponseBody res
  if (checkContents contents)
    then
      return $ Just contents
    else
      return Nothing

checkContents :: BL.ByteString -> Bool
checkContents contents = BL.take 5 contents == "GIF89"

getChords :: [(String, String)] -> IO ()
getChords [] = return ()
getChords ((chord, folder):xs) = do
  let currentChordVariations = map (chord ++) chordVariations
  getChordVariations folder currentChordVariations
  getChords xs

getChordVariations :: String -> [String] -> IO ()
getChordVariations folder (file:xs) = do
  maybeContents <- getFileContents file
  case maybeContents of
    Nothing -> return ()
    Just contents -> do
      let fullPath = mconcat
            [ savePath
            , folder
            ]
      createDirectoryIfMissing True fullPath
      let fileToSave = mconcat
            [ fullPath
            , "/"
            , file
            , fileExtension
            ]
      BL.writeFile fileToSave contents
      getChordVariations folder xs

main :: IO ()
main = getChords chordsList

