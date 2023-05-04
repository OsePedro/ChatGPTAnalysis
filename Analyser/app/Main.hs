module Main where

import Control.Applicative
import Control.Arrow
import Control.Monad
import Data.Char
import Data.Function
import Data.List
import qualified Data.Set as S
import Prelude hiding(Word)
import System.Directory
import System.FilePath.Posix


main = do
  let responsesRoot = "../Responses/Prompt-00"
  let maxWordSeqLength = 10
  adjDirs <-
    directoryEntries responsesRoot >>= filterM doesDirectoryExist . sort
  responseFiles <- sequence $
    ((>>= filterM isResponseFile . sort) . directoryEntries) <$> adjDirs
  responseStrings <- sequence $ sequence . map readFile <$> responseFiles
  let tkns = map tokens <$> responseStrings
  let allTraitResponses = traitResponses <$> zip adjDirs tkns
  print $ responseWordSequences maxWordSeqLength $ head $ fromTraitResponses $ head allTraitResponses
  return ()

responseWordSequences :: Int -> Response -> S.Set [Word]
responseWordSequences maxNoWords =
  S.unions . map (wordSequences maxNoWords) . fromResponse

wordSequences :: Int -> Sentence -> S.Set [Word]
wordSequences maxNoWords = S.fromList . go . fromSentence
  where
  go :: [Word] -> [[Word]]
  go [] = []
  go ws = ((`take` ws) <$> [1..maxNoWords]) ++ go (tail ws)

traitResponses :: (FilePath,[[Token]]) -> TraitResponses
traitResponses (dir,tkns) =
  let trait = dropWhile (not . isLetter) $ last $ splitDirectories dir
  in  TraitResponses (response <$> tkns) trait

response :: [Token] -> Response
response = Response . unfoldr sentence

sentence :: [Token] -> Maybe (Sentence,[Token])
sentence ts =
  let isStop = ("." ==). fromToken
      (sntnce,rest) = break isStop $ dropWhile isStop ts
  in  if null sntnce
      then Nothing
      else Just (Sentence $ Word . fromToken <$> filter tIsWord sntnce, rest)

tokens :: String -> [Token]
tokens = unfoldr nextToken

nextToken :: String -> Maybe (Token,String)
nextToken s = nonSpaceString s >>= \(nss0,rest) ->
  let labels = labelStrings nss0
      mPair = takeInitialPunctuation labels <|> takeMeasurement labels <|>
              takeAcceptable labels
  in  mPair >>= Just . second (++rest)
  where

  takeInitialPunctuation :: [Label String] -> Maybe (Token,String)
  takeInitialPunctuation (l:rest)
    | lIsPunctuation l = Just (Token (unlabel l) False, merge rest)
    | otherwise = Nothing

  -- Pre: first Label String is not punctuation
  takeAcceptable :: [Label String] -> Maybe (Token,String)
  takeAcceptable ls = let (a,rest) = span alwaysAcceptable ls
                      in  Just (Token (merge a) True, merge rest)

  alwaysAcceptable :: Label String -> Bool
  alwaysAcceptable s =
    not (lIsPunctuation s) || alwaysAcceptablePunc (unlabel s)

  alwaysAcceptablePunc :: String -> Bool
  alwaysAcceptablePunc "-" = True
  alwaysAcceptablePunc "'" = True
  alwaysAcceptablePunc _ = False

  takeMeasurement :: [Label String] -> Maybe (Token,String)
  takeMeasurement (a:b:c:d:rest)
    | isNumStr a && unlabel b == "'" && isNumStr c && unlabel d == "\"" =
        Just (Token (merge [a,b,c,d]) True, merge rest)
    | otherwise = Nothing
    where isNumStr = all isDigit . unlabel
  takeMeasurement _ = Nothing

  merge :: [Label String] -> String
  merge = concat . map unlabel

labelStrings :: NonSpaceString -> [Label String]
labelStrings (NonSpaceString s) = labels False $ labelChar <$> s
  where
  labels :: Bool -> [Label Char] -> [Label String]
  labels _ [] = []
  labels expectingPunc lChars =
    let (same,rest) = span ((expectingPunc ==) . lIsPunctuation) lChars
    in  if null same
        then  labels (not expectingPunc) lChars
        else  merge same : labels (not expectingPunc) rest

  -- Pre: lChars is not null and labels are uniform
  merge :: [Label Char] -> Label String
  merge lChars = Label (unlabel <$> lChars) $ lIsPunctuation $ head lChars

nonSpaceString :: String -> Maybe (NonSpaceString,String)
nonSpaceString s =
  let (nonSpace,rest) = break isSpace $ dropWhile isSpace s
  in  if null nonSpace then Nothing else Just (NonSpaceString nonSpace, rest)

-- Pre: the `Char` is not a space character
labelChar :: Char -> Label Char
labelChar c = Label c $ isPunc c
  where
  isPunc c  | isPunctuation c = True
            | isLetter c || isDigit c = False
            | otherwise = error $ "Unrecognised character: "++show c

directoryEntries :: FilePath -> IO [FilePath]
directoryEntries dir = map (dir </>) <$> listDirectory dir

isResponseFile :: FilePath -> IO Bool
isResponseFile path = do
  isFile <- doesFileExist path
  return $ isFile && isResponseFileName path

isResponseFileName :: FilePath -> Bool
isResponseFileName path =
  all isDigit (takeBaseName path) && takeExtension path == ".txt"


data TraitResponses =
  TraitResponses {fromTraitResponses :: [Response], trait :: String}
newtype Response = Response {fromResponse :: [Sentence]}
newtype Sentence = Sentence {fromSentence :: [Word]}
newtype Word = Word {fromWord :: String}
data Token = Token {fromToken :: String, tIsWord :: Bool}
data Label a = Label {unlabel :: a, lIsPunctuation :: Bool}
newtype NonSpaceString = NonSpaceString String

instance Show Sentence where show = show . fromSentence
instance Show Word where show = fromWord
instance Show Token where show = fromToken
instance Eq Word where (==) = (==) `on` lower
instance Ord Word where compare = compare `on` lower

lower = map toLower . fromWord
