{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}

module Fluffy.Parser where

import Text.Pandoc
import qualified Text.Pandoc as Pandoc
import qualified Data.ByteString.Lazy as BL
import Text.Parsec
import qualified Text.Parsec as Parsec
import Data.List
import Data.Char hiding (Space)
import Data.Maybe

import qualified Database.PostgreSQL.Simple as PG
import Database.PostgreSQL.Simple(Only(..))
import Database.PostgreSQL.Simple.SqlQQ
import Database.PostgreSQL.Simple.ToField (ToField(..),Action(Many))
import qualified Database.PostgreSQL.Simple.ToField as PG
import Database.PostgreSQL.Simple.Types(PGArray(..))
import Data.List
import Data.Binary.Builder(putCharUtf8)


-- True or False
data TrueOrFalse = TrueOrFalse
                   { tofBody :: String
                   , tofAnswer :: Bool
                   , tofRationale :: Maybe String
                   , tofDifficulty :: Maybe String
                   , tofReference :: Maybe Int
                   , tofLearningObjectives :: Maybe String
                   , tofNationalStandards :: Maybe String
                   , tofTopics :: Maybe String
                   , tofKeyWords :: [String]
                   }
                   deriving (Show,Eq)

instance ToField TrueOrFalse where
  toField TrueOrFalse{..} = Many
    [ toField tofBody
    , PG.Plain (putCharUtf8 ',')
    , toField tofAnswer
    , PG.Plain (putCharUtf8 ',')
    , toField tofRationale
    , PG.Plain (putCharUtf8 ',')
    , toField tofDifficulty
    , PG.Plain (putCharUtf8 ',')
    , toField tofReference
    , PG.Plain (putCharUtf8 ',')
    , toField tofLearningObjectives
    , PG.Plain (putCharUtf8 ',')
    , toField tofNationalStandards
    , PG.Plain (putCharUtf8 ',')
    , toField tofTopics
    , PG.Plain (putCharUtf8 ',')
    , toField $ PGArray tofKeyWords
    ]

-- Gap filling
data GapFilling = GapFilling
                  { gfBody :: String
                  , gfAnswer :: String
                  , gfDifficulty :: Maybe String
                  , gfReference :: Maybe Int
                  , gfLearningObjectives :: Maybe String
                  , gfNationalStandards :: Maybe String
                  , gfTopics :: Maybe String
                  , gfKeyWords :: [String]
                  }
                  deriving (Show,Eq)

instance ToField GapFilling where
  toField GapFilling{..} = Many
    [ toField gfBody
    , PG.Plain (putCharUtf8 ',')
    , toField gfAnswer
    , PG.Plain (putCharUtf8 ',')
    , toField gfDifficulty
    , PG.Plain (putCharUtf8 ',')
    , toField gfReference
    , PG.Plain (putCharUtf8 ',')
    , toField gfLearningObjectives
    , PG.Plain (putCharUtf8 ',')
    , toField gfNationalStandards
    , PG.Plain (putCharUtf8 ',')
    , toField gfTopics
    , PG.Plain (putCharUtf8 ',')
    , toField $ PGArray gfKeyWords
    ]

-- Multiple Choice
data MultipleChoice = MultipleChoice
                    { mcBody :: String
                    , mcAnswer :: Int
                    , mcChoices :: [String]
                    }
                    deriving (Show,Eq)

instance ToField MultipleChoice where
  toField MultipleChoice{..} = Many
    [ toField mcBody
    , PG.Plain (putCharUtf8 ',')
    , toField mcAnswer
    , PG.Plain (putCharUtf8 ',')
    , toField $ PGArray mcChoices
    ]

replace160 :: String -> String
replace160 = map (\x -> if x == '\160' then ' ' else x)


loadFileWithDocx :: FilePath -> IO (Either PandocError Pandoc)
loadFileWithDocx fp = runIO . readDocx def =<< BL.readFile fp

renderText :: [Inline] -> String
renderText = concat . map renderTextStep
  where renderTextStep (Code       _ str) = str
        renderTextStep (Str          str) = str
        renderTextStep (Math       _ str) = str
        renderTextStep (RawInline  _ str) = str
        renderTextStep (Emph          il) = renderText il
        renderTextStep (Strong        il) = renderText il
        renderTextStep (Strikeout     il) = renderText il
        renderTextStep (Superscript   il) = renderText il
        renderTextStep (Subscript     il) = renderText il
        renderTextStep (SmallCaps     il) = renderText il
        renderTextStep (Quoted      _ il) = renderText il
        renderTextStep (Cite        _ il) = renderText il
        renderTextStep (Span        _ il) = renderText il
        renderTextStep (Link      _ il _) = renderText il
        renderTextStep (Image     _ il _) = renderText il
        renderTextStep Space              = " "
        renderTextStep SoftBreak          = "\n"
        renderTextStep LineBreak          = "\n"
        renderTextStep _                  = ""

fetchTOFInfo :: Block -> TrueOrFalse ->  TrueOrFalse
fetchTOFInfo (Table _ _ _ bs' bs) tof =
  let sets = map parseTOFSet $  bs' :  bs
  in foldl' (\tof fun -> fun tof) tof sets
fetchTOFInfo x _ = error $ "inter error" ++ show x


fetchTOFBody :: Block -> TrueOrFalse -> TrueOrFalse
fetchTOFBody (Para il) tof =
  let body = parseBody $ renderText il
  in case body of
    Right (_,b) -> tof {tofBody = b}
    Left  _ -> tof

fetchGFInfo :: Block -> GapFilling -> GapFilling
fetchGFInfo (Table _ _ _ bs' bs) gf =
  let sets = map parseGFSet $ bs' : bs
  in foldl' (\gf fun -> fun gf) gf sets
fetchGFInfo x _ = error $ "inter error" ++ show x

fetchGFBody :: Block -> GapFilling -> GapFilling
fetchGFBody (Para il) gf =
  let body = parseBody $ renderText il
  in case body of
    Right (_,b) -> gf {gfBody = b}
    Left  _ -> gf

parserTOFSet' :: Stream s m Char => ParsecT s u m ([String] -> TrueOrFalse -> TrueOrFalse)
parserTOFSet' = do
  skipMany (char '\160' <|> space)
  key <- many (noneOf ":")
  return $ case key of
    "answer"              -> \str tof -> tof {tofAnswer = read (head str)}
    "rationale"           -> \str tof -> tof {tofRationale = Just (head str)}
    "difficulty"          -> \str tof -> tof {tofDifficulty = Just (head str)}
    "references"          -> \str tof -> tof {tofReference = parseReference (head str)}
    "learning objectives" -> \str tof -> tof {tofLearningObjectives = Just (head str)}
    "national standards"  -> \str tof -> tof {tofNationalStandards = Just (head str)}
    "topics"              -> \str tof -> tof {tofTopics = Just (head str)}
    "keywords"            -> \str tof -> tof {tofKeyWords = str}
    _                     -> \_   tof -> tof

parseTOFSet :: [[Block]] -> (TrueOrFalse -> TrueOrFalse)
parseTOFSet bs =
  let (Plain key'  ) = head $ bs !! 0
      (Plain value') = head $ bs !! 1
      key   = map toLower $ renderText key'
      value = renderText value'
      rt = parse parserTOFSet' "function parseTOFSet" $ replace160 key
  in case rt of
    Right f -> f [value]
    Left i  -> id

parserGFSet' :: Stream s m Char => ParsecT s u m ([String] -> GapFilling -> GapFilling)
parserGFSet' = do
  skipMany (char '\160' <|> space)
  key <- many (noneOf ":")
  return $ case key of
    "answer"              -> \str gf -> gf {gfAnswer = head str}
    "difficulty"          -> \str gf -> gf {gfDifficulty = Just (head str)}
    "references"          -> \str gf -> gf {gfReference = parseReference (head str)}
    "learning objectives" -> \str gf -> gf {gfLearningObjectives = Just (head str)}
    "national standards"  -> \str gf -> gf {gfNationalStandards = Just (head str)}
    "topics"              -> \str gf -> gf {gfTopics = Just (head str)}
    "keywords"            -> \str gf -> gf {gfKeyWords = str}
    _                     -> \_   gf -> gf

parseGFSet :: [[Block]] -> (GapFilling -> GapFilling)
parseGFSet bs =
  let (Plain key'  ) = head $ bs !! 0
      (Plain value') = head $ bs !! 1
      key   = map toLower $ renderText key'
      value = renderText value'
      rt = parse parserGFSet' "function parseTOFSet" $ replace160 key
  in case rt of
    Right f -> f [value]
    Left i  -> id


parserReference' :: Stream s m Char => ParsecT s u m Int
parserReference' = do
  skipMany (char '\160' <|> space)
  char 'p'
  skipMany (char '\160' <|> space)
  char '.'
  skipMany (char '\160' <|> space)
  digits <- many1 digit
  return $ read digits


parseReference :: String -> Maybe Int
parseReference str =
  let rt = parse parserReference' "function parseReference" $ replace160 str
  in case rt of
    Right i -> Just i
    Left _ -> Nothing

parserBody' :: Stream s m Char => ParsecT s u m (Int,String)
parserBody' = do
  spaces
  pid <- read <$> many1 digit
  char '.'
  skipMany (char '\160' <|> space)
  body <- many anyChar
  return (pid,body)

parseBody :: String -> Either ParseError (Int, String)
parseBody = parse parserBody' "function parseBody" . replace160


toTOF :: Block -> TrueOrFalse
toTOF (Table _ _ _ _ bs') =
  let bs = head $ head bs'
      body = bs !! 0
      info = bs !! 2
      defTOF = TrueOrFalse
               { tofBody = ""
               , tofAnswer = True
               , tofRationale = Nothing
               , tofDifficulty = Nothing
               , tofReference = Nothing
               , tofLearningObjectives = Nothing
               , tofNationalStandards = Nothing
               , tofTopics = Nothing
               , tofKeyWords = []
               }
  in fetchTOFBody body $ fetchTOFInfo info defTOF


toGF :: Block -> GapFilling
toGF (Table _ _ _ _ bs') =
  let bs = head $ head bs'
      body = bs !! 0
      info = bs !! 1
      defGF = GapFilling
              { gfBody = ""
              , gfAnswer = ""
              , gfDifficulty = Nothing
              , gfReference = Nothing
              , gfLearningObjectives = Nothing
              , gfNationalStandards = Nothing
              , gfTopics = Nothing
              , gfKeyWords = []
              }
  in fetchGFBody body $ fetchGFInfo info defGF


updateTOF :: PG.Connection -> TrueOrFalse -> IO ()
updateTOF conn tof = do
  PG.execute conn [sql|
                   INSERT INTO table_true_or_false(
                       key_body, key_answer, key_rationale, key_difficulty,
                       key_references, key_learning_objectives, key_national_standards,
                       key_topics, key_words)
                   VALUES (?)
                   |]
    (Only tof)
  return ()

updateGF :: PG.Connection -> GapFilling -> IO ()
updateGF conn gf = do
  PG.execute conn [sql|
                      INSERT INTO table_gap_filling(
                          key_body, key_answer, key_difficulty, key_references,
                          key_learning_objectives, key_national_standards,
                          key_topics, key_words)
                      VALUES (?)
                      |]
    (Only gf)
  return ()



data MultipleChoiceContext
  = MCCQuestBody String
  | MCCQuestHead String
  | MCCQuestItem Int String
  | MCCNull
  deriving (Show,Eq)


parserMCQuestBody :: Stream s m Char => ParsecT s u m (Int,String)
parserMCQuestBody = do
  skipMany $ noneOf ['A'..'Z']
  ans <- (\x -> x - ord 'A') . ord <$> oneOf ['A'..'Z']
  str <- many anyChar
  return (ans, reverse str)

parserMCQuestHead :: Stream s m Char => ParsecT s u m MultipleChoiceContext
parserMCQuestHead = do
  spaces
  string "Question"
  spaces
  many digit
  spaces
  char ':'
  MCCQuestHead <$> many1 anyChar

parserMCQuestRest :: Stream s m Char => ParsecT s u m MultipleChoiceContext
parserMCQuestRest = MCCQuestBody <$> many1 anyChar

parserMCQuestItem :: Stream s m Char => ParsecT s u m MultipleChoiceContext
parserMCQuestItem = do
  item <- (\x -> x - ord 'A') . ord <$> oneOf ['A'..'Z']
  char ':'
  spaces
  str <- many1 anyChar
  return $ MCCQuestItem item str
  
parserMC :: Stream s m Char => ParsecT s uP m MultipleChoiceContext
parserMC = try parserMCQuestHead <|> try parserMCQuestItem <|> parserMCQuestRest

parseMCBody :: [(Int,String)] -> String -> MultipleChoiceProb
parseMCBody c b =
  let (Right (ans,body)) = parse parserMCQuestBody "function parseMCBody" $ replace160 $ reverse b
  in MultipleChoiceProb body ans c

parseMCfBlock :: Block -> MultipleChoiceContext
parseMCfBlock (Para il) =
  let body = renderText il
      rt   = parse parserMC "function parseMCBlock" $ replace160 body
  in case rt of
    Right i -> i
    Left _ -> MCCNull
    
data MultipleChoiceProb = MultipleChoiceProb
                        { mcbBody :: String
                        , mcbAns  :: Int
                        , mcbChos :: [(Int,String)]
                        }
                        deriving (Show,Eq)

data MCCInter = MCCInterNull
              | MCCInterItem String [(Int,String)]
              deriving (Show,Eq)

toMCPfMCCStep :: MCCInter -> [MultipleChoiceContext] -> [MultipleChoiceProb]
toMCPfMCCStep MCCInterNull [] = []
toMCPfMCCStep (MCCInterItem b c) [] = [parseMCBody c b]
toMCPfMCCStep a (MCCNull:mccs) = toMCPfMCCStep a mccs
toMCPfMCCStep MCCInterNull (MCCQuestHead b:mccs) = toMCPfMCCStep (MCCInterItem b [])  mccs
toMCPfMCCStep MCCInterNull (MCCQuestBody b:mccs) = toMCPfMCCStep (MCCInterItem b [])  mccs
toMCPfMCCStep (MCCInterItem b cs'') (MCCQuestBody cb:mccs) =
  if null cs''
  then toMCPfMCCStep (MCCInterItem (b++cb) cs'') mccs
  else
    let ((i,c):cs) = cs''
        c' = (i,c++cb)
        cs' = c':cs
    in toMCPfMCCStep (MCCInterItem b cs') mccs
toMCPfMCCStep (MCCInterItem b cs) (MCCQuestHead b':mccs) = parseMCBody cs b:toMCPfMCCStep (MCCInterItem b' []) mccs
toMCPfMCCStep (MCCInterItem b cs) (MCCQuestItem i c:mccs) = toMCPfMCCStep (MCCInterItem b ((i,c):cs)) mccs
toMCPfMCCStep _ (_:mccs) = toMCPfMCCStep MCCInterNull mccs
toMCPfMCC :: [MultipleChoiceContext] -> [MultipleChoiceProb]
toMCPfMCC = toMCPfMCCStep MCCInterNull

toMCfMCP :: MultipleChoiceProb -> MultipleChoice
toMCfMCP (MultipleChoiceProb b a cs) = MultipleChoice b a $ map snd $ sort cs

updateMC :: PG.Connection -> MultipleChoice -> IO ()
updateMC conn mc = do
  PG.execute conn [sql|
                      INSERT INTO table_multiple_choice(
                        key_body, key_answer, key_choices)
                      VALUES (?)
                      |]
    (Only mc)
  return ()
