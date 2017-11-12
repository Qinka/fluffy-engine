{-# LANGUAGE ViewPatterns      #-}
{-# LANGUAGE MultiWayIf        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE RecordWildCards   #-}


module Main where

import           Control.Monad
import           Control.Monad.Random
import           Data.Aeson
import qualified Data.ByteString       as B hiding (pack, unpack)
import qualified Data.ByteString.Char8 as B (pack, unpack)
import           Data.Char
import           Data.Maybe
import           Data.Pool
import qualified Data.Text             as T
import qualified Data.Text.Encoding    as TE
import           Data.Time
import           Database.PostgreSQL.Simple
import           Database.PostgreSQL.Simple.FromRow
import           Database.PostgreSQL.Simple.SqlQQ
import           Database.PostgreSQL.Simple.Types
import           System.Environment
import           Yesod.Core
import           Yesod.Core.Json
import           Yesod.Core.Content


-- Site
data Fluffy = Fluffy
              { fluffyConnectionPool :: Pool Connection
              }

-- True or False
data TrueOrFalse = TrueOrFalse
                   { tofId :: Int
                   , tofBody :: T.Text
                   , tofAnswer :: Bool
                   , tofRationale :: Maybe T.Text
                   , tofDifficulty :: Maybe T.Text
                   , tofReference :: Maybe Int
                   , tofLearningObjectives :: Maybe T.Text
                   , tofNationalStandards :: Maybe T.Text
                   , tofTopics :: Maybe T.Text
                   , tofKeyWords :: [T.Text]
                   }
                   deriving (Show,Eq)

-- Gap filling
data GapFilling = GapFilling
                  { gfId :: Int
                  , gfBody :: T.Text
                  , gfAnswer :: T.Text
                  , gfDifficulty :: Maybe T.Text
                  , gfReference :: Maybe Int
                  , gfLearningObjectives :: Maybe T.Text
                  , gfNationalStandards :: Maybe T.Text
                  , gfTopics :: Maybe T.Text
                  , gfKeyWords :: [T.Text]
                  }
                  deriving (Show,Eq)

-- Multiple Choice
data MultipleChoice = MultipleChoice
                    { mcId :: Int
                    , mcBody :: T.Text
                    , mcAnswer :: Int
                    , mcChoices :: [T.Text]
                    }
                    deriving (Show,Eq)

instance FromRow TrueOrFalse where
  fromRow = TrueOrFalse
    <$> field
    <*> field
    <*> field
    <*> field
    <*> field
    <*> field
    <*> field
    <*> field
    <*> field
    <*> (fromPGArray <$> field)

instance FromRow GapFilling where
  fromRow = GapFilling
    <$> field
    <*> field
    <*> field
    <*> field
    <*> field
    <*> field
    <*> field
    <*> field
    <*> (fromPGArray <$> field)

instance FromRow MultipleChoice where
  fromRow = MultipleChoice
    <$> field
    <*> field
    <*> field
    <*> (fromPGArray <$> field)


mkYesod "Fluffy" [parseRoutes|
/                            HomeR              GET
/true-or-false/#Int          TrueOrFalseR       GET
/gap-filling/#Int            GapFillingR        GET
/multiple-choice/#Int        MultipleChoiceR    GET
/.clean-history              CleanHistory       GET
|]

instance Yesod Fluffy


-- home page
getHomeR :: Handler Html
getHomeR = defaultLayout
  [whamlet|
          <h1> Fluffy Engine
          <p> Hi
          <p> <a href=@{TrueOrFalseR 0}> Go to True or False
          <p> <a href=@{GapFillingR 0}> Go to Gap Filling
          <p> <a href=@{MultipleChoiceR 0}> Go to Multiple Choice
  |]

getTrueOrFalseR :: Int -> Handler Html
getTrueOrFalseR i = do
  answer <- lookupGetParam "answer"
  cp  <- fluffyConnectionPool <$> getYesod
  qrt <- liftIO $ withResource cp $ \c -> do
    query c [sql|
                SELECT key_id, key_body, key_answer, key_rationale, key_difficulty,
                       key_references, key_learning_objectives, key_national_standards,
                       key_topics, key_words
                FROM table_true_or_false
                WHERE key_id = ?
                |]
      (Only i)
  when (null qrt) notFound
  let TrueOrFalse{..} = head qrt
  goal'  <- fromMaybe 0 . fmap (read . T.unpack) <$> lookupSession "goal"
  total' <- fromMaybe 0 . fmap (read . T.unpack) <$> lookupSession "total"
  (goal, total, con) <- 
    case answer of
      Just x -> do
        let rt = (x == "t") == tofAnswer
            goal  = goal'  + (if rt then 1 else 0)
            total = total' + 1
        setSession "goal" $ T.pack $ show $ goal
        setSession "total" $ T.pack $ show $ total
        return (goal, total,
          [whamlet|
                  <h3> Answer #{show rt}
                  <p> #{show tofAnswer}
                  $maybe ra <- tofRationale
                    <h3> Reationale
                    <p> #{ra}
                  $maybe d <- tofDifficulty
                    <h3> Difficluty
                    <p> #{d}
                  $maybe r <- tofReference
                    <h3> Reference
                    <p> p.#{show r}
                  $maybe l <- tofLearningObjectives
                    <h3> Learning Objectives
                    <p> #{l}
                  $maybe n <- tofNationalStandards
                    <h3> National Standards
                    <p> #{n}
                  $if not (null tofKeyWords)
                    <h3> Key Words
                    <ul>
                    $forall kw <- tofKeyWords
                     <li> #{kw}
                  $if not (null tofTopics)
                    <h3> Topics
                    <ul>
                    $forall t <- tofTopics
                     <li> #{t}
                |])
      Nothing -> return (goal', total',
        [whamlet|
                <form>
                  <input type=radio name=answer value=t checked> True
                  <input type=radio name=answer value=f>         False
                  <input type=submit value=Check>
                |])
  defaultLayout $ do
    [whamlet|
            <h3> Goal / Total
            <p> #{show goal} / #{show total}
            <h1> True or False
            <h2> Problem #{show i}
            <h2> #{tofBody}
            |]
    con


getGapFillingR :: Int -> Handler Html
getGapFillingR i = do
  answer <- lookupGetParam "answer"
  cp  <- fluffyConnectionPool <$> getYesod
  qrt <- liftIO $ withResource cp $ \c -> do
    query c [sql|
                SELECT key_id, key_body, key_answer, key_difficulty,
                       key_references, key_learning_objectives, key_national_standards,
                       key_topics, key_words
                FROM table_gap_filling
                WHERE key_id = ?
                |]
      (Only i)
  when (null qrt) notFound
  let GapFilling{..} = head qrt
  goal'  <- fromMaybe 0 . fmap (read . T.unpack) <$> lookupSession "goal"
  total' <- fromMaybe 0 . fmap (read . T.unpack) <$> lookupSession "total"
  (goal, total, con) <- 
    case answer of
      Just x' -> do
        let x = T.reverse . T.dropWhile (==' ') . T.reverse $ T.dropWhile (== ' ') x'
            rt = x == gfAnswer
            goal  = goal'  + (if rt then 1 else 0)
            total = total' + 1
        setSession "goal" $ T.pack $ show $ goal
        setSession "total" $ T.pack $ show $ total
        return (goal, total,
          [whamlet|
                  <h3> Answer #{show rt}
                  <p> #{show gfAnswer}
                  $maybe d <- gfDifficulty
                    <h3> Difficluty
                    <p> #{d}
                  $maybe r <- gfReference
                    <h3> Reference
                    <p> p.#{show r}
                  $maybe l <- gfLearningObjectives
                    <h3> Learning Objectives
                    <p> #{l}
                  $maybe n <- gfNationalStandards
                    <h3> National Standards
                    <p> #{n}
                  $if not (null gfKeyWords)
                    <h3> Key Words
                    <ul>
                    $forall kw <- gfKeyWords
                     <li> #{kw}
                  $if not (null gfTopics)
                    <h3> Topics
                    <ul>
                    $forall t <- gfTopics
                     <li> #{t}
                |])
      Nothing -> return (goal', total',
        [whamlet|
                <form>
                  <input type=text name=answer checked>
                  <input type=submit value=Check>
                |])
  defaultLayout $ do
    [whamlet|
            <h3> Goal / Total
            <p> #{show goal} / #{show total}
            <h1> Gap Filling
            <h2> Problem #{show i}
            <h2> #{gfBody}
            |]
    con


getMultipleChoiceR :: Int -> Handler Html
getMultipleChoiceR i =  do
  answer <- lookupGetParam "answer"
  cp  <- fluffyConnectionPool <$> getYesod
  qrt <- liftIO $ withResource cp $ \c -> do
    query c [sql|
                SELECT key_id, key_body, key_answer, key_choices
                FROM table_multiple_choice
                WHERE key_id = ?
                |]
      (Only i)
  when (null qrt) notFound
  let MultipleChoice{..} = head qrt
      toId  = T.pack . show
      toAns = T.pack . pure . chr . (+ ord 'A')
      mixAns = zip [0..]
  goal'  <- fromMaybe 0 . fmap (read . T.unpack) <$> lookupSession "goal"
  total' <- fromMaybe 0 . fmap (read . T.unpack) <$> lookupSession "total"
  (goal, total, con) <- 
    case answer of
      Just x' -> do
        let x = read $ T.unpack x'
            rt = x == mcAnswer
            goal  = goal'  + (if rt then 1 else 0)
            total = total' + 1
        setSession "goal" $ T.pack $ show $ goal
        setSession "total" $ T.pack $ show $ total
        return (goal, total,
          [whamlet|
                  <h3> Answer #{show rt}
                  <p> #{toAns mcAnswer}
                  <h3> Choices
                  <ul>
                    $forall (l,c) <- mixAns mcChoices
                      <li> #{toAns l}: #{c}
                |])
      Nothing -> return (goal', total',
        [whamlet|
                <form>
                  $forall (l,c) <- mixAns mcChoices
                    <p> <input type=radio name=answer value=#{toId l}> #{toAns l}: #{c}
                  <input type=submit value=Check>
                |])
  defaultLayout $ do
    [whamlet|
            <h3> Goal / Total
            <p> #{show goal} / #{show total}
            <h1> Multiple Choice
            <h2> Problem #{show i}
            <h2> #{mcBody}
            |]
    con


getCleanHistory :: Handler Html
getCleanHistory = do
  deleteSession "goal"
  deleteSession "total"
  redirect HomeR

main :: IO ()
main = do
  port':dbHost' <- getArgs
  let dbHost = unwords dbHost'
      port   = read port'
  pgPool <-
    createPool
    (connectPostgreSQL $ B.pack dbHost)
    close
    100
    1000
    200
  warp port $ Fluffy pgPool
