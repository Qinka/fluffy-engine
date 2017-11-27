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
import           Paths_fluffy
import           Yesod.Static


-- Site
data Fluffy = Fluffy
              { fluffyConnectionPool :: Pool Connection
              , fluffyStatic         :: Static
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
/static                      StaticR            Static fluffyStatic
/.clean-history              CleanHistory       GET
|]

instance Yesod Fluffy where
  makeSessionBackend site = pure <$> defaultClientSessionBackend (7 * 24 * 60)  "client_session_key.aes"
  defaultLayout widget = do
    pc <- widgetToPageContent widget
    withUrlRenderer
      [hamlet|
              $doctype 5
              <html>
                <head>
                  <title> #{pageTitle pc}
                  <meta charset=utf-8>
                  <meta name=viewport content="width=device-width,initial-scale=1.0,maximum-scale=1.0,user-scalable=no">
                  <script src=/static/prelude.js>
                  ^{pageHead pc}
                <body>
                  <div id=fluffybody>
                    ^{pageBody pc}
              |]

-- home page
getHomeR :: Handler Html
getHomeR = do
  goal  <- fromMaybe 0 . fmap (read . T.unpack) <$> lookupSession "goal"
  total <- fromMaybe 0 . fmap (read . T.unpack) <$> lookupSession "total"
  defaultLayout $ do
    setTitle "Fluffy Engine"
    pageAbove Nothing Nothing goal total
    [whamlet|
            <div class=fehead>
              <h1> Fluffy Engine
              <p> Hi
            <div class=tofl>
              <a href=@{TrueOrFalseR 1}> Go to True or False
            <div class=gfl>
              <a href=@{GapFillingR 1}> Go to Gap Filling
            <div class=mcl>
              <a href=@{MultipleChoiceR 1}> Go to Multiple Choice
           |]

data TOFRes = TOFProb
            | TOFForm Bool
            deriving (Show,Eq)

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
        return (goal, total, TOFForm rt)
      Nothing -> return (goal', total', TOFProb)
  let last = if i > 0 then TrueOrFalseR (i-1) else HomeR
      next = TrueOrFalseR (i+1)
  setSession "goal" $ T.pack $ show $ goal
  setSession "total" $ T.pack $ show $ total
  defaultLayout $ do
    setTitle $ toHtml $ T.pack $ "True or False: " ++ show i
    pageAbove (Just last) (Just next) goal total
    [whamlet|
            <div class=tofhead>
              <h3> True or False
            <div class=tofpb>
              <h2> Problem
              <p> #{tofBody}
            |]
    case con of
      TOFProb ->
        [whamlet|
                <div class=tofform>
                  <form>
                    <div class=toftrue>
                      <input type=radio name=answer value=t checked> True
                    <div class=toffalse>
                      <input type=radio name=answer value=f>         False
                    <div class=tofcheck>
                      <input type=submit value=Check>
                |]
      TOFForm rt ->
        [whamlet|
                <div class=tofans>
                  <h3> Answer #{show rt}
                  <p> #{show tofAnswer}
                $maybe ra <- tofRationale
                  <div class=tofrationale>
                    <h3> Rationale
                    <p> #{ra}
                $maybe d <- tofDifficulty
                  <div class=tofdifficulty>
                    <h3> Difficluty
                    <p> #{d}
                $maybe r <- tofReference
                  <div class=tofref>
                    <h3> Reference
                    <p> p.#{show r}
                $maybe l <- tofLearningObjectives
                  <div class=toflo>
                    <h3> Learning Objectives
                    <p> #{l}
                $maybe n <- tofNationalStandards
                  <div class=tofns>
                    <h3> National Standards
                    <p> #{n}
                $if not (null tofKeyWords)
                  <div class=tofkw>
                    <h3> Key Words
                    <ul>
                    $forall kw <- tofKeyWords
                      <li> #{kw}
                $if not (null tofTopics)
                  <div class=toftopic>
                    <h3> Topics
                    <ul>
                    $forall t <- tofTopics
                      <li> #{t}
               |]


data GFRes = GFProb
           | GFForm Bool
           deriving (Show,Eq)

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
            rt = x `elem` T.lines gfAnswer
            goal  = goal'  + (if rt then 1 else 0)
            total = total' + 1
        return (goal, total,GFForm rt)
      Nothing -> return (goal', total', GFProb)
  let last = if i > 0 then GapFillingR (i-1) else HomeR
      next = GapFillingR (i+1)
  setSession "goal" $ T.pack $ show $ goal
  setSession "total" $ T.pack $ show $ total
  defaultLayout $ do
    setTitle $ toHtml $ T.pack $ "Gap Filling: " ++ show i
    pageAbove (Just last) (Just next) goal total
    [whamlet|
            <div class=gfhead>
              <h3> Gap Filling
            <div class=gfpb>
              <h2> Problem
              <p> #{gfBody}
            |]
    case con of
      GFProb ->
        [whamlet|
                <div class=gfform>
                  <form>
                    <div class=gfinput>
                      <input type=text name=answer  autocomplete=off>
                    <div class=gfcheck>
                      <input type=submit value=Check>
                |]
      GFForm rt ->
        [whamlet|
                <div class=gfans>
                  <h3> Answer #{show rt}
                  <p> #{show gfAnswer}
                $maybe d <- gfDifficulty
                  <div class=gfdiff>
                    <h3> Difficluty
                    <p> #{d}
                $maybe r <- gfReference
                  <div class=gfref>
                    <h3> Reference
                    <p> p.#{show r}
                $maybe l <- gfLearningObjectives
                  <div class=gflo>
                    <h3> Learning Objectives
                    <p> #{l}
                $maybe n <- gfNationalStandards
                  <div class=gfns>
                    <h3> National Standards
                    <p> #{n}
                $if not (null gfKeyWords)
                  <div class=gfkw>
                    <h3> Key Words
                    <ul>
                    $forall kw <- gfKeyWords
                      <li> #{kw}
                $if not (null gfTopics)
                  <div class=gftopic>
                    <h3> Topics
                    <ul>
                    $forall t <- gfTopics
                      <li> #{t}
                |]

data MCRes = MCProb
           | MCForm Bool
           deriving (Show,Eq)

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
        return (goal, total, MCForm rt)
      Nothing -> return (goal', total', MCProb)
  setSession "goal" $ T.pack $ show $ goal
  setSession "total" $ T.pack $ show $ total
  let last = if i > 0 then MultipleChoiceR (i-1) else HomeR
      next = MultipleChoiceR (i+1)
  defaultLayout $ do
    setTitle $ toHtml $ T.pack $ "Multiple Choice: " ++ show i
    pageAbove (Just last) (Just next) goal total
    [whamlet|
            <div class=mchead>
              <h3> Multiple Choice
            <div class=mcpb>
              <h2> Problem
              <p> #{mcBody}
            |]
    case con of
      MCProb ->
        [whamlet|
                <div class=mcform>
                  <form>
                    <div class=mccs>
                      $forall (l,c) <- mixAns mcChoices
                        <p> <input type=radio name=answer value=#{toId l}> #{toAns l}: #{c}
                    <div class=mcchecked>
                      <input type=submit value=Check>
                |]
      MCForm rt -> 
        [whamlet|
                <div class=mcans>
                  <h3> Answer #{show rt}
                  <p> #{toAns mcAnswer}
                <div class=mcchoices>
                  <h3> Choices
                  <ul>
                    $forall (l,c) <- mixAns mcChoices
                      <li> #{toAns l}: #{c}
                |]


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
  s <- getDataDir >>= static
  warp port $ Fluffy pgPool s


pageAbove :: (a ~ Route Fluffy, b ~ Route Fluffy)
          => Maybe a -- ^ last page
          -> Maybe b -- ^ next page
          -> Int     -- ^ goal
          -> Int     -- ^ total
          -> WidgetT Fluffy IO ()
pageAbove l n g t=
  [whamlet|
          <div class=navbutton>
            $maybe ll <- l
              <div class=lastbutton>
                <a href=@{ll}> Last
            $maybe nn <- n
              <div class=nextbutton>
                <a href=@{nn}> Next
            <div class=cleanbutton>
              <a href=@{CleanHistory}> Clean History
          <div class=gthead>
            <h3> Goal / Total
            <p> #{show g} / #{show t}
          |]
