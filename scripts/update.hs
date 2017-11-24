#!env runhaskell

{-# LANGUAGE PackageImports #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

import "fluffy-parser" Fluffy.Parser
import "postgresql-simple" Database.PostgreSQL.Simple
import "base" System.Environment
import "pandoc" Text.Pandoc
import  qualified "bytestring" Data.ByteString.Char8 as BC8
import "monad-logger" Control.Monad.Logger
import "base" Control.Monad.IO.Class
import "fast-logger" System.Log.FastLogger
import qualified "text" Data.Text as T


readUpdateTOF :: MonadIO m => Connection -> FilePath -> (Int,Int) -> m ()
readUpdateTOF c fp (d,t) = do
  Right (Pandoc _ bs) <- liftIO $ loadFileWithDocx fp
  let tofb = take t $ drop d bs
      tofs = map toTOF tofb
  mapM_ (liftIO.updateTOF c) tofs

readUpdateGF :: MonadIO m => Connection -> FilePath -> (Int,Int) -> m ()
readUpdateGF c fp (d,t) = do
  Right (Pandoc _ bs) <- liftIO $ loadFileWithDocx fp
  let gfb = take t $ drop d bs
      gfs = map toGF gfb
  mapM_ (liftIO.updateGF c) gfs


readUpdateMC :: MonadIO m => Connection -> FilePath -> m ()
readUpdateMC c fp = do
  Right (Pandoc _ bs) <- liftIO $ loadFileWithDocx fp
  let mccs = map parseMCfBlock bs
      mcps = toMCPfMCC $ concat mccs
      mcs  = map toMCfMCP mcps
  mapM_ (liftIO.updateMC c) mcs


newtype LIO a =
  LIO { fromLIO :: LoggingT IO a
      }
  deriving (Applicative,Functor,Monad,MonadIO,MonadLogger)

runLIO :: LIO a -> IO a
runLIO lio = (runLoggingT (fromLIO lio))
  (\loc src level msg -> BC8.putStr $ fromLogStr $ defaultLogStr loc src level msg)


main :: IO ()
main = runLIO $ do
  $(logInfo) "Test"
  prefix':connStr <- liftIO $ getArgs
  $(logInfo) $ T.pack $ "prefix: " ++ prefix'
  $(logInfo) $ T.pack $ "connection string" ++ show connStr
  c <- liftIO $ connectPostgreSQL $ BC8.pack $ unwords connStr
  $(logInfo) "Connected"
  let prefix x = prefix' ++ x
      cha1  = prefix "/TrueFalse/Chapter_1_Introduction_to_Project_Management.docx"
      cha2  = prefix "/TrueFalse/Chapter_2_The_Project_Management_and_Information_Technology_Context.docx"
      cha3  = prefix "/TrueFalse/Chapter_3_The_Project_Management_Process_Groups_A_Case_Study.docx"
      mc1   = prefix "/MultipleChoice/Functional.docx"
      mc2   = prefix "/MultipleChoice/Time.docx"
      mc3   = prefix "/MultipleChoice/Quality.docx"
      mc4   = prefix "/MultipleChoice/Scope.docx"
      mc5   = prefix "/MultipleChoice/HumanResource.docx"
      mc6   = prefix "/MultipleChoice/Cost.docx"
      mc7   = prefix "/MultipleChoice/Communication.docx"
      mc8   = prefix "/MultipleChoice/Integration.docx"
  $(logInfo) "to update tof cha1"
  readUpdateTOF c cha1 (1,31)
  $(logInfo) "to update tof cha2"
  readUpdateTOF c cha2 (1,26)
  $(logInfo) "to update tof cha3"
  readUpdateTOF c cha3 (1,25)
  $(logInfo) "to update gf cha1"
  readUpdateGF  c cha1 (33,24)
  $(logInfo) "to update gf cha2"
  readUpdateGF  c cha2 (28,20)
  $(logInfo) "to update gf cha3"
  readUpdateGF  c cha3 (27,21)
  $(logInfo) "to update mc1"
  readUpdateMC  c mc1
  $(logInfo) "to update mc2"
  readUpdateMC  c mc2
  $(logInfo) "to update mc3"
  readUpdateMC  c mc3
  $(logInfo) "to update mc4"
  readUpdateMC  c mc4
  $(logInfo) "to update mc5"
  readUpdateMC  c mc5
  $(logInfo) "to update mc6"
  readUpdateMC  c mc6
  $(logInfo) "to update mc7"
  readUpdateMC  c mc7
  $(logInfo) "to update mc8"
  readUpdateMC  c mc8
