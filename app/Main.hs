{-# LANGUAGE TypeApplications #-}

module Main (main) where

import Control.Concurrent (threadDelay)
import Control.Exception (throwIO)
import qualified DBus
import DBus.Client (MatchRule)
import qualified DBus.Client as DBus
import qualified DBus.Notify as Notify
import Data.Aeson (FromJSON, ToJSON, (.:), (.:?), (.=))
import qualified Data.Aeson as Aeson
import qualified Data.List as List
import Network.HTTP.Req (POST (..), (/:))
import qualified Network.HTTP.Req as Req
import Paths_habiticad
import System.Exit (ExitCode (ExitSuccess))
import System.IO (BufferMode (LineBuffering), hSetBuffering)
import System.IO.Error (userError)
import qualified System.Process as Process

-- CONSTANTS --

habiticaApi :: Req.Url 'Req.Https
habiticaApi = Req.https "habitica.com" /: "api" /: "v3"

-- DATA TYPES --

data Difficulty
  = Trivial
  | Easy
  | Medium
  | Hard
  deriving stock (Show, Eq, Ord)

difficultyToNumber :: Difficulty -> Double
difficultyToNumber = \case
  Trivial -> 0.1
  Easy -> 1
  Medium -> 1.5
  Hard -> 2

data Todo = Todo
  { todoText :: Text,
    todoDifficulty :: Difficulty
  }
  deriving stock (Show, Eq, Ord)

instance ToJSON Todo where
  toJSON Todo {todoText, todoDifficulty} =
    Aeson.object
      [ "text" .= todoText,
        "type" .= ("todo" :: Text),
        "priority" .= difficultyToNumber todoDifficulty
      ]

newtype TodoId = TodoId Text deriving stock (Show, Eq, Ord)

instance FromJSON TodoId where
  parseJSON = Aeson.withObject "TodoId" $ \o -> TodoId <$> o .: "id"

data HabiticaResponse body
  = Failure Text
  | Data body
  deriving stock (Show, Eq, Ord)

instance FromJSON body => FromJSON (HabiticaResponse body) where
  parseJSON = Aeson.withObject "HabiticaResponse" $ \o -> do
    ifM
      (o .: "success")
      (Data <$> o .: "data")
      (Failure <$> o .: "message")

data HabiticaHeaders = HabiticaHeaders
  { headersUserId :: ByteString,
    headersApiKey :: ByteString
  }
  deriving stock (Eq, Ord, Show)

data QuestProgress = QuestProgress
  { qpCollection :: Maybe Double,
    qpDamage :: Maybe Double
  }
  deriving stock (Show, Eq, Ord)

instance FromJSON QuestProgress where
  parseJSON = Aeson.withObject "QuestProgress" $ \o -> do
    qpCollection <- o .:? "collection"
    qpDamage <- o .:? "progressDelta"
    pure QuestProgress {qpCollection, qpDamage}

data ScoreResult = ScoreResult
  { scoreDrop :: Maybe Text,
    scoreQuestProgress :: Maybe QuestProgress
  }
  deriving stock (Show, Eq, Ord)

instance FromJSON ScoreResult where
  parseJSON = Aeson.withObject "ScoreResult" $ \o -> do
    mbTmp <- o .:? "_tmp"
    case mbTmp of
      Nothing -> pure $ ScoreResult Nothing Nothing
      Just tmp -> do
        mbDrop <- tmp .:? "drop"
        scoreDrop <- case mbDrop of
          Nothing -> pure Nothing
          Just hdrop -> Just <$> hdrop .: "dialog"
        scoreQuestProgress <- tmp .:? "quest"
        pure ScoreResult {scoreDrop, scoreQuestProgress}

headersToOption :: HabiticaHeaders -> Req.Option 'Req.Https
headersToOption HabiticaHeaders {headersUserId, headersApiKey} =
  mconcat
    [ Req.header "x-client" (headersUserId <> "-habiticad"),
      Req.header "x-api-user" headersUserId,
      Req.header "x-api-key" headersApiKey
    ]

newtype NotifyBody = NotifyBody {unNotifyBody :: Notify.Body}
  deriving newtype (Show, Eq)

instance Semigroup NotifyBody where
  (NotifyBody b1) <> (NotifyBody b2) = NotifyBody (Notify.Concat b1 b2)

instance IsString NotifyBody where
  fromString = NotifyBody . Notify.Text

mapNotifyBody :: (Notify.Body -> Notify.Body) -> NotifyBody -> NotifyBody
mapNotifyBody f = NotifyBody . f . unNotifyBody

bold :: NotifyBody -> NotifyBody
bold = mapNotifyBody Notify.Bold

-- MAIN --

notify :: ScoreResult -> IO ()
notify scoreResult = do
  client <- Notify.connectSession
  iconPath <- getDataFileName "assets/images/habitica.png"
  case scoreDrop scoreResult of
    Nothing -> pass
    Just dropText -> void $ Notify.notify client (dropToNote iconPath dropText)
  case scoreQuestProgress scoreResult of
    Nothing -> pass
    Just questProgress -> void $ Notify.notify client (questProgressToNote iconPath questProgress)
  where
    dropToNote iconPath dropText =
      Notify.blankNote
        { Notify.appName = "habitica",
          Notify.summary = "Drop received!",
          Notify.appImage = Just (Notify.File iconPath),
          Notify.body = Just $ Notify.Text (toString dropText)
        }
    questProgressToNote iconPath QuestProgress {qpCollection, qpDamage} =
      Notify.blankNote
        { Notify.appName = "habitica",
          Notify.summary = "Quest Progress",
          Notify.appImage = Just (Notify.File iconPath),
          Notify.body =
            let collected = case qpCollection of
                  Just num -> bold "Collected: " <> prettyNumber num <> "\n"
                  Nothing -> ""
                damage = case qpDamage of
                  Just num -> bold "Damage dealt: " <> prettyNumber num
                  Nothing -> ""
             in Just . unNotifyBody $ collected <> damage
        }

    prettyNumber :: Double -> NotifyBody
    prettyNumber num = show @NotifyBody @Double $ fromIntegral @Int (round (num * 100)) / 100

scoreTaskWithDifficulty :: HabiticaHeaders -> Difficulty -> IO ()
scoreTaskWithDifficulty headers difficulty = Req.runReq httpConfig $ do
  infoLog $ "[Habitica] Creating task with difficulty " <> show difficulty
  res <-
    Req.req
      POST
      (habiticaApi /: "tasks" /: "user")
      (Req.ReqBodyJson todo)
      Req.jsonResponse
      (headersToOption headers)
  case Req.responseBody res of
    Failure msg -> errorLog $ "[Habitica] " <> msg
    Data (TodoId uuid) -> do
      infoLog $ "[Habitica] Task created with UUID " <> uuid
      infoLog "[Habitica] Marking task complete"
      res2 <-
        Req.req
          POST
          (habiticaApi /: "tasks" /: uuid /: "score" /: "up")
          Req.NoReqBody
          Req.jsonResponse
          (headersToOption headers)
      case Req.responseBody res2 of
        Failure msg -> errorLog $ "[Habitica] " <> msg
        Data scoreResult -> do
          infoLog $ "[Habitica] " <> show scoreResult
          liftIO $ notify scoreResult
  where
    todo =
      Todo
        { todoText = "Some " <> show difficulty <> " task",
          todoDifficulty = difficulty
        }
    httpConfig =
      Req.defaultHttpConfig
        { Req.httpConfigCheckResponse = \_ _ _ -> Nothing
        }

onHabitProgress :: HabiticaHeaders -> DBus.Signal -> IO ()
onHabitProgress headers signal = case DBus.signalMember signal of
  "OnProgress" -> scoreTaskWithDifficulty headers Trivial
  "OnCompleted" -> scoreTaskWithDifficulty headers Easy
  "OnAllCompleted" -> scoreTaskWithDifficulty headers Hard
  _ -> pass

readHabiticaHeaders :: IO HabiticaHeaders
readHabiticaHeaders = do
  infoLog "Looking up Habitica headers"
  (gotUserId, userId, _) <- Process.readProcessWithExitCode "pass" ["websites/habitica.com/user-id"] ""
  (gotApiKey, apiKey, _) <- Process.readProcessWithExitCode "pass" ["websites/habitica.com/api-key"] ""
  if all (== ExitSuccess) [gotUserId, gotApiKey]
    then pure $ HabiticaHeaders (fromString $ stripNewline userId) (fromString $ stripNewline apiKey)
    else do
      errorLog "Unable to read Habitica auth headers"
      throwIO (userError "Unable to read Habitica auth headers")
  where
    stripNewline :: String -> String
    stripNewline = concat . filter (/= "") . List.lines

infoLog :: MonadIO m => Text -> m ()
infoLog txt = liftIO $ putTextLn $ "[INFO] " <> txt

errorLog :: MonadIO m => Text -> m ()
errorLog txt = liftIO $ putTextLn $ "[ERR]  " <> txt

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  infoLog "Starting up"
  infoLog "Connecting to DBus"
  client <- DBus.connectSession
  headers <- readHabiticaHeaders
  infoLog "Adding listener for DBus signals from habit script"
  _ <- DBus.addMatch client matchHabit (onHabitProgress headers)
  infoLog "Looping forever"
  _ <- forever (threadDelay 1000000)
  infoLog "Shutting down"
  where
    matchHabit :: MatchRule
    matchHabit =
      DBus.matchAny {DBus.matchInterface = Just "rhit.habits.progress"}
