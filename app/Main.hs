{-# LANGUAGE TypeApplications #-}

module Main (main) where

import Control.Concurrent (threadDelay)
import Control.Exception (catch, throwIO)
import Control.Monad.Catch (MonadThrow, throwM)
import qualified DBus
import DBus.Client (MatchRule)
import qualified DBus.Client as DBus
import qualified DBus.Notify as Notify
import Data.Aeson (FromJSON, ToJSON, (.:), (.:?), (.=))
import qualified Data.Aeson as Aeson
import qualified Data.List as List
import qualified Data.Text as T
import Network.HTTP.Req (GET (GET), HttpException, POST (..), (/:))
import qualified Network.HTTP.Req as Req
import Paths_habiticad
import qualified System.Directory as Dir
import System.Exit (ExitCode (ExitSuccess))
import System.FilePath ((</>))
import System.IO (BufferMode (LineBuffering), hSetBuffering)
import System.IO.Error (userError)
import qualified System.Process as Process

-- CONSTANTS --

habiticaApi :: Req.Url 'Req.Https
habiticaApi = Req.https "habitica.com" /: "api" /: "v3"

-- DATA TYPES --

data PlayerData = PlayerData
  { playerGold :: Double,
    playerExp :: Double,
    playerLevel :: Int,
    playerExpNeeded :: Maybe Double
  }
  deriving stock (Show, Eq, Ord)

instance FromJSON PlayerData where
  parseJSON = Aeson.withObject "PlayerData" $ \o -> do
    playerGold <- o .: "gp"
    playerExp <- o .: "exp"
    playerLevel <- o .: "lvl"
    playerExpNeeded <- o .:? "toNextLevel"
    pure PlayerData {playerGold, playerExp, playerLevel, playerExpNeeded}

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

data Drop = Drop
  { dropDialog :: Text,
    dropType :: Text,
    dropKey :: Text
  }
  deriving stock (Show, Eq, Ord)

instance FromJSON Drop where
  parseJSON = Aeson.withObject "Drop" $ \o -> do
    dropDialog <- o .: "dialog"
    dropType <- o .: "type"
    dropKey <- o .: "key"
    pure Drop {dropDialog, dropType, dropKey}

data ScoreResult = ScoreResult
  { scoreDrop :: Maybe Drop,
    scoreQuestProgress :: Maybe QuestProgress,
    scorePlayerData :: PlayerData
  }
  deriving stock (Show, Eq, Ord)

instance FromJSON ScoreResult where
  parseJSON = Aeson.withObject "ScoreResult" $ \o -> do
    mbTmp <- o .:? "_tmp"
    (scoreDrop, scoreQuestProgress) <- case mbTmp of
      Nothing -> pure (Nothing, Nothing)
      Just tmp -> do
        scoreDrop <- tmp .:? "drop"
        scoreQuestProgress <- tmp .:? "quest"
        pure (scoreDrop, scoreQuestProgress)
    scorePlayerData <- Aeson.parseJSON (Aeson.Object o)
    pure ScoreResult {scoreDrop, scoreQuestProgress, scorePlayerData}

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

italic :: NotifyBody -> NotifyBody
italic = mapNotifyBody Notify.Italic

-- MAIN --

notify :: PlayerData -> ScoreResult -> IO ()
notify oldPlayerData scoreResult = do
  client <- Notify.connectSession
  sparklePath <- getDataFileName "assets/images/sparkles.png"
  coinPath <- getDataFileName "assets/images/coin.png"
  swordPath <- getDataFileName "assets/images/sword.png"
  let stagger = threadDelay 250000
  case scoreDrop scoreResult of
    Nothing -> pass
    Just theDrop -> do
      dropIconPath <- getDropIconFile theDrop
      void $ Notify.notify client (dropToNote dropIconPath $ dropDialog theDrop)
      stagger
  case scoreQuestProgress scoreResult of
    Nothing -> pass
    Just questProgress -> do
      void $ Notify.notify client (questProgressToNote swordPath questProgress)
      stagger
  void $ Notify.notify client (playerGoldChangeNote coinPath) >> stagger
  void $ Notify.notify client (playerExpChangeNote sparklePath)
  where
    dropToNote iconPath dropText =
      Notify.blankNote
        { Notify.appName = "habitica",
          Notify.summary = "Drop received!",
          Notify.appImage = Just (Notify.File iconPath),
          Notify.body =
            Just (Notify.Text $ toString dropText)
        }

    questProgressToNote iconPath QuestProgress {qpCollection, qpDamage} =
      Notify.blankNote
        { Notify.appName = "habitica",
          Notify.summary = "Quest progress",
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

    double :: Double -> Double
    double n = fromIntegral @Int @Double (floor (n * 100)) / 100

    int :: Double -> Int
    int = floor

    signed :: (Num a, Ord a, Show a) => a -> NotifyBody
    signed n =
      if n >= 0
        then "+" <> show n
        else show n

    unsigned :: (Show a) => a -> NotifyBody
    unsigned = show

    prettyNumber :: Double -> NotifyBody
    prettyNumber num = show @NotifyBody @Double $ fromIntegral @Int (round (num * 100)) / 100

    newPlayerData = scorePlayerData scoreResult
    newLevel = playerLevel newPlayerData

    playerGoldChangeNote iconPath =
      Notify.blankNote
        { Notify.appName = "habitica",
          Notify.summary = "Gold found!",
          Notify.appImage = Just (Notify.File iconPath),
          Notify.body =
            let goldDiffStr =
                  bold "Gold: "
                    <> (signed . double) (playerGold newPlayerData - playerGold oldPlayerData)
                    <> italic
                      ( " ("
                          <> (unsigned . int) (playerGold newPlayerData)
                          <> ")"
                      )
             in Just $ unNotifyBody goldDiffStr
        }
    playerExpChangeNote iconPath =
      Notify.blankNote
        { Notify.appName = "habitica",
          Notify.summary = "Experience gained!",
          Notify.appImage = Just (Notify.File iconPath),
          Notify.body =
            let levelDiff = playerLevel newPlayerData - playerLevel oldPlayerData
                levelProgressStr =
                  case playerExpNeeded newPlayerData of
                    Nothing -> ""
                    Just toNextLevel ->
                      italic $
                        " ("
                          <> (unsigned . int) (playerExp newPlayerData)
                          <> "/"
                          <> (unsigned . int) (playerExp newPlayerData + toNextLevel)
                          <> ")"
                expDiffStr =
                  if
                      | levelDiff == 0 ->
                        bold "Exp: "
                          <> (signed . double) (playerExp newPlayerData - playerExp oldPlayerData)
                          <> levelProgressStr
                      | levelDiff > 0 ->
                        "\n" <> bold "You leveled up!" <> " New level: " <> show newLevel <> "."
                      | otherwise ->
                        "\n" <> bold "You lost a level..." <> " New level: " <> show newLevel <> "."
             in Just $ unNotifyBody expDiffStr
        }

data DropFileData = DropFileData
  { dropFileGitHubDir :: Text,
    dropFileName :: Text
  }
  deriving stock (Show, Eq, Ord)

getDropIconFile :: Drop -> IO FilePath
getDropIconFile Drop {dropType, dropKey} = do
  cache <- Dir.getXdgDirectory Dir.XdgCache "habiticad"
  dropFileData@DropFileData {dropFileName} <- getDropFileData
  let dropFilePath = cache </> toString dropFileName
  ( do
      unlessM (Dir.doesFileExist dropFilePath) $ do
        infoLog "[GitHub] Drop icon missing; fetching from GitHub"
        Dir.createDirectoryIfMissing True cache
        downloadDropFile dropFilePath dropFileData
      pure dropFilePath
    )
    `catch` ( \(_ :: HttpException) -> do
                errorLog "[GitHub] Got bad response. Returning default icon."
                getDataFileName "assets/images/habitica.png"
            )
  where
    getDropFileData :: MonadThrow m => m DropFileData
    getDropFileData = do
      dropFileGitHubDir <- case dropType of
        "Food" -> pure "food"
        "Egg" -> pure "eggs"
        "HatchingPotion" -> pure "potions"
        _ ->
          throwM
            ( userError $
                "Unknown drop type received from Habitica: "
                  <> toString dropType
            )
      let dropFileName = "Pet_" <> dropType <> "_" <> dropKey <> ".png"
      pure DropFileData {dropFileGitHubDir, dropFileName}

    remoteDropUrl :: DropFileData -> Req.Url 'Req.Https
    remoteDropUrl DropFileData {dropFileGitHubDir, dropFileName} =
      Req.https "raw.githubusercontent.com"
        /: "HabitRPG"
        /: "habitica"
        /: "v4.214.3" -- Individual sprites removed shortly after this version
        /: "website"
        /: "raw_sprites"
        /: "spritesmith"
        /: "stable"
        /: dropFileGitHubDir
        /: dropFileName

    downloadDropFile :: FilePath -> DropFileData -> IO ()
    downloadDropFile dropFilePath dropFileData = do
      res <-
        Req.runReq Req.defaultHttpConfig $
          Req.req GET (remoteDropUrl dropFileData) Req.NoReqBody Req.lbsResponse mempty
      infoLog $ "Caching drop icon file at: " <> toText dropFilePath
      writeFileLBS dropFilePath (Req.responseBody res)

newtype PlayerDataFromStats = PlayerDataFromStats PlayerData

instance FromJSON PlayerDataFromStats where
  parseJSON = Aeson.withObject "PlayerDataFromStats" $ \o -> do
    stats <- o .: "stats"
    playerData <- Aeson.parseJSON (Aeson.Object stats)
    pure $ PlayerDataFromStats playerData

getInitialPlayerData :: HabiticaHeaders -> IO PlayerData
getInitialPlayerData headers = Req.runReq httpConfig $ do
  infoLog "[Habitica] Fetching player data (gold, exp, level) from Habitica"
  let memberId = decodeUtf8 $ headersUserId headers
  res <-
    Req.req
      GET
      (habiticaApi /: "members" /: memberId)
      Req.NoReqBody
      Req.jsonResponse
      (headersToOption headers)
  case Req.responseBody res of
    Failure msg -> do
      errorLog $ "[Habitica] " <> msg
      liftIO $ throwIO (userError "Failed to obtain initial player data")
    Data (PlayerDataFromStats playerData) -> do
      infoLog $ "[Habitica] " <> show playerData
      pure playerData

scoreTaskWithDifficulty :: HabiticaHeaders -> IORef PlayerData -> Difficulty -> IO ()
scoreTaskWithDifficulty headers playerData difficulty = Req.runReq httpConfig $ do
  infoLog $ "[Habitica] Creating task with difficulty " <> show difficulty
  oldPlayerData <- readIORef playerData
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
          newPlayerData <- updatePlayerData headers playerData (scorePlayerData scoreResult)
          liftIO $ notify oldPlayerData (scoreResult {scorePlayerData = newPlayerData})
  where
    todo =
      Todo
        { todoText = "Some " <> show difficulty <> " task",
          todoDifficulty = difficulty
        }

updatePlayerData :: MonadIO m => HabiticaHeaders -> IORef PlayerData -> PlayerData -> m PlayerData
updatePlayerData headers playerData newPlayerData = liftIO $ do
  infoLog "Updating playerData to new values"
  oldPlayerData <- readIORef playerData
  evenNewerPlayerData <-
    if playerLevel oldPlayerData /= playerLevel newPlayerData
      then do
        infoLog "Level change detected; new 'toNextLevel' required"
        getInitialPlayerData headers
      else do
        infoLog "Subtracting exp gained from exp needed"
        let expGained = playerExp newPlayerData - playerExp oldPlayerData
            toNextLevel = (-) <$> playerExpNeeded oldPlayerData <*> pure expGained
            evenNewerPlayerData = newPlayerData {playerExpNeeded = toNextLevel}
        pure evenNewerPlayerData
  modifyIORef' playerData (const evenNewerPlayerData)
  infoLog $ "New player data: " <> show evenNewerPlayerData
  pure evenNewerPlayerData

httpConfig :: Req.HttpConfig
httpConfig =
  Req.defaultHttpConfig
    { Req.httpConfigCheckResponse = \_ _ _ -> Nothing
    }

onHabitProgress :: HabiticaHeaders -> IORef PlayerData -> DBus.Signal -> IO ()
onHabitProgress headers playerData signal = case DBus.signalMember signal of
  "OnProgress" -> scoreTaskWithDifficulty headers playerData Trivial
  "OnCompleted" -> scoreTaskWithDifficulty headers playerData Easy
  "OnAllCompleted" -> scoreTaskWithDifficulty headers playerData Hard
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
  initialPlayerData <- getInitialPlayerData headers
  playerData <- newIORef initialPlayerData
  infoLog "Adding listener for DBus signals from habit script"
  _ <- DBus.addMatch client matchHabit (onHabitProgress headers playerData)
  infoLog "Looping forever"
  _ <- forever (threadDelay 1000000)
  infoLog "Shutting down"
  where
    matchHabit :: MatchRule
    matchHabit =
      DBus.matchAny {DBus.matchInterface = Just "rhit.habits.progress"}
