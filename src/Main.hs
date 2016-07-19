module Main where
    import ClassyPrelude
    import Data.Maybe (fromJust, fromMaybe)
    import qualified Database.PostgreSQL.Simple as PG
    import System.Environment (getEnv, lookupEnv)
    import Text.Read (read)
    import Data.String.Conversions (cs)
    import System.Directory (getCurrentDirectory)

    import qualified App
    import App (Environment (Production), SlackApiCredentials (SlackApiCredentials))

    main :: IO ()
    main = do
        dbConfig <- getEnv "DB"
        port <- getEnv "PORT"
        environment <- lookupEnv "ENV"
        clientId <- getEnv "SLACK_CLIENT_ID"
        clientSecret <- getEnv "SLACK_SECRET"
        token <- getEnv "SLACK_TOKEN"
        publicDirectory <- findPublicDirectory
        connection <- PG.connectPostgreSQL $ cs dbConfig
        deliverTips <- async (App.deliverTips connection)
        putStrLn $ "Listening on port " <> tshow port
        App.run publicDirectory connection (read port) (SlackApiCredentials (cs clientId) (cs clientSecret) (cs token)) (fromMaybe Production $ read `fmap` environment)


    findPublicDirectory :: IO FilePath
    findPublicDirectory = getCurrentDirectory >>= (\cwd -> return $ cwd <> "/src/public")
