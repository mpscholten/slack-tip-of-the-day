module App where
    import ClassyPrelude
    import qualified Network.Wai.Handler.Warp as Warp
    import Network.Wai (pathInfo, queryString, Application, responseLBS, responseFile)
    import Network.HTTP.Types (status200, status302)
    import Network.HTTP.Simple
    import Data.Maybe (fromJust)
    import qualified Database.PostgreSQL.Simple as PG
    import System.Environment (getEnv)
    import Text.Read (read)
    import Data.String.Conversions (cs)
    import qualified Network.Wai.Parse
    import Network.Wai.Middleware.Static
    import Network.Wai.Middleware.RequestLogger
    import System.Directory (getCurrentDirectory)

    import qualified Slack
    import qualified Database
    import Database (Channel (Channel))

    data Environment = Development | Production
                     deriving (Read)

    data SlackApiCredentials = SlackApiCredentials {
            clientId :: Text,
            clientSecret :: Text,
            requestToken :: Text
        }

    type Port = Int

    run :: FilePath -> PG.Connection -> Port -> SlackApiCredentials -> Environment -> IO ()
    run publicDirectory connection port slackApiCredentials environment = do
        let publicFilesMiddleware = staticPolicy $ addBase (publicDirectory)
        let logMiddleware =
                case environment of
                    Development -> logStdoutDev
                    Production -> id
        Warp.run port $
            publicFilesMiddleware $
                logMiddleware $
                    app connection slackApiCredentials publicDirectory
        return ()

    app :: PG.Connection -> SlackApiCredentials -> FilePath -> Application
    app connection slackApiCredentials publicDirectory request respond =
            case pathInfo request of
                []          -> respond $ responseFile status200 [("Content-Type", "text/html")] (publicDirectory <> "/index.html") Nothing
                ["connect"] -> connect connection request respond
                ["add"]     -> add connection request respond
                ["list"]    -> list connection request respond
                ["delete"]  -> delete connection request respond
        where
            connect connection request respond = do
                    oauthAccess <- Slack.oauthAccess (clientId slackApiCredentials) (clientSecret slackApiCredentials) code
                    let teamId = Slack.oauthAccessTeamId oauthAccess
                    let webhookUrl = Slack.incomingWebhookUrl (Slack.oauthAccessIncomingWebhook oauthAccess)
                    let channelId = Slack.incomingWebhookChannelId (Slack.oauthAccessIncomingWebhook oauthAccess)
                    existingChannel <- Database.findChannelByTeamIdAndChannelId connection teamId channelId
                    case existingChannel of
                        Just existingChannel ->
                            Database.updateChannel connection teamId channelId webhookUrl
                        Nothing ->
                            do
                                Database.saveChannel connection teamId channelId webhookUrl
                                Slack.sendMessage webhookUrl ":robot_face: Tip of the day was just installed. You can add a new tip of the day via `/tipoftheday add Use the tip of the day slack bot for spreading knowledge`"
                    respond $ responseLBS status302 [("Location", "/connected.html")] mempty
                where
                    query = queryString request
                    code = cs $ fromJust $ join $ lookup "code" query

            add connection request respond = do
                    (params, []) <- parseRequestBody request
                    ensureRequestIsFromSlack params
                    let teamId = cs $ fromJust $ lookup "team_id" params
                    let userId = cs $ fromJust $ lookup "user_id" params
                    let channelId = cs $ fromJust $ lookup "channel_id" params
                    let tip = cs $ fromJust $ lookup "text" params
                    addTip teamId userId channelId tip
                    respond $ responseLBS status200 [("Content-Type", "text/plain")] "The tip was added sucessfully :)"
                where
                    addTip = Database.addTip connection

            list connection request respond = do
                    (params, []) <- parseRequestBody request
                    ensureRequestIsFromSlack params
                    let userId = cs $ fromJust $ lookup "user_id" params
                    let channelId = cs $ fromJust $ lookup "channel_id" params
                    tips' <- getTipsByUser userId channelId
                    let tips = map Database.tipContent tips'
                    respond $ responseLBS status200 [("Content-Type", "text/plain")] ("_Tips added by you:_\n" <> (cs $ intercalate "\n" (map (\(id, content) -> tshow id <> ": " <> content) $ zip [1..] tips)))
                where
                    getTipsByUser = Database.getTipsByUser connection

            delete connection request respond = do
                    (params, []) <- parseRequestBody request
                    ensureRequestIsFromSlack params
                    let userId = cs $ fromJust $ lookup "user_id" params
                    let channelId = cs $ fromJust $ lookup "channel_id" params
                    let tipNumber = read $ cs $ fromJust $ lookup "text" params
                    tips <- getTipsByUser userId channelId
                    let selectedTip = fromJust $ lookup tipNumber $ zip [1..] tips
                    Database.deleteTip connection channelId userId (Database.tipId selectedTip)
                    respond $ responseLBS status200 [("Content-Type", "text/plain")] "The tip was deleted successfully"
                where
                    getTipsByUser = Database.getTipsByUser connection

            ensureRequestIsFromSlack :: [(ByteString, ByteString)] -> IO ()
            ensureRequestIsFromSlack params = do
                let token = cs $ fromJust $ lookup "token" params
                if token /= (requestToken slackApiCredentials) then
                    error "Invalid request token"
                else
                    return ()

    deliverTips connection = do
        let findChannels = Database.findChannels connection
        let alreadyDelivered = Database.alreadyDelivered connection
        let getRandomTip = Database.getRandomTip connection
        let markAsDelivered = Database.markAsDelivered connection
        channels <- findChannels
        forM_ channels $ \channel -> do
            let channelId = Database.channelId channel
            delivered <- alreadyDelivered channel
            if delivered then
                putStrLn $ "Channel " <> (tshow channelId) <> " got already delivered "
            else
                do
                    putStrLn $ "Channel " <> (tshow channelId) <> " will be delivered "
                    tip <- getRandomTip channelId
                    case tip of
                        Just tip ->
                            do
                                Slack.sendMessage (Database.channelWebhookUrl channel) (Database.tipContent tip <> "\n\n_add your own via /tipsoftheday-add_")
                                markAsDelivered channelId (Database.tipId tip)
                        Nothing -> putStrLn "No tip found for channel"
        putStrLn "Deliveries done"


    parseRequestBody = Network.Wai.Parse.parseRequestBodyEx Network.Wai.Parse.defaultParseRequestBodyOptions Network.Wai.Parse.lbsBackEnd
