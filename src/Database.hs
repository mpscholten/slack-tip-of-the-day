module Database where
    import ClassyPrelude hiding (head)
    import Database.PostgreSQL.Simple
    import Database.PostgreSQL.Simple.FromRow
    import Data.List (head)

    data Channel = Channel {
        channelId :: Text,
        channelTeamId :: Text,
        channelWebhookUrl :: Text
    }

    data Tip = Tip {
        tipId :: Int,
        tipChannelId :: Text,
        tipUserId :: Text,
        tipContent :: Text
    }

    instance FromRow Channel where
        fromRow = Channel <$> field <*> field <*> field

    instance FromRow Tip where
        fromRow = Tip <$> field <*> field <*> field <*> field

    saveChannel connection teamId channelId webhookUrl =
        execute connection "INSERT INTO channels (id, teamId, webhookUrl) VALUES (?, ?, ?)" (channelId, teamId, webhookUrl)

    updateChannel connection teamId channelId webhookUrl = do
        execute connection "UPDATE channels SET webhookUrl = ? WHERE teamId = ? AND id = ?" (webhookUrl, teamId, channelId)
        return ()

    addTip :: Connection -> Text -> Text -> Text -> Text -> IO ()
    addTip connection teamId userId channelId content = do
        execute connection "INSERT INTO tips (userId, channelId, content) VALUES (?, ?, ?)" (userId, channelId, content)
        return ()

    getTipsByUser :: Connection -> Text -> Text -> IO [Tip]
    getTipsByUser connection userId channelId = do
        query connection "SELECT id, channelId, userId, content FROM tips WHERE channelId = ? AND userId = ?" (channelId, userId)

    getRandomTip :: Connection -> Text -> IO (Maybe Tip)
    getRandomTip connection channelId = do
        result <- query connection "SELECT id, channelId, userId, content FROM tips WHERE channelId = ? ORDER BY RANDOM() LIMIT 1" [channelId]
        return $ headMay result

    findChannels :: Connection -> IO [Channel]
    findChannels connection =
        query_ connection "SELECT id, teamId, webhookUrl FROM channels"

    alreadyDelivered :: Connection -> Channel -> IO Bool
    alreadyDelivered connection channel = do
        [Only count] <- (query connection "SELECT COUNT(*) FROM deliveries WHERE channelId = ? AND date = current_date" [channelId channel]) :: IO [Only Int]
        return $ count > 0

    markAsDelivered :: Connection -> Text -> Int -> IO ()
    markAsDelivered connection channelId tipId = do
        execute connection "INSERT INTO deliveries (date, channelId, tipId) VALUES (current_date, ?, ?)" (channelId, tipId)
        return ()

    findChannelByTeamIdAndChannelId :: Connection -> Text -> Text -> IO (Maybe Channel)
    findChannelByTeamIdAndChannelId connection teamId channelId = do
        result <- query connection "SELECT id, teamId, webhookUrl FROM channels WHERE teamId = ? AND id = ? LIMIT 1" (teamId, channelId)
        return $ headMay result
