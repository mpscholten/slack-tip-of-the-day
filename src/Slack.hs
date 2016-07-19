module Slack where
    import ClassyPrelude
    import Data.Aeson
    import Network.HTTP.Simple
    import Data.String.Conversions (cs)

    data IncomingWebhook = IncomingWebhook {
            incomingWebhookUrl :: Text,
            incomingWebhookChannelId :: Text
        }

    data OAuthAccess = OAuthAccess {
            oauthAccessTeamId :: Text,
            oauthAccessAccessToken :: Text,
            oauthAccessIncomingWebhook :: IncomingWebhook
        }

    instance FromJSON OAuthAccess where
        parseJSON (Object v) = OAuthAccess <$> v.: "team_id" <*> v.: "access_token" <*> v.: "incoming_webhook" 
        parseJSON _ = empty

    instance FromJSON IncomingWebhook where
        parseJSON (Object v) = IncomingWebhook <$> v.: "url" <*> v.: "channel_id"
        parseJSON _ = empty

    oauthAccess :: Text -> Text -> Text -> IO OAuthAccess
    oauthAccess clientId clientSecret code = do
        request' <- parseRequest "POST https://slack.com/api/oauth.access"
        let request = setRequestQueryString [
                    ("client_id", Just $ cs clientId),
                    ("client_secret", Just $ cs clientSecret),
                    ("code", Just $ cs code)
                ] request'
        response <- httpJSON request
        return $ getResponseBody response

    sendMessage :: Text -> Text -> IO ()
    sendMessage webhookUrl text = do
        request' <- parseRequest $ "POST " <> cs webhookUrl
        let request = setRequestBodyJSON (object ["text" .= text]) request'
        _ <- httpLBS request
        return ()
