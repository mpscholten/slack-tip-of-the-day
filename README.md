# slack-tip-of-the-day
Share Small Bits Of Knowledge With Your Team, daily.

## Usage
1. [Add to your slack room](https://slack-tip-of-the-day.mpscholten.de/)
2. Everyone in the channel can add some tips via `/tipoftheday-add You can use X to archieve Y`
3. Every day one random tip will posted inside your slack room

## Hacking

This bot is built with haskell, cabal, postgresql and nix (you don't need to have haskell installed, though. The nix package manager will handle that for you).

To get started just clone this repo and run `nix-shell` inside the project directory. This will install all the required dependencies.

Then run
```
SLACK_CLIENT_ID=your_slack_api_id\
SLACK_SECRET=your_slack_api_secret\
SLACK_TOKEN=your_slack_api_token\
DB="user=your_pg_user dbname=slack-tip-of-the-day"\
PORT=8080\
cabal run
```
to start the bot.

You can get the required slack api credentials (`SLACK_CLIENT_ID`, `SLACK_SECRET`, `SLACK_TOKEN`) by [creating a new private slack app](https://api.slack.com/apps/new).

### DB Schema

To get the bot running, make sure you have the following tables setup up and running on your local postgre database.

To get a local pg instance running use `nix-env -i postgresql` to install postgres. Then run `initdb slack-tip-of-the-day` to initialize the database inside a new `slack-tip-of-the-day` directory. Run `pg_ctl -D slack-tip-of-the-day -l logfile start` to start the pg server. Later run `pg_ctl -D slack-tip-of-the-day stop` to stop it.

```sql
CREATE TABLE channels
(
    id TEXT PRIMARY KEY NOT NULL,
    teamid TEXT NOT NULL,
    webhookurl TEXT NOT NULL
);

CREATE TABLE deliveries
(
    date DATE NOT NULL,
    channelid TEXT NOT NULL,
    tipid INTEGER NOT NULL
);

CREATE TABLE tips
(
    id INTEGER PRIMARY KEY NOT NULL,
    channelid TEXT NOT NULL,
    userid TEXT NOT NULL,
    content TEXT NOT NULL
);
```

### Slack API Setup

To get the bot fully working locally, slack needs to be able to call the bot via http. Thus you need a server to run the bot on. Slack also requires the server to support https (you can use letsencrypt for that). As this is lots of work, I suggest just making sure that the code compiles when contributing. If you still want to run your own instance, here are the slack api settings you need to get everything up and running:
- *App name:* `Tip of the day`
- *Short description:* `Share A Bit Of Knowledge Every Day`
- *Long description:* `Shares a tip of the day submitted by everyone`

After that you need to create the required slash-commands. You can do this by click the slash-commands menu on the app overview. There add the followign commands:
- *Command:* `/tipoftheday-add`
  - *Request URL:* `https://slack-tip-of-the-day.example.com/add`
  - *Short Description:* `Adds a new tip`
  - *Usage Hint:* `Your tip`
- *Command:* `/tipoftheday-list`
  - *Request URL:* `https://slack-tip-of-the-day.example.com/list`
  - *Short Description:* `Lists tips added by you`
  - *Usage Hint:* *empty*

