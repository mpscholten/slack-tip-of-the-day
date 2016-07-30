# slack-tip-of-the-day
Share Small Bits Of Knowledge With Your Team, daily.

## Usage
1. [Add to your slack room](https://slack-tip-of-the-day.mpscholten.de/)
2. Everyone in the channel can add some tips via `/tipoftheday-add You can use X to archieve Y`.
3. Every day one random tip will posted inside your slack room.

## Hacking

This bot is built with haskell, cabal, postgresql and nix (you don't need to have haskell installed, though. The nix package manager will handle that for you).

To get started just clone this repo and run `nix-shell` inside the project directory. This will install all the required dependencies.

Then run `cabal run` to start the bot.
