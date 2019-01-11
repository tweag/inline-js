# `inline-js`

[![CircleCI](https://circleci.com/gh/tweag/inline-js/tree/master.svg?style=shield)](https://circleci.com/gh/tweag/inline-js/tree/master)

## Note

This repository contains two packages, `inline-js` and `inline-js-core`. Chances are, you won't be interested in working with `inline-js-core` directly, and will want to use the QuasiQuoter interface found in `inline-js`.

Currently, this repository is an active laboratory. The API is subject to radical changes based on feedback from the development of `asterius`.

Still, we are interested in how `inline-js` may be useful to your projects. If you have any questions, suggestions or criticisms, please don't hesitate to file an issue!

## How it Works

`inline-js` follows in the tradition of [inline-c](http://hackage.haskell.org/package/inline-c) and [inline-java](http://hackage.haskell.org/package/inline-java) in enabling developers to bridge the gap between Haskell and another programming language. It comes with two QuasiQuoters, `expr` and `block`. Also included is an instance for data-types which are instances of the `aeson` `ToJSON` and `FromJSON` typeclasses, to allow their use across the Haskell-JS boundary.

### A sample use of `expr`:

``` haskell
sumInJS :: Int -> Int -> IO Int
sumInJS v1 v2 =
    withJSSession defJSSessionOpts [expr| $v1 + $v2 |]
```

To antiquote a Haskell variable into the JavaScript context, just precede its name with the dollar symbol. The two `Int` values can be inserted without any typeclass derivation as `Int` is already an instance of `ToJSON` and `FromJSON`.

Note that there is an implicit `return` inserted when using `expr`, so don't add one yourself. If you need to do more complex work, you'll want to use `block`.

### A sample use of `block`:
``` haskell
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

data GameWorld = GameWorld
  { playerCharacter :: PlayerCharacter
  } deriving (Generic, ToJSON, FromJSON)

data PlayerCharacter = PlayerCharacter
  { hp :: Int
  , maxHp :: Int
  , isDead :: Bool
  } deriving (Generic, ToJSON, FromJSON)

getCharacterStatus :: IO String
getCharacterStatus =
    let gameWorld =
            GameWorld
              { playerCharacter =
                  PlayerCharacter {hp = 10, maxHp = 30, isDead = False}
              }
    in
        withJSSession defJSSessionOpts [block|
            const gameWorld = $gameWorld;
            const player = gameWorld.playerCharacter;
            const renderHp = somePlayer => somePlayer.hp + "/" + somePlayer.maxHp;
            if (player.hp <= 0 || player.isDead) {
                return "Game Over";
            } else {
                return "Your HP: " + renderHp(player);
            }
        |]
```

Here, you can see the Haskell value `gameWorld` (which has both `ToJSON` and `FromJSON` derived) serialized into the JavaScript quotation with the antiquotation `$gameWorld`. Explicit use of `return` is required to extract a value out from the quotation.

### Session Management

`inline-js` also exposes the `withJSSession`, `startJSSession` and `killJSSession` functions, with options for configuring the session. `inline-js` works by communicating with a Node.js script which evaluates the quoted JavaScript code. Within a session, global variables are maintained across splices. `let`, `var` and `const` variable declarations are not shared even within the same session.

Important note: `withJSSession` wraps the session with cleanup logic, but when you create a session with `startJSSession`, you will need to clean up with `killJSSesssion`.

## Sponsors

[![Tweag I/O](https://www.tweag.io/img/tweag-small.png)](https://www.tweag.io)

`inline-js` and `inline-js-core` are developed by [Tweag I/O](https://tweag.io/).
