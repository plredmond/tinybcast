{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
-- {-# LANGUAGE OverloadedStrings #-}
module Lib where

import System.Environment (getArgs, getProgName)
import Control.Monad (forever)
--import Network.Socket
--import Network.Socket.ByteString.Lazy
--import qualified Codec.Compression.GZip as GZip
import qualified Control.Concurrent.Async as Async
import qualified Control.Concurrent.STM as STM
import qualified Control.Exception as Exception
import qualified Graphics.Vty as Vty

withVty :: (Vty.Vty -> IO c) -> IO c
withVty = Exception.bracket
    (Vty.mkVty =<< Vty.standardIOConfig)
    (Vty.shutdown)

main :: IO ()
main = withVty $ \vty -> do
    username <- getArgs >>= \case
        [name] -> return name
        _ -> getProgName >>= \prog -> error $ "USAGE: "++prog++" username"
    state <- State
        <$> return vty
        <*> (STM.newTVarIO AppState{username, buffer="", curPos=0, history=[]})
        <*> STM.newTChanIO
        <*> STM.newTChanIO
    Async.mapConcurrently_
        ($ state)
        [ forever . frontendInput
        , forever . frontendDisplay
        ]

-- * Framework

data State fs as ae = State
    { frontendState :: fs
    , appState :: STM.TVar as
    , netOutbox :: STM.TChan ae
    , netInbox :: STM.TChan ae
    }

-- | Block until a vty event, then emit net events and update local app state
frontendInput :: State Vty.Vty AppState NetEvent -> IO ()
frontendInput State{frontendState, appState, netOutbox} = do
    localEvent <- Vty.nextEvent frontendState -- XXX this is specific to the application
    STM.atomically $ do
        appState_ <- STM.readTVar appState
        maybe (return ()) (STM.writeTChan netOutbox) $ generateNetEvent appState_ localEvent
        STM.writeTVar appState $ applyLocalEvent appState_ localEvent

-- | Render and then block until appstate changes
frontendDisplay :: State Vty.Vty AppState NetEvent -> IO ()
frontendDisplay State{frontendState, appState, netInbox} = do
    appState_ <- STM.atomically . STM.readTVar $ appState
    Vty.update frontendState $ render appState_ -- XXX this is specific to the application
    STM.atomically $ STM.check . (/= appState_) =<< STM.readTVar appState

-- * Application

data AppState = AppState
    { username :: String
    , buffer :: String
    , curPos :: Int
    -- | stored in reverse order
    , history :: [String]
    } deriving Eq
type LocalEvent = Vty.Event
data NetEvent = NetEvent
    { sender :: String
    , message :: String
    }

-- | This function enforces the state invariants which you might want to model
-- using LH.
fixState :: AppState -> AppState
fixState state@AppState{buffer, curPos, history} = state
        { curPos = max 0 . min (length buffer) $ curPos
        , history = take 10 history
        }

applyLocalEvent :: AppState -> LocalEvent -> AppState
applyLocalEvent state@AppState{buffer, curPos, history} event = fixState $ case event of
    Vty.EvKey  Vty.KEnter   [] -> state{buffer="", curPos=0}
    Vty.EvKey  Vty.KHome    [] -> state{curPos=0}
    Vty.EvKey  Vty.KEnd     [] -> state{curPos=length buffer}
    Vty.EvKey  Vty.KLeft    [] -> state{curPos=curPos-1}
    Vty.EvKey  Vty.KRight   [] -> state{curPos=curPos+1}
    Vty.EvKey (Vty.KChar c) [] -> state{buffer=atLoc curPos buffer $ \fore aft -> fore++c:aft, curPos=curPos+1}
    Vty.EvKey  Vty.KBS      [] -> state{buffer=atLoc curPos buffer $ \fore aft -> safeInit fore++aft, curPos=curPos-1}
    Vty.EvKey  Vty.KEsc     [] -> error "done"
    ev                         -> state{history=("Unknown input: "++show ev):history}
  where
    atLoc n xs f = let (fore, aft) = splitAt n xs in f fore aft
    safeInit [] = []
    safeInit xs = init xs

generateNetEvent :: AppState -> LocalEvent -> Maybe NetEvent
generateNetEvent AppState{username, buffer} event = case event of
    Vty.EvKey  Vty.KEnter   [] -> Just NetEvent{sender=username, message=buffer}
    _                          -> Nothing

applyNetEvent :: AppState -> NetEvent -> AppState
applyNetEvent state@AppState{history} event = fixState $ case event of
    NetEvent{sender, message} -> state{history=('[':sender++']':' ':message):history}

render :: AppState -> Vty.Picture
render AppState{username, buffer, curPos, history} = Vty.emptyPicture
    { Vty.picCursor = Vty.Cursor (length username + 3 + curPos) (length history)
    , Vty.picLayers = (: []) $
        (mconcat . fmap (Vty.string Vty.defAttr) . reverse $ history)
            <> Vty.string (Vty.withStyle Vty.defAttr Vty.reverseVideo) ('[':username++']':' ':buffer)
    }
