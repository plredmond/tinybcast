{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
module Lib where

import Control.Monad (forever)
import System.Environment (getArgs, getProgName)
import Text.Read (readMaybe)
import qualified Control.Concurrent.Async as Async
import qualified Control.Concurrent.STM as STM
import qualified Control.Exception as Exception
import qualified Data.ByteString.Char8 as BS
import qualified Graphics.Vty as Vty
import qualified Network.Socket as S
import qualified Network.Socket.ByteString as SBS

-- | Setup and teardown a vty controlling the terminal
withVty :: (Vty.Vty -> IO c) -> IO c
withVty = Exception.bracket
    (Vty.mkVty =<< Vty.standardIOConfig)
    (Vty.shutdown)

-- | Open, bind, and close a broadcast socket
withSock :: Maybe S.HostName -> Maybe S.ServiceName -> (S.Socket -> IO c) -> IO c
withSock host service = Exception.bracket
    (do addr:_ <- S.getAddrInfo (Just S.defaultHints{S.addrSocketType=S.Datagram}) host service
        sock <- S.socket (S.addrFamily addr) (S.addrSocketType addr) (S.addrProtocol addr)
        S.setSocketOption sock S.Broadcast 1
        S.bind sock $ S.addrAddress addr
        return sock)
    S.close

main :: IO ()
main = do
    opts@(username, listenHost, listenPort, dests) <- getArgs >>= \case
        [name] -> return (name, "0.0.0.0", "8999", [("192.168.1.255", "8999")])
        (name:listenHost:listenPort:dests) -> return (name, listenHost, listenPort, pairs dests)
        _ -> getProgName >>= \prog -> error $ "USAGE: \n\t"++prog++" username\n\t"++prog++" username listenHost listenPort [destHost destPort ...]"
    print opts
    state <- State
        <$> (STM.newTVarIO AppState{username, buffer="", curPos=0, history=[]})
        <*> STM.newTChanIO
        <*> STM.newTChanIO
    destAddrs <- flip mapM dests $ \(destHost, destPort) -> do
        addr:_ <- S.getAddrInfo  (Just S.defaultHints{S.addrSocketType=S.Datagram}) (Just destHost) (Just destPort)
        return $ S.addrAddress addr
    withSock (Just listenHost) (Just listenPort) $ \sock ->
        withVty $ \vty -> do
            S.getSocketName sock >>= \n -> STM.atomically $ do
                logAct "listen-on" (show n) state
                logAct "send-to" (unwords $ show <$> destAddrs) state
            _ <- Async.waitAny =<< mapM (\act -> Async.async . forever . act $ state)
                [ forever . frontendInput vty
                , forever . frontendDisplay vty
                , forever . backendSend sock destAddrs
                , forever . backendReceive sock
                , forever . protocolReordering
                ]
            return ()
  where
    pairs (a:b:xs) = (a,b):pairs xs
    pairs [] = []
    pairs [x] = error $ "expected a pair of items, got: " ++ x

-- * Framework

data State as ev = State
    { appState :: STM.TVar as
    , netOutbox :: STM.TChan ev
    , netInbox :: STM.TChan ev
    }

-- | Logging utility
logAct :: String -> String -> State AppState ev -> STM.STM ()
logAct tag message State{appState} = STM.writeTVar appState . applyLog tag message =<< STM.readTVar appState

-- | Block until a `vty` event, apply the event to `appState`, optionally emit
-- to `netOutbox`.
frontendInput :: Vty.Vty -> State AppState NetEvent -> IO ()
frontendInput vty State{appState, netOutbox} = do
    localEvent <- Vty.nextEvent vty -- XXX this is specific to the application
    STM.atomically $ do
        appState_ <- STM.readTVar appState
        maybe (return ()) (STM.writeTChan netOutbox) $ generateNetEvent localEvent appState_
        STM.writeTVar appState $ applyLocalEvent localEvent appState_

-- | Render `appState` to `vty` and then block until `appState` changes.
frontendDisplay :: Vty.Vty -> State AppState NetEvent -> IO ()
frontendDisplay vty State{appState} = do
    appState_ <- STM.atomically . STM.readTVar $ appState
    Vty.update vty $ render appState_ -- XXX this is specific to the application
    STM.atomically $ STM.check . (/= appState_) =<< STM.readTVar appState

-- | Block until a `netOutbox` event, broadcast it to the network (and also
-- copy it over to `netInbox`).
backendSend :: S.Socket -> [S.SockAddr] -> State AppState NetEvent -> IO ()
backendSend sock dests State{netOutbox, netInbox} = do
    ev <- STM.atomically . STM.readTChan $ netOutbox
    mapM_ (SBS.sendAllTo sock . BS.pack . show $ ev) dests
    STM.atomically $ STM.writeTChan netInbox ev -- XXX skip the network for ui feedback?

-- | Block until the network receives a packet, deserialize it and emit to
-- `netInbox`.
backendReceive :: S.Socket -> State AppState NetEvent -> IO ()
backendReceive sock s@State{netInbox} = do
    (raw, _) <- SBS.recvFrom sock 4096
    STM.atomically . maybe (logAct "recv-malformed" (BS.unpack raw) s) (STM.writeTChan netInbox) . readMaybe . BS.unpack $ raw

-- | Block until a `netInbox` event and apply it to `appState`.
protocolReordering :: State AppState NetEvent -> IO ()
protocolReordering State{appState, netInbox} = do
    STM.atomically $ do
        ev <- STM.readTChan netInbox
        STM.writeTVar appState . applyNetEvent ev =<< STM.readTVar appState

-- * Application

-- | Application/UI state
data AppState = AppState
    { username :: String
    , buffer :: String
    , curPos :: Int
    -- | History is stored in reverse order
    , history :: [String]
    } deriving Eq
-- | Local events used for UI updates
type LocalEvent = Vty.Event
-- | Replicated event type for sharing state, and also the wire format
data NetEvent = NetEvent
    { sender :: String
    , message :: String
    } deriving (Show, Read)

-- | This function enforces the state invariants which you might want to model
-- using LH.
fixState :: AppState -> AppState
fixState state@AppState{buffer, curPos, history} = state
        { curPos = max 0 . min (length buffer) $ curPos
        , history = take 10 history
        }

-- | State transformer for emitting log messages
applyLog :: String -> String -> AppState -> AppState
applyLog tag message state@AppState{history} = state{history=('<':tag++'>':' ':message):history}

-- | State transformer for handling UI input
applyLocalEvent :: LocalEvent -> AppState -> AppState
applyLocalEvent event state@AppState{buffer, curPos} = fixState $ case event of
    Vty.EvKey  Vty.KEnter   [] -> state{buffer="", curPos=0}
    Vty.EvKey  Vty.KHome    [] -> state{curPos=0}
    Vty.EvKey  Vty.KEnd     [] -> state{curPos=length buffer}
    Vty.EvKey  Vty.KLeft    [] -> state{curPos=curPos-1}
    Vty.EvKey  Vty.KRight   [] -> state{curPos=curPos+1}
    Vty.EvKey (Vty.KChar c) [] -> state{buffer=atLoc curPos buffer $ \fore aft -> fore++c:aft, curPos=curPos+1}
    Vty.EvKey  Vty.KBS      [] -> state{buffer=atLoc curPos buffer $ \fore aft -> safeInit fore++aft, curPos=curPos-1}
    Vty.EvKey  Vty.KEsc     _  -> error "Esc key pressed"
    ev                         -> applyLog "unknown-input" (show ev) state
  where
    atLoc n xs f = let (fore, aft) = splitAt n xs in f fore aft
    safeInit [] = []
    safeInit xs = init xs

-- | Generate a net-event from a local-event, for replication
generateNetEvent :: LocalEvent -> AppState -> Maybe NetEvent
generateNetEvent event AppState{username, buffer} = case event of
    Vty.EvKey  Vty.KEnter   [] -> if null buffer then Nothing else Just NetEvent{sender=username, message=buffer}
    _                          -> Nothing

-- | State transformer for handling replication events
applyNetEvent :: NetEvent -> AppState -> AppState
applyNetEvent event state@AppState{history} = fixState $ case event of
    NetEvent{sender, message} -> state{history=('[':sender++']':' ':message):history}

-- | Draw the UI
render :: AppState -> Vty.Picture
render AppState{username, buffer, curPos, history} = Vty.emptyPicture
    { Vty.picCursor = Vty.Cursor (length username + 3 + curPos) (length history)
    , Vty.picLayers = (: []) $
        (mconcat . fmap (Vty.string Vty.defAttr) . reverse $ history)
            <> Vty.string (Vty.withStyle Vty.defAttr Vty.reverseVideo) ('[':username++']':' ':buffer)
    }
