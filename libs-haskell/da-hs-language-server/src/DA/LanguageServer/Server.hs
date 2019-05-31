-- Copyright (c) 2019 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
-- SPDX-License-Identifier: Apache-2.0

{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
module DA.LanguageServer.Server
  ( runServer
  ) where


import Control.Monad
import Control.Concurrent
import Control.Concurrent.Async
import Control.Concurrent.Extra
import Control.Concurrent.STM

import Data.Default

import           DA.LanguageServer.Protocol
import qualified DA.Service.Logger                as Logger

import qualified Data.Aeson                       as Aeson
import qualified Data.Aeson.Text as Aeson
import qualified Data.Text.Lazy as TL
import qualified Data.Text as T

import           System.IO
import           GHC.IO.Handle                    (hDuplicate, hDuplicateTo)

import qualified Language.Haskell.LSP.Control as LSP
import qualified Language.Haskell.LSP.Core as LSP
import qualified Language.Haskell.LSP.Messages as LSP
import qualified Language.Haskell.LSP.Types as LSP

------------------------------------------------------------------------
-- Server execution
------------------------------------------------------------------------

runServer
    :: Logger.Handle IO
    -> ((forall resp. resp -> ResponseMessage resp) ->
        (ErrorCode -> ResponseMessage ()) ->
        ServerRequest ->
        IO LSP.FromServerMessage)
    -- ^ Request handler for language server requests
    -> (ServerNotification -> IO ())
    -- ^ Notification handler for language server notifications
    -> TChan ClientNotification
    -- ^ Channel for notifications towards the client
    -> IO ()
runServer loggerH reqHandler notifHandler notifChan = do
    -- DEL-6257: Move stdout to another file descriptor and duplicate stderr
    -- to stdout. This guards against stray prints from corrupting the JSON-RPC
    -- message stream.
    newStdout <- hDuplicate stdout
    stderr `hDuplicateTo` stdout

    -- Print out a single space to assert that the above redirection works.
    -- This is interleaved with the logger, hence we just print a space here in
    -- order not to mess up the output too much. Verified that this breaks
    -- the language server tests without the redirection.
    putStr " " >> hFlush stdout
    clientMsgChan <- newTChanIO
    -- These barriers are signaled when the threads reading from these chans exit.
    -- This should not happen but if it does, we will make sure that the whole server
    -- dies and can be restarted instead of losing threads silently.
    clientMsgBarrier <- newBarrier
    notifBarrier <- newBarrier
    void $ waitAnyCancel =<< traverse async
        [ void $ LSP.runWithHandles
            stdin
            newStdout
            ( const $ Right ()
            , handleInit (signalBarrier clientMsgBarrier ()) (signalBarrier notifBarrier ()) clientMsgChan
            )
            (handlers clientMsgChan)
            options
            Nothing
        , void $ waitBarrier clientMsgBarrier
        , void $ waitBarrier notifBarrier
        ]
    where
        reqHandler' :: (ServerRequest, LspId) -> IO LSP.FromServerMessage
        reqHandler' (req, reqId) =
            reqHandler
                (\res -> ResponseMessage "2.0" (responseId reqId) (Just res) Nothing)
                (\err -> ResponseMessage "2.0" (responseId reqId) Nothing (Just $ ResponseError err "" Nothing))
                req
        handleInit :: IO () -> IO () -> TChan LSP.FromClientMessage -> LSP.LspFuncs () -> IO (Maybe LSP.ResponseError)
        handleInit exitClientMsg exitNotif clientMsgChan LSP.LspFuncs{..} = do
            _ <- flip forkFinally (const exitClientMsg) $ forever $ do
                msg <- atomically $ readTChan clientMsgChan
                case convClientMsg msg of
                    Nothing -> Logger.logError loggerH $ "Unknown client msg: " <> T.pack (show msg)
                    Just (Left notif) -> notifHandler notif
                    Just (Right req) -> sendFunc =<< reqHandler' req
            _ <- flip forkFinally (const exitNotif) $ forever $ do
                notif <- atomically $ readTChan notifChan
                sendFunc $ convToServerNotif notif
            pure Nothing

convToServerNotif :: ClientNotification -> LSP.FromServerMessage
convToServerNotif not = case not of
    ShowMessage p -> wrap LSP.NotShowMessage LSP.WindowShowMessage p
    LogMessage p -> wrap LSP.NotLogMessage LSP.WindowLogMessage p
    SendTelemetry p -> wrap LSP.NotTelemetry LSP.TelemetryEvent p
    PublishDiagnostics p -> wrap LSP.NotPublishDiagnostics LSP.TextDocumentPublishDiagnostics p
    CustomNotification method p -> wrap LSP.NotCustomServer method p
    where wrap constr method params = constr $ LSP.NotificationMessage "2.0" method params

convClientMsg :: LSP.FromClientMessage -> Maybe (Either ServerNotification (ServerRequest, LspId))
convClientMsg msg = case msg of
    LSP.ReqInitialize m -> unknownReq m
    LSP.ReqShutdown m -> Just $ Right (Shutdown, reqId m)

    LSP.ReqHover m -> toReq Hover m

    LSP.ReqCompletion m -> toReq Completion m
    LSP.ReqCompletionItemResolve m -> unknownReq m

    LSP.ReqSignatureHelp m -> toReq SignatureHelp m

    LSP.ReqDefinition m -> toReq Definition m
    LSP.ReqTypeDefinition m -> toReq Definition m
    LSP.ReqImplementation m -> toReq Definition m

    LSP.ReqFindReferences m -> toReq References m
    LSP.ReqDocumentHighlights m -> unknownReq m
    LSP.ReqDocumentSymbols m -> toReq DocumentSymbol m
    LSP.ReqWorkspaceSymbols m -> toReq WorkspaceSymbol m
    LSP.ReqCodeAction m -> unknownReq m

    LSP.ReqCodeLens m -> toReq CodeLens m
    LSP.ReqCodeLensResolve m -> unknownReq m

    LSP.ReqDocumentLink m -> unknownReq m
    LSP.ReqDocumentLinkResolve m -> unknownReq m
    LSP.ReqDocumentColor m -> unknownReq m
    LSP.ReqColorPresentation m -> unknownReq m

    LSP.ReqDocumentFormatting m -> toReq Formatting m
    LSP.ReqDocumentRangeFormatting m -> unknownReq m
    LSP.ReqDocumentOnTypeFormatting m -> unknownReq m

    LSP.ReqRename m -> toReq Rename m

    LSP.ReqFoldingRange m -> unknownReq m
    LSP.ReqExecuteCommand m -> unknownReq m
    LSP.ReqWillSaveWaitUntil m -> unknownReq m
    LSP.ReqCustomClient m -> case reqMethod m of
        "daml/keepAlive" -> Just $ Right (KeepAlive, reqId m)
        _ -> unknownReq m

    LSP.NotInitialized m -> unknownNot m
    LSP.NotExit m -> unknownNot m
    LSP.NotCancelRequestFromClient m -> unknownNot m
    LSP.NotDidChangeConfiguration m -> unknownNot m
    LSP.NotDidOpenTextDocument m -> toNot DidOpenTextDocument m
    LSP.NotDidChangeTextDocument m -> toNot DidChangeTextDocument m
    LSP.NotDidCloseTextDocument m -> toNot DidCloseTextDocument m
    LSP.NotWillSaveTextDocument m -> unknownNot m
    LSP.NotDidSaveTextDocument m -> toNot DidSaveTextDocument m
    LSP.NotDidChangeWatchedFiles m -> unknownNot m
    LSP.NotDidChangeWorkspaceFolders m -> unknownNot m
    LSP.NotProgressCancel m -> unknownNot m
    LSP.NotCustomClient m -> unknownNot m

    LSP.RspApplyWorkspaceEdit _ -> Nothing
    LSP.RspFromClient _ -> Nothing
  where toReq constr msg = Just $ Right (constr $ reqParams msg, reqId msg)
        toNot constr msg = Just $ Left $ constr $ notParams msg
        unknownReq (LSP.RequestMessage _ id method params) =
            Just $ Right (UnknownRequest (TL.toStrict $ Aeson.encodeToLazyText method) (Aeson.toJSON params), id)
        unknownNot (LSP.NotificationMessage _ method params) =
            Just $ Left $ UnknownNotification (TL.toStrict $ Aeson.encodeToLazyText method) (Aeson.toJSON params)
        -- Type-restricted wrappers to make DuplicateRecordFields less annoying.
        reqParams :: RequestMessage m req resp -> req
        reqParams = _params
        reqId :: RequestMessage m req resp -> LspId
        reqId = _id
        reqMethod :: RequestMessage m req resp -> m
        reqMethod = _method
        notParams :: NotificationMessage m a -> a
        notParams = _params

handlers :: TChan LSP.FromClientMessage -> LSP.Handlers
handlers chan = def
    { LSP.hoverHandler = emit LSP.ReqHover
    , LSP.definitionHandler = emit LSP.ReqDefinition
    , LSP.codeLensHandler = emit LSP.ReqCodeLens
    , LSP.didOpenTextDocumentNotificationHandler = emit LSP.NotDidOpenTextDocument
    , LSP.didChangeTextDocumentNotificationHandler = emit LSP.NotDidChangeTextDocument
    , LSP.didCloseTextDocumentNotificationHandler = emit LSP.NotDidCloseTextDocument
    , LSP.didSaveTextDocumentNotificationHandler = emit LSP.NotDidSaveTextDocument
    , LSP.initializedHandler = emit LSP.NotInitialized
    , LSP.exitNotificationHandler = emit LSP.NotExit
    , LSP.customRequestHandler = emit LSP.ReqCustomClient
    }
    where
        emit :: (a -> LSP.FromClientMessage) -> Maybe (LSP.Handler a)
        emit f = Just $ atomically . writeTChan chan . f

options :: LSP.Options
options = def
    { LSP.textDocumentSync = Just TextDocumentSyncOptions
          { _openClose = Just True
          , _change = Just TdSyncFull
          , _willSave = Just True
          , _willSaveWaitUntil = Nothing
          , _save = Just $ SaveOptions $ Just False
          }
    , LSP.codeLensProvider = Just $ CodeLensOptions $ Just False
    }
