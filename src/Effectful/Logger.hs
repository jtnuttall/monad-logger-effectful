{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Effectful.Logger (
    module MonadLogger,
    Logging,
    runStdErrLogging,
    runStdOutLogging,
    runNoLogging,
) where

import Control.Monad.Logger
import Control.Monad.Logger as MonadLogger (
    LogLevel (..),
    MonadLogger (..),
    MonadLoggerIO (..),
    defaultOutput,
    logDebug,
    logDebugCS,
    logDebugN,
    logDebugS,
    logDebugSH,
    logError,
    logErrorCS,
    logErrorN,
    logErrorS,
    logErrorSH,
    logInfo,
    logInfoCS,
    logInfoN,
    logInfoS,
    logInfoSH,
    logWarn,
    logWarnCS,
    logWarnN,
    logWarnS,
    logWarnSH,
 )
import Effectful
import Effectful.Dispatch.Static
import System.IO (stderr, stdout)

data Logging :: Effect
type instance DispatchOf Logging = Static WithSideEffects
newtype instance StaticRep Logging = LoggerFn
    {loggerFn :: Loc -> LogSource -> LogLevel -> LogStr -> IO ()}

instance (Logging :> es) => MonadLogger (Eff es) where
    monadLoggerLog a b c d = do
        LoggerFn f <- getStaticRep
        unsafeEff_ $ f a b c (toLogStr d)

instance (Logging :> es, IOE :> es) => MonadLoggerIO (Eff es) where
    askLoggerIO = loggerFn <$> getStaticRep

runStdErrLogging :: (IOE :> es) => Eff (Logging : es) a -> Eff es a
runStdErrLogging = evalStaticRep (LoggerFn (defaultOutput stderr))

runStdOutLogging :: (IOE :> es) => Eff (Logging : es) a -> Eff es a
runStdOutLogging = evalStaticRep (LoggerFn (defaultOutput stdout))

runNoLogging :: (IOE :> es) => Eff (Logging : es) a -> Eff es a
runNoLogging = evalStaticRep (LoggerFn (\_ _ _ _ -> pure ()))
