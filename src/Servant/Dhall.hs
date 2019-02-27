{-# LANGUAGE CPP                   #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}

-- | A @DHALL@ empty datatype with `MimeRender` and `MimeUnrender` instances for
-- /Dhall/'s 'Interpret' and 'Inject' classes.
--
-- >>> type Eg = Get '[DHALL] Integer
--
-- /Note:/ reading and executing Dhall expressions from untrusted source is
-- a security risk.
--
module Servant.Dhall (
    DHALL,
    DHALL',
    HasInterpretOptions,
    DefaultInterpretOptions,
    ) where

import           Prelude ()
import           Prelude.Compat

import           Control.Monad
                 (unless)
import           Data.Proxy
                 (Proxy (..))
import           Data.Text.Encoding.Error
                 (lenientDecode)
import qualified Data.Text.Lazy                          as TL
import qualified Data.Text.Lazy.Encoding                 as TLE
import           Data.Text.Prettyprint.Doc
                 (Pretty (pretty), defaultLayoutOptions, layoutPretty,
                 layoutSmart, line)
import           Data.Text.Prettyprint.Doc.Render.String
                 (renderString)
import           Data.Text.Prettyprint.Doc.Render.Text
                 (renderLazy)
import           Data.Traversable
                 (for)
import           Data.Typeable
                 (Typeable)
import           Dhall
                 (Inject (..), InputType (..), Interpret (..),
                 InterpretOptions, Type (..), defaultInterpretOptions)
import qualified Dhall.Core
import           Dhall.Parser
                 (exprFromText, unwrap)
import qualified Dhall.TypeCheck
import qualified Network.HTTP.Media                      as M
import           Servant.API
                 (Accept (..), MimeRender (..), MimeUnrender (..))
import qualified Text.Megaparsec                         as MP

type DHALL = DHALL' DefaultInterpretOptions
data DHALL' opt deriving (Typeable)

instance Accept (DHALL' opts) where
    contentType _ = "application" M.// "x-dhall"

-------------------------------------------------------------------------------
-- Encoding
-------------------------------------------------------------------------------

instance (Inject a, HasInterpretOptions opts) => MimeRender (DHALL' opts) a where
    mimeRender _ x
        = TLE.encodeUtf8
        $ renderLazy
        $ layoutSmart defaultLayoutOptions
        $ (`mappend` line)
        $ pretty
        $ embed ty x
      where
        ty :: InputType a
        ty = injectWith (interpretOptions (Proxy :: Proxy opts))

-------------------------------------------------------------------------------
-- Decoding
-------------------------------------------------------------------------------

instance (Interpret a, HasInterpretOptions opts) => MimeUnrender (DHALL' opts) a where
    mimeUnrender _ lbs = do
        expr0  <- firstEither showParseError $ exprFromText "(input)" te
        expr1  <- for expr0 $ \i -> Left $ "Import found: " ++ ppExpr i
        tyExpr <- firstEither showTypeError $ Dhall.TypeCheck.typeOf expr1
        unless (Dhall.Core.judgmentallyEqual tyExpr $ expected ty) $
            Left $ "Expected and actual types don't match : "
                ++ ppExpr (expected ty) ++ " /= " ++ ppExpr tyExpr
        case extract ty (Dhall.Core.normalizeWith (const (pure Nothing)) expr1) of
            Just x  -> Right x
            Nothing -> Left "Invalid type"
      where
        showParseError = MP.errorBundlePretty . unwrap
        showTypeError e = "Type error: " ++ ppExpr e

        te = TL.toStrict $
            TLE.decodeUtf8With lenientDecode lbs

        ty :: Type a
        ty = autoWith (interpretOptions (Proxy :: Proxy opts))

        ppExpr :: Pretty pp => pp -> String
        ppExpr = renderString . layoutPretty defaultLayoutOptions .  pretty

firstEither :: (a -> b) -> Either a c -> Either b c
firstEither f (Left a)  = Left (f a)
firstEither _ (Right c) = Right c

-------------------------------------------------------------------------------
-- Options
-------------------------------------------------------------------------------

class HasInterpretOptions opts where
    interpretOptions :: Proxy opts -> InterpretOptions

-- | 'defaultInterpretOptions'
data DefaultInterpretOptions deriving (Typeable)

instance HasInterpretOptions DefaultInterpretOptions where
    interpretOptions _ = defaultInterpretOptions
