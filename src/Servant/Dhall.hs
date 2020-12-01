{-# LANGUAGE CPP                   #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}

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
    HasInterpretOptions(..),
    DefaultInterpretOptions,
    ) where

import           Prelude ()
import           Prelude.Compat

import           Control.Monad
                 (unless)
import           Data.Either.Validation
                 (Validation (..), validationToEither)
import           Data.Function
                 ((&))
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
                 (auto, ToDhall (..), Encoder (..), FromDhall (..), inject,
                 InterpretOptions, Decoder (..), defaultInterpretOptions)
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

instance ToDhall a => MimeRender (DHALL' opts) a where
    mimeRender _ x
        = TLE.encodeUtf8
        $ renderLazy
        $ layoutSmart defaultLayoutOptions
        $ (`mappend` line)
        $ pretty
        $ embed inject x

-------------------------------------------------------------------------------
-- Decoding
-------------------------------------------------------------------------------

instance FromDhall a => MimeUnrender (DHALL' opts) a where
    mimeUnrender _ lbs = do
        expr0  <- firstEither showParseError $ exprFromText "(input)" te
        expr1  <- for expr0 $ \i -> Left $ "Import found: " ++ ppExpr i
        tyExpr <- firstEither showTypeError $ Dhall.TypeCheck.typeOf expr1
        tyExprExpected <- expected (auto @a) & validationToEither & firstEither show
        unless (Dhall.Core.judgmentallyEqual tyExpr $ tyExprExpected) $
            Left $ "Expected and actual types don't match : "
                ++ ppExpr tyExprExpected ++ " /= " ++ ppExpr tyExpr
        case extract auto (Dhall.Core.normalizeWith Nothing expr1) of
             Success x -> Right x
             Failure _ -> Left "Invalid type"
      where
        showParseError = MP.errorBundlePretty . unwrap
        showTypeError e = "Type error: " ++ ppExpr e

        te = TL.toStrict $
            TLE.decodeUtf8With lenientDecode lbs

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
