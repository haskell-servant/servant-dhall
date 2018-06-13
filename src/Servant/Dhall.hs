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
import qualified Data.Text.Lazy.Builder                  as TLB
import qualified Data.Text.Lazy.Encoding                 as TLE
import           Data.Text.Prettyprint.Doc
                 (defaultLayoutOptions, layoutPretty, layoutSmart, line)
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
import           Dhall.Pretty
                 (prettyExpr)
import qualified Dhall.TypeCheck
import           Formatting.Buildable
                 (Buildable (..))
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
        $ prettyExpr
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
        expr1  <- for expr0 $ \i -> Left $ "Import found: " ++ fromBuildable i
        tyExpr <- firstEither showTypeError $ Dhall.TypeCheck.typeOf expr1
        unless (Dhall.Core.judgmentallyEqual tyExpr $ expected ty) $
            Left $ "Expected and actual types don't match : "
                ++ ppExpr (expected ty) ++ " /= " ++ ppExpr tyExpr
        case extract ty (Dhall.Core.normalizeWith (const Nothing) expr1) of
            Just x  -> Right x
            Nothing -> Left "Invalid type"
      where
        showParseError = MP.parseErrorPretty . unwrap
        showTypeError e = "Type error: " ++ fromBuildable e

        te = TLE.decodeUtf8With lenientDecode lbs

        ty :: Type a
        ty = autoWith (interpretOptions (Proxy :: Proxy opts))

        ppExpr = renderString . layoutPretty defaultLayoutOptions .  prettyExpr

        fromBuildable :: Buildable b => b -> String
        fromBuildable = TL.unpack . TLB.toLazyText . build

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
