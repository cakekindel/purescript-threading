module Pipes.CBOR where

import Prelude

import Control.Monad.Error.Class (class MonadThrow, liftEither)
import Control.Monad.Except (runExcept)
import Control.Monad.Rec.Class (class MonadRec, forever)
import Data.Bifunctor (lmap)
import Data.CBOR (class ReadCBOR, class WriteCBOR, readCBOR, writeCBOR)
import Data.Maybe (Maybe)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (liftEffect)
import Effect.Exception (Error, error)
import Node.Buffer (Buffer)
import Node.Stream.CBOR.Decode as CBOR.Decode
import Node.Stream.CBOR.Encode as CBOR.Encode
import Pipes (await, yield, (>->))
import Pipes.Core (Pipe)
import Pipes.Node.Stream as Pipes.Stream

-- | Transforms buffer chunks of a CBOR file to parsed values
-- | of type `a`.
decode
  :: forall m @a
   . MonadRec m
  => MonadAff m
  => MonadThrow Error m
  => ReadCBOR a
  => Pipe (Maybe Buffer) (Maybe a) m Unit
decode = do
  raw <- liftEffect $ CBOR.Decode.make {}
  let
    unmarshal = forever do
      r <- await
      yield =<< liftEither (lmap (error <<< show) $ runExcept $ readCBOR @a r)
    parser = Pipes.Stream.fromTransform $ CBOR.Decode.toObjectStream raw
  parser >-> Pipes.Stream.inEOS unmarshal

-- | Encode purescript values as CBOR buffers
encode
  :: forall m a
   . MonadAff m
  => MonadThrow Error m
  => MonadRec m
  => WriteCBOR a
  => Pipe (Maybe a) (Maybe Buffer) m Unit
encode = do
  raw <- liftEffect $ CBOR.Encode.make {}
  let
    printer = Pipes.Stream.fromTransform $ CBOR.Encode.toObjectStream raw
    marshal = forever $ yield =<< (writeCBOR <$> await)
  Pipes.Stream.inEOS marshal >-> printer
