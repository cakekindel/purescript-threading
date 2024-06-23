module Pipes.CBOR where

import Prelude

import Control.Monad.Error.Class (class MonadThrow, liftEither)
import Control.Monad.Except (runExcept)
import Control.Monad.Rec.Class (class MonadRec)
import Data.Bifunctor (lmap)
import Data.CBOR (class ReadCBOR, class WriteCBOR, readCBOR, writeCBOR)
import Data.Either (Either)
import Data.Maybe (Maybe)
import Data.Profunctor as Pro
import Data.Traversable (traverse)
import Effect.Aff.Class (class MonadAff)
import Effect.Exception (Error, error)
import Foreign (Foreign, MultipleErrors)
import Node.Buffer (Buffer)
import Node.Stream.CBOR.Decode as CBOR.Decode
import Node.Stream.CBOR.Encode as CBOR.Encode
import Pipes.Async (AsyncPipe)
import Pipes.Core (Pipe)
import Pipes.Node.Stream (TransformContext)
import Pipes.Node.Stream as Pipes.Stream
import Pipes.Prelude as Pipe

-- | Transforms buffer chunks of a CBOR file to parsed values
-- | of type `a`.
decode
  :: forall m @a
   . MonadRec m
  => MonadAff m
  => MonadThrow Error m
  => ReadCBOR a
  => AsyncPipe (TransformContext Buffer Foreign) m (Maybe Buffer) (Maybe (Either MultipleErrors a))
decode = do
  let
    parser = Pipes.Stream.fromTransform $ CBOR.Decode.toObjectStream <$> CBOR.Decode.make {}
  Pro.rmap (map (runExcept <<< readCBOR @a)) parser

decodeError :: forall m a r. MonadThrow Error m => Pipe (Maybe (Either MultipleErrors a)) (Maybe a) m r
decodeError = Pipe.mapM (traverse liftEither <<< map (lmap $ error <<< show))

-- | Encode purescript values as CBOR buffers
encode
  :: forall m a
   . MonadAff m
  => MonadThrow Error m
  => MonadRec m
  => WriteCBOR a
  => AsyncPipe (TransformContext Foreign Buffer) m (Maybe a) (Maybe Buffer)
encode =
  let
    p = Pipes.Stream.fromTransform $ CBOR.Encode.toObjectStream <$> CBOR.Encode.make {}
  in
    Pro.lcmap (map writeCBOR) p
