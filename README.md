# purescript-cbor-stream

Type-safe bindings for the streaming API of `cbor-x`

## Installing
```bash
spago install cbor-stream
{bun|yarn|npm|pnpm} install cbor-x
```

## Examples

### Convert a cbor-encoded dataset to csv
```purescript
import Pipes.Node.Stream as Pipes.Stream
import Pipes.Node.FS as Pipes.FS
import Pipes.Node.Buffer as Pipes.Buffer
import Pipes.CBOR as Pipes.CBOR
import Pipes.CSV as Pipes.CSV
import Pipes.Prelude ((>->))
import Pipes.Prelude as Pipes

Pipes.runEffect
  $ Pipes.FS.read "foo.bin"
    >-> Pipes.CBOR.decode @{id :: Int, name :: String}
    >-> Pipes.CSV.stringify
    >-> Pipes.FS.write "foo.csv"
```
