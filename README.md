# purescript-threading
Concurrency primitives inspired by python's multithreading and rust, allowing for
predictable concurrency with `Aff`

## Use Cases
* Create a background worker thread
* Communicate between threads (`Threading.Channel`)
* Limit access to a resource _(eg. a database connection pool, file handle)_ to 1 concurrent actor (`Threading.RWLock`, `Threading.Mutex`)
* Coordinate concurrent threads, waiting for some common goal to be reached before continuing (`Threading.Barrier`)
* Create a pool of concurrent "threads" that can pull work from a queue, with graceful exiting and error handling
* Remotely kill a thread, or non-blockingly ask if it has exited

## Installing
```bash
spago install threading
```
