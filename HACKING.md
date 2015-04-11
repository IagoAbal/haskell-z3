
# Contributing to Haskell Z3 bindings

Most modules include a HACKING documentation section at the top that you should read before.

We appreciate your contact before contributing to the project,
in order to avoid duplication of work and agree on possible design decisions.

Please follow roughly the same coding style than us.

Let us know if you find any barrier to contribute, so we can fix that :-)

## Adding support an API function

1. Declare the functions in Z3/Lang/C.hsc.
1. Lift the function to the IO monad in Z3/Base.hs,
there is a bunch of marshalling helpers that should make this trivial in most cases.
1. Lift the function to the Z3 monad in Z3/Monad.hs,
this is trivially done using one of the _liftFun_ helpers.
