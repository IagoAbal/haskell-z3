
# The Z3-Haskell CHANGELOG

## 0.3.2

Thanks to Scott West and Nadia Polikarpova for contributing to this release.

### Fixes

* Fixed _solverCheckAndGetModel_ to return a model from an unknown satisfiability result, if one exists. (Nadia Polikarpova)
* Fixed mkMap API function to do **not** take the number of arrays as an input parameter, this should be equals to the length of the input array list.
  Strictly speaking this is a minor break of the API but it was considered a but and therefore fixed.

### Refactoring and clean-up

* Reduced marshalling boilerplate in Z3.Base.
  This is a very important step towards supporting Z3 4.0 API.
* Improved API documentation.
  If you are having difficulties due to (presumably) poor API or source documentation, please let us know.

### New features

* Support running multiple queries under the same logical context, using _evalZ3WithEnv_. (Scott West)

### Newly supported API functions

* Many solver-related API functions (Scott West and Nadia Polikarpova).
* z3_mk_forall_const (Scott West), z3_mk_exists_const.
* Z3_get_version.

### Deprecations

* We deprecate _showContext_, _showModel_ and _getModel_; since we prefer to avoid deviations from Z3 API names. 
  Use _contextToString_, _modelToString_ and _checkAndGetModel_ instead.
* We deprecate the Z3.Lang interface, that will be moved to a separate pacakge.
  Few people is using this (_DSL_ish) interface and it is arguably more unstable than Z3.Base or Z3.Monad. 
  It also introduces dependencies with GHC extensions like type families that we prefer to avoid in a more stable package.

### Misc

* Add SMT to categories in z3.cabal.
