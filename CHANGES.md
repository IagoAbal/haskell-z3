
# The Z3 Haskell Package CHANGELOG

## 4.0.0

This release brings support for the new Z3 4.x API,
and **removes** support for the old API.
We are following a new version policy, yet compatible with Haskell's PVP.
So new versions are of the form *x.y.z*,
where *x* is the version of Z3 API supported,
*y* is a major revision of the bindings,
and *z* is a minor revision of the bindings.
Consequently, we bumped the version to *4.0.0* :-).

Special thanks to Nadia Polikarpova,
who diagnosed a problem in our use of ForeignPtr finalizers,
and proposed a fix.

### Cabal package

* Relaxed dependencies, and removed upper bounds.

### New features and API functions

* Switched to Z3 4.x API.
* Reference counting is managed by the gargabe collector.
* Algebraic datatypes. (KC Sivaramakrishnan)
* Z3_get_ast_kind, Z3_solver_check_assumptions, Z3_solver_get_unsat_core. (Nadia Polikarpova)
* Z3_mk_fresh_const, Z3_mk_fresh_func_decl. (KC Sivaramakrishnan)
* MonadFix instance for the Z3 monad. (Pepe Iborra)
* Hspec test-suite, with just a couple of tests, more to come...
* A few more helpers for creating numerals, evaluating expressions, and so on.
* Module Z3.Base.C, a very low-level interface to Z3 C API, is now exported.
  Just in case you want to write your own marshaling layer ;-)

### API-breaking changes

* MonadZ3 must be Applicative.
* Numerals API is now closer to Z3 C API.
  So, for instance, 'mkInt' now takes both an integer and a sort.
  You can use 'mkInteger' or 'mkIntNum' instead.
* Z3.Monad.assertCnstr is now Z3.Monad.assert

### Removals

* No more support for the old Z3 3.x API.

### Refactoring and clean-up

* Reduce boilerplate in Z3.Base.
* Fix docs to distinguish Z3 API functions and helpers.

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
