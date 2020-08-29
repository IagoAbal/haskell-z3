
# Release Notes

## 408.2

Tested (sort of) with Z3 4.8.8.

* Added Optimization API. (David Cao)
* Added Sequences and regular expressions API. (Carlo Nucera)
* Added z3_solver_get_proof. ("0xd34df00d")
* Added MonadZ3 instance for ReaderT. ("0xd34df00d")
* Fixed two typos in docs. (Mauro Bringolf)

## 408.1

Tested (sort of) with Z3 4.8.4.

* Added bindings to `substitute` and `isEqAST`. (Hengchu Zhang)
* Added `MonadFail` instance for `Z3`, required by GHC >=8.6. (Conal Elliott)
* Updated `Z3_get_error_msg` signature (Z3 C API 4.8.7). (Kevin Quick)
* Removed bindings to `Z3_fixedpoint_push` and `Z3_fixedpoint_pop` (Z3 C API 4.8.5). (Eric Walkingshaw)
* Replaced `z3_get_error_msg_ex` with `z3_get_error_msg` (Z3 C API 4.8.5). (Alexander Knauth)
* Added semigroups to dependencies for GHC <= 7. (Hogeyama)

## 408.0

* Enabled support for Z3 4.8. (Carlo Nucera)
    - Required removing the Inteprolation API, `z3_get_parser_error` and other functions!
* Do not crash on empty lists. (M Farkas-Dyck)
* Added binding for `Z3_mk_finite_domain_sort`. (M Farkas-Dyck)
* Implemented workaround for `Z3.Base.toBool` to work on OS X. (Matthew Doty)

By the way, you have read correctly, this new version is _408.0_.
Find more details about the new version policy in the [README](README.md).

## 4.3.1

* Fixed compatibility with base 4.11 due to SMP. (M Farkas-Dyck)

NOTE: This version didn't make it into Hackage.

## 4.3

### Fixes

* Fixed memory leak due to typo in ```Z3.Base.C```. (Deian Stefan)

### New API support

* Added partial support for _Fixedpoint_ API. (David Heath)
* Added more bindings to the _Models_ API. (Daniel Gröber)
* Added _Parser interface_ API. (Tristan Knoth)

### Misc

* After many requests, I have moved the repository to GitHub: https://github.com/IagoAbal/haskell-z3.
* From now on the revision number (aka patch level) 0 will be omitted (so x.y instead of x.y.0).

## 4.2.0

This release removes support for SMT-LIB 1.x in order to be compatible with Z3 4.6.

## 4.1.2

This minor release extends the supported quantifiers API, and adds
partial support for Z3's proof tactics.

* Improved support for theorem proving with quantifiers. (Jakub Daniel)

## 4.1.1

Another small release, made possible thanks to third-party contributions.

* Added bindings for getting the accessors of datatype constructors. (William Hallahan)
* Added bindings for declaring mutually recursive datatypes. (William Hallahan)
* Added partial support for the _Interpolation_ API. (Jakub Daniel)
* Added bindings for deconstructing function applications. (Jakub Daniel)
* Added bindings for ```simplify``` and ```simplifyEx```. (Gleb Popov)
* Some code cleanup. (Jakub Daniel and Iago Abal)

## 4.1.0

Small maintenance release that however introduces one API breaking change.

* Added bindings for the _Set_ API. (Nadia Polikarpova)
* Fix #4: Replace the now deprecated Z3_eval with Z3_model_eval.
* Fix #5: Check error codes right after an API call. (David Castro)

To fix #4 we had to change the type of ```modelEval```, which now takes
an extra ```Bool``` parameter (to force model completion).

## 4.0.0

This release brings support for the new Z3 4.x API,
and *removes* support for the old API.
We are following a new version policy, yet compatible with Haskell's PVP.
So new versions are of the form *x.y.z*,
where *x* is the version of Z3 API supported,
*y* is a major revision of the bindings,
and *z* is a minor revision of the bindings.
Consequently, we bumped the version to *4.0.0* :-).

Special thanks to Nadia Polikarpova,
who diagnosed a problem in our use of ```ForeignPtr``` finalizers,
and proposed a fix.

### Cabal package

* Relaxed dependencies, and removed upper bounds.

### New features and API functions

* Switched to Z3 4.x API.
* Reference counting is managed by the gargabe collector.
* Algebraic datatypes. (KC Sivaramakrishnan)
* ```Z3_get_ast_kind```, ```Z3_solver_check_assumptions```, ```Z3_solver_get_unsat_core```. (Nadia Polikarpova)
* ```Z3_mk_fresh_const```, ```Z3_mk_fresh_func_decl```. (KC Sivaramakrishnan)
* ```MonadFix``` instance for the ```Z3``` monad. (Pepe Iborra)
* Hspec test-suite, with just a couple of tests, more to come...
* A few more helpers for creating numerals, evaluating expressions, and so on.
* Module ```Z3.Base.C```, a very low-level interface to Z3 C API, is now exported.
  Just in case you want to write your own marshaling layer ;-)

### Removals and API-breaking changes

* No more support for the old Z3 3.x API.
* Removed ```Z3.Lang``` module, this should re-appear as a separate package soon.
* ```MonadZ3``` instances must be ```Applicative```.
* Numerals API is now closer to Z3 C API.
  So, for instance, ```mkInt``` now takes both an integer and a sort.
  You can use ```mkInteger``` or ```mkIntNum``` instead.
* ```Z3.Monad.assertCnstr``` is now called ```Z3.Monad.assert```.

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
