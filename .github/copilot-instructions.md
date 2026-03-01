# GitHub Copilot Instructions for numVerDrvtn

## Project Overview

**Package Name:** numVerDrvtn  
**Version:** 0.1.0  
**Purpose:** Numerically Verified Derivations — S4 class and operators  
**GitHub:** https://github.com/markthallman/numVerDrvtn

**Note:** See global copilot instructions (`~/.github/copilot-instructions.md`) for general R development conventions, Git workflow, file operations, code style, and testing philosophy. This file contains only numVerDrvtn-specific information.

This package provides:
- An S4 virtual class `definedExpression` (no slots, marker/mixin)
- The `:=` operator: assign + print + set comparison_reference
- The `. ==` syntax (via `dotSentinel` S4 class + `==` method) for step-by-step equivalence checking
- `.eq`, `.eq.last`, `.asgn` helper functions
- Derivative verification via `numDeriv::jacobian()` with caching

**Primary User:** Mark Thallman (USDA-ARS, USMARC)  
**Domain:** Computational linear algebra, statistical computing, animal breeding  
**Ported from:** `C:\Source\R\mtmixed2\definedExpression.R`

## Repository Structure

```
numVerDrvtn/              # Git repo root
  numVerDrvtn/            # R package source
    R/
      numVerDrvtn-package.R   # Package documentation
      package-env.R           # .nvd_env — package-level state (no <<-)
      definedExpression-class.R  # S4 virtual class + dotSentinel
      operators.R             # :=, . ==, .eq, .eq.last, .asgn
      derivatives.R           # df_dp, dv_dv, derivative context
    tests/testthat/
      test-definedExpression-class.R
      test-operators.R
      test-derivatives.R
    inst/examples/            # Usage scripts from mtmixed2
    DESCRIPTION
    NAMESPACE
    README.md
  .github/
    copilot-instructions.md
```

## Design Decisions

### Global state → .nvd_env
The original `definedExpression.R` used `<<-` and `assign(..., envir = .GlobalEnv)`.
The package replaces this with a package-level environment `.nvd_env` in `package-env.R`.
- `comparison_reference` lives in `.nvd_env$comparison_reference`
- `last_deriv_ctx` (the active `DerivativeContext`) lives in `.nvd_env$last_deriv_ctx`
- `print_differences` lives in `.nvd_env$print_differences`

### '. ==' syntax via dotSentinel
The `. ==` syntax requires intercepting `==` when the left operand is the `.` sentinel.
Rather than overriding `==` for all characters, we use:
```r
setClass("dotSentinel", contains = "character")
. <- new("dotSentinel", ".")
setMethod("==", c("dotSentinel", "ANY"), function(e1, e2) .eq.last(e2))
```
This means `. == expr` dispatches cleanly without affecting other character comparisons.

### PartitionedMatrix coupling removed
The original code called `singlePartMatrix()` for non-PartitionedMatrix inputs in `:=`.
This coupling is removed: `:=` just calls `drop0()` for sparse matrices and leaves other
types unchanged. PartitionedMatrix is listed as a `Suggests` dependency only.

### Internal functions named with '.'
Helpers not intended for direct use by API consumers are prefixed with `.`:
- `.hash_derivative_key()`
- `.df_dp_sub()`
- `.run_jacobian()`
- `.unify_sparse_pattern()`

## Current State

### Tests
Run with: `& "C:\Program Files\R\R-4.3.2\bin\Rscript.exe" -e "devtools::test()"`

Initial test coverage:
- `test-definedExpression-class.R`: Class existence, inheritance, is.definedExpression
- `test-operators.R`: :=, .eq, .eq.last, . ==, .asgn, definedExpression.options
- `test-derivatives.R`: context creation, df_dp numerical correctness

### Known TODOs (from derivatives.R)
1. **Allow numeric parm_value** — currently `.df_dp_sub()` requires a `Matrix` parameter.
   The TODO comment is: "allow numeric in both parm and result; provide for more than 2 dimensions."
2. **Jacobian caching to .rds** — avoid recomputation across sessions.
3. **Allow df_dp to work with non-Matrix result** — currently assumes sparse Matrix result.

## Dependencies

- **R (>= 4.0)**
- **Matrix**: matrix operations, sparse structure
- **numDeriv**: `jacobian()` for numerical Jacobians
- **digest**: xxhash64 cache keys for Jacobians
- **methods**: S4 class system
- **testthat (>= 3.0.0)** [Suggests]: tests
- **PartitionedMatrix** [Suggests]: the primary use-case consumer

## Relationship to PartitionedMatrix

The `PartitionedMatrix` class can (and should) inherit from `definedExpression`:
```r
# In PartitionedMatrix package — add "definedExpression" to contains
setClass("PartitionedMatrix", contains = c("definedExpression", ...))
```

This is deferred until `numVerDrvtn` is stable and published/installed.
Until then, `PartitionedMatrix` objects work with all operators in `numVerDrvtn`
without explicit inheritance.

## Workflow

### Before starting work
1. `Set-Location C:\Source\R\numVerDrvtn`
2. `git status`
3. Run tests: `& "C:\Program Files\R\R-4.3.2\bin\Rscript.exe" -e "devtools::test('numVerDrvtn')"`

### Adding a new operator or function
1. Add to `R/operators.R` or `R/derivatives.R` as appropriate
2. Export in `NAMESPACE` (and update `Collate:` in DESCRIPTION if new file)
3. Add roxygen2 documentation
4. Write tests in `tests/testthat/`

## Update History
- 2026-03-01: Initial package scaffold created from definedExpression.R
