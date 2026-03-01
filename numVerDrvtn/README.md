# numVerDrvtn: Numerically Verified Derivations

An R package for conducting mathematical derivations with automatic
numerical verification at each step.

## Overview

`numVerDrvtn` provides:

- An S4 virtual class `definedExpression` — a marker class with no slots.
  Any class (e.g. `PartitionedMatrix`, `Matrix`, numeric) can inherit from
  it to signal participation in verified derivations.
- The `:=` operator: assign an expression to a variable, print it, and store
  it as the *comparison reference* for subsequent checks.
- The `. ==` syntax: compare a rearrangement against the previous reference.
- `.eq()` / `.eq.last()`: functional equivalents to `. ==`.
- Derivative verification via `numDeriv::jacobian()` — `df_dp()` and
  `dv_dv()` compute numerical Jacobians and cache them for reuse.

## Basic usage

```r
library(numVerDrvtn)

# Assign the first form of an expression and set it as the reference
A := R.i - R.i %*% Z %*% solve(I + G %*% t(Z) %*% R.i %*% Z) %*% G %*% t(Z) %*% R.i

# Assert that a rearrangement is numerically equivalent
. == equivalent_rearrangement_of_A
```

## Derivation workflow

```r
# Each ':=' line introduces a new equivalence being claimed
V.i := R.i - R.i %*% Z %*% solve(I_g + G %*% t(Z) %*% R.i %*% Z) %*% G %*% t(Z) %*% R.i
  . == solve(V)          # check this is the inverse of V

# Continue multi-step derivation
temp := V %*% V.i
  . == I_n              # should be identity
```

## Derivative verification

```r
init_derivative_args(X = X_mat, Z = Z_mat, G = G_mat)

# Analytical derivative function
d_logdet_dG <- function(G, ctx = get_context()) {
  args <- get_args(ctx, G)
  # ... analytical formula ...
}

# Numerical check: derivative of log|V| wrt G[i,j]
df_dp(d_logdet_dG, G_mat, row = 1L, col = 1L)
```

## Installation

```r
# From source (development):
devtools::install("path/to/numVerDrvtn/numVerDrvtn")
```

## Relationship to PartitionedMatrix

`PartitionedMatrix` objects work directly with all operators in this package.
The `PartitionedMatrix` class can be extended to inherit from
`definedExpression` to enable type-testing:

```r
# In PartitionedMatrix package:
setClass("PartitionedMatrix", contains = c("definedExpression", ...))
```

## Dependencies

- **R** (>= 4.0)
- **Matrix**: underlying matrix objects
- **numDeriv**: numerical Jacobian computation
- **digest**: Jacobian cache keying

## Author

Mark Thallman, USDA-ARS, USMARC
