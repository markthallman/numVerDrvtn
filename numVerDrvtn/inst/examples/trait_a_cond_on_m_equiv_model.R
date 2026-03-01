# Build and solve the MME for a standard multi-trait model and then
# 	use those model components to construct the model components of
# 	an equivalent model for traits a, b, and g conditional on traits m and r

# example data.R (or alternative versions of it) leaves objects that describe a data set in the global environment
## Required objects include: data, ped1, A.i, fixed_model, Phi, Sigma, Xi, b
source("example_data.R")
# To rebuild the data set, delete the rds file containing the data (e.g. Trait5Animal6.rds)
# 	and then source example_data.R or another version of it
# In order to change use_complete_data, the data set must be rebuilt

cat(sprintf("use_complete_data = %s", use_complete_data))
print(data)

# Test traits a and m with the standard model.

# Use GLS for trait m only *****************************************************
Phi.i <- Matrix(data=c(1), nrow = 1)
rownames(Phi.i) <- colnames(Phi.i) <- c("m")
Phi <- .i(Phi.i)
V_m <- build_model_components(data, fixed_models = fixed_model[c("m")], Phi, Xi, epsilon = 0, Upsilon, A.i, b)

# Compute GLS solution for beta and u
bu.hat_gls_m <- build_and_solve_GLS(X, Z, y, G, V_m)
b.hat_gls_m <- bu.hat_gls_m["X", ]
u.hat_gls_m <- bu.hat_gls_m["Z", ]
bu.hat_gls_m == build_and_solve_GLS2(X, Z, y, G, V_m)

# Save components for use in building joint MME
XV.iX_m <- XV.iX
XV.iy_m <- XV.iy

# Use GLS on standard model for traits a and m *********************************
# Run with 0 variance for trait m **********************************************
# set (co)variance parameters to give round numbers for 2 trait model
Phi.i <- Matrix(data=c(1, -0.5, -0.5, 1), nrow = 2)
rownames(Phi.i) <- colnames(Phi.i) <- c("a", "m")
Phi <- singlePartMatrix(.i(Phi.i))
V <- build_model_components(data, fixed_models = fixed_model[c("a", "m")], Phi, Xi, epsilon = 0, Upsilon, A.i, b)

# Compute GLS solution for beta and u
bu.hat_gls <- build_and_solve_GLS(X, Z, y, G, V)

# Build model of trait a conditional on trait m ********************************
X_a <- X["a", "a"]
X_m <- X["m", "m"]
Z_a <- Z["a", "a"]
Z_m <- Z["m", "m"]
y_a <- y["a"]
y_m <- y["m"]
V_aa <- V["a", "a"]
V_am <- V["a", "m"]
V_ma <- V["m", "a"]
V_mm <- V["m", "m"]
G_aa <- Phi["a", "a"] * partMatrixFromTemplate(A, Z_a)
G_am <- Phi["a", "m"] * partMatrixFromTemplate(A, t(Z_a), Z_m)
G_mm <- Phi["m", "m"] * partMatrixFromTemplate(A, t(Z_m), Z_m)

# Estimates for trait m from the joint analysis of traits a and m
#b.hat_m <- bu.hat_gls["X:m"]
b.hat_m <- as.numeric(bu.hat_gls_m["X"])
#u.hat_m <- bu.hat_gls["Z:m"]
u.hat_m <- partMatrixFromTemplate(bu.hat_gls_m["Z"], G_mm)

# Adjust the observation vector
y_a.star <- y["a"] - V_am %*% .i(V_mm) %*% (y_m - X_m %*% b.hat_m)
R_a <- R["a", "a"]
#G_a.star <- G_aa - G_am %*% t(Z_m) %*% .i(V_mm) %*% Z_m %*% t(G_am)
P_mm <-  .i(V_mm) -.i(V_mm)%*% X_m %*%.i(t(X_m)%*%.i(V_mm)%*% X_m)%*%t(X_m) %*% .i(V_mm)
G_a.star <- G_aa - G_am %*% t(Z_m) %*% P_mm %*% Z_m %*% t(G_am)		# This works correctly with marginal estimates for trait m
# Build and solve the MME for trait a conditional on trait m
b_u_hat_a.star <- build_and_solve_MME(X_a, Z_a, y_a.star, .i(G_a.star), .i(R_a))
b.hat_a <- b_u_hat_a.star["X", "All"]
u.hat_a.star <- b_u_hat_a.star["Z", "All"]
u.hat_a <- u.hat_a.star + partMatrixFromTemplate(G_am %*% t(Z_m) %*% .i(V_mm)%*% Z_m  %*% u.hat_m, u.hat_a.star)


y_a.star == y["a"] - V_am %*% P_mm %*% (y_m)
# Check whether the solutions to the conditional model MME are equal to those from the full GLS model
b.hat_a == partMatrixFromTemplate(bu.hat_gls["X:a", "All"], b.hat_a)
u.hat_a == partMatrixFromTemplate(bu.hat_gls["Z:a", "All"], u.hat_a)

# Compute and compare likelihoods

## Construct the beta vector used to simulate the data
beta <- b[[traits[1L]]]
for (t in seq_along(traits[-1L]) + 1L) {
	beta <- c(beta, b[[traits[t]]])
}

## Likelihood from joint GLS of traits a and m Searle (1989) (4.1)
N_am <- nrow(X)
L_gls_am <- - (1/2) * N_am * log(2 * pi) - (1/2) * .log(.det(V)) -
	(1/2) * t(y - X %*% beta) %*% V.i %*% (y - X %*% beta)

## Likelihood from model of trait a conditional on trait m
N_a <- nrow(X_a)
V_a.m <- V_aa - V_am %*% .i(V_mm) %*% t(V_am)
y_a.m <- y["a"] - V_am %*% .i(V_mm) %*% (y_m - X_m %*% b[["m"]])
L_gls_a.m <- - (1/2) * N_a * log(2 * pi) - (1/2) * .log(.det(V_a.m)) -
	(1/2) * t(y_a.m - X_a %*% b[["a"]]) %*% .i(V_a.m) %*% (y_a.m - X_a %*% b[["a"]])

## Marginal likelihood of trait m
N_m <- nrow(X_m)
L_gls_m <- - (1/2) * N_m * log(2 * pi) - (1/2) * .log(.det(V_mm)) -
	(1/2) * t(y_m - X_m %*% b[["m"]]) %*% .i(V_mm) %*% (y_m - X_m %*% b[["m"]])

# Check that likelihoods are partitioned correctly
L_gls_am == L_gls_a.m + L_gls_m

# beta is a parameter of the likelihood; the restricted likelihood is conditional on beta.hat

# Compute and compare log restricted likelihoods
# Let Lr represent the portion of the log restricted likelihood that is invariant to fixed effect parameters
# 	following Searle (1989) (5.3)

# Added by rmt *********************************************************************************************************
## Log restricted likelihood from joint GLS of traits a and m from Searle (1989) Sections 2.6.e and 5.1
### Restricted likelihood is the likelihood K'y
### Following is the value of K' that corresponds to F = I
K <- null_space_basis(X)
N_am_rX <- ncol(K) # Number of records minus the rank of X
P <- V.i - V.i %*% X %*% .i(t(X) %*% V.i %*% X) %*% t(X) %*% V.i
K %*% .i(t(K) %*% V %*% K) %*% t(K) == P
.det(t(K) %*% V %*% K) == .det(V) * .det(t(X) %*% .i(V) %*% X) / .det(t(X) %*% X)

#### Check that E(y) and Var(y) are compatible with the model
t(y) %*% P %*% y == t(y - X %*% bu.hat_gls["X"]) %*% V.i %*% (y - X %*% bu.hat_gls["X"])

### Lr1 represents the entire restricted likelihood
Lr1_gls_am := - (1/2) * N_am_rX * log(2 * pi) - (1/2) * .log(.det(t(K) %*% V %*% K)) -
						 (1/2) * t(y) %*% K %*% .i(t(K) %*% V %*% K) %*% t(K) %*% y
. == - (1/2) * N_am_rX * log(2 * pi) + (1/2) * .log(.det(t(X) %*% X)) -
			 (1/2) * .log(.det(V)) - (1/2) * .log(.det(t(X) %*% .i(V) %*% X)) - (1/2) * t(y) %*% P %*% y

### Lr represents the invariant portion of the restricted likelihood
Lr_gls_am := - (1/2) * .log(.det(V)) - (1/2) * .log(.det(t(X) %*% .i(V) %*% X)) - (1/2) * t(y) %*% P %*% y

### Log restricted likelihood from joint GLS of traits a and m from Searle (1989) (5.2)
Lr_gls_am == - (1/2) * .log(.det(V)) - (1/2) * .log(.det(t(X) %*% .i(V) %*% X)) - (1/2) * t(y) %*% P %*% y

### Log restricted likelihood from model of trait a conditional on trait m from Searle (1989) Sections 2.6.e and 5.1
K_a <- null_space_basis(X_a)
N_a_rX_a <- ncol(K_a) # Number of records for trait a minus the rank of X_a
# TODO: Following is different than Elyse defined it to be subsequently
## Revise it to condition on K_m'y_m instead of y_m
# P_a.m <- .i(V_a.m) - .i(V_a.m) %*% X_a %*% .i(t(X_a) %*% .i(V_a.m) %*% X_a) %*% t(X_a) %*% .i(V_a.m)
# K_a %*% .i(t(K_a) %*% V_a.m %*% K_a) %*% t(K_a) == P_a.m
# .det(t(K_a) %*% V_a.m %*% K_a) == .det(V_a.m) * .det(t(X_a) %*% .i(V_a.m) %*% X_a) / .det(t(X_a) %*% X_a)

#### Check that E(y) and Var(y) are compatible with the model
# t(y_a.star) %*% P_a.m %*% y_a.star == t(y_a.star - X_a %*% bu.hat_gls["X:a"]) %*% .i(V_a.m) %*% (y_a.star - X_a %*% bu.hat_gls["X:a"])
#
# ### Lr1 represents the entire restricted likelihood
# Lr1_gls.am := - (1/2) * N_a_rX_a * log(2 * pi) - (1/2) * .log(.det(t(K_a) %*% V_a.m %*% K_a)) -
# 	(1/2) * t(y_a.star) %*% K_a %*% .i(t(K_a) %*% V_a.m %*% K_a) %*% t(K_a) %*% y_a.star
# . == - (1/2) * N_a_rX_a * log(2 * pi) + (1/2) * .log(.det(t(X_a) %*% X_a)) -
# 	(1/2) * .log(.det(V_a.m)) - (1/2) * .log(.det(t(X_a) %*% .i(V_a.m) %*% X_a)) - (1/2) * t(y_a.star) %*% P_a.m %*% y_a.star
#
# ### Lr represents the invariant portion of the restricted likelihood
# Lr_gls_a.m := - (1/2) * .log(.det(V_a.m)) - (1/2) * .log(.det(t(X_a) %*% .i(V_a.m) %*% X_a)) - (1/2) * t(y_a.star) %*% P_a.m %*% y_a.star
#
# ### Log restricted likelihood from model of trait a conditional on trait m from Searle (1989) (5.2)
# Lr_gls_a.m == - (1/2) * .log(.det(V_a.m)) - (1/2) * .log(.det(t(X_a) %*% .i(V_a.m) %*% X_a)) -
# (1/2) * t(y_a.star - X_a %*% bu.hat_gls["X:a"]) %*% .i(V_a.m) %*% (y_a.star - X_a %*% bu.hat_gls["X:a"])

# **********************************************************************************************************************
## Log restricted likelihood from joint GLS of traits a and m
Lr_gls_am <- - (1/2) * .log(.det(V)) - (1/2) * .log(.det(t(X) %*% .i(V) %*% X)) -
	(1/2) * t(y - X %*% bu.hat_gls["X"]) %*% V.i %*% (y - X %*% bu.hat_gls["X"])

## Log restricted likelihood from model of trait a conditional on trait m
KmV_a.m <- V_aa - V_am %*% P_mm %*% t(V_am)
Lr_gls_a.m <- - (1/2) * .log(.det(KmV_a.m))  - (1/2) * .log(.det(t(X_a) %*% .i(KmV_a.m) %*% X_a)) -
	(1/2) * t(y_a.star- X_a %*% as.numeric(b.hat_a)) %*% .i(KmV_a.m) %*% (y_a.star - X_a %*% as.numeric(b.hat_a))
## Log restricted marginal likelihood of trait m
Lr_gls_m <- - (1/2) * .log(.det(V_mm)) - (1/2) * .log(.det(t(X_m) %*% .i(V_mm) %*% X_m)) -
	(1/2) * t(y_m - X_m %*% b.hat_m) %*% .i(V_mm) %*% (y_m - X_m %*% b.hat_m)

# Check that restricted likelihoods are partitioned correctly
Lr_gls_am == Lr_gls_a.m + Lr_gls_m

# rmt: Derivatives from joint GLS of traits a and m ********************************************************************
# Test functions to verify derivatives
## Define subsets of Phi_Xi and Sigma specific to the subset of traits included in this context
trts <- c("a", "m")
Phi_Xi_all <- Phi_Xi
Phi_Xi <- Phi_Xi_all[trts, trts]
Sigma_all <- Sigma
Sigma <- Sigma_all[trts, trts]

# The following line stores all arguments required by the functions for which derivatives will be computed
## Those arguments are loaded into the function's env by get_args(), which should be the first line of the function body
## This allows calls to the derivative functions to be short enough to include in derivations
j_ctx <- init_derivative_args(X = X[trts, trts], Z = Z[trts, trts], y = y[trts], A = A, Phi_Xi = Phi_Xi, Sigma = Sigma, trts = trts)

## Compute derivative
dG.dPaa  <- df_dp(.G, Phi_Xi, "a", "a")
dG.dPam  <- df_dp(.G, Phi_Xi, "a", "m") # TODO: make Phi and Sigma packed symmetric matrices so this derivative is correct
dG.dPmm  <- df_dp(.G, Phi_Xi, "m", "m")
dR.dSaa  <- df_dp(.R, Sigma, "a", "a")

P <- V.i - V.i %*% X %*% .i(t(X) %*% V.i %*% X) %*% t(X) %*% V.i
dV.dSaa := df_dp(.V, Sigma, "a", "a")
dV.dSaa == (1/as.numeric(Sigma["a", "a"])) * R

# Derivation of Searle (1989) (5.14) verified by numerical derivatives
dLr.dSaa_gls_am := df_dp(.Lr, Sigma, "a", "a")
. == df_dp(function(parm_value, ctx = get_context()) {
	      args = get_args(ctx, parm_value)
				- (1/2) * .log(.det(.V(parm_value))) - (1/2) * .log(.det(t(X) %*% .V.i(parm_value) %*% X)) - (1/2) * t(y) %*% .P(parm_value) %*% y
			}, Sigma, "a", "a")
. == (- (1/2) * tr(.V.i(Sigma) %*% df_dp(.V, Sigma, "a", "a"))
			- (1/2) * tr(.i(t(X) %*% .V.i(Sigma) %*% X) %*% t(X) %*% df_dp(.V.i, Sigma, "a", "a") %*% X)
			- (1/2) * t(y) %*% df_dp(.P, Sigma, "a", "a") %*% y)
. == (- (1/2) * tr(.V.i(Sigma) %*% dV.dSaa)
			- (1/2) * tr(.i(t(X) %*% .V.i(Sigma) %*% X) %*% t(X) %*% (- .V.i(Sigma) %*% dV.dSaa %*% .V.i(Sigma)) %*% X)
			- (1/2) * t(y) %*% (- .P(Sigma) %*% dV.dSaa %*% .P(Sigma)) %*% y)
. == (- (1/2) * tr(V.i %*% dV.dSaa)
			- (1/2) * tr(V.i %*% X %*% .i(t(X) %*% V.i %*% X) %*% t(X) %*% (- V.i) %*% dV.dSaa)
			+ (1/2) * t(y) %*% P %*% dV.dSaa %*% P %*% y)
. == (- (1/2) * tr((V.i - V.i %*% X %*% .i(t(X) %*% V.i %*% X) %*% t(X) %*% V.i) %*% dV.dSaa)
			+ (1/2) * t(y) %*% P %*% dV.dSaa %*% P %*% y)
. == (- (1/2) * tr(P %*% dV.dSaa) + (1/2) * t(y) %*% P %*% dV.dSaa %*% P %*% y)
# **********************************************************************************************************************

# Compute and compare derivatives of restricted likelihoods wrt (co)variance parameters
# 	following Searle (1989) (5.14)

## Derivatives from joint GLS of traits a and m
P <- V.i - V.i %*% X %*% .i(t(X) %*% V.i %*% X) %*% t(X) %*% V.i
dV.dSaa := df_dp(.V, Sigma, "a", "a")
. == (1/as.numeric(Sigma["a", "a"])) * R
dLr.dSaa := df_dp(.Lr, Sigma, "a", "a")
. == - (1/2) * tr(P %*% dV.dSaa) + (1/2) * t(y) %*% P %*% dV.dSaa %*% P %*% y

dV.dPaa <- dV.dPam <- dV.dPmm <- 0 * R

dV.dPaa := df_dp(.V, Phi_Xi, "a", "a")
dV.dPaa["a", "a"] == Z_a %*% A %*% t(Z_a)
dLr.dPaa_gls_am := df_dp(.Lr, Phi_Xi, "a", "a")
. == - (1/2) * tr(P %*% dV.dPaa) + (1/2) * t(y) %*% P %*% dV.dPaa %*% P %*% y

# TODO: As Phi_Xi is currently defined Phi_Xi["a", "m"] and Phi_Xi["a", "m"] are distinct parameters
## It needs to be defined as a packed symmetric matrix so they are considerd the same
dV.dPam := df_dp(.V, Phi_Xi, "a", "m") + df_dp(.V, Phi_Xi, "m", "a")
dV.dPam["a", "m"] == Z_a %*% A %*% t(Z_m)
dV.dPam["m", "a"] == Z_m %*% A %*% t(Z_a)
dLr.dPam_gls_am := df_dp(.Lr, Phi_Xi, "a", "m") + df_dp(.Lr, Phi_Xi, "m", "a")
. == - (1/2) * tr(P %*% dV.dPam) + (1/2) * t(y) %*% P %*% dV.dPam %*% P %*% y

dV.dPmm := df_dp(.V, Phi_Xi, "m", "m")
dV.dPmm["m", "m"] == Z_m %*% A %*% t(Z_m)
dLr.dPmm_gls_am := df_dp(.Lr, Phi_Xi, "m", "m")
. == - (1/2) * tr(P %*% dV.dPmm) + (1/2) * t(y) %*% P %*% dV.dPmm %*% P %*% y

## Derivatives of marginal restricted likelihood of trait m
# When a new derivative context is defined, it replaces the old one unless the old one is explicitly specified in function calls
m_ctx <- init_derivative_args(X = X["m", "m"], Z = Z["m", "m"], y = y["m"], A = A, Phi_Xi = Phi_Xi["m", "m"], Sigma = Sigma["m", "m"], trts = "m")

P_mm == .P(Phi_Xi["m", "m"])
P_mm == .i(V_mm) - .i(V_mm) %*% X_m %*% .i(t(X_m) %*% .i(V_mm) %*% X_m) %*% t(X_m) %*% .i(V_mm)

dV.dPmm := df_dp(.V, Phi_Xi["m", "m"], "m", "m")
dV.dPmm_mm <-  Z_m %*% A %*% t(Z_m)

dLr.dPmm_gls_mm := df_dp(.Lr, Phi_Xi["m", "m"], "m", "m")
. == - (1/2) * tr(P_mm %*% dV.dPmm_mm) + (1/2) * t(y_m) %*% P_mm %*% dV.dPmm_mm %*% P_mm %*% y_m

## Derivatives from model of trait a conditional on K_m'y_m (rmt: is this true?)
KmV_a.m == V_aa - V_am %*% P_mm %*% t(V_am)
KmV_a.m.i <- .i(KmV_a.m) #inverse of conditional variance
P_a.m <- KmV_a.m.i - KmV_a.m.i %*% X_a %*% .i(t(X_a) %*% KmV_a.m.i %*% X_a) %*% t(X_a) %*% KmV_a.m.i #P matrix for ya* REML
Lr_gls_a.m == - (1/2) * .log(.det(KmV_a.m))  - (1/2) * .log(.det(t(X_a) %*% .i(KmV_a.m) %*% X_a)) -
	(1/2) * t(y_a.star- X_a %*% as.numeric(b.hat_a)) %*% .i(KmV_a.m) %*% (y_a.star - X_a %*% as.numeric(b.hat_a))

# Set up to verify conditional model by numerical derivatives - rmt **********************************************************************
# The following line stores all arguments required by the functions for which derivatives will be computed
## Those arguments are loaded into the function's env by get_args(), which should be the first line of the function body
c_ctx <- init_derivative_args(X_a = X_a, Z_a = Z_a, y_a = y_a, X_m = X_m, Z_m = Z_m, y_m = y_m, A = A,
																				b.hat_a = as.numeric(b.hat_a), Phi_Xi = Phi_Xi, Sigma = Sigma, trts = trts)

# Define functions of (co)variance parameters
.P_mm <- function(parm_value, ctx = get_context()) { # parm_value must be Phi_Xi
	args = get_args(ctx, parm_value)
	V_mm.i <- .i(Phi_Xi["m", "m"] * Z_m %*% A %*% t(Z_m))
	return(V_mm.i - V_mm.i %*% X_m %*% .i(t(X_m) %*% V_mm.i %*% X_m) %*% t(X_m) %*% V_mm.i)
}
.P_mm(Phi_Xi) == P_mm

.R_a <- function(parm_value, ctx = get_context()) { # parm_value must be Sigma
	args = get_args(ctx, parm_value)
	return(Sigma["a", "a"] * Z_a %*% t(Z_a))
}
.R_a(Sigma) == R_a

# I struggled mightily to figure out what KmV_a.m represented and finally decided it was
# 	the variance of y_a conditional on  K_m'y_m. The following notation is more intuitive to me.
.V_a.Km <- function(parm_value, ctx = get_context()) {
	args <- get_args(ctx, parm_value)
	V_aa <- Phi_Xi["a", "a"] * Z_a %*% A %*% t(Z_a) + .R_a(Sigma)
	V_am <- Phi_Xi["a", "m"] * Z_a %*% A %*% t(Z_m)
	return(V_aa - V_am %*% .P_mm(Phi_Xi) %*% t(V_am))
}
.V_a.Km(Phi_Xi) == KmV_a.m

.P_a.m <- function(parm_value, ctx = get_context()) {
	args <- get_args(ctx, parm_value)
	V_a.Km.i <- .i(.V_a.Km(parm_value))
	return(V_a.Km.i - V_a.Km.i %*% X_a %*% .i(t(X_a) %*% V_a.Km.i %*% X_a) %*% t(X_a) %*% V_a.Km.i)
}
.P_a.m(Phi_Xi) == P_a.m
.P_a.m(Sigma) == P_a.m

.y_a.star <- function(parm_value, ctx = get_context()) { # parm_value must be Phi_Xi
	args <- get_args(ctx, parm_value)
	Phi_Xi <- parm_value

	V_am <- Phi_Xi["a", "m"] * Z_a %*% A %*% t(Z_m)
	return(y["a"] - V_am %*% .P_mm(Phi_Xi) %*% (y_m))
}
.y_a.star(Phi_Xi) == y_a.star

.ytPy_a.m <- function(parm_value, ctx = get_context()) {
	args <- get_args(ctx, parm_value)
	V_a.Km.i <- .i(.V_a.Km(parm_value))
	y_a.star_0 <- .y_a.star(Phi_Xi) - X_a %*% as.numeric(b.hat_a)
	return(t(y_a.star_0) %*% V_a.Km.i %*% y_a.star_0)
}
.ytPy_a.m(Phi_Xi) == t(y_a.star- X_a %*% as.numeric(b.hat_a)) %*% .i(KmV_a.m) %*% (y_a.star - X_a %*% as.numeric(b.hat_a))
.ytPy_a.m(Phi_Xi) == t(y_a.star) %*% P_a.m %*% y_a.star
.ytPy_a.m(Sigma) == t(y_a.star- X_a %*% as.numeric(b.hat_a)) %*% .i(KmV_a.m) %*% (y_a.star - X_a %*% as.numeric(b.hat_a))
.ytPy_a.m(Sigma) == t(y_a.star) %*% P_a.m %*% y_a.star

.Lr_gls_a.m <- function(parm_value, ctx = get_context()) {
	args <- get_args(ctx, parm_value)
	V_a.Km <- .V_a.Km(parm_value)
	V_a.Km.i <- .i(V_a.Km)
	Lr_gls_a.m <- - (1/2) * .log(.det(V_a.Km))  - (1/2) * .log(.det(t(X_a) %*% V_a.Km.i %*% X_a)) - (1/2) * .ytPy_a.m(parm_value)
	return(Lr_gls_a.m)
}
.Lr_gls_a.m(Phi_Xi) == Lr_gls_a.m
.Lr_gls_a.m(Sigma) == Lr_gls_a.m
# **********************************************************************************************************************

#trait a variances straightforward
dV.dSaa_a.m := df_dp(.V_a.Km, Sigma, "a", "a")
. == (1/as.numeric(Sigma["a", "a"])) * R_a
dLr.dSaa_gls_a.m := df_dp(.Lr_gls_a.m, Sigma, "a", "a")
. == - (1/2) * tr(P_a.m %*% dV.dSaa_a.m) + (1/2) * t(y_a.star) %*% P_a.m %*% dV.dSaa_a.m %*% P_a.m %*% y_a.star

dV.dPaa_a.m := df_dp(.V_a.Km, Phi_Xi, "a", "a")
. == Z_a %*% A %*% t(Z_a)
dLr.dPaa_gls_a.m := df_dp(.Lr_gls_a.m, Phi_Xi, "a", "a")
. == - (1/2) * tr(P_a.m %*% dV.dPaa_a.m) + (1/2) * t(y_a.star) %*% P_a.m %*% dV.dPaa_a.m %*% P_a.m %*% y_a.star

# rmt ******************************************************************************************************************
# Elyse: I was not able to determine what path you took to get to your results that were correct but different in sign
# 	from what you expected.
## The derivations below get to your correct results on lines 360 and 376
#covariances
dV.dPam_a.m := df_dp(.V_a.Km, Phi_Xi, "a", "m") # rmt: I expected to have to multiply this by 2 because df_dp considers Pam and Pma to be distinct parameters; I need to allow PartitionedMatrix to be packed symmetric
. == -2*Z_a %*% A %*% t(Z_m)%*%P_mm%*%V_ma #derivative of the conditional variance w.r.t phi_am
dYstar.dPam_a.m := df_dp(.y_a.star, Phi_Xi, "a", "m")
. == -Z_a %*% A %*% t(Z_m) %*% P_mm %*% y_m #derivative of y_a* w.r.t phi_am
dP.dPam_a.m := df_dp(.P_a.m, Phi_Xi, "a", "m")
. == -P_a.m %*% dV.dPam_a.m %*% P_a.m #derivative of P_a.m w.r.t phi_am
dytPy.dPam_a.m := df_dp(.ytPy_a.m, Phi_Xi, "a", "m")
. == 2 * t(y_a.star) %*% P_a.m %*% dYstar.dPam_a.m + t(y_a.star) %*% dP.dPam_a.m %*% y_a.star  #derivative of y_a*' P_a.m y_a* w.r.t phi_am
dLr.dPam_gls_a.m := df_dp(.Lr_gls_a.m, Phi_Xi, "a", "m")
. == -(1/2)*tr(P_a.m %*% dV.dPam_a.m) - (1/2) * dytPy.dPam_a.m
. == -(1/2)*tr(P_a.m %*% dV.dPam_a.m) - (1/2) * (2 * t(y_a.star) %*% P_a.m %*% dYstar.dPam_a.m + t(y_a.star) %*% dP.dPam_a.m %*% y_a.star)
. == -(1/2)*tr(P_a.m %*% dV.dPam_a.m) - (1/2) * t(y_a.star) %*% dP.dPam_a.m %*% y_a.star - t(y_a.star) %*% P_a.m %*% dYstar.dPam_a.m
. == -(1/2)*tr(P_a.m %*% dV.dPam_a.m) - (1/2) * t(y_a.star) %*% (-P_a.m %*% dV.dPam_a.m %*% P_a.m) %*% y_a.star - t(y_a.star) %*% P_a.m %*% dYstar.dPam_a.m
. == -(1/2)*tr(P_a.m %*% dV.dPam_a.m) + (1/2) * (t(y_a.star) %*% P_a.m %*% dV.dPam_a.m %*% P_a.m %*% y_a.star) - t(y_a.star) %*% P_a.m%*%dYstar.dPam_a.m # Elyse's version

#trait m variances
dV.dPmm_a.m := df_dp(.V_a.Km, Phi_Xi, "m", "m")
. == V_am %*% P_mm %*% dV.dPmm_mm %*% P_mm %*% V_ma #derivative of the conditional variance w.r.t phi_mm
dYstar.dPmm_a.m :=  df_dp(.y_a.star, Phi_Xi, "m", "m")
. == V_am %*% P_mm %*% dV.dPmm_mm %*% P_mm %*% y_m #derivative of y_a* w.r.t phi_mm
dP.dPmm_a.m := df_dp(.P_a.m, Phi_Xi, "m", "m")
. == -P_a.m %*% dV.dPmm_a.m %*% P_a.m #derivative of P_a.m w.r.t phi_am
dytPy.dPmm_a.m := df_dp(.ytPy_a.m, Phi_Xi, "m", "m")
. == 2 * t(y_a.star) %*% P_a.m %*% dYstar.dPmm_a.m + t(y_a.star) %*% dP.dPmm_a.m %*% y_a.star  #derivative of y_a*' P_a.m y_a* w.r.t phi_mm
dLr.dPmm_gls_a.m := df_dp(.Lr_gls_a.m, Phi_Xi, "m", "m")
. == -(1/2) * tr(P_a.m %*% dV.dPmm_a.m) - (1/2) * dytPy.dPmm_a.m
. == -(1/2) * tr(P_a.m %*% dV.dPmm_a.m) - (1/2) * (2 * t(y_a.star) %*% P_a.m %*% dYstar.dPmm_a.m + t(y_a.star) %*% dP.dPmm_a.m %*% y_a.star)
. == -(1/2) * tr(P_a.m %*% dV.dPmm_a.m) - (1/2) * t(y_a.star) %*% dP.dPmm_a.m %*% y_a.star - t(y_a.star) %*% P_a.m %*% dYstar.dPmm_a.m
. == -(1/2) * tr(P_a.m %*% dV.dPmm_a.m) - (1/2) * t(y_a.star) %*% (-P_a.m %*% dV.dPmm_a.m %*% P_a.m) %*% y_a.star - t(y_a.star) %*% P_a.m %*% dYstar.dPmm_a.m
. == -(1/2) * tr(P_a.m %*% dV.dPmm_a.m) + (1/2) * (t(y_a.star) %*% P_a.m %*% dV.dPmm_a.m %*% P_a.m %*% y_a.star) - t(y_a.star) %*% P_a.m%*%dYstar.dPmm_a.m # Elyse's version
# **********************************************************************************************************************

## Note: the minus sign in front of the third term in both dLr.dPam_gls_a.m and dLr.dPmm_gls_a.m is not what I expect; I expected these to both be plus signs
### The reason I expect the plus sign is that it would arise from applying the product rule to the quadratic term (which is a + 1/2(..))
### The signs associated with derivatives of dYstar.dPmm_a.m, dYstar.dPam_a.m are kept separately in the definitions of those terms

# Verify the derivatives
dLr.dSaa_gls_am == dLr.dSaa_gls_a.m
dLr.dPaa_gls_am == dLr.dPaa_gls_a.m
dLr.dPam_gls_am == dLr.dPam_gls_a.m
dLr.dPmm_gls_am == dLr.dPmm_gls_a.m + dLr.dPmm_gls_mm

# Computationally feasible form of the conditional model

# Build a permutation matrix that reorders experimental units (e.g., animals) into 2 partitions
## The first partition is those without records and the second is those without records
## Thus, this permutation matrix is trait-specific
## The bottom partition is Z for that trait, where Z is a design matrix for a factor
##	that has either 0 or 1 record per experimental unit
## The top partition of the permutation matrix, labelled F, is the remaining rows of the identity matrix,
##	after those comprising Z, are removed
## FZ <- rbind(F, Z)
##	FZ'FZ = FZ FZ' = I_q, where I_q is identity corresponding to experimental units
.FZ <- function(Z) {
	null_rows <- setdiff(colnames(Z), rownames(Z))
	I_q <- Id(ncol(Z), t(Z))
	return(rbind(F = I_q[null_rows, ], Z = Z))
}
FZ_m := .FZ(Z_m)
crossprod(FZ_m) == Id(ncol(FZ_m), t(FZ_m))
tcrossprod(FZ_m) == Id(ncol(FZ_m), FZ_m)

# Var(u_m | K_m'y_m)
.G_a.Km <- function(parm_value, ctx = get_context()) {
	args <- get_args(ctx, parm_value)
	G_aa <- Phi_Xi["a", "a"] * A
	G_ma <- Phi_Xi["a", "m"] * Z_m %*% A
	V_mm.i <- .i(Phi_Xi["m", "m"] * Z_m %*% A %*% t(Z_m))
	P_mm <- V_mm.i - V_mm.i %*% X_m %*% .i(t(X_m) %*% V_mm.i %*% X_m) %*% t(X_m) %*% V_mm.i
	return(G_aa - t(G_ma) %*% .P_mm(Phi_Xi) %*% G_ma)
}
.V_a.Km(Phi_Xi) == Z_a %*% .G_a.Km(Phi_Xi) %*% t(Z_a) +  .R_a(Sigma)

.B_a.Km <- function(parm_value, ctx = get_context()) {
	args <- get_args(ctx, parm_value)
	B <- t(cbind(X = X_a, Z = Z_a)) %*% .i(.R_a(Sigma)) %*% cbind(X = X_a, Z = Z_a %*% .G_a.Km(Phi_Xi))
	B["Z", "Z"] <- B["Z", "Z"] + Id(ncol(Z_a), B["Z", "Z"])
	return(B)
}
B_a.m := .B_a.Km(Phi_Xi)

Lr2_a.m  <- - (1/2) * .log(.det(R_a)) - (1/2) * .log(.det(B_a.m)) -
	(1/2) * t(y_a.star- X_a %*% as.numeric(b.hat_a)) %*% .i(KmV_a.m) %*% (y_a.star - X_a %*% as.numeric(b.hat_a))

.Lr2_a.m <- function(parm_value, ctx = get_context()) {
	args <- get_args(ctx, parm_value)
	Lr_gls_a.m <- - (1/2) * .log(.det(.R_a(Sigma))) - (1/2) * .log(.det(.B_a.Km(Phi_Xi))) - (1/2) * .ytPy_a.m(parm_value)
	return(Lr_gls_a.m)
}
Lr2_a.m == Lr_gls_a.m
.Lr2_a.m(Phi_Xi) == Lr2_a.m
.Lr2_a.m(Sigma) == Lr2_a.m

cat("End of script")
