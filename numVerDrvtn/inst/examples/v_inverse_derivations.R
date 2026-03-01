source("example_data.R")

# Test traits a and b
# C <- build_MME(data, fixed_models = fixed_model[c("a", "b")], Phi, Xi, epsilon = 0, Upsilon, A.i, b)
V_ab <- build_model_components(data, fixed_models = fixed_model[c("a", "b")], Phi, Xi, epsilon = 0, Upsilon, A.i, b)
R.i <- .i(R)
# Form for singular G
V.i := R.i - R.i %*% Z %*% .i(I_g + G %*% t(Z) %*% R.i %*% Z) %*% G %*% t(Z) %*% R.i
  . == .i(V_ab)
# Form for non-singular G only
V.i := R.i - R.i %*% Z %*% .i(.i(G) + t(Z) %*% R.i %*% Z) %*% t(Z) %*% R.i
	. == .i(V_ab)

# Test traits a, b and m
# C <- build_MME(data, fixed_models = fixed_model[c("a", "b", "m")], Phi, Xi, epsilon = 0, Upsilon, A.i, b)
V <- build_model_components(data, fixed_models = fixed_model[c("a", "b", "m")], Phi, Xi, epsilon = 0, Upsilon, A.i, b)

# Build identity matrices with some blocks 0
n <- nrow(X)
n_m<- nrow(data[!is.na(m), ])
I_n10 <- partMatrixFromTemplate(drop0(Diagonal(n, x = c(rep(1, n - n_m), rep(0, n_m))), 1e-8), R)
I_n01 <- partMatrixFromTemplate(drop0(Diagonal(n, x = c(rep(0, n - n_m), rep(1, n_m))), 1e-8), R)

# Form for singular G
R.1 <- R
R.1["S", "S"] <- Id(nrow(R["S", "S"]), R["S", "S"])
R.i1 <- .i(R.1)
# v.i1 is a first guess at what the expression for V.i might look like
V.i1 := R.i1 - R.i1 %*% Z %*% .i(I_g + G %*% t(Z) %*% R.i1 %*% Z) %*% G %*% t(Z) %*% R.i1
# Derive expression for V times v.i1
VxV.i1 := V %*% (V.i1)

# The amount needed to be added to V.i1 to make it the inverse of V is:
F <- .i(V) %*% (as(I_n, "generalMatrix") - VxV.i1)
V.i <- .i(V)

  . == V %*% (R.i1 - R.i1 %*% Z %*% .i(I_g + G %*% t(Z) %*% R.i1 %*% Z) %*% G %*% t(Z) %*% R.i1)
  . == (Z %*% G %*% t(Z) + R) %*% (R.i1 - R.i1 %*% Z %*% .i(I_g + G %*% t(Z) %*% R.i1 %*% Z) %*% G %*% t(Z) %*% R.i1)
  . == (Z %*% G %*% t(Z) %*% (R.i1 - R.i1 %*% Z %*% .i(I_g + G %*% t(Z) %*% R.i1 %*% Z) %*% G %*% t(Z) %*% R.i1)
			 + R %*% (R.i1 - R.i1 %*% Z %*% .i(I_g + G %*% t(Z) %*% R.i1 %*% Z) %*% G %*% t(Z) %*% R.i1))
  . == (Z %*% G %*% t(Z) %*% R.i1
			 + R %*% R.i1
			 - Z %*% G %*% t(Z) %*% R.i1 %*% Z %*% .i(I_g + G %*% t(Z) %*% R.i1 %*% Z) %*% G %*% t(Z) %*% R.i1
			 - R %*% R.i1 %*% Z %*% .i(I_g + G %*% t(Z) %*% R.i1 %*% Z) %*% G %*% t(Z) %*% R.i1)
  . == (I_n10
			 + Z %*% G %*% t(Z) %*% R.i1
			 - Z %*% ((G %*% t(Z) %*% R.i1 %*% Z) %*% (.i(I_g + G %*% t(Z) %*% R.i1 %*% Z))) %*% G %*% t(Z) %*% R.i1
			 - I_n10 %*% Z %*% (.i(I_g + G %*% t(Z) %*% R.i1 %*% Z)) %*% G %*% t(Z) %*% R.i1
			 - I_n01 %*% Z %*% (.i(I_g + G %*% t(Z) %*% R.i1 %*% Z)) %*% G %*% t(Z) %*% R.i1
			 + I_n01 %*% Z %*% (.i(I_g + G %*% t(Z) %*% R.i1 %*% Z)) %*% G %*% t(Z) %*% R.i1)
  . == (I_n10
			 + Z %*% G %*% t(Z) %*% R.i1
			 - Z %*% ((G %*% t(Z) %*% R.i1 %*% Z) %*% (.i(I_g + G %*% t(Z) %*% R.i1 %*% Z))) %*% G %*% t(Z) %*% R.i1
			 - Z %*% ((I_g) %*% (.i(I_g + G %*% t(Z) %*% R.i1 %*% Z))) %*% G %*% t(Z) %*% R.i1
			 + I_n01 %*% Z %*% (.i(I_g + G %*% t(Z) %*% R.i1 %*% Z)) %*% G %*% t(Z) %*% R.i1)
  . == (I_n10
			 + I_n01 %*% Z %*% (.i(I_g + G %*% t(Z) %*% R.i1 %*% Z)) %*% G %*% t(Z) %*% R.i1)
print(I_n10)
print(I_n01 %*% Z %*% (.i(I_g + G %*% t(Z) %*% R.i1 %*% Z)) %*% G %*% t(Z) %*% R.i1)

# The previous derivation showed that V %*% V.i1 returns a matrix that is Identity for the block corresponding
#		to the traits with non-singular R
##	The next step is to find an algebraic expression for the second term, F, in V.i
##		that will make the rest of V %*% V.i1 equal to Identity
V %*% V.i1 ==
		I_n10 + I_n01 %*% Z %*% (.i(I_g + G %*% t(Z) %*% R.i1 %*% Z)) %*% G %*% t(Z) %*% R.i1
I_n10 + I_n01 ==
		V %*% V.i1 + V %*% F
V %*% F ==
		I_n10 + I_n01 - V %*% V.i1
VxF := I_n10 + I_n01 - (I_n10 + I_n01 %*% Z %*% (.i(I_g + G %*% t(Z) %*% R.i1 %*% Z)) %*% G %*% t(Z) %*% R.i1)
	. == I_n01 - I_n01 %*% Z %*% (.i(I_g + G %*% t(Z) %*% R.i1 %*% Z)) %*% G %*% t(Z) %*% R.i1
	. == (Z %*% G %*% t(Z) + R) %*% F
n_ab <- n - n_m
idx_ab <- seq_len(n_ab)
idx_m <- (n_ab + 1L):n

# Define variables representing the 4 quadrants of Z %*% G %*% t(Z) + R0
Z_a <- Z[c("a", "b"), c("a", "b")]
Z_m <- Z["m", "m"]
G_aa <- G[c("a", "b"), c("a", "b")]
G_am <- G[c("a", "b"), "m"]
G_ma <- G["m", c("a", "b")]
G_mm <- G["m", "m"]
R_ab <- R[c("a", "b"), c("a", "b")]
ZGZ_R_aa <- Z_a %*% G_aa %*% t(Z_a) + R_ab
ZGZ_am <- Z_a %*% G_am %*% t(Z_m)
ZGZ_ma <- Z_m %*% G_ma %*% t(Z_a)
ZGZ_mm <- Z_m %*% G_mm %*% t(Z_m)
F_aa <- F[idx_ab, idx_ab]
F_am <- F[idx_ab, idx_m]
F_ma <- F[idx_m, idx_ab]
F_mm <- F[idx_m, idx_m]

# Equate what the quadrants of V x F are symbolically to what they are observed to be numerically
# Solve for F_am in terms of F_mm
VxF_am := VxF[idx_ab, idx_m]
	. == (Z[idx_ab, ] %*% G %*% t(Z) + R[idx_ab, ]) %*% F[, idx_m]
	. == ZGZ_R_aa %*% F_am + ZGZ_am %*% F_mm
	. == partZeroes(VxF_am) # All zeroes
F_am == -.i(ZGZ_R_aa) %*% ZGZ_am %*% F_mm

# Solve for F_mm in terms of F_am
VxF_mm := VxF[idx_m, idx_m]
	. == (Z[idx_m, ] %*% G %*% t(Z) + R[idx_m, ]) %*% F[, idx_m]
	. == ZGZ_ma %*% F_am + ZGZ_mm %*% F_mm
I_m <- as(drop0(Diagonal(n_m), 1e-8), "generalMatrix")

# The following lines are not TRUE #***********************************************************************************
# .eq.last(I_m) # Identity
#
# .eq(F_mm,     .i(ZGZ_mm) %*% (I_m - ZGZ_ma %*% F_am))
# .eq(F_mm,     .i(ZGZ_mm) %*% (I_m + ZGZ_ma %*% .i(ZGZ_R_aa) %*% ZGZ_am %*% F_mm))
# .eq(F_mm - .i(ZGZ_mm) %*% ZGZ_ma %*% .i(ZGZ_R_aa) %*% ZGZ_am %*% F_mm,     .i(ZGZ_mm) %*% I_m)
# .eq((I_m - .i(ZGZ_mm) %*% ZGZ_ma %*% .i(ZGZ_R_aa) %*% ZGZ_am) %*% F_mm,      .i(ZGZ_mm))
# .eq(F_mm,     .i(I_m - .i(ZGZ_mm) %*% ZGZ_ma %*% .i(ZGZ_R_aa) %*% ZGZ_am) %*% .i(ZGZ_mm))
# .eq(F_mm,     .i(ZGZ_mm - ZGZ_ma %*% .i(ZGZ_R_aa) %*% ZGZ_am))
# # Express F_mm as a Schur complement of V
# S_mm <- ZGZ_mm - ZGZ_ma %*% .i(ZGZ_R_aa) %*% ZGZ_am
# .eq(F_mm,     .i(S_mm))
#
# # Express F_am as a function of the Schur complement of V
# .eq(F_am,   -.i(ZGZ_R_aa) %*% ZGZ_am %*% F_mm) #             This line is TRUE!
# .eq(F_am,   -.i(ZGZ_R_aa) %*% ZGZ_am %*% .i(S_mm))
# .eq(F_ma,   -.i(S_mm) %*% ZGZ_ma %*% .i(ZGZ_R_aa))
#*********************************************************************************************************************

# Solve for F_aa in terms of F_ma
.asgn(VxF_aa,   VxF[idx_ab, idx_ab])
.eq.last((Z[idx_ab, ] %*% G %*% t(Z) + R[idx_ab, ]) %*% F[, idx_ab])
.eq.last(ZGZ_R_aa %*% F_aa + ZGZ_am %*% F_ma)
.eq.last(partZeroes(VxF_aa)) # All zeroes
.eq(F_aa, -.i(ZGZ_R_aa) %*% ZGZ_am %*% F_ma)

# The following lines are not TRUE #**********************************************************************************
# .eq(F_aa, .i(ZGZ_R_aa) %*% ZGZ_am %*% .i(S_mm) %*% ZGZ_ma %*% .i(ZGZ_R_aa))
#
# # The form of the blocks of F being expressed as functions of the Schur complement of V suggests:
# .asgn(F,   .i(V) %*% (I_n - VxV.i1))
# V.i2 <- V.i
# V.i2 <- subassign(V.i2, idx_ab, idx_ab, V.i2[idx_ab, idx_ab] - .i(ZGZ_R_aa))
# .eq.last(V.i2)
#
# # Put F back together
# .eq.last(rbind(cbind(F_aa, F_am),
# 							 cbind(F_ma, F_mm))) #                             This line is TRUE!
# .eq(.i(V),   rbind(cbind(F_aa + .i(ZGZ_R_aa), F_am),
# 									 cbind(F_ma, F_mm)))
#********************************************************************************************************************

# Adjust V.i1 with F
.eq(V.i1,   R.i1 - R.i1 %*% Z %*% .i(I_g + G %*% t(Z) %*% R.i1 %*% Z) %*% G %*% t(Z) %*% R.i1)
.asgn(V.i3,   V.i1 + F)
drop0(V %*% V.i3, tol = 1e-12)
.eq.last(R.i1 - R.i1 %*% Z %*% .i(I_g + G %*% t(Z) %*% R.i1 %*% Z) %*% G %*% t(Z) %*% R.i1 + F)

# The following line is not TRUE #***********************************************************************************
# .eq.last(R.i1 - R.i1 %*% Z %*% .i(I_g + G %*% t(Z) %*% R.i1 %*% Z) %*% G %*% t(Z) %*% R.i1
# 				 + rbind(cbind(.i(ZGZ_R_aa) %*% ZGZ_am %*% .i(S_mm) %*% ZGZ_ma %*% .i(ZGZ_R_aa), -.i(ZGZ_R_aa) %*% ZGZ_am %*% .i(S_mm)),
# 				 				cbind(-.i(S_mm) %*% ZGZ_ma %*% .i(ZGZ_R_aa), .i(S_mm))))

cat("End of script")
