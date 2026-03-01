# Demonstrate that V_mm.i can be easily computed from the NRM inverse
# 	for a pedigree reduced to individuals with marker phenotypes

source("example_data.R")

# Test traits a and m for singular G
C <- build_MME(data, fixed_models = fixed_model[c("a", "m")], Phi, Xi, epsilon = 0, Upsilon, A.i, b)

# Make versions of A.i and A that are conformable with Z_m
A.i_m := singlePartMatrix(A.i, "m")
A_m := singlePartMatrix(.i(A.i), "m")

# Build the inverse of the subset of A corresponding to only animals with marker phenotypes
animal_M_idx <- which(ped1$label %in% as.character(data[!is.na(data[["m"]]), ]$ANIMAL))
animal_M <- ped1$label[animal_M_idx]
sire_M <- ped1$sire[animal_M_idx]
sire_M[!(sire_M %in% animal_M)] <- NA_character_
dam_M <- ped1$dam[animal_M_idx]
dam_M[!(dam_M %in% animal_M)] <- NA_character_
ped_M <- editPed(label = animal_M, sire = sire_M, dam = dam_M)
pedigree_M <- pedigree(label = ped_M$label, sire = ped_M$sire, dam = ped_M$dam)
A.i_M <- singlePartMatrix(getAInv(pedigree_M), "m")

# Build matrices necessary to perform computations on A.i
Z_a <- Z["a", "a"]
Z_m := Z["m", "m"]

# The marginal variance of phenotypes of trait m, V_mm, is Phi_mm times the rows and columns of A with those phenotypes
V_mm := Z_m %*% G["m", "m"] %*% t(Z_m)
. == Phi["m", "m"] * Z_m %*% A_m %*% t(Z_m)
. == Phi["m", "m"] * .i(A.i_M)

# A computationally feasible formula for the inverse of the marginal variance of m (V_m)
V_mm.i := .i(V_mm)
	. == (1 / Phi["m", "m"]) * A.i_M
