# CACC
Conjunctive Analysis of Case Configurations

Usage

CACC(data)
CACC(data, x, y)
Arguments

data	
Data set
x	
Dependent variable (binary)
y	
Independents variables
Examples

library("CACC")
# ------------------------------- Conduct a CACC -------------------------------
# Alternative 1: let the function automatize the variable selection process.
CACC_matrix <- CACC(df)

# Alternative 2: define all variables, both independent variables and DV.
# matrix1 <- cacc(df, c("x1", "x2", "x3", "x4", "x5", "x6", "x7", "x8"), "y")

# Alternative 3: define the IV and let the function define the DV automatically.
# matrix2 <- cacc(df, c("x1", "x2", "x3", "x4", "x5", "x6", "x7", "x8"))

# Alternative 4: define the DV and let the function define the IV automatically.
# matrix3 <- cacc(df, y = "y")

print(CACC_matrix)

# ------------------------- Conduct a Chi-square test --------------------------
xsq <- CACC_XSQ(CACC_matrix)
