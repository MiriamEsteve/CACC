# CACC: Conjunctive Analysis of Case Configurations

# Usage
CACC(data)

CACC(data, x, y)

# Arguments

data	-- > Data set

x    -->  Dependent variable (binary)

y	   -->  Independents variables

# Examples

-------------------------- Conduct a CACC matrix ---------------------------

- Alternative 1: let the function automatize the variable selection process.
CACC_matrix <- CACC(df)

- Alternative 2: define all variables, both independent variables and DV.
CACC_matrix <- CACC(df, c("x1", "x2", "x3", "x4", "x5", "x6", "x7", "x8"), "y")

- Alternative 3: define the IV and let the function define the DV automatically.
 CACC_matrix <- CACC(df, c("x1", "x2", "x3", "x4", "x5", "x6", "x7", "x8"))

- Alternative 4: define the DV and let the function define the IV automatically.
 CACC_matrix <- CACC(df, y = "y")

print(CACC_matrix)

 -------------------------- Importance Variable  ------------------------------
 
imp_var <- importance_variable(df)

 ------------------------------- Main effect  ---------------------------------
 
main_eff <- main_effect(df)

 ------------------------- Conduct a Chi-square test --------------------------
 
xsq <- CACC_XSQ(CACC_matrix)

 -------------------------------- Conduct a SCI  ------------------------------
 
SCI <- SCI(CACC_matrix)

 ----------------------- Conduct a Plot Lorenz Curve  -------------------------
 
ggLorenzCurve(CACC_matrix)
