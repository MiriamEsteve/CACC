<a href="https://zenodo.org/badge/latestdoi/209760615"><img src="https://zenodo.org/badge/209760615.svg" alt="DOI"></a>
 
# CACC: Conjunctive Analysis of Case Configurations

# Usage
cacc(data)

cacc(data, x, y)

# Arguments

data	-- > Data set

x    -->  Dependent variable (binary)

y	   -->  Independents variables

# Examples

------------------------------ Read dataset (df) ---------------------------

df <- read_excel(path = "", sheet = "")

 ------------------------------- Conduct a CACC -------------------------------
- Alternative 1: let the function automatize the variable selection process.

cacc_matrix <- cacc(df)


- Alternative 2: define all variables, both independent variables and DV.

cacc_matrix <- cacc(df, c("x1", "x2", "x3", "x4", "x5", "x6", "x7", "x8"), "y")

- Alternative 3: define the IV and let the function define the DV automatically.

cacc_matrix <- cacc(df, c("x1", "x2", "x3", "x4", "x5", "x6", "x7", "x8"))

- Alternative 4: define the DV and let the function define the IV automatically.

cacc_matrix <- cacc(df, y = "y")

 -------------------------- Importance Variable  ------------------------------
- Alternative 1: let the function automatize the variable selection process.

imp_var <- importance_variable(df)


- Alternative 2: define all variables, both independent variables and DV.

imp_var <- importance_variable(df, c("x1", "x2", "x3", "x4", "x5", "x6", "x7", "x8"), "y")

- Alternative 3: define the IV and let the function define the DV automatically.

imp_var <- importance_variable(df, c("x1", "x2", "x3", "x4", "x5", "x6", "x7", "x8"))

- Alternative 4: define the DV and let the function define the IV automatically.

imp_var <- importance_variable(df, y = "y")

 ------------------------------- Main effect  ---------------------------------
- Alternative 1: let the function automatize the variable selection process.

main_eff <- main_effect(df)


- Alternative 2: define all variables, both independent variables and DV.

main_eff <- main_effect(df, c("x1", "x2", "x3", "x4", "x5", "x6", "x7", "x8"), "y")

- Alternative 3: define the IV and let the function define the DV automatically.

main_eff <- main_effect(df, c("x1", "x2", "x3", "x4", "x5", "x6", "x7", "x8"))

- Alternative 4: define the DV and let the function define the IV automatically.

main_eff <- main_effect(df, y = "y")

 ------------------------- Conduct a Chi-square test --------------------------

xsq <- cacc_xsq(CACC_matrix)

 -------------------------------- Conduct a SCI  ------------------------------

sci <- sci(CACC_matrix)

 ----------------------- Conduct a Plot Lorenz Curve  -------------------------

gg_lorenz_curve(CACC_matrix)

