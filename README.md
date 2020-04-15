<a href="https://zenodo.org/badge/latestdoi/209760615"><img src="https://zenodo.org/badge/209760615.svg" alt="DOI"></a>
 
# CACC R package: Conjunctive Analysis of Case Configurations

# Usage
cacc(dataset)

cacc(dataset, x, y)

# Arguments

dataset	-- > Data set

x    -->  Dependent variable (binary)

y	   -->  Independents variables

# Examples

------------------------------ Read dataset (df) ---------------------------

dataset <- read_excel(path = "", sheet = "")

 ------------------------------- Conduct a CACC -------------------------------
- Alternative 1: let the function automatize the variable selection process.

cacc_matrix <- cacc(dataset)


- Alternative 2: define all variables, both independent variables and DV.

cacc_matrix <- cacc(dataset, c("x1", "x2", "x3", "x4", "x5", "x6", "x7", "x8"), "y")

- Alternative 3: define the IV and let the function define the DV automatically.

cacc_matrix <- cacc(dataset, c("x1", "x2", "x3", "x4", "x5", "x6", "x7", "x8"))

- Alternative 4: define the DV and let the function define the IV automatically.

cacc_matrix <- cacc(dataset, y = "y")

 -------------------------- Importance Variable  ------------------------------
- Alternative 1: let the function automatize the variable selection process.

imp_var <- importance_variable(dataset)


- Alternative 2: define all variables, both independent variables and DV.

imp_var <- importance_variable(dataset, c("x1", "x2", "x3", "x4", "x5", "x6", "x7", "x8"), "y")

- Alternative 3: define the IV and let the function define the DV automatically.

imp_var <- importance_variable(dataset, c("x1", "x2", "x3", "x4", "x5", "x6", "x7", "x8"))

- Alternative 4: define the DV and let the function define the IV automatically.

imp_var <- importance_variable(dataset, y = "y")

 ------------------------------- Main effect  ---------------------------------
- Alternative 1: let the function automatize the variable selection process.

main_eff <- main_effect(dataset)


- Alternative 2: define all variables, both independent variables and DV.

main_eff <- main_effect(dataset, c("x1", "x2", "x3", "x4", "x5", "x6", "x7", "x8"), "y")

- Alternative 3: define the IV and let the function define the DV automatically.

main_eff <- main_effect(dataset, c("x1", "x2", "x3", "x4", "x5", "x6", "x7", "x8"))

- Alternative 4: define the DV and let the function define the IV automatically.

main_eff <- main_effect(dataset, y = "y")

 ------------------------- Conduct a Chi-square test --------------------------

xsq <- cacc_xsq(dataset)

 -------------------------------- Conduct a SCI  ------------------------------

sci <- sci(dataset)

 ----------------------- Conduct a Plot Lorenz Curve  -------------------------

gg_lorenz_curve(dataset)

