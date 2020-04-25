# ==============================================================================
# Project: CACC R package
# Script purpose: To develop functions for an automatic application of CACC
# Date: 2020-04-15
# Authors: Esteve, M., Moneva, A., & Hart, T. C.
# R version 4.0.0 (2020-04-24) -- "Arbor Day"
# ==============================================================================


# ==============================================================================
# Load packages
# ==============================================================================
library(tidyr)
library(dplyr)
library(stats)
library(ggplot2)

# ==============================================================================
# CACC function with "dplyr" package
#     dataset = database
#     x = independent variables (IV)
#     y = dependet variable (DV)
#!diagnostics off
# ==============================================================================
cacc <- function (dataset,x, y) {
  
  # If CACC matrix is already calculated stop execution
  if(("N_Break" %in% names(dataset)) == TRUE){
    print("CACC matrix is already calculated")
    return(dataset)
  }
  
  # Dependent variable
  x = colnames(dataset[, - ncol(dataset)])
  # Independet variable
  y = colnames(dataset[, ncol(dataset)])
  # ------------------------------ Preprocessing -------------------------------
  # Check the dependent variable (DV):
  #   if it is not numeric, convert its values to numeric and replace them with
  #     0 / 1.
  #   if it is not binary, the function returns an ERROR.
  #   if it is binary.
  # ----------------------------------------------------------------------------
  if (length(unique(dataset[[y]])) != 2) {
    stop ("ERROR. The dependent variable must be binary")
  } else if (! is.numeric(dataset[[y]])) {
    print("ERROR. The dependent variable must be numeric and binary.")
    print("Preprocessing...")
    
    # Convert the variable into a factor,
    dataset[[y]] <- as.factor(dataset[[y]])
    
    # Replace categories with 0 / 1.
    levels(dataset[[y]]) <- c(0, 1)
    print("Done!")
    
  } else if (is.double(dataset[[y]])) {
    
    # First, the variable must be converted into a integer.
    dataset[[y]] <- as.integer(dataset[[y]])
    
    # Second, the variable must be converted into a factor.
    dataset[[y]] <- as.factor(dataset[[y]])
    
    # Third, categories must be replaced with 0 / 1.
    levels(dataset[[y]]) <- c(0, 1)
  }
  
  # --------- Handle dominant profiles depending on sample size ---------
  if (nrow(dataset) < 1000) {
    dom_pro = 5
  } else {
    dom_pro = 10
  }
  
  # ------------------------- Generate the CACC matrix -------------------------
  # Generate a matrix with total frequencies.
  matrixT <- dataset %>%
    dplyr::count(.dots = x) %>%
    dplyr::arrange(desc(n)) %>%
    dplyr::rename(N_Break = "n")
  
  # Generate a matrix with DV frequencies based on the positive class (i.e. 1).
  matrix1 <- dataset %>%
    dplyr::filter(get(y) == 1) %>%
    dplyr::count(.dots = x) %>%
    dplyr::arrange(desc(n)) %>%
    dplyr::rename(N_1 = "n")
  
  # Calculate the probabilities for each dominant profile
  cacc_matrix <- dplyr::full_join(matrixT,
                                  matrix1,
                                  by = x
  ) %>%
    dplyr::mutate(p = N_1 / N_Break) %>%
    dplyr::filter(N_Break >= dom_pro) %>%
    dplyr::arrange(desc(p)) %>%
    dplyr::select(- one_of("N_1"))
  
  # Return the CACC matrix.
  return (cacc_matrix)
}

# ==============================================================================
# main_Effect function in base R
#     data = database
#     x = independent variables (IV)
#     y = dependet variable (DV)
#!diagnostics off
# ==============================================================================
### Function that requests the values of each IV as DV ###
my_scan <- function(list_values, column_name){
  # Order ascending
  list_values <- t( list_values[order(list_values[[1]]), ] )
  cat("Analyze value for column ", column_name, " (values: ",list_values[1,],") or -1 (Skip this variable): \n")
  value <- scan(nmax = 1)
  
  # If user don't enter a option repeat question
  if (length(value) == 0)
  {
    cat("ERROR value, repeat: column ", column_name, " (values: ",list_values[1,],") or -1 (Skip this variable): \n")
    value <- scan(nmax = 1)
    
    while(is.na( match(value, list_values) ) & value != -1)
    {
      cat("ERROR value, repeat: column ", column_name, " (values: ",list_values[1,],") or -1 (Skip this variable): \n")
      
      value <- scan(nmax = 1)
    }
  }else
    while(is.na( match(value, list_values) ) & value != -1)
    {
      cat("ERROR value, repeat: column ", column_name, " (values: ",list_values[1,],") or -1 (Skip this variable): \n")
      
      value <- scan(nmax = 1)
    }
  
  
  return (value)
}

### Function Main Effect ###
main_effect <- function(dataset, x, y){
  # First, calculate CACC matrix
  #   If the Break variable does not exist, cacc matrix is calculated.
  if(("N_Break" %in% names(dataset)) == FALSE){
    # Dependent variables
    x = colnames(dataset[, - ncol(dataset)])
    #Independent variables
    y = colnames(dataset[, ncol(dataset)])
    #Cacc matrix
    cacc_matrix <- cacc(dataset)
  }else{
    cacc_matrix <- dataset
    # Dependent variables
    x = colnames(dataset[, !names(dataset) %in% c("N_Break", "p")])
    #Independent variables
    y = colnames(dataset[, ncol(dataset)])
  }
  
  # Replace 0 to NA values
  cacc_matrix[is.na(cacc_matrix)] <- 0
  
  # Create Data Frame with vector differences of all DV
  columns <- length(x) #Number of DV
  
  # Colnames list with DV that have grouping
  col_names <- list()
  
  list_total = list() #List of lists
  
  for(i in 1:columns){
    # Select and check values
    list_values <- unique(cacc_matrix[i])
    
    # If DV is unevaluated continues with the following variable
    if(nrow(list_values) < 2){
      cat("\nWARNING. The function has ignored the variable", x[i], "because it is unevaluated\n\n")
      next #It continues with the following variable
    }
    
    value <- my_scan(list_values, x[i])
    
    # If user decide to no analyze this variable continue
    if(value == -1){
      x <- x[-i]
      columns <- length(x) #New number of DV
      next #It continues with the following variable
    }
    
    # Grouping IV with x[i] variable as DV
    PDI <- cacc_matrix %>%
      dplyr::group_by_at(dplyr::vars(-c(i, N_Break, p))) %>%
      dplyr::filter(dplyr::n() > 1)
    
    # If there is no grouping for this variable with its value it is ignored
    if(nrow(PDI) == 0){
      cat("WARNING. The function has ignored the variable", x[i], "with value", value, "because it is not exist one grouping\n\n")
      next #It continues with the following variable
    }
    else{
      PDI <- PDI  %>%
        dplyr::arrange(.[[i]], .by_group = TRUE) %>%
        dplyr::mutate(diff = if_else(
          ((dplyr::nth(p, which(.[[i]] == value)[1])) != p),
          ((dplyr::nth(p, which(.[[i]] == value)[1])) - p ),
          (dplyr::nth(p, which(.[[i]] == value)[1]))
        )
        )
      col_names <- c(col_names, x[i])
    }
    
    # Add list into list of lists
    list_total[length(list_total)+1] <- list(PDI$diff)
  }
  
  # Create Data Frame
  dataF <- data.frame()
  for(i in seq(along=list_total))
    for(j in seq(list_total[[i]]))
      dataF[j,i] <- list_total[[i]][j]
  
  # Columns names with grouping
  colnames(dataF) <- col_names
  
  # Descriptive
  print(summary(dataF))
  
  # Boxplot graph
  #   Pair key-value
  dataF <- dataF %>%
    tidyr::gather()
  
  # Plot the main effects with ggplot
  print(
    ggplot2::ggplot(data = na.omit(dataF),
                    ggplot2::aes(x = stats::reorder(x = key,
                                                    X = stats::na.omit(dataF$value),
                                                    FUN = stats::median),
                                 y = value)
    ) +
      ggplot2::geom_boxplot() +
      ggplot2::geom_hline(yintercept = 0,
                          linetype = 2) +
      ggplot2::xlab("Variables") +
      ggplot2::ylab("Main effect") +
      ggplot2::theme_bw()
  )
  
  return(dataF)
}


# ==============================================================================
# CACC_Xsq function in base R
#!diagnostics off
# ==============================================================================
cacc_xsq <- function (dataset, x, y){
  # First, calculate CACC matrix
  #   If the Break variable does not exist, cacc matrix is calculated.
  if(("N_Break" %in% names(dataset)) == FALSE){
    # Dependent variables
    x = colnames(dataset[, - ncol(dataset)])
    #Independent variables
    y = colnames(dataset[, ncol(dataset)])
    #Cacc matrix
    cacc_matrix <- cacc(dataset)
  }else{
    cacc_matrix <- dataset
    # Dependent variables
    x = colnames(dataset[, !names(dataset) %in% c("N_Break", "p")])
    #Independent variables
    y = colnames(dataset[, ncol(dataset)])
  }
  
  # Declare the variable containing the number of times each dominant profile is
  #   observed in the sample.
  obs <- cacc_matrix$N_Break
  
  # Count the dominant profiles observed.
  N_obs <- nrow(data.frame(obs))
  
  # Obtain the expected count vector by weighting the total amount of dominant
  #   observations by the amount of dominant profiles observed.
  exp <- rep(sum(obs) / N_obs, N_obs)
  
  # Perform the Chi-square test.
  #   rescale.p = TRUE because probabilities must sum 1.
  xsq <- chisq.test(x = obs, p = exp, rescale.p = TRUE)
  
  # Show result
  xsq <- data.frame("X_Square" = xsq$statistic,
                    "df" = xsq$parameter,
                    "p" = xsq$p.value)
  rownames(xsq) <- NULL
  
  return (xsq)
}

# ==============================================================================
# Lorenz curve function in base R
#   A function that, after preparing the cacc_matrix, calculates its SCI and the
#   A and B areas of the Lorenz Curve.
# ==============================================================================
### Function that prepares the cacc_matrix properly on which to apply the SCI ###
data_prepare <- function(cacc_matrix){
  # Total configuration cases
  N <- nrow(cacc_matrix)
  
  # N_Break ordered from lowest to highest
  cacc_matrix <- cacc_matrix[order(cacc_matrix$N_Break, decreasing = TRUE), ]
  
  # Insert new row with total sum of N_Break
  vector <- c(rep(0, length(cacc_matrix)))  #Vector of zeros
  cacc_matrix <- rbind(vector, cacc_matrix) #Add vector into dataframe
  cacc_matrix$N_Break[1] <- sum(cacc_matrix$N_Break) #Sum N_Break
  
  # Cumulative N_Break
  cacc_matrix$N_Break_D <- 0 #Initialise column
  
  for(i in 1:N){
    if(i == 1){
      cacc_matrix$N_Break_D[i] <- cacc_matrix$N_Break[i]
      next
    }
    cacc_matrix$N_Break_D[i] <- cacc_matrix$N_Break_D[i - 1] - cacc_matrix$N_Break[i]
  }
  
  # %CFD of N_Break_D
  maxAcumulative <- cacc_matrix$N_Break[1]
  cacc_matrix$p_N_Break_D <- cacc_matrix$N_Break_D/maxAcumulative
  
  # Column Configs
  vector <- seq(from = 1, to = 0, length.out = N + 1)  #Vector of zeros
  cacc_matrix$Config <- vector
  
  # Number of Configs
  vector <- N:0  #Vector of number case configurations
  cacc_matrix$num_Config <- vector
  
  # CDF % of Configs
  cacc_matrix$p_Configs <- cacc_matrix$num_Config/N
  
  
  # Area under L Curve
  cacc_matrix$L_Curve <- 0 #Initialise column
  
  for (i in 1:N){
    cacc_matrix$L_Curve[i] <- ((cacc_matrix$p_N_Break_D[i+1]+cacc_matrix$p_N_Break_D[i])/2)*(1/N)
  }
  
  
  return (cacc_matrix)
}

### Function that calculate SCI value ###
sci <- function(dataset, x, y){
  # First, calculate CACC matrix
  #   If the Break variable does not exist, cacc matrix is calculated.
  if(("N_Break" %in% names(dataset)) == FALSE){
    # Dependent variables
    x = colnames(dataset[, - ncol(dataset)])
    #Independent variables
    y = colnames(dataset[, ncol(dataset)])
    #Cacc matrix
    cacc_matrix <- cacc(dataset)
  }else{
    cacc_matrix <- dataset
    # Dependent variables
    x = colnames(dataset[, !names(dataset) %in% c("N_Break", "p")])
    #Independent variables
    y = colnames(dataset[, ncol(dataset)])
  }
  
  # If the entered dataset does not have the necessary variables to calculate
  #   the Lorentz Curve, the following are calculated
  if(("N_Break_D" %in% names(cacc_matrix)) == FALSE){
    # Prepare cacc_matrix to calculate Curve Lorenz
    cacc_matrix <- data_prepare(cacc_matrix)
  }
  
  # Sum L_Curve to calculate area L Curve
  area_L_Curve <- sum(cacc_matrix$L_Curve)
  
  # Area A
  A <- 0.5 - area_L_Curve
  
  # Area B
  B <- area_L_Curve/0.5
  
  SCI <- 1-B
  
  return (SCI)
}

### Function that plot Lorenz Curve ###
gg_lorenz_curve <- function(dataset, x, y){
  # First, calculate CACC matrix
  #   If the Break variable does not exist, cacc matrix is calculated.
  if(("N_Break" %in% names(dataset)) == FALSE){
    # Dependent variables
    x = colnames(dataset[, - ncol(dataset)])
    #Independent variables
    y = colnames(dataset[, ncol(dataset)])
    #Cacc matrix
    cacc_matrix <- cacc(dataset)
  }else{
    cacc_matrix <- dataset
    # Dependent variables
    x = colnames(dataset[, !names(dataset) %in% c("N_Break", "p")])
    #Independent variables
    y = colnames(dataset[, ncol(dataset)])
  }
  
  # If the entered dataset does not have the necessary variables to calculate
  #   the Lorentz Curve, the following variables are calculated
  if(("N_Break_D" %in% names(cacc_matrix)) == FALSE){
    # Prepare a cacc_matrix to calculate the Lorenz Curve
    cacc_matrix <- data_prepare(cacc_matrix)
  }
  
  # Plot Lorenz Curve
  ggplot2::ggplot(data = cacc_matrix,
                  mapping = aes(x = Config,
                                y = p_N_Break_D)) +
    ggplot2::ggtitle("Lorenz Curve") +
    ggplot2::geom_area() +
    ggplot2::scale_x_continuous(name = "Cumulative share of X",
                                limits = c(0, 1),
                                expand = c(0, 0)) +
    ggplot2::scale_y_continuous(name = "Cumulative share of Y",
                                limits = c(0, 1),
                                expand = c(0, 0)) +
    ggplot2::geom_abline() +
    ggplot2::annotate(geom = "text",
                      x = min(cacc_matrix$Config) + 0.15,
                      y = max(cacc_matrix$p_N_Break_D) - 0.1,
                      label = paste("SCI = ", round(sci(cacc_matrix), digits = 3)),
                      size = 5) +
    ggplot2::theme_bw() +
    ggplot2::theme(plot.title = element_text(hjust = 0.5),
                   plot.margin = unit(x = c(.15, .2, .15, .15),
                                      units = "in"))
}
