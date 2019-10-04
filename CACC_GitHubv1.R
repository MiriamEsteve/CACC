# ==============================================================================
# Project: CACC R package
# Script purpose: To develop functions for an automatic application of CACC
# Date: 20-12-2018
# Authors: Esteve, M., Moneva, A., & Hart, T. C.
# R version 1.0.1 (2019-10-03) -- "Feather Spray"
# ==============================================================================


# ==============================================================================
# Load packages
# ==============================================================================

library(tidyverse)
library(ggplot2)

# ==============================================================================
# CACC function with "dplyr" package
#     data = database
#     x = independent variables (IV)
#     y = dependet variable (DV)
# ==============================================================================

cacc <- function (data,
                 x = colnames(data[, - ncol(data)]),
                 y = colnames(data[, ncol(data)])) {

  # ------------------------------ Preprocessing -------------------------------
  # Check the dependent variable (DV):
  #   If it is not numeric, convert its values to numeric and replace them with
  #     0 / 1.
  #   If it is not binary, the function returns an ERROR.
  #   If it is binary:
  if (length(unique(data[[y]])) != 2) {
    stop ("ERROR. The dependent variable has to be binary")
  } else if (! is.numeric(data[[y]])) {
    print("ERROR. The dependent variable has to be numeric and binary.")
    print("Preprocessing...")

  #     Convert the variable into a factor,
    data[[y]] <- as.factor(data[[y]])

  #     Replace categories with 0 / 1.
    levels(data[[y]]) <- c(0, 1)
    print("Done.")

  #     Problem: if R understands that "No" > "Yes" then this variable gives the
  #     value "1" to "No" and "0" to "Yes".
  } else if (is.double(data[[y]])) {

  #       First, the variable must be converted into a integer.
    data[[y]] <- as.integer(data[[y]])

  #       Then, the variable must be converted into a factor.
    data[[y]] <- as.factor(data[[y]])

  #       Lastly, categories must be replaced with 0 / 1.
    levels(data[[y]]) <- c(0, 1)
  }

  # --------- Handle dominant profiles depending on predetermined size ---------
  if (nrow(data) < 1000) {
    dom_pro = 5
  } else {
    dom_pro = 10
  }

  # ------------------------- Generate the CACC matrix -------------------------
  # Generate a matrix with total frequencies.
  matrixT <- data %>%
    dplyr::count(.dots = x) %>%
    dplyr::arrange(desc(n)) %>%
    dplyr::rename(N_Break = "n")

  # Generate a matrix with DV frequencies based on the positive class (i.e. 1).
  matrix1 <- data %>%
    dplyr::filter(get(y) == 1) %>%
    dplyr::count(.dots = x) %>%
    dplyr::arrange(desc(n)) %>%
    dplyr::rename(N_1 = "n")

  # Calculate the probabilities for each dominant profile
  cacc_matrix <- dplyr::full_join(matrixT, matrix1,
                             by = x) %>%
    dplyr::mutate(p = N_1 / N_Break) %>%
    dplyr::filter(N_Break >= dom_pro) %>%
    dplyr::arrange(desc(p)) %>%
    dplyr::select(- one_of("N_1"))

  # Return the CACC matrix.
  return (cacc_matrix)
}


# ==============================================================================
# importance_variable function in base R
#     data = database
#     x = independent variables (IV)
#     y = dependet variable (DV)
# ==============================================================================
### Function that normalizes the results ###
normalize <- function(x) {
    return ((x - min(x)) / (max(x) - min(x)))
}

### Importance variable Ranking ###
importance_variable <- function(data,
                                x = colnames(data[, - ncol(data)]),
                                y = colnames(data[, ncol(data)])){
  #First, calculate CACC matrix
    #If the Break variable does not exist, cacc matrix is calculated.
  if(("N_Break" %in% names(data)) == FALSE){
    cacc_matrix <- cacc(data)
  }

  # Calculate total importance from cacc_matrix
  cacc_matrix[is.na(cacc_matrix)] <- 0   #Replace 0 to NA values

  # Weighted average
  mean_x <- mean(cacc_matrix$p)
  support <-  nrow(cacc_matrix)

  var_imp <- mean_x * support

  #Second, for each x calculate your CACC_matrix without it
  for(i in 1:length(x)){
    cacc <- cacc(data, x = x[-i])

    cacc[is.na(cacc)] <- 0   #Replace 0 to NA values

    mean_x <- mean(cacc$p)
    support <-  nrow(cacc)
    var_imp <-  union(var_imp, mean_x * support) #Add in a array P for each x
  }
  
  # Normalize results
  normal <- normalize(abs(var_imp - var_imp[1]))*100
  
  #Save result in a data frame
  Var_Importance <- data.frame("Attribute" = x,
                    "Importance.of.Variable" = normal[-1])

  #Sort descending
  Var_Importance <- Var_Importance[order(-Var_Importance$Importance.of.Variable), ]


  return(Var_Importance)
}

# ==============================================================================
# main_Effect function in base R
#     data = database
#     x = independent variables (IV)
#     y = dependet variable (DV)
# ==============================================================================
my_scan <- function(list_values, column_name){
  #Order ascending
  list_values <- t( list_values[order(list_values[[1]]), ] )
  cat("Analyze value for column ", column_name, " (values: ",list_values[1,],") or -1 (Skip this variable): ")
  value <- scan(nmax = 1)

  #If user don't enter a option repeat question
  if (length(value) == 0)
  {
    cat("Err value, repeat: column ", column_name, " (values: ",list_values[1,],") or -1 (Skip this variable): ")
    value <- scan(nmax = 1)
  
    while(is.na( match(value, list_values) ) & value != -1)
    {
      cat("Err value, repeat: column ", column_name, " (values: ",list_values[1,],") or -1 (Skip this variable): ")
      
      value <- scan(nmax = 1)
    }
  }else
    while(is.na( match(value, list_values) ) & value != -1)
    {
      cat("Err value, repeat: column ", column_name, " (values: ",list_values[1,],") or -1 (Skip this variable): ")
      
      value <- scan(nmax = 1)
    }
  
  
  return (value)
}

main_effect <- function(data,
                        x = colnames(data[, - ncol(data)]),
                        y = colnames(data[, ncol(data)])){
  #First, calculate CACC matrix
  cacc_matrix <- cacc(data)
  
  #Replace 0 to NA values
  cacc_matrix[is.na(cacc_matrix)] <- 0
  
  #Create Data Frame with vector differences of all DV
  columns <- length(x) #Number of VD
  
  list_total = list() #List of lists
  
  for(i in 1:columns){
    #Select and check values
    value <- my_scan(unique(cacc_matrix[i]), x[i])
    
    #If user decide to no analyze this variable continue
    if(value == -1){
      x <- x[-i]
      next
    }
    
    PDI <- cacc_matrix %>% 
      group_by_at(vars(-c(i, N_Break, p))) %>% 
      filter(n() > 1)  %>% 
      arrange(.[[i]], .by_group = TRUE) %>%
      mutate(diff = ifelse(
                            ((nth(p, which(.[[i]] == value)[1])) != p),
                                (nth(p, which(.[[i]] == value)[1]) - p ), 
                                (nth(p, which(.[[i]] == value)[1]))
                            )
            )

    #Add list into list of lists
    list_total[length(list_total)+1] <- list(PDI$diff)
  }
  
  #Create Data Frame
  dat <- data.frame()
  for(i in seq(along=list_total)) 
    for(j in seq(list_total[[i]]))
      dat[j,i] <- list_total[[i]][j]

  colnames(dat) <- x

  #Descriptive
  print(summary(dat))
  
  #Boxplot graph
  #boxplot(dat)
  #Pair key-value
  dat <- dat %>%
      gather()
  
  # Plot the main effects with ggplot
  print(
    ggplot(data = na.omit(dat),
           aes(x = reorder(x = key,
                           X = na.omit(dat$value),
                           FUN = median
                          ),
               y = value
           )
    ) +
      geom_boxplot() +
      scale_x_discrete(labels = x) +
      geom_hline(yintercept = 0,
                 linetype = 2) +
      xlab("Variables") +
      ylab("Main effect") +
      theme_classic()
  )
  
  return(dat)
}


# ==============================================================================
# CACC_Xsq function in base R
# ==============================================================================

cacc_xsq <- function (cacc_matrix){
  #If the Break variable does not exist, cacc matrix is calculated.
  if(("N_Break" %in% names(cacc_matrix)) == FALSE){
    cacc_matrix <- cacc(cacc_matrix)
    print("CACC matrix has been calculate")
  }

  # Declare the variable containing the number of times each dominant profile is
  #   observed in the sample.
  obs <- cacc_matrix$N_Break

  # Count the dominant profiles observed.
  N_obs <- nrow(data.frame(obs))

  # Obtain the expected count vector by weighting the total amount of dominant
  # observations by the amount of dominant profiles observed.
  exp <- rep(sum(obs) / N_obs, N_obs)

  # Perform the Chi-square test.
  # rescale.p = TRUE because probabilities must sum 1.
  xsq <- chisq.test(x = obs, p = exp, rescale.p = TRUE)

  #Show result
  xsq <- data.frame("X_Square" = xsq$statistic,
                    "df" = xsq$parameter,
                    "p" = xsq$p.value)
  rownames(xsq) <- NULL

  return (xsq)
}


# ==============================================================================
# data_prepare function in base R
# A function that prepares the cacc_matrix properly on which to apply the SCI
# ==============================================================================

data_prepare <- function(cacc_matrix){
  #If the Break variable does not exist, cacc matrix is calculated.
  if(("N_Break" %in% names(cacc_matrix)) == FALSE){
    cacc_matrix <- cacc(cacc_matrix)
    print("CACC matrix has been calculated")
  }

  #If the entered dataset have the necessary variables to calculate
  #the Lorentz Curve, the following are not calculated
  if(("N_Break_D" %in% names(cacc_matrix)) == TRUE){
    return(cacc_matrix)
  }
  
  #Total configuration cases
  N <- nrow(cacc_matrix)

  #N_Break ordered from lowest to highest
  cacc_matrix <- cacc_matrix[order(cacc_matrix$N_Break, decreasing = TRUE), ]

  #Insert new row with total sum of N_Break
  vector <- c(rep(0, length(cacc_matrix)))  #Vector of zeros
  cacc_matrix <- rbind(vector, cacc_matrix) #Add vector into dataframe
  cacc_matrix$N_Break[1] <- sum(cacc_matrix$N_Break) #Sum N_Break

  #Cumulative N_Break
  cacc_matrix$N_Break_D <- 0 #Initialise column

  for(i in 1:N){
    if(i == 1){
      cacc_matrix$N_Break_D[i] <- cacc_matrix$N_Break[i]
      next
    }
    cacc_matrix$N_Break_D[i] <- cacc_matrix$N_Break_D[i - 1] - cacc_matrix$N_Break[i]
  }

  #%CFD of N_Break_D
  maxAcumulative <- cacc_matrix$N_Break[1]
  cacc_matrix$p_N_Break_D <- cacc_matrix$N_Break_D/maxAcumulative

  #Column Configs
  vector <- seq(from = 1, to = 0, length.out = N + 1)  #Vector of zeros
  cacc_matrix$Config <- vector

  #N?? of Configs
  vector <- N:0  #Vector of number case configurations
  cacc_matrix$num_Config <- vector

  #CDF % of Configs
  cacc_matrix$p_Configs <- cacc_matrix$num_Config/N


  #Area under L Curve
  cacc_matrix$L_Curve <- 0 #Initialise column

  for (i in 1:N){
    cacc_matrix$L_Curve[i] <- ((cacc_matrix$p_N_Break_D[i+1]+cacc_matrix$p_N_Break_D[i])/2)*(1/N)
  }


  return (cacc_matrix)
}

# ==============================================================================
# SCI function in base R
# A function that, after preparing the cacc_matrix, calculates its SCI and the 
# A and B areas of the Lorenz Curve.
# ==============================================================================

sci <- function(cacc_matrix){
  #If the Break variable does not exist, cacc matrix is calculated.
  if(("N_Break" %in% names(cacc_matrix)) == FALSE){
    cacc_matrix <- cacc(cacc_matrix)
    print("CACC matrix has been calculated")
  }

  #If the entered dataset does not have the necessary variables to calculate
  #the Lorentz Curve, the following are calculated
  if(("N_Break_D" %in% names(cacc_matrix)) == FALSE){
    #Prepare cacc_matrix to calculate Curve Lorenz
    cacc_matrix <- data_prepare(cacc_matrix)
    print("Dataset has been prepared")
  }

  #Sum L_Curve to calculate area L Curve
  area_L_Curve <- sum(cacc_matrix$L_Curve)

  #Area A
  A <- 0.5 - area_L_Curve

  #Area B
  B <- area_L_Curve/0.5

  SCI <- 1-B

  return (SCI)
}

# ==============================================================================
# Plot Lorenz curve
# ==============================================================================
gg_lorenz_curve <- function(cacc_matrix){
  #If the Break variable does not exist, cacc matrix is calculated.
  if(("N_Break" %in% names(cacc_matrix)) == FALSE){
    cacc_matrix <- cacc(cacc_matrix)
    print("CACC matrix has been calculate")
  }

  #If the entered dataset does not have the necessary variables to calculate
  #the Lorentz Curve, the following are calculated
  if(("N_Break_D" %in% names(cacc_matrix)) == FALSE){
    #Prepare cacc_matrix to calculate Curve Lorenz
    cacc_matrix <- data_prepare(cacc_matrix)
    print("Dataset has been prepare")
  }

  # plot
  ggplot(data = cacc_matrix,
         mapping = aes(x = cacc_matrix$Config,
                       y = cacc_matrix$p_N_Break_D)) +
    ggtitle("Lorenz Curve") +
    geom_area() +
    scale_x_continuous(name = "Cumulative share of X",
                       limits = c(0, 1),
                       expand = c(0,0)) +
    scale_y_continuous(name = "Cumulative share of Y",
                       limits = c(0, 1),
                       expand = c(0,0)) +
    geom_abline() +
    geom_text(x=min(cacc_matrix$Config)+0.1,
              y=max(cacc_matrix$p_N_Break_D)-0.1,
              label = paste("SCI = ", round(sci(prepared_cacc_matrix_vict), digits = 3)),
              size = 5) +
    theme_bw() +
    theme(plot.title = element_text(hjust = 0.5),
          plot.margin = unit(x = c(.15, .2, .15, .15),
                             units = "in")
          )
}
