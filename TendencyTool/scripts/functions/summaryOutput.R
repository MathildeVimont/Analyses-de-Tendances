#' summaryOutput
#' 
#' A function that formats the output of a regression model
#' 
#' @param model an object containing results from the regression model 
#' @param distribution a `string` that is the distribution used in the model ("gaussian", "poisson", "binomial")
#' @param factorVariables a `vector` containing variables that should be treated as factor
#' @param transform a `boolean`, TRUE if coefficients should be tranformed due to distribution
#' @param scaling a `boolean`, TRUE if numeric variables have been scaled
#' 
#' @return 
#' A dataframe containing outputs of a regression model, that is correctly formatted 
#' 
#' @example 
summaryOutput <- function(model, 
                          distribution, 
                          factorVariables,
                          transform = FALSE,
                          scaling = FALSE){
  
  source(paste0(scrDir, "functions/transformDistrib.R"))
  
  # Extract estimates from the model
  coef <- summary(model)$coefficients$cond
  
  # Turn to dataframe
  dataCoef <- data.frame(coef)
  
  # Extract rownames from dataframe
  rows <- rownames(dataCoef)
  
  # Erase T/Z Values and save pvalues
  dataCoef$z.value <- NULL

  # # If numeric variables have been scaled, then rescale estimates
  # if (scaling){
  #   # Initialise empty lists of means and standard deviations
  #   listMean <- list() 
  #   listSd <- list()
  #   for (e in rownames(dataCoef)){
  #     
  #     if (e  %in% colnames(data)){
  #       
  #       # Extract standard deviations of numeric variables 
  #       stDev <- sd(data[,e], na.rm = T)
  #       listSd[e] <- stDev
  #       
  #       # Extract mean of numeric variables 
  #       mean <- mean(data[,e], na.rm = T)
  #       listMean[e] <- mean
  #       
  #       # Rescale estimates with this standard deviation
  #       dataCoef[e,]$Estimate <- dataCoef[e,]$Estimate / stDev
  #       
  #     }
  #   }
  #   
  # 
  #   dataCoef["(Intercept)",] <-dataCoef["(Intercept)",] 
  #   
  # }
  
  # Create confidence interval
  dataCoef$IC_inf <- dataCoef$Estimate - 1.96 * dataCoef$Std..Error
  dataCoef$IC_sup <- dataCoef$Estimate + 1.96 * dataCoef$Std..Error
  
  # Adapt transformation to distribution if required
  if (transform){
    dataCoef$Estimate <- transformDistrib(dataCoef$Estimate, distribution)
    dataCoef$Std..Error <- transformDistrib(dataCoef$Std..Error, distribution)
    dataCoef$IC_inf <- transformDistrib(dataCoef$IC_inf, distribution)
    dataCoef$IC_sup <- transformDistrib(dataCoef$IC_sup, distribution)

  }
  
  # Interpret pvalues as significant or not
  sign <- ifelse(dataCoef[,"Pr...z.."] < 0.05, "Yes", "No")
  
  # Round to 2 digits while keeping scientific notation
  dataCoef <- apply(X = dataCoef, 
                    MARGIN = 2, 
                    FUN = function(x) {
                      formatC(x, format = "e", digits = 2)
                    })
  
  
  # Keep format as data.frame
  dataCoef <- data.frame(dataCoef)

  # Add a significant column to the summary
  dataCoef$sign <- sign
  
  # Rename columns
  colnames(dataCoef) <- c("Estimates", "Standard Errors", "P Values", 
                          "IC_inf", "IC_sup", "Significant")
  
  # Reorder columns
  dataCoef = dataCoef[,c("Estimates", "Standard Errors", "IC_inf", 
                         "IC_sup", "P Values", "Significant")]
  
  # Reformat and rename rows
  for (f in factorVariables){
    rownames(dataCoef) <- str_replace(string = rows, 
                                      pattern = f,
                                      replacement = paste0(f, " : "))
    
  }
  
  return(dataCoef)
  
}
