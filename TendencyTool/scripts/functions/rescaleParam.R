#' rescaleParam
#'
#' A function that transform estimates associated with scaled variables
#'
#' @param data a `data.frame` containing dependent and independent variables
#' @param fixedEffects a `vector` of variables that should be treated as fixed effects
#' @param factorVariables a `vector` of variables that should be treated as factors
#'
#' @return
#' A dataframe where the requested variables have been scaled
#'
#' @export
#'
#' @example scaleData(data, fixedEffects = ")
rescaleParam <- function(dataCoef, data, 
                         fixedEffects = NULL,
                         factorVariables = NULL){
  
  # Extract numeric variables from effects 
  numVar <- fixedEffects[!match(fixedEffects, factorVariables)]
  
  # Initialize the value of estimates / sd related to the intercept 
  interceptEst <- dataCoef["(Intercept)",]$Estimate
  interceptStdErr <- dataCoef["(Intercept)",]$Std..Error
  
  for (v in numVar){
    # Extract mean and standard deviation from original dataset
    meanVar <- mean(data[,v])
    sdVar <- sd(data[,v])
    
    # Calculate new coefficients and standard deviations for the considered variable
    # Bs = B(X) / sd(X)
    dataCoef[v,]$Estimate <- dataCoef[v,]$Estimate / sdVar
    dataCoef[v,]$Std..Error <- dataCoef[v,]$Std..Error / sdVar
    
    # Calculate new coefficients and standard deviations for intercept
    # A = As - Bs(X) * mean(X) / sd(X)
    interceptEst <- interceptEst - (dataCoef[v,]$Estimate * meanVar / sdVar)
    interceptStdErr <- interceptStdErr - (dataCoef[v,]$interceptStdErr * meanVar / sdVar)
  }
  
  # Calculate new coefficients and standard deviations for the intercept
  dataCoef["(Intercept)",]$Estimate <- interceptEst
  dataCoef["(Intercept)",]$Std..Error <- interceptStdErr
  
  return(dataCoef)
  
}

