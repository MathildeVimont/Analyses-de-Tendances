#' analyseCoef
#' 
#' A function that calculates the trend and percentage of evolution from a regression model
#' 
#' @param model an object containing results from the regression model 
#' @param distribution a `string` that is the distribution used in the model ("gaussian", "poisson", "binomial")
#' @param effectVar a `string` that is the explanatory variable we aim to study (ex : "year")  
#' @param varRange an `integer`, that is the range of values of the effectVar. Default is 1.
#' 
#' @return 
#' A list of 2 elements:
#' - trend, that is the 
#' - perc, that is the percentage of evolution of abundance in between the varRange 
#' 
#' @example
#'  

analyseCoef <- function(model, distribution, 
                        effectVar = "year", 
                        varRange = 1){
  source(paste0(scrDir, "functions/transformDistrib.R"))
  
  # Extract estimates from the model
  coef <- summary(model)$coefficients$cond
  
  # Turn to dataframe
  dataCoef <- data.frame(coef)
  
  # Extract coefficient from the chosen explanatory variable
  interestCoef <- dataCoef[effectVar,]$Estimate
  
  # Extract the trend from the estimates 
  # "For an increase of x in year, the mean count was multiplied by trend"
  
  trend <- transformDistrib(value = interestCoef * varRange, 
                            distribution = distribution)

  # Turn the trend to a percentage of variation
  # "For an increase of x in year, the mean count was in/decreased by perc"
  perc <- 100 * (trend - 1)
  
  # Create a list with those indicators
  coefList <- list(trend = trend, perc = perc)
  
  # Round those indicators so that only 2 decimals are kept
  coefList <- lapply(coefList, function(x) round(x, 2))
  
  return(coefList)
  
}
