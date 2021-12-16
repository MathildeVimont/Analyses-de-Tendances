#' scaleData
#'
#' A function that scale numeric variables needed in a regression
#'
#' @param data a `data.frame` containing dependent and independent variables
#' @param fixedEffects a `vector` of variables that should be treated as fixed effects
#' @param randomEffects a `vector` of variables that should be treated as random effects
#' @param factorVariables a `vector` of variables that should be treated as factors
#'
#' @return
#' A dataframe where the requested variables have been scaled
#'
#' @export
#'
#' @example scaleData(data, fixedEffects = ")
scaleData <- function(data, fixedEffects = NULL,
                      randomEffects = NULL,
                      factorVariables = NULL){

  # Extract all variables of interest
  toScaleVars <- c(fixedEffects, randomEffects)

  # Erase the ones that are categorical
  if(!is.null(randomEffects)){
    toScaleVars <- toScaleVars[toScaleVars != factorVariables]
  }

  # Scale variables
  data[, toScaleVars] = scale(data[, toScaleVars])

  return(data)

}
