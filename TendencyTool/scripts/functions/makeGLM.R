#' makeGLM
#'
#' A function that makes a GLM with specified
#'
#' @param data a `dataframe` containing the variables used in the GLM
#' @param fixedEffects a `vector` of variables that should be treated as fixed effects
#' @param randomEffects a `vector` of variables that should be treated as random effects
#' @param factorVariables a `vector` of variables that should be treated as factors
#' @param method a `string` corresponding to the method that must be applied (by default : "glm")
#' @param distribution a `string` containing the chosen distribution (by default : "poisson")
#' @param scaling a `boolean` indicating whether numeric variables should be scaled
#'
#' @importFrom glmmTMB glmmTMB
#'
#' @return a glm objects
#' @example
makeGLM <- function(data, interestVar = "count", fixedEffects = NULL,
                    randomEffects = NULL, factorVariables = NULL,
                    method = "glm", distribution = "poisson",
                    scaling = FALSE){
  ####################
  # Error management #
  ####################
  
  # Check class of data
  if (class(data) != "data.frame"){
    stop("'data' should be a data.frame")
  }

  # Check that all variables exist
  vars <- c(interestVar, fixedEffects, randomEffects)

  if (any(!(vars %in% colnames(data)))){
    missingVars <- vars[which(!(vars %in% colnames(data)))]
    stop("the variable '", paste0(missingVars, "' is not found in dataframe"))
  }
  
  # Check distribution exists
  if (!(distribution %in% c("binomial", "gaussian", "poisson"))){
    stop("the chosen distribution doesn't exist. \nPlease chose between : binomial, gaussian or poisson")
  }
  
  
  ####################
  # Import Functions #
  ####################
  # To erase when package
  source(paste0(scrDir, "functions/scaleData.R"))
  source(paste0(scrDir, "functions/writeFormula.R"))
  
  ###############
  # Format data #
  ###############
  
  # Check that factor variables correspond to factor columns
  
  if (!is.null(factorVariables)){
    
    for (var in factorVariables){
      
      if (!is.factor(data[,var])){
        data[,var] <- as.factor(data[,var])
      }
      
    }
  }
  
  # Scale variables of interest if requested
  
  if(scaling){
    data <- scaleData(data, fixedEffects, randomEffects, factorVariables)
  }
  
  ###############################
  # Write correctly the formula #
  ###############################
  
  formula <- writeFormula(interestVar = interestVar,
                          fixedEffects = fixedEffects,
                          randomEffects = randomEffects)
  
  #####################
  # Tendency modeling #
  #####################
  
  if (method == "glm") {
    model <- suppressWarnings(glmmTMB(formula, data = data, family = distribution))
    warnings <- names(warnings(model))
    
  }
  
  ##################
  # Save estimates #
  ##################
  
  return(model = model)
}
