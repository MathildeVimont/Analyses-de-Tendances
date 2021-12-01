#' transformDistrib
#' 
#' A function that transforms a value depending on the chosen distribution
#' 
#' @param x a `numeric` value to transform
#' @param distribution a `string` that is the distribution used in the model ("gaussian", "poisson", "binomial")
#' 
#' @return 
#' The value transformed according to distribution 
#' 
#' @example
transformDistrib <- function(value, distribution){
  
  # No transformation if gaussian
  if(distribution == "gaussian"){
    transfValue <- value
  }
  
  ## Log-transformation if poisson
  else if(distribution == "poisson"){
    transfValue <- exp(value)
  }
  
  ## Logit-transformation if binomial
  else if(distribution == "binomial"){
    transfValue <- exp(value) / (1 + exp(-1*value))
    
    
  }
  
  return(transfValue)
}

########
# TEST #
########

# transformDistrib(value = 10, distribution = "gaussian") # 10
# transformDistrib(value = 10, distribution = "poisson")  # exp(10)
# transformDistrib(value = 10, distribution = "binomial") # exp(10) / (1 + exp(10)) 