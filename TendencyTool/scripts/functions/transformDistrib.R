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
transformDistrib <- function(value, distribution = "gaussian"){
  
  if(!is.numeric(value)){
    stop("param 'value' should be numeric")
  }
  
  if(!(distribution %in% c("gaussian", "binomial", "poisson"))){
    stop("param 'distribution' should be either 'gaussian', 'binomial' or 'poisson'")
  }
  
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
    transfValue <- exp(value) / (1 + exp(value))
    
    
  }
  
  return(transfValue)
}

########
# TEST #
########

# test_that("transformDistrib makes right calculation", {
#   expect_equal(transformDistrib(value = 10, distribution = "gaussian"), 10)
#   expect_equal(transformDistrib(value = 10, distribution = "poisson"), exp(10))
#   expect_equal(transformDistrib(value = 10, distribution = "binomial"), exp(10) / (1 + exp(10)) )
#   
# })
# 
# test_that("transformDistrib stops when parameters are wrong", {
#   expect_error(transformDistrib(value = "10", distribution = "gaussian"))
#   expect_error(transformDistrib(value = 10, distribution = "tiptop"))
# })
