#' measureVIF
#' 
#' A function that calculates Variance Inflation Indicators for each variable
#' @param model an object containing the output of the regression
#' 
#' @return 
#' @example 
measureVIF <- function (model) {
  
  # Extract variance-Covariance matrix from model output
  v <- vcov(model)$cond

  # Exclude intercepts
  v <- v[-1,-1]
  
  # Extract names of the matrix
  cols <- colnames(v) 
  
  # Square root of the diagonal of the variance-covariance matrix
  d <- sqrt(diag(v))
  
  # Stop the measure of VIF if NAs are created
  if(any(is.na(d))) {
    stop(paste0("The diagonal matrice of the variance-covariance matrix produced",
                "at least one negative value\n",
                "The VIFs cannot be assessed"))
  }
  
  # Variance-covariance matrix on outer product of d
  prodD <- v/(d %o% d)
  
  # Inverse d
  invD <- solve(prodD)
  
  # Return the diagonal of d
  VIF <- diag(invD)
  
  return(VIF)
  
}


#' #' measureVIF
#' #' 
#' #' A function that calculates Variance Inflation Indicators for each variable
#' #' @param data
#' #' @param fixedEffects
#' #' @param randomEffects
#' #' 
#' #' @return 
#' #' @example 
#' measureVIF <- function(data, 
#'                        fixedEffects = NULL, 
#'                        randomEffects = NULL){
#'   
#'   # Extract the variables of interests
#'   dataVIF <- data[,c(fixedEffects, randomEffects)]
#'   
#'   # Extract the correlation matrix from all variables of interest
#'   corMat <- cor(dataVIF)
#'   
#'   # If possible, reverse the correlation matrix
#'   # !reversing not always!
#'   invCorMat <- try(solve(corMat), silent = T)
#'   
#'   if(class(invCorMat)[1] == "try-error"){
#'     cat("ERROR : the correlation matrice is not reversible")
#'     cat(" --> VIFs cannot be measured")
#'     
#'   }else{
#'     
#'     # Extract VIF values 
#'     vifValues = diag(invCorMat)
#'     
#'     # Round VIF values
#'     vifValues = round(vifValues, 2)
#'     
#'     return(vifValues)
#'   }
#'   
#'   
#' }

