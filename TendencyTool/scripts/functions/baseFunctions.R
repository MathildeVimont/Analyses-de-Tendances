#' invLogit
#' 
#' A function that
#' @param x a value
#' 
#' @return 
#' @example 
invLogit <- function(x){
  y <- exp(x) / (1 + exp(x))
}
