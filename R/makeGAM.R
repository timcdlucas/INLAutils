
#' A function to create the formulae needed to fit GAMs in R.
#' 
#' Given the beginning of a formula (e.g. y ~ 0 + Intercept) this function builds the string needed to create 
#' GAM models either with or withour the linear terms as well.
#' This can then be returned as a formula (if it is to be used directly) or as a string (if other elements such
#' as random effects are yet to be added).
#' 
#' @param varnames Character vector giving the names of the variables to be used.
#' @param response String giving the name of the response variable
#' @param invariant Any string to be invluded at the beginning of the formula such as identifying the intercept
#' @param linear If TRUE, all variables are included as linear terms. If a character vector, variables in the 
#'   vector are included as linear terms. If FALSE, no linear terms are included.
#' @param returnstring If TRUE, return formula as a string (which can later be turned into a formula with 
#'   \code{\link{formula}}). If FALSE, return a formula object.
#' 
#' @export
#' @name makeGAM
#' 
#' 

makeGAM <- function(varnames, response = 'y', invariant = '0 + Intercept', linear = TRUE, returnstring = TRUE){
  
  if(!(is.character(varnames) & is.character(response) & is.character(invariant))){
    stop('varnames, response and invariant must all be characters')
  }
  
  if(length(response) > 1 | length(invariant) > 1){
    stop('response and invariant must be length 1')
  }
  
  if(!is.logical(linear) & !is.character(linear)){
    stop('linear must either be a logical or a character vector')
  }
  
  base <- paste(response, ' ~ ', invariant)
  if(is.character(linear)){
    base <- paste(base, '+', paste(linear, collapse = ' + '))
  } else if(linear){
    base <- paste(base, '+', paste(varnames, collapse = ' + '))
  }
  
  gam <- paste0("f(inla.group(", varnames, "), model = 'rw2')", collapse = ' + ')
  formula <- paste(base, ' + ', gam)
  
  if(!returnstring){
    formula <- formula(formula)
  }
  
  return(formula)
  
}
