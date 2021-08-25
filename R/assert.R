#' @title Assert the params.
#'
#' @description
#' Assert.
#'
#' @param model fitted \code{lm(y ~ x)}  object
#' @param lag Integer lag length. For example, if the value is \code{10}, results will come for lag lengths of \code{1:10}.
#' @export
#' @examples
#' assert(ts_model,10)
assert <- function(model,lag) {
  if (lag>20 || typeof(lag) != "double") {
    stop("Lag value can not be greater than 20 and it must be integer.")
  }
  
  if (as.character(model$call[[1]])!="lm") {
    stop("model must be lm(y ~ x) object.")
  }
}