#Assert function
assert <- function(model,lag) {
  if (lag>20 || typeof(lag) != "double") {
    stop("Lag value can not be greater than 20 and it must be integer.")
  }
  
  if (as.character(model$call[[1]])!="lm") {
    stop("model must be lm(y ~ x) object.")
  }
}