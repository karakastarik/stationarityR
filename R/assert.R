#Assert lag
assert_lag <- function(lag) {
  if (lag>20 || typeof(lag) != "double") {
    stop("lag value can not be greater than 20 and it must be integer.")
  }
}

#Assert model
assert_model <- function(model) {
  if (as.character(model$call[[1]])!="lm") {
    stop("model must be lm(y ~ x) object.")
  }
}

#Assert difference
assert_difference <- function(difference) {
  if (typeof(difference) != "double" || difference < 1) {
    stop("difference value must be integer and greater than 1.")
  }
}


