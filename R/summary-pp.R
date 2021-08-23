#' @title PP Test Results.
#'
#' @description
#' This function computes PP statistics and critical values and aggregate them in one dataframe for all possible situations.
#'
#' @param model fitted \code{lm(y ~ x)}  object
#' @param lag Integer lag length. For example, if the value is \code{10}, results will come for lag lengths of \code{1:10}.
#' @export
#' @examples
#' summary.pp(ts_model,10)

summary.pp <- function(model,lag) {
  
  if (lag>20) {
    stop("Lag can not be greater than 20.")
  }
  
  variables <-c(as.data.frame(model$model),as.data.frame(model$residuals)) %>% as.data.frame()
  
  pp_type <- c("constant","trend")
  pp = list()
  for (variable in names(variables)) {
    for (type in pp_type) {
      for (i in 1:10) {
        pp_new <- ur.pp(variables[[variable]],type = c("Z-tau"), model=type, use.lag=i)
        pp_result <- as.data.frame(cbind(Test="PP",Type=type,Variable=variable,Lag=i,"Statistic" = pp_new@teststat,pp_new@cval)) %>%
          mutate(Statistic=as.numeric(.data[["Statistic"]]),
                 "1pct"=as.numeric(.data[["1pct"]]),
                 "5pct"=as.numeric(.data[["5pct"]]),
                 "10pct"=as.numeric(.data[["10pct"]]))
        row.names(pp_result) <- NULL
        pp_result <- pp_result %>% mutate("2.5pct"="Not avaiable for PP") %>% relocate("2.5pct", .before = "10pct")
        pp_result["10% result"] <- ifelse(abs(pp_result[["Statistic"]])>abs(pp_result[["10pct"]]),"pass","fail")
        pp_result["5% result"] <- ifelse(abs(pp_result[["Statistic"]])>abs(pp_result[["5pct"]]),"pass","fail")
        pp_result["2.5% result"] <- "Not available for PP"
        pp_result["1% result"] <- ifelse(abs(pp_result[["Statistic"]])>abs(pp_result[["1pct"]]),"pass","fail")
        pp[[paste0(i,type,variable)]] <- pp_result # add it to list
      }
    }
  }
  pp_full <- do.call(rbind, pp)
  row.names(pp_full) <- NULL
  
  return(pp_full)
  
}