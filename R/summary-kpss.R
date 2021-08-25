
#' @title KPSS Test result.
#'
#' @description
#' This function computes KPSS statistics and critical values and aggregate them in one dataframe for all possible situations.
#'
#' @param model fitted \code{lm(y ~ x)}  object
#' @param lag Integer lag length. For example, if the value is \code{10}, results will come for lag lengths of \code{1:10}.
#' @export
#' @examples
#' summary_kpss(ts_model,10)

summary_kpss <- function(model,lag) {
  
  # if (lag>20 || typeof(lag) != "double") {
  #   stop("Lag value can not be greater than 20 and it must be integer.")
  # }
  # 
  # if (as.character(model$call[[1]])!="lm") {
  #   stop("model must be lm(y ~ x) object.")
  # }
  assert(model,lag)
  
  variables <-c(as.data.frame(model$model),as.data.frame(model$residuals)) %>% as.data.frame()
  
  kpss_type <-c("mu","tau")
  kpss = list()
  for (variable in names(variables)) {
    for (type in kpss_type) {
      for (i in 1:lag) {
        kpss_new <- ur.kpss(variables[[variable]],type = type, use.lag=i)
        kpss_result <- as.data.frame(cbind(Test="KPSS",Type=type,Variable=variable,Lag=i,"Statistic"=kpss_new@teststat[1],kpss_new@cval)) %>%
          mutate(Statistic=as.numeric(.data[["Statistic"]]),
                 "1pct"=as.numeric(.data[["1pct"]]),
                 "5pct"=as.numeric(.data[["5pct"]]),
                 "10pct"=as.numeric(.data[["10pct"]]))
        row.names(kpss_result) <- NULL
        kpss_result[["Type"]] <- ifelse(kpss_result[["Type"]]=="mu","constant","trend")
        kpss_result["10% result"] <- ifelse(kpss_result[["Statistic"]]<kpss_result[["10pct"]],"pass","fail")
        kpss_result["5% result"] <- ifelse(kpss_result[["Statistic"]]<kpss_result[["5pct"]],"pass","fail")
        kpss_result["2.5% result"] <- ifelse(kpss_result[["Statistic"]]<kpss_result[["2.5pct"]],"pass","fail")
        kpss_result["1% result"] <- ifelse(kpss_result[["Statistic"]]<kpss_result[["1pct"]],"pass","fail")
        kpss[[paste0(i,type,variable)]] <- kpss_result # add it to list
      }
    }
  }
  kpss_full <- do.call(rbind, kpss)
  row.names(kpss_full) <- NULL
  return(kpss_full)
  
}