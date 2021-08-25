#' @title ADF Test Results.
#'
#' @description
#' This function computes ADF statistics and critical values and aggregate them in one dataframe for all possible situations.
#'
#' @param model fitted \code{lm(y ~ x)}  object
#' @param lag Integer lag length. For example, if the value is \code{10}, results will come for lag lengths of \code{1:10}.
#' @export
#' @examples
#' summary_adf(ts_model,10)

summary_adf <- function(model,lag=5,difference=NA) {
  
  assert_lag(lag)
  assert_model(model)
  
  if (!is.na(difference)) {
    variables <- df_diff(model,difference)
  } else {
    variables <-c(as.data.frame(model$model),as.data.frame(model$residuals)) %>% as.data.frame()
  }
  
  adf_type <- c("none","drift","trend")
  adf = list()
  for (variable in names(variables)) {
    for (type in adf_type) {
      for (i in 1:10) {
        adf_new <- ur.df(variables[[variable]],type = type, lags = i)
        adf_result <- as.data.frame(cbind(Test="ADF",Type=type,Variable=variable,Lag=i,"Statistic"=adf_new@teststat[1],as.data.frame(adf_new@cval)[1,])) %>%
          mutate(Statistic=as.numeric(.data[["Statistic"]]),
                 "1pct"=as.numeric(.data[["1pct"]]),
                 "5pct"=as.numeric(.data[["5pct"]]),
                 "10pct"=as.numeric(.data[["10pct"]]))
        row.names(adf_result) <- NULL
        adf_result <- adf_result %>% mutate("2.5pct"="Not avaiable for ADF") %>% relocate("2.5pct", .before = "10pct")
        adf_result["10% result"] <- ifelse(abs(adf_result[["Statistic"]])>abs(adf_result[["10pct"]]),"pass","fail")
        adf_result["5% result"] <- ifelse(abs(adf_result[["Statistic"]])>abs(adf_result[["5pct"]]),"pass","fail")
        adf_result["2.5% result"] <- "Not available for ADF"
        adf_result["1% result"] <- ifelse(abs(adf_result[["Statistic"]])>abs(adf_result[["1pct"]]),"pass","fail")
        adf[[paste0(i,type,variable)]] <- adf_result # add it to list
      }
    }
  }
  adf_full <- do.call(rbind, adf)
  row.names(adf_full) <- NULL
  return(adf_full)
  
}