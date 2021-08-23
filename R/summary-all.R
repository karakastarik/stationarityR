#' @title Aggregate KPSS, ADF and PP Test Results
#'
#' @description
#' This function computes unit root test results and aggregate them in one dataframe for all possible situations.
#'
#' @param model fitted \code{lm(y ~ x)}  object
#' @param lag Integer lag length. For example, if the value is \code{10}, results will come for lag lengths of \code{1:10}.
#' @export
#' @examples
#' summary_all(ts_model,10)

summary_all <- function(model,lag) {

  if (lag>20) {
    stop("Lag can not be greater than 20.")
  }
  kpss_type <-c("mu","tau")
  variables <-c(as.data.frame(model$model),as.data.frame(model$residuals)) %>% as.data.frame()

  #loop for kpss
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
  #loop for adf
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

  #loop for pp
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

  all_tests <- rbind(kpss_full,adf_full)
  all_tests <- rbind(all_tests,pp_full)
  return(all_tests)
}
