#' Function to flag outliers from Grubb's test for outliers
#' @param x column of analysis results
#' @param ... other arguments passed to grubbs test
#' @export

grubbs_flag <- function(x, ...) {
  outliers <- NULL
  test <- x
  grubbs.result <- grubbs.test(test, ...)
  pv <- grubbs.result$p.value
  
  while (pv < 0.05) {
    outliers <- c(outliers, as.numeric(strsplit(
      grubbs.result$alternative," ")[[1]][3]))
    test <- x[!x %in% outliers]
    grubbs.result <- grubbs.test(test)
    pv <- grubbs.result$p.value
  }
  return(data.frame(X = x, Outlier = (x %in% outliers)))
}

#' Function to flag outliers from Rosner's test for outliers
#' @param x column of analysis results
#' @param ... other arguments passed to rosner test
#' @export

rosner_flag <- function(x, ...) {
  
  outliers <- NULL
  test <- x
  
  rosner.result <- EnvStats::rosnerTest(test, ...)
  
  outliers <- cbind(rosner.result$all.stats["Outlier"],
                    rosner.result$all.stats["Value"])
  
  outliers %>% filter(Outlier == TRUE) %>% select(Value)
  
  return(data.frame(X = x, Outlier = (x %in% outliers)))
}