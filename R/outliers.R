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
#' @param df data.frame of groundwater data 
#' @param x column of analysis results
#' @param replace value to replace outliers with
#' @param ... other arguments passed to rosner test
#' @export

rosner_flag <- function(df, x = "analysis_result", replace = NULL, ...) {
  
  outliers <- NULL
  test <- df[, x]
  
  rosner.result <- EnvStats::rosnerTest(test, ...)
  
  outliers <- rosner.result$all.stats[c("Value", "Outlier")] %>%
    filter(Outlier == TRUE) %>%
    select(Value)
 
  df$outlier <- ifelse(df$analysis_result %in% outliers$Value, TRUE, FALSE)
  
  if (!missing(replace)) {
    df$replaced_values <- ifelse(df$outlier == TRUE, replace, 
                                 df$analysis_result)
  }
   
  return(df)
}