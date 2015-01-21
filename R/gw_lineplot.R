#' Comparison lineplot
#' 

gw_lineplot <- function(df, title = NULL, line1 = NULL, line2 = NULL) {
  
  df$non_detect <- ifelse(
    df$lt_measure == "<", "non-detect", "detected"
  )
  
  df$param_name <- paste(df$param_name, " (", 
                         df$default_unit, ")", sep = "")
  
  p <- ggplot(df, 
              aes(x = sample_date, y = analysis_result, color = location_id)
              ) + 
    geom_point(data = df, aes(shape = factor(non_detect)), size = 3) + 
    scale_shape_manual(values = c("non-detect" = 1, "detected" = 16)) +
    facet_wrap(~param_name, scale = "free") + 
    ggtitle(paste(title)) +
    ylab("Analysis Result\n") +
    xlab("\nSample Date") +
    scale_x_datetime(labels = scales::date_format("%Y")) +
    theme(plot.margin = grid::unit(c(1, 1, 1, 1), "lines")) + 
    theme_bw() +  
    theme(axis.title.x = element_text(size = 15, vjust = -0.3)) +
    theme(axis.title.y = element_text(size = 15, vjust = 0.3)) 
  
  if(!missing(line1)){
    line1 <- as.quoted(line1)
    df$line1_name <- paste(line1[[1]])
    p <- p + geom_hline(data = df, aes_string(yintercept = line1, 
                                              linetype = "line1_name"),
                        size = 1,
                        show_guide = TRUE) +
      guides(linetype = guide_legend("Limit"),
             shape = guide_legend("Legend", 
                                  override.aes = list(linetype = 0)),
             color = guide_legend("Location", 
                                  override.aes = list(linetype = 0)))
  }
  
  if(!missing(line2)){
    line2 <- as.quoted(line2)
    df$line2_name <- paste(line2[[1]])
    p <- p + geom_hline(data = df, aes_string(yintercept = line2, 
                                   linetype = "line2_name"),
                        size = 1,
                        show_guide = TRUE) +
      guides(linetype = guide_legend("Limit"),
             shape = guide_legend("Legend", 
                                  override.aes = list(linetype = 0)),
             color = guide_legend("Location", 
                                  override.aes = list(linetype = 0)))
  }
  
  if(!missing(line1) & !missing(line2)){
    p <- p + guides(linetype = guide_legend("Limits"),
           shape = guide_legend("Legend", override.aes = list(linetype = 0)),
           color = guide_legend("Location", override.aes = list(linetype = 0)))
    scale_linetype_manual(values=c("solid", "dotdash")) 
  }
  return(p)
}