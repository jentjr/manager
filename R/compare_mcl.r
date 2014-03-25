#' Function to compare groundwater results to EPA primary and seconday standards
#' 

compare_MCL <- function(df) {
  ddply(df, .(param_name), )
}
  

parse_mcl <- function(x = "param_name") {
  x <- as.quoted(x)
  df[,x] <- grepl("Arsenic", df[,x])
  df[,x] <- grepl("Antimony", df[,x])
  df[,x] <- grepl("Barium", df[,x])
  df[,x] <- grepl("Beryllium", df[,x])
  df[,x] <- grepl("Cadmium", df[,x])
  df[,x] <- grepl("Chromium", df[,x])
  df[,x] <- grepl("Mercury", df[,x])
  df[,x] <- grepl("Selenium", df[,x])
  df[,x] <- grepl("Thallium", df[,x])
  df[,x] <- grepl("Uranium", df[,x])
  df[,x] <- grepl("Nitrite", df[,x])
  df[,x] <- grepl("Nitrate", df[,x])
  df[,x] <- grepl("Zinc", df[,x])
  df[,x] <- grepl("Aluminum", df[,x])
  df[,x] <- grepl("Iron", df[,x])
  df[,x] <- grepl("Manganese", df[,x])
  df[,x] <- grepl("Chloride", df[,x])
  df[,x] <- grepl("Fluoride", df[,x])
  df[,x] <- grepl("Total Dissolved Solids", df[,x])
  df[,x] <- grepl("TDS", df[,x])
  df[,x] <- grepl("Sulfate", df[,x])
  df[,x] <- grepl("pH", df[,x])
  df[,x] <- grepl("Copper", df[,x])
  df[,x] <- grepl("Lead", df[,x])
  return(df)
}


