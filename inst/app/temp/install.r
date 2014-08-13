if (!require('devtools') || packageVersion('devtools') < 1.4) install.packages('devtools')

devtools::install_github(c('rstudio/shiny','rstudio/httpuv'))

install.packages(c('RODBC','ggplot2','scales'), dependencies=TRUE)
