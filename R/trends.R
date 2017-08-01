#' Function to get slope and intercept from Theil-Sen slope for plotting
#' 
#' @param df dataframe of groundwater data in tidy format
#' @param x column for analysis result
#' @param y column for sample date
#' @param ... other arguements passed to \code{\link{kendallTrendTest}}
#' @export 

get_theilsen <- function(df, x = "analysis_result", y = "sample_date", ...) {
  
  x <- df[, x]
  y <- df[, y]
  
  kendall <- EnvStats::kendallTrendTest(x, y, ...)
  est <- kendall$estimate
  pv <- kendall$p.value
  
  return(c(est, pv))
}

#' Kendall Trend
#' 
#' @param df dataframe of groundwater data
#' @param conf.level confidence level between 0 and 1
#' @export

kendall_trend <- function(df, conf.level = 0.99) {
  
  EnvStats::kendallTrendTest(analysis_result ~ sample_date, 
                             conf.level = conf.level, 
                             data = df)
}

#' Function to calculate the Theil-Sen confidence band
#' 

theil_sen_band <- function(analysis_result, sample_date, conf.level) {
  
  x= sample_date
  y= analysis_result
  conf = conf.level
  
  elimna <- function(m){
    
    # remove any rows of data having missing values
    m= as.matrix(m)
    ikeep= c(1:nrow(m))
    
    for(i in 1:nrow(m)) if (sum(is.na(m[i,])>=1)) ikeep[i]= 0
    elimna= m[ikeep[ikeep>=1],]
    elimna
  }
  
  theilsen2= function(x,y){
    #
    # Compute the Theil-Sen regression estimator
    # Do not compute residuals in this version
    # Assumes missing pairs already removed
    #
    ord= order(x)
    xs= x[ord]
    ys= y[ord]
    vec1= outer(ys,ys,"-")
    vec2= outer(xs,xs,"-")
    v1= vec1[vec2>0]
    v2= vec2[vec2>0]
    slope= median(v1/v2)
    coef= 0
    coef[1]= median(y)-slope*median(x)
    coef[2]= slope
    list(coef=coef)
  }
  nb= 1000
  temp= matrix(c(x,y),ncol=2)
  temp= elimna(temp) #remove any pairs with missing values
  x= temp[,1]
  y= temp[,2]
  n= length(x)
  ord= order(x)
  cut= min(x) + (0:100)*(max(x)-min(x))/100 #compute 101 cut pts
  t0= theilsen2(x,y) #compute trend line on original data
  tmp= matrix(nrow=nb,ncol=101)
  for (i in 1:nb) {
    idx= sample(ord,n,rep=T)
    xboot= x[idx]
    yboot= y[idx]
    tboot= theilsen2(xboot,yboot)
    tmp[i,]= tboot$coef[1] + cut*tboot$coef[2]
  }
  lb= 0; ub= 0
  for (i in 1:101){
    lb[i]= quantile(tmp[,i],c((1-conf)/2))
    ub[i]= quantile(tmp[,i],c((1+conf)/2))
  }
  tband= list(xcut=cut,lo=lb,hi=ub,ths0=t0)
  yt= tband$ths0$coef[1] + tband$ths0$coef[2]*tband$xcut
  plot(yt~tband$xcut,type='l',xlim=range(x),ylim=c(min(tband$lo),max(tband$hi)),xlab='Date',ylab='Conc')
  points(x,y,pch=16)
  lines(tband$hi~tband$xcut,type='l',lty=2)
  lines(tband$lo~tband$xcut,type='l',lty=2)
}
