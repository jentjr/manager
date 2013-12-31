#' Compute multiplier for intrawell prediction limit using either regular or 
#' pooled standard deviation estimate and 1-of-m retesting for either 
#' observations or means of order p.
#' 
#' Solve for kappa given an Site-Wide False Positive Rate (SWFPR) adjusted for 
#' number of constituents and wells, then rate by effective power.
#' Rate power at 3 and 4 SD units above BG; use ERPC power values as the reference power.

#' @param ne number of yearly evaluations
#' Note: ne=4 (quarterly eval), ne=2 (semi-annual), ne=1 (annual)
#' @param n intrawell background sample size
#' @param w  number of wells 
#' @param coc number of constituents
#' @param df degrees of freedom associated with variance estimate of prediction limit formula
#' 
#' Note: if the usual standard deviation for a single well is used, set df = (n-1);
#' if using a pooled SD estimate across w equal sized wells, set df= w*(n-1) or
#' df = (sum of well n's) - w, if w pooled wells are of different sizes
#' 
#' @param alph  per-test false positive rate
#' @param m type of 1-of-m retesting scheme (usually m= 1, 2, 3, or 4)
#' @param ord order of the mean to be predicted (for tests on observations, set ord = 1)
#' @param swfpr the targeted network-wide false positive rate, by default set to 10%
#'
#'
#' @export


calc_kappa <- function(n, w, coc, ne, m, ord = 1, swfpr = 0.1){
  
  # user supplied values of n, w, coc, df, evaluation frequency, m, and ord
  n <- n
  w <- w
  coc <- coc
  df <- w * (n - 1)
  ne <- ne
  m <- 2
  ord <- ord
  swfpr <- swfpr
  alph <- 1 - (1 - swfpr) ^ (1 / (coc * w))
  
  ref= c()
  
  if (ne == 1) ref = c(0.54, 0.81)
  if (ne == 2) ref = c(0.59, 0.85)
  if (ne == 4) ref = c(0.60, 0.86)
  
  # default tolerance values for convergence
  tol= 0.000001
  tol2= 0.0001
  
  # default lower and upper limits on range for desired multiplier
  ll= 0
  ul= 15
  
  # recursive function to compute correct multiplier within limits (lo, hi)
  kfind <- function(lo, hi, n, alph, ne, tol) {
    
    if (abs(hi - lo) < tol2) return(lo)
    
    nc <- function(x) sqrt(n) * qnorm(x) / sqrt(ord)
    
    tt <- sqrt(n) * lo
    
    g <- function(x) ne * m * (1 - (1 - x) ^ m) ^ (ne - 1) * (1 - x) ^ (m - 1) * pt(tt, df, nc(x))
    
    klo <- 1 - alph - integrate(g, 0, 1)$value
    
    if (abs(klo) <= tol) return(lo)
    
    tt <- sqrt(n) * hi
    
    khi <- 1 - alph - integrate(g, 0, 1)$value
    
    if (abs(khi) <= tol) return(hi)
    
    tt <- sqrt(n) * (mean(c(lo, hi)))
    
    kmid <- 1 - alph - integrate(g, 0, 1)$value
    
    if (abs(kmid) <= tol) {
      return(mean(c(lo, hi)))
    }
    
    if (sign(klo) != sign(khi)) {
      if (sign(klo) != sign(kmid)) {
        kfind(lo, mean(c(lo, hi)), n, alph, ne, tol) 
      }
      else {
        kfind(mean(c(lo, hi)), hi, n, alph, ne, tol) } 
    }    
    else {
      stop('bad limits') 
    }
  }
  
  del <- c(3, 4)
  pow <- c()
  powrate <- c()
  kap <- kfind(ll, ul, n, alph, ne, tol)
  
  for (jj in 1:length(del)) {
    
    dc <- del[jj]
    tt <- sqrt(n) * kap
    
    nc <- function(x) {
      sqrt(n) * (qnorm(x) / sqrt(ord) + del[jj])
    }
    
    h <- function(x) {
      if (ne == 1) {
        m * ((1 - x) ^ (m - 1)) * pt(tt, df, nc(x))
      }
      else {
        ne * m * ((1 - (1 - x) ^ m) ^ (ne - 1)) * ((1 - x) ^ (m - 1)) * pt(tt, df, nc(x))
      }
    }
    pow[jj] <- 1 - integrate(h, 0, 1, stop.on.error = F)$value
  }
  
  if ((pow[1] >= ref[1]) && (pow[2] >= ref[2])) powrate= 'GOOD'
  if ((pow[1] < ref[1]) && (pow[2] >= ref[2])) powrate= 'ACCEPTABLE'
  if ((pow[1] >= ref[1]) && (pow[2] < ref[2])) powrate= 'ACCEPTABLE'
  if ((pow[1] < ref[1]) && (pow[2] < ref[2])) powrate= 'LOW'
  
  
  list("background sample size" = n,
       "number of wells" = w,
       "number of constituents" = coc,
       "sampling frequncy" = ne,
       "type of 1-of-m retesting scheme" = m,
       "order of the mean to be predicted" = ord,
       "kappa" = kap,
       "ref power from ERPC at 3 and 4 SDs" = ref,
       "calculated power at 3 and 4 SDs" = pow,
       "power rating" = powrate)
  
}
