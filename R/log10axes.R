#' Simple logarithmic axes function
#' @description Based on magaxis, with some modifications. Unlike \code{\link{magaxis}}, the labels are always of the form 10^i, where magaxis sometimes uses 10^i, other times it uses 10,100, etc., depending on the range. 
#' @export
log10axes <- function(side=1:2, logged=NULL, labels=TRUE){
  
  magaxis(side=side, unlog=side, labels=FALSE)
  
  loggedaxes <- c(FALSE,FALSE)
  if(!is.null(logged)){
    loggedaxes[logged] <- TRUE  
  }
  
  for(ii in side){
    lims <- sort(par("usr")[(1 + (ii-1)*2):(2 + (ii-1)*2)])
    sci.tick <- maglab(10^lims, n = 5, log = TRUE, exptext = TRUE, 
                       crunch = TRUE, logpretty = TRUE, usemultloc = FALSE, 
                       prettybase = 10, hersh = FALSE)
    
    if(labels){
      lab <- do.call(expression, lapply(log10(sci.tick$labat), function(i) bquote(10^.(i))))
      
      if(loggedaxes[ii]){
        axis(side = ii, at = sci.tick$labat, tick = FALSE, 
             labels = lab, mgp = par("mgp"))
      } else {
        axis(side = ii, at = log10(sci.tick$labat), tick = FALSE, 
             labels = lab, mgp = par("mgp"))
      } 
    }
  }
}

