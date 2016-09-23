#' Plot and overlay histograms by group
#'@description A simple function to plot histograms by group, and overlay them. This is notoriously difficult to achieve in base R.
#'
#'@export
#'@examples
#'library(wesanderson)
#'hist_bygroup(Petal.Length, Species, iris, col=wes_palette("Chevalier"), breaks=50)
#'
#'hist_bygroup(Petal.Length, Species, iris, col=wes_palette("Chevalier"), breaks=50,
#' group.expression=abline(v=mean(Y), col=col.cur, lwd=2))
#' 
#' hist_bygroup(Petal.Length, Species, iris, col=wes_palette("Chevalier"), what="density",
#' breaks=40,
#' group.expression=curve(dnorm(x, mean(Y), sd=sd(Y)), add=T))
#'
hist_bygroup <- function(yvar, group, dataset,
                      breaks=25,
                      what=c("counts","density"),
                      log=FALSE,
                      col=palette(),
                      xlab=NULL,
                      ylab="Nr. individuals",
                      ylim=NULL,
                      xlim=NULL,
                      legend.text=NULL,
                      legend.cex=1,
                      xaxis=NULL,
                      overlay=TRUE,
                      group.expression=NULL,
                      ...){
  
  yall <- eval(substitute(yvar), dataset)
  what <- match.arg(what)
  dataset$Group <- eval(substitute(group), dataset)
  
  # find breaks across all data
  h <- hist(yall, breaks=breaks, plot=FALSE)
  breaks <- h$breaks

  # Width of bins
  w <- breaks[2] - breaks[1]
  
  d <- split(dataset, dataset$Group)
  
  if(is.null(legend.text))legend.text <- names(d)
  if(is.null(xaxis))xaxis <- 1:length(d)
  if(is.null(xlab))xlab <- substitute(yvar)
  
  for(i in 1:length(d)){
    
    x <- d[[i]]
    Y <- eval(substitute(yvar),x)
    Y <- Y[!is.na(Y)]
    col.cur <- col[i]
    h <- hist(Y, breaks=breaks, plot=FALSE)
    breaks <- h$breaks
    w <- breaks[2] - breaks[1] 
    
    if(is.null(ylim))ylim <- c(0,max(h[[what]]))
    
    if(!overlay || (overlay & i == 1)){
      plot(breaks, breaks, ylim=ylim, xlim=xlim, 
           xlab=xlab, ylab=ylab,
           axes=FALSE, type='n',...)
    }
    for(j in 1:length(h[[what]])){
      n <- h[[what]][j]
      m <- h$mids[j]
      if(n == 0)next
      rect(xleft=m-w/2, xright=m+w/2, ybottom=0, ytop=n,  border=NA,col=col.cur)
    }
    
    if(i %in% xaxis){
      if(log)
        magaxis(side=1, unlog=1, tcl=-0.4)
      else
        axis(1)
    }
    axis(2)
    
    #u <- par()$usr
    #text(x=u[1], y=0.96*u[4], legend.text[i], cex=legend.cex,font=2,pos=4)
    
    eval(substitute(group.expression))
    
  }
  
  #mtext(side=2, line=3, text=ylab, outer=TRUE, las=3)
  #mtext(side=1, line=3, text=xlab, outer=TRUE, las=1)
}

