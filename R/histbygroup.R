#' Plot and overlay histograms by group
#'@description A simple function to plot histograms by group, and overlay them. This is notoriously difficult to achieve in base R.
#'
#'@export
hist_bygroup <- function(yvar, group, dataset,
                      nbin=100,
                      plotwhat=c("counts","density"),
                      log=TRUE,
                      col=1:5,
                      meanlinecol="black",
                      cicol=alpha("grey",0.6),
                      xlab=NULL,
                      ylab="Nr. individuals",
                      ylim=NULL,
                      xlim=NULL,
                      legend.text=NULL,
                      legend.cex=1,
                      xaxis=NULL,
                      overlay=FALSE){
  
  yall <- eval(substitute(yvar), dataset)
  plotwhat <- match.arg(plotwhat)
  dataset$Group <- eval(substitute(group), dataset)
  
  mn <- min(yall, na.rm = TRUE)
  mx <- max(yall, na.rm = TRUE)
  br <- seq(mn - 0.01*(mx-mn),mx + 0.01*(mx-mn),length=nbin)
  w <- br[2]-br[1]
  
  d <- split(dataset, dataset$Group)
  if(is.null(legend.text))legend.text <- names(d)
  
  if(is.null(xaxis))xaxis <- 1:length(d)
  
  
  for(i in 1:length(d)){
    
    x <- d[[i]]
    Y <- eval(substitute(yvar),x)
    Y <- Y[!is.na(Y)]
    
    h <- hist(Y, breaks=br, plot=FALSE)
    if(is.null(ylim))
      ylim <- c(0,max(h[[plotwhat]]))
    else
      Ylim <- ylim
    
    if(!overlay || (overlay & i == 1)){
      plot(br, br, ylim=ylim, xlim=xlim, axes=FALSE, type='n')
    }
    for(j in 1:length(h[[plotwhat]])){
      n <- h[[plotwhat]][j]
      m <- h$mids[j]
      if(n == 0)next
      rect(xleft=m-w/2, xright=m+w/2, ybottom=0, ytop=n,  border=NA,col=col[i])
    }
    
    if(i %in% xaxis){
      if(log)
        magaxis(side=1, unlog=1, tcl=-0.4)
      else
        axis(1)
    }
    axis(2)
    
    
    u <- par()$usr
    text(x=u[1], y=0.96*u[4], legend.text[i], cex=legend.cex,font=2,pos=4)
    
  }
  
  mtext(side=2, line=3, text=ylab, outer=TRUE, las=3)
  mtext(side=1, line=3, text=xlab, outer=TRUE, las=1)
}

