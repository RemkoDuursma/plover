#' Adds an axis title in the margin
#' @description A simple wrapper for \code{\link{mtext}}, making sure that the character size is adjusted to be identical to the character size used on the ordinary axes. This is especially handy when plotting multiple panels (via the use of \code{par(mfrow=...)}), because axis labels are automatically adjusted in that case (see entry for \code{mfrow} in \code{\link{par}}).
#' @examples
#' # Plot a second axis
#' x <- rnorm(100)
#' y <- x + rnorm(100)
#' z <- rnorm(100) - x
#' 
#' o <- par(mar=c(5,5,2,5)) 
#' plot(x, y, ylab="X variable")
#' points(x, z, pch=19)
#' axis_title("Z variable", side = 4)
#' axis(4)
#' par(o)
#' @export
axis_title <- function(text, side, ...){
  
  # Get actual cex.lab, see ?par under mfrow
  mf <- par()$mfrow
  m <- 1
  if(mf[1] ==2 & mf[2] == 2)m <- 0.83
  if(max(mf) > 2)m <- 0.66
  mtext(side=side, text=text, cex=m*par()$cex.lab, line=par()$mgp[1], ...)
  
}

