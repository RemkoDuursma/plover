% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/add_errorbars.R
\name{add_errorbars}
\alias{add_errorbars}
\title{Adds error bars to a plot}
\usage{
add_errorbars(x, y, SE, direction, barlen = 0.04, ...)
}
\arguments{
\item{x}{The x coordinates of start of the error bar}

\item{y}{The y coordinates of start of the error bar}

\item{SE}{The length of the error bar}

\item{direction}{One of 'up', 'down', 'right', 'left', 'updown' or 'rightleft'.}

\item{barlen}{The length of the cross-bar at the end of the error bar.}

\item{\ldots}{Additional parameters passed to \code{\link{arrows}}, such as the colour (\code{col}).
#' @details Simple wrapper for \code{\link{arrows}}, where \code{angle=90} and \code{code=3}. The \code{barlen} argument corresponds to \code{length} in \code{arrows}.}
}
\description{
Yet another function that adds error bars. The user must specify the length of the error bars.
}
\examples{
# A simple example. Also note that we can specify the colour of the error bars, or other parameters
# that arrows() recognizes.
x <- rnorm(20)
y <- x + rnorm(20)
se <- runif(20, 0.2,0.4)
plot(x,y,pch=21,bg="white",panel.first=add_errorbars(x,y,se,direction="updown", col="darkgrey"))
}

