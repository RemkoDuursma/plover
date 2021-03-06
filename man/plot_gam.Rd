% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot_gam.R
\name{plot_gam}
\alias{plot_gam}
\title{Plot a generalized additive model}
\usage{
plot_gam(x, y, g = NULL, data, fittype = c("gam", "lm"), kgam = 4,
  R = NULL, randommethod = c("gamm", "aggregate"), log = "",
  axes = TRUE, fitoneline = FALSE, pointcols = NULL, linecols = NULL,
  xlab = NULL, ylab = NULL, band = TRUE, bandcolor = alpha("lightgrey",
  0.7), plotit = TRUE, add = FALSE, npred = 101, lwd = 2, ...)
}
\arguments{
\item{x}{Variable for X axis (unquoted)}

\item{y}{Variable for Y axis (unquoted)}

\item{g}{Variable for grouping (unquoted); optional}

\item{data}{Dataframe containing x and y}

\item{kgam}{the \code{k} parameter for smooth terms in gam.}

\item{R}{An optional random effect (quoted)}

\item{randommethod}{If 'gamm', passes the random effect variable to \code{\link{gamm}}, otherwise simply averages the data by the random effect variable.}

\item{log}{Whether to add log axes for x or y (but no transformations are done).}

\item{fitoneline}{Whether to fit only one curve to the entire dataset, regardless of whether a grouping variable was defined}

\item{pointcols}{Colours of the points, can be a vector}

\item{linecols}{Colours of the linces, can be a vector}

\item{xlab}{X-axis label}

\item{ylab}{Y-axis label}

\item{band}{Logical. If true, plots the confidence band (as a transparent polygon).}
}
\description{
Plot a generalized additive model
}
\examples{
data(Loblolly)
plot_gam(age, height, data=Loblolly)
plot_gam(age, height, Seed, data=Loblolly, band=FALSE, linecols="black")
plot_gam(age, height, Seed, data=Loblolly, band=FALSE, linecols="black", fittype="lm")

data(ChickWeight)
library(wesanderson)
plot_gam(Time, weight, Diet, R="Chick", data=ChickWeight, linecols=wes_palette("Rushmore"))
}

