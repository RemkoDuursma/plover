


library(lattice)
library(mosaic)
latticeParseFormula(log(Sepal.Length) ~ Sepal.Width | Species, iris)


latticeParseFormula(log(Sepal.Length) + Petal.Length ~ Sepal.Width | Species, iris)


iris$Factor2 <- sample(letters[1:2], nrow(iris), replace=T)

f <- formula(log(Sepal.Length) + Petal.Length + Sepal.Width ~ Sepal.Width | Species)
f2 <- formula(log(Sepal.Length) ~ Sepal.Width | Species)
f3 <- formula(log(Sepal.Length) + Petal.Length + Sepal.Width ~ Sepal.Width | Species + Factor2)


has_plus <- function(f){
  any(grepl("+", as.character(f), fixed=TRUE))
}


split_formula <- function(f){
  
  fc <- as.character(f)
  
  # Check if more than one variable on left-hand side
  p <- parse.formula(f)
  
  if(has_plus(p$lhs)){
   lhs <- fc[2]
   lhs_vars <- strsplit(lhs, "+", fixed=TRUE)[[1]]
   l <- lapply(lhs_vars, function(x)as.formula(paste(x, "~", fc[3])))
  } else {
    l <- list(f)
  }
  
l
}


parse_formula <- function(f, data){
  
  # list of formulas, possibly split by lhs variables (but always a list)
  l <- split_formula(f)
  
  # eval
  e <- lapply(l, latticeParseFormula, data=data)
  
e
}


plotBy <- function(formula, data, how=c("colour","panel"), fit=c("none","gam","lm"), ...){
  
  how <- match.arg(how)
  fit <- match.arg(fit)
  
  # List with every component a dataframe for the left-hand variables in the formula
  e <- parse_formula(formula, data)
  
  for(i in 1:length(e)){
   plot_group(e[[i]],how,fit, ...) 
  }
  
}


plot_group <- function(x, how, fit, ...){
  
  if(fit != "none"){
    dat <- x[c("left","right","condition")]
    dat$condition <- dat$condition[[1]]
    dat <- as.data.frame(dat)
    plot_gam(right, left, condition, data=dat, fittype=fit, ...)
  } else {
    
    X <- split(x$right, x$condition[[1]])
    Y <- split(x$left, x$condition[[1]])
    
    if(how == "colour"){
      plot(x$right, x$left, type='n')
      for(i in 1:length(X)){
        points(X[[i]], Y[[i]], col=palette()[i])
      }
    }
    if(how == "panel"){
      for(i in 1:length(X)){
        plot(X[[i]], Y[[i]], col=palette()[i])
      }
    }
  }
}


par(mfrow=c(1,2))
plotBy(log(Sepal.Length) + Petal.Length ~ Sepal.Width | Species, data=iris, fit="gam")

par(mfrow=c(1,2))
plotBy(log(Sepal.Length) + Petal.Length ~ Sepal.Width | Species, data=iris, fit="lm")

par(mfrow=c(1,2))
plotBy(log(Sepal.Length) + Petal.Length ~ Sepal.Width | Species, data=iris, how="colour")

par(mfrow=c(2,3))
plotBy(log(Sepal.Length) + Petal.Length ~ Sepal.Width | Species, data=iris, how="panel")


par(mfrow=c(1,1))
plotBy(weight ~ Time|Chick, fit="gam", data=ChickWeight, kgam=5, linecols="black", band=F, lwd=1)



