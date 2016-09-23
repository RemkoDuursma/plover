
recycle <- function(x,n){
  len <- length(x)
  rep(x, ceiling(n/len))[1:n]
}

set_cols <- function(cols1, cols2, pal){
  
  if(is.null(cols1))cols1 <- cols2
  if(is.null(cols2))cols2 <- cols1
  if(is.null(cols1) && is.null(cols2)){
    cols1 <- cols2 <- pal
  }
  return(list(cols1=cols1, cols2=cols2))
}

