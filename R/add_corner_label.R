#' Simple function for placing text labels in the corner of a figure.
#' @description Designed for labelling figures in the corner with 'A', 'B', and so on. Finds the coordinates for you.
#' @export
add_corner_label <- function(txt, where, inset=0.08, inset.x=inset, inset.y=inset, log.y=FALSE, log.x=FALSE,...){
  u <- par()$usr
  if(grepl("left",where))x <- u[1] + inset.x*(u[2]-u[1])
  if(grepl("right",where))x <- u[2] - inset.x*(u[2]-u[1])
  if(grepl("bottom",where))y <- u[3] + inset.y*(u[4]-u[3])
  if(grepl("top",where))y <- u[4] - inset.y*(u[4]-u[3])
  
  if(log.x)
    x <- 10^x
  
  if(log.y)
    y <- 10^y
  
  text(x,y,txt,...)
}
