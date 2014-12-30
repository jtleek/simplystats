#' A function for adding points using the simplystats theme
#' 
#' This function creates scatterplots. To use it, you should pass a vector
#' of numeric color values (it will create a palette to match the length). You can
#' also specify whether to add the simplystats logo or not.  
#' 
#' @param ... Pass any usual plotting parameters to points
#' @param col A numeric vector specifying the colors of points 
#' 
#' @export
#' 


simplylines = function(...,col=1){
  simplycols = get_simplycols(col)
  lines(...,
         pch=19,col=simplycols[col])
  par(op)
}