#' A function for creating simplystats themed time-series plots
#' 
#' This function creates time series plots. To use it, you should pass a vector
#' of numeric color values (it will create a palette to match the length). You can
#' also specify whether to add the simplystats logo or not.  
#' 
#' @param ... Pass any usual plotting parameters 
#' @param col A numeric vector specifying the colors of points
#' @param pch The default pch is 19, cause it is the best. 
#' @param logo Logical, if true, then the simplystats logo is added to the plot
#' @param theme Options are "black" or "white"
#' 
#' @export
#' 
#' 

simplyts = function(...,col=1,pch=19,logo=TRUE,theme="black"){
  op = set_simplypar(theme)
  simplycols = get_simplycols(col)
  plot(...,
       panel.first = grid(lty="dashed"),
       bty="n",pch=19,col=simplycols[col],yaxt="n")
  add_simplyaxis_ts()
  if(logo){add_simply_logo(theme)}
  par(op)
}