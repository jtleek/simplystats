#' A function for creating simplystats themed histograms
#' 
#' This function creates histograms. The default histogram is simplystats blue
#' and there is no default title. You can
#' also specify whether to add the simplystats logo or not.  
#' 
#' @param ... Pass any usual histogram parameters 
#' @param col A numeric vector specifying the colors of points
#' @param logo Logical, if true, then the simplystats logo is added to the plot
#' @param main The title of the plot, defaults to no title
#' @param theme Options are "black" or "white"
#' 
#' @export
#' 
#' 

simplyhist = function(...,col=1,logo=TRUE,main="",theme="black"){
  op = set_simplypar(theme)
  simplycols = get_simplycols(col)
  
  hist(...,col=simplycols[col], 
       bty="n",xaxt="n",yaxt="n",main="")
  add_simplyaxis()
  if(logo){add_simply_logo(theme)}
  par(op)
}