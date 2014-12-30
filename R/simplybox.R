#' A function for creating simplystats themed boxplots
#' 
#' This function creates boxplots.  You can
#' also specify whether to add the simplystats logo or not.  
#' 
#' @param ... Pass any usual boxplot parameters
#' @param col A numeric vector specifying the colors of points
#' @param logo Logical, if true, then the simplystats logo is added to the plot
#' @param theme Options are "black" or "white"
#' 
#' @export
#' 
#' 

simplybox = function(...,col=1,logo=TRUE,theme="black"){
  op = set_simplypar(theme)
  simplycols = get_simplycols(col)
  pointcol = "black"; if(theme=="black"){pointcol="white"}
  tmp = boxplot(...)
  grid(nx=NA, ny=NULL) #grid over boxplot

  boxplot(...,col=simplycols[col], 
       bty="n",xaxt="n",yaxt="n",main="",frame.plot=FALSE)
  at2 = axTicks(2)
  grid(nx=NA,ny=NULL)
  stripchart(...,vertical=T,method="jitter",add=TRUE,pch=19,col=pointcol)
  
  axis(side=1,at=1:length(tmp$names),labels=tmp$names,tick=FALSE)
  axis(side=2,at=at2,tick=FALSE)
  
  if(logo){add_simply_logo(theme)}
  par(op)
}