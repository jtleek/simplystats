#' A function for creating simplystats themed heatmaps
#' 
#' This function creates histograms. The default is a theme that runs
#' from blue (low) to orange (high). The heatmap is not clustered and 
#' is oriented the "right" way so that the upper left cell of the matrix
#' is the upper left cell plotted in the heatmap. 
#' 
#' @param dat A data matrix
#' @param x The x axis labels, must be of length dim(dat)[2]
#' @param y The y axis labels, must be of length dim(dat)[1]
#' @param ... Other arguments to the heatmap function
#' @param ncol The number of colors that you want 
#' @param logo Logical, if true, then the simplystats logo is added to the plot
#' @param theme Options are "black" or "white"
#' 
#' @export
#' 
#' 

simplyheat = function(dat,x=NULL,y=NULL,...,ncol=11,logo=TRUE,theme="black"){
  if(is.null(x)){x = 1:dim(dat)[2]}
  if(is.null(y)){y = 1:dim(dat)[1]}
  
  op = set_simplypar(theme)
  simplycols = get_simplycols(3)
  heatcols = colorRampPalette(c(simplycols[1],"grey",simplycols[2]))(ncol)
  probs = seq(0,1,length=(ncol+1))
  
  layout(t(matrix(c(1,1,1,1,2,1,1,1,1,2,1,1,1,1,2),byrow=F,nrow=5)))
  image(x,y,t(dat)[,nrow(dat):1],
        col=heatcols,
        breaks=quantile(dat,probs),
        bty="n",xaxt="n",yaxt="n",...)
  add_simplyaxis(...)
  
  image(t(as.matrix(1:length(heatcols))),
        col=heatcols,xaxt="n",yaxt="n")
  axis(2,at=seq(0,1,length=ncol),
       labels=round(quantile(dat,probs),2)[2:(ncol+1)],las=2)
  if(logo){add_simply_logo(theme)}
  par(op,new=TRUE)
}