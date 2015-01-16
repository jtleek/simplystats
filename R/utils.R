#' A function for creating simplystats themed heatmaps
#' 
#' Add the logo in black or white depending on the theme
#' 
#' @param theme Options are "black" or "white" 
#' 
#' @export
#' 
#' 


add_simply_logo = function(theme="black"){
  if(theme=="black"){
    simplylogo = readPNG(system.file("simplystats-black.png",package = "simplystats"))
  }
  if(theme=="white"){
    simplylogo = readPNG(system.file("simplystats-white.png",package = "simplystats"))
  }
  
  dims<-dim(simplylogo)[1:2] #number of x-y pixels for the logo (aspect ratio)
  AR<-dims[1]/dims[2]*1.2
  
  logo=rasterGrob(simplylogo, 
                  x=unit(0.90,"npc"), y=unit(0.03,"npc"),
                  width=unit(0.2,"npc"), height=unit((AR*0.2),"npc"))
  grid.draw(logo)
}


#' A function for adding simplystats themed axes
#' 
#' Add axis to plot
#' 
#' 
#' @export
#' 
#' 


add_simplyaxis = function(){
  at1 = axTicks(1)
  at2 = axTicks(2)
  axis(side=1,at=at1,tick=FALSE)
  axis(side=2,at=at2,tick=FALSE)
}

#' A function for adding simplystats themed axes
#' 
#' Add axis to time series plot
#' 
#' 
#' 
#' @export
#' 
#' 

add_simplyaxis_ts = function(){
  at2 = axTicks(2)
  axis(side=2,at=at2,tick=FALSE)
}



#' Set the overall plotting parameters for the two simplystats themes
#' 
#' The theme can be either black or white. 
#'
#' @param theme Options are "black" or "white" 
#' 
#' @export
#' 
#' 


set_simplypar = function(theme="black"){
 
  if(theme=="black"){
    bg = "black"
    fg="white"
    col.axis="white"
    col.lab="white"
    col.main="white"
  }
  
  if(theme=="white"){
    bg = "white"
    fg="black"
    col.axis="black"
    col.lab="black"
    col.main="black"
  }
  
  op = par(no.readonly=TRUE)
  par(bg=bg,fg=fg,
      col.axis=col.axis,col.lab=col.lab,
      col.main=col.main,mar=c(6.1,4.1,4.1,2.1))
  return(op)
}


#' Get the simplystats colors
#' 
#' Colors for plots. 
#'
#' @param col The numeric color vector input by the user. 
#' 
#' @export
#' 
#' 


get_simplycols = function(col){
  cols = c("#20B2E3", "#F89915","#6AB833")
  colr = colorRampPalette(cols)
  if(length(unique(col)) <= 3){simplycols = colr(3)}else{simplycols=colr(length(unique(col)))}
  return(simplycols)
}

#' Add a simplystats legend
#' 
#' Colored like simplystats
#'
#' @param col Parameters to legend
#' 
#' @export
#' 
#' 


add_simplylegend = function(...,col=1){
 simplycols = get_simplycols(col)
 legend(...,col=simplycols[col],bty="n")
}
