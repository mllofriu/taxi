require(ggplot2)

circleFun <- function(center = c(0,0),diameter = 1, npoints = 100){
  r = diameter / 2
  tt <- seq(0,2*pi,length.out = npoints)
  xx <- center[1] + r * cos(tt)
  yy <- center[2] + r * sin(tt)
  return(data.frame(x = xx, y = yy))
}

drawRobot <- function(robot){
  orientLine <- data.frame(
    x=robot$x, 
    y=robot$y,
    xend=robot$x+robotDiam/2*cos(robot$theta),
    yend=robot$y+robotDiam/2*sin(robot$theta))
  
  list(geom_path(data=circleFun(c(robot$x,robot$y), diameter=robotDiam), aes(x=x,y=y)),
    geom_segment(data=orientLine, aes(x=x,y=y,xend=xend,yend=yend)))
}

drawGrid <- function(xdim, ydim){
  hVals <- seq(-halfSquareSide,((xdim-1)+halfSquareSide), by=2*halfSquareSide)
  vVals <- seq(-halfSquareSide,((ydim-1)+halfSquareSide), by=2*halfSquareSide)
  
  hLines <- data.frame(t(sapply(hVals, 
                                function(y) {
                                  c(x=min(hVals), y=y, xend=max(hVals), yend=y)
                                }
            )))
  vLines <- data.frame(t(sapply(vVals, 
                function(x) {
                  c(x=x, y=min(vVals), xend=x, yend=max(vVals))
                }
            )))
  
  lines <- merge(hLines, vLines, all=T)
  
  list(geom_segment(data=lines,aes(x=x,xend=xend,y=y,yend=yend)))
}

drawWalls <- function(walls){
  plots <- sapply(coordinates(walls), function (w) {
    d = data.frame(x=w[1,1],y=w[1,2],xend=w[2,1],yend=w[2,2])
    geom_segment(data=d, aes(x=x,y=y,xend=xend,yend=yend, size=2))
  })
}

drawPlaces <- function(places){
  geom_text(data=places, aes(x=x,y=y,label=label))
}

blank_theme <- function() {
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        axis.line = element_blank(), 
        axis.text.x = element_blank(), 
        axis.text.y = element_blank(),
        axis.ticks = element_blank(), 
        axis.title.x = element_blank(), 
        axis.title.y = element_blank(),
        legend.position = "none") 
        
}

draw <- function(robot, world){
  ggplot() +
    drawGrid(world.xDim,world.yDim) +
    drawWalls(world.walls) +
    drawPlaces(world.places) +
    drawRobot(robot) +
    blank_theme()
}



