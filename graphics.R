require(ggplot2)

basePlot <- NULL

# Function to draw a cricle
circleFun <- function(center = c(0,0),diameter = 1, npoints = 100){
  r = diameter / 2
  tt <- seq(0,2*pi,length.out = npoints)
  xx <- center[1] + r * cos(tt)
  yy <- center[2] + r * sin(tt)
  return(data.frame(x = xx, y = yy))
}

# Function to draw the robot
# Robot should have x, y and theta columns
drawRobot <- function(robot){
  # Orientation line
  orientLine <- data.frame(
    x=robot$x, 
    y=robot$y,
    xend=robot$x+world.robotDiam/2*cos(robot$theta),
    yend=robot$y+world.robotDiam/2*sin(robot$theta))
  
  # Return both the line and a circle
  list(geom_path(data=circleFun(c(robot$x,robot$y), diameter=world.robotDiam), aes(x=x,y=y), color="gold4"),
    geom_segment(data=orientLine, aes(x=x,y=y,xend=xend,yend=yend), color="gold4"))
# list(geom_point(aes(x=x, y=y), data=robot, size=30, shape=1, color="gold4"),
#          geom_segment(data=orientLine, aes(x=x,y=y,xend=xend,yend=yend),color="gold4"))

}

# Draw the grid of the world
drawGrid <- function(xdim, ydim){
  # The values of the line-ends
  hVals <- seq(-world.halfSquareSide,((xdim-1)+world.halfSquareSide), by=2*world.halfSquareSide)
  vVals <- seq(-world.halfSquareSide,((ydim-1)+world.halfSquareSide), by=2*world.halfSquareSide)
  # Horizontal and vertical lines
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
  # Merge both lines
  lines <- rbind(hLines, vLines)
  # 
  list(geom_segment(data=lines,aes(x=x,xend=xend,y=y,yend=yend)))
}

# Draw the walls as thick segments
drawWalls <- function(walls){
  plots <- rbind(sapply(coordinates(walls), function (w) {
    d = c(x=w[1,1],y=w[1,2],xend=w[2,1],yend=w[2,2])
  }))
  geom_segment(data=data.frame(t(plots)), aes(x=x,y=y,xend=xend,yend=yend), size=2)
}

# Draw the interest places
drawPlaces <- function(places){
  geom_text(data=places, aes(x=x,y=y,label=label), size = 10)
}

# Draw ql value
drawValue <- function(value) {
  sData <- data.frame()
  for (i in 0:4){
    for (j in 0:4){
      m <- min(max (getQLVals(data.frame(x=i,y=j),0:3,value)/.1),1)
      sData <- rbind(sData, data.frame(
        xmin = i - world.halfSquareSide, xmax =  i + world.halfSquareSide,
        ymin = j - world.halfSquareSide, ymax = j + world.halfSquareSide, fill = m))
    }
  }
#   print(sData$fill)
  list(geom_rect(data = sData, aes(xmin=xmin,xmax=xmax, ymin=ymin,ymax=ymax,fill=fill), alpha=.5)
       , scale_fill_gradient(limits=c(0,1),low='white', high='blue'))
}

# Blank theme
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

# Draw the world and robot
draw <- function(robot, world, value=NULL){
  # If there is no base plot, replot the whole thing
  if (is.null(basePlot)){
    ggplot() +
      drawGrid(world.xDim,world.yDim) +
      drawWalls(world.walls) +
      drawPlaces(world.places) +
      drawRobot(robot) +
      blank_theme()
  } else {
    # use the old base plot to avoid recalculating
    if (is.null(value))
      basePlot + drawRobot(robot)
    else {
      ggplot() +
        drawValue(value) +
        drawGrid(world.xDim,world.yDim) +
        drawWalls(world.walls) +
        drawPlaces(world.places) +
        drawRobot(robot) +
        blank_theme()
    }
  }
  
}

# Save the base plot to avoid recalculating
saveBasePlot <- function(world){
  basePlot <<- ggplot() +
    drawGrid(world.xDim,world.yDim) +
    drawWalls(world.walls) +
    drawPlaces(world.places) +
    blank_theme()
}

# draw(data.frame(x=1,y=1,theta=0))

