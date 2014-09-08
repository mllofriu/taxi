require('plotrix')
require('graphics')

# Function to draw the robot
# Robot should have x, y and theta columns
drawRobot <- function(robot, world){
  draw.circle(x=robot$x, y=robot$y, world$robotDiam/2)
  
  # Orientation line
#   orientLine <- data.frame(
#     x=robot$x, 
#     y=robot$y,
#     xend=robot$x+world$robotDiam/2*cos(robot$theta),
#     yend=robot$y+world$robotDiam/2*sin(robot$theta))
  lines(x=c(robot$x, robot$x+world$robotDiam/2*cos(robot$theta)), 
        y=c(robot$y, robot$y+world$robotDiam/2*sin(robot$theta)))
#   
#   # Return both the line and a circle
#   list(geom_path(data=circleFun(c(robot$x,robot$y), diameter=world$robotDiam), aes(x=x,y=y), color="gold4"),
#     geom_segment(data=orientLine, aes(x=x,y=y,xend=xend,yend=yend), color="gold4"))
# list(geom_point(aes(x=x, y=y), data=robot, size=30, shape=1, color="gold4"),
#          geom_segment(data=orientLine, aes(x=x,y=y,xend=xend,yend=yend),color="gold4"))

}

# Draw the grid of the world
drawGrid <- function(xdim, ydim){
  # The values of the line-ends
  hVals <- seq(-world$halfSquareSide,((xdim-1)+world$halfSquareSide), by=2*world$halfSquareSide)
  vVals <- seq(-world$halfSquareSide,((ydim-1)+world$halfSquareSide), by=2*world$halfSquareSide)
  # Horizontal and vertical lines
  hLines <- data.frame(t(sapply(hVals, 
                                function(y) {
                                  c(x=min(hVals), y=y, xend=max(hVals), yend=y)
                                  lines(x=c(min(hVals),max(hVals)), y=c(y,y))
                                }
            )))
  vLines <- data.frame(t(sapply(vVals, 
                function(x) {
                  c(x=x, y=min(vVals), xend=x, yend=max(vVals))
                  lines(x=c(x,x), y=c(min(vVals),max(vVals)))
                }
            )))
}

# Draw the walls as thick segments
drawWalls <- function(walls){
  plots <- rbind(sapply(coordinates(walls), function (w) {
#     d = c(x=w[1,1],y=w[1,2],xend=w[2,1],yend=w[2,2])
    lines(x=c(w[1,1], w[2,1]), y=c(w[1,2], w[2,2]), lwd=4)
  }))
#   geom_segment(data=data.frame(t(plots)), aes(x=x,y=y,xend=xend,yend=yend), size=2)
}

# Draw the interest places
drawPlaces <- function(places){
  text(places$x, places$y, places$label, cex=2)
#   geom_text(data=places, aes(x=x,y=y,label=label), size = 10)
}

# Draw ql value
drawValue <- function(rlData, goal) {
  sData <- data.frame()
  for (i in 0:9){
    for (j in 0:9){
      val <- getStateValue(rlData,data.frame(x=i,y=j),goal)/25
      val <- max(min(val,1), -1)
#       sData <- rbind(sData, data.frame(
#         xmin = i - world$halfSquareSide, xmax =  i + world$halfSquareSide,
#         ymin = j - world$halfSquareSide, ymax = j + world$halfSquareSide, fill = m))
      rect(i - world$halfSquareSide,j - world$halfSquareSide,
           i + world$halfSquareSide, j + world$halfSquareSide,
           col = rgb(.5-val/2,.5-val/2,1, alpha = .5))
    }
  }
#   print(sData$fill)
  
#   list(geom_rect(data = sData, aes(xmin=xmin,xmax=xmax, ymin=ymin,ymax=ymax,fill=fill), alpha=.5)
#        , scale_fill_gradient(limits=c(0,1),low='white', high='blue'))
}


# Draw the world and robot
draw <- function(robot, goal, world, rlData=NULL){
  # If there is no base plot, replot the whole thing
  dev.hold()
  plot(NULL,xlim=c(0 - world$halfSquareSide,world$xDim -1 + world$halfSquareSide),
       ylim=c(0 - world$halfSquareSide,world$yDim -1  + world$halfSquareSide),
       xaxt='n', yaxt='n', ylab="", xlab="")
  if (!is.null(rlData))
    drawValue(rlData, goal)
  drawGrid(world$xDim,world$yDim)
  drawRobot(robot, world)
  drawWalls(world$walls)
  drawPlaces(world$places)
  dev.flush()
}

par(bty = 'n') 
par(mar=rep(.2,4))

# draw(data.frame(x=1,y=1,theta=0))

# drawActivation <- function() {
#   sData <- data.frame()
#   
#   x=c(3)
#   y=c(3)
#   for (i in x){
#     for (j in y){
#       m <- getActivation(i,j,3,3,"small")
#       sData <- rbind(sData, data.frame(
#         xmin = i - world$halfSquareSide, xmax =  i + world$halfSquareSide,
#         ymin = j - world$halfSquareSide, ymax = j + world$halfSquareSide, fill = m))
#     }
#   }
#   #   print(sData$fill)
#   list(geom_rect(data = sData, aes(xmin=xmin,xmax=xmax, ymin=ymin,ymax=ymax,fill=fill), alpha=.5)
#        , scale_fill_gradient(limits=c(0,1),low='white', high='red'))
# }

# system.time(draw( robot, world, value))
