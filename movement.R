
stepSize <- 1

actions <- c(0, pi/2, pi, -pi/2)

possibleActions <- function(robot, world){
  wallssp <- SpatialLines(list(world.walls))
  
  posActions <- c()
  
  for (posAction in actions) {
    newTheta <- (robot$theta + posAction) %% (2 * pi)
    nPos <- c(robot$x + stepSize * cos(newTheta), robot$y + stepSize * sin(newTheta))
    path <- Line(rbind(c(robot$x, robot$y),nPos))
    pathsp <- SpatialLines(list(Lines(list(path),"path")))
    
    #   print (wallssp)
    #   print (gIntersects(wallssp, pathsp))
    if (!gIntersects(wallssp, pathsp)){
      posActions <- c(posActions, posAction)
    }
  }
  
  posActions
}

move <- function(robot, action){
  # Rotated robot
  newRob <- data.frame(x=robot$x, y=robot$y, theta=(robot$theta+action) %% (2*pi))
  # Move it
  newRob$x <- newRob$x + stepSize * cos(newRob$theta)
  newRob$y <- newRob$y + stepSize * sin(newRob$theta)

  newRob
}