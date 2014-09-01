
# Size of a robot's step
stepSize <- 1

# Possible actions (angles)
actions <- c(0, pi/2, pi, -pi/2)

# Get the possible actions, which dont go through walls
possibleActions <- function(robot, world){
  # Get SpatialLines object for the walls
  wallssp <- SpatialLines(list(world.walls))
  
  # Initialize posible actions
  posActions <- c()
  # Fore each one
  for (posAction in actions) {
    # Simulate the action
    # New theta
    newTheta <- (robot$theta + posAction) %% (2 * pi)
    # New position
    nPos <- c(robot$x + stepSize * cos(newTheta), robot$y + stepSize * sin(newTheta))
    # The path traveled
    path <- Line(rbind(c(robot$x, robot$y),nPos))
    # SpatialLines object for the path
    pathsp <- SpatialLines(list(Lines(list(path),"path")))
    
    # IF does not intersect any wall, add the action to possible actions
    if (!gIntersects(wallssp, pathsp)){
      posActions <- c(posActions, posAction)
    }
  }
  # Return the collection of possible actions
  posActions
}

# Move the robot, rotating action degrees and advancing
move <- function(robot, action){
  # Rotated robot
  newRob <- data.frame(x=robot$x, y=robot$y, theta=(robot$theta+action) %% (2*pi))
  # Move it
  newRob$x <- newRob$x + stepSize * cos(newRob$theta)
  newRob$y <- newRob$y + stepSize * sin(newRob$theta)
  # Return the robot
  newRob
}