halfVisionField <- pi/2

dist <- function(p1,p2){
  sqrt((p1$x - p2$x)^2 + (p1$y - p2$y)^2)
}

visible <- function(robot, goal, walls, eps){
  # If they are the same point
  if (dist(robot, goal) < eps)
    T
  else {
    # Check line of sight
    wallssp <- SpatialLines(list(walls))
    path <- Line(rbind(c(robot$x, robot$y),c(goal$x, goal$y)))
    pathsp <- SpatialLines(list(Lines(list(path),"path")))
    # Check angle to goal in visual range
    angleToGoal <- atan2(goal$y - robot$y, goal$x - robot$x)
#     print(angleToGoal)
#     print(robot$theta)
    angleOrientDiff <- atan2(sin(angleToGoal-robot$theta),cos(robot$theta-angleToGoal))
#     print (angleOrientDiff)
#     print((abs(angleOrientDiff) <= halfVisionField))
    (! gIntersects(wallssp, pathsp) && (abs(angleOrientDiff) <= halfVisionField))
  }
 
}

selectAction <- function(robot, posActions, world, goal){
  action <- posActions[1]
  
  if (visible(robot, goal, world.walls, world.eps)){
    print("Taxic")
    # Go to the goal
    # Simulate all actions and minimize distance subject to visibility
    d <- dist(robot,goal)
    for (posAction in posActions) {
      newTheta <- (robot$theta + posAction) %% (2 * pi)
      nPos <- c(robot$x + stepSize * cos(newTheta), robot$y + stepSize * sin(newTheta))
      newRob <- data.frame(x=nPos[1], y=nPos[2], theta=newTheta)
      if (visible(newRob, goal, world.walls, world.eps) && dist(newRob,goal) < d){
        action <- posAction
        d <- dist(newRob,goal)
      }
    }
  } else {
    print("Exploring")
    # Explore
    # Favor forward motions
    if (0 %in% posActions)
      if (runif(1) > .8)
        action <- 0
    else{
      posActions <- posActions[posActions != 0]
      action <- posActions[[sample(1:length(posActions), 1)]]
    }
    else
      action <- posActions[[sample(1:length(posActions), 1)]]
  }
  action
}