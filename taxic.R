halfVisionField <- pi/2
angleEps <- 1e-10

goalVal <- 10


# dist <- function(p1,p2){
#   sqrt((p1$x - p2$x)^2 + (p1$y - p2$y)^2)
# }

visible <- function(robot, place, walls, eps){
  # If they are the same point
  if (dist(rbind(robot[c('x','y')], place[c('x','y')])) < eps)
    FALSE
  else {
    # Check line of sight
    wallssp <- SpatialLines(list(walls))
    path <- Line(rbind(c(robot$x, robot$y),c(place$x, place$y)))
    pathsp <- SpatialLines(list(Lines(list(path),"path")))
    # Check angle to place in visual range
    angleToGoal <- atan2(place$y - robot$y, place$x - robot$x)
#     print(angleToGoal)
#     print(robot$theta)
    angleOrientDiff <-  atan2(sin(angleToGoal-robot$theta),cos(robot$theta-angleToGoal))
#     print (angleOrientDiff)
#     print((abs(angleOrientDiff) <= halfVisionField))
#     (! gIntersects(wallssp, pathsp) && (abs(angleOrientDiff) <= (halfVisionField + angleEps)) && dist(rbind(robot[c('x','y')], place[c('x','y')])) <= 4)
    (! gIntersects(wallssp, pathsp) && (abs(angleOrientDiff) <= (halfVisionField + angleEps)))
  }
 
}

taxicVals <- function(robot, posActions, places, eps){
  
  
  actionVals <- rep(0, length(posActions))
  
  for (p in 1:4){
    place <- places[p,]
#     print(place)
    if (visible(robot, place, world$walls, world$eps)){
#       print("Taxic")
      # Go to the goal
      # Simulate all actions and minimize distance subject to visibility
      d <- dist(rbind(robot[c('x','y')], place[c('x','y')]))
      for (i in seq(1,length(posActions))) {
  #       newTheta <- (robot$theta + posActions[i]) %% (2 * pi) # Relative actions
        newTheta <- (posActions[i] * pi/2)
        nPos <- c(robot$x + stepSize * cos(newTheta), robot$y + stepSize * sin(newTheta))
        newRob <- data.frame(x=nPos[1], y=nPos[2], theta=newTheta)
        # IF arrived or closer
        if (dist(rbind(newRob[c('x','y')], place[c('x','y')])) < world$eps || 
              (visible(newRob, place, world$walls, world$eps) 
               && dist(rbind(newRob[c('x','y')], place[c('x','y')])) < d)){
          action <- i
          d <- dist(rbind(newRob[c('x','y')], place[c('x','y')]))
          
        }
      }
    
      
#       if (d < world$eps)
#         actionVals[action] <- 2*goalVal
#       else
      actionVals[action] <- goalVal
#   / max(d,1)
    } 
  }

  actionVals
}