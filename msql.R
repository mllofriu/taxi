
alpha <- .8
gamma <- 1
goalReward <- 100
nonGoalReward <- 0


initValue <- function(dimx, dimy, numActions, world){
  value <- expand.grid(x=0:(dimx-1), y=0:(dimy-1), type=factor(x=c("small", "large")), action=0:3)
  value$value <- 0
  
  # Get SpatialLines object for the walls
  wallssp <- SpatialLines(list(world.walls))
  
  # Erase walls
  largeAndHitsWalls <- apply(value, 1, function (state) {
    x <- as.numeric(state[1])
    y <- as.numeric(state[2])
    type <- state[3]
    
    if (type == "large"){
      ventralConnectLines <- Lines(list(
        Line(rbind(c(x-1,y), c(x,y))),
        Line(rbind(c(x,y), c(x+1,y))),
        Line(rbind(c(x,y-1), c(x,y))),
        Line(rbind(c(x,y), c(x,y+1)))
      ),
      "ventralConnect")
      gIntersects(wallssp, SpatialLines(list(ventralConnectLines)))
    } else
      FALSE
    })
  
  # Return only those that are not large or that do not hit a wall
#   print(unlist(largeAndHitsWalls))
  
  value[!largeAndHitsWalls,]
#   value
}


update <- function(preRobot, posRobot, action, value, reward){
  # Get the max action for the robot after movement
  # Only get value from large cells
  maxVal <- max(getQLVals(posRobot,0:3,value[]))
  
  value[value$action==action,5] <- apply(value[value$action==action,],1, function (state){
    x <- as.numeric(state[1])
    y <- as.numeric(state[2])
    type <- state[3]
    action <- as.numeric(state[4])
    val <- as.numeric(state[5])
    
    # Unnormalized version of activation
    activation <- getActivation(preRobot$x, preRobot$y, x, y, type)

    activation * (val + alpha * (reward + gamma * maxVal - val)) + (1-activation) * val 
  })

  value
}

getActivation <- function(currX, currY, x, y, type){
#   print("Tipo ")
#   print(type)
#   activation <- 0
  currX <- round(currX)
  currY <- round(currY)
  
  if (type == "small"){
    if (currX == x && currY == y)
      # if the current cell is the state cell, full value
      activation <- 1
    else
      activation <- 0
  } else if (type == "large"){
    if (currX == x && currY == y){
      activation <- 1
    } else if (dist(list(x=x,y=y), list(x=currX, y=currY)) <= 1){ 
      activation <- .8
    }  else if (dist(list(x=x,y=y), list(x=currX, y=currY)) <= sqrt(2)){ 
      activation <- .7
    } else {
      activation <- 0
    }
  } 
  # Normalize activity - 1 from small , .4 + 4*.15 from large
  activation
}

stateV <- function(currX, currY, action, value) {
  currX <- round(currX)
  currY <- round(currY)
  sum(
    apply(value[value$action==action,],1, function(s){
      x <- as.numeric(s[1])
      y <- as.numeric(s[2])
      type <- s[3]
      stateVal <- as.numeric(s[5])
      
      # Normalize when calculating total value
      activation <- getActivation(currX, currY, x, y, type) / 10
      
      stateVal * activation
    })
  )
}

getQLVals <- function(robot, posActions, value){
  # Get the value for each action
  sapply(posActions, function(action) stateV(robot$x, robot$y,action, value))
  #stateV(robot$x, robot$y, posActions, value)
}

getQLActionVals <- function(robot, posActions, value){
  # Get the value for each action
  sapply(posActions, function(action) stateV(robot$x, robot$y,action, value))
  #stateV(robot$x, robot$y, posActions, value)
}

reward <- function(postRobot, goal, eps){
  # If in the goal
  if (dist(postRobot, goal) < eps)
    goalReward
  else 
    nonGoalReward
}

getMethod <- function(){
  "Multi-Scale QL"
}