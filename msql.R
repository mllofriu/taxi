



msql <- function(dimx, dimy, numGoals, numActions, world){
  rlData <- list()
  value <- expand.grid(x=0:(dimx-1), y=0:(dimy-1), goal=1:(numGoals), type=factor(x=c("small", "large")), action=0:3)
  value$value <- 0
  
  # Get SpatialLines object for the walls
  wallssp <- SpatialLines(list(world$walls))
  
  # Erase walls
  largeAndHitsWalls <- apply(value, 1, function (state) {
    x <- as.numeric(state[1])
    y <- as.numeric(state[2])
    type <- state[4]
    
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
  
  rlData$value <- value[!largeAndHitsWalls,]
  rlData$alpha <- .8
  rlData$gamma <- 1
  rlData$goalReward <- 1000
  rlData$nonGoalReward <- 0
  class(rlData) <- "msql"
  rlData
}


update.msql <- function(rlData, preRobot, posRobot, goal, action, reward, taxicBefore, maxValAfter){
  # Get the max action for the robot after movement
  # Only get value from large cells
  maxVal <- max(getActionVals(rlData,posRobot,goal,0:3))
  value <- rlData$value
  
  value[value$action==action & value$goal == goal,'value'] <- apply(
    value[value$action==action & value$goal == goal,],1,
    function (state){
      x <- as.numeric(state[1])
      y <- as.numeric(state[2])
      type <- state[4]
      action <- as.numeric(state[5])
      val <- as.numeric(state[6])
      
      # Unnormalized version of activation
      activation <- getActivation.msql(preRobot$x, preRobot$y, x, y, type)
  
      activation * (val + rlData$alpha * (reward + rlData$gamm * (maxVal) - (val+taxicBefore))) + (1-activation) * val 
   }
  )

  rlData$value <- value
  rlData
}

getActivation.msql <- function(currX, currY, x, y, type){
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
    } else if (dist(rbind(c(x=x,y=y), c(x=currX, y=currY)))<= 1){ 
      activation <- .8
    }  else if (dist(rbind(c(x=x,y=y), c(x=currX, y=currY))) <= sqrt(2)){ 
      activation <- .7
    } else {
      activation <- 0
    }
  } 
  # Normalize activity - 1 from small , .4 + 4*.15 from large
  activation
}

stateV.msql <- function(rlData, currX, currY,goal, action) {
  value <- rlData$value
  
  currX <- round(currX)
  currY <- round(currY)
  sum(
    # Only apply to nearby cells
    apply(value[abs(value$x-currX) <= 1 & abs(value$y - currY) <= 1 &
                  value$action==action & value$goal == goal,],1, function(s){
      x <- as.numeric(s[1])
      y <- as.numeric(s[2])
      type <- s[4]
#       cat('type is ', type, '\n')
      stateVal <- as.numeric(s[6])
      
      # Normalize when calculating total value
      activation <- getActivation.msql(currX, currY, x, y, type) / 10
      
      stateVal * activation
    })
  )
}

getActionVals.msql <- function(rlData, robot, goal,posActions){
  # Get the value for each action
  sapply(posActions, function(action) stateV.msql(rlData,robot$x, robot$y,goal,action))
  #stateV(robot$x, robot$y, posActions, value)
}

getStateValue.msql <- function(rlData, robot, goal){
  max(getActionVals(rlData,robot,goal, 0:3))
}

# getActionVals.msql <- function(rlData, robot, goal, posActions){
#   value <- rlData$value
#   # Get the value for each action
#   sapply(posActions, function(action) stateV(robot$x, robot$y,goal,action, value))
#   #stateV(robot$x, robot$y, posActions, value)
# }

reward.msql <- function(rlData, postRobot, goalPos, eps){
  # If in the goal
  if (dist(rbind(postRobot[c('x','y')], goalPos[c('x','y')])) < eps)
    rlData$goalReward
  else 
    rlData$nonGoalReward
}

getMethod.msql <- function(rlData){
  "Multi-Scale QL"
}