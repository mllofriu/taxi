library('sp')
library('rgeos')

msac <- function(dimx, dimy, numGoals, numActions, world){
  value <- expand.grid(x=0:(dimx-1), y=0:(dimy-1), goal=1:(numGoals), type=factor(x=c("small", "large")))
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
  actionVals <- expand.grid(x=0:(dimx-1), y=0:(dimy-1), goal=1:(numGoals), type=factor(x=c("small", "large")), action=0:(numActions-1))
  actionVals$value <- 0
  
  rlData <- list(stateValues=value[!largeAndHitsWalls,],
                 actionVals=actionVals[!largeAndHitsWalls,])
  
  rlData$alpha <- .95
  rlData$gamma <- 1
  rlData$goalReward <- 100
  rlData$nonGoalReward <- -5
  rlData$activNorm <- 5
  
  class(rlData) <- "msac"
  
  rlData
}

getActionVals.msac <- function(rlData, robot, goal, posActions){
  # Get the value for each action
  sapply(posActions, function(action) actionValues.msac(rlData, robot$x, robot$y,goal,action))
}

actionValues.msac <- function(rlData, currX, currY,goal, action){
  av <- rlData$actionVals
  currX <- round(currX)
  currY <- round(currY)
  sum(
    # Only apply to nearby cells
    apply(av[abs(av$x-currX) <= 2 &
             abs(av$y - currY) <= 2 &
             av$action==action &
             av$goal == goal &
             av$type == "small",],1, 
          function(s){
            x <- as.numeric(s[1])
            y <- as.numeric(s[2])
            type <- s[4]
            stateVal <- as.numeric(s[6])
            
            # Normalize when calculating total value
            activation <- getActivation(currX, currY, x, y, type)
            
            stateVal * activation
          })
  )
}

getStateValue.msac <- function(rlData, robot, goal){
  currX <- round(robot$x)
  currY <- round(robot$y)
  sum(
    # Only apply to nearby cells
    apply(rlData$stateValues[abs(rlData$stateValues$x-currX) <= 2 & 
                        abs(rlData$stateValues$y - currY) <= 2 &
                        rlData$stateValues$goal == goal,],1, 
          function(s){
            x <- as.numeric(s[1])
            y <- as.numeric(s[2])
            type <- s[4]
            stateVal <- as.numeric(s[5])
            
            # Normalize when calculating total value
            activation <- getActivation(currX, currY, x, y, type) / rlData$activNorm
            
            stateVal * activation
          })
  )
}

update.msac <- function(rlData, preRobot, posRobot, goal, action, reward, taxicBefore, taxicAfter){
  preVal <- getStateValue(rlData, preRobot, goal)
  postVal <- getStateValue(rlData, posRobot, goal)
  
  error <- rlData$gamma*(postVal + taxicAfter) + reward - (preVal + taxicBefore)
#   error <- gamma*(postVal ) + reward - (preVal) 

  # update value
  rlData$stateValues[rlData$stateValues$goal == goal,'value'] <- apply(
    rlData$stateValues[rlData$stateValues$goal == goal,],1,
    function (state){
      x <- as.numeric(state[1])
      y <- as.numeric(state[2])
      type <- state[4]
      val <- as.numeric(state[5])
      
      # Unnormalized version of activation
      activation <- getActivation(preRobot$x, preRobot$y, x, y, type)
      
      activation * (val + rlData$alpha * error) + (1-activation) * val 
    }
  )
  
  # update action values
  rlData$actionVals[rlData$actionVals$goal == goal & 
                        rlData$actionVals$action == action & rlData$actionVals$type == "small",'value'] <- apply(
    rlData$actionVals[rlData$actionVals$goal == goal & rlData$actionVals$action == action & rlData$actionVals$type == "small",],1,
    function (state){
      x <- as.numeric(state[1])
      y <- as.numeric(state[2])
      type <- state[4]
      val <- as.numeric(state[6])
      
      # Unnormalized version of activation
      activation <- getActivation(preRobot$x, preRobot$y, x, y, type)
      
      activation * (val + rlData$alpha * error) + (1-activation) * val 
    }
  )
  
  rlData
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
    } else if (dist(rbind(c(x,y), c(currX, currY))) <= 1){ 
      activation <- .5
    }  else if (dist(rbind(c(x,y), c(currX, currY))) <= sqrt(2)){ 
      activation <- .25
    } else {
      activation <- 0
    }
  }
  activation
}

reward.msac <- function(rlData, postRobot, goalPos, eps){
  # If in the goal
  if (dist(rbind(postRobot[c('x','y')], goalPos[c('x','y')])) < eps)
    rlData$goalReward
  else 
    rlData$nonGoalReward
}

getMethod.msac <- function(rlData){
  "Multi-Scale AC"
}
