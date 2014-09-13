



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
        Line(rbind(c(x,y), c(x,y+1))),
        Line(rbind(c(x,y), c(x-1,y-1))),
        Line(rbind(c(x,y), c(x+1,y+1))),
        Line(rbind(c(x,y), c(x-1,y+1))),
        Line(rbind(c(x,y), c(x+1,y-1)))
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
  rlData$gamma <- .90
  rlData$goalReward <- 1000
  rlData$nonGoalReward <- -5
  class(rlData) <- "msql"
  rlData
}


update.msql <- function(rlData, preRobot, posRobot, goal, action, reward, taxicBefore, maxValAfter){
  # Get the max action for the robot after movement
  maxVal <- max(getActionVals(rlData, posRobot, goal, 0:3))
  
  active <- getActivation.msql(rlData, preRobot$x, preRobot$y , goal, action)
  active['value'] <- active['activation'] * (active['value'] + rlData$alpha * (reward + rlData$gamm * maxVal - active['value'])) + (1-active['activation']) * active['value'] 
  
  # Keep subset in sync with getActivation
  rlData$value[abs(rlData$value$x-round(preRobot$x)) <= 2 &
                 abs(rlData$value$y - round(preRobot$y)) <= 2 &
                 rlData$value$action==action &
                 rlData$value$goal == goal, 'value'] <- active['value'] 
  
#   rows <- rlData$value$action==action & rlData$value$goal == goal & 
#     abs(rlData$value$x-round(preRobot$x)) <= 2 & abs(rlData$value$y - round(preRobot$y)) <= 2
#   rlData$value[rows,'value'] <- apply(
#     rlData$value[rows,],1,
#     function (state){
#       x <- as.numeric(state[1])
#       y <- as.numeric(state[2])
#       type <- state[4]
#       action <- as.numeric(state[5])
#       val <- as.numeric(state[6])
#       
#       # Unnormalized version of activation
#       activation <- getActivation.msql(preRobot$x, preRobot$y, x, y, type)
#   
#       activation * (val + rlData$alpha * (reward + rlData$gamm * (maxVal) - (val+taxicBefore))) + (1-activation) * val 
#    }
#   )

  rlData
}

getActivation.msql <- function(rlData,currX, currY, goal, action){

  currX <- round(currX)
  currY <- round(currY)
  
  active <- rlData$value[
    abs(rlData$value$x-currX) <= 2 &
    abs(rlData$value$y - currY) <= 2 &
    rlData$value$action==action &
    rlData$value$goal == goal,]
  
  active['activation'] <- 0
  active[active$x == currX && active$y == currY, 'activation'] <- 1
  active[active$type == 'large' & 
           (abs(active$x-currX) == 1 & abs(active$y - currY) == 0),'activation' ] <- .8
  active[active$type == 'large' & 
           (abs(active$x-currX) == 0 & abs(active$y - currY) == 1),'activation' ] <- .8
  active[active$type == 'large' &
           (abs(active$x-currX) == 1 & abs(active$y - currY) == 1),'activation' ] <- .7
  
  active
}

stateV.msql <- function(rlData, currX, currY,goal, action) {
  active <- getActivation.msql(rlData, currX, currY, goal, action)
  
  totalActivation <- sum(active$activation)
  active$activation <- active$activation / totalActivation

  sum(active$activation * active$value)
}

getActionVals.msql <- function(rlData, robot, goal,posActions){
  # Get the value for each action
  sapply(posActions, function(action) stateV.msql(rlData,robot$x, robot$y,goal,action))
  #stateV(robot$x, robot$y, posActions, value)
}

getStateValue.msql <- function(rlData, robot, goal){
  max(getActionVals(rlData, robot, goal, 0:3))
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