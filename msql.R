
alpha <- .8
gamma <- .95
goalReward <- 2
nonGoalReward <- 0


initValue <- function(dimx, dimy, numActions){
  value <- expand.grid(x=0:4, y=0:4, type=factor(x=c("small", "large")), action=0:3)
  value$value <- 0
  value
}


update <- function(preRobot, posRobot, action, value, reward){
  # Get the max action for the robot after movement
  maxVal <- max(getQLVals(posRobot,1:4,value))
  
  value[,5] <- apply(value,1, function (state){
    x <- as.numeric(state[1])
    y <- as.numeric(state[2])
    type <- state[3]
    action <- as.numeric(state[4])
    val <- as.numeric(state[5])
    
    activation <- getActivation(preRobot$x, preRobot$y, x, y, type)
    activation * (val + alpha * (reward + gamma * maxVal - val)) + (1-activation) * val 
  })

  value
}

getActivation <- function(currX, currY, x, y, type){
#   print(type)
  if (type == "small"){
    if (currX == x && currY == y)
      # if the current cell is the state cell, full value
      activation <- 1
    else
      activation <- 0
  } else if (type == "large"){
    if (currX == x && currY == y)
      activation <- .4
    else if (dist(list(x=x,y=y), list(x=currX, y=currY)) == 1)
      activation <- .15
    else
      activation <- 0
  }
  # Normalize activity - 1 from small , .4 + 4*.15 from large
  activation / ( 1 + 1)
}

stateV <- function(currX, currY, theta, value) {
  currX <- round(currX)
  currY <- round(currY)
  action <- round((theta + 2 * pi) / (pi/2)) %% 4
  sum(
    apply(value[value$action==action,],1, function(s){
      x <- as.numeric(s[1])
      y <- as.numeric(s[2])
      type <- s[3]
      stateVal <- as.numeric(s[5])
      
      activation <- getActivation(currX, currY, x, y, type)
      
      stateVal * activation
    })
  )
}

getQLVals <- function(robot, posActions, value){
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