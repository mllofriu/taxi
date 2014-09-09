
alpha <- .8
gamma <- 1
goalReward <- 100
nonGoalReward <- -5

ql <- function(dimx, dimy, numGoals, numActions){
  rlData$q <- expand.grid(x=0:(dimx-1), y=0:(dimy-1), goal=1:(numGoals), action=0:(numActions-1))
  rlData$q$value <- 0
  class(rlData) <- "ql"
  rlData
}

stateV.ql <- function(rlData, x, y, goal, action) {
  # Round x, y and the action (the action in pi/2 intervals)
  q <- rlData$q
  q[q$x==round(x) & q$y==round(y) & q$goal==goal & q$action==action, 'value']
}

getActionVals.ql <- function(rlData, robot, goal, posActions){
  # Get the value for each action
  sapply(posActions, function(action) stateV.ql(rlData, robot$x, robot$y, goal, action))
}

getStateValue.ql <- function(rlData, robot, goal){
  max(getActionVals(rlData, robot, goal, 0:3))
}


update.ql <- function(rlData, preRobot, posRobot, goal, action, reward, taxicBefore, taxicAfter){
  val <- stateV.ql(rlData, preRobot$x, preRobot$y, goal, action)
  maxValPost <- max(stateV.ql(rlData, posRobot$x, posRobot$y, goal, action))
  q <- rlData$q
  q[q$x==round(preRobot$x) & q$y==round(preRobot$y) & q$goal==goal & q$action==action, 'value'] <-
    val +
    alpha * (reward + gamma * maxValPost - val)
  
  rlData
}

reward.ql <- function(rlData, postRobot, goalPos, eps){
  # If in the goal
  if (dist(rbind(postRobot[c('x','y')], goalPos[c('x','y')])) < eps)
    goalReward
  else 
    nonGoalReward
}

getMethod.ql <- function(rlData){
  "Normal QL"
}