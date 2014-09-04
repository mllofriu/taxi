
alpha <- .8
gamma <- .95
goalReward <- 2
nonGoalReward <- 0

initValue <- function(dimx, dimy, numActions, world){
  array(0, dim=c(dimx, dimy, numActions))
}

stateV <- function(x, y, action, value) {
  # Round x, y and the action (the action in pi/2 intervals)
  value[round(x) + 1, round(y) + 1, action + 1]
}

getQLVals <- function(robot, posActions, value){
  # Get the value for each action
  stateV(robot$x, robot$y, posActions, value)
}

getQLActionVals <- getQLVals

update <- function(preRobot, posRobot, action, value, reward){
  val <- stateV(preRobot$x, preRobot$y, action, value)
  value[round(preRobot$x) + 1, round(preRobot$y) + 1, action + 1] <-
    val +
    alpha * (reward + gamma * max(value[round(posRobot$x) + 1, round(posRobot$y) + 1, 1:4]) - val)
  
  value
}

reward <- function(postRobot, goal, eps){
  # If in the goal
  if (dist(postRobot, goal) < eps)
    goalReward
  else 
    nonGoalReward
}

getMethod <- function(){
  "ql"
}