
alpha <- .8
gamma <- .95
goalReward <- 2
nonGoalReward <- 0

initValue <- function(dimx, dimy, numActions){
  array(0, dim=c(dimx, dimy, numActions))
}

stateV <- function(x, y, theta, value) {
  value[round(x) + 1, round(y) + 1, round((theta) / (pi/2) %% 4)]
}

qlVals <- function(robot, posActions, value){
  # Get the value for each action
  sapply(posActions, function (x) { 
    # Round x, y and the action (the action in pi/2 intervals)
    stateV(robot$x, robot$y, x, value)
  })
}

update <- function(preRobot, posRobot, action, value, reward){
  val <- stateV(preRobot$x, preRobot$y, action, value)
  value[round(preRobot$x) + 1, round(preRobot$y) + 1, round((action) / (pi/2) %% 4)] <-
    val +
    alpha * (reward + gamma * max(value[round(posRobot$x) + 1, round(posRobot$y) + 1, 1:4]) - val)
  value
}

reward <- function(postRobot, goal){
  # If in the goal
  if (all(c(postRobot$x, postRobot$y) - c(goal$x, goal$y) < eps))
    goalReward
  else 
    nonGoalReward
}