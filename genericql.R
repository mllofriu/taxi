getActionVals <- function(rlData, ...){
  UseMethod("getActionVals", rlData)
}

getStateValue <- function(rlData, ...){
  UseMethod("getStateValue", rlData)
}

update <- function(rlData, ...){
  UseMethod("update", rlData)
}

reward <- function(rlData, ...){
  UseMethod("reward", rlData)
}

getMethod <- function(rlData, ...){
  UseMethod("getMethod", rlData)
}