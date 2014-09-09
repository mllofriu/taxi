getExplorationVals <- function(posActions) {
  expVals <- rep(0, length(posActions))
#   if (any((robot$theta)-(posActions*pi/2) %% (2*pi)< world$eps)){
#     forward <- (robot$theta)-(posActions*pi/2) %% (2*pi)< world$eps
#     prob <- rep((1-forwardExplorationProb)/(length(posActions)-1),length(posActions))
#     prob[forward] <- forwardExplorationProb
#     randomAction <- sample(1:length(posActions), 1,prob=prob)
#   } else {
#     randomAction <- sample(1:length(posActions),1)
#   }
  randomAction <- sample(1:length(posActions),1)
  expVals[randomAction] <- expVals[randomAction] + explorationVal

  expVals
}