library('sp')
library('rgeos')

library('doParallel')

source('graphics.R')
source('movement.R')
source('taxic.R')
source('ql.R')
#source('msql.R')



numTrials <- 20
numEpisodes <- 20

# World object
# Grid and robot size parameters
world.halfSquareSide <- .5
world.robotDiam <- world.halfSquareSide
# World dimension
world.xDim <- 5
world.yDim <- 5
# Epsilon for position comparison
world.eps <- 1e-10
# Interesting places
# x=c(0, 0, 3, 4)
# y=c(0, 4, 0, 4)
# label=c('Y', 'R', 'B', 'G')
x <- c(0)
y <- c(4)
label <- c('G')
world.places=data.frame(x,y,label)
# Walls
world.walls <- Lines(list(
            Line(rbind(c(1,0) - world.halfSquareSide, c(1,2) - world.halfSquareSide)),
            Line(rbind(c(3,0) - world.halfSquareSide, c(3,2) - world.halfSquareSide)),
            Line(rbind(c(2,5) - world.halfSquareSide, c(2,3) - world.halfSquareSide)),
            Line(rbind(c(0,0) - world.halfSquareSide, c(0,5) - world.halfSquareSide)),
            Line(rbind(c(5,0) - world.halfSquareSide, c(5,5) - world.halfSquareSide)),
            Line(rbind(c(0,0) - world.halfSquareSide, c(5,0) - world.halfSquareSide)),
            Line(rbind(c(0,5) - world.halfSquareSide, c(5,5) - world.halfSquareSide))
            ),
            "walls")

# Set a goal - TODO: pick from places
goal <- data.frame(x=0, y=4)
# Plot opt.
# saveBasePlot(world)
# quartz("Maze", 5, 5, antialias = T)
# For each episode
runtimes <- expand.grid(trial=1:numTrials, episode=1:numEpisodes)

cl <- makeCluster(detectCores())
registerDoParallel(cl)
qlRT <- foreach (trial=1:numTrials, .packages=c('sp','rgeos', 'ggplot2'), .combine=rbind) %dopar%{
  # Init ql value

  value <- initValue(world.xDim, world.yDim, 4)
  for (episode in 1:numEpisodes){
    steps <- 0
    robot <- data.frame(x=4,y=0,theta=pi/2)
    # While the robot has not reach the goal
    while (!(dist(robot,goal)< world.eps)){
      # Draw the world
#       if (visible(robot, goal, world.walls, world.eps) || 
#             all(robot == data.frame(x=4,y=0,theta=pi/2)))
#         print(draw(robot, world,value),newpage=F)
#       print(robot)
      # Get affordances
      posActions <- possibleActions(robot, world)
      # Get taxic values
      tVals <- taxicVals(robot,posActions, world, goal)
      # Get QL values
      # Only get action values from the small ones
      qlVals <- getQLActionVals(robot, posActions, value)
      # Get total values as the sum
      actionVals <- tVals + qlVals
      # Select maximum value action
      action <- posActions[match(max(actionVals), actionVals)]
      # Move the robot according to picked action
      postRobot <- move(robot, action)
      # Compute Ql reward
      r <- reward(postRobot, goal, world.eps)
      # Update Ql Value
      value <- update(robot, postRobot, action, value, r)
      # Update robot
      robot <- postRobot
      # Increase step count
      steps <- steps + 1
    }
    runtimes[runtimes$trial == trial & runtimes$episode == episode, "steps"] <- steps
#     print(runtimes)
  }
  cat ("Trial ", trial, " finished\n")
  runtimes[!is.na(runtimes$steps),]
}

qlRTSum <- ddply(qlRT, .(episode), summarise, meanSteps = mean(steps))
save(qlRTSum, qlRT, file='runtimes.Rdata')
qplot(episode, meanSteps, data=qlRTSum)
