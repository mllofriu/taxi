library('sp')
library('rgeos')

library('doParallel')
library('plyr')

source('graphics.R')
source('movement.R')
source('taxic.R')
#  source('ql.R')
source('msql.R')

showPlots <- TRUE

numTrials <- 1
numEpisodes <- 10

# World object
# Grid and robot size parameters
world.halfSquareSide <- .5
world.robotDiam <- world.halfSquareSide
# World dimension
world.xDim <- 10
world.yDim <- 10
# Epsilon for position comparison
world.eps <- 1e-10
# Interesting places
label=c('Y', 'R', 'B', 'G')
x <- c(0,0,6,9)
y <- c(0,9,0,9)
world.places=data.frame(x,y,label)
# Walls
world.walls <- Lines(list(
            Line(rbind(c(2,0) - world.halfSquareSide, c(2,4) - world.halfSquareSide)),
            Line(rbind(c(6,0) - world.halfSquareSide, c(6,4) - world.halfSquareSide)),
            Line(rbind(c(4,10) - world.halfSquareSide, c(4,6) - world.halfSquareSide)),
            Line(rbind(c(0,0) - world.halfSquareSide, c(0,10) - world.halfSquareSide)),
            Line(rbind(c(10,0) - world.halfSquareSide, c(10,10) - world.halfSquareSide)),
            Line(rbind(c(0,0) - world.halfSquareSide, c(10,0) - world.halfSquareSide)),
            Line(rbind(c(0,10) - world.halfSquareSide, c(10,10) - world.halfSquareSide))
            ),
            "walls")


# Plot opt.
quartz("taxi", 5, 5)

# For each episode
runtimes <- expand.grid(trial=1:numTrials, episode=1:numEpisodes)

if (!showPlots){
  cl <- makeCluster(detectCores())
  registerDoParallel(cl)
}


# rte <- foreach (method=c('multiscale','normal'), .combine=rbind) %do% {
for (method in c('multiscale')){
  if (method == 'normal')
    source('ql.R')
  else
    source('msql.R')
  
#   foreach (trial=1:numTrials, .packages=c('sp','rgeos', 'ggplot2'), .combine=rbind, .export=c('%do%', 'foreach')) %do%{
  for (trial in 1:numTrials)  {
    # Init ql value
    value <- initValue(world.xDim, world.yDim, 4, 4)
#     print(value)
#     foreach (episode=1:numEpisodes, .combine=rbind) %do% {
    for(episode in 1:numEpisodes){
        
      steps <- 0
      # Choose goal random
      goal <- sample(1:4, 1)
#       goal <- 3
#       goal <- 4
      robot <- data.frame(x=9,y=0,theta=pi)
      # While the robot has not reach the goal
      while (!(dist(robot,world.places[goal,c('x','y')])< world.eps)){
        # Draw the world
        if (showPlots){
          #         visible(robot, goal, world.walls, world.eps) ||
#           if ( 
#             all(robot == data.frame(x=9,y=0,theta=pi/2))){
            if(steps %% 100 == 0)
              print(system.time(draw(robot, goal, world, value)))
#           }
        }
        #       
#         print(robot)
        # Get affordances
        posActions <- possibleActions(robot, world)
        # Get taxic values
        tVals <- taxicVals(robot,posActions, world.places)
        # Get QL values
        # Only get action values from the small ones
        qlVals <- getQLActionVals(robot, goal, posActions, value)
        # Get total values as the sum
        actionVals <- tVals + qlVals
        #       actionVals <- qlVals
        # Select maximum value action
        action <- posActions[match(max(actionVals), actionVals)]
        # Move the robot according to picked action
        postRobot <- move(robot, action)
        # Compute Ql reward
#         print(goal)
        r <- reward(postRobot, world.places[goal,c('x','y')], world.eps)
        # Update Ql Value
        tValBefore <- tVals[match(max(actionVals), actionVals)]
        postPosActions <- possibleActions(postRobot, world)
        tValAfter <- max(taxicVals(postRobot,postPosActions, world.places))
        qlValsAfter <- getQLActionVals(postRobot, goal, postPosActions, value)
#         cat('tvals ', tValBefore, tValAfter, '\n')
        print(sum(0+qlValsAfter))
        value <- update(robot, postRobot, goal, action, value, r, tValBefore,max(0+qlValsAfter))
        # Update robot
        robot <- postRobot
        # Increase step count
        steps <- steps + 1
#               Sys.sleep(1)
      }
      
      data.frame(trial=trial, episode=episode, steps=steps, method=getMethod())  
    }
  }

}


# qlRTSum <- ddply(qlRT, .(episode), summarise, meanSteps = mean(steps))
# save(qlRTSum, qlRT, file='mlruntimes.Rdata')
# qplot(episode, meanSteps, data=qlRTSum)

rteSum <- ddply(rte, .(episode, method), summarise, meanSteps = mean(steps))
rte.aov <- aov(steps ~ factor(episode):method, data=rte)
tuk <- TukeyHSD(rte.aov)
save (rte, file="rte.Rdata")
names(rteSum)[names(rteSum) == "method"] <- "Method"
pdf('resultPrelim090314.pdf')
qplot(episode, meanSteps, data=rteSum, geom=c('point', 'line'), color=Method) +
  ylab("Num. of Steps") + xlab("Episode") + 
  theme(legend.text = element_text(size=16), legend.title = element_text(size=16))
dev.off()


