library('sp')
library('rgeos')

library('doParallel')
library('plyr')

source('graphics.R')
source('movement.R')
source('taxic.R')
source('genericql.R')
source('ql.R')
# source('msql.R')
source('msac.R')
source('exploration.R')
source('world.R')

showPlots <- FALSE

numTrials <- 2
numEpisodes <- 2

explorationVal <- 5
forwardExplorationProb <- .3

world <- initWorld()

# Plot opt.
quartz("taxi", 5, 5)

# For each episode
runtimes <- expand.grid(trial=1:numTrials, episode=1:numEpisodes)

if (!showPlots){
  cl <- makeCluster(detectCores())
  registerDoParallel(cl)
}


rte <- foreach (method=c('ql','msac'), .combine=rbind) %do% {
# for (method in c('ql','msac')){
  foreach (trial=1:numTrials, .packages=c('sp','rgeos', 'ggplot2'), .combine=rbind, .export=c('%do%', 'foreach')) %dopar%{
#   for (trial in 1:numTrials)  {
    # Init ql value
    if (method == 'msac')
      rlData <- msac(world$xDim, world$yDim, 4, 4, world)
    else if (method == 'ql')
      rlData <- ql(world$xDim, world$yDim, 4, 4)

    foreach (episode=1:numEpisodes, .combine=rbind) %do% {
#     for(episode in 1:numEpisodes){
      source('genericql.R')
      source('ql.R')
      # source('msql.R')
      source('msac.R')
      steps <- 0
      # Choose goal random
      goal <- sample(1:4, 1)
#       goal <- 3
#       goal <- 4
      goalLocation <- world$places[goal,c('x','y')]
      robot <- data.frame(x=9,y=0,theta=pi)
      # While the robot has not reach the goal
      while (!(dist(rbind(robot[c('x','y')],goalLocation[c('x','y')]))< world$eps)){
        # Draw the world
        if (showPlots){
          #         visible(robot, goal, world$walls, world$eps) ||
#           if ( 
#             all(robot == data.frame(x=9,y=0,theta=pi/2))){
            if(steps %% 100 == 0)
              draw(robot, goal, world, rlData)
#           }
        }
        #       
#         print(robot)
        # Get affordances
        posActions <- possibleActions(robot, world)
        # Get taxic values
        if (method != 'ql')
          tVals <- taxicVals(robot,posActions, world$places, world$eps)
        else 
          tVals <- 0
        # Get QL values
        # Only get action values from the small ones
        qlVals <- getActionVals(rlData,robot, goal, posActions)
        # Exploration vals
        expVals <- getExplorationVals(posActions)
        # Get total values as the sum
        actionVals <- tVals + qlVals + expVals
        #       actionVals <- qlVals
        # Select maximum value action
        action <- posActions[match(max(actionVals), actionVals)]
        # Move the robot according to picked action
        postRobot <- move(robot, action)
        # Compute Ql reward
#         print(goal)
        r <- reward(rlData, postRobot, world$places[goal,c('x','y')], world$eps)
        # Update Ql Value
#         tValBefore <- tVals[match(max(actionVals), actionVals)]
        tValBefore <- max(tVals)
        postPosActions <- possibleActions(postRobot, world)
        tValAfter <- max(taxicVals(postRobot,postPosActions, world$places, world$eps))
#         qlValsAfter <- getQLActionVals(postRobot, goal, postPosActions, value)
#         cat('tvals ', tValBefore, tValAfter, '\n')
#         print(sum(0+qlValsAfter))
        rlData <- update(rlData, robot, postRobot, goal, action, r, tValBefore,tValAfter)
        # Update robot
        robot <- postRobot
        # Increase step count
        steps <- steps + 1
#               Sys.sleep(1)
      }
      
      data.frame(trial=trial, episode=episode, steps=steps, method=getMethod(rlData))  
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
pdf('rte.pdf')
qplot(episode, meanSteps, data=rteSum, geom=c('point', 'line'), color=Method) +
  ylab("Num. of Steps") + xlab("Episode") + 
  theme(legend.text = element_text(size=16), legend.title = element_text(size=16))
dev.off()
r

