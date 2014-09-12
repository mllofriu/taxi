library('sp')
library('rgeos')

library('doParallel')
library('plyr')

source('graphics.R')
source('movement.R')
source('taxic.R')
source('genericql.R')
source('ql.R')
source('msql.R')
# source('msac.R')
source('exploration.R')
source('world.R')

# showPlots <- TRUE
showPlots <- FALSE

numTrials <- 25
numEpisodes <- 30

explorationVal <- 5
forwardExplorationProb <- .3

world <- initWorld()

# Plot opt.
if (showPlots)
  quartz("taxi", 5, 5)

# For each episode
runtimes <- expand.grid(trial=1:numTrials, episode=1:numEpisodes)

if (!showPlots){
  cl <- makeCluster(detectCores())
  registerDoParallel(cl)
}


rte <- foreach (method=c('msql','ql'), .combine=rbind) %do% {
# for (method in c('msql','ql')){
  foreach (trial=1:numTrials,.verbose=T, .packages=c('foreach','sp','rgeos','plotrix','plyr', 'ggplot2'), .combine=rbind, .export=c(as.vector(lsf.str()))) %dopar%{
#   for (trial in 1:numTrials)  {
    # Init ql value
    if (method == 'msac')
      rlData <- msac(world$xDim, world$yDim, 4, 4, world)
    else if (method == 'ql')
      rlData <- ql(world$xDim, world$yDim, 4, 4)
    else if (method == 'msql')
      rlData <- msql(world$xDim, world$yDim, 4, 4, world)

    foreach (episode=1:numEpisodes, .combine=rbind) %do% {
#     for(episode in 1:numEpisodes){
      steps <- 0
      # Choose goal random
      goal <- sample(1:4, 1)
#             goal <- 3
#       goal <- 4
      cat ("Going to goal", as.character(world$places[goal,'label']), "\n")

      goalLocation <- world$places[goal,c('x','y')]
      robot <- data.frame(x=4,y=4,theta=-pi/2)
      # While the robot has not reach the goal
      while (!((dist(rbind(robot[c('x','y')],goalLocation[c('x','y')]))< world$eps ) || 
                 steps > 1000)){
        # Draw the world
        if (showPlots && episode > 0){
          #         visible(robot, goal, world$walls, world$eps) ||
#           if ( 
#             all(robot == data.frame(x=9,y=0,theta=pi/2))){
            if(steps %% 50 == 0)
              draw(robot, goal, world, rlData)
#           }
        }
        #       
        print(robot)
        # Get affordances
        posActions <- possibleActions(robot, world)
        # Get taxic values
        tVals <- taxicVals(robot, goalLocation, posActions,world$eps)

        # Get QL values
        # Only get action values from the small ones
        qlVals <- getActionVals(rlData,robot, goal, posActions)
#         print(qlVals)
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
#         qlValsAfter <- getQLActionVals(postRobot, goal, postPosActions, value)
#         cat('tvals ', tValBefore, tValAfter, '\n')
#         print(sum(0+qlValsAfter))
        rlData <- update(rlData, robot, postRobot, goal, action, r, 0,0)
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

