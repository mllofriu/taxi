
library('doParallel')
library('plyr')
library('ggplot2')

source('graphics.R')
source('movement.R')
source('taxic.R')
source('genericql.R')
source('ql.R')
source('msql.R')
# source('msac.R')
source('exploration.R')
source('world.R')

args <- commandArgs(trailingOnly = TRUE)
if (length(args)==0) {
  procnum = 1
} else {
  procnum = args[1] 
}

# showPlots <- TRUE
showPlots <- FALSE

numTrials <- 2
numEpisodes <- 1

explorationVal <- 25
forwardExplorationProb <- .3

world <- initWorld(TRUE)


rte <- foreach (method=c('ql', 'msql')) %do%{
  foreach (trial=1:numTrials) %do%{
    # Init ql value
    if (method == 'msac')
      rlData <- msac(world$xDim, world$yDim, 4, 4, world)
    else if (method == 'ql')
      rlData <- ql(world$xDim, world$yDim, 4, 4)
    else if (method == 'msql')
      rlData <- msql(world$xDim, world$yDim, 4, 4, world)
    
    foreach (episode=1:numEpisodes, .combine=rbind) %do% {
      print(paste("Method", method, "Trial", trial, "Episode", episode))
      #     for(episode in 1:numEpisodes){
      steps <- 0
      # Choose goal random
      #       goal <- sample(1:4, 1)
      goal <- 1
      cat ("Going to goal", as.character(world$places[goal,'label']), "\n")
      
      goalLocation <- world$places[goal,c('x','y')]
      robot <- data.frame(x=10,y=10,theta=pi/2)
      # While the robot has not reach the goal
      while (!((dist(rbind(robot[c('x','y')],goalLocation[c('x','y')]))< world$eps ) || 
               steps > 10000)){
        # Draw the world
        if (showPlots && episode == 1){
          #         visible(robot, goal, world$walls, world$eps) ||
          #           if ( 
          #             all(robot == data.frame(x=10,y=10,theta=pi/2))){
          if(steps %% 500 == 0){
            png('snapshot.png')
            draw(robot, goal, world, rlData)
            dev.off()
          }
          #           }
        }
        #      
        # if (showPlots)
        #   print(round(robot))
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

save (rte, file=paste("rte-", procnum, ".Rdata", sep = ""))



# qlRTSum <- ddply(qlRT, .(episode), summarise, meanSteps = mean(steps))
# save(qlRTSum, qlRT, file='mlruntimes.Rdata')
# qplot(episode, meanSteps, data=qlRTSum)
#rte[rte$episode>=90 & rte$steps > 1000 & rte$method == 'Multi-Scale QL',]

# rteSum <- ddply(rte, .(episode, method), summarise, meanSteps = mean(steps))
# rte.aov <- aov(steps ~ factor(episode):method, data=rte)
# tuk <- TukeyHSD(rte.aov)
# index <- foreach (i=1:numEpisodes) %do% paste(i,":Multi-Scale QL-",i,":Normal QL",sep="")
# ps <- tuk$"factor(episode):method"[unlist(index),'p adj']
# ps < 0.05
# names(rteSum)[names(rteSum) == "method"] <- "Method"
# png('rte.png')
# #pdf('rte.pdf')
# qplot(episode, meanSteps, data=rteSum, geom=c('point', 'line'), color=Method) +
#   ylab("Num. of Steps") + xlab("Episode") + 
#   theme(legend.text = element_text(size=16), legend.title = element_text(size=16), text = element_text(size=16)) +
#   theme(legend.position = c(1, 1), 
#         legend.justification = c(1, 1),
#         legend.background = element_rect(colour = NA, fill = NA))
# dev.off()

