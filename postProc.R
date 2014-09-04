rtsSum <- ddply(rts, .(episode, method), summarise, meanSteps = mean(steps))
rts.aov <- aov(steps ~ factor(episode):method, data=rts)
tuk <- TukeyHSD(rts.aov)