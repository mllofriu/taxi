load(file = '100trialsmlqlvsqlWith8NeighVentralDoingAction.Rdata')
load(file = 'bothRuntimes.Rdata')

require(ggplot2)
data <- rte
levels(data$method) <- c("Multi-Scale QL", "CanonicalQL")
p <- ggplot(data = data, aes(x=factor(episode), y = steps, fill = method)) 
p <- p+ geom_boxplot(notch = TRUE, outlier.shape = NA)
p <- p + ylab("Completion time (s)") + xlab("Episode") 
p <- p + theme_bw() + ylim(c(0,750))
p <- p + scale_x_discrete(breaks = c(1,5,10,15,20,25,30)) + scale_fill_discrete(name = "Method")
p <- p + theme(legend.text = element_text(size=15), legend.title = element_text(size=15), text = element_text(size=20)) 
p <- p + theme(legend.key.height = unit(3,"line"), legend.key.width = unit(3,"line"), legend.position = "right", legend.justification = c(1, 1), legend.background = element_rect(colour = NA, fill = NA))
p

ggsave(file = "runtimes.pdf")