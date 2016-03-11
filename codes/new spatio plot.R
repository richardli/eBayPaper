library(ggplot2)
library(MASS)

## generate some random data
# data <- data.frame(a=simall[, 1], b=simall[, 2])
# truth <- data.frame(a = testing.new$Time, b = testing.new$Price)
data <- data.frame(a = c(simall[,1], testing.new$Time[testing.new$Time > 6.5]), b = c(simall[, 2], testing.new$Price[testing.new$Time > 6.5]), c = c(rep(0, dim(simall)[1]), rep(1, length(testing.new$Time[testing.new$Time > 6.5]))))
## layout settings for ggplot
t2 <- theme(
    axis.line = element_line(colour = "black"),
    axis.text = element_text(colour = "black"),
    axis.ticks = element_line(colour = "black"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.border = element_blank(),
    panel.background = element_blank()
)


## generate the "z" coordinate (density) just for the correct midpoint in the color gradient
z <- kde2d(data$a, data$b)

g <- ggplot(data, aes(x=a, y=b)) +
  stat_density2d(aes(fill=..density..), geom="tile", contour=FALSE, data = subset(data, c==0)) +
  scale_fill_gradient2(low="#44aa00", mid="#ffcc00", high="#502d16", midpoint=mean(range(z$z))) +
  ## limit scale ( density is calculated on the limited scale, all other points are removed from calculation)
  #xlim(0,1) +
  #ylim(0,1) +
  ## limit view area ( density is calcluated on all points, no points removed )
  coord_cartesian(xlim = range(data$a), ylim=range(data$b)) +
  xlab("x method") +
  ylab("y method") +
  ## add points to the density map (comment it if not desired)
  geom_point(size=5, colour="red",shape= 20,  data = subset(data, c == 1)) +
  ## make a line from (0,0) - (1,1)
  #geom_segment(aes(x=0, y=0, xend=1, yend=1)) +
  ## or just create a abline with slope 1 and intercept 0
  geom_abline() +
  t2

pdf("density.pdf", width=0.25*6+6, height=6)
g
dev.off()
