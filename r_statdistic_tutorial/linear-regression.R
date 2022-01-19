library(gridExtra)
library(e1071)
head(cars)

# draw a scatter plot
ggplot(data=cars, mapping=aes(x=speed, y=dist)) +
  geom_point(pch=21, size=2) +
  geom_smooth(colour="blue", se=FALSE) +
  labs(y='distance')

# Separate box plots for both speed and distance
par(mfrow=(c(1,2)))
boxplot(cars$speed, 
        main="Speed", 
        sub=paste("Outlier rows: ", 
                  boxplot.stats(cars$speed)$out))  # box plot for 'speed'
boxplot(cars$dist, 
        main="Distance", 
        sub=paste("Outlier rows: ", 
                  boxplot.stats(cars$dist)$out))  # box plot for 'distance'

# Density plots - trying to see the skewness of the graph
speed_plot <-ggplot(data=cars) +
  geom_density(aes(x=speed), color="darkblue", fill="lightblue") +
  geom_vline(aes(xintercept=mean(speed)),
             color="blue", linetype="dashed", size=1) +
  labs(x=paste('speed', round(e1071::skewness(cars$speed), 2)))
  
dist_plot <-ggplot(data=cars) +
  geom_density(aes(x=dist), color="darkblue", fill="lightblue") +
  geom_vline(aes(xintercept=mean(dist)),
             color="blue", linetype="dashed", size=1) +
  labs(x=paste('distance', round(e1071::skewness(cars$dist), 2)))

grid.arrange(speed_plot, dist_plot, ncol=2)

# Find the correlation between the independent and dependent variable
cor(cars$speed, cars$dist) # ~ 0.8 indicates a good positive correlation

# build the linear model
linear_mod <- lm(dist ~ speed, data=cars)
print(linear_mod)
summary(linear_mod)
  