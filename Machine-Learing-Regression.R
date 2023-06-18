library(caTools)
library(ggplot2)

bike <- read.csv('bikeshare.csv')

plot <- ggplot(bike,aes(temp,count)) + geom_point(alpha=0.1)

bike$datetime <- as.POSIXct(bike$datetime)

plot2 <- ggplot(bike,aes(datetime,count)) +geom_point(alpha=0.5, aes(color=temp))

plot2 <- plot2 + scale_color_continuous(low='lightgreen', high='lightpink')

cor(bike[,c('temp','count')])

box_plot <- ggplot(bike, aes(factor(season),count)) + geom_boxplot(aes(color = factor(season)))

bike$hour <- sapply(bike$datetime, function(x){format(x,"%H")})
bike$hour <- sapply(bike$hour,as.numeric)

countVhour <- ggplot(filter(bike,workingday == 1),aes(hour,count)) 
countVhour <- countVhour + geom_point(position=position_jitter(w=1, h=0),aes(color=temp)) 
countVhour <- countVhour + scale_color_gradientn(colours = c('darkblue', 'blue', 'lightblue', 'lightgreen', 'yellow', 'orange','red' ))

print(countVhour + theme_bw())

sample

temp.model <- lm(count ~ . -casual - registered - datetime - atemp, bike)

print(summary(temp.model))


