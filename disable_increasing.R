setwd("/Users/cpprhtn/Desktop")
library(tidyr)
library(reshape2)
library(dplyr)
read.csv("disable.csv",header = T,fileEncoding = "CP949") -> dis
dis[3,] -> new_dis
new_dis
new_dis2<- melt(new_dis, id.vars = c("시도별.1.","장애유형별.1."),variable.name = "Date")
new_dis2
new_dis2$year =substr(new_dis2$Date,2,5)

dis_final
str(dis_final)
dis_final$year = as.numeric(dis_final$year)
dis_final$value = as.numeric(dis_final$value)
#선형모델 제작
fit <- lm(dis_final$value ~ dis_final$year)
fit
#시각화
plot(dis_final$year,dis_final$value,col="red",type = "b", ylim = c(1000000,3000000),
     main = "Disable increasing trend", xlim = c(2000,2020),
     xlab = "Year",ylab = "Increasing")
abline(fit, col = "blue")
summary(fit)
#library(ggplot2)
#ggplot(dis_final,aes(x=year,y=value)) + geom_bar(stat = "identity", fill = "light blue", colour = "black") +
#  ggtitle("Disable increasing trend")

#ggplot(dis_final,aes(x=year,y=value)) + geom_line(colour = "red") + geom_point() +
#  ggtitle("Disable increasing trend")