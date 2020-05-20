setwd("/Users/cpprhtn/Desktop")
library(tidyr)
library(reshape2)
library(dplyr)
read.csv("disable_employment.csv",header = T,fileEncoding = "CP949") -> dis
dis
dis2<- melt(dis, id.vars = "구분",variable.name = "Date")
dis2$year =substr(dis2$Date,2,5)

new_dis <- dis2[,c(4,3)]
count = 1
for(i in 1:44){
  if(i%%4==2){
    new_dis[count,] <- new_dis[i,c(1,2)]
    count = count+1
  }
  if(i%%4==0){
    new_dis[count,] <- new_dis[i,c(1,2)]
    count = count+1
  }
}

dis_final <- new_dis[c(1:22),]

str(dis_final)
dis_final$year <- as.numeric(dis_final$year)
dis_final$value <- as.numeric(dis_final$value)

dis_Population <- dis_final
dis_employment_rate <- dis_final
num=1
for(j in 1:22){
  if(j%%2 == 1){
    dis_Population[num,] <- dis_final[j,]
  }
  else{
    dis_employment_rate[num,] <- dis_final[j,]
    num = num+1
  }
}

new_Population <- dis_Population[c(1:11),]
new_employment_rate <- dis_employment_rate[c(1:11),]

plot(new_Population$year,new_Population$value, type = "b", xlim = c(2008,2020))
fit <- lm(new_Population$value ~ new_Population$year)
fit
abline(fit, col = "blue")

plot(new_employment_rate$year,new_employment_rate$value, type = "b", xlim = c(2008,2020))
fit2 <- lm(new_employment_rate$value ~ new_employment_rate$year)
fit2
abline(fit2, col = "red")
