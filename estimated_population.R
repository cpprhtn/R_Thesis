setwd("/Users/cpprhtn/Desktop")
library(tidyr)
library(reshape2)
library(dplyr)
read.csv("expected population.csv",header = T,fileEncoding = "CP949") -> pop
pop2 <- melt(pop, id.vars = "시나리오별",variable.name = "year")
pop2$year =substr(pop2$year,2,5)

pp1 <- pop2[c(3:104),c(2,3)]
pp2 <- pop2[c(3:104),c(2,3)]
count = 1
for(i in 3:104){
  if(i%%2 == 1){
    pp1[count,] <- pop2[i,c(2,3)]
  }
  else{
    pp2[count,] <- pop2[i,c(2,3)]
    count=count+1
  }
}
new_pop <- pp1[c(1:51),]
new_rate <- pp2[c(1:51),]

plot(new_pop$year,new_pop$value)
str(new_pop)
new_pop$value <- as.numeric(new_pop$value)
new_pop$year <- as.factor(new_pop$year)
new_rate$value <- as.numeric(new_rate$value)
plot(type = 'l',new_pop$year,new_pop$value,xlab="Year", ylab="Population",main="Estimated population",
     sub="Childbirth Rate/Life-Middle/Non-International Migration Maintained in 2018",ylim=c(0,60000000))
plot(type = 'l',new_rate$year,new_rate$value,xlab="Year", ylab="rate",main="population growth rate")

#만약 인구증가율이 0.0일때, 정부에서 출산관련 정책을 크게 확대하여 조금씩 증가할 경우

for (j in 12:51){
  
  if(new_rate[j,2] < 0.5){
    new_rate[j,2] <- new_rate[j,2]*-1
  }
  else{
    new_rate[j,2] <- 0.5
  }
}

prediction <- new_pop
for(k in 2:51){
  prediction[k,2] <- prediction[k-1,2]*(new_rate[k,2]/100) + prediction[k-1,2]
}
str(prediction)
plot(prediction$year,prediction$value)
format(prediction$value,scientific = FALSE)
plot(prediction$year,prediction$value,ylim=c(0,73204678),main="Population growth forecast",xlab="Year", ylab="Population")



