setwd("/Users/cpprhtn/Desktop")
library(tidyr)
library(reshape2)
library(dplyr)
read.csv("disable_type.csv",header = T,fileEncoding = "CP949") -> new_type
head(new_type)
new_type2 <- new_type[c(2),]
head(new_type2)
new_type3<- melt(new_type2, id.vars = c("장애유형별.1."))
head(new_type3)
new_type3$year =substr(new_type3$variable,2,5)
new_final <- new_type3[,c(4,3)]
new_final

#2005년에만 있는 blank오류 제거
new_data <- new_final[c(1:10,12:34),]
View(new_data)
str(new_data)
new_data[i,1]
#형변환
new_data$year = as.numeric(new_data$year)
new_data$value = as.numeric(new_data$value)
new_data
#변수 설정
innate = 0
childbirth = 0
acquired = 0
unknown = 0
for(i in 1:33){
  #if(isTRUE(new_data[i,1]<= 2000)){
  print(i)
  if(i<=10){
    if(i%%4==1){
      innate = innate+new_data[i,2]
    }
    else if(i%%4==2){
      childbirth = childbirth+new_data[i,2]
    }
    else if(i%%4==3){
      acquired = acquired+new_data[i,2]
    }
    else{
      unknown = unknown+new_data[i,2]
    }
  }
  else{
    if(i%%5==1){
      acquired = acquired+new_data[i,2]
    }
    else if(i%%5==2){
      acquired = acquired+new_data[i,2]
    }
    else if(i%%5==3){
      unknown = unknown+new_data[i,2]
    }
    else if(i%%5==4){
      innate = innate+new_data[i,2]
    }
    else{
      childbirth = childbirth+new_data[i,2]
    }
  }
  print(innate)
  print(childbirth)
  print(acquired)
  print(unknown)
  print("*****")
}
c(innate,childbirth,acquired,unknown) -> x
c(1:4) -> y
barplot(x,y,main="Types of Disability in the Person with Disabilities(1995~2017)",
        xlab="innate(31.1)               childbirth(9)               acquired(624)               unknown(35.8)",
        ylab="Total data for seven years",
        sub = "The sum of the four types in a year is 100%",
        ylim = c(0,700),col.main="dark green",col="light blue")
        
