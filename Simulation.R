library(tidyr,dplyr)

means.county <- rep(rnorm(1,0,1),58)
num.county <- 1:58
u.level.county <- rep(rnorm(1,0,1),58)
county.table <- cbind(num.county,means.county,u.level.county)%>%as.data.frame()

mean.floor <- c(2,1)
sd.floor = 1

sd.county = 2
sd.house = 1

house.num <- 1:1673
house.county <- sample(x = num.county,replace = TRUE,size = length(house.num))
house.floor <- rbinom(n =length(house.num),size = 1,prob = .5 )
house.table <- cbind(house.num,house.county,house.floor)%>%as.data.frame



