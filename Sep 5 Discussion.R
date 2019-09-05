library(arm)
pacman::p_load("learnr","foreign","knitr","plotly","arm","ggplot2")
setwd("~/Desktop/MSSP\ MA\ 678/MA678")
rents <- read.dta("rent99.dta")
ggplot(rents,mapping=aes(yearc,rent))+
  geom_point(alpha=0.5)+
  geom_smooth(method="lm")+
  theme_classic()

ggplot(rents,mapping=aes(area,rent))+
  geom_point(alpha=0.5)+
  geom_smooth()+
  theme_classic()

rents$location = as.factor(rents$location)

ggplot(rents,mapping=aes(location,rent))+
  geom_violin(fill="skyblue",alpha=0.5)+
  theme_classic()

ggplot(rents,mapping=aes(area,rent,color=location))+
  geom_point(alpha=0.5)+
  geom_smooth(method="lm")+
  theme_classic()

library(ggplot2)
rents_gp <- ggplot(rents)

## split it up by location
rents_gp + aes(x=area, y=rent, colour = factor(location))+ 
  geom_point()+ stat_smooth(method=lm,color="orange")+
  facet_grid( .~location, scales="fixed", labeller= label_both)

regout2 <- lm(rent ~ area, data=rents)   
summary(regout2) 
display(regout2)

regout <- lm(rent ~ yearc + factor(location) + area + district, data=rents) 
summary(regout) 

coefplot(regout)

plot(regout,which=1)

ggplot(rents, aes(area, rentsqm)) + geom_point(color="orange")+geom_smooth()

regoutPoly <- lm(rentsqm ~ area + I(area^2), data=rents)  
summary(regoutPoly)

plot( rents$area,rents$rentsqm) 
predx<-seq(range(rents$area)[1],range(rents$area)[2])
predregoutPoly<-predict(regoutPoly,list(area=predx))
lines(predx,predregoutPoly,col="red")

ggplot(rents, aes(area, rentsqm)) + 
  geom_point(color="orange")+geom_smooth()+
  scale_y_continuous(trans="log")

regoutPoly <- lm(rentsqm ~ I(area^2), data=rents)  
summary(regoutPoly)
