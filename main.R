library(dplyr)
library(magrittr)
library(ggplot2)
library(purrr)
library(leaps)
set.seed(1)

read.csv("dataset/crimes.csv") -> data
select(data,-"name") -> dataWithoutName
colnames(data) -> names

numOfSamples = 15:nrow(dataWithoutName)

lapply(numOfSamples,function(x){
  dataWithoutName[sample(nrow(dataWithoutName),x),] -> s
  cov(data.matrix(s))  
}) -> covs

sapply(covs[-length(covs)],function(x,best=covs[[length(covs)]]){
  norm(best-x,type="2")
}) -> dist

data.frame(numOfSamples[-length(numOfSamples)],dist) -> distFrame
c("count","dist") -> colnames(distFrame)

lm("dist ~ count",distFrame) -> distModel
print(distModel$coefficients)
plot(distFrame$dist)
abline(a=distModel$coefficients[1],b=distModel$coefficients[2])

#Wählen Sie ein konkretes (einfaches) wahres Modell:
c(
  "crimes", #Zielgröße
  "wcon", # Wöchentlicher Lohn im Baugewerbe
  "density", # Bevölkerungsdichte
  "pctymale" # Anteil der jungen männlichen Bevölkerung
  ) -> simpleModelColumns

lm(
  "crimes ~ .",
  data = select(
    data,
    simpleModelColumns
    )
  ) -> simpleModel

#With leaps
leaps(
  data.matrix(select(data,-c(crimes,region,name))),
  data$crimes,
  method="Cp",
  nbest=5,
  strictly.compatible = FALSE
  ) -> leapsSelection

which(leapsSelection$Cp == min(leapsSelection$Cp)) -> bestLeapsModel

leapsSelection$label[leapsSelection$which[bestLeapsModel,]] -> bestLeapsLabels





lm(
  "crimes ~ .",
  data = select(
    data,
    c(bestLeapsLabels,'crimes')
  )
) -> bestLeapsModel



