library(dplyr)
library(magrittr)
library(ggplot2)
library(purrr)
library(leaps)

read.csv("dataset/crimes.csv") -> data
colnames(data) -> names

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





