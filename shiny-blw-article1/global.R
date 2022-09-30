library(shiny)
library("shinyWidgets")
library("here")
library("corrplot")



#path_ <- here("shiny-blw-article1")

#dtoto
dat2 <- read.table("tabletoto_corr.csv", header=T, sep=";", dec=".")

#moyenne
tabres <- read.table("tabre_moy.csv", header=T, sep=";", dec=".")

ls_traits <- unique(dat2$nomTrait)
ls_mng <- unique(dat2$MngN)
