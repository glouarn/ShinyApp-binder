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

# sous echantillon de colonnes du tableau initial
#subx <- dat2[,c(5:29, 270:275, 278, 281:291, 293,50,51,55,57,59,61,63,65,67,69,71,73,75,77,125,126,145,205,130:132 )]
#names(subx)
#write.table(subx, "dtoto_reduced.csv", sep=";", row.names = F)
