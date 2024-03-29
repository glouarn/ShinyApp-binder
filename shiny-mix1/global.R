library(shiny)
library("shinyWidgets")
library(shinythemes)
#library("here")
#library("corrplot")
library(readxl)


#path_ <- file.choose()
#"C:\\devel\\shiny\\ShinyApp-binder\\shiny-mix1\\tabmoys.csv"


#tabmoys_m2 <- read.table("tabmoys.csv", header=T, sep=",", dec=".")
#tabmoys_m2 <- read.table(path_, header=T, sep=",", dec=".")
# tabmoy local pas complet!!


#tabmoys_m2 <- read.csv("https://onedrive.live.com/download?cid=C31CBDE465CD1370&resid=C31CBDE465CD1370%214082&authkey=AN-6wl3mf7PEeGM")#'tabmoys_merge2-7.csv'
#file.path("C:\\devel\\shiny\\ShinyApp-binder\\shiny-mix1", "tabmoys_merge2-7.csv")
#tabmoys_m2 <- read.csv(file.path("C:\\devel\\shiny\\ShinyApp-binder\\shiny-mix1", "tabmoys_merge2-7.csv"))#'tabmoys_merge2-7.csv'
#tabmoys_m2 <- read.csv(file.path("C:\\devel\\shiny\\ShinyApp-binder\\shiny-mix1", "tabmoys_merge2-7_CESE.csv"))#'tabmoys_merge2-7.csv'
tabmoys_m2 <- read.csv("tabmoys_merge2-7_CESE.csv")#'tabmoys_merge2-7.csv'



spmn <- split(tabmoys_m2, tabmoys_m2$Mng) #decoupe par niveau d'N
ls_tabmoys <- vector("list", length(spmn))
names(ls_tabmoys) <- names(spmn)

for (traitN in names(spmn))
{
  spmn[[traitN]]$keysc <- as.factor(as.character(paste(spmn[[traitN]]$mix , spmn[[traitN]]$sc)))#spmn[[traitN]]$keysc)) #! enelve Mng de la cle!!
  ls_tabmoys[[traitN]] <- split(spmn[[traitN]], spmn[[traitN]]$keysc)
}

length(ls_tabmoys[[1]])
names(ls_tabmoys) <- c('0N', '300N', '120N')#c('0N',  '120N')#

ls_keysc <- names(ls_tabmoys[[1]])#clesscenario sans doublon management
ls_mng <- c('0N', '120N', '300N')


####
#import des valeurs de parametres et calcul de delta dans dparams 



mix <- "Fix2-nonFixSimTest"
params <- vector("list", 2)
names(params) <- strsplit(mix, '-')[[1]]

#path_param <- "C:/devel/l-egume/legume/input/liste_scenarios.xls"
#path_param <- file.path("C:\\devel\\shiny\\ShinyApp-binder\\shiny-mix1", "liste_scenarios.xls")
path_param <-  "liste_scenarios.xls"

params[[1]] <- as.data.frame(read_excel(path_param, sheet = names(params)[1]))
params[[2]] <- as.data.frame(read_excel(path_param, sheet = names(params)[2]))


#faire le lien entre les valeurs de difference de parametre et les scenaios

#liste unique des scenarios
dt <- as.data.frame(do.call("rbind", strsplit(ls_keysc, " ")))
lsc <- as.data.frame(do.call("rbind", strsplit(as.character((dt$V1)), "-")))
lsp <- as.data.frame(do.call("rbind", strsplit(as.character((dt$V2)), "-")))
dparams <- cbind(lsc, lsp)
names(dparams) <-c("esp2", "esp1","sc1", "sc2" )#c("sc1", "sc2", "esp2", "esp1")#verif si numero espece pas inverses?


#calcul des differences de valeurs de parametres
res <- NULL
for (i in 1:length(dparams$sc1))
{
  p1 <- params[[as.character(dparams$esp1[i])]][params[[as.character(dparams$esp1[i])]]$id_scenario == dparams$sc1[i] , c(2,3,4,5,6)]
  p2 <- params[[as.character(dparams$esp2[i])]][params[[as.character(dparams$esp2[i])]]$id_scenario == dparams$sc2[i] , c(2,3,4,5,6)]
  res <- rbind(res, p1[1,]-p2[1,])
}

dparams <- cbind(dparams, res)
dparams$keysc <- ls_keysc

resnorm <- res
names(resnorm) <- c("normq", "normLen", "normVmax2", "normRUE", "normMaxFix")
resnorm$normq[resnorm$normq>0] <- 1
resnorm$normq[resnorm$normq<0] <- -1
resnorm$normLen[resnorm$normLen>0] <- 1
resnorm$normLen[resnorm$normLen<0] <- -1
resnorm$normVmax2[resnorm$normVmax2>0] <- 1
resnorm$normVmax2[resnorm$normVmax2<0] <- -1
resnorm$normRUE[resnorm$normRUE=='0.2'] <- -0.5
resnorm$normRUE[resnorm$normRUE=='0.6'] <- -1
resnorm$normMaxFix[resnorm$normMaxFix<0] <- 1#inverse

dparams <- cbind(dparams, resnorm)





# fonctions
CalcOpt <- function(modeltot , xx, yy)
{
  ## calculla proportion et la valeur max de l'overyielding
  pred <- predict(modeltot, seq(0,1,0.001))
  #xx <- tabmoy$Yprop1
  lintot <- lsfit(c(xx[1], xx[7]), c(yy[1], yy[7]))#c(tabmoy$Ytot[1], tabmoy$Ytot[7]))
  ylin <- lintot$coefficients[["Intercept"]] + seq(0,1,0.001)*lintot$coefficients[["X"]]
  
  diff_predlin <- pred$y - ylin
  
  idopt <- which(abs(diff_predlin ) == max(abs(diff_predlin )))
  propOpt <- pred$x[idopt]
  OverMax <- diff_predlin[idopt]
  
  #calcul du max de rendement absolu e de la prop correspodante
  Ytotmax <- max(pred$y)
  idmax <- which(pred$y == Ytotmax)[1] #le premier si plusieurs
  propMax <- pred$x[idmax]
  
  c(propOpt, OverMax, idopt, Ytotmax, propMax)
}


CalcPropactu50 <- function (modelesp1, modelesp2, idopt)
{
  #calcul prop a laquelle biomasse fait 50/50 (2 modeles se croisent) et prop debiomase a l'otimum d'overyielding
  pred1 <- predict(modelesp1, seq(0,1,0.001))
  pred2 <- predict(modelesp2, seq(0,1,0.001))
  delta <- abs(pred1$y-pred2$y)
  idmin <- which(delta == min(delta))
  propsowing50 <- pred1$x[idmin]
  propLegOtp <- pred1$y[idopt]/(pred1$y[idopt]+pred2$y[idopt])
  c(propLegOtp, propsowing50)
}


YtotvsProp <- function(tabmoy, Ymax=2200, nom="", optProp="sowing",visuplot=T, visutext=T, colv=c(1,2,4),...)
{
  ## calcul des composante de l'overyielding biomasse et fait un plot (visutext=visualisation des valeurs; visuplot=visulaisation des )
  
  
  #actual or sowing proportions?
  if (optProp=="sowing")
  {
    xx <- tabmoy$Semprop1
    labx <- 'Sowing proportion (Esp. 1)'
  }
  if (optProp=="actual")
  {
    xx <- tabmoy$Yprop1
    labx <- 'Actual proportion (Esp. 1)'
  }
  
  #calcul des fits des valeurs moyennes
  modeltot <- smooth.spline(xx, tabmoy$Ytot)
  inttot = sum(predict(modeltot, seq(0,1,0.001))$y*0.001) - (tabmoy$Ytot[1]+tabmoy$Ytot[7])/2
  
  modelesp1 <- smooth.spline(xx, tabmoy$YEsp1)
  intesp1 = sum(predict(modelesp1, seq(0,1,0.001))$y*0.001) - (tabmoy$YEsp1[1]+tabmoy$YEsp1[7])/2
  
  modelesp2 <- smooth.spline(xx, tabmoy$YEsp2)
  intesp2 = sum(predict(modelesp2, seq(0,1,0.001))$y*0.001) - (tabmoy$YEsp2[1]+tabmoy$YEsp2[7])/2
  
  #cacul des autres indices
  ids <- CalcOpt(modeltot , xx, tabmoy$Ytot)
  propOpt <- ids[1]
  OverMax <- ids[2]
  Ytotmax <- ids[4]
  propYtotmax <- ids[5]
  ids1 <- CalcPropactu50(modelesp1, modelesp2, ids[3])
  propsowing50 <- ids1[2]
  propLegOtp <- ids1[1]
  
  #plot des valeur moyennes Ytot si option activee
  if (visuplot==T)
  {
    plot(xx, tabmoy$Ytot, ylim=c(0,Ymax), xlab=labx, ylab='Total annual yield (g.m-2)', main=nom, col=colv[1], ...)
    #segments(tabmoy$Semprop1, tabmoy$Ytot, tabmoy$Semprop1, tabmoy$Ytot+tabmoy$Ytotsd)
    #segments(tabmoy$Semprop1, tabmoy$Ytot, tabmoy$Semprop1, tabmoy$Ytot-tabmoy$Ytotsd)
    segments(xx[1], tabmoy$Ytot[1], xx[7], tabmoy$Ytot[7], lty=2, col=colv[1])
    lines(modeltot, col=colv[1])
    
    points(xx, tabmoy$YEsp1,col=2)
    segments(xx[1], tabmoy$YEsp1[1], xx[7], tabmoy$YEsp1[7], lty=2, col=colv[2])
    lines(modelesp1, col=colv[2])
    
    points(xx, tabmoy$YEsp2,col=4)
    segments(xx[1], tabmoy$YEsp2[1], xx[7], tabmoy$YEsp2[7], lty=2, col=colv[3])
    lines(modelesp2, col=colv[3])
    
  }
  
  if (visutext==T & visuplot==T)
  {
    text(0.15, 0.97*Ymax, paste('overY: ' ,round(inttot,2)))
    text(0.15, 0.93*Ymax, paste('Esp1: ' , round(intesp1,2)),col=2)
    text(0.15,0.89*Ymax, paste('Esp2: ' ,round(intesp2,2)),col=4)
  }
  
  #renvoie valeurs calculees
  res <- as.list(c(inttot, intesp1, intesp2, propOpt, OverMax, propsowing50, propLegOtp, Ytotmax, propYtotmax))
  names(res) <- c("inttot", "intesp1", "intesp2", "propOpt", "OverMax", "propsowing50", "propLegOtp", "Ytotmax", "propYtotmax")
  res
  
}




OverYvsAll <- function(ls_tabmoys, key, Ymax=300, nom="", optProp="sowing", visuplot=T, colv=c("light grey","blue"),...)
{
  #key <- ls_keysc[20]
  #figure de tous les overyielding
  ls_keysc = names(ls_tabmoys)
  
  if (optProp=="sowing")
  { labx <- 'Sowing proportion (Esp. 1)'
    laby <- 'Overyieding (g.m-2)'
  }
  if (optProp=="actual")
  { labx <- 'Actual proportion (Esp. 1)'
    laby <- 'Actual Overyieding (g.m-2)'
  }
  
  if (visuplot==T)
  {
    plot(-100, -100, ylim=c(-Ymax,Ymax), xlim=c(0,1), main=nom, xlab=labx, ylab=laby, ...)
    segments(0, 0, 1, 0, col=1)
  }
  
  resx <- NULL
  resy <- NULL
  
  for (keysc in ls_keysc)
  {
    #keysc <- ls_keysc[3]
    tabmoy <- ls_tabmoys[[keysc]]
    
    #xx <- tabmoy$Semprop1#tabmoy$Yprop1#
    yy <- tabmoy$Ytot
    #actual or sowing proportions?
    if (optProp=="sowing")
    {
      xx <- tabmoy$Semprop1
      labx <- 'Sowing proportion (Esp. 1)'
    }
    if (optProp=="actual")
    {
      xx <- tabmoy$Yprop1
      labx <- 'Actual proportion (Esp. 1)'
    }
    
    lintot <- lsfit(c(xx[1], xx[7]), c(yy[1], yy[7]))
    ylin <- lintot$coefficients[["Intercept"]] + xx*lintot$coefficients[["X"]]
    overY <- yy - ylin
    
    if (keysc != key)
    {
      if (visuplot==T)
      { points(xx, overY, pch=16, col=colv[1]) }
      resx <- cbind(resx,xx)
      resy <- cbind(resy,overY)
    } else
    {
      savexx <- xx
      saveyy <- overY
    }
  }
  if (visuplot==T)
  { points(savexx, saveyy, pch=16, col=colv[2], type='l')
    points(savexx, saveyy, pch=16, col=colv[3])
    }
  resx <- cbind(resx,savexx)
  resy <- cbind(resy,saveyy)
  data.frame(x=as.numeric(resx), y=as.numeric(resy))
}





# figure colors
colbg <- "#333333" #NA
collab <- "#FFFFCC"#"white"



