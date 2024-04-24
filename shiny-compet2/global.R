library(shiny)
library("shinyWidgets")
library("corrplot")
library(imager)


#path_ <- here("shiny-blw-article1")
#"C:\\devel\\shiny\\ShinyApp-binder\\shiny-compet2\\dtoto_competAll_1v1NoVarbis_OY.csv"
# f1 <- file.choose()
# setwd(dirname(f1))

#dtoto
dat1 <- read.table("competAll_1v1NoVarbis.csv", header=T, sep=";", dec=".") #non fix
dat2 <- read.table("competAll_1v1NoVarAsso.csv", header=T, sep=";", dec=".") #asso
dat3 <- read.table("competAll_1v1NoVarLeg.csv", header=T, sep=";", dec=".") #leg

#dat1 <- read.table("competAll_1v1Varbis.csv", header=T, sep=";", dec=".") #non fix
#dat2 <- read.table("competAll_1v1VarAsso.csv", header=T, sep=";", dec=".") #asso
#dat3 <- read.table("competAll_1v1VarLeg.csv", header=T, sep=";", dec=".") #leg
#manque simul avec le 46 pour var

ls_cases <- c("NonFix-NonFix", "NonFix-Fix", "Fix-Fix")
ls_env <- c("0N- Shallow", "0N-Deep", "100N- Shallow", "100N-Deep","300N- Shallow", "300N-Deep")#sort(unique(dat1$EnvirOK))

ls_dat <- vector("list", length=3)
names(ls_dat) <- ls_cases

ls_dat[["NonFix-NonFix"]] <- dat1
ls_dat[["NonFix-Fix"]] <- dat2
ls_dat[["Fix-Fix"]] <- dat3



im19 <- load.image("19-spe.png")
im19fix <- load.image("19-spe-fix.png")
im6env <- load.image("6_envs.png")

posiSp <- read.table("tab_posiSp.csv", header=T, sep=";", dec=".")


ordre_croi_ref <- c("46","45","44","38","39","40","55","1","54","53","47","50","48","41","49","42","51","43","52")
ordre_croi_ref_fix <- c("46f","45f","39f","40f","44f","38f","55f","54f","53f","1f","47f","48f","49f","41f","50f","42f","51f","43f","52f")

ls_spAll <- c(sort(ordre_croi_ref), sort(ordre_croi_ref_fix))


#ls_scenar <- sort(unique(dat1$scenario))#sort(unique(c(dat1$scenario,dat2$scenario,dat3$scenario)))#
#pas liste a jour / complete -> revoir




#mef par esp avec ajout score
ls_res <- vector("list", length=3)
j <- 1

for (dati in list(dat1,dat2,dat3))
{

  #dat <- dat3
  ls_sp <- sort(unique(dati$scenario2))
  
  res <- vector("list", length=length(ls_sp))
  names(res) <- as.character(ls_sp)
  
  
  for (sp in ls_sp)
  {
    #sp <- 1
    
    x <- dati[dati$scenario1 == sp | dati$scenario2 == sp,]
    
    #calculs
    v_p50s <- NULL
    v_nom <- NULL
    v_points <- NULL
    v_adv <- NULL
    for (i in 1:dim(x)[1])
    {
      if(x$scenario1[i] == sp)
      {
        p50s <- x$Yprop1[i]
        nom <- paste(x$scenario1[i], x$scenario2[i],sep="-")
        adv <- x$scenario2[i]
      }
      else
      {
        p50s <- x$Yprop2[i]
        nom <- paste(x$scenario2[i], x$scenario1[i],sep="-")
        adv <- x$scenario1[i]
      }
      
      #points # compter nb fois >0.525 / <0.748 / egalite
      if (p50s>0.525)
      {
        points <- 2
      }
      else if(p50s<0.475)
      {
        points <- 0
      }
      else
      {
        points <- 1
      }
      
      v_p50s <- rbind(v_p50s, p50s)
      v_nom <- rbind(v_nom, nom)
      v_points <- rbind(v_points, points)
      v_adv <- rbind(v_adv, adv)
    }
    
    x$p50s <- as.numeric(v_p50s)
    x$nom_trait <- as.vector(v_nom)
    x$Adv <- as.vector(v_adv)
    x$score <- as.numeric(v_points)
    
    #boxplot(p50s~scenario, x, main=sp, las=3, ylim=c(0,1))
    
    res[[as.character(sp)]] <- x
    
  }

  ls_res[[j]] <- res
  j <- j+1

}

names(ls_res) <- ls_cases
