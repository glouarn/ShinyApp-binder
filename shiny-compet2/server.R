

# Define server logic required to draw a histogram
shinyServer(function(input, output) {


    # Plots ----
    
    # Overview cas mixture
    output$res_plot1 <- renderPlot({
        
      #mise a jour tableau
      dat <- ls_dat[[input$cas]]
         
      
      sol <- ifelse (input$depth == "Deep soil", "Deep", "Shallow ")
      Nlevel <- ifelse (input$Nlevel == 0, "0N", ifelse (input$Nlevel == 300, "300N", "100N"))
      myenv <- paste(Nlevel, sol, sep="")
      #marche pas
      
      
      dat_env <- dat[dat$EnvirOK== input$myenv, ] #jeu de donnee complet pour cas et envir
      dat_envMix <- dat_env[(dat_env$scenario1 == input$scenar1 & dat_env$scenario2 == input$scenar2) | (dat_env$scenario1 == input$scenar2 & dat_env$scenario2 == input$scenar1),] #asso ciblee
      
      id_col <- which(ls_env== input$myenv)
      ls_col <- rep("grey",6)
      ls_col[id_col] <- "#DF536B"#"red"
      
      
      layout(matrix(1:3, 1,3))
      boxplot(Ytot~EnvirOK, data=dat,  ylim=c(0, 2000) ,las=3, cex.axis=0.7, col=ls_col, xlab='', main="")
      ypts <- dat_envMix$Ytot
      points(rep(id_col, length(ypts)), ypts, col=3, pch=16)
      
      boxplot(Yprop2~EnvirOK, data=dat,  ylim=c(0, 1) ,las=3, cex.axis=0.7, col=ls_col, xlab='', ylab='p50s', main=input$cas)
      ypts <- dat_envMix$Yprop2
      points(rep(id_col, length(ypts)), ypts, col=3, pch=16)
      
      boxplot(OY~EnvirOK, data=dat, ylim=c(-250, 500) , las=3, cex.axis=0.7, col=ls_col, xlab='', main="")
      segments(0,0,6,0,col=4,lty=3)
      ypts <- dat_envMix$OY
      points(rep(id_col, length(ypts)), ypts, col=3, pch=16)
      
      
        
    })
    
    
    
    # LERp
    output$res_plot2 <- renderPlot({
        
      #mise a jour tableau
      dat <- ls_dat[[input$cas]]
      dat_env <- dat[dat$EnvirOK== input$myenv, ] #jeu de donnee complet pour cas et envir
      dat_envMix <- dat_env[(dat_env$scenario1 == input$scenar1 & dat_env$scenario2 == input$scenar2) | (dat_env$scenario1 == input$scenar2 & dat_env$scenario2 == input$scenar1),] #asso ciblee
      
      
      layout(matrix(1:2, 1,2))
      
      plot(LERp2~LERp1, data=dat, ylim=c(0, 1),  xlim=c(0, 1), main=input$cas)
      points(LERp2~LERp1, data=dat_env, col=2)
      points(LERp2~LERp1, data=dat_envMix, col=3, pch=16)
      
      segments(0,0.5,1,0.5,col=4, lty=2)
      segments(0.5,0.,0.5,1,col=4, lty=2)
      segments(0.,1.,1.,0,col=4, lty=2)
      
      plot(OY~Yprop2, data=dat, ylim=c(-250, 500),  xlim=c(0, 1), xlab='p50s')
      points(OY~Yprop2, data=dat_env, col=2)
      points(OY~Yprop2, data=dat_envMix, col=3, pch=16)
      segments(0,0,1,0,col=4,lty=2)
        
    })
    
      
    
    # mat OY
    output$res_plot3 <- renderPlot({
        
      #mise a jour tableau
      dat <- ls_dat[[input$cas]]
      dat_env <- dat[dat$EnvirOK== input$myenv, ] #jeu de donnee complet pour cas et envir
      
      
      tabOY <- aggregate(OY~EnvirOK+scenario, data=dat_env[,c("EnvirOK", "scenario", "OY")], mean)
      tabOY$OYnorm <- tabOY$OY/500#300 #normalise a 300 g.m-2
      tabOY$OYnorm[tabOY$OYnorm>1] <- 1
      
      xx <- as.data.frame(t(as.data.frame(strsplit(tabOY$scenario, "-"))))
      row.names(xx) <- 1:dim(xx)[1]
      names(xx) <- c("scenario1", "scenario2")
      tabOY$scenario1 <- xx$scenario1
      tabOY$scenario2 <- xx$scenario2
      stabOY <- split(tabOY, tabOY$EnvirOK)
      

      x <- stabOY[[input$myenv]]
      
      #matrice OY (faire fonction)
      mat1 <- matrix(rep(0,19*19), 19, 19)
      rownames(mat1) <- sort(unique(x$scenario1))
      colnames(mat1) <- sort(unique(x$scenario2))
      
      for(i in 1:dim(x)[1])
      {
        mat1[x[i,"scenario2"], x[i,"scenario1"]] <- x[i,"OYnorm"]
      }
      
      
      if (input$cas == "NonFix-NonFix")
      {
        corrplot(mat1, method = 'square', type = 'upper', main="")
      } else
      {
        colnames(mat1) <- paste(sort(unique(x$scenario2)), "f",sep="")
        if (input$cas == "NonFix-Fix")
        {
          corrplot(mat1, method = 'square', main="")
        } else
        {
          row.names(mat1) <- paste(sort(unique(x$scenario2)), "f",sep="")
          corrplot(mat1, method = 'square', type = 'upper', main="")
        }
      }
      
      #ajout window mixture position
      if (input$scenar1 %in% rownames(mat1) & input$scenar2 %in% colnames(mat1))
      {
        y0 <- which(rownames(mat1) == input$scenar1)
        x0 <- which(colnames(mat1) == input$scenar2)
        rect(x0+0.5,(20-y0)+0.5,x0-0.5, (20-y0)-0.5, border=3, lwd=2)
        #points(x0,20-y0, pch=16, col=3)
        #points(1,18, pch=16) #???
        #text(10, 10, paste(x0,y0))
        # position bonnes mais manque 1eres et dernieres lignes ds triangle?? peut pas afficher?? ok dans carre!
      }
      
      
      
    })
    
    
    
    # mat p50s
    output$res_plot4 <- renderPlot({
        
      #mise a jour tableau
      dat <- ls_dat[[input$cas]]
      dat_env <- dat[dat$EnvirOK== input$myenv, ] #jeu de donnee complet pour cas et envir
      
      
      #matrice p50s
      tabp50s <- aggregate(Yprop2~EnvirOK+scenario, data=dat_env[,c("EnvirOK", "scenario", "Yprop2")], mean)
      
      xx <- as.data.frame(t(as.data.frame(strsplit(tabp50s$scenario, "-"))))
      row.names(xx) <- 1:dim(xx)[1]
      names(xx) <- c("scenario1", "scenario2")
      tabp50s$scenario1 <- xx$scenario1
      tabp50s$scenario2 <- xx$scenario2
      stabp50s <- split(tabp50s, tabp50s$Envir)
      
      
      x <- stabp50s[[input$myenv]]
      
      
      mat2 <- matrix(rep(0,19*19), 19, 19)
      rownames(mat2) <- sort(unique(x$scenario1))
      colnames(mat2) <- sort(unique(x$scenario2))
      
      for(i in 1:dim(x)[1])
      {
        mat2[x[i,"scenario2"], x[i,"scenario1"]] <- x[i,"Yprop2"]
      }
      
      
      if (input$cas == "NonFix-NonFix")
      {
        corrplot(mat2, method = 'square', type = 'upper', main="")
      } else
      {
        colnames(mat2) <- paste(sort(unique(x$scenario2)), "f",sep="")
        if (input$cas == "NonFix-Fix")
        {
          corrplot(mat2, method = 'square', main="")
        } else
        {
          row.names(mat2) <- paste(sort(unique(x$scenario2)), "f",sep="")
          corrplot(mat2, method = 'square', type = 'upper', main="")
        }
      }
      
      #ajout window mixture position
      if (input$scenar1 %in% rownames(mat2) & input$scenar2 %in% colnames(mat2))
      {
        y0 <- which(rownames(mat2) == input$scenar1)
        x0 <- which(colnames(mat2) == input$scenar2)
        rect(x0+0.5,(20-y0)+0.5,x0-0.5, (20-y0)-0.5, border=3, lwd=2)
      }
        
        
    })
    
    
    
    
    # Win proba
    output$res_plot5 <- renderPlot({
        
      #mise a jour sous-tableau
           
      res <- ls_res[[input$cas]]
      env <- c(input$myenv)#unique(dat$Envir)
      
      # liste order des esp 1 et 2
      if(input$cas == "NonFix-NonFix")
      {
        ls1 <- ordre_croi_ref
      }else
      {
        ls1 <- ordre_croi_ref_fix
      }
      
      if(input$cas == "Fix-Fix")
      {
        ls2 <- ordre_croi_ref_fix
      }else
      {
        ls2 <- ordre_croi_ref
      }
      
      #build ta_proba
      tab_proba <- vector("list", length=length(ls1))
      names(tab_proba ) <- as.character(ls1)
      
      for (sp in as.character(ls1))
      {
        x <- res[[sp]]
        spx <- split(x, x$Adv)
        
        
        resadv <- NULL
        for (adv in as.character(ls2)) #marche slmt si adversaire identique a toi meme
        {
          xx <- spx[[adv]]
          #xx <- xx[xx$Mng %in% traitN, ]
          xx <- xx[xx$EnvirOK %in% env, ]
          proba_adv <- sum(xx$score) / (2*dim(xx)[1])
          resadv <- cbind(resadv, proba_adv)
        }
        resadv <- as.data.frame(resadv)
        names(resadv) <- as.character(ls2)
        
        tab_proba[[sp]] <- resadv
        
      }
      mat_proba <- do.call("rbind", tab_proba)
      
      #plot win proba 
      corrplot(as.matrix(mat_proba))
      segments(20,0,0,20,col=4)
        
        
    })
    
    
    
    # CE-SE map
    output$res_plot7 <- renderPlot({
        
      #mise a jour tableau
      dat <- ls_dat[[input$cas]]
      dat_env <- dat[dat$EnvirOK== input$myenv, ] #jeu de donnee complet pour cas et envir
      
      
      #mat CE
      tabCE <- aggregate(CE~EnvirOK+scenario, data=dat_env[,c("EnvirOK", "scenario", "CE")], mean)
      tabCE$CEnorm <- tabCE$CE/400#300 #normalise a 300 g.m-2
      tabCE$CEnorm[tabCE$CEnorm>1] <- 1
      

      xx <- as.data.frame(t(as.data.frame(strsplit(tabCE$scenario, "-"))))
      row.names(xx) <- 1:dim(xx)[1]
      names(xx) <- c("scenario1", "scenario2")
      tabCE$scenario1 <- xx$scenario1
      tabCE$scenario2 <- xx$scenario2
      stabp50s <- split(tabCE, tabCE$EnvirOK)
      
      x <- stabp50s[[input$myenv]]
      
      mat2 <- matrix(rep(0,19*19), 19, 19)
      rownames(mat2) <- sort(unique(x$scenario1))
      colnames(mat2) <- sort(unique(x$scenario2))
      
      for(i in 1:dim(x)[1])
      {
        mat2[x[i,"scenario2"], x[i,"scenario1"]] <- x[i,"CEnorm"]
      }
      
      
      #mat SE
      tabSE <- aggregate(SE~EnvirOK+scenario, data=dat_env[,c("EnvirOK", "scenario", "SE")], mean)
      tabSE$SEnorm <- tabSE$SE/600#300 #normalise a 300 g.m-2
      tabSE$SEnorm[tabSE$SEnorm>1] <- 1
      
      
      xx <- as.data.frame(t(as.data.frame(strsplit(tabSE$scenario, "-"))))
      row.names(xx) <- 1:dim(xx)[1]
      names(xx) <- c("scenario1", "scenario2")
      tabSE$scenario1 <- xx$scenario1
      tabSE$scenario2 <- xx$scenario2
      stabp50s <- split(tabSE, tabSE$EnvirOK)
      
      x <- stabp50s[[input$myenv]]
      
      mat1 <- matrix(rep(0,19*19), 19, 19)
      rownames(mat1) <- sort(unique(x$scenario1))
      colnames(mat1) <- sort(unique(x$scenario2))
      
      for(i in 1:dim(x)[1])
      {
        mat1[x[i,"scenario2"], x[i,"scenario1"]] <- x[i,"SEnorm"]
      }
      
      
      # plot CE et SE
      layout(matrix(1:2, 1,2))
      
      #CE
      if (input$cas == "NonFix-NonFix")
      {
        corrplot(mat2, method = 'square', type = 'upper', main="")
      } else
      {
        colnames(mat2) <- paste(sort(unique(x$scenario2)), "f",sep="")
        if (input$cas == "NonFix-Fix")
        {
          corrplot(mat2, method = 'square', main="")
        } else
        {
          row.names(mat2) <- paste(sort(unique(x$scenario2)), "f",sep="")
          corrplot(mat2, method = 'square', type = 'upper', main="")
        }
      }
      
      #ajout window mixture position
      if (input$scenar1 %in% rownames(mat2) & input$scenar2 %in% colnames(mat2))
      {
        y0 <- which(rownames(mat2) == input$scenar1)
        x0 <- which(colnames(mat2) == input$scenar2)
        rect(x0+0.5,(20-y0)+0.5,x0-0.5, (20-y0)-0.5, border=3, lwd=2)
      }
      
      # SE
      if (input$cas == "NonFix-NonFix")
      {
        corrplot(mat1, method = 'square', type = 'upper', main="")
      } else
      {
        colnames(mat1) <- paste(sort(unique(x$scenario2)), "f",sep="")
        if (input$cas == "NonFix-Fix")
        {
          corrplot(mat1, method = 'square', main="")
        } else
        {
          row.names(mat1) <- paste(sort(unique(x$scenario2)), "f",sep="")
          corrplot(mat1, method = 'square', type = 'upper', main="")
        }
      }
      
      #ajout window mixture position
      if (input$scenar1 %in% rownames(mat1) & input$scenar2 %in% colnames(mat1))
      {
        y0 <- which(rownames(mat1) == input$scenar1)
        x0 <- which(colnames(mat1) == input$scenar2)
        rect(x0+0.5,(20-y0)+0.5,x0-0.5, (20-y0)-0.5, border=3, lwd=2)
      }
      

    })
    
    
    
    
    # input parameter visualisation (plant)
    output$res_plot6 <- renderPlot({
      
      
      layout(matrix(1:2, 1,2))
      
      
      if (input$cas == "NonFix-NonFix")
      {
        plot(im19, main="Sp1")
        idsp1 <- which(input$scenar1 == posiSp$Sp)
        y0 <- posiSp$y0[idsp1]
        x0 <- posiSp$x0[idsp1]
        deltax <- 55
        deltay <- 130
        if (input$scenar1 %in% ordre_croi_ref)
        {
          rect(x0+deltax,(20-y0)+deltay,x0-deltax, (20-y0)-deltay, border=3, lwd=2)
        }
        
        
        
        plot(im19, main="Sp2")
        idsp2 <- which(input$scenar2 == posiSp$Sp)
        y0 <- posiSp$y0[idsp2]
        x0 <- posiSp$x0[idsp2]
        deltax <- 55
        deltay <- 130
        if (input$scenar2 %in% ordre_croi_ref)
        {
          rect(x0+deltax,(20-y0)+deltay,x0-deltax, (20-y0)-deltay, border=3, lwd=2)
        }
        
        
        
      } else
      {
        if (input$cas == "Fix-Fix")
        {
          plot(im19fix, main="Sp1")
          idsp1 <- which(input$scenar1 == posiSp$Sp)
          y0 <- posiSp$y0[idsp1]
          x0 <- posiSp$x0[idsp1]
          deltax <- 55
          deltay <- 130
          if (input$scenar1 %in% ordre_croi_ref_fix)
          {
            rect(x0+deltax,(20-y0)+deltay,x0-deltax, (20-y0)-deltay, border=3, lwd=2)
          }
          
          
          plot(im19fix, main="Sp2")
          idsp2 <- which(input$scenar2 == posiSp$Sp)
          y0 <- posiSp$y0[idsp2]
          x0 <- posiSp$x0[idsp2]
          deltax <- 55
          deltay <- 130
          if (input$scenar2 %in% ordre_croi_ref_fix)
          {
            rect(x0+deltax,(20-y0)+deltay,x0-deltax, (20-y0)-deltay, border=3, lwd=2)
          }
          
          
        } else
        {
          plot(im19, main="Sp1")
          idsp1 <- which(input$scenar1 == posiSp$Sp)
          y0 <- posiSp$y0[idsp1]
          x0 <- posiSp$x0[idsp1]
          deltax <- 55
          deltay <- 130
          if (input$scenar1 %in% ordre_croi_ref)
          {
            rect(x0+deltax,(20-y0)+deltay,x0-deltax, (20-y0)-deltay, border=3, lwd=2)
          }
          
          
          plot(im19fix, main="Sp2")
          idsp2 <- which(input$scenar2 == posiSp$Sp)
          y0 <- posiSp$y0[idsp2]
          x0 <- posiSp$x0[idsp2]
          deltax <- 55
          deltay <- 130
          if (input$scenar2 %in% ordre_croi_ref_fix)
          {
            rect(x0+deltax,(20-y0)+deltay,x0-deltax, (20-y0)-deltay, border=3, lwd=2)
          }
          
        }
      }
      
      
    })
    
    # input parameter visualisation (sol)
    output$res_plot8 <- renderPlot({
      
      layout(matrix(1:2, 1,2))
      plot(im6env, main="Environment")
      
      idx <- which(input$myenv == c("0N-Deep", "100N-Deep", "300N-Deep","0N- Shallow", "100N- Shallow", "300N- Shallow"))
      y0 <- -240
      x0 <- c(100,300,500,720,920,1120)[idx]
      delta <- 110
      rect(x0+delta,(20-y0)+delta,x0-delta, (20-y0)-delta, border=2, lwd=2)
      
      
    })
    
    
    
    #test
    output$res_plotTest <- renderPlot({
        
        #mise a jour sous-tableau
        
        
    })

    
    
    # Texte ----
    output$res_text6 <- renderText({
        paste0("Fig A: Selected plant senarios in the virtual experiment ")
    })
    
    output$res_text8 <- renderText({
      paste0("Fig B: Selected environment in the virtual experiment")
    })
    
    output$res_text1 <- renderText({
        paste0("Fig C: Total annual yield (Ytot), species proportion (p50s) and overyielding (OY) of simulated binary mixtures in the 6 tested environments; the red box highlitghts the subset of data of all mixtures in the selected environement; Green points highlight values for the current mixture selected")
    })
    
    output$res_text2 <- renderText({
        paste0("Fig D: Relationships between partial Land Equivalent Ratio (LERp) of Sp.1 and Sp.2 (A) and between overyielding (OY) and species proportion (p50s) (B); Red points highlitght the subset of data for all mixtures in the selected environment; Green points highlight values for the current mixture selected")
    })
    
    output$res_text3 <- renderText({
        paste0("Fig E: Distribution of OY by individual mixtures in the selected environement; values are normalised by maximum overyielding (OY/500)")
    })
    
    output$res_text4 <- renderText({
        paste0("Fig F:  Distribution of species proportion (p50s) by individual mixtures in the selected environement")
    })
    
    output$res_text5 <- renderText({
        paste0("Fig G: Dominance (or 'winning') probability of Sp1 over Sp2 in the selected environement - Species are ordered according to their overall probability of dominance accross all environments to highlight shifts in species ranking")
    })
    
    output$res_text7 <- renderText({
        paste0("Fig H: Distribution of (A) Complementarity effect (CE) and (B) Selection effect (SE) by individual mixtures in the selected environement; values are normalised by maximum CE (CE/400) and SE (SE/600)")
    })
    
    
    
})
