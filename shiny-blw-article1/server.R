

# Define server logic required to draw a histogram
shinyServer(function(input, output) {


    # Plots ----
    
    # Yield
    output$res_plot1 <- renderPlot({
        
        #mise a jour sous-tableau
        subx <- dat2[dat2$MngN == input$traitN & dat2$nomTrait==input$scenar & dat2$sd_val==input$CV,]
        subxmoy <- tabres[tabres$MngN == input$traitN & tabres$nomTrait==input$scenar & tabres$sd_val==input$CV,]
        subxmoy <- subxmoy[order(subxmoy$sc_val),]
        subxmoy_ref <- tabres[tabres$MngN == input$traitN & tabres$nomTrait==input$scenar & tabres$sd_val==0.001,]
        subxmoy_ref <- subxmoy_ref[order(subxmoy_ref$sc_val),]
        
        
        #couleur
        col_border <- if(input$traitN == "N+") "red" else "blue"
        
        #position cadre highlight
        x_ <- (input$delta+1.5)*2
        
        layout(matrix(1:2,1,2))
        
        #Ytot
        boxplot(Ytot~sc_val, subx, ylim=c(0,1900), col=rgb(1,0.5,0.3, alpha=0.0), border=col_border, main="Total Yield", ylab="Ytot (g.m-2)", xlab="Mean trait divergence (delta)")
        polygon(c(x_+0.5, x_+1.5, x_+1.5,x_+0.5), c(0.,0.,1900,1900), col='light grey', border='light grey')
        boxplot(Ytot~sc_val, subx, border=col_border, add=T, col=rgb(0, 0, 1, alpha = 0.1))
        
        #OY
        boxplot(OY~sc_val, subx, ylim=c(-250, 250), col=rgb(1,0.5,0.3, alpha=0.0), border=col_border, main="Overyielding", Ylab="OY (g.m-2)", xlab="Mean trait divergence (delta)")
        polygon(c(x_+0.5, x_+1.5, x_+1.5,x_+0.5), c(-250,-250,250,250), col='light grey', border='light grey')
        segments(0.,0,8.,0)
        #points(1:7, subxmoy_ref$OY, type='l', col='dark grey', lwd=2)
        boxplot(OY~sc_val, subx, border=col_border, add=T, col=rgb(0, 0, 1, alpha = 0.1))
        

        
    })
    
    
    
    # p50s
    output$res_plot2 <- renderPlot({
        
        #mise a jour sous-tableau
        subx <- dat2[dat2$MngN == input$traitN & dat2$nomTrait==input$scenar & dat2$sd_val==input$CV,]
        subxmoy <- tabres[tabres$MngN == input$traitN & tabres$nomTrait==input$scenar & tabres$sd_val==input$CV,]
        subxmoy <- subxmoy[order(subxmoy$sc_val),]
        subxmoy_ref <- tabres[tabres$MngN == input$traitN & tabres$nomTrait==input$scenar & tabres$sd_val==0.001,]
        subxmoy_ref <- subxmoy_ref[order(subxmoy_ref$sc_val),]
        
        #couleur
        col_border <- if(input$traitN == "N+") "red" else "blue"
        
        #position cadre highlight
        x_ <- (input$delta+1.5)*2
        
        
        layout(matrix(1:2,1,2))
        
        #p50S sp1
        boxplot(Yprop1~sc_val, subx, ylim=c(0,1.099), col=rgb(1,0.5,0.3, alpha=0.0), border=col_border, xlab="Mean trait divergence (delta)", ylab="p50s (sp1)", main="Sp. 1")
        polygon(c(x_+0.5, x_+1.5, x_+1.5,x_+0.5), c(0.,0.,0.95,0.95), col='light grey', border='light grey')
        segments(0.,0.5,8.,0.5)
        points(1:7, subxmoy_ref$Yprop1, type='l', col='dark grey', lwd=2)
        boxplot(Yprop1~sc_val, subx, border=col_border, add=T, col=rgb(0, 0, 1, alpha = 0.1))
        segments(1:7,0.95,1:7, 0.95+subxmoy$stab, lwd=3)
        #arrows(1:7,0.95,1:7, 0.95+subxmoy$stab, lwd=3, length=0.05)
        
        #p50s sp2
        boxplot(Yprop2~sc_val, subx, ylim=c(0,1.099), col=rgb(1,0.5,0.3, alpha=0.0), border=col_border, xlab="Mean trait divergence (delta)", ylab="p50s (sp2)", main="Sp. 2")
        polygon(c(x_+0.5, x_+1.5, x_+1.5,x_+0.5), c(0.,0.,0.95,0.95), col='light grey', border='light grey')
        segments(0.,0.5,8.,0.5)
        points(1:7, subxmoy_ref$Yprop2, type='l', col='dark grey', lwd=2)
        boxplot(Yprop2~sc_val, subx, border=col_border, add=T, col=rgb(0, 0, 1, alpha = 0.1))
        segments(1:7,0.95,1:7, 0.95+subxmoy$stab, lwd=3)
        
    })
    
    
    
    
    # Gini
    output$res_plot3 <- renderPlot({
        
        #mise a jour sous-tableau
        subx <- dat2[dat2$MngN == input$traitN & dat2$nomTrait==input$scenar & dat2$sd_val==input$CV,]
        subxmoy <- tabres[tabres$MngN == input$traitN & tabres$nomTrait==input$scenar & tabres$sd_val==input$CV,]
        subxmoy <- subxmoy[order(subxmoy$sc_val),]
        subxmoy_ref <- tabres[tabres$MngN == input$traitN & tabres$nomTrait==input$scenar & tabres$sd_val==0.001,]
        subxmoy_ref <- subxmoy_ref[order(subxmoy_ref$sc_val),]
        
        
        #couleur
        col_border <- if(input$traitN == "N+") "red" else "blue"
        
        #position cadre highlight
        x_ <- (input$delta+1.5)*2
        
        layout(matrix(1:2,1,2))
        
        #gini1
        boxplot(gini1~sc_val, subx, ylim=c(0,1), col=rgb(1,0.5,0.3, alpha=0.0), border=col_border, main="Sp. 1", xlab="Mean trait divergence (delta)")
        polygon(c(x_+0.5, x_+1.5, x_+1.5,x_+0.5), c(0.,0.,0.95,0.95), col='light grey', border='light grey')
        points(1:7, subxmoy_ref$gini1, type='l', col='dark grey', lwd=2)
        boxplot(gini1~sc_val, subx, border=col_border, add=T, col=rgb(0, 0, 1, alpha = 0.1))
        
        #gini2
        boxplot(gini2~sc_val, subx, ylim=c(0,1), col=rgb(1,0.5,0.3, alpha=0.0), border=col_border, main="Sp. 2", xlab="Mean trait divergence (delta)")
        polygon(c(x_+0.5, x_+1.5, x_+1.5,x_+0.5), c(0.,0.,0.95,0.95), col='light grey', border='light grey')
        points(1:7, subxmoy_ref$gini2, type='l', col='dark grey', lwd=2)
        boxplot(gini2~sc_val, subx, border=col_border, add=T, col=rgb(0, 0, 1, alpha = 0.1))
        
        
    })
    
    
    
    # D5param
    output$res_plot4 <- renderPlot({
        
        #mise a jour sous-tableau
        subx <- dat2[dat2$MngN == input$traitN & dat2$nomTrait==input$scenar & dat2$sd_val==input$CV,]
        subxmoy <- tabres[tabres$MngN == input$traitN & tabres$nomTrait==input$scenar & tabres$sd_val==input$CV,]
        subxmoy <- subxmoy[order(subxmoy$sc_val),]
        subxmoy_ref <- tabres[tabres$MngN == input$traitN & tabres$nomTrait==input$scenar & tabres$sd_val==0.001,]
        subxmoy_ref <- subxmoy_ref[order(subxmoy_ref$sc_val),]
        
        
        #couleur
        col_border <- if(input$traitN == "N+") "red" else "blue"
        
        
        
        subxD <- subx[subx$sc_val == input$delta, ]
        #refait un tableau adapte pour boxplot avec les D5param
        
        layout(matrix(1:2,1,2))
        
        # Sp 1
        D5_Fix0 <- subxD[, c("dec5_Len_Fix0", "dec5_Lfeuille_Fix0", "dec5_phyllochron_Fix0", "dec5_Vmax2_Fix0", "dec5_ELmax_Fix0", "dec5_PPtreshh_Fix0")]
        names(D5_Fix0) <- c("LenMax", "LfMax","Phyllo1", "Vmax2", "ELmax", "PPtreshh")
        
        boxplot(D5_Fix0, las=3, ylab="D5param", ylim=c(10,90), col=rgb(1,0.5,0.3, alpha=0.0), border=col_border, main="Sp. 1", boxwex=0.4)
        segments(0.,50,8.,50)
        text(1,85,paste("delta=", input$delta))
        
        
        #Sp 2
        D5_Fix1 <- subxD[, c("dec5_Len_Fix1", "dec5_Lfeuille_Fix1", "dec5_phyllochron_Fix1", "dec5_Vmax2_Fix1", "dec5_ELmax_Fix1", "dec5_PPtreshh_Fix1")]
        names(D5_Fix1) <- c("LenMax", "LfMax","Phyllo1", "Vmax2", "ELmax", "PPtreshh")
        
        boxplot(D5_Fix1, las=3, ylab="D5param", ylim=c(10,90), col=rgb(1,0.5,0.3, alpha=0.0), border=col_border, main="Sp. 2", boxwex=0.4)
        segments(0.,50,8.,50)
        text(1,85,paste("delta=", input$delta))
        
        
        
    })
    
    
    
    
    # MS indiv correlations
    output$res_plot5 <- renderPlot({
        
        #mise a jour sous-tableau
        subxmoy <- tabres[tabres$MngN == input$traitN & tabres$nomTrait==input$scenar & tabres$sd_val==input$CV,]
        subxmoy <- subxmoy[order(subxmoy$sc_val),]
        
        
        # matrice comple
        
        df <- subxmoy[,c("sc_val","Cor_PARi","Cor_Nuptake","Fix0_Cor_ParamAllNorm", "Fix1_Cor_ParamAllNorm", "Cor_PARinonKin")]#,"Cor_PARiKin","Cor_PARivois"
        df <- df[order(df$sc_val),]
        
        mat_ <- t(as.matrix(df[,c("Cor_PARi","Cor_Nuptake","Fix0_Cor_ParamAllNorm", "Fix1_Cor_ParamAllNorm", "Cor_PARinonKin")])) #,"Cor_PARiKin","Cor_PARivois"
        colnames(mat_) <- df$sc_val
        rownames(mat_) <- c("PARi","Nupt","Pscore Sp.1", "Pscore Sp.2", "RnonKin")#, "RKin" ,"Rlocal"
        
        corrplot(mat_, main="titre")
        
        #position cadre highlight
        x_ <- (input$delta+1.5)*2
        
        polygon(c(x_+0.5, x_+1.5, x_+1.5,x_+0.5), c(0.,0.,6.5, 6.5), col='light grey', border="red",  density=0)
        
        
        
    })
    
    
    
    # input parameter visualisation
    output$res_plot6 <- renderPlot({
        
        layout(matrix(c(1,1,2),1,3))
        
        m_ <- 2
        std_ <- max(0.005, input$CV) #0.005#15#30#15#30
        deltamax = 0.3*m_
        
        x <- seq(0, 4, length=10000)
        y <- dnorm(x, mean= m_, sd= std_)
        plot(x, y, type="l", lwd=1, xlab="",ylab="", main=paste(input$scenar, "delta=",input$delta, "CV=",input$CV), xaxt='n', yaxt='n')
        polygon(x,y, col="grey")
        
        
        text(3.5, 1.*max(y),  expression(underline("Parameters:")))
        
        if(input$scenar %in% c('T1-Len','T3L','T6')) {text(3.5, 0.95*max(y), "LenMax")}
        if(input$scenar %in% c('T1-Lf','T3L','T6')) {text(3.5, 0.9*max(y), "LfMax")}
        if(input$scenar %in% c('T1-phyllo','T3L','T6')) {text(3.5, 0.85*max(y), "Phyllo1")}
        
        if(input$scenar %in% c('T1-Vmax2','T3N','T6')) {text(3.5, 0.75*max(y), "Vmax2")}
        if(input$scenar %in% c('T1-ELmax','T3N','T6')) {text(3.5, 0.7*max(y), "Elmax")}
        if(input$scenar %in% c('T1-PPtresh','T3N','T6')) {text(3.5, 0.65*max(y), "PPtreshh")}
        
        #DELTA 0.5
        DELTA = 0.5*deltamax
        y2 <- dnorm(x, mean= m_+DELTA, sd= std_)
        points(x, y2, type="l", lwd=1, col="light grey")
        y2 <- dnorm(x, mean= m_-DELTA, sd= std_)
        points(x, y2, type="l", lwd=1, col="light grey")
        
        #DELTA 1
        DELTA = 1*deltamax
        y2 <- dnorm(x, mean= m_+DELTA, sd= std_)
        points(x, y2, type="l", lwd=1, col="light grey")
        y2 <- dnorm(x, mean= m_-DELTA, sd= std_)
        points(x, y2, type="l", lwd=1, col="light grey")
        
        #DELTA 1.5
        DELTA = 1.5*deltamax
        y2 <- dnorm(x, mean= m_+DELTA, sd= std_)
        points(x, y2, type="l", lwd=1, col="light grey")
        y2 <- dnorm(x, mean= m_-DELTA, sd= std_)
        points(x, y2, type="l", lwd=1, col="light grey")
        
        
        #variable
        DELTA = input$delta*deltamax
        y2 <- dnorm(x, mean= m_+DELTA, sd= std_)
        points(x, y2, type="l", lwd=2, col=2)
        
        text(2, 0.1*max(y), "Sp. 1")
        text(m_+DELTA, 0.15*max(y), "Sp. 2", col="red")
        
        
    })
    
    
    
    
    # CE-SE
    output$res_plot7 <- renderPlot({
        
        #mise a jour sous-tableau
        subx <- dat2[dat2$MngN == input$traitN & dat2$nomTrait==input$scenar & dat2$sd_val==input$CV,]
        subxmoy <- tabres[tabres$MngN == input$traitN & tabres$nomTrait==input$scenar & tabres$sd_val==input$CV,]
        subxmoy <- subxmoy[order(subxmoy$sc_val),]
        subxmoy_ref <- tabres[tabres$MngN == input$traitN & tabres$nomTrait==input$scenar & tabres$sd_val==0.001,]
        subxmoy_ref <- subxmoy_ref[order(subxmoy_ref$sc_val),]
        
        
        #couleur
        col_border <- if(input$traitN == "N+") "red" else "blue"
        
        #position cadre highlight
        x_ <- (input$delta+1.5)*2
        
        
        layout(matrix(1:2,1,2))
        
        #CE
        boxplot(CE~sc_val, subx, ylim=c(-250, 250), col=rgb(1,0.5,0.3, alpha=0.0), border=col_border, main='Complementarity Effect (CE)', xlab="Mean trait divergence (delta)")
        polygon(c(x_+0.5, x_+1.5, x_+1.5,x_+0.5), c(-250,-250.,250,250), col='light grey', border='light grey')
        points(1:7, subxmoy_ref$CE, type='l', col='dark grey')
        boxplot(CE~sc_val, subx, border=col_border, add=T, col=rgb(0, 0, 1, alpha = 0.1))
        
        #SE
        boxplot(0.5*SE~sc_val, subx, ylim=c(-250, 250), col=rgb(1,0.5,0.3, alpha=0.0), border=col_border, main='Selecetion Effect (SE)', ylab="SE", xlab="Mean trait divergence (delta)")
        polygon(c(x_+0.5, x_+1.5, x_+1.5,x_+0.5), c(-250,-250.,250,250), col='light grey', border='light grey')
        points(1:7, 0.5*subxmoy_ref$SE, type='l', col='dark grey')
        boxplot(0.5*SE~sc_val, subx, border=col_border, add=T, col=rgb(0, 0, 1, alpha = 0.1))
        
        
        
    })
    

    
    
    # Texte ----
    output$res_text6 <- renderText({
        paste0("Fig A: ...")
    })
    
    output$res_text1 <- renderText({
        paste0("Fig B: ...")
    })
    
    output$res_text2 <- renderText({
        paste0("Fig C: ...")
    })
    
    output$res_text3 <- renderText({
        paste0("Fig D: ...")
    })
    
    output$res_text4 <- renderText({
        paste0("Fig E: ...")
    })
    
    output$res_text5 <- renderText({
        paste0("Fig F: ...")
    })
    
    output$res_text7 <- renderText({
        paste0("Fig G: ...")
    })
    
})
