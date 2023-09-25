

# Define server logic required to draw a histogram
shinyServer(function(input, output) {


    # Plots ----
    
    
    # Biopmass production / proportion
    output$res_plot1 <- renderPlot({
        
        if (input$groupN1 == "Legume") 
        {bw_Fix <- 1
        } else 
        {bw_Fix <- 0}
        
        #seletedkey <- dparams[dparams$normq == input$bw_q & dparams$normLen == input$bw_Len & dparams$normVmax2 == input$bw_Vmax2 & dparams$normRUE == input$bw_RUE & dparams$normMaxFix == input$bw_Fix, "keysc"]
        seletedkey <- dparams[dparams$normq == input$bw_q & dparams$normLen == input$bw_Len & dparams$normVmax2 == input$bw_Vmax2 & dparams$normRUE == input$bw_RUE & dparams$normMaxFix == bw_Fix, "keysc"]
        tabmoy <- ls_tabmoys[[input$nfert_level]][[seletedkey]]
        
        
        colv_ <- c("#FFFF99", "#FF0033","#0033CC") #c(1,2,4)
        par(bg=colbg, col.lab=collab,col.axis=collab)
        
        res_sowing <- YtotvsProp(tabmoy, nom="", optProp="sowing", visuplot=T, visutext=F, colv=colv_)
        text(0.15, 2000,"Sp.1", col=colv_[2])
        text(0.15, 1900,"Sp.2", col=colv_[3])
        text(0.15, 2100,"Ytot = Sp.1+Sp.2", col=colv_[1])
        
        axis(1,col.ticks=collab)
        axis(2,col.ticks=collab)
        box(col=collab)
        
        
        
    })
    
    
    
    # Overyielding vs all
    output$res_plot2 <- renderPlot({
        
        if (input$groupN1 == "Legume") 
        {bw_Fix <- 1
        } else 
        {bw_Fix <- 0}
        
        #seletedkey <- dparams[dparams$normq == input$bw_q & dparams$normLen == input$bw_Len & dparams$normVmax2 == input$bw_Vmax2 & dparams$normRUE == input$bw_RUE & dparams$normMaxFix == input$bw_Fix, "keysc"]
        seletedkey <- dparams[dparams$normq == input$bw_q & dparams$normLen == input$bw_Len & dparams$normVmax2 == input$bw_Vmax2 & dparams$normRUE == input$bw_RUE & dparams$normMaxFix == bw_Fix, "keysc"]
        tabmoy <- ls_tabmoys[[input$nfert_level]][[seletedkey]]
        
        
        
        isfix <- grepl('Fix2-', names(ls_tabmoys[[input$nfert_level]]))
        #split en pur ou association, puis graph ttes les situations
        #if (input$bw_Fix == 0) 
        if (bw_Fix == 0)
        {ls_tab <- ls_tabmoys[[input$nfert_level]][!isfix]
        } else 
        {ls_tab <- ls_tabmoys[[input$nfert_level]][isfix]}
        
        
        
        colv_ <- c("#CCCCCC", "#FFFF99", "#FF0033","#0033CC")
        par(bg=colbg, col.lab=collab,col.axis=collab)
        
        #res_sowing <- YtotvsProp(tabmoy, nom="", optProp="sowing", visuplot=T, visutext=F, colv=colv_)
        OverYvsAll(ls_tab, seletedkey, nom="", optProp="sowing", cex.lab=1, Ymax=650, colv=colv_)
        text(0.15, -400,"aOY = Ytot - Ytheo", col=colv_[2])
        text(0.15, -480,"All other mixtures", col=colv_[1])
        
        axis(1,col.ticks=collab)
        axis(2,col.ticks=collab)
        box(col=collab)
        
        
        
    })
    
    # Resource use vs all
    output$res_plot3 <- renderPlot({
        
        if (input$groupN1 == "Legume") 
        {bw_Fix <- 1
        } else 
        {bw_Fix <- 0}
        
        #seletedkey <- dparams[dparams$normq == input$bw_q & dparams$normLen == input$bw_Len & dparams$normVmax2 == input$bw_Vmax2 & dparams$normRUE == input$bw_RUE & dparams$normMaxFix == input$bw_Fix, "keysc"]
        seletedkey <- dparams[dparams$normq == input$bw_q & dparams$normLen == input$bw_Len & dparams$normVmax2 == input$bw_Vmax2 & dparams$normRUE == input$bw_RUE & dparams$normMaxFix == bw_Fix, "keysc"]
        tabmoy <- ls_tabmoys[[input$nfert_level]][[seletedkey]]
        
        
        
        isfix <- grepl('Fix2-', names(ls_tabmoys[[input$nfert_level]]))
        #split en pur ou association, puis graph ttes les situations
        #if (input$bw_Fix == 0) 
        if (bw_Fix == 0)
        {ls_tab <- ls_tabmoys[[input$nfert_level]][!isfix]
        } else 
        {ls_tab <- ls_tabmoys[[input$nfert_level]][isfix]}
        
        
        
        colv_ <- c("#CCCCCC", "#FFFF99", "#FF0033","#0033CC")
        par(bg=colbg, col.lab=collab,col.axis=collab)
        
        #res_sowing <- YtotvsProp(tabmoy, nom="", optProp="sowing", visuplot=T, visutext=F, colv=colv_)
        OverYvsAll(ls_tab, seletedkey, nom="", optProp="actual", cex.lab=1, Ymax=650, colv=colv_)
        text(0.15, -480,"Total Sp.1+Sp.2", col=colv_[2])
        text(0.15, -400,"All mixtures", col=colv_[1])
        
        axis(1,col.ticks=collab)
        axis(2,col.ticks=collab)
        box(col=collab)
        
        
        
    })
    
    
    # CE-SE
    output$res_plot4 <- renderPlot({
        
        if (input$groupN1 == "Legume") 
        {bw_Fix <- 1
        } else 
        {bw_Fix <- 0}
        
        #seletedkey <- dparams[dparams$normq == input$bw_q & dparams$normLen == input$bw_Len & dparams$normVmax2 == input$bw_Vmax2 & dparams$normRUE == input$bw_RUE & dparams$normMaxFix == input$bw_Fix, "keysc"]
        seletedkey <- dparams[dparams$normq == input$bw_q & dparams$normLen == input$bw_Len & dparams$normVmax2 == input$bw_Vmax2 & dparams$normRUE == input$bw_RUE & dparams$normMaxFix == bw_Fix, "keysc"]
        tabmoy <- ls_tabmoys[[input$nfert_level]][[seletedkey]]
        
        par(bg=colbg, col.lab=collab,col.axis=collab)
        
        plot(tabmoy$Semprop1, (tabmoy$CE+0.5*tabmoy$SE), type='b',ylim=c(-650,650), main="", ylab="Overyieding (g.m-2)", xlab="Sowing proportion (Esp. 1)", cex.lab=1., pch=0)
        
        rect(par("usr")[1], par("usr")[3],
             par("usr")[2], par("usr")[4],
             col = "#f7f7f7")
        
        col1 <- rgb(0,0,1,1/4)#4
        x <- c(0., tabmoy$Semprop1, 1.)
        y <- c(0, tabmoy$CE,0)
        polygon(x,y,col=col1)
        col2 <- rgb(1,0,0,1/4)#2
        y <- c(0, 0.5*tabmoy$SE,0)
        polygon(x,y,col=col2)
        points(tabmoy$Semprop1, (tabmoy$CE+0.5*tabmoy$SE), type='l', lwd=2,pch=0)
        points(tabmoy$Semprop1, (tabmoy$CE+0.5*tabmoy$SE), col="#FF0033", pch=16)
        
        text(0.18, -550,"CE (complementarity)", col=col1)
        text(0.18, -480,"SE (selection)", col=col2)
        text(0.18, -400,"aOY = CE + SE", col=1)
        
        axis(1,col.ticks=collab)
        axis(2,col.ticks=collab)
        box(col=collab)
        
        

        
    })
    
    
    # LER
    output$res_plot5 <- renderPlot({
        
        if (input$groupN1 == "Legume") 
        {bw_Fix <- 1
        } else 
        {bw_Fix <- 0}
        
        #seletedkey <- dparams[dparams$normq == input$bw_q & dparams$normLen == input$bw_Len & dparams$normVmax2 == input$bw_Vmax2 & dparams$normRUE == input$bw_RUE & dparams$normMaxFix == input$bw_Fix, "keysc"]
        seletedkey <- dparams[dparams$normq == input$bw_q & dparams$normLen == input$bw_Len & dparams$normVmax2 == input$bw_Vmax2 & dparams$normRUE == input$bw_RUE & dparams$normMaxFix == bw_Fix, "keysc"]
        tabmoy <- ls_tabmoys[[input$nfert_level]][[seletedkey]]
        
        par(bg=colbg, col.lab=collab,col.axis=collab)
        
        plot(tabmoy$Semprop1, tabmoy$LER, type='b',ylim=c(0,2), main="", ylab="Land Equivalent Ratio", xlab="Sowing proportion (Esp. 1)", cex.lab=1., pch=0)
        
        rect(par("usr")[1], par("usr")[3],
             par("usr")[2], par("usr")[4],
             col = "#f7f7f7")
        
        col1 <- rgb(0,0,1,1/4)#4
        col2 <- rgb(1,0,0,1/4)#2
        
        x <- c(0., tabmoy$Semprop1, 1.)
        y <- c(0, tabmoy$LER1,0)
        polygon(x,y,col=col2)
        
        y <- c(0, tabmoy$LER2,0)
        polygon(x,y,col=col1)
        segments(0,1,1,1, type='l', lty=2)
        points(tabmoy$Semprop1, tabmoy$LER, type='b', lwd=2,pch=0)
        
        
        text(0.2, 1.85,"pLER1", col=col2)
        text(0.2, 1.7,"pLER2", col=col1)
        text(0.2, 2,"LER = pLER1 + pLER2", col=1)
        
        axis(1,col.ticks=collab)
        axis(2,col.ticks=collab)
        box(col=collab)
        
        
        
        
    })
    
    
    # input parameter visualisation
    output$table1 <- renderDataTable({
        
        if (input$groupN1 == "Legume") 
        {bw_Fix <- 1
        } else 
        {bw_Fix <- 0}
        
        var_ <- c("Len", "Vmax2", "q", "RUE")
        dat_ <- dparams[dparams$normq == input$bw_q & dparams$normLen == input$bw_Len & dparams$normVmax2 == input$bw_Vmax2 & dparams$normRUE == input$bw_RUE & dparams$normMaxFix == bw_Fix, var_]
        val_init <- c(3.4875, 0.05, 3.22, 2.3)
        delta <- as.numeric(dat_)
        df <- data.frame(Parameters = var_, Sp1=val_init, Sp2=val_init + delta, delta_Sp2Sp1=delta)

        
        df 
        
        
        }, options = list(pageLength = 4,dom = "t"))
    
    
    
    output$img_Len <- renderImage({
        
        if (input$bw_Len == 0.) 
        {imp <- "./www/Len_0.png"
        } else if(input$bw_Len == 1.)
        {imp <- "./www/Len_1.png"
        } else 
        {imp <- "./www/Len_-1.png"}
        
        
        list(src = imp,
             contentType = 'image/png',
             width = "100%",
             alt = "This is alternate text")
        
    }, deleteFile = FALSE)
    
    
    output$img_Vmax <- renderImage({
        
        if (input$bw_Vmax2 == 0.) 
        {imp <- "./www/Vmax2_0.png"
        } else if(input$bw_Vmax2 == 1.)
        {imp <- "./www/Vmax2_1.png"
        } else 
        {imp <- "./www/Vmax2_-1.png"}
        
        list(src = imp,
             contentType = 'image/png',
             width = "100%",
             alt = "This is alternate text")
        
    }, deleteFile = FALSE)
    
    
    output$img_q <- renderImage({
        
        if (input$bw_q == 0.) 
        {imp <- "./www/q_0.png"
        } else if(input$bw_q == 1.)
        {imp <- "./www/q_1.png"
        } else 
        {imp <- "./www/q_-1.png"}
        
        list(src = imp,
             contentType = 'image/png',
             width = "100%",
             alt = "This is alternate text")
        
    }, deleteFile = FALSE)
    
    
    output$img_MaxFix <- renderImage({
        
        if (input$groupN1 == "Legume") 
        {imp <- "./www/MaxFix_1.png"
        } else 
        {imp <- "./www/MaxFix_0.png"}
        
        list(src = imp,
             contentType = 'image/png',
             width = "100%",
             alt = "This is alternate text")
        
    }, deleteFile = FALSE)

    
    
    # Texte ----
    output$res_text6 <- renderText({
        paste0("Fig A: Parameter values for Sp.1 and Sp.2 in the selected scenario")
    })
    
    output$res_text1 <- renderText({
        paste0("Fig B: Total annual yield (Ytot, yellow) and partial yields of Sp1 (red) and Sp2. (blue) in response to sowing proportions in the selected scenario; dashed lines: theorical yields; solid lines: simulated yields")
    })
    
    output$res_text2 <- renderText({
        paste0("Fig C: Overyielding (aOY, yellow line and red dots) in response to sowing proportions in the selected scenario; Grey dots represent all other simulated mixtures grown in the same environment; black line: zero overyielding  of theoretical reference mixture")
    })
    
    output$res_text3 <- renderText({
        paste0("Fig D: Overyielding (aOY, black line and red dots) in response to sowing proportions in the selected scenario and decomposed in two additive components according to Loreau & Hector (2001): species complementarity effect (CE, blue) and selection effect (SE, red) ")
    })
    
    output$res_text4 <- renderText({
        paste0("Fig E: Land Equivalent Ratio (LER, black) and partial LER of Sp1 (red) and Sp2. (blue) in response to sowing proportions in the selected scenario; Horinzontal dashed line: reference for LER=1")
    })
    
    output$res_text5 <- renderText({
        paste0("Fig F: Overyielding (OY, yellow line and red dots) in response to actual species proportions in the selected scenario; Grey dots represent all other simulated mixtures grown in the same environment; black line: zero overyielding  of theoretical reference mixture")
    })
    
    output$res_text7 <- renderText({
        paste0("Fig G: ...")
    })
    
})
