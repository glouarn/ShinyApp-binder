
#

library(shiny)
library("shinyWidgets")
library("here")

#dtoto
path_ <- here::here()
path_
dat2 <- read.table(paste0(path_ ,"/", "tabletoto_corr.csv"), header=T, sep=";", dec=".")

#moyenne
tabres <- read.table(paste0(path_,"/","tabre_moy.csv"), header=T, sep=";", dec=".")

ls_traits <- unique(dat2$nomTrait)
ls_mng <- unique(dat2$MngN)



# Define UI for application that draws a histogram
shinyUI(

    fluidPage(

    # Application title
    titlePanel("Impact of intraspecific genetic variability on interspecific competition: a theorical case study on binary mixtures"),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
               
            selectInput("traitN", label = "Environment:",  choices = ls_mng, selected = "0N"),
            
            
            radioButtons("scenar", label = "Parameter Scenarios:", 
                         choices = ls_traits[c(8,3,4,9,2,5,6,7,1)], selected = "T1-Len"),
            
            sliderInput("CV", label = "CV:",
                        min = 0.001, max = 0.30, value = 0.001, step = 0.15),
            
            sliderInput("delta", label = "delta:",
                        min = -1.5, max = 1.5, value = 0., step = 0.5),
        ),

        
        # gestion multi-panneau
        mainPanel(
        tabsetPanel(type = "tabs",
                    tabPanel("A) Inputs", plotOutput("res_plot6"), textOutput("res_text6")),
                    tabPanel("B) Yield", plotOutput("res_plot1"), textOutput("res_text1")),
                    tabPanel("C) Sp. proportion", plotOutput("res_plot2"), textOutput("res_text2")),
                    tabPanel("D) Gini", plotOutput("res_plot3"), textOutput("res_text3")),
                    tabPanel("E) D5param", plotOutput("res_plot4"), textOutput("res_text4")),
                    tabPanel("F) DM correlations", plotOutput("res_plot5"), textOutput("res_text5")),
                    tabPanel("G) CE-SE", plotOutput("res_plot7"), textOutput("res_text7")),
                    
        ))
        
        
        # Show a plot of the generated distribution
        #mainPanel(
        #    plotOutput("distPlot")
        #)
    )
))
