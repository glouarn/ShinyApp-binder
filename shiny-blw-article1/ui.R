

# Define UI for application that draws a histogram
shinyUI(

    fluidPage(

    # Application title
    titlePanel(h2("Impact of intraspecific genetic variation on interspecific competition: a theorical case study of binary mixtures")),
    headerPanel(h4("Authors: Beatrice WOLFF, Bernadette JULIER & Gaetan LOUARN")),
    headerPanel(h5("14/10/2022")),
    headerPanel(h5("article doi:...")),
    
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
        width=3),

        
        # gestion multi-panneau
        mainPanel(
        tabsetPanel(type = "tabs",id = "tabset",
                    tabPanel("A) Inputs", plotOutput("res_plot6"), textOutput("res_text6")),
                    tabPanel("B) Yield", plotOutput("res_plot1"), textOutput("res_text1")),
                    tabPanel("C) Sp. proportion", plotOutput("res_plot2"), textOutput("res_text2")),
                    tabPanel("D) Gini", plotOutput("res_plot3"), textOutput("res_text3")),
                    tabPanel("E) D5param", plotOutput("res_plot4"), textOutput("res_text4")),
                    tabPanel("F) Biomass Cor.", plotOutput("res_plot5"), textOutput("res_text5")),
                    tabPanel("G) CE-SE", plotOutput("res_plot7"), textOutput("res_text7")),
                    
        ))
        
        
        
        
    ),
    
    
))
