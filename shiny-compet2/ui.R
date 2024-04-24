

# Define UI for application that draws a histogram
shinyUI(

    fluidPage(

    # Application title
    titlePanel(h2("Dashboard for exploring binary mixture performance ")),
    headerPanel(h4("Authors: Gaetan LOUARN")),
    headerPanel(h5("16/04/2024")),
    headerPanel(h5("article doi:...")),
    
    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
               
            
            radioButtons("cas", label = "Types of mixtures:", choices = ls_cases, selected = "NonFix-NonFix"),
            
            selectInput("myenv", label = "Environment:",  choices = ls_env, selected = "300N-Deep"),
            
            #radioButtons("depth", label = "Environment:", choices = c("Deep soil", "Shallow soil"), selected = "Deep soil"),
            
            #sliderInput("Nlevel", label = "mineral N:", min = 0, max = 300, value = 300., step = 150),
            
            selectInput("scenar1", label = "Sp1:",  choices = ls_spAll, selected = ls_spAll[1]),
            
            selectInput("scenar2", label = "Sp2:",  choices = ls_spAll, selected = ls_spAll[1]),
            
            #sliderInput("CV", label = "CV:", min = 0.001, max = 0.30, value = 0.001, step = 0.30),
            
        width=3),

        
        # gestion multi-panneau
        mainPanel(
        tabsetPanel(type = "tabs",id = "tabset",
                    tabPanel("A) Inputs(1)", plotOutput("res_plot6"), textOutput("res_text6")),
                    tabPanel("B) Inputs(2)", plotOutput("res_plot8"), textOutput("res_text8")),
                    tabPanel("C) Outputs overview", plotOutput("res_plot1"), textOutput("res_text1")),
                    tabPanel("D) LERp", plotOutput("res_plot2"), textOutput("res_text2")),
                    tabPanel("E) OY map", plotOutput("res_plot3"), textOutput("res_text3")),
                    tabPanel("F) CE-SE maps", plotOutput("res_plot7"), textOutput("res_text7")),
                    tabPanel("G) p50s map", plotOutput("res_plot4"), textOutput("res_text4")),
                    tabPanel("H) Win proba", plotOutput("res_plot5"), textOutput("res_text5")),
                    #tabPanel("test", plotOutput("res_plotTest"), textOutput("res_textTest")),
                    #
        ))
        
        
        
        
    ),
    
    
))
