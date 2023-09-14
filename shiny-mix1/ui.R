

# Define UI for application that draws a histogram
shinyUI(

    fluidPage( theme = shinytheme("darkly"),

    # Application title
    titlePanel(h2("VGL mixture explorer")),
    headerPanel(h4("Dataset from 'Towards intercrop ideotypes: non-random trait assembly can promote overyielding and stability of species proportion in simulated legume-based mixtures' ")),
    headerPanel(h4("Authors: Gaetan LOUARN")),
    headerPanel(h5("09/08/2018")),
    tags$a(href="https://academic.oup.com/aob/article/126/4/671/5719429", "Louarn et al. (2020)"),
    headerPanel(h5(" ")),
    
    
    img(src="qr-code-binder-mix1.png" , height="20%", width="20%", align="right"),
    img(src="img-mix1.png" ,  height="40%", width="40%", align="left"),
    headerPanel(h5("article doi: 10.1093/aob/mcaa014")),
    
    
    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
               
            
            radioButtons("nfert_level", label = "Environment:",  
                         choices = c('0N', '120N', '300N'), selected = '120N'),
            
            selectInput("groupN1", label = "Sp1. functional group:",  choices = c("Legume", "Non-legume"), selected = "Legume"),
            
            
            selectInput("groupN2", label = "Sp2. functional group:",  choices = c("Non-legume"), selected = "Non-legume"),
            

            
            sliderInput("bw_Len", label = "Light foraging ('Len'):", 
                        min = -1, max = 1., value = 0., step = 1.),
            
            sliderInput("bw_Vmax2", label = "Soil N foraging ('Vmax2'):", 
                        min = -1, max = 1., value = 0., step = 1.),
            
            sliderInput("bw_q", label = "Temporal growth ('q'):", 
                        min = -1, max = 1., value = 0., step = 1.),
            
            sliderInput("bw_RUE", label = "Ressource use ('RUEmax'):", 
                        min = -1., max = 0., value = 0., step = 0.5),
            
            #sliderInput("bw_Fix", label = "Ressource use (Fixation rate 'FixMax', g DM.MJ-1):", 
            #            min = 0., max = 1., value = 0., step = 1.),
            
        width=3),

        
        # gestion multi-panneau
        mainPanel(
        tabsetPanel(type = "tabs",id = "tabset",
                    tabPanel("A) Inputs", dataTableOutput("table1"), textOutput("res_text6")),
                    tabPanel("B) Biomass Prod.", plotOutput("res_plot1"), textOutput("res_text1")),
                    tabPanel("C) Overyielding", plotOutput("res_plot2"), textOutput("res_text2")),
                    tabPanel("D) CE-SE", plotOutput("res_plot4"), textOutput("res_text3")),
                    tabPanel("E) LER", plotOutput("res_plot5"), textOutput("res_text4")),
                    #tabPanel("F) Actual OY", plotOutput("res_plot3"), textOutput("res_text3")),
                    
                    #tabPanel("G) CE-SE", plotOutput("res_plot7"), textOutput("res_text7")),
                    #tabPanel("test", plotOutput("res_plotTest"), textOutput("res_textTest")),
                    
        ))
        
        
        
      
    ),
    
    
))
