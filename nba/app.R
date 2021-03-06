#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(shinyWidgets)
library(magrittr)
library(tableHTML)


datosA=read.csv("DatosNBA.csv", sep=";", dec=".")
datos=datosA[,2:30]
datos %<>% mutate_at(c("Player", "Pos","Age","Tm"), as.factor)
datos_Player = levels(datos$Player) %>% str_sort()
datos_Pos = levels(datos$Pos) %>% str_sort()
datos_Age = levels(datos$Age) %>% str_sort()
datos_Tm = levels(datos$Tm) %>% str_sort()
list_choicesr=colnames(datos)
list_choices=list_choicesr[5:29]

dataPanel <- tabPanel("Data Players",     tags$style(HTML("
      @import url('https://fonts.googleapis.com/css2?family=Yusei+Magic&display=swap');
      .navbar-default {
    background-color: beige !important;
}

.tabPanel-default:hover {
    background-color: lightslategray !important;
    color: yellow;
}")),
                      
                      tabsetPanel(type = "tabs",id="SET",
                                             
                                  tabPanel("Table", tableOutput("data")),
                                  

                                  tabPanel("Histogram" ,
                                           sidebarLayout(position="right",

                                                          sidebarPanel( 
                                           selectInput("select", label = h3("Plot by Stat"), 
                                                                                         choices = list_choices,
                                                                                         selected = 1)
                                                ),
                                           mainPanel(plotOutput("histSummary"))
                                           
                                            )
                                           ),
                                  tabPanel("Plot",
                                           sidebarLayout(position="right",
                                                         
                                                         sidebarPanel( 
                                                           selectInput("select2", label = h3("Plot by Stat 1"), 
                                                                       choices = list_choices,
                                                                       selected = 1),
                                                           
                                                           selectInput("select3", label = h3("Plot by Stat 2"), 
                                                                       choices = list_choices,
                                                                       selected = 5)
                                                         ),
                                                         
                                                      
                                           mainPanel(plotOutput("PlotPlayer", click = "plot_click"),
                                                                verbatimTextOutput("info"))
                                                     ) 
                                           
                                           ),
                                  tags$style(HTML("
      @import url('https://fonts.googleapis.com/css2?family=Yusei+Magic&display=swap');
       
                a{color: black !important;}
    
      


a:hover {
    color: yellow;
}"))
                      ))



plotPanel <- tabPanel("Plot",
                      fluidRow(
                          column(width = 8,
                                 plotOutput("plot",
                                            hover = hoverOpts(id = "plot_hover", delayType = "throttle"),
                                 )),
                          column(width = 4,
                                 verbatimTextOutput("plot_hoverinfo")
                          
                                 
                                 
                                 ),
                          

                          fluidPage(
                              
                              tags$h2("Add a shiny app background image"),
                              setBackgroundImage(
                                  src = "https://cdn.hipwallpaper.com/m/60/78/3NC5fa.jpg",
                              )
                          )
                          
                          
                      )
)
myHeader <- div(
    
    selectInput(
        inputId = "selPos",
        label = "Select the Position of the players",
        multiple = TRUE,
        choices = datos_Pos,
        selected = c(datos_Pos[1])
    ),
    selectInput(
        inputId = "selAge",
        label = "Select the Age of the players",
        multiple = TRUE,
        choices = datos_Age,
        selected = c(datos_Age[1])
    )  , 
    selectInput(
        inputId = "selTm",
        label = "Select the Team of the players",
        multiple = TRUE,
        choices = datos_Tm,
        selected = c(datos_Tm[1])
    )
    
)







ui <- navbarPage("shiny App",
            
                 dataPanel,
                 plotPanel,
                 header = myHeader
)


server <- function(input, output, session) { 
    datos2 <- reactive({datos %>%
                filter( Age %in% input$selAge,Pos %in% input$selPos,Tm %in% input$selTm)})
    output$data <- renderTable(datos2());
    a=reactive(as.vector(as.matrix(datos%>% select(input$select))))
    b=reactive(as.vector(as.matrix(datos%>% select(input$select2))))
    c=reactive(as.vector(as.matrix(datos%>% select(input$select3))))
    
    output$histSummary <- renderPlot(hist(a(),main="Histogram of data player",xlab=input$select))
    output$PlotPlayer <- renderPlot(plot(x=b(),y=c(),main="Plot of data player",xlab=input$select2,ylab=input$select3,col = "#00AFBB"))
    output$info <- renderText({
      paste0("x=", input$plot_click$x, "\ny=", input$plot_click$y)
    })
    
    
    
    }
    
    
    

shinyApp(ui = ui, server = server)
