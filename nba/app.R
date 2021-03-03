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


datos=read.csv("DatosNBA.csv", sep=";", dec=".")
datos %<>% mutate_at(c("Player", "Pos","Age","Tm"), as.factor)
datos_Player = levels(datos$Player) %>% str_sort()
datos_Pos = levels(datos$Pos) %>% str_sort()
datos_Age = levels(datos$Age) %>% str_sort()
datos_Tm = levels(datos$Tm) %>% str_sort()



dataPanel <- tabPanel("Data Players",
                      tableOutput("data")
)

plotPanel <- tabPanel("Plot",
                      fluidRow(
                          column(width = 8,
                                 plotOutput("plot",
                                            hover = hoverOpts(id = "plot_hover", delayType = "throttle"),
                                 )),
                          column(width = 4,
                                 verbatimTextOutput("plot_hoverinfo")
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

    }
    
    
    

shinyApp(ui = ui, server = server)
