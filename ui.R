library(shiny)
library(shinydashboard)
library(mapview)

header <- dashboardHeader(title="Endroits avec le plus d'accidents impliquant des piétons et des vélos rapportés à la police, 2011-2017")

sidebar <- dashboardSidebar(
)

body <- dashboardBody(
  # Define UI for application
  fluidPage(
    mainPanel(
      leafletOutput("mapplot"),
      mapview:::plainViewOutput("test")
    ))
)

ui <- dashboardPage(header, sidebar, body, skin="black")

# library(shiny)
# library(sf)
# library(leaflet)
# library(tidyverse)
# 
# # Define UI for application that plots random distributions 
# shinyUI(fluidPage(
#   
#   fluidRow( 
#     column(1,
#            h2("header"),
#            "texte",
#            tags$a("url", href="https://ici.radio-canada.ca/nouvelle/665247/mon-trajet-telechargement-application"),
#            p(),
#     ),
#     column(2, 
#            textInput(inputId= "polygone", 
#                      label= "Encoded Polyline", 
#                      value="cgn|GpporLsWgYvUgg@~Lqz@|~@g{AovAi`Cra@gr@|MnXtMyJ}_@gaAo|@ooBwUyCwr@m{Ael@ylAqd@y_AaQa`@cSyl@_OdQjAbDOpRxBhN}ZnSf@vMwN|LePkOiK|So^rZwH~IqQ`S}|@bv@oi@pe@qj@`_@ub@bh@"),  
#            actionButton(inputId = "recalc", 
#                         label = "Mettre à jour (environ 60 secondes)")
#     )
#   ),
#   p(),
#   leafletOutput("mymap")#,
#   # p(),
#   # fluidRow(
#   #   column(4, tableOutput("mytable")),
#   #   column(8, plotOutput("myplot"))
#   # )
#   
# )
# )