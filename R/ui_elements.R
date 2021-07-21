library(shiny)
library(shinythemes)
library(leaflet)
library(highcharter)
library(dplyr)
library(DT)
# Tab panels

maintab<- tabPanel(
  "Summary",
  div(class="outer",
      tags$head(
        includeCSS("./css/custom.css")
      ),
      leafletOutput("mainplot", width = "100%", height = "100%")
      ),
  absolutePanel(id = "prod", class = "panel panel-default", fixed = TRUE,
                draggable = TRUE, top = "auto", left = "auto", right = 20, bottom = 20,
                width = 750, height = "auto",
                tagList(
                  "Production Data",
                  highchartOutput("productionplot")
                )
                
  ),
  
  absolutePanel(id = "resources", class = "panel panel-default", fixed = TRUE,
                draggable = TRUE, top = 50, left = "auto", right = 20, bottom = "auto",
                width = 750, height = "auto",
                tagList(
                  textOutput("restitle"),
                  fluidRow(
                    column(width = 4,
                           highchartOutput("stateprod",height = "200px")
                           ),
                    column(width = 4,
                           highchartOutput("species", height = "200px")
                    ),
                    column(width = 4,
                           highchartOutput("resourcesplot", height = "200px")
                           )
                  ),
                  textOutput("riverlength")
                )
  )
)

detailtab<- tabPanel(
  "Other Details",
  tabsetPanel(
    type = "pills",
    tabPanel("Demand",
             fluidRow(
               column(
                 width = 6,
                 highchartOutput("indiaexport")
               ),
               column(
                 width = 6,
                 highchartOutput("globalprod")
               )
               
             ),
             br(),
             fluidRow(
               column(width = 6,
                      highchartOutput("consumption")
                      ),
               column(width = 6,
                      highchartOutput("vegpop")
                      )
             )
             ),
    tabPanel("Productivity",
             fluidRow(
               column(width = 6,
                      highchartOutput("productivity")),
               column(width = 6,
                      highchartOutput("trainingplot"))
             ),
             h2("States with lower than average productivity"),
             fluidRow(
               column(width = 6,
                      h3("Inland"),
                      DTOutput("inlandlow")),
               column(width = 6,
                      h3("Marine"),
                      DTOutput("marinelow"))
             )
             ),
    tabPanel("Other info",
             h3("Fishing Population"),
             fluidRow(
               column(width = 6,
                      leafletOutput("populationmap")),
               column(
                 width = 6,
                 highchartOutput("fishingperth")
                 
               )
                      ),
             h3("Investments"),
             fluidRow(
               column(width = 3,
                      radioButtons("newold","Select", choices = c("new","renovation","installation"), selected="new"),
                      radioButtons("unitarea","Select", choices = c("Unit","Area"), selected="Area"),
                      radioButtons("type","Select", choices = c("pond","cage pen","re-circulatory aquaculture"), selected="pond"),
                      "Empty chart indicates no data"
                      ),
               column(width = 9,
                      highcol("infrastructure")
                      )
             ),
             fluidRow(
               h3("Reliefs"),
               column(width = 6,
                      radioButtons("reliefsel","Select", choices = c("Fishermen", "Total Amount"), selected = "Total Amount", inline = T)),
               highcol("relief")
             ),
             fluidRow(
               h3("Fishseed Production"),
               highchartOutput("seedprod")
             )
             
  )
  
)
)