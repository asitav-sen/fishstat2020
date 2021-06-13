#' infra UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#' @importFrom dplyr group_by summarize rename mutate select inner_join ungroup filter
#' @importFrom tidyr pivot_longer
#' @importFrom magrittr %>%
#' @import plotly
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_infra_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(
      radioButtons(ns("newreno"), "Select one", choices = c("new", "renovation"), selected = "new", inline = T),
      radioButtons(ns("noarea"), "Select one", choices = c("Units", "Area"), selected = "Area", inline = T),
      radioButtons(ns("resources"), "Select one", choices = c("pond", "cage pen", "re-circulatory aquaculture"), selected = "pond", inline = T)
    ),
      shinycssloaders::withSpinner(plotly::plotlyOutput(ns("hectares"), height = "300px"), image="./www/fish.gif", image.width = "75px")
      
  )
}
    
#' infra Server Functions
#'
#' @noRd 
mod_infra_server <- function(id, df){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
 output$hectares<- renderPlotly({
   a<-df %>% 
     filter(type==input$newreno) %>% 
     filter(resource==input$resources)
   fig<-
     if(input$noarea=="Area"){
       plot_ly(
         a,
         x = ~reorder(State, Area),
         y = ~Area,
         type = "bar",
         marker = list(color = "antiquewhite2",
                       line = list(color = 'white',
                                   width = 1.5))
       ) %>% 
         layout(
           title= "",
           xaxis = list(title = "", color = "white", showgrid=F),
           yaxis = list(title = "Area in ha", color = "white", showgrid=F),
           plot_bgcolor = 'transparent',
           paper_bgcolor = 'transparent',
           hoverlabel=list(bgcolor="black"),
           images = list(source = "./www/logo.png",
                         xref = "paper",
                         yref = "paper",
                         x = 0, y = 0.8,
                         sizex = 0.2, sizey = 0.2,
                         #opacity= 0.7,
                         xref = "paper", yref = "paper",
                         xanchor = "left", yanchor = "bottom"
           )
         ) %>% config(displayModeBar = FALSE)
     } else {
       
       plot_ly(
         a,
         x = ~reorder(State, Units),
         y = ~Units,
         type = "bar",
         marker = list(color = "antiquewhite2",
                       line = list(color = 'white',
                                   width = 1.5))
       ) %>% 
         layout(
           title= "",
           xaxis = list(title = "", color = "white", showgrid=F),
           yaxis = list(title = "Units", color = "white", showgrid=F),
           plot_bgcolor = 'transparent',
           paper_bgcolor = 'transparent',
           hoverlabel=list(bgcolor="black"),
           images = list(source = "./www/logo.png",
                         xref = "paper",
                         yref = "paper",
                         x = 0, y = 0.8,
                         sizex = 0.2, sizey = 0.2,
                         #opacity= 0.7,
                         xref = "paper", yref = "paper",
                         xanchor = "left", yanchor = "bottom"
           )
         ) %>% config(displayModeBar = FALSE)
     }

   
 })
  })
}
    
## To be copied in the UI
# mod_infra_ui("infra_1")
    
## To be copied in the server
# mod_infra_server("infra_1")
