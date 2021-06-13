#' relief UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_relief_ui <- function(id){
  ns <- NS(id)
  tagList(
 radioButtons(ns("relieft"),"Select One", choices=c("fishermen","total_amount"), inline = T),
 shinycssloaders::withSpinner(plotly::plotlyOutput(ns("relief"), height = "300px"), image="./www/fish.gif", image.width = "75px")
 
  )
}
    
#' relief Server Functions
#'
#' @noRd 
mod_relief_server <- function(id,df){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 output$relief<- renderPlotly({
   a<-df %>% 
     group_by(State) %>% 
     summarize(fishermen=sum(fishermen, na.rm = T), total_amount=sum(total_amount, na.rm = T))
   
   if(input$relieft =="fishermen"){
     b<-a %>% 
       select(State, fishermen) %>% 
       rename(Value=fishermen)
   } else {
     b<-a %>% 
       select(State, total_amount) %>% 
       rename(Value=total_amount)
   }
   
   plot_ly(
     b,
     x= ~reorder(State, Value),
     y= ~Value,
     type = "bar",
     marker = list(color = "antiquewhite2",
                   line = list(color = 'white',
                               width = 1.5))
   ) %>% 
     layout(
       title= "",
       xaxis = list(title = "", color = "white", showgrid=F),
       yaxis = list(title = ifelse(input$relieft=="fishermen","No. of Fishermen", "Amount Disbursed"), color = "white", showgrid=F),
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
 })
  })
}
    
## To be copied in the UI
# mod_relief_ui("relief_1")
    
## To be copied in the server
# mod_relief_server("relief_1")
