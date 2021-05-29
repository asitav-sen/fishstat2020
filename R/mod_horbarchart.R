#' horbarchart UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#' @param datatoplot,title,xtitle,ytitle,src Parameters for calculation
#' @noRd 
#'
#' @importFrom shiny NS tagList
#' @importFrom shinycssloaders withSpinner
#' @importFrom plotly plot_ly plotlyOutput renderPlotly layout config event_data event_register
#' @importFrom magrittr %>%
#' 
mod_horbarchart_ui <- function(id){
  ns <- NS(id)
  tagList(
    shinycssloaders::withSpinner(plotly::plotlyOutput(ns("hbarchart"), height="900px"),image="./www/fish.gif", image.width = "75px")
  )
}
    
#' horbarchart Server Functions
#' @noRd 
mod_horbarchart_server <- function(id, datatoplot, title="plot", xtitle="", ytitle="", src="A"){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    output$hbarchart<-renderPlotly({
      datatoplot %>% 
        plot_ly(
          y =  ~name,
          x =  ~value,
          type = "bar",
          #opacity = 0.7, 
          source = src
        ) %>%
        layout(
          title= "",
          xaxis = list(title = xtitle, color = "white"),
          yaxis = list(title = ytitle, color = "white"),
          plot_bgcolor = 'transparent',
          paper_bgcolor = 'transparent',
          hoverlabel=list(bgcolor="black"),
          images = list(source = "./www/logo.png",
                        xref = "paper",
                        yref = "paper",
                        x = 0.8, y = 0.9,
                        sizex = 0.2, sizey = 0.2,
                        #opacity= 0.7,
                        xref = "paper", yref = "paper",
                        xanchor = "left", yanchor = "bottom"
          )
        ) %>% config(displayModeBar = FALSE) %>% 
        event_register("plotly_click")
    })
  })
}
    
## To be copied in the UI
# mod_horbarchart_ui("horbarchart_1")
    
## To be copied in the server
# mod_horbarchart_server("horbarchart_1")
