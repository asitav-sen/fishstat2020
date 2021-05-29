#' barchart UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#' @param datatoplot,title,xtitle,ytitle,src,orient Parameters for calculation
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
#' @importFrom shinycssloaders withSpinner
#' @importFrom plotly plot_ly plotlyOutput renderPlotly layout config event_data event_register
#' @importFrom magrittr %>%
mod_barchart_ui <- function(id){
  ns <- NS(id)
  tagList(
    shinycssloaders::withSpinner(plotly::plotlyOutput(ns("barchart"), height = "300px"), image="./www/fish.gif", image.width = "75px")
 
  )
}
    
#' barchart Server Functions
#' @noRd 
mod_barchart_server <- function(id, datatoplot, title="plot", xtitle="", ytitle="", src="A", orient="v"){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    output$barchart<- renderPlotly({
      if(ncol(datatoplot)==2){
      datatoplot %>% 
        plot_ly(
          x =  ~name,
          y =  ~value,
          type = "bar",
          #opacity = 0.7, 
          source = src,
          orientation = orient
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
                 x = 0, y = 0.8,
                 sizex = 0.2, sizey = 0.2,
                 #opacity= 0.7,
                 xref = "paper", yref = "paper",
                 xanchor = "left", yanchor = "bottom"
            )
        ) %>% config(displayModeBar = FALSE) %>% 
          event_register("plotly_click")
        
      }
      
      else {
        datatoplot %>% 
          plot_ly(
            x =  ~name,
            y =  ~value,
            type = "bar",
            color= ~Type,
            #opacity = 0.7, 
            source = src,
            orientation = orient
          ) %>%
          layout(
            #barmode = 'stack',
            title= "",
            xaxis = list(title = xtitle, color = "white"),
            yaxis = list(title = ytitle, color = "white"),
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
            ),
            legend = list(orientation = 'h', font=list(color = "white"))
          ) %>% config(displayModeBar = FALSE) %>% 
          event_register("plotly_click")
      }
      
    })
    

    # eventClick <- reactive(event_data("plotly_click", source = src))
    # return(eventClick)
 
  })
}
    
## To be copied in the UI
# mod_barchart_ui("barchart_1")
    
## To be copied in the server
# mod_barchart_server("barchart_1")
