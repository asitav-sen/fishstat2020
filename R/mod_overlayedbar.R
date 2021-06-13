#' overlayedbar UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
#' @importFrom shinycssloaders withSpinner
#' @importFrom plotly plot_ly plotlyOutput renderPlotly layout config event_data event_register add_trace
mod_overlayedbar_ui <- function(id){
  ns <- NS(id)
  tagList(
    shinycssloaders::withSpinner(plotly::plotlyOutput(ns("obarchart")),image="./www/fish.gif", image.width = "75px")
  )
}
    
#' overlayedbar Server Functions
#'
#' @noRd 
mod_overlayedbar_server <- function(id, datatoplot, title="plot", xtitle="", ytitle="", src="A", avg=NULL){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    output$obarchart<-renderPlotly({
     # print(head(datatoplot))
      datatoplot %>% 
        plot_ly(
          x =  ~name,
          y =  ~value,
          type = "bar",
          #color= ~Type,
          #colors = c("dodgerblue", "cadetblue3", "cornflowerblue","deepskyblue3", "lightblue1", "darkslategray3"),
          #opacity = 0.7, 
          source = src,
          name = "Output (T) per Person",
          marker = list(#color = 'transparent',
            line = list(color = 'white',
                        width = 1.5))
        ) %>%
        add_trace(
          x =  ~name,
          y =  ~value2,
          #color= ~Type,
          #colors = c("firebrick1", "indianred1", "lightsalmon1","magenta2", "orangered1", "red1"),
          type = "bar",
          width = 0.3, 
          name = "Production in 100k T",
          marker = list(#color = 'transparent',
            line = list(color = 'red',
                        width = 1.5))
        ) %>% 
        layout(
          title= "",
          barmode = 'overlay',
          xaxis = list(title = xtitle, color = "white", showgrid=F),
          yaxis = list(title = ytitle, color = "white", showgrid=F),
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
          ),
          annotations = list(x=8, y=max(max(datatoplot$value1, na.rm = T),max(datatoplot$value2, na.rm = T)), text=paste0("Avg. Output per person: ",round(avg,2)," Tonnes per person"),showarrow = F,
                             font=list(size=10,color="white")),
          legend = list(x = 0.1, y = 0.9, font=list(color = "white"))
        ) %>% config(displayModeBar = FALSE) %>% 
        event_register("plotly_click")
    })
 
  })
}
    
## To be copied in the UI
# mod_overlayedbar_ui("overlayedbar_1")
    
## To be copied in the server
# mod_overlayedbar_server("overlayedbar_1")
