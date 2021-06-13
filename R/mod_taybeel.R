#' taybeel UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#' @import DT
#' @importFrom shiny NS tagList 
#' @importFrom magrittr %>%
mod_taybeel_ui <- function(id){
  ns <- NS(id)
  tagList(
    DTOutput(ns("taybeel"))
  )
}
    
#' taybeel Server Functions
#'
#' @noRd 
mod_taybeel_server <- function(id, df){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    output$taybeel <- renderDT({
      
      df%>%
        datatable(
          rownames = FALSE,
          colnames=c("State", "Type", "Year", "Prod_inLac_T", "Fishing_pop", "Output_per_per_T"),
          class = "compact",
          #selection = list(mode="single",target = 'row', selected = c(1)),
          extensions = 'Responsive',
          style = "bootstrap",
          options = list(pageLength = 6)
        ) 
    }
    )
    
  })
}
    
## To be copied in the UI
# mod_taybeel_ui("taybeel_1")
    
## To be copied in the server
# mod_taybeel_server("taybeel_1")
