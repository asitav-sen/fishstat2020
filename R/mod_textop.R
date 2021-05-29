#' textop UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_textop_ui <- function(id){
  ns <- NS(id)
  tagList(
    textOutput(ns("desctext"))
 
  )
}
    
#' textop Server Functions
#'
#' @noRd 
mod_textop_server <- function(id, pre="",txt,post=""){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    output$desctext<-renderText({
      print(paste0(pre,txt,post))
    })
 
  })
}
    
## To be copied in the UI
# mod_textop_ui("textop_1")
    
## To be copied in the server
# mod_textop_server("textop_1")
