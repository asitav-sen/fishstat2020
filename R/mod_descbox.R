#' descbox UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_descbox_ui <- function(id){
  ns <- NS(id)
  tagList(
    uiOutput(ns("descbox"))
 
  )
}
    
#' descbox Server Functions
#'
#' @noRd 
mod_descbox_server <- function(id, 
                               num = NULL,
                               headr = NULL,
                               txt = NULL,
                               rightBrdr = TRUE,
                               marginBttm = FALSE){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    output$descbox<- renderUI({
      icn<-ifelse(num>=0,"caret-up","caret-down")
      descriptionBlock(
        number = paste0(num," %"),
        numberColor = ifelse(num>=0,"olive","warning"),
        numberIcon = icon(icn),
        header = headr,
        text = txt,
        rightBorder = rightBrdr,
        marginBottom = marginBttm
      )
    })
 
  })
}
    
## To be copied in the UI
# mod_descbox_ui("descbox_1")
    
## To be copied in the server
# mod_descbox_server("descbox_1")
