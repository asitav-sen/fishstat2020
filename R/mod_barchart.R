library(highcharter)
library(shiny)

highcol <- function(id) {
  ns <- NS(id)
  highchartOutput(ns("colchart"))
}

highcolServer <- function(id, inputdata=NULL, series_name=NULL, ytitle=NULL,xtitle=NULL, plottitle=NULL, fname=NULL) {
  moduleServer(
    id,
    function(input, output, session) {
      validate(
        need(nrow(inputdata)>0, "No Data Available")
      )
      output$colchart<- renderHighchart({
        highchart() %>% 
          hc_xAxis(type = "category", title=list(text = xtitle)) %>% 
          hc_add_series(
            inputdata,
            "column",
            hcaes(x = name, y = value),
            name = series_name,
            showInLegend = F
          ) %>% 
          hc_yAxis(title = list(text = ytitle)) %>%
          hc_colors("white") %>%
          hc_title(text=plottitle,
                   style = list(color = "white")) %>% 
          hc_tooltip() %>%
          hc_add_theme(hc_theme_alone()) %>%
          hc_exporting(enabled = TRUE, # always enabled
                       filename = fname)
      })
      
    }
  )
}