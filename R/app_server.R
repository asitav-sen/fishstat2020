#' The application server-side
#' 
#' @param input,output,session Internal parameters for {shiny}. 
#'     DO NOT REMOVE.
#' @import shiny 
#' @import waiter
#' @noRd
app_server <- function(input, output, session) {
  options(shiny.reactlog = TRUE)
  # Your application server logic
  # Import production data
  fish.prod <-
    read.csv(
      "https://docs.google.com/spreadsheets/d/e/2PACX-1vTd25DeVpaTzt2LvB1afoZ4b-GbbMIZDp4z0PQPmT8lsB-DqClxkmFu09INxn_MZ_z1ZnQNp8LWdIhv/pub?gid=1096991567&single=true&output=csv"
    )
  inland.resources<- read.csv(
    "https://docs.google.com/spreadsheets/d/e/2PACX-1vTd25DeVpaTzt2LvB1afoZ4b-GbbMIZDp4z0PQPmT8lsB-DqClxkmFu09INxn_MZ_z1ZnQNp8LWdIhv/pub?gid=837622468&single=true&output=csv"
                              )
  marine.resources<- read.csv(
    "https://docs.google.com/spreadsheets/d/e/2PACX-1vTd25DeVpaTzt2LvB1afoZ4b-GbbMIZDp4z0PQPmT8lsB-DqClxkmFu09INxn_MZ_z1ZnQNp8LWdIhv/pub?gid=1167008367&single=true&output=csv"
                              )
  prod.overall <- fish.prod.byyear(fish.prod)
  mod_barchart_server("prod_by_year", datatoplot = prod.overall, ytitle =
                        "Production (100k T)")
  prod.by.type <- fish.prod.yeartype(fish.prod)
  mod_barchart_server(
    "prod_by_type",
    datatoplot = prod.by.type,
    ytitle = "Production (100k T)",
    src = "B"
  )

  n.rows <- nrow(prod.overall)
  latest.prod <- prod.overall$value[n.rows]
  last.prod <- prod.overall$value[n.rows - 1]
  prod.growth <- round((latest.prod - last.prod) * 100 / last.prod, 2)
  mod_descbox_server(
    "descoverall",
    num = prod.growth,
    headr = paste0(latest.prod, " '100k T"),
    txt = "Total Production",
    rightBrdr = F,
    marginBttm = FALSE
  )
  
  ind.inland.resources<-india.inland.resources(inland.resources)
  
  mod_infobox_server("india_rivers",val=ind.inland.resources[ind.inland.resources$Type=="Rivers_Canals",4], subt="Rivers(km)", icn="dragon", clr="white", wd=12, hrf="")
  mod_infobox_server("india_sr",val=ind.inland.resources[ind.inland.resources$Type=="small_reser",3], subt="Small Reservior(Ha)", icn="joint", clr="white", wd=12, hrf="")
  mod_infobox_server("india_lr",val=ind.inland.resources[ind.inland.resources$Type=="med_large_reser",3], subt="Large Reservior(Ha)", icn="screwdriver", clr="white", wd=12, hrf="")
  mod_infobox_server("india_tp",val=ind.inland.resources[ind.inland.resources$Type=="tanks_ponds",3], subt="Ponds(Ha)", icn="arrows-alt", clr="white", wd=12, hrf="")
  mod_infobox_server("india_bw",val=ind.inland.resources[ind.inland.resources$Type=="brackish_water",3], subt="Brackish Water(Ha)", icn="bahai", clr="white", wd=12, hrf="")
  mod_infobox_server("india_ol",val=ind.inland.resources[ind.inland.resources$Type=="beel_oxbow_lakes",3], subt="Beel(Ha)", icn="beer", clr="white", wd=12, hrf="")
  mod_infobox_server("india_o",val=ind.inland.resources[ind.inland.resources$Type=="Other",3], subt="Other", icn="bowling-ball", clr="white", wd=12, hrf="")
  mod_infobox_server("india_c",val=sum(marine.resources$Coast_line_km, na.rm = T), subt="Coastline(km)", icn="ship", clr="white", wd=12, hrf="")
  
  
  
  event_click_year<-reactive(event_data("plotly_click", source = "A"))
  
  prod.by.state<-reactiveVal({
    fish.prod.by.state(fish.prod[fish.prod$Year=="2019-20",])
  })
  
  observeEvent(
    c(event_click_year(),prod.by.state()),
    {
      if(!is.null(event_click_year()))
      prod.by.state(
        fish.prod.by.state(fish.prod[fish.prod$Year==event_click_year()$x,])
        )
      mod_horbarchart_server(
        "prod_by_state",
        datatoplot = prod.by.state(),
        xtitle = "Production (100k T)",
        src = "C"
      )
      mod_textop_server("selyear",pre="Production by state for year: ",txt=ifelse(!is.null(event_click_year()$x),event_click_year()$x,"2019-20"))
    }
  )
  
  



   event_click_state<-reactive(event_data("plotly_click", source = "C"))

  sel.state.prod<-reactiveVal({
    fish.prod.byyear(fish.prod[fish.prod$State=="Andhra Pradesh",])
  })
  
  observeEvent(
    c(event_click_state(), sel.state.prod()),
    {
      if(!is.null(event_click_state())){
        sel.state.prod(
          fish.prod.byyear(fish.prod[fish.prod$State==event_click_state()$y,])
        )
      }
        
      mod_barchart_server(
        "prod_sel_state",
        datatoplot = sel.state.prod(),
        ytitle = "Production (100k T)",
        src = "D"
      )
      mod_textop_server("selstate",pre="Production by year of: ",txt=ifelse(!is.null(event_click_state()$y),event_click_state()$y,"Andhra Pradesh"))
      
    }
  )


  
  waiter_hide()
  
}
