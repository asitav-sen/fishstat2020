#' The application server-side
#' 
#' @param input,output,session Internal parameters for {shiny}. 
#'     DO NOT REMOVE.
#' @import shiny 
#' @import waiter
#' @import plotly
#' @noRd
app_server <- function(input, output, session) {
  options(shiny.reactlog = TRUE)
  # Your application server logic
  # Import production data
  fish.prod <-
    read.csv(
      "https://docs.google.com/spreadsheets/d/e/2PACX-1vTd25DeVpaTzt2LvB1afoZ4b-GbbMIZDp4z0PQPmT8lsB-DqClxkmFu09INxn_MZ_z1ZnQNp8LWdIhv/pub?gid=1096991567&single=true&output=csv"
    )
  fish.seed <- read.csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vTd25DeVpaTzt2LvB1afoZ4b-GbbMIZDp4z0PQPmT8lsB-DqClxkmFu09INxn_MZ_z1ZnQNp8LWdIhv/pub?gid=702957413&single=true&output=csv")
  inland.resources<- read.csv(
    "https://docs.google.com/spreadsheets/d/e/2PACX-1vTd25DeVpaTzt2LvB1afoZ4b-GbbMIZDp4z0PQPmT8lsB-DqClxkmFu09INxn_MZ_z1ZnQNp8LWdIhv/pub?gid=837622468&single=true&output=csv"
                              )
  marine.resources<- read.csv(
    "https://docs.google.com/spreadsheets/d/e/2PACX-1vTd25DeVpaTzt2LvB1afoZ4b-GbbMIZDp4z0PQPmT8lsB-DqClxkmFu09INxn_MZ_z1ZnQNp8LWdIhv/pub?gid=1167008367&single=true&output=csv"
                              )
  species.prod<- read.csv(
    "https://docs.google.com/spreadsheets/d/e/2PACX-1vTd25DeVpaTzt2LvB1afoZ4b-GbbMIZDp4z0PQPmT8lsB-DqClxkmFu09INxn_MZ_z1ZnQNp8LWdIhv/pub?gid=259951981&single=true&output=csv"
  )
  
  fish.eng<- read.csv(
    "https://docs.google.com/spreadsheets/d/e/2PACX-1vTd25DeVpaTzt2LvB1afoZ4b-GbbMIZDp4z0PQPmT8lsB-DqClxkmFu09INxn_MZ_z1ZnQNp8LWdIhv/pub?gid=1393998625&single=true&output=csv"
  )
  
  fish.gen<- read.csv(
    "https://docs.google.com/spreadsheets/d/e/2PACX-1vTd25DeVpaTzt2LvB1afoZ4b-GbbMIZDp4z0PQPmT8lsB-DqClxkmFu09INxn_MZ_z1ZnQNp8LWdIhv/pub?gid=1352918917&single=true&output=csv"
  )
  
  fish.dis<- read.csv(
    "https://docs.google.com/spreadsheets/d/e/2PACX-1vTd25DeVpaTzt2LvB1afoZ4b-GbbMIZDp4z0PQPmT8lsB-DqClxkmFu09INxn_MZ_z1ZnQNp8LWdIhv/pub?gid=2034371076&single=true&output=csv"
  )
  
  cons<- read.csv(
    "https://docs.google.com/spreadsheets/d/e/2PACX-1vTd25DeVpaTzt2LvB1afoZ4b-GbbMIZDp4z0PQPmT8lsB-DqClxkmFu09INxn_MZ_z1ZnQNp8LWdIhv/pub?gid=1938251657&single=true&output=csv"
  )
  
  ind.export<-read.csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vTd25DeVpaTzt2LvB1afoZ4b-GbbMIZDp4z0PQPmT8lsB-DqClxkmFu09INxn_MZ_z1ZnQNp8LWdIhv/pub?gid=54953417&single=true&output=csv")
  
  global.prod<-read.csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vTd25DeVpaTzt2LvB1afoZ4b-GbbMIZDp4z0PQPmT8lsB-DqClxkmFu09INxn_MZ_z1ZnQNp8LWdIhv/pub?gid=141134708&single=true&output=csv")
  
  veg.pop<- read.csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vTd25DeVpaTzt2LvB1afoZ4b-GbbMIZDp4z0PQPmT8lsB-DqClxkmFu09INxn_MZ_z1ZnQNp8LWdIhv/pub?gid=1213657637&single=true&output=csv")
  
  trainings<- read.csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vTd25DeVpaTzt2LvB1afoZ4b-GbbMIZDp4z0PQPmT8lsB-DqClxkmFu09INxn_MZ_z1ZnQNp8LWdIhv/pub?gid=386582880&single=true&output=csv")
  
  infra<- read.csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vTd25DeVpaTzt2LvB1afoZ4b-GbbMIZDp4z0PQPmT8lsB-DqClxkmFu09INxn_MZ_z1ZnQNp8LWdIhv/pub?gid=1680610277&single=true&output=csv")
  
  relief.dat<- read.csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vTd25DeVpaTzt2LvB1afoZ4b-GbbMIZDp4z0PQPmT8lsB-DqClxkmFu09INxn_MZ_z1ZnQNp8LWdIhv/pub?gid=1310395062&single=true&output=csv")
  
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
  
  mod_infobox_server("india_rivers",val=ind.inland.resources[ind.inland.resources$Type=="Rivers_Canals",4], subt="Rivers(km)", icn="dragon", clr="lightblue", wd=12, hrf="")
  mod_infobox_server("india_sr",val=ind.inland.resources[ind.inland.resources$Type=="small_reser",3], subt="Small Reservior(Ha)", icn="joint", clr="lightblue", wd=12, hrf="")
  mod_infobox_server("india_lr",val=ind.inland.resources[ind.inland.resources$Type=="med_large_reser",3], subt="Large Reservior(Ha)", icn="screwdriver", clr="lightblue", wd=12, hrf="")
  mod_infobox_server("india_tp",val=ind.inland.resources[ind.inland.resources$Type=="tanks_ponds",3], subt="Ponds(Ha)", icn="arrows-alt", clr="lightblue", wd=12, hrf="")
  mod_infobox_server("india_bw",val=ind.inland.resources[ind.inland.resources$Type=="brackish_water",3], subt="Brackish Water(Ha)", icn="bahai", clr="lightblue", wd=12, hrf="")
  mod_infobox_server("india_ol",val=ind.inland.resources[ind.inland.resources$Type=="beel_oxbow_lakes",3], subt="Beel(Ha)", icn="beer", clr="lightblue", wd=12, hrf="")
  mod_infobox_server("india_o",val=ind.inland.resources[ind.inland.resources$Type=="Other",3], subt="Other", icn="bowling-ball", clr="lightblue", wd=12, hrf="")
  mod_infobox_server("india_c",val=sum(marine.resources$Coast_line_km, na.rm = T), subt="Coastline(km)", icn="ship", clr="lightblue", wd=12, hrf="")
  mod_infobox_server("inland_fe",
                     val=paste0(round(sum(fish.gen[fish.gen$Type=="Inland" & fish.gen$Gender=="Female",4],na.rm=T)*100/sum(fish.gen[fish.gen$Type=="Inland",4], na.rm = T),2)," %"), 
                     subt="Female in Inland", icn="venus", clr="lightblue", wd=12, hrf="")
  mod_infobox_server("marine_fe",
                     val=paste0(round(sum(fish.gen[fish.gen$Type=="Marine" & fish.gen$Gender=="Female",4],na.rm=T)*100/sum(fish.gen[fish.gen$Type=="Marine",4], na.rm = T),2)," %"), 
                     subt="Female in Marine", icn="venus", clr="lightblue", wd=12, hrf="")
  
  ind.prod.spe<-spe.prod(species.prod)
  
  mod_barchart_server(
    "prod_by_spe",
    datatoplot = ind.prod.spe,
    ytitle = "Production (100k T)",
    src = "F"
  )
  
  #ind.fish.eng<-fish.eng.o(fish.eng)
  ind.fish.seed<-ind.fish.seed(fish.seed)
  mod_barchart_server(
    "fish_seed",
    datatoplot = ind.fish.seed,
    ytitle = "Fish Seed Production",
    src = "G"
  )
  
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
   
   sel.state<-reactiveVal("Andhra Pradesh")

  sel.state.prod<-reactiveVal({
    fish.prod.byyear(fish.prod[fish.prod$State=="Andhra Pradesh",])
  })
  state.prod.by.type<- reactiveVal({
    fish.prod.yeartype(fish.prod[fish.prod$State=="Andhra Pradesh",])
  })
  state.prod.species<- reactiveVal({
    spe.prod(species.prod[species.prod$State=="Andhra Pradesh",])
  })
  
  observeEvent(event_click_state(),{
    if(!is.null(event_click_state())){
      sel.state(event_click_state()$y)
    }
  })
  
  observeEvent(
    sel.state(),
    {
        sel.state.prod <- reactive(
          fish.prod.byyear(fish.prod[fish.prod$State==sel.state(),])
        )
        
        state.prod.by.type<- reactive(
          fish.prod.yeartype(fish.prod[fish.prod$State==sel.state(),])
        )
        
        state.prod.species<- reactive(
          spe.prod(species.prod[species.prod$State==sel.state(),])
          )
        
      mod_barchart_server(
        "prod_sel_state",
        datatoplot = sel.state.prod(),
        ytitle = "Production (100k T)",
        src = "D"
      )
      mod_textop_server("selstate",pre="Production of: ",txt=ifelse(!is.null(event_click_state()$y),event_click_state()$y,"Andhra Pradesh"))
    
      mod_barchart_server(
        "state_prod_by_type",
        datatoplot = state.prod.by.type(),
        ytitle = "Production (100k T)",
        src = "E"
      )
      
      mod_barchart_server(
        "state_by_spe",
        datatoplot = state.prod.species(),
        ytitle = "Production (100k T)",
        src = "G"
      )
      
      mod_infobox_server("state_rivers",
                         val=inland.resources[inland.resources$Type=="Rivers_Canals" & 
                                                    inland.resources$State==sel.state(),5], 
                         subt="Rivers(km)", icn="dragon", clr="lightblue", wd=12, hrf="")
      mod_infobox_server("state_sr",val=inland.resources[inland.resources$Type=="small_reser" & 
                                                               inland.resources$State==sel.state(),4], 
                         subt="Small Reservior(Ha)", icn="joint", clr="lightblue", wd=12, hrf="")
      mod_infobox_server("state_lr",val=inland.resources[inland.resources$Type=="med_large_reser" & 
                                                               inland.resources$State==sel.state(),4],
                         subt="Large Reservior(Ha)", icn="screwdriver", clr="lightblue", wd=12, hrf="")
      mod_infobox_server("state_tp",val=inland.resources[inland.resources$Type=="tanks_ponds" & 
                                                               inland.resources$State==sel.state(),4], 
                         subt="Ponds(Ha)", icn="arrows-alt", clr="lightblue", wd=12, hrf="")
      mod_infobox_server("state_bw",val=inland.resources[inland.resources$Type=="brackish_water" & 
                                                               inland.resources$State==sel.state(),4], 
                         subt="Brackish Water(Ha)", icn="bahai", clr="lightblue", wd=12, hrf="")
      mod_infobox_server("state_ol",val=inland.resources[inland.resources$Type=="beel_oxbow_lakes" & 
                                                               inland.resources$State==sel.state(),4], 
                         subt="Beel(Ha)", icn="beer", clr="lightblue", wd=12, hrf="")
      mod_infobox_server("state_o",val=inland.resources[inland.resources$Type=="Other" & 
                                                              inland.resources$State==sel.state(),4], 
                         subt="Other", icn="bowling-ball", clr="lightblue", wd=12, hrf="")
      mod_infobox_server("state_c",val=sum(marine.resources[marine.resources$State==sel.state(),]$Coast_line_km, na.rm = T), 
                         subt="Coastline(km)", icn="ship", clr="lightblue", wd=12, hrf="")
      mod_infobox_server("state_inland_fe",
                         val=paste0(round(sum(fish.gen[fish.gen$Type=="Inland" & fish.gen$Gender=="Female" & 
                                                         fish.gen$State==sel.state(),4],na.rm=T)*100/sum(fish.gen[fish.gen$Type=="Inland",4], na.rm = T),2)," %"), 
                         subt="Female in Inland", icn="venus", clr="lightblue", wd=12, hrf="")
      mod_infobox_server("state_marine_fe",
                         val=paste0(round(sum(fish.gen[fish.gen$Type=="Marine" & fish.gen$Gender=="Female" &
                                                         fish.gen$State==sel.state(),4],na.rm=T)*100/sum(fish.gen[fish.gen$Type=="Marine",4], na.rm = T),2)," %"), 
                         subt="Female in Marine", icn="venus", clr="lightblue", wd=12, hrf="")
      
      mod_textop_server("selstateresources",pre="Resources snapshot of: ",txt=ifelse(!is.null(event_click_state()$y),event_click_state()$y,"Andhra Pradesh"))
      mod_textop_server("selstateotherdetails",pre="Fish Seed Production: ",txt=ifelse(!is.null(event_click_state()$y),event_click_state()$y,"Andhra Pradesh"))
      state.fish.seed<- reactive(ind.fish.seed(fish.seed[fish.seed$State==sel.state(),]))
      mod_barchart_server(
        "state_fish_seed",
        datatoplot = state.fish.seed(),
        ytitle = "Fish Seed Production",
        src = "H"
      )
      
      
    }
  )
  

  # Advanced


  mod_barchart_server("indexp",ind.export, title="Export by India (MT)", xtitle="", ytitle="MT of fish", src="I", orient="v", lo='h')
  mod_barchart_server("globalprod",global.prod, title="Global Prod", xtitle="", ytitle="MT of fish", src="J", orient="v", lo='h')
  
  
  
  output$consumption<- renderPlotly({
    cons.cal(cons) %>% 
      plot_ly(
        x =  ~name,
        y =  ~value,
        type = "bar",
        color = ~type,
        #colors = c("antiquewhite2", "azure3", "gray96","khaki3", "lightblue1", "gray67"),
        #opacity = 0.7, 
        marker = list(
                      line = list(color = 'white',
                                  width = 1.5))
      ) %>%
      layout(
        title= "",
        xaxis = list(title = "", color = "white", showgrid=F),
        yaxis = list(title = "Per capita annual consumption (kg)", color = "white", showgrid=F),
        plot_bgcolor = 'transparent',
        paper_bgcolor = 'transparent',
        hoverlabel=list(bgcolor="black"),
        legend = list(orientation = "v", font=list(color = "white")),
        images = list(source = "./www/logo.png",
                      xref = "paper",
                      yref = "paper",
                      x = 0, y = 0.8,
                      sizex = 0.2, sizey = 0.2,
                      #opacity= 0.7,
                      xref = "paper", yref = "paper",
                      xanchor = "left", yanchor = "bottom"
        ),
        shapes= list(
          type="line",
          line = list(color = "red"),
          y0 = 20.3, 
          y1 = 20.3, 
          yref = "y",
          xref="paper",
          x0 = 0,
          x1 = 33
        ),
        annotations = list(x=10, y=23, text="Global Level is 20.3 kg per annum",showarrow = F,
                           font=list(size=14,color="white"))
      ) %>% config(displayModeBar = FALSE)
  })
  
  mod_barchart_server("vegpop", veg.cal(veg.pop), xtitle="", ytitle="Population",lo="v")
  
  out.p.fis.in<-op.p.fish(df=fish.prod[fish.prod$Year=="2019-20" & fish.prod$Type =="Inland",], df2=fish.gen)
  out.p.fis.ma<-op.p.fish(df=fish.prod[fish.prod$Year=="2019-20" & fish.prod$Type =="Marine",], df2=fish.gen)
  avg.in<- sum(out.p.fis.in$value2)*100000/sum(out.p.fis.in$fishin_pop)
  avg.ma<- sum(out.p.fis.ma$value2)*100000/sum(out.p.fis.ma$fishin_pop)
  
  mod_overlayedbar_server("in_out_p_fish", 
                          datatoplot= out.p.fis.in, 
                          ytitle="Tonnes per person & Total", 
                          avg=avg.in, src="I")
  mod_overlayedbar_server("ma_out_p_fish", 
                          datatoplot= out.p.fis.ma, 
                          ytitle="Tonnes per person & Total", 
                          avg=avg.ma, src="J")
  
  mod_taybeel_server("InlandFiltered", out.p.fis.in[out.p.fis.in$value>avg.in,])
  mod_taybeel_server("MarineFiltered", out.p.fis.ma[out.p.fis.ma$value>avg.ma,])
  
  mod_barchart_server("trainings", train.cal(trainings), xtitle="", ytitle="Trained People",lo="v")
  
  mod_barchart_server("totfishpop", fishing.pop.cal(fish.dis), xtitle="", ytitle="Fishing population",lo="v")
  mod_barchart_server("fishpoppert", fishing.pop.per.cal(fish.dis), xtitle="", ytitle="Fishing population per thousand",lo="v")
  
  mod_infra_server("infradetails",infra(infra))
  mod_relief_server("relief",relief.dat)
  
  waiter_hide()
  
}
