#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinythemes)
library(leaflet)
library(highcharter)
library(dplyr)
library(tidyr)
library(DT)

source("./R/mod_barchart.R")
source("./R/helpers.R")
source("./R/ui_elements.R")
source("./R/server_elements.R")



# Define UI for application that draws a histogram
ui <- tagList(
    tags$head(includeScript("navAppend.js")),
    navbarPage(
        # Defining name of the App
        title="India FishStat 2020",
        position = "static-top",
        # Daaark theme
        theme = shinytheme("slate"),
        maintab,
        detailtab,
        navbarMenu(title = "Meet the Sponsor"
                   ),
        navbarMenu(title = "Get a dashboard like this"
                   ),
        navbarMenu(title = "Previous Report"
                   )
    )
    
)
    

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$mainplot<-renderLeaflet({
        dt <- states
        pal <-
            colorBin("Blues",
                     domain = dt@data$total_prod,
                     bins = bins)
        labels <- 
            sprintf(
                "<strong>%s</strong><br/>Click on the state to see more details<br/>Production in lac(100 k) Tonne<br/>Total : %g<br/>Inland: %g <br/> Marine: %g",
                dt@data$ST_NM,
                dt@data$total_prod,
                dt@data$Inland,
                dt@data$Marine
            ) %>% lapply(htmltools::HTML)
        
        leaflet(dt) %>%
            addProviderTiles("CartoDB.DarkMatterNoLabels", group = "OSM") %>%
            addPolygons(
                fillColor = ~ pal(total_prod),
                weight = 1,
                opacity = 1,
                color = "white",
                dashArray = "3",
                fillOpacity = 0.7,
                highlight = highlightOptions(
                    weight = 2,
                    color = "#666",
                    dashArray = "",
                    fillOpacity = 0.7,
                    bringToFront = TRUE
                ),
                label = labels,
                labelOptions = labelOptions(
                    style = list("font-weight" = "normal", padding = "3px 8px"),
                    textsize = "15px",
                    direction = "auto"
                ),
                layerId = ~ dt@data$ST_NM
            ) %>% 
            setView(lat=24.813193, lng = 99.998796, zoom = 5)
       
    })
    
    output$productionplot<-renderHighchart({
        prod.chart
    })
    
    sel.state <- reactive({
        ifelse(is.null(input$mainplot_shape_click),
               state_names,
               unlist(input$mainplot_shape_click))
    })
    
    output$restitle<-renderText({paste0("Details: ",sel.state())})
    output$stateprod<- renderHighchart({
        req(sel.state())
        fp<-fish.prod %>% 
            filter(State %in% sel.state()) %>% 
            group_by(Year) %>% 
            summarise(prod=sum(Prod_in_lac_tonne, na.rm = T))
        highchart() %>% 
            hc_xAxis(type = "category") %>% 
            hc_add_series(
                fp,
                "area",
                hcaes(x = Year, y = prod),
                name = "India Production",
                showInLegend = F
            ) %>% 
            hc_title(text="Production by year",
                     style = list(color = "white", fontSize=2)) %>% 
            #hc_yAxis(title = list(text = "Production in lac (100k) Tonne")) %>%
            hc_colors("white") %>%
            hc_tooltip() %>%
            hc_add_theme(thm) %>%
            hc_exporting(enabled = TRUE, # always enabled
                         filename = "Production_Data")
            
    })
    
    output$resourcesplot <- renderHighchart({
        req(sel.state())
        dt<- dat[dat$ST_NM==sel.state(),]
        dt %>% 
            select(c(ST_NM,small_reser,med_large_reser,tanks_ponds,brackish_water,beel_oxbow_lakes,Other.x)) %>% 
            pivot_longer(cols = c(small_reser,med_large_reser,tanks_ponds,brackish_water,beel_oxbow_lakes,Other.x)) %>% 
            mutate(name=c("Small Reserviour", "Medium/Large Reserviour", "Tanks/Ponds", "Brackish Water",
                          "Beel/Oxbow/Lakes","Other")) %>% 
            select(-1) %>% 
            hchart("columnpyramid", hcaes(x=name,y=value),name="Area in Hectare") %>% 
            hc_colors("white") %>%
            hc_xAxis(type = "category", title=list(text = "")) %>% 
            hc_yAxis(title = list(text = "Area (Ha)")) %>% 
            hc_title(text="Inland resources",
                     style = list(color = "white", fontSize=2)) %>% 
            hc_tooltip() %>%
            hc_add_theme(thm) %>%
            hc_exporting(enabled = TRUE, # always enabled
                         filename = "inland resources")
        
    })
    
    output$species <- renderHighchart({
        req(sel.state())
        dt<- dat[dat$ST_NM==sel.state(),]
        dt %>% 
            select(c(ST_NM,`Major Carps`,`Minor Carps`,`Exotic Carps`,`Murrel`,`Catfish`, `Other.y`)) %>% 
            pivot_longer(cols=c(`Major Carps`,`Minor Carps`,`Exotic Carps`,`Murrel`,`Catfish`, `Other.y`)) %>% 
            mutate(name=c("Major Carps","Minor Carps","Exotic Carps","Murrel","Catfish","Other")) %>% 
            hchart("pie", hcaes(x=name,y=value), name="Lac (100k) Tonne") %>% 
            #hc_colors("white") %>%
            hc_title(text="Inland production 2019-20",
                     style = list(color = "white", fontSize=2)) %>% 
            hc_tooltip() %>%
            hc_add_theme(thm) %>%
            hc_exporting(enabled = TRUE, # always enabled
                         filename = "species")
            
    })
    
    output$riverlength<-renderText({
        req(sel.state())
        dt<- dat[dat$ST_NM==sel.state(),]
        paste0("River Length: ", dt$Rivers_Canals," km")
    })
    
    output$indiaexport<-renderHighchart({
        india_export_plot
    })
    
    output$globalprod<-renderHighchart({
        global_production
    })

    output$consumption<-renderHighchart({
        consumption.plot
    })
    
    output$vegpop<-renderHighchart({
        veg.plot
    })
    
    output$productivity<-renderHighchart({
        productivity.plot
    })
    
    output$inlandlow<-renderDT({
        lowinland %>% 
            datatable(
                style = "bootstrap",
                options = list(
                    pageLength = 5,
                    scrollX = TRUE)
            )
    })
    
    output$marinelow<-renderDT({
        lowmarine %>% 
            datatable(
                style = "bootstrap",
                options = list(
                    pageLength = 5,
                    scrollX = TRUE)
            )
    })
    
    output$trainingplot<- renderHighchart({
        training.plot
    })
    
    output$populationmap<- renderLeaflet({
        binsnew <-
            c(0,
              10000,
              200000,
              500000,
              1000000,
              2000000,
              Inf)
        
        dt <- states
        dt@data<-
            dt@data%>% 
            left_join(fish.dis, by=c("ST_NM"="State"))
        dt@data$fishermen<-dt@data$Inland_Male+dt@data$Inland_Female+dt@data$Marine_Male+dt@data$Marine_Female
        pal <-
            colorBin("Blues",
                     domain = dt@data$fishermen,
                     bins = binsnew)
        labels <- 
            sprintf(
                "<strong>%s</strong><br/>Total fishing population : %g<br/>Fishing population per 1000 : %g",
                dt@data$ST_NM,
                dt@data$fishermen,
                dt@data$fish_per_1000
            ) %>% lapply(htmltools::HTML)
        
        
        leaflet(dt) %>%
            addProviderTiles("CartoDB.DarkMatterNoLabels", group = "OSM") %>%
            addPolygons(
                fillColor = ~ pal(fishermen),
                weight = 1,
                opacity = 1,
                color = "white",
                dashArray = "3",
                fillOpacity = 0.7,
                highlight = highlightOptions(
                    weight = 2,
                    color = "#666",
                    dashArray = "",
                    fillOpacity = 0.7,
                    bringToFront = TRUE
                ),
                label = labels,
                labelOptions = labelOptions(
                    style = list("font-weight" = "normal", padding = "3px 8px"),
                    textsize = "15px",
                    direction = "auto"
                )
            )
    })
    
    output$fishingperth<- renderHighchart({
        fishingpopperthous
    })
    
    infra.dat<- reactive({
        infr<-
            infra %>% 
            filter(resource==input$type) %>% 
            filter(type==input$newold) %>% 
            group_by(State, resource, type) %>% 
            summarise(Area_ha=sum(Area_ha,na.rm = T), Number=sum(Number,na.rm = T)) %>% 
            ungroup()
        if(input$unitarea=="Area") {
            dt<- infr %>% 
                select(c(State,Area_ha)) %>% 
                rename(name=State, value=Area_ha) %>% 
                arrange(value)
        } else {
            dt<- infr %>% 
                select(c(State,Number)) %>% 
                rename(name=State, value=Number) %>% 
                arrange(value)
        }
        return(dt)
    })
    
    observeEvent(c(input$type,input$newold,input$unitarea),{
        highcolServer("infrastructure",inputdata=infra.dat(), series_name=input$unitarea, 
                      ytitle=ifelse(input$unitarea=="Area","Area_ha","Units"),xtitle="", plottitle="Investments in Infrastructure since 2015-16",
                      fname="Infra")
    })
    
    
    relief.dt<- reactive({
        relief<-
            relief.dat %>% 
            group_by(State) %>% 
            summarise(total_amount=sum(total_amount,na.rm = T), fishermen=sum(fishermen, na.rm = T)) %>% 
            ungroup()
        
        if(input$reliefsel=="Fishermen") {
            dt<- relief.dat %>% 
                select(State, fishermen) %>% 
                rename(name=State, value=fishermen) %>% 
                filter(value>0) %>% 
                arrange(value)
        } else {
            dt<- relief.dat %>% 
                select(State, total_amount) %>% 
                rename(name=State, value=total_amount) %>% 
                filter(value>0) %>%
                arrange(value)
        }
        return(dt)
    })
    
    observeEvent(input$reliefsel,{
        highcolServer("relief",inputdata=relief.dt(), series_name=input$reliefsel, 
                      ytitle=input$reliefsel,xtitle="", plottitle="Reliefs provided since 2015-16",
                      fname="Relief")
    })
    
    output$seedprod<- renderHighchart({
        seed.chart
    })
    
    
}

# Run the application 
shinyApp(ui = ui, server = server)
