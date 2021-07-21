library(highcharter)
prod.chart<-readRDS("./data/production_chart.rds")
states<-readRDS("./data/indiastates.rds")
bins <-
  c(0,
    1,
    5,
    10,
    25,
    50,
    Inf)
m<- readRDS("./data/mainmap.rds")

state_names<-readRDS("./data/state_names.rds")

dat<-states@data
fish.prod<-readRDS("./data/fishprod.rds")

thm<-hc_theme_merge(
  hc_theme_alone(),
  hc_theme(chart=
             list(
               backgroundColor = "transparent"   
             )
  )
)

india_export_plot<-readRDS("./data/india_export.rds")
global_production<-readRDS("./data/global_production.rds")

consumption.plot<-readRDS("./data/consumption_plot.rds")

veg.plot<-readRDS("./data/veg_plot.rds")
productivity.plot<-readRDS("./data/productivity_plot.rds")


lowmarine<-readRDS("./data/lowmarine.rds")
lowinland<-readRDS("./data/lowinland.rds")

training.plot<-readRDS("./data/trainingplot.rds")

fishingpopulation<-readRDS("./data/fishingpopulationmap.rds")

fishingpopperthous<-readRDS("./data/fishingpoppert.rds")

infra<- readRDS("./data/infra.rds")
relief.dat<- readRDS("./data/relief.rds")

seed.chart<-readRDS("./data/seed_chart.rds")


binsnew <-
  c(0,
    10000,
    200000,
    500000,
    1000000,
    2000000,
    Inf)
fish.dis<- readRDS("./data/districts.rds")


