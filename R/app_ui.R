#' The application User-Interface
#' 
#' @param request Internal parameter for `{shiny}`. 
#'     DO NOT REMOVE.
#' @import shiny
#' @import bs4Dash
#' @import waiter
#' @importFrom fresh use_theme
#' @noRd
app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # Your application UI logic 
    
    dashboardPage(
      fresh::use_theme("./www/daark.css"),
      tags$head(includeHTML(("ga.html"))),
      header = dashboardHeader(
        title=dashboardBrand(
          title = "India FishStat 2020",
          color = "primary",
          href = "",
          image = "./www/logo.png"
        ),
        skin = "dark"
      ),
      sidebar = dashboardSidebar(
        #skin = "dark",
        collapsed = TRUE,
        sidebarMenu(
          id="sdbar",
          menuItem(
            "Basic",
            tabName="basic",
            icon=icon("search"),
            selected = T
          ),
          menuItem(
            "Advanced",
            tabName="advanced",
            icon=icon("binoculars")
          ),
          menuItem(
            "Meet Jaljeevika",
            badgeLabel = "Sponsor",
            badgeColor = "danger",
            href = "https://www.jaljeevika.org/",
            icon=icon("wallet")
          ),
          menuItem(
            "Meet Me",
            badgeLabel = "Consultant",
            badgeColor = "success",
            href = "https://www.asitavsen.com/",
            icon=icon("laptop-code")
          ),
          menuItem(
            "Old reports",
            href = "https://asitavsen.com/files/india-fisheries-report/",
            icon=icon("file")
          )
        )
      ),
      controlbar = NULL,
      footer = dashboardFooter(
        left = a(
          href = "https://asitavsen.com",
          target = "_blank", "Build a dashboard like this"
        ),
        right = a(
          href = "https://github.com/asitav-sen/fishstat2020",
          target = "_blank", "Code in Github. MIT license"
        )
      ),
      body = dashboardBody(
        tabItems(
          tabItem(
            tabName= "basic",
            fluidRow(
              column(
                width = 4,
                box(
                  title= "Fish Production India",
                  solidHeader = T,
                  status= "secondary",
                  mod_barchart_ui("prod_by_type"),
                  "Click and drag to zoom. Click on bar to see state-wise production of that year below",
                  mod_barchart_ui("prod_by_year"),
                  background="gray-dark",
                  width=12,
                  footer=mod_descbox_ui("descoverall")
                )
                
              ),
              column(
                width = 4,
                box(
                  title= "Production in 2019-20 by type",
                  solidHeader = T,
                  status = "secondary",
                  background = "gray-dark",
                  width=12,
                  mod_barchart_ui("prod_by_spe")
                ),
                box(
                  title= "Fishing population",
                  solidHeader = T,
                  status = "secondary",
                  background = "gray-dark",
                  width=12,
                  mod_barchart_ui("fish_seed")
                )
              ),
              column(
                width = 4,
                box(
                  title= "Resources Snapshot",
                  solidHeader = T,
                  status= "secondary",
                  background="gray-dark",
                  width=12,
                  fluidRow(
                    column(
                      width=6,
                      mod_infobox_ui("india_rivers"),
                      mod_infobox_ui("india_sr"),
                      mod_infobox_ui("india_lr"),
                      mod_infobox_ui("india_tp"),
                      mod_infobox_ui("inland_fe")
                    ),
                    column(
                      width=6,
                      mod_infobox_ui("india_bw"),
                      mod_infobox_ui("india_ol"),
                      mod_infobox_ui("india_o"),
                      mod_infobox_ui("india_c"),
                      mod_infobox_ui("marine_fe")
                    )
                  )
                )
                
              )
            ),
            fluidRow(
              column(
                width = 4,
                box(
                  title= mod_textop_ui("selyear"),
                  solidHeader = T,
                  status= "secondary",
                  "Click and drag to zoom. Click on bar to see yearly production of the state",
                  mod_horbarchart_ui("prod_by_state"),
                  background="gray-dark",
                  width=12
                )
              ),
              column(
                width = 4,
                box(
                  title= mod_textop_ui("selstate"),
                  solidHeader = T,
                  status= "secondary",
                  mod_barchart_ui("prod_sel_state"),
                  mod_barchart_ui("state_prod_by_type"),
                  mod_barchart_ui("state_by_spe"),
                  background="gray-dark",
                  width=12
                )
              ),
              column(
                width = 4,
                box(
                  title=mod_textop_ui("selstateresources"),
                  solidHeader = T,
                  status = "secondary",
                  width =12,
                  fluidRow(
                    column(
                      width=6,
                      mod_infobox_ui("state_rivers"),
                      mod_infobox_ui("state_sr"),
                      mod_infobox_ui("state_lr"),
                      mod_infobox_ui("state_tp"),
                      mod_infobox_ui("state_inland_fe")
                    ),
                    column(
                      width=6,
                      mod_infobox_ui("state_bw"),
                      mod_infobox_ui("state_ol"),
                      mod_infobox_ui("state_o"),
                      mod_infobox_ui("state_c"),
                      mod_infobox_ui("state_marine_fe")
                    )
                  )
                ),
                box(
                  title=mod_textop_ui("selstateotherdetails"),
                  solidHeader = T,
                  status = "secondary",
                  background = "gray-dark",
                  width = 12,
                  mod_barchart_ui("state_fish_seed")
                )
              )
            ),
          ),
          tabItem(
            tabName= "advanced",
            fluidRow(
              
              tabBox(
                id = "tabcard",
                title = "Advanced Viz",
                selected = "Demand",
                status = "secondary",
                solidHeader = TRUE,
                background="gray-dark",
                type = "pills",
                width =12,
                tabPanel(
                  title = "Demand",
                  fluidRow(
                    column(
                      width = 6,
                      h4("Export By India"),
                      br(),
                      mod_barchart_ui("indexp")
                    ),
                    column(
                      width = 6,
                      br(),
                      h4("Global Production"),
                      mod_barchart_ui("globalprod")
                    )
                  ),
                  tags$hr(),
                  br(),
                  h4("Consumption in India"),
                  br(),
                  shinycssloaders::withSpinner(plotly::plotlyOutput("consumption", height = "300px"), image="./www/fish.gif", image.width = "75px"),
                  br(),
                  tags$hr(),
                  br(),
                  h4("Vegetarian population in India"),
                  br(),
                  mod_barchart_ui("vegpop"),
                  br(),
                  "Income distribution required for accurate policy making"
                  
                ),
                tabPanel(
                  title="Productivity (Output per fisherwoman)",
                  fluidRow(
                    column(
                      width = 6,
                      h4("Inland"),
                      br(),
                      mod_overlayedbar_ui("in_out_p_fish"),
                      br(),
                      h4("List of States with less than average productivity"),
                      br(),
                      br(),
                      mod_taybeel_ui("InlandFiltered")
                    ),
                    column(
                      width = 6,
                      h4("Marine"),
                      mod_overlayedbar_ui("ma_out_p_fish"),
                      br(),
                      h4("List of States with less than average productivity"),
                      br(),
                      br(),
                      mod_taybeel_ui("MarineFiltered")
                    )
                  ),
                  tags$hr(),
                  br(),
                  h4("No. of people trained since 2015-16"),
                  br(),
                  mod_barchart_ui("trainings")
                ),
                tabPanel(
                  title = "Fishing Population",
                  fluidRow(
                    column(
                      width = 6,
                      h4("Total fishing population"),
                      br(),
                      #mod_barchart_ui("totfishpop"),
                      htmlOutput("totfishpop"),
                    ),
                    column(
                      width = 6,
                      h4("Fishing population per 1000"),
                      br(),
                      mod_barchart_ui("fishpoppert"),
                    )
                  ),
                  br(),
                  tags$hr(),
                  br(),
                  h4("Infrastructure Investments since 2015-16"),
                  br(),
                  mod_infra_ui("infradetails"),
                  br(),
                  tags$hr(),
                  br(),
                  h4("Relief provided since 2015-16"),
                  br(),
                  mod_relief_ui("relief")
                )
            )
            )
          )
        ),
      ),
      dark = T,
      scrollToTop = TRUE,
      #skin="midnight",
      fullscreen=T,
      preloader= 
        list(html = img(src=paste0("./www/fish.gif")), 
             #color = "#3483ee"
             color=transparent(.5)
             )
    )
  )
}

#' Add external Resources to the Application
#' 
#' This function is internally used to add external 
#' resources inside the Shiny application. 
#' 
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function(){
  
  add_resource_path(
    'www', app_sys('app/www')
  )
 
  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys('app/www'),
      app_title = 'fishstat2020'
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert() 
  )
}


# fresh::create_theme(
#   fresh::bs4dash_vars(
#     navbar_light_color = "#000000",
#     navbar_light_active_color = "#FFF",
#     navbar_light_hover_color = "#FFF"
#   ),
#   fresh::bs4dash_yiq(
#     contrasted_threshold = 10,
#     text_dark = "#FFF",
#     text_light = "#272c30"
#   ),
#   fresh::bs4dash_layout(
#     main_bg = "#353c42"
#   ),
#   fresh::bs4dash_sidebar_light(
#     bg = "#272c30",
#     color = "#bec5cb",
#     hover_color = "#FFF",
#     submenu_bg = "#272c30",
#     submenu_color = "#FFF",
#     submenu_hover_color = "#FFF"
#   ),
#   fresh::bs4dash_status(
#     primary = "#5E81AC", danger = "#BF616A", light = "#272c30"
#   ),
#   fresh::bs4dash_color(
#     gray_900 = "#FFF"
#   ),
#   output_file="daark.css"
# )

