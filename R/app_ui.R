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
      #fresh::use_theme("./www/daark.css"),
      header = dashboardHeader(
        title=dashboardBrand(
          title = "India FishStat 2020",
          color = "primary",
          href = "https://adminlte.io/themes/v3",
          image = "/www/logo.png"
        )#,
        #skin = "dark"
      ),
      sidebar = dashboardSidebar(
        #skin = "dark",
        collapsed = TRUE
      ),
      controlbar = dashboardControlbar(),
      footer = dashboardFooter(
        left = a(
          href = "https://asitavsen.com",
          target = "_blank", "Build a dashboard like this"
        ),
        right = "2020"
      ),
      body = dashboardBody(
        fluidRow(
          column(
            width = 6,
            box(
              title= "Fish Production India",
              solidHeader = T,
              status= "secondary",
              "Click and drag to zoom. Click on bar to see state-wise production of that year below",
              mod_barchart_ui("prod_by_year"),
              mod_barchart_ui("prod_by_type"),
              background="gray-dark",
              width=12,
              footer=mod_descbox_ui("descoverall")
            )
            
          ),
          column(
            width = 6,
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
                mod_infobox_ui("india_tp")
              ),
              column(
                width=6,
                mod_infobox_ui("india_bw"),
                mod_infobox_ui("india_ol"),
                mod_infobox_ui("india_o"),
                mod_infobox_ui("india_c")
              )
              )
            )

          )
        ),
        fluidRow(
          column(
            width = 6,
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
            width = 6,
            box(
              title= mod_textop_ui("selstate"),
              solidHeader = T,
              status= "secondary",
              mod_barchart_ui("prod_sel_state"),
              background="gray-dark",
              width=12
            )
          )
        )
      ),
      dark = F,
      scrollToTop = TRUE,
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
#   theme="default",
#   fresh::bs4dash_vars(
#     navbar_light_color = "#bec5cb",
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
#   output_file = "daark.css"
# )

