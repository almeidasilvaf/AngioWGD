#' wgd_by_species UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList helpText
#' @importFrom shinydashboard box
#' @importFrom shinyjs useShinyjs
#' @importFrom shinycssloaders withSpinner
#' @importFrom DT dataTableOutput
mod_wgd_by_species_ui <- function(id){
  ns <- NS(id)
  tagList(
      shinyjs::useShinyjs(),
      fluidRow(
          shinydashboard::box(
              solidHeader = TRUE,
              title = "WGD events per species",
              status = "success", width = 12,
              withSpinner(
                  DT::dataTableOutput(ns("wgd_byspecies")),
                  color = "#276c4c"
              ),
              helpText(
                  "Explore WGD events for each species. You can visualize", 
                  "posterior distribution(s) of WGD ages for a selected WGD event."
              )
          )
      )

       
  )
}
    
#' wgd_by_species Server Functions
#'
#' @noRd 
mod_wgd_by_species_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    # Define reactive values ----
    ## Table of WGD events
    wgd_table <- reactive({
        df <- wgd_dates |>
            dplyr::select(
                Species = species,
                `WGD ID` = wgd_id,
                `Ks peak` = ks_peak,
                `Ks credible range` = credible_range,
                `Posterior mean (WGD age)` = posterior_mean,
                `Posterior median (WGD age)` = posterior_median,
                `Posterior mode (WGD age)` = posterior_mode,
                `90% HPD` = x90_percent_hpd
            )
        
        df
    })
    
    
    # Render outputs
    output$wgd_byspecies <- DT::renderDataTable({
        DT::datatable(
            wgd_table(),
            selection = 'single',
            style = "bootstrap",
            rownames = FALSE,
            filter = 'top',
            options = list(
                lengthMenu = c(5, 10, 25, 50, 100), 
                pageLength = 5
            )
        )
    })
    
  })
}
    
## To be copied in the UI
# mod_wgd_by_species_ui("wgd_by_species_1")
    
## To be copied in the server
# mod_wgd_by_species_server("wgd_by_species_1")
