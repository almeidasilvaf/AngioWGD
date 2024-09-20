#' original_data UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
#' @importFrom DT dataTableOutput
mod_original_data_ui <- function(id){
  ns <- NS(id)
  tagList(
      fluidRow(
          shinydashboardPlus::box(
              solidHeader = TRUE,
              title = "Species included in this resource",
              status = "success", width = 12,
              withSpinner(
                  DT::dataTableOutput(ns("species_table")),
                  color = "#276c4c"
              ),
              hr(),
              helpText(
                  "Explore taxonomic information for all species", 
                  "included in this resource and download FASTA and GFF3 files",
                  "with CDS and gene annotation, respectively."
              )
          )
      )
 
  )
}
    
#' original_data Server Functions
#'
#' @importFrom DT renderDataTable datatable
#' @noRd 
mod_original_data_server <- function(id) {
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    # Define reactive values
    ## Data frame of taxonomic information for each species
    
    # Render tables
    output$species_table <- DT::renderDataTable({
        
        dt <- DT::datatable(
            data_urls,
            selection = 'single',
            style = "bootstrap",
            colnames = c("Species", "Family", "Order", "Clade", "CDS URL", "Annotation URL"),
            rownames = FALSE,
            escape = FALSE,
            filter = 'top',
            options = list(
                lengthMenu = c(5, 10, 25, 50, 100), 
                pageLength = 5
            )
        )
        
        dt
        
    })
    
 
  })
}
    
## To be copied in the UI
# mod_original_data_ui("original_data_1")
    
## To be copied in the server
# mod_original_data_server("original_data_1")
