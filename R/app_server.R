#' The application server-side
#' 
#' @param input,output,session Internal parameters for {shiny}. 
#'     DO NOT REMOVE.
#' @noRd
#' @importFrom shiny renderUI strong
app_server <- function(input, output, session) {
    
    output$citation_text <- renderUI({
        if (!isTRUE(input$sidebarCollapsed)) {
            tagList(
                tags$p(strong("Citation:"), style = "color: #333; text-align: justify; margin-inline-start: 10px; margin-inline-end: 10px;"),
                tags$p("Chen H, Almeida-Silva F, Logghe G, Bonte D, Van de Peer Y. (2024). The rise of polyploids during environmental catastrophes.", tags$em("bioRxiv."), "DOI: xxx/yyy", style = "color: #333; text-align: justify; margin-inline-start: 10px; margin-inline-end: 10px;")
            )
        }
    })
    
    mod_explore_wgd_events_server("explore_wgd_events_ui_1")
    mod_wgd_by_species_server("wgd_by_species_1")
    mod_original_data_server("original_data_1")
    
}
