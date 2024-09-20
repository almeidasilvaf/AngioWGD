#' wgd_by_species UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList helpText br downloadButton
#' @importFrom shinyjs useShinyjs
#' @importFrom shinycssloaders withSpinner
#' @importFrom DT dataTableOutput
mod_wgd_by_species_ui <- function(id) {
    ns <- NS(id)
    tagList(
        shinyjs::useShinyjs(),
        fluidRow(
            shinydashboardPlus::box(
                solidHeader = TRUE,
                title = "WGD events per species",
                status = "success", width = 12,
                withSpinner(
                    DT::dataTableOutput(ns("wgd_byspecies")),
                    color = "#276c4c"
                ),
                fluidRow(column(
                    6, offset = 5,
                    actionButton(
                        ns("button_plot"),
                        label = "Show age distributions"
                    )
                )),
                hr(),
                helpText(
                    "Explore WGD events for each species. You can visualize", 
                    "posterior age distribution(s) for a WGD event by selecting",
                    "a row and clicking the 'Show age distributions' button."
                ),
                sidebar = shinydashboardPlus::boxSidebar(
                    id = ns("wgd_byspecies_sidebar"),
                    width = 25,
                    icon = shiny::icon("download"),
                    fluidRow(
                        column(
                            7, selectInput(
                                ns("tableformat"),
                                label = "Choose file format:",
                                choices = c(".tsv", ".tsv.gz")
                            )
                        ),
                        column(
                            5, 
                            shiny::br(),
                            downloadButton(ns("download_table"), label = "Download")
                        )
                    )
                )
            )
        ),
        # Another box to be shown if button is clicked
        shinyjs::hidden(
            div(
                id = ns("plot_box"),
                shinydashboardPlus::box(
                    solidHeader = TRUE, status = "success", width = 12,
                    title = "Posterior distributions of WGD age",
                    plotOutput(ns("age_distros")),
                    sidebar = shinydashboardPlus::boxSidebar(
                        id = ns("age_distros_sidebar"),
                        width = 25,
                        icon = shiny::icon("download"),
                        selectInput(
                            ns("figformat"),
                            label = "Choose file format:",
                            choices = c(".pdf", ".svg")
                        ),
                        column(
                            6,
                            numericInput(
                                ns("figwidth"), 
                                label = "Figure width:",
                                value = 10, min = 3, max = 30, step = 1
                            )
                        ),
                        column(
                            6,
                            numericInput(
                                ns("figheight"), 
                                label = "Figure height:",
                                value = 5, min = 3, max = 30, step = 1
                            )
                        ),
                        column(
                            6, offset = 3,
                            downloadButton(ns("download_fig"), label = "Download")
                        )
                    )
                )
            )
            
        )
        
        
    )
}
    
#' wgd_by_species Server Functions
#'
#' @noRd 
#' @importFrom patchwork wrap_plots
#' @importFrom stats setNames
#' @importFrom ggplot2 ggsave
#' @importFrom utils write.table
#' @importFrom shiny downloadHandler req moduleServer observe observeEvent
mod_wgd_by_species_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    # Define reactive values ----
    ## Table of WGD events
    wgd_table <- reactive({
        
        df <- wgd_dates
        df$species <- gsub("_", " ", df$species)
        df$wgd_id <- as.factor(df$wgd_id)
        scols <- stats::setNames(
            c("wgd_id", "species", "ks_peak", "credible_range", "posterior_mean", 
              "posterior_median", "posterior_mode", "x90_percent_hpd", 
              "consensus_peak", "x90_percent_hcr"),
            c("WGD ID", "Species", "Ks peak", "Ks credible range", 
              "Posterior mean (WGD age)", "Posterior median (WGD age)", 
              "Posterior mode (WGD age)", "90% HPD", "Consensus peak", 
              "90% HCR")
        ) 
        df <- df[, scols]
        names(df) <- names(scols)

        df
    })
    
    ## Age distributions per species
    distro_plot <- reactive({
        req(selected_wgd())
        
        ## Plot 1: by species
        pdata1 <- posterior_hist$byspecies 
        pdata1 <- pdata1[pdata1$WGD_ID == selected_wgd(), ]
        
        p1 <- plot_age_distro(pdata1)
        
        ## Plot 2: all combined
        pdata2 <- posterior_hist$combined 
        pdata2 <- pdata2[pdata2$WGD_ID == selected_wgd(), ]

        p2 <- plot_consensus_age_distro(pdata2)
        
        ## Combining plots
        final_plot <- patchwork::wrap_plots(p1, p2, nrow = 1)
        
        final_plot
    })
    
    # Show plot box if button is clicked
    observe({
        toggleState("button_plot", !is.null(input$wgd_byspecies_rows_selected))
    })
    
    selected_wgd <- reactive({
        w <- NULL
        if(input$button_plot && !is.null(input$wgd_byspecies_rows_selected)) {
            idx <- input$wgd_byspecies_rows_selected
            w <- as.character(wgd_table()[idx, 1])
        }
        
        w
    })
    
    observeEvent(input$button_plot, {
        shinyjs::toggle("plot_box")
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
    
    output$age_distros <- renderPlot({
        distro_plot()
    }, res = 96)
    
    
    ## Download figure with age distros to PDF/SVG file
    output$download_fig <- downloadHandler(
        filename = function() {
            paste0("posterior_age_distros_", selected_wgd(), input$figformat)
        },
        content = function(file) {
            ggsave(
                distro_plot(), filename = file, 
                width = input$figwidth, height = input$figheight
            )
        }
    )
    
    ## Download table with WGD info per species
    output$download_table <- downloadHandler(
        filename = function() {
            paste0("wgds_per_species", input$tableformat)
        },
        content = function(file) {
            write.table(
                wgd_table(), file = file, quote = FALSE, sep = "\t", 
                row.names = FALSE
            )
        }
    )
    
    
  })
}
    
## To be copied in the UI
# mod_wgd_by_species_ui("wgd_by_species_1")
    
## To be copied in the server
# mod_wgd_by_species_server("wgd_by_species_1")
