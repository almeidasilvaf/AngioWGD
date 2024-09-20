#' global_expression_viewer UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList fluidRow column selectizeInput a div
#' sliderInput actionButton helpText downloadButton plotOutput
#' selectInput numericInput updateSelectizeInput
#' @importFrom shinydashboardPlus box boxSidebar
#' @importFrom shinyjs hidden show useShinyjs onclick
#' @importFrom shinycssloaders withSpinner
#' @importFrom shinyWidgets radioGroupButtons switchInput
#' @importFrom DT dataTableOutput 
mod_explore_wgd_events_ui <- function(id) {
    
    ns <- NS(id)
    tagList(
        shinyjs::useShinyjs(),
        # Create a center-aligned box ------------------------------------------
        fluidRow(
            column(
                width = 8, offset = 4,
                # Center-aligned box
                shinydashboardPlus::box(
                    status = "success",
                    solidHeader = TRUE, title = "Query options",
                    ## Row 1 of the box: dropdown button to select part(s)
                    fluidRow(
                        column(
                            12, offset = 0,
                            selectizeInput(
                                ns("input_clade"),
                                label = "Clade:",
                                choices = NULL
                            )
                        )
                    ),
                    ## Row 2
                    fluidRow(
                        column(
                            12, offset = 0,
                            shinyWidgets::radioGroupButtons(
                                inputId = ns("layout"),
                                label = "Tree layout", 
                                choices = c("Circular", "Rectangular"),
                                status = "primary",
                                justified = TRUE
                            )
                        )
                    ),
                    # add button "Show advanced options" to show extra options when clicking
                    # Extra options: "Highlight age range:" - sliderInput like SEA to choose min and max
                    
                    fluidRow(
                        column(
                            12, offset = 0,
                            a(id = ns("show_advanced"), "Show/hide advanced options", href = "#"),
                            shinyjs::hidden(
                                div(
                                    id = ns("advanced"),
                                    div(style = "display: flex; align-items: center;",
                                        column(
                                            9, offset = 0,
                                            sliderInput(
                                                ns("age_range"),
                                                "Highlight age range",
                                                min = 0, max = 201, value = c(50, 80)
                                            )
                                        ),
                                        column(
                                            2, offset = 0,
                                            shinyWidgets::switchInput(
                                                ns("activate_highlight"),
                                                size = "small"
                                            )
                                        )
                                    ),
                                    fluidRow(
                                        column(
                                            12, offset = 0,
                                            shinyWidgets::radioGroupButtons(
                                                inputId = ns("show_tiplabel"),
                                                label = "Tip (species) labels", 
                                                choices = c("Hide", "Show"),
                                                status = "primary",
                                                justified = TRUE,
                                                selected = "Hide"
                                            )
                                        )
                                    )
                            )
                            )
                    )
                )
            ))
        ),
        fluidRow(
            # Create a box to contain the tree -----------------------
            shinydashboardPlus::box(
                solidHeader = TRUE,
                title = "Phylogenetic tree with WGD events",
                status = "success", width = 7,
                sidebar = shinydashboardPlus::boxSidebar(
                    id = ns("treeviz_sidebar"),
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
                            value = 10, min = 4, max = 30, step = 1
                        )
                    ),
                    column(
                        6,
                        numericInput(
                            ns("figheight"), 
                            label = "Figure height:",
                            value = 10, min = 4, max = 30, step = 1
                        )
                    ),
                    column(
                        6, offset = 2, 
                        downloadButton(ns("download_fig"), label = "Download")
                    )
                ),
                withSpinner(
                    shiny::plotOutput(ns("tree_viz"), height = "800px"),
                    color = "#276c4c"
                ),
                hr(),
                helpText(
                )
            ),
            ## Add box to contain DT DataTable with WGD ages
            shinydashboardPlus::box(
                solidHeader = TRUE,
                title = "WGD ages",
                status = "success", width = 5,
                withSpinner(
                    DT::dataTableOutput(ns("wgd_table")),
                    color = "#276c4c"
                ),
                fluidRow(column(
                    6, offset = 4,
                    actionButton(
                        ns("button_highlight"),
                        label = "Highlight selected WGD events"
                    )
                )),
                hr(),
                helpText(
                    "Explore date statistics for each WGD event."
                ),
                ### Add sidebar to box form where users can download the table
                sidebar = shinydashboardPlus::boxSidebar(
                    id = ns("wgd_table_sidebar"),
                    width = 25,
                    icon = shiny::icon("download"),
                    selectInput(
                        ns("tableformat"),
                        label = "Choose file format:",
                        choices = c(".tsv", ".tsv.gz")
                    ),
                    column(
                        6, 
                        downloadButton(ns("download_table"), label = "Download")
                    )
                ) # /end of box sidebar
            ) # /end of box
        )
    )
}

    
#' explore_wgd_events Server Functions
#'
#' @noRd 
#' @importFrom shiny eventReactive renderPlot reactive renderImage renderText
#' @importFrom ggplot2 labs geom_point ggplot aes theme_minimal
#' scale_color_gradient scale_fill_manual theme_bw theme scale_color_manual
#' geom_bar element_blank coord_radial
#' @importFrom plotly renderPlotly ggplotly layout
#' @importFrom dplyr mutate group_by summarise
#' @importFrom stats median
#' @importFrom shinyjs onclick toggle toggleState
#' @importFrom ggsci scale_color_d3
mod_explore_wgd_events_server <- function(id) {
    
    moduleServer(id, function(input, output, session) {
        
        suppressPackageStartupMessages(library(ggplot2))
        
        ns <- session$ns
        data(periods, package = "deeptime")
        
        # Update selectizeInput() with clade name ----
        updateSelectizeInput(
            session, "input_clade", 
            choices = available_clades,
            server = TRUE,
            selected = "All"
        )
    
            
        shinyjs::onclick(
            "show_advanced",
            shinyjs::toggle(id = "advanced", anim = TRUE)
        )
        
        # Define reactive values -----------------------------------------------
        # Tree to use
        final_tree <- reactive({
            req(input$input_clade)
            ftree <- tree
            
            if(input$input_clade != "All") {
                ftree <- subset_tree(tree, species_metadata, input$input_clade)
            }

            ftree
        })
        
        # Number of species
        nspecies <- reactive({
            n <- length(final_tree()$tip.label)
            n
        })
        
        # Element sizes (e.g., height of WGD rectangles, y-axis padding, etc)
        es <- reactive({
            
            ylim <- c(-0.5, NA)
            label_size <- 1.4
            if(nspecies() <100 & nspecies() >50) {
                height <- 0.35
                label_size <- 3
            } else if(nspecies() <50 & nspecies() >10) {
                height <- 0.25
                label_size <- 3
            } else if(nspecies() <10) {
                height <- 0.15
                label_size <- 3
                ylim <- c(-0.01, NA)
            } else {
                height <- 0.5
            }
            
            s <- list(rh = height, ylim = ylim, label_size = label_size)
            s
        })
        
        # Data frame with unique WGD events 
        wgd_unique <- reactive({
            req(input$input_clade)
            wgd_df <- subset_wgd_per_clade(wgd_dates, species_metadata, input$input_clade)
            
            wgd_df
        })
        
        # ggplot object with tree + WGD events in a reactive object
        treeplot <- reactive({
            lab <- ifelse(input$show_tiplabel == "Show", TRUE, FALSE)
    
            if(input$layout == "Circular") {
                p <- plot_timetree_circular(
                    final_tree(),
                    metadata = species_metadata, 
                    add_labels = lab,
                    label_size = es()$label_size
                )
            } else {
                p <- plot_timetree_rectangular(
                    final_tree(),
                    metadata = species_metadata,
                    add_labels = lab,
                    pointsize = 1,
                    ylim = es()$ylim,
                    label_size = es()$label_size
                )
            }
            
            # Add WGD rectangles
            p <- add_wgd_rects(
                p, final_tree(), wgd_dates, rh = es()$rh, 
                highlight = selected_wgds()
            )
            
            # Highlight age range
            if(input$activate_highlight) {
                hdf <- data.frame(
                    xmin = -input$age_range[1],
                    xmax = -input$age_range[2],
                    ymin = -Inf, 
                    ymax = Inf
                )
                
                p <- p + geom_rect(
                    data = hdf,
                    aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
                    inherit.aes = FALSE, alpha = 0.3, fill = "darkolivegreen"
                )
            }
            
            p
        })
        
        # Render UI elements ----
        ## Render plot with phylogenetic tree
        output$tree_viz <- renderPlot({
            treeplot()
        }, res = 96)
        
        ## Render interactive table with WGD dates
        output$wgd_table <- DT::renderDataTable({
            
            DT::datatable(
                wgd_unique(),
                style = "bootstrap",
                rownames = FALSE,
                filter = 'none',
                colnames = c("90% HCR" = "`90% HCR`"),
                options = list(
                    pageLength = 10,
                    autoWidth = TRUE,
                    pagingType = "simple",
                    scrollX = TRUE,
                    dom = "Bfrtip"
                )
            )
        })
        
        # Highlight selected WGD events
        observe({
            toggleState("button_highlight", !is.null(input$wgd_table_rows_selected))
        })
        
        selected_wgds <- reactive({
            w <- NULL
            if(input$button_highlight && !is.null(input$wgd_table_rows_selected)) {
                idx <- input$wgd_table_rows_selected
                w <- as.character(wgd_unique()[idx, 1])
            }
            
            w
        })
        
        ## Download figure with tree to PDF/SVG file
        output$download_fig <- downloadHandler(
            filename = function() {
                paste0("tree_with_WGD_events_", input$input_clade, input$figformat)
            },
            content = function(file) {
                ggsave(
                    treeplot(), filename = file, 
                    width = input$figwidth, height = input$figheight
                )
            }
        )
        
        ## Download table with WGD dates
        output$download_table <- downloadHandler(
            filename = function() {
                paste0("wgd_dates_", input$input_clade, input$tableformat)
            },
            content = function(file) {
                readr::write_tsv(wgd_unique(), file = file)
            }
        )
        
        
    })
}

## To be copied in the UI
# mod_explore_wgd_events_ui("explore_wgd_events_ui_1")
    
## To be copied in the server
# mod_explore_wgd_events_server("explore_wgd_events_ui_1")
