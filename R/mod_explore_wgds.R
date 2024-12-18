#' global_expression_viewer UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList fluidRow column selectizeInput a div
#' sliderInput actionButton helpText downloadButton plotOutput hr
#' selectInput numericInput updateSelectizeInput
#' @importFrom shinydashboardPlus box boxSidebar
#' @importFrom shinyjs hidden show useShinyjs onclick
#' @importFrom shinycssloaders withSpinner
#' @importFrom shinyWidgets radioGroupButtons switchInput
#' @importFrom DT dataTableOutput 
#' @importFrom ggiraph girafeOutput
mod_explore_wgds_ui <- function(id) {
    
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
                    ## Row 1 of the box: dropdown button to select clade
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
                    ## Row 2: switch button to select tree layout
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
                    ## Row 3: switch button to select if labels must be shown
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
                    ),
                    # add button "Show advanced options" to show extra options when clicking
                    # Extra options: "Highlight age range:" - sliderInput like SEA to choose min and max
                    fluidRow(
                        column(
                            12, offset = 0,
                            a(id = ns("show_advanced"), "Show/hide advanced options", href = "#"),
                            shinyjs::hidden(
                                # Div area containing advanced options
                                div(
                                    id = ns("advanced"),
                                    div(style = "display: flex; align-items: center;",
                                        # Option 1: age range
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
                                    )
                                )
                            )
                        )
                    )
                )
            )
        ),
        fluidRow(
            # Create a box to contain the tree -----------------------
            shinydashboardPlus::box(
                solidHeader = TRUE,
                title = "Phylogenetic tree with WGD events",
                status = "success", width = 12, 
                withSpinner(
                    ggiraph::girafeOutput(
                        ns("tree_viz"),
                        height = "700px"
                    ),
                    color = "#276c4c"
                ),
                hr(),
                helpText(
                    "Note: dates of WGDs marked in orange (N = 13) are discordant with",
                    "their expected phylogenetic locations, probably due to",
                    "shifts in substitution rates. See FAQ for more details",
                    "on how WGDs are positioned in nodes of the tree.",
                    "Use the toolbar on top of the plot to activate pan/zoom."
                ),
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
                    fluidRow(
                        class = "text-center",
                        downloadButton(ns("download_fig"), label = "Download")
                    )
                )
            )
        ),
        fluidRow(
            ## Add box to contain DT DataTable with WGD ages
            shinydashboardPlus::box(
                solidHeader = TRUE,
                title = "WGD ages",
                status = "success", width = 12,
                withSpinner(
                    DT::dataTableOutput(ns("wgd_table")),
                    color = "#276c4c"
                ),
                fluidRow(
                    class = "text-center",
                    actionButton(
                        ns("button_highlight"),
                        label = "Highlight selected WGD events"
                    )
                ),
                hr(),
                helpText(
                    "Explore date statistics for each WGD event."
                ),
                ### Add sidebar to box form where users can download the table
                sidebar = shinydashboardPlus::boxSidebar(
                    id = ns("wgd_table_sidebar"),
                    width = 40,
                    icon = shiny::icon("download"),
                    selectInput(
                        ns("tableformat"),
                        label = "Choose file format:",
                        choices = c(".tsv", ".tsv.gz")
                    ),
                    fluidRow(
                        class = "text-center",
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
#' @importFrom stats median
#' @importFrom shinyjs onclick toggle toggleState
#' @importFrom ggiraph renderGirafe girafe opts_zoom
mod_explore_wgds_server <- function(id) {
    
    moduleServer(id, function(input, output, session) {
        ns <- session$ns
        
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
            n <- length(final_tree()@phylo$tip.label)
            n
        })
        
        # Element sizes (e.g., height of WGD rectangles, y-axis padding, etc)
        es <- reactive({
            
            ylim <- c(-0.5, NA)
            label_size <- 1
            if(nspecies() >=100 & nspecies() < 150) {
                label_size <- 1.5
                height <- 0.5
            } else if(nspecies() <100 & nspecies() >50) {
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
                p <- plot_itimetree_circular(
                    final_tree(),
                    metadata = species_metadata, 
                    add_labels = lab,
                    label_size = es()$label_size
                ) 
            } else {
                p <- plot_itimetree_rectangular(
                    final_tree(),
                    metadata = species_metadata,
                    add_labels = lab,
                    pointsize = 1,
                    ylim = es()$ylim,
                    label_size = es()$label_size
                )
            }
            
            # Add WGD rectangles
            p <- add_iwgd_rects(
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
                    aes(xmin = .data$xmin, xmax = .data$xmax, ymin = .data$ymin, ymax = .data$ymax),
                    inherit.aes = FALSE, alpha = 0.3, fill = "darkolivegreen"
                )
            }
            
            p
        })
        
        # Render UI elements ----
        ## Render plot with phylogenetic tree
        output$tree_viz <- ggiraph::renderGirafe({
            
            girafe(
                ggobj = treeplot(),
                width_svg = 12,
                height_svg = 9,
                options = list(
                    ggiraph::opts_zoom(min = 1, max = 4),
                    ggiraph::opts_toolbar(
                        hidden = c("lasso_select", "lasso_deselect"),
                        position = "top"
                    )
                )
            )
        })
        
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
                write.table(
                    wgd_unique(), file = file, quote = FALSE, sep = "\t", 
                    row.names = FALSE
                )
            }
        )
        
    })
}

## To be copied in the UI
# mod_explore_wgds_ui("explore_wgds_1")
    
## To be copied in the server
# mod_explore_wgds_server("explore_wgds_1")
