#' The application User-Interface
#' 
#' @param request Internal parameter for `{shiny}`. 
#'
#' @importFrom shiny tags h2 h3 HTML icon uiOutput
#' @importFrom shinydashboard dashboardPage dashboardHeader 
#' dashboardBody valueBoxOutput menuItem sidebarMenu menuItem tabItems
#' @importFrom htmltools includeMarkdown
#' @importFrom shinydashboardPlus dashboardSidebar
#' @importFrom shinyjs useShinyjs
#' @noRd
app_ui <- function(request) {
    shiny::tagList(
        golem_add_external_resources(),
        shinydashboardPlus::dashboardPage(
            skin = "green",
            shinydashboard::dashboardHeader(
                title = HTML(
                    '<span class="logo-lg"><em>Angio</em><strong>WGD</strong></span>
                    <span class="logo-mini"><em>A</em><strong><sub>WGD</strong></sub></span>'
                )
            ),
            # Dashboard sidebar --------------------------------------------------------
            shinydashboardPlus::dashboardSidebar( 
                HTML("<br>"),
                shiny::tags$div(
                    align = "center",
                    shiny::tags$img(
                        src = "www/logo.png", width = "70%", align = "center"
                    )
                ),
                HTML("<br>"),
                # Dashboard sidebar menu ---------------------------------------
                shinydashboard::sidebarMenu(
                    shinydashboard::menuItem(
                        " Explore WGD events", 
                        tabName = "tab_wgds",
                        icon = icon("magnifying-glass", verify_fa = FALSE)
                    ),
                    shinydashboard::menuItem(
                        " WGDs by species",
                        tabName = "tab_species",
                        icon = icon("dna", verify_fa = FALSE)
                    ),
                    shinydashboard::menuItem(
                        " Original data",
                        tabName = "tab_data",
                        icon = icon("database", verify_fa = FALSE)
                    ),
                    shinydashboard::menuItem(
                        " FAQ", 
                        tabName = "tab_faq", 
                        icon = icon("question-circle", verify_fa = FALSE)
                    )
                ),
                br(),
                br(),
                uiOutput("citation_text"),
                minified = TRUE
            ),
            # Dashboard body ---------------------------------------------------
            shinydashboard::dashboardBody(
                shinyjs::useShinyjs(),
                shinydashboard::tabItems(
                    # Frontpage - tab_wgds -------------------------------------
                    shinydashboard::tabItem(
                        "tab_wgds",
                        tags$h3("WGDs across the angiosperm phylogeny", class = "title-header"),
                        mod_explore_wgds_ui("explore_wgds_1")
                    ),
                    # Frontpage - tab_species ----------------------------------
                    shinydashboard::tabItem(
                        "tab_species",
                        tags$h3("Search WGDs by species", class = "title-header"),
                        mod_wgd_by_species_ui("wgd_by_species_1")
                    ),
                    # Frontpage - tab_data -------------------------------------
                    shinydashboard::tabItem(
                        "tab_data",
                        tags$h3("Access original data used in this resource", class = "title-header"),
                        mod_original_data_ui("original_data_1")
                    ),
                    # Frontpage - tab_faq --------------------------------------
                    shinydashboard::tabItem(
                        "tab_faq",
                        tags$h3("Frequently Asked Questions", class = "title-header"),
                        shinydashboardPlus::box(
                            width = 12, status = "success",
                            htmltools::includeMarkdown(
                                system.file("extdata", "faq.md", package = "AngioWGD")
                            )
                        )
                    )
                )
            )
        )
    )
}

#' Add external Resources to the Application
#' 
#' This function is internally used to add external 
#' resources inside the Shiny application. 
#' 
#' @importFrom shiny tags includeHTML
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
    
    golem::add_resource_path(
        'www', app_sys('app/www')
    )
    
    tags$head(
        favicon("favicon"),
        bundle_resources(
            path = app_sys('app/www'),
            app_title = 'AngioWGD'
        ),
        tags$link(rel="stylesheet", type="text/css", href="www/custom.css")
    )
}

