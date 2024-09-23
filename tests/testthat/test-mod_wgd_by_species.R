
# Start tests ----
test_that("mod_wgd_by_species works as expected", {
    
    shiny_app <- run_app()
    app <- shinytest2::AppDriver$new(shiny_app, name = "mod_wgd_by_species")
    
    #app$set_inputs(`explore_wgd_events_ui_1-input_clade` = "All")
    app$set_window_size(width = 1381, height = 987)
    app$click("wgd_by_species_1-button_plot")
    app$expect_values()
})
