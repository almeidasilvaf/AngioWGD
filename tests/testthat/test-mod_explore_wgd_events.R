
# Start tests ----
test_that("mod_explore_wgd_events works as expected", {
    
    shiny_app <- run_app()
    app <- shinytest2::AppDriver$new(shiny_app, name = "mod_explore_wgd_events")
    app$set_window_size(width = 1381, height = 987)
    app$set_inputs(`explore_wgd_events_ui_1-input_clade` = "Fabaceae")
    app$set_inputs(`explore_wgd_events_ui_1-layout` = "Rectangular")
    app$set_inputs(`explore_wgd_events_ui_1-activate_highlight` = TRUE)
    app$set_inputs(`explore_wgd_events_ui_1-show_tiplabel` = "Show")
    app$click("explore_wgd_events_ui_1-button_highlight")
    app$expect_values()
})
