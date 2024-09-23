
# Start tests ----
test_that("mod_original_data works", {
    shiny::testServer(mod_original_data_server, {
        expect_true("json" %in% class(output$species_table))
    }) 
})

