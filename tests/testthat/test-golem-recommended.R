
# Start {golem}-recommended tests ----
test_that("app ui", {
    ui <- app_ui()
    golem::expect_shinytaglist(ui)
    # Check that formals have not been removed
    fmls <- formals(app_ui)
    for (i in c("request")) {
        expect_true(i %in% names(fmls))
    }
})


test_that("app server", {
    server <- app_server
    expect_type(server, "closure")
    # Check that formals have not been removed
    fmls <- formals(app_server)
    for (i in c("input", "output", "session")) {
        expect_true(i %in% names(fmls))
    }
})


test_that("app_sys works", {
    expect_true(app_sys("golem-config.yml") != "")
})


test_that("golem-config works", {
    config_file <- app_sys("golem-config.yml")
    
    expect_true(config_file != "")
})


test_that("app launches", {
    golem::expect_running(sleep = 3)
})

test_that("run_app() works", {
    a <- run_app()
    
    expect_equal(class(a), "shiny.appobj")
})


