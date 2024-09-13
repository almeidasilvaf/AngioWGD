
test_that("app server", {
  server <- app_server
  expect_equal(class(server), "function")
  # Check that formals have not been removed
  fmls <- formals(app_server)
  for (i in c("input", "output", "session")){
    expect_true(i %in% names(fmls))
  }
})

# Configure this test to fit your need
test_that(
  "app launches",{
    golem::expect_running(sleep = 3)
  }
)
