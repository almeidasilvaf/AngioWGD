
# Start tests ----
test_that("get_golem_config() returns configs", {
    
    configs <- get_golem_config("golem_name")
    
    expect_equal(class(configs), "character")
})
