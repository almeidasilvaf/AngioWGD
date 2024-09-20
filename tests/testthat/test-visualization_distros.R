
# Load data ----
data(posterior_hist)

# Start tests ----
test_that("plot_age_distro() returns a ggplot object", {
    hist_data <- posterior_hist$byspecies 
    hist_data <- hist_data[hist_data$WGD_ID == "JUGL", ]
    p <- plot_age_distro(hist_data)
    
    expect_true("ggplot" %in% class(p))
})


test_that("plot_consensus_age_distro() returns a ggplot object", {
    hist_data <- posterior_hist$combined
    hist_data <- hist_data[hist_data$WGD_ID == "JUGL", ]
    p <- plot_consensus_age_distro(hist_data)
    
    expect_true("ggplot" %in% class(p))
})
