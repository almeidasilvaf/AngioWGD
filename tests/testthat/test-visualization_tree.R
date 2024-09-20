
# Load data ----
data(tree)
data(species_metadata)
data(wgd_dates)

# Start tests ----
test_that("plot_timetree_circular() returns a ggplot object", {
    p <- plot_timetree_circular(tree, species_metadata, add_labels = TRUE)
    
    expect_true("ggplot" %in% class(p))
})

test_that("plot_timetree_rectangular() returns a ggplot object", {
    p <- plot_timetree_rectangular(tree, species_metadata, add_labels = TRUE)
    
    expect_true("ggplot" %in% class(p))
})


test_that("add_wgd_rectangles() returns a ggplot object", {
    keep <- species_metadata[species_metadata$family == "Fabaceae", "latin_name"]
    ftree <- ape::keep.tip(tree, keep)
    p <- plot_timetree_rectangular(ftree, species_metadata)
    p <- add_wgd_rects(p, ftree, wgd_dates, rh = 0.3, highlight = "PAPI")
    
    expect_true("ggplot" %in% class(p))
})
