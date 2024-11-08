
# Load required data ----
data(tree)
data(species_metadata)
data(wgd_dates)
data(posterior_hist)

# Start tests ----
test_that("get_nodepath_and_age() returns a 3-column data frame", {
    df <- get_nodepath_and_age(tree, "Glycine_max", 467)
    
    expect_true(all.equal(names(df), c("from", "to", "age")))
    expect_equal(ncol(df), 3)
})


test_that("find_column_name() returns a character scalar with column name", {
    cn <- find_column_name(species_metadata, "Poales")
    
    expect_equal(cn, "order")
})


test_that("subset_tree() returns a filtered version of a tree", {
    ftree <- subset_tree(tree, species_metadata, "Fabaceae")
    
    expect_true("treedata" %in% class(ftree))
    expect_true(ftree@phylo$Nnode < tree@phylo$Nnode)
})


test_that("make_gradient_fill() returns an object of class GridLinearGradient()", {
    m <- make_gradient_fill(100, 110)
    
    expect_true("GridLinearGradient" %in% class(m))
})


test_that("subset_wgd_per_clade() returns WGDs for a selected clade", {
    wgds <- subset_wgd_per_clade(wgd_dates, species_metadata, clade = "Fabales")
    
    expect_equal(class(wgds), "data.frame")
})


test_that("pal_auto() automatically chooses color palettes", {
    p1 <- pal_auto(paste0("l", seq_len(10)))
    p2 <- pal_auto(paste0("l", seq_len(15)))
    p3 <- pal_auto(paste0("l", seq_len(25)))
    p4 <- pal_auto(paste0("l", seq_len(5)))
    
    expect_length(p1, 10)
    expect_length(p2, 15)
    expect_length(p3, 25)
    expect_length(p4, 5)
})


test_that("hist2dens() creates x and y density coordinates", {
    
    hdata1 <- posterior_hist$byspecies
    hdata1 <- hdata1[hdata1$WGD_ID == "JUGL", ]
    dens1 <- hist2dens(hdata1)
    
    hdata2 <- posterior_hist$combined
    hdata2 <- hdata2[hdata2$WGD_ID == "JUGL", ]
    dens2 <- hist2dens(hdata2, byspecies = FALSE)
    
    expect_equal(ncol(dens1), 4)
    expect_equal(class(dens1), "data.frame")
    
    expect_equal(ncol(dens2), 3)
    expect_equal(class(dens2), "data.frame")
})





