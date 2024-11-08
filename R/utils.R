
#' Get name of metadata column containing an input clade
#'
#' @param metadata A data frame of species metadata with taxonomic information.
#' @param clade Character indicating which clade to subset.
#'
#' @return Character indicating the column name.
#'
#' @noRd
#'
find_column_name <- function(metadata, clade) {
    
    col_name <- apply(metadata, 2, function(x) {
        return(grepl(clade, x))
    })
    col_name <- names(which(colSums(col_name) >0))
    
    return(col_name)
}

#' Subset tree to include only a specific clade and outgroup
#'
#' @param tree A `phylo` object with a species tree.
#' @param metadata A data frame of species metadata with taxonomic information.
#' @param clade Character indicating which clade to subset.
#' 
#' @return A `phylo` object with a subset of the original tree.
#' 
#' @importFrom phangorn Descendants
#' @importFrom tidytree keep.tip
#' @rdname subset_tree
#' @export
#' @examples
#' data(tree)
#' data(species_metadata)
#'
#' final_tree <- subset_tree(tree, species_metadata, "Fabaceae")
subset_tree <- function(tree, metadata, clade) {
    
    # Get species that belong to clade specified in `clade`
    col_name <- find_column_name(metadata, clade)
    clade_species <- metadata[metadata[[col_name]] == clade, "latin_name"]
    
    # Select a suitable outgroup: the most closely-related clade
    ## For each node (from tip to root), get represented clades
    sample_sp <- clade_species[1]
    path_to_root <- get_nodepath_and_age(tree@phylo, sample_sp, length(tree@phylo$tip.label) + 1)
    
    clades_per_node <- lapply(path_to_root$from, function(x) {
        species_id <- phangorn::Descendants(tree@phylo, x, type = "tips")[[1]]
        species <- tree@phylo$tip.label[species_id]
        included_clades <- metadata[metadata$latin_name %in% species, col_name]
        included_clades <- unique(included_clades)
        
        return(included_clades)
    })

    ## In node where 2+ clades are represented, choose an outgroup
    node_id <- which(lengths(clades_per_node) >1)[1]
    outgroup <- clades_per_node[[node_id]]
    outgroup <- outgroup[outgroup != clade][1]
    outgroup_species <- metadata[metadata[[col_name]] == outgroup, "latin_name"]
    
    # Subset tree
    final_tree <- tidytree::keep.tip(tree, c(clade_species, outgroup_species))

    return(final_tree)
}


#' Create a linearGradient color palette given xmin and xmax coordinates
#' 
#' @param xmin Numeric, x-axis coordinate.
#' @param xmax Numeric, x-axis coordinate.
#' @param n Numeric, number of steps to use to create the gradient.
#' @param color Character indicating the name of the color to use to create
#' the gradient. Default: "firebrick".
#' 
#' @return An object of class `GridLinearGradient`.
#' 
#' @importFrom grid linearGradient
#' @importFrom scales rescale alpha
#' @noRd
#' 
make_gradient_fill <- function(xmin, xmax, n = 100, color = "firebrick") {
    slab_data <- data.frame(
        x = seq(xmin, xmax, length.out = n),
        fill = color,
        alpha = scales::rescale(
            abs(seq(xmin, xmax, length.out = n) - mean(c(xmin, xmax))),
            to = c(1, 0.3),
            from = c(0, (xmax - xmin) / 2)
        )
    )
    
    gradient_args <- list(
        colours = alpha(slab_data$fill, slab_data$alpha),
        stops = (slab_data$x - xmin) / (xmax - xmin),
        x1 = 0,
        x2 = 1,
        y1 = 0.5,
        y2 = 0.5
    )
    
    return(do.call(grid::linearGradient, gradient_args))
}


#' Subset a data frame of WGD events to include only species in a clade
#'
#' @param wgd_table A data frame with WGD events and statistics as 
#' in package data \strong{wgd_dates}.
#' @param metadata A data frame of species metadata with taxonomic information.
#' @param clade Character indicating which clade to subset.
#' 
#' @return A data frame with WGD events for species in selected clade.
#'
#' @importFrom stats setNames
#'
#' @export
#' @rdname subset_wgd_per_clade
#' @examples
#' data(wgd_dates)
#' data(species_metadata)
#' 
#' subset_wgd_per_clade(wgd_dates, species_metadata, clade = "Fabales")
subset_wgd_per_clade <- function(wgd_table, metadata, clade) {
    
    wgd_df <- wgd_table
    
    if(clade != "All") {
        # Get species that belong to clade specified in `clade`
        col_name <- find_column_name(metadata, clade)
        clade_species <- metadata[metadata[[col_name]] == clade, "latin_name"]
        
        # Subset WGD table to include only species in `clade_species`
        wgd_df <- wgd_table[wgd_table$species %in% clade_species, ]
    }
    
    # Keep only unique WGD events and select meaningful columns
    cols <- setNames(
        c("wgd_id", "phylogenetic_location", "consensus_peak", "consensus_mean", "x90_percent_hcr"),
        c("WGD ID", "Location", "Consensus Peak", "Consensus Mean", "`90% HCR`")
    )
    wgd_df <- wgd_df[!duplicated(wgd_df$wgd_id), cols]
    names(wgd_df) <- names(cols)
    rownames(wgd_df) <- NULL
    
    return(wgd_df)
}


#' Choose color palette automatically based on the number of levels
#'
#' @param lev A character vector of levels to which unique colors should 
#' be mapped.
#' 
#' @return A character vector of colors for each unique element of \strong{lev}.
#' @importFrom grDevices hcl.colors
#' @noRd
pal_auto <- function(lev) {
    
    len <- length(unique(lev))
    
    # Default palette: Set2 from RColorBrewer
    pal <- c(
        "#66C2A5", "#FC8D62", "#8DA0CB", "#E78AC3", "#A6D854", 
        "#FFD92F", "#E5C494", "#B3B3B3"
    )
    
    if(len >8 & len <=12) {
        # Set3 from RColorBrewer for 9<n<=12 elements
        pal <- c(
            "#8DD3C7", "#FFFFB3", "#BEBADA", "#FB8072", "#80B1D3", "#FDB462", 
            "#B3DE69", "#FCCDE5", "#D9D9D9", "#BC80BD", "#CCEBC5", "#FFED6F"
        )
    } else if(len >12 & len<=20) {
        # D3's category 20 for n>12
        pal <- c(
            "#1F77B4FF", "#FF7F0EFF", "#2CA02CFF", "#D62728FF", "#9467BDFF", 
            "#8C564BFF", "#E377C2FF", "#7F7F7FFF", "#BCBD22FF", "#17BECFFF", 
            "#AEC7E8FF", "#FFBB78FF", "#98DF8AFF", "#FF9896FF", "#C5B0D5FF", 
            "#C49C94FF", "#F7B6D2FF", "#C7C7C7FF", "#DBDB8DFF", "#9EDAE5FF"
        )
    } else if(len >20) {
        pal <- grDevices::hcl.colors(len)
    }
    
    pal <- pal[seq_len(len)]
    
    return(pal)
}


#' Create density data from pre-computed histogram data
#'
#' @param hdata A data frame with pre-computed histogram statistics,
#' including columns `xmin`, `xmax`, `mids`, `counts`, and `density`.
#'
#' @return A data frame with x and y coordinates of the density line to plot.
#' @noRd
#' @importFrom stats density
#' @examples
#' data(posterior_hist)
#' 
#' hdata <- posterior_hist$byspecies
#' hdata <- hdata[hdata$WGD_ID == "JUGL", ]
#' hist2dens(hdata)
hist2dens <- function(hdata, byspecies = TRUE) {

    if(byspecies) {
        hdata_split <- split(hdata, as.character(hdata$species))
        dens_df <- Reduce(rbind, lapply(hdata_split, function(x) {
            d <- density(rep(x$mids, times = x$counts), bw = 2.5)
            df <- data.frame(
                WGD_ID = x$WGD_ID[1], species = x$species[1],
                x = d$x, y = d$y
            )
            return(df)
        }))
        
    } else {
        d <- density(rep(hdata$mids, times = hdata$counts), bw = 2.5)
        dens_df <- data.frame(
            WGD_ID = hdata$WGD_ID[1], x = d$x, y = d$y
        )
    }
    
    return(dens_df)
}
