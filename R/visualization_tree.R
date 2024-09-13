
#' Plot a timetree for angiosperms in circular layout
#'
#' @param tree A `phylo` object with a species tree.
#' @param metadata A data frame with taxonomic information for each
#' tip in \strong{tree}.
#' @param color_by Character indicating the name of the column 
#' in \strong{metadata} to use when coloring tip points.
#' @param periods_alpha Numeric indicating the alpha aesthetic of the colors
#' inside the tree used to represent geological periods.
#' @param add_labels Logical indicating whether or not to add labels to tips
#' (i.e., species names). Default: FALSE.
#' @param label_size Numeric indicating the size of the tip labels. Only
#' valid if \strong{add_labels = TRUE}. Default: 1.4.
#' 
#' @return A `ggtree` object with a phylogenetic tree visualization.
#' 
#' @importFrom ggtree ggtree %<+% geom_tippoint revts
#' @importFrom ggplot2 scale_y_continuous scale_x_continuous theme_minimal
#' @import ggplot2
#' @importFrom geomtextpath geom_textpath
#' @importFrom deeptime coord_geo_radial guide_geo
#' @rdname plot_timetree_circular
#' @examples
#' data(tree)
#' data(species_metadata)
#' metadata <- species_metadata
#' plot_timetree_circular(tree, species_metadata, color_by = "taxonomy_3")
plot_timetree_circular <- function(
        tree, metadata, color_by = "taxonomy_3", 
        periods_alpha = 0.4,
        add_labels = FALSE,
        label_size = 1.4
) {
    
    # Define period boundaries and colors
    age_breaks <- c(201.4, 145.0, 66.0, 23.03)
    fill <- rev(c("#b1b9a4", "#bdc4b4", "#c6cebb", "#d0dbc3", "#dfe5da", "#eff3eb"))
    
    # Plot tree
    p <- revts(ggtree(tree)) %<+% metadata +
        coord_geo_radial(
            dat = "periods", fill = fill, alpha = periods_alpha, 
            lty = "dotted", lwd = 0.6, color = "#b1b9a4",
            end = 1.45 * pi
        ) +
        scale_x_continuous(
            breaks = -age_breaks,
            labels = format(age_breaks, drop0trailing = FALSE),
            expand = expansion(mult = c(0.05, 0)),
            guide = guide_axis_stack(
                guide_geo(
                    "periods", neg = TRUE, rot = -90, size = "auto",
                    height = unit(1, "line"), alpha = 0.6
                ),
                guide_axis(),
                spacing = unit(0, "line")
            ),
            limits = if(add_labels) c(-230, 80) else c(-230, 0), 
        ) +
        scale_y_continuous(guide = NULL, expand = expansion(mult = c(0.005, 0.01))) +
        theme_classic() +
        geom_tippoint(aes(color = .data[[color_by]])) +
        labs(color = "Clade")
    
    # Add labels to tips (species names)? 
    if(add_labels) {
        p <- p + geomtextpath::geom_textpath(
            aes(x = x + 5, label = species_name), 
            size = label_size, hjust = 1
        )
    }
    
    
    return(p)
}



#' Plot a timetree for angiosperms in rectangular layout
#'
#' @param tree A `phylo` object with an ultrametric tree.
#' @param metadata A data frame of species metadata (e.g., taxonomic 
#' information and any other relevant variables describing species).
#' @param color_by Character indicating which column of the data frame
#' in \strong{metadata} should be used to color tip points. 
#' Default: "taxonomy_3".
#' @param xlim Numeric vector of length 2 indicating the x-axis limits.
#' Default: \code{c(-220, 2)}.
#' @param pointsize Numeric indicating the size of the point 
#' in `ggtree::geom_tippoint()`. Default: 2.
#' @param pointalpha Numeric indicating the alpha aesthetic of the point
#' in `ggtree::geom_tippoint()`. Default: 0.8.
#' @param add_labels Logical indicating whether or not to add labels to tips
#' (i.e., species names). Default: FALSE.
#' @param label_size Numeric indicating the size of the tip labels. Only
#' valid if \strong{add_labels = TRUE}. Default: 1.4.
#' @param ... Additional arguments to \code{deeptime::coord_geo()}.
#' 
#' @return A `ggtree` object.
#'
#' @export
#' @rdname plot_timetree_rectangular
#' @importFrom ggtree theme_tree2 geom_tippoint revts %<+%
#' @importFrom deeptime coord_geo
#' @importFrom ggplot2 scale_x_continuous
#' @importFrom ape Ntip
#' @examples
#' data(tree)
#' data(species_metadata)
#' p <- plot_timetree_rectangular(tree, species_metadata)
plot_timetree_rectangular <- function(
        tree, metadata, color_by = "taxonomy_3", 
        pointsize = 2, pointalpha = 0.8, 
        add_labels = FALSE,
        label_size = 1.4,
        ...
) {
    
    age_breaks <- c(300, 201.4, 145.0, 66.0, 23.03)
    
    # Get circular tree with chronostratigraphic info
    p <- revts(ggtree(tree)) %<+% metadata +
        geom_tippoint(
            aes(color = .data[[color_by]]), 
            size = pointsize, alpha = pointalpha
        ) +
        deeptime::coord_geo(
            neg = TRUE, 
            abbrv = TRUE, 
            alpha = 0.6, 
            expand = TRUE,
            xlim = if(add_labels) c(-230, 60) else c(-230, 0), 
            ...
        ) +
        scale_x_continuous(
            breaks = -age_breaks,
            labels = format(age_breaks, drop0trailing = FALSE)
        ) +
        ggtree::theme_tree2() +
        labs(color = "Clade")
    
    # Add labels to tips (species names)?
    if(add_labels) {
        p <- p + ggtree::geom_tiplab(aes(x = x + 5, label = species_name), size = label_size)
    }
    
    return(p)
} 


#' Add rectangles indicating WGD events in a phylogenetic tree
#'
#' @param p A `ggtree/ggplot` object containing a phylogenetic tree 
#' visualization.
#' @param tree A `phylo` object with the species tree used to create
#' the plot in \strong{p}.
#' @param wgd_dates Data frame with WGD date information.
#' @param rh Numeric indicating the height of the rectangles representing
#' WGD events. Default: 0.15.
#' @param highlight A character vector with IDs of WGD events to highlight.
#'
#' @return A `ggtree` object.
#' 
#' @importFrom ggplot2 geom_rect coord_radial
#' @export
#' @rdname add_wgd_rects
#' @examples
#' data(tree)
#' data(species_metadata)
#' data(wgd_dates)
#' keep <- species_metadata |> 
#'   dplyr::filter(family %in% c("Fabaceae", "Amborellaceae")) |>
#'   dplyr::pull(latin_name)
#' 
#' ftree <- ape::keep.tip(tree, keep)
#'
#' p <- plot_timetree_rectangular(ftree, species_metadata, color_by = "taxonomy_3")
#' add_wgd_rects(p, ftree, wgd_dates, rh = 0.3)
#'
add_wgd_rects <- function(p, tree, wgd_dates, rh = 0.25, highlight = NULL) {
    
    # Get ID of root node
    root_id <- length(tree$tip.label) + 1
    
    # Keep only species in the tree
    wgd_dates <- wgd_dates[wgd_dates$species %in% tree$tip.label, ]
    
    if(nrow(wgd_dates) >0) {
        
        # Create a data frame with node ID, WGD ID, and rectangle x coordinates
        wgds <- wgd_dates[!duplicated(wgd_dates$wgd_id), ]
        rect <- Reduce(rbind, lapply(seq_len(nrow(wgds)), function(x) {
            
            mu <- wgds$consensus_mean[x]
            hcr_min <- as.numeric(gsub("-.*", "", wgds$x90_percent_hcr[x]))
            hcr_max <- as.numeric(gsub(".*-", "", wgds$x90_percent_hcr[x]))
            sp <- wgds$species[x]
            
            path <- get_nodepath_and_age(tree, sp, root_id)
            idx <- min(which(path$age >= mu)) - 1
            node <- path[idx, "from"]
            
            # For species-specific WGDs
            if(idx == 0) { node <- path[1, "to"] }
            df <- data.frame(
                node = node, 
                wgd_id = wgds$wgd_id[x],
                xmin = -hcr_min, 
                xmax = -hcr_max
            )
            
            return(df)
        }))
        
        # Add y axis coordinates (fixed)
        rect_coord <- as.data.frame(merge(p$data, rect))
        rect_coord$ymin <- rect_coord$y - rh
        rect_coord$ymax <- rect_coord$y + rh
        
        # Add rectangles to plot using a linear gradient color scheme
        for(i in seq_len(nrow(rect_coord))) {
            
            if(!is.null(highlight)) {
                if(rect_coord$wgd_id[i] %in% highlight) {
                    bg <- make_gradient_fill(rect_coord$xmax[i], rect_coord$xmin[i], color = "goldenrod3")
                } else {
                    bg <- make_gradient_fill(rect_coord$xmax[i], rect_coord$xmin[i])
                }
            } else {
                bg <- make_gradient_fill(rect_coord$xmax[i], rect_coord$xmin[i])
            }
            
            p <- p + geom_rect(
                aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
                data = rect_coord[i, ], inherit.aes = FALSE, 
                fill = bg, color = NA
            )
        }
    }
    
    return(p)
}


