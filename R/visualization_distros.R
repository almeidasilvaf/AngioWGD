
#' Plot a histogram and density line with posterior distributions of WGD ages 
#'
#' @param hist_data A data frame of pre-computed histogram data for 
#' a specific WGD as in package data \strong{hist_data}.
#'
#' @return A ggplot object.
#' @importFrom ggplot2 ggplot geom_col scale_y_continuous geom_line 
#' theme_classic labs aes
#' @rdname plot_age_distro
#' @examples
#' data(posterior_hist)
#' 
#' hist_data <- posterior_hist$byspecies |> dplyr::filter(WGD_ID == "MUSA_beta")
#' p <- plot_age_distro(hist_data)
plot_age_distro <- function(hist_data) {
    
    species <- as.character(unique(hist_data$species))
    wgd_id <- gsub("_", " ", as.character(hist_data$WGD_ID[1]))
    dens_data <- hist2dens(hist_data)
        
    p <- ggplot() +
        # Plot histogram bars from pre-computed data
        geom_col(
            data = hist_data, 
            aes(x = .data$mids, y = .data$density, fill = .data$species), 
            position = "identity", alpha = 0.5, 
            width = hist_data$xmax - hist_data$xmin
        ) +
        # Plot density line from pre-computed data
        geom_line(
            data = dens_data, 
            aes(x = .data$x, y = .data$y, color = .data$species), 
            linewidth = 1
        ) +
        theme_classic() +
        labs(
            title = paste0("Posterior distributions of WGD age for WGD '", wgd_id, "'"),
            x = "Time (in million years ago)",
            y = "Density",
            fill = "Species", color = "Species"
        ) +
        scale_fill_manual(values = pal_auto(species)) +
        scale_color_manual(values = pal_auto(species)) +
        scale_y_continuous(expand = c(0.001, 0.001))
    
    # Handle legend
    nsp <- length(species)
    ncol <- ifelse(nsp <=10, 1, 2)
    p <- p + guides(
        fill = guide_legend(ncol = ncol), 
        color = guide_legend(nrow = ncol)
    ) +
        theme(legend.position = "right")

    return(p)
}


#' Plot a histogram and density line with a consensus posterior distributions of WGD ages 
#'
#' @param hist_data A data frame of pre-computed histogram data for 
#' a specific WGD as in package data \strong{hist_data}.
#'
#' @return A ggplot object.
#' @importFrom ggplot2 ggplot geom_col scale_y_continuous geom_line 
#' theme_classic labs aes
#' @rdname plot_consensus_age_distro
#' @examples
#' data(posterior_hist)
#' 
#' hist_data <- posterior_hist$combined |> dplyr::filter(WGD_ID == "MUSA_beta")
#' p <- plot_consensus_age_distro(hist_data)
plot_consensus_age_distro <- function(hist_data) {
    
    wgd_id <- gsub("_", " ", as.character(hist_data$WGD_ID[1]))
    dens_data <- hist2dens(hist_data, byspecies = FALSE)
    
    # Plot
    p_cdistro <- ggplot() +
        geom_col(
            data = hist_data, 
            aes(x = .data$mids, y = .data$density), 
            fill = "black", alpha = 0.2, 
            position = "identity"
            #width = hist_data$xmax - hist_data$xmin
        ) +
        # Consensus density line
        geom_line(
            data = dens_data, 
            aes(x = .data$x, y = .data$y), 
            color = "gray40", 
            linewidth = 1
        ) +
        theme_classic() +
        labs(
            title = paste0("Consensus posterior distribution of WGD age for WGD '", wgd_id, "'"),
            x = "Time (in million years ago)",
            y = "Density"
        ) +
        scale_y_continuous(expand = c(0.001, 0.001))
    
    return(p_cdistro)
}


