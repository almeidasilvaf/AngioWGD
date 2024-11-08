
#' Get the node path from a tip of the tree to another node
#'
#' @param tree A `phylo` object with an ultrametric species tree.
#' @param tip Character indicating the label of tr he tip to start from.
#' @param node Numeric indicating the ID of the node where the path ends.
#'
#' @return A 3-column data frame with the following variables:
#' \itemize{
#'   \strong{from} numeric, node ID.
#'   \strong{to} numeric, node ID.
#'   \strong{age} numeric, node age (distance from the tip).
#' }
#'
#' @importFrom ape nodepath
#' @export
#' @rdname get_nodepath_and_age
#' @examples
#' data(tree)
#' df <- get_nodepath_and_age(tree, "Glycine_max", 467)
get_nodepath_and_age <- function(tree, tip, node = 467) {
    
    # Get node paths and edge lengths
    node_ids <- ape::nodepath(tree, which(tree$tip.label == tip), node)
    node_len <- as.data.frame(cbind(tree$edge, tree$edge.length))
    
    # Keep only relevant nodes and add dates (cumulative sum of edge lengths)
    node_len <- node_len[node_len$V2 %in% node_ids, ]
    node_len <- node_len[match(node_len$V2, node_ids), ]
    node_len$V3 <- cumsum(node_len$V3)
    names(node_len) <- c("from", "to", "age")
    
    return(node_len)
}

#' Position WGD in a node of a tree
#'
#' @param tree A `phylo` object with the species tree used to create
#' the plot in \strong{p}.
#' @param wgd_dates Data frame with WGD date information.
#' @param method Character indicating which method to use to position WGDs.
#' One of 'mrca' (using MRCA of all species in variable \strong{full_species})
#' or 'consensus_mean' (using consensus mean only). Default: 'mrca'.
#'
#' @return A data frame with WGD and the node IDs where they should be placed,
#' along with xmin and xmax coordinates based on 90\% HCR.
#' @noRd
#'
position_wgd <- function(tree, wgd_dates, method = "mrca") {
    
    root_id <- length(tree@phylo$tip.label) + 1
    
    # Create a data frame with node ID, WGD ID, and rectangle x coordinates
    wgds <- wgd_dates[!duplicated(wgd_dates$wgd_id), ]
    rect <- Reduce(rbind, lapply(seq_len(nrow(wgds)), function(x) {
        
        mu <- wgds$consensus_mean[x]
        hcr_min <- as.numeric(gsub("-.*", "", wgds$x90_percent_hcr[x]))
        hcr_max <- as.numeric(gsub(".*-", "", wgds$x90_percent_hcr[x]))
        
        if(method == "consensus_mean") {
            sp <- wgds$species[x]
            path <- get_nodepath_and_age(tree@phylo, sp, root_id)
            idx <- min(which(path$age >= mu)) - 1
            node <- path[idx, "from"]
            
            # For species-specific WGDs
            if(idx == 0) { node <- path[1, "to"] }
            
        } else if(method == "mrca") {
            sps <- unique(unlist(strsplit(wgds$full_species[x], ", ")))
            node <- ape::getMRCA(tree@phylo, sps)
            if(is.null(node)) { node <- which(tree@phylo$tip.label == sps) }
        } else {
            stop("Invalid method.")
        }

        df <- data.frame(
            node = node, 
            wgd_id = wgds$wgd_id[x],
            xmin = -hcr_min, 
            xmax = -hcr_max
        )
        
        return(df)
    }))
    
    return(rect)
}

