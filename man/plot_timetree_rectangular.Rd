% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/visualization_tree.R
\name{plot_timetree_rectangular}
\alias{plot_timetree_rectangular}
\title{Plot a timetree for angiosperms in rectangular layout}
\usage{
plot_timetree_rectangular(
  tree,
  metadata,
  pointsize = 2,
  pointalpha = 0.8,
  add_labels = FALSE,
  label_size = 1.4,
  ...
)
}
\arguments{
\item{tree}{A `phylo` object with an ultrametric tree.}

\item{metadata}{A data frame of species metadata (e.g., taxonomic 
information and any other relevant variables describing species).}

\item{pointsize}{Numeric indicating the size of the point 
in `ggtree::geom_tippoint()`. Default: 2.}

\item{pointalpha}{Numeric indicating the alpha aesthetic of the point
in `ggtree::geom_tippoint()`. Default: 0.8.}

\item{add_labels}{Logical indicating whether or not to add labels to tips
(i.e., species names). Default: FALSE.}

\item{label_size}{Numeric indicating the size of the tip labels. Only
valid if \strong{add_labels = TRUE}. Default: 1.4.}

\item{...}{Additional arguments to \code{deeptime::coord_geo()}.}
}
\value{
A `ggtree` object.
}
\description{
Plot a timetree for angiosperms in rectangular layout
}
\examples{
data(tree)
data(species_metadata)
plot_timetree_rectangular(tree, species_metadata)
}
