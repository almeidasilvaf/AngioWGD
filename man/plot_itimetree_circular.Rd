% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/visualization_tree_interactive.R
\name{plot_itimetree_circular}
\alias{plot_itimetree_circular}
\title{Plot an interactive timetree for angiosperms in circular layout}
\usage{
plot_itimetree_circular(
  tree,
  metadata,
  periods_alpha = 0.4,
  add_labels = FALSE,
  label_size = 1.4
)
}
\arguments{
\item{tree}{A `phylo` object with a species tree.}

\item{metadata}{A data frame with taxonomic information for each
tip in \strong{tree}.}

\item{periods_alpha}{Numeric indicating the alpha aesthetic of the colors
inside the tree used to represent geological periods.}

\item{add_labels}{Logical indicating whether or not to add labels to tips
(i.e., species names). Default: FALSE.}

\item{label_size}{Numeric indicating the size of the tip labels. Only
valid if \strong{add_labels = TRUE}. Default: 1.4.}
}
\value{
A `ggtree` object with a phylogenetic tree visualization.
}
\description{
Plot an interactive timetree for angiosperms in circular layout
}
\examples{
data(tree)
data(species_metadata)
metadata <- species_metadata
plot_itimetree_circular(tree, species_metadata)
}
