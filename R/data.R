
#' Time-calibrated species tree for 466 angiosperm species
#' 
#' @name tree
#' @format An object of class `phylo` with 466 tips and 465 internal nodes
#' @examples
#' data(tree)
#' @usage data(tree)
"tree"


#' Taxonomic information for all 466 species in \strong{tree}
#' 
#' @name species_metadata
#' @format A data frame with the following variables:
#' \describe{
#'   \item{latin_name}{Character, latin name for each species.}
#'   \item{family}{Character, family for each species.}
#'   \item{order}{Character, order for each species.}
#'   \item{taxonomy_1}{Character, one of 'Commenilids', 'Lamiids',
#'   'Campanulids', 'Fabids', or 'Malvids'.}
#'   \item{taxonomy_2}{Character, one of 'Asterids' or 'Rosids'.}
#'   \item{taxonomy_3}{Character, one of 'ANA grade', 'Monocots', 'Magnoliids',
#'   'Chloranthales', 'Ceratophyllales', or 'Eudicots'.}
#'   \item{species_name}{Character, 'clean' species names.}
#' }
#'
#' @examples
#' data(species_metadata)
#' @usage data(species_metadata)
"species_metadata"


#' WGD events and ages for all 466 species in \strong{tree}
#' 
#' @name wgd_dates
#' @format A data frame with the following variables:
#' \describe{
#'   \item{wgd_id}{Character, unique ID of WGD events.}
#'   \item{phylogenetic_location}{Character, branch of the plant phylogeny 
#'   where WGD event is.}
#'   \item{species}{Character, species name.}
#'   \item{ks_peak}{Numeric, Ks peak.}
#'   \item{credible_range}{Character, credible range for the Ks peak.}
#'   \item{posterior_mean}{Numeric, mean of the posterior distribution of
#'   WGD ages.}
#'   \item{posterior_median}{Numeric, median of the posterior distribution
#'   of WGD ages.}
#'   \item{posterior_mode}{Numeric, mode of the posterior distribution of
#'   WGD ages.}
#'   \item{x90_percent_hpd}{Character, range with 90\% HPD.}
#'   \item{consensus_peak}{Numeric, peak of the consensus distribution
#'   of WGD ages.}
#'   \item{consensus_mean}{Numeric, mean of the consensus distribution of
#'   WGD ages.}
#'   \item{x90_percent_hcr}{Character, range with 90\% HCR.}
#'   \item{full_species}{Character with the names of all species sharing that 
#'   WGD in their history.}
#' }
#'
#' @examples
#' data(wgd_dates)
#' @usage data(wgd_dates)
"wgd_dates"


#' Available clades that users can select from
#'
#' @name available_clades
#' @format A character vector with all possible clades associated with species
#' in this app.
#' @examples
#' data(available_clades)
#' @usage data(available_clades)
"available_clades"


#' Data frame with URLs to original data (CDS and annotation) for each species
#' 
#' @name data_urls
#' @format A data frame with the following variables:
#' \describe{
#'   \item{species_name}{Character, latin name for each species.}
#'   \item{family}{Character, family for each species.}
#'   \item{order}{Character, order for each species.}
#'   \item{clade}{Character, one of 'ANA grade', 'Monocots', 'Magnoliids',
#'   'Chloranthales', 'Ceratophyllales', or 'Eudicots'.}
#'   \item{CDS_URL}{Character, URL to FASTA file with CDS.}
#'   \item{Annotation_URL}{Character, URL to GFF3 file with annotation.}
#' }
#' 
#' @examples
#' data(data_urls)
#' @usage data(data_urls)
"data_urls"


#' Pre-computed histogram statistics for posterior distributions of WGD ages
#' 
#' @name posterior_hist
#' @format A list of two data frames named \strong{byspecies} 
#' and \strong{combined} with histogram statistics for each WGD grouped by
#' species and not grouped by species, respectively. Data frames have the
#' following variables:
#' \describe{
#'   \item{WGD_ID}{Factor, WGD ID.}
#'   \item{species}{Factor, species name. Only present in \strong{byspecies} data frame.}
#'   \item{total_count}{Numeric, total number of observations.}
#'   \item{xmin}{Numeric, lower boundary of the histogram bin.}
#'   \item{xmax}{Numeric, upper boundary of the histogram bin.}
#'   \item{mids}{Numeric, difference between upper and lower boundary.}
#'   \item{counts}{Numeric, number of observations in each bin.}
#'   \item{density}{Numeric, density of each bin.}
#' }
#' 
#' @examples
#' data(posterior_hist)
#' @usage data(posterior_hist)
"posterior_hist"


#' Period data from the International Commission on Stratigraphy (v2023/06)
#' 
#' Same data set as in \strong{deeptime::periods}. See `?deeptime::periods`
#' for more details.
#' 
#' @name periods
#' @format A data frame with 22 rows and 5 variables. See `?deeptime::periods`
#' for more details.
#' 
#' @examples
#' data(periods)
#' @usage data(periods)
"periods"


