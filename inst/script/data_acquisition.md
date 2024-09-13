Data acquisition
================

# Overview

In this document, we will describe the code used to obtain data in
`data/` and `inst/extdata`.

``` r
set.seed(123)

library(here)
library(ape)
library(tidyverse)
library(readxl)
library(taxize)
library(rvest)
library(httr)
```

# Data in `data/`

## wgd_dates.rda

This is a data frame with WGD dates and summary statistics.

``` r
wgd_dates <- read_tsv(
    here("inst", "extdata", "wgd_dates_maintree.tsv.gz"),
    show_col_types = FALSE
) |>
    janitor::clean_names() |>
    as.data.frame() |>
    dplyr::select(-full_species)

usethis::use_data(wgd_dates, compress = "xz", overwrite = TRUE)
```

## tree.rda

This is an object of class `phylo` with a phylogenetic tree for all
species in the database.

``` r
tree <- read.tree(here("inst", "extdata", "tree.nw"))
tree$edge.length <- tree$edge.length * 100

usethis::use_data(tree, compress = "xz", overwrite = TRUE)
```

## species_metadata.rda

This is a data frame of taxonomic information for each species in the
database.

``` r
species_metadata <- readr::read_tsv(
    here::here("inst", "extdata", "species_taxonomy.tsv"),
    show_col_types = FALSE
) |>
    janitor::clean_names() |>
    as.data.frame()

tax_info <- taxize::classification(
    gsub("_", " ", species_metadata$latin_name), 
    db = "ncbi"
)

test <- taxize::classification(
    gsub("_", " ", species_metadata$latin_name[1:5]), 
    db = "eol"
)


tax_df <- lapply(tax_info, function(x) {
    
    df <- NULL
    if(is.data.frame(x)) {
        species <- x[x$rank == "species", "name"]
        family <- x[x$rank == "family", "name"]
        order <- x[x$rank == "order", "name"]
        class <- x[x$rank == "class", "name"]
        clade <- paste(x[x$rank == "clade", "name"], collapse = "-")
        
        df <- data.frame(species, family, order, class, clade)
    }
    
    return(df)
}) |>
    dplyr::bind_rows()

species_metadata <- species_metadata |>
    mutate(
        family = replace_na(family, "Other"),
        order = replace_na(order, "Other"),
        taxonomy_1 = replace_na(taxonomy_1, "Other"),
        taxonomy_2 = replace_na(taxonomy_2, "Other"),
        taxonomy_3 = replace_na(taxonomy_3, "Other"),
        species_name = str_replace_all(latin_name, "_", " ")
    ) |>
    as.data.frame()

usethis::use_data(species_metadata, compress = "xz", overwrite = TRUE)
```

## available_clades.rda

This object contains a character vector of available clades that users
can select to subset the tree and WGD date table.

``` r
data(species_metadata)

available_clades <- c(
    species_metadata$family,
    species_metadata$order,
    species_metadata$taxonomy_1,
    species_metadata$taxonomy_2,
    species_metadata$taxonomy_3, 
    "All"
) |>
    na.omit() |>
    unique()

available_clades <- available_clades[available_clades != "Other"]

usethis::use_data(available_clades, compress = "xz", overwrite = TRUE)
```

## data_urls.rda

This object contains URLs to FASTA files containing CDS and GFF3 files
containing gene annotation for each species, available in a FigShare
repository associated with the publication describing this resource. To
create it, I first went to [the FigShare repo with CDS
data](https://figshare.com/articles/dataset/Supplementary_cds_files_of_470_angiosperm_species/27011128)
on Google Chrome and did the following:

1.  Click on the “Next page” arrow button to see all files (27 pages,
    thumbnail view).
2.  Once all pages have been shown, the Javascript-created HTML file
    will contain data on all files. Then, we open Developer Tools, go to
    the Console tab, and run the following code:

<!-- -->

    copy(document.documentElement.outerHTML);

3.  This code will copy the content of the HTML page in the clipboard.
    Then, we can paste the text in the clipboard to an empty file with
    an `.html` extension (e.g., `figshare_cds.html`).

Then, I went to [the FigShare repo with annotation
data](https://figshare.com/articles/dataset/Supplementary_gff3_files_of_470_angiosperm_species/27011134)
and repeated the steps above, but this time saving the
Javascript-generated HTML file to `figshare_annotation.html`.

Finally, I used the following code to scrape the HTML files, extract
URLs, and store them in a tidy data frame.

``` r
# Define function to get a data frame of URL, filename, and species
scrape_figshare <- function(html_path, n = 471) {
    
    # Read page and extract divs
    page <- read_html(html_path)
    
    # Extract all divs that are present `n` times
    div_count <- page |> html_elements("div") |> html_attr("class") |> table()
    divs <- div_count[div_count == n] |> names()
    
    # Create a data frame with file name and URL
    df <- lapply(divs, function(x) {
        
        div <- page |> html_elements(xpath = paste0("//div[@class='", x, "']"))
        
        # Get file names (if any)
        df <- NULL
        fn <- div |> html_attr("title")
        fn <- unlist(lapply(div, html_attr, "title"))
        fn <- fn[!is.na(fn)]
        
        # Get file URLs
        if(length(fn) != 0) {
            urls <- div |> html_elements("a") |> html_attr("href")
            df <- data.frame(fn = fn, url = urls)
        }
        
        return(df)
    }) |> bind_rows()

    # Combine results in a data frame
    url_df <- df |>
        dplyr::distinct(.keep_all = TRUE)
    
    return(url_df)
}

# Get URLs to FASTA files
cds_urls <- scrape_figshare("~/Downloads/figshare_cds.html") |>
    mutate(
        species = str_replace_all(fn, "\\.fa.gz$", "")
    ) |>
        dplyr::select(species, CDS_URL = url)

# Get URLs to GFF files
annotation_urls <- scrape_figshare("~/Downloads/figshare_annotation.html") |>
    mutate(
        species = str_replace_all(fn, "\\.gff3.gz$", "")
    ) |>
        dplyr::select(species, Annotation_URL = url)

# Create a final data frame
data_urls <- species_metadata |>
    dplyr::rename(species = latin_name) |>
    left_join(cds_urls) |>
    left_join(annotation_urls) |>
    select(species_name, family, order, clade = taxonomy_3, CDS_URL, Annotation_URL) |>
    mutate(
        CDS_URL = paste0("<a href='", CDS_URL, "' target='_blank'>", "Download FASTA</a>"),
        Annotation_URL = paste0("<a href='", Annotation_URL, "' target='_blank'>", "Download GFF3</a>")
    )

# Save object
usethis::use_data(data_urls, compress = "xz")
```

# Data in `inst/extdata/`