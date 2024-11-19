FROM rocker/verse:4.4.1

# Install system dependencies
RUN apt-get update && apt-get install -y \
    libcairo2-dev \
    libcurl4-openssl-dev \
    libfontconfig1-dev \
    libfreetype6-dev \
    libfribidi-dev \
    libglpk-dev \
    libharfbuzz-dev \
    libicu-dev \
    libjpeg-dev \
    libpng-dev \
    libssl-dev \
    libxml2-dev \
    make \
    pandoc \
    zlib1g-dev && \
    rm -rf /var/lib/apt/lists/*

# Install packages
RUN mkdir -p /usr/local/lib/R/etc/ /usr/lib/R/etc/
RUN echo "options(repos = c(CRAN = 'https://cran.rstudio.com/'), download.file.method = 'libcurl', Ncpus = 4)" | tee /usr/local/lib/R/etc/Rprofile.site | tee /usr/lib/R/etc/Rprofile.site
RUN R -e 'install.packages("remotes")'
RUN R -e 'install.packages("BiocManager")'
RUN R -e 'BiocManager::install("ggtree", ask = FALSE, update = FALSE)'
RUN Rscript -e 'remotes::install_version("rlang",upgrade="never", version = "1.1.4")'
RUN Rscript -e 'remotes::install_version("htmltools",upgrade="never", version = "0.5.8.1")'
RUN Rscript -e 'remotes::install_version("scales",upgrade="never", version = "1.3.0")'
RUN Rscript -e 'remotes::install_version("shiny",upgrade="never", version = "1.8.1.1")'
RUN Rscript -e 'remotes::install_version("ggplot2",upgrade="never", version = "3.5.1")'
RUN Rscript -e 'remotes::install_version("testthat",upgrade="never", version = "3.2.1.1")'
RUN Rscript -e 'remotes::install_version("config",upgrade="never", version = "0.3.2")'
RUN Rscript -e 'remotes::install_version("ape",upgrade="never", version = "5.8")'
RUN Rscript -e 'remotes::install_version("shinydashboard",upgrade="never", version = "0.7.2")'
RUN Rscript -e 'remotes::install_version("Cairo",upgrade="never", version = "1.6-2")'
RUN Rscript -e 'remotes::install_version("shinytest2",upgrade="never", version = "0.3.2")'
RUN Rscript -e 'remotes::install_version("shinyjs",upgrade="never", version = "2.1.0")'
RUN Rscript -e 'remotes::install_version("ggiraph",upgrade="never", version = "0.8.10")'
RUN Rscript -e 'remotes::install_version("shinycssloaders",upgrade="never", version = "1.0.0")'
RUN Rscript -e 'remotes::install_version("golem",upgrade="never", version = "0.4.1")'
RUN Rscript -e 'remotes::install_version("patchwork",upgrade="never", version = "1.2.0")'
RUN Rscript -e 'remotes::install_version("DT",upgrade="never", version = "0.33")'
RUN Rscript -e 'remotes::install_version("shinyWidgets",upgrade="never", version = "0.8.6")'
RUN Rscript -e 'remotes::install_version("phangorn",upgrade="never", version = "2.11.1")'
RUN Rscript -e 'remotes::install_version("tidytree",upgrade="never", version = "0.4.6")'
RUN Rscript -e 'remotes::install_version("geomtextpath",upgrade="never", version = "0.1.4")'
RUN Rscript -e 'remotes::install_github("RinteRface/shinydashboardPlus@2b1caf45df07592d3385f6bafeb05f96445b009d")'
RUN Rscript -e 'remotes::install_github("willgearty/deeptime@7e4ea448d3be7f5921cd022fd84cc2d5844eff96")'

RUN mkdir /build_zone
ADD . /build_zone
WORKDIR /build_zone
RUN R -e 'remotes::install_local(upgrade="never")'
RUN rm -rf /build_zone
EXPOSE 80
CMD R -e "options('shiny.port'=80,shiny.host='0.0.0.0'); library(AngioWGD); library(Cairo); options(shiny.usecairo = TRUE); AngioWGD::run_app()"
