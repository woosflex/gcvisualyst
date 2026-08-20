# gcvisualyst Shiny app — self-hosted container
# Deploys the bundled Shiny app from the gcvisualyst R package.
FROM rocker/shiny:4.5.3

# Debian build deps for compiling R packages (tidyverse stack)
RUN apt-get update && apt-get install -y --no-install-recommends \
    libcurl4-openssl-dev libssl-dev libxml2-dev libfontconfig1-dev \
    libharfbuzz-dev libfribidi-dev libfreetype6-dev libpng-dev \
    libtiff5-dev libjpeg-dev libuv1 git \
    && rm -rf /var/lib/apt/lists/*

# Install gcvisualyst (pulls dplyr/ggplot2/tidyr/stringr/purrr/readr) + bslib/shiny for the app
RUN install2.r --error --skipinstalled --ncpus 4 \
        bslib dplyr ggplot2 tidyr stringr purrr readr remotes
RUN R -e 'remotes::install_github("woosflex/gcvisualyst", ref="main", upgrade="never")'

# Publish the bundled app under shiny-server's app dir
RUN cp -r /usr/local/lib/R/site-library/gcvisualyst/shiny /srv/shiny-server/gcvisualyst \
    && chmod -R 755 /srv/shiny-server/gcvisualyst

EXPOSE 3838
CMD ["/usr/bin/shiny-server"]
