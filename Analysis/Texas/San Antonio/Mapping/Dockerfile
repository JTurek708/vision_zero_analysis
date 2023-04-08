FROM rocker/r-ver:4.1.0

RUN apt-get update && apt-get install -y \
    libssl-dev \
    libxml2-dev \
    libcurl4-openssl-dev \
    libssh2-1-dev \
    libpq-dev \
    git \
    pandoc \
    pandoc-citeproc

RUN R -e "install.packages(c('shiny', 'dplyr', 'leaflet', 'tidyverse', 'rsconnect', 'ggrepel', 'leaflet.extras'))"

COPY map_2013.R /root/app.R
COPY ped_cycle.csv /root/ped_cycle.csv

EXPOSE 8080

CMD ["R", "-e", "shiny::runApp('/root/app.R', host = '0.0.0.0', port = 8080)"]


