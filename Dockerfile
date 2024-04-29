FROM rocker/rstudio:latest

RUN apt-get update && apt-get install -y \
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev \
    libudunits2-dev \
    libprotobuf-dev \
    protobuf-compiler \
    libproj-dev \
    libgdal-dev \
    libmagick++-dev \
    && rm -rf /var/lib/apt/lists/*

RUN mkdir -p /home/epic
WORKDIR /home/epic

# Install required R packages from CRAN
RUN install2.r --error \
    shiny \
    sf \
    leaflet \
    reactable \
    aws.s3 \
    geojsonsf \
    leaflet.extras \
    htmltools \
    dplyr \
    ggplot2 \
    plotly \
    shinybusy \
    stringr \
    scales \
    shinyalert \
    reactable \
    shinyjs \
    shinycssloaders \
    purrr \
    shinyBS \
    rlang \
    viridis \
    reactablefmtr \
    googlesheets4 \
    reactable.extras \
    tippy \
    httpuv \
    aws.ec2metadata \
    remotes \
    promises \
    future \ 
    maps \
    knitr 

# Install the bivariatechoropleths package from GitHub
RUN R -e "remotes::install_github('chris31415926535/bivariatechoropleths')"

# Copy the R script
ADD app/tx-dw-tool-app.R /home/epic/

# Expose ports
EXPOSE 2000 2001

# Set the working directory and command to run the app
WORKDIR /home/epic
CMD ["R", "-e", "httpuv::startServer('0.0.0.0', 2001, list(call = function(req) { list(status = 200, body = 'OK', headers = list('Content-Type' = 'text/plain')) })); shiny::runApp('/home/epic/tx-dw-tool-app.R', port = 2000, host = '0.0.0.0')"]
