
FROM rocker/r-ver:4.4.1

RUN apt-get update && apt-get install -y 
    libcurl4-openssl-dev libssl-dev libxml2-dev 
 && rm -rf /var/lib/apt/lists/*

RUN R -q -e "install.packages(c('plumber','jsonlite'), repos='https://cloud.r-project.org')"

WORKDIR /src
COPY . /src
RUN R -q -e "install.packages('devtools', repos='https://cloud.r-project.org'); devtools::install_local('/src', upgrade = 'never')"

EXPOSE 8080
CMD ["R","-q","-e","rpcompute::serve(host='0.0.0.0', port=8080)"]
