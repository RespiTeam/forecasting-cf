FROM rocker/r-ver:4.4.3

# Required packages for httpuv
RUN apt-get update && apt-get install -y \
  pkg-config \
  zlib1g-dev \
  libssl-dev \
  libxml2-dev \
  libcurl4-openssl-dev
  
# Only if odbc is needed
#  unixodbc \
#  unixodbc-dev

ENV RENV_VERSION=v1.1.2
RUN R -e "install.packages('remotes')"
RUN R -e "remotes::install_github('rstudio/renv@${RENV_VERSION}')"
# to install from package manager instead of cran for faster installation
RUN R -e "options(renv.config.repos.override = 'https://packagemanager.posit.co/cran/latest')"

COPY . /app
WORKDIR /app

RUN R -e "renv::restore()"

EXPOSE 3838

CMD ["R", "-e", "shiny::runApp(host='0.0.0.0', port = 3838)"]