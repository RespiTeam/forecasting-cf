FROM rocker/r-ver:4.4.3

# Only if odbc is needed
# RUN apt-get update && apt-get install -y \
#  unixodbc \
#  unixodbc-dev

ENV RENV_VERSION=v1.0.7
RUN R -e "install.packages('remotes')"
RUN R -e "remotes::install_github('rstudio/renv@${RENV_VERSION}')"
# to install from package manager instead of cran for faster installation
RUN R -e "options(renv.config.repos.override = 'https://packagemanager.posit.co/cran/latest')"

COPY . /app
WORKDIR /app

RUN R -e "renv::restore()"

EXPOSE 3838

CMD ("R", "e", "shiny::runApp('./app.R', host='0.0.0.0', port = 3838)")