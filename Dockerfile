FROM rocker/r-ver:4.4.3

# Required packages for httpuv
RUN apt-get update && apt-get install -y \
  pkg-config \
  zlib1g-dev \
  libpq-dev \
  libssl-dev \
  libxml2-dev \
  libcurl4-openssl-dev \
#  curl ca-certificates && rm -rf /var/lib/apt/lists/*
# Only if odbc is needed
  unixodbc \
  unixodbc-dev

ARG DB_NAME
ARG DB_USER
ARG DB_HOST
ARG DB_PASS
ARG DB_PORT

ENV RENV_VERSION=v1.1.2
RUN R -e "install.packages('remotes')"
RUN R -e "remotes::install_github('rstudio/renv@${RENV_VERSION}')"
# to install from package manager instead of cran for faster installation
RUN R -e "options(renv.config.repos.override = 'https://packagemanager.posit.co/cran/latest')"

# 3. Set working directory
#WORKDIR  /srv/shiny-server/cf-app
# COPY . /app
WORKDIR /app

# ---- CACHING STRATEGY STARTS HERE ----
# 1. Copy ONLY the lockfile first. This file defines your dependencies.
# This layer will only be invalidated if you change renv.lock.

COPY renv ./renv/
COPY renv.lock .Rprofile .
# COPY .Rprofile ./app

# 2. Run renv::restore() to install all the packages.
# This expensive step will now be cached most of the time!
RUN R -e "renv::restore()"

# 3. NOW copy the rest of your application code.
# Changing your R scripts will only invalidate this cache and subsequent layers.
COPY . .

RUN echo "DB_NAME=${DB_NAME}" \
  >> ./.Renviron \
  && echo "DB_USER=${DB_USER}" \
  >> ./.Renviron \
  && echo "DB_HOST=${DB_HOST}" \
  >> ./.Renviron \
  && echo "DB_PASS=${DB_PASS}" \
  >> ./.Renviron \
  && echo "DB_PORT=${DB_PORT}" \
  >> ./.Renviron
  
# To test locally
# EXPOSE 3838

# CMD ["R", "-e", "shiny::runApp(host='0.0.0.0', port=3838)"]

# For deploying in shinyappsio
CMD Rscript deploy.R
