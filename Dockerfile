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

# 3. Set working directory
WORKDIR /app

# ---- CACHING STRATEGY STARTS HERE ----
# 1. Copy ONLY the lockfile first. This file defines your dependencies.
# This layer will only be invalidated if you change renv.lock.
COPY renv.lock ./

# 2. Run renv::restore() to install all the packages.
# This expensive step will now be cached most of the time!
RUN R -e "renv::restore()"

# Purge renv download cache to save ~1-2GB in the final image
RUN R -e "renv::purge()"

# 3. NOW copy the rest of your application code.
# Changing your R scripts will only invalidate this cache and subsequent layers.
COPY . ./

# ---- END OF CACHING STRATEGY ----

EXPOSE 3838

CMD ["R", "-e", "shiny::runApp('/app', host='0.0.0.0', port = 3838)"]
