FROM rocker/r-ver:4.4.3

# Required packages for httpuv
RUN apt-get update && apt-get install -y \
  pkg-config \
  zlib1g-dev \
  libssl-dev \
  libxml2-dev \
  libcurl4-openssl-dev
#  curl ca-certificates && rm -rf /var/lib/apt/lists/*
  
# Only if odbc is needed
#  unixodbc \
#  unixodbc-dev

ENV RENV_VERSION=v1.1.2
RUN R -e "install.packages('remotes')"
RUN R -e "remotes::install_github('rstudio/renv@${RENV_VERSION}')"
# to install from package manager instead of cran for faster installation
RUN R -e "options(renv.config.repos.override = 'https://packagemanager.posit.co/cran/latest')"

# 3. Set working directory
#WORKDIR  /srv/shiny-server/cf-app
WORKDIR  cf-app

# ---- CACHING STRATEGY STARTS HERE ----
# 1. Copy ONLY the lockfile first. This file defines your dependencies.
# This layer will only be invalidated if you change renv.lock.
COPY renv.lock .

RUN R -e "renv::activate()"

# 2. Run renv::restore() to install all the packages.
# This expensive step will now be cached most of the time!
RUN R -e "renv::restore()"

# Purge renv download cache to save ~1-2GB in the final image
# RUN R -e "renv::purge()"

# 3. NOW copy the rest of your application code.
# Changing your R scripts will only invalidate this cache and subsequent layers.
COPY . .

# ---- END OF CACHING STRATEGY ----

# ARG INIT_URL='https://elasticbeanstalk-ca-central-1-258591806620.s3.ca-central-1.amazonaws.com/cf-app/data/initPop.csv?response-content-disposition=inline&X-Amz-Content-Sha256=UNSIGNED-PAYLOAD&X-Amz-Security-Token=IQoJb3JpZ2luX2VjEHEaDGNhLWNlbnRyYWwtMSJHMEUCIQDtmi0yBWH3a4Rrml%2B6Pa460m8b%2FpDEZRWh8eOB%2BL76xQIgaIKvImjqVeFS%2FUZinY61Y3j5i4rBodwXHnTD6DUzacwquQMIOhAAGgwyNTg1OTE4MDY2MjAiDN1Nguh6xFb%2BXUhZ7CqWA9UJxS9JsFmk71ITKr2VcDiwJi5dAyecM%2FrgzIVcg%2BA%2Bt6IIcImCq7z%2B9b%2BmT6jht7Rv8pFG29i0hT3TfGDCQsxePrRgNhjDgNyjhK05GvDPCtFOAn8CBJjY7ZCfecn%2BauNPfyCVpNqhU15%2FbmPKnCeQArqA0479DlXqcWzRw3%2FceiY%2BgEzXDGTzi8m6cHzER41KEst6EEnyjbLr7E4IIfnyxsVUkR9J1qRNZ1C0FMPJDs%2BdkRi6CYV4AfihQFd5xd%2Fgoih0Tj3kf8RuuPqs8HyPgIijlmnRpWv3G19gns1ebnvLrWs11cJ5WDggxcI%2Bk95XYf7y%2BgGE8GnrdFeHsxmiekfOtHzU%2Bu0DVCwYIT8QtyX20yxUq6EPk9qfV9m9NhdwVmb8xXKLT%2F%2FGeWwSS6OUZYv6y982CSEzH%2FSWxg4QpONXYNj93xY8dI5Kr97UOVZGyBLaMYLS8n524UBbQxZoG%2BkncQX%2B9vfpUt0d0QSW%2FCOryWqM6%2B1UIxMawctWC7cBAmPsYqjVTtOjzzi00oMkLq%2Fvqnww2vjjzgY63gLhag%2B7%2B%2BLIthQuPAWkRicwamkhDyMvtBeY44E%2FoU3RJiHwf7tN8IvfuoDqzLkQWowmyEugVQz966nOtDaIVVNUTyNss%2BTNwpffMC7m9iH%2FzkW6gFxaz%2BekBKmLEz62FEtFqMEB9zoovUqtD7KkMWTOsRXoDgLIsCuB7pvcW8RmUnh5VLYvgL%2Fq5CQNX4LSmPJ4ENy0V5gZlmxTKyKsno5SlBFNJStv8aI8YzYXzQLeOmqoIH7brfLkoQa%2BATABVsLm5Q6ZDqFfJHbYd9kRuXfZZ76eW5HnvBgNj%2FbIY0saTizfm2kmq%2BdvlY01jLg7rfLF8eGlSflpGABzVjXyAL4gs2z3qEm%2ByclBMXuJU33OBL1%2FPgJaH1OBstk8Aend59zSY9FyOwsGq1xW97yVLBth8ALRPyNmzQlE3Fxao6iNoPZQCYjH%2BrjODmw%2FsXpg4%2BB%2FxIwmNOWpdPJL3TlA8A%3D%3D&X-Amz-Algorithm=AWS4-HMAC-SHA256&X-Amz-Credential=ASIATYNKF2SOB67XJQRS%2F20260411%2Fca-central-1%2Fs3%2Faws4_request&X-Amz-Date=20260411T011942Z&X-Amz-Expires=3600&X-Amz-SignedHeaders=host&X-Amz-Signature=600da15c7e40c9ff5c5e8ec9e2c7ab569cdcce294f14c04f7a7c37d47b74f0db'
# ARG COMOR_DES_URL='https://elasticbeanstalk-ca-central-1-258591806620.s3.ca-central-1.amazonaws.com/cf-app/data/ComorbiditiesDescription.csv?response-content-disposition=inline&X-Amz-Content-Sha256=UNSIGNED-PAYLOAD&X-Amz-Security-Token=IQoJb3JpZ2luX2VjEHEaDGNhLWNlbnRyYWwtMSJHMEUCIGi8n3sFsM2C8ERI3fV5%2Fg4mRFXs0KX53GQDImfdA%2FE8AiEA0QBkfqBq%2F%2BwedQuMjyAc8PCDhrub4h7X52YoVfP6oWwquQMIOxAAGgwyNTg1OTE4MDY2MjAiDJVV3B0CyDr2Y%2F3vriqWA5aEW4Suh30mahOKAqqdS5cW1XKhZRhhhuKYzQ1ap40YGLxRDQvd7oAoUX8xRbHw48Oo82SCYAzDgnBTpQhYZUYVncZdKtM61joYrCv8YlnrKZ6Rei44k%2Ft56pwEllatQ1GwdRCm05U6IePkt6isz5zw%2FIUfPOE0eoQgtT1Pv8F2kvkdOD6XavHca0DyEpQefC8dKcy%2B5sSL%2Bb4CVw7NgSLBw343t%2B4JBNJJjsgelfbZtijCBI1v%2Bco%2FkMxYTCpDc4H0GhfKaPbNa8Bk7ixv8CtsRaMfiEEbTKtOXiswF4ALxzqDK0FcCR%2Fsp7AoSBdeOz%2FIWMTjIyWp1BjX7JPLsJ848Vh6IpfzDKbHn6f0GmmDx%2Bh36NX8rLYgGWydj6hMrlHNcNOqHHusbgpCcPPgdAKXwnNrQyR%2B6m2zHn%2FhjcYmbTL5Cv6mzyVTSmwGs6LvFdhCLNe%2FhEX8tnGkQCACWyiaAxxir%2FySp8rmt%2FV29uDSXBaNMydnd4JUFaLYGZ9htNBMUJYdL8gP3ky5zT4NbiuBTgjVNjww2vjjzgY63gLl7uBhwfBkcd8RTIPOTH1p7Hxcssd3rxs7LUunXMwQC8DuOvRSzzrEbNqpYKSqi3g4a6jZmMHDnYWpONpvSCDj7gw5vY2H27WxDHGiJUucI7IVN5Z%2ByjsQgnsoqLbKQinpCgyejK5VfPS%2FD5t2PonsXZ22DWuYizkNOFY041%2F7n%2BRS9hY2FAWYsH63WVmHfnBSLHOb%2BOSpxzsbRjguLFls10MxVbUaBWEZ3CI%2F2ex3BG2ci1jdXtgYPvZBzZfehkKxMvP%2BQBu82dWFWQk4dLemJvK1HKI9Hg0SwadcxcbcCRdjaMjR%2F4gndAQ53bAjGPDsz2N2abi1SYkzCGmQc8gxfgUY3UggmVlCXVnfFbYFXg4bR%2BRvsH62DEVNJxSpvoYXryXR%2FiDFbLBfegc6CMXwROO0WBGU0xEhYMDBL7%2B%2Bv4eQLvwzV%2Fcs0JGyUuNlPEOOlR7L1cO%2BYt9xTo%2F9Iw%3D%3D&X-Amz-Algorithm=AWS4-HMAC-SHA256&X-Amz-Credential=ASIATYNKF2SOGFNUBYKN%2F20260411%2Fca-central-1%2Fs3%2Faws4_request&X-Amz-Date=20260411T012830Z&X-Amz-Expires=3600&X-Amz-SignedHeaders=host&X-Amz-Signature=6416f5cd19136ed40bcadc777ea9949378e4c60705f5a4ffaf915c648f8c61df'
# ARG COMOR_RATIOS_URL='https://elasticbeanstalk-ca-central-1-258591806620.s3.ca-central-1.amazonaws.com/cf-app/data/commorbidities_ratios.csv?response-content-disposition=inline&X-Amz-Content-Sha256=UNSIGNED-PAYLOAD&X-Amz-Security-Token=IQoJb3JpZ2luX2VjEHEaDGNhLWNlbnRyYWwtMSJHMEUCIGi8n3sFsM2C8ERI3fV5%2Fg4mRFXs0KX53GQDImfdA%2FE8AiEA0QBkfqBq%2F%2BwedQuMjyAc8PCDhrub4h7X52YoVfP6oWwquQMIOxAAGgwyNTg1OTE4MDY2MjAiDJVV3B0CyDr2Y%2F3vriqWA5aEW4Suh30mahOKAqqdS5cW1XKhZRhhhuKYzQ1ap40YGLxRDQvd7oAoUX8xRbHw48Oo82SCYAzDgnBTpQhYZUYVncZdKtM61joYrCv8YlnrKZ6Rei44k%2Ft56pwEllatQ1GwdRCm05U6IePkt6isz5zw%2FIUfPOE0eoQgtT1Pv8F2kvkdOD6XavHca0DyEpQefC8dKcy%2B5sSL%2Bb4CVw7NgSLBw343t%2B4JBNJJjsgelfbZtijCBI1v%2Bco%2FkMxYTCpDc4H0GhfKaPbNa8Bk7ixv8CtsRaMfiEEbTKtOXiswF4ALxzqDK0FcCR%2Fsp7AoSBdeOz%2FIWMTjIyWp1BjX7JPLsJ848Vh6IpfzDKbHn6f0GmmDx%2Bh36NX8rLYgGWydj6hMrlHNcNOqHHusbgpCcPPgdAKXwnNrQyR%2B6m2zHn%2FhjcYmbTL5Cv6mzyVTSmwGs6LvFdhCLNe%2FhEX8tnGkQCACWyiaAxxir%2FySp8rmt%2FV29uDSXBaNMydnd4JUFaLYGZ9htNBMUJYdL8gP3ky5zT4NbiuBTgjVNjww2vjjzgY63gLl7uBhwfBkcd8RTIPOTH1p7Hxcssd3rxs7LUunXMwQC8DuOvRSzzrEbNqpYKSqi3g4a6jZmMHDnYWpONpvSCDj7gw5vY2H27WxDHGiJUucI7IVN5Z%2ByjsQgnsoqLbKQinpCgyejK5VfPS%2FD5t2PonsXZ22DWuYizkNOFY041%2F7n%2BRS9hY2FAWYsH63WVmHfnBSLHOb%2BOSpxzsbRjguLFls10MxVbUaBWEZ3CI%2F2ex3BG2ci1jdXtgYPvZBzZfehkKxMvP%2BQBu82dWFWQk4dLemJvK1HKI9Hg0SwadcxcbcCRdjaMjR%2F4gndAQ53bAjGPDsz2N2abi1SYkzCGmQc8gxfgUY3UggmVlCXVnfFbYFXg4bR%2BRvsH62DEVNJxSpvoYXryXR%2FiDFbLBfegc6CMXwROO0WBGU0xEhYMDBL7%2B%2Bv4eQLvwzV%2Fcs0JGyUuNlPEOOlR7L1cO%2BYt9xTo%2F9Iw%3D%3D&X-Amz-Algorithm=AWS4-HMAC-SHA256&X-Amz-Credential=ASIATYNKF2SOGFNUBYKN%2F20260411%2Fca-central-1%2Fs3%2Faws4_request&X-Amz-Date=20260411T012908Z&X-Amz-Expires=3600&X-Amz-SignedHeaders=host&X-Amz-Signature=2ae23d1b71dac8eeac4cc096f19ebe2ea0cb8589718b1224980665f144f9f68b'

RUN mkdir outputs
#    mkdir data && \ 
#    curl -fsSL "$INIT_URL" -o data/initPop.csv && \
#    curl -fsSL "$COMOR_DES_URL" -o data/ComorbiditiesDescription.csv && \
#    curl -fsSL "$COMOR_RATIOS_URL" -o data/commorbidities_ratios.csv

EXPOSE 3838

# CMD ["/init"]

CMD ["R", "-e", "shiny::runApp(host='0.0.0.0', port=3838)"]
