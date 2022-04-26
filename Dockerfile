FROM rocker/r-apt:bionic
WORKDIR /api_modelled_need

# Install R packages from Ubuntu binaries where possible
RUN apt-get update && apt-get install -y -qq \
  r-cran-plumber \
  r-cran-e1071 \
  r-cran-caret \
  r-cran-tidyverse \
  r-cran-rlang \
  r-cran-mass \
  r-cran-rpostgresql \
  r-cran-plyr

# Install remaining R packages from source
RUN R -e "install.packages(c('config', 'PRROC', 'xgboost', 'rapiclient'))"

# Copy over scripts and data needed to run R API
COPY ./user_defined_functions/ user_defined_functions/.
COPY ./ModelledNeedsAPI.R .
COPY ./sql.server.credentials.yml .
COPY ./config.yml .
COPY ./RunModelledNeeds.R .

# Set environment variables
ARG PGDATABASE
ARG PGPORT
ARG POSTGRES_PW
ARG POSTGRES_UN
ENV PG_DATABASE=${PGDATABASE}
ENV PG_PORT=${PGPORT}
ENV PG_PASSWORD=${POSTGRES_PW}
ENV PG_USERNAME=${POSTGRES_UN}
ARG SITE_URL
ENV SITE_URL=${SITE_URL}

# Release port for API call
EXPOSE 8092

# Run R API
ENTRYPOINT ["R", "-e", "source('RunModelledNeeds.R')"]
