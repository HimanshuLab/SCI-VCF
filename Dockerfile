# # Get base image 
# FROM rstudio/r-base:4.2-focal

# Base image https://hub.docker.com/u/rocker/
FROM rocker/shiny:4.2.2
# ENV SHINY_SERVER_VERSION 1.5.14.948
# RUN /rocker_scripts/install_shiny_server.sh


# Add metadata to the image
LABEL maintainer="Venkatesh K <ic11570@imail.iitm.ac.in>"

# system libraries of general use
## install debian packages
RUN apt-get update -qq && apt-get -y --no-install-recommends install \
    libxml2-dev \
    libcairo2-dev \
    libsqlite3-dev \
    libmariadbd-dev \
    libpq-dev \
    libssh2-1-dev \
    unixodbc-dev \
    libcurl4-openssl-dev \
    libssl-dev

## update system libraries
RUN apt-get update && \
    apt-get upgrade -y && \
    apt-get clean





## Install required R packages
RUN Rscript -e 'install.packages("here")'
RUN Rscript -e 'install.packages("vcfR")'
RUN Rscript -e 'install.packages("ggplot2")'
RUN Rscript -e 'install.packages("scales")'
RUN Rscript -e 'install.packages("eulerr")'
RUN Rscript -e 'install.packages("dplyr")'
RUN Rscript -e 'install.packages("tidyr")'
RUN Rscript -e 'install.packages("reshape2")'
RUN Rscript -e 'install.packages("shiny")'
RUN Rscript -e 'install.packages("shinycustomloader")'
RUN Rscript -e 'install.packages("colourpicker")'
RUN Rscript -e 'install.packages("plotly")'
RUN Rscript -e 'install.packages("reactable")'
RUN Rscript -e 'install.packages("htmltools")'


# copy necessary files
## sci-vcf.proj
COPY /SCI-VCF.Rproj ./SCI-VCF.Rproj
## app folder
COPY /R ./R


# expose port
EXPOSE 3000

# run app on container start
CMD ["R", "-e", "shiny::runApp('/R', host = '0.0.0.0', port = 3000)"]
