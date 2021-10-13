# get shiny server plus tidyverse packages image
FROM rocker/shiny-verse:latest
# system libraries of general use
RUN apt-get update && apt-get install -y \
    sudo \
    pandoc \
    pandoc-citeproc \
    libcurl4-gnutls-dev \
    libcairo2-dev \
    libxt-dev \
    libssl-dev \
    libssh2-1-dev \
    lbzip2 \
    libfftw3-dev \
    libgdal-dev \
    libgeos-dev \
    libgsl0-dev \
    libgl1-mesa-dev \
    libglu1-mesa-dev \
    libhdf4-alt-dev \
    libhdf5-dev \
    libjq-dev \
    libpq-dev \
    libproj-dev \
    libprotobuf-dev \
    libnetcdf-dev \
    libsqlite3-dev \
    libssl-dev \
    libudunits2-dev \
    netcdf-bin \
    postgis \
    protobuf-compiler \
    sqlite3 \
    tk-dev \
    unixodbc-dev

# install R packages required 
# (change it depending on the packages you need)
RUN R -e "install.packages('shinydashboard', repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('tidyverse', repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('dplyr', repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('ggplot2', repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('purrr', repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('sp', repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('rgdal', repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('sf', repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('sp', repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('geosphere', repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('Matrix', repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('igraph', repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('shp2graph', repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('maptools', repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('rgeos', repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('plotrix', repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('shiny', repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('shinythemes', repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('cowplot', repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('ggmap', repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('classInt', repos='http://cran.rstudio.com/')"
#RUN R -e "install.packages('deldir', repos='http://cran.rstudio.com/')"
#RUN R -e "install.packages('hdf5r', repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('mapdata', repos='http://cran.rstudio.com/')"
#RUN R -e "install.packages('ncdf4', repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('proj4', repos='http://cran.rstudio.com/')"
#RUN R -e "install.packages('spdep', repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('geoR', repos='http://cran.rstudio.com/')"
RUN R -e "remotes::install_github('fditraglia/forcedMigration')"

# Copy configuration files into the Docker image
COPY shiny-server.conf  /etc/shiny-server/shiny-server.conf
COPY /app /srv/shiny-server/
# Make the ShinyApp available at port 80
EXPOSE 80
# Copy further configuration files into the Docker image
COPY shiny-server.sh /usr/bin/shiny-server.sh
COPY userconf.sh /usr/bin/userconf.sh
RUN ["chmod", "+x", "/usr/bin/shiny-server.sh"]
RUN ["chmod", "+x", "/usr/bin/userconf.sh"]
CMD ["/usr/bin/shiny-server.sh"]