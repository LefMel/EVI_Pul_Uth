# Upload Shiny app

#install.packages('rsconnect')
library(rsconnect)

rsconnect::setAccountInfo(name='lefmel',
                          token='AA86A3445C51DA1A48DC0F63BE3D6297',
                          secret='2sRpAPtGz2dtG7Wi97wWoKcFqQGOQD3NsVdyyL6B')

setwd("C:/Users/LefMel/Documents/GitHub/EVI_Pul_Uth/Shiny_App")

deployApp()
rsconnect::showLogs()
# googledrive in a non-interactive session?

getwd()
list.files()


# Install the latest odbc release from CRAN:
install.packages("odbc")

# Or the the development version from GitHub:
# install.packages(devtools)
devtools::install_github("rstats-db/odbc")

con <- DBI::dbConnect(odbc::odbc(),
                      driver = "PostgreSQL Driver",
                      database = "test_db",
                      UID    = rstudioapi::askForPassword("Database user"),
                      PWD    = rstudioapi::askForPassword("Database password"),
                      host = "localhost",
                      port = 5432)