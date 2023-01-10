library(googledrive)
library(XML)
library(rlist)
library(EVI) # doesn't work

#getwd()
#setwd("C:/Users/LefMel/Documents/EVI Uth/Final_files")
getwd()

# R lang update to 1.0.6.
#install.packages("pak")
#pak::pkg_install("r-lib/rlang")

drive_deauth()
temp <- tempfile(tmpdir = getwd(), fileext = ".zip")
dl <- drive_download(
  as_id("1gdWDhELYMU0lmCQz3po3Ydb7Eb1gF7j_NCs_xluCXQI"), path = temp, overwrite = TRUE)
out <- unzip(temp, exdir = tempdir())

link = paste("file:///",out[1], sep="")

# Works 
tables <- readHTMLTable(link)
tables <- list.clean(tables, fun = is.null, recursive = FALSE)
data = data.frame(tables[1])
data = data[-1,-1]
headers = c("Date",	"Respiratory Infections", "Respiratory Infections_Cum",
            "Pneumonia", "Pneumonia_Cum", "Flu", "Flu_Cum",
            "Attendance at Emergency Department",
            "COVID-19 Cases",	"COVID-19 Cases_Cum",	"Total","Total_Cum")
colnames(data) = headers
#n.rows <- unlist(lapply(tables, function(t) dim(t)[1]))

data = data[-1,1:12]

getwd()

library(EVI)

remotes::install_github("ku-awdc/EVI")

EVI_Res_Inf = deviant(as.numeric(data$`Respiratory Infections`), r_a = 14)
EVI_Pneum = deviant(as.numeric(data$Pneumonia, r_a=14))
EVI_COVID = deviant(as.numeric(data$`COVID-19 Cases`[62:length(data$`COVID-19 Cases`)]), r_a=14)
