
# Packages
library(googledrive)
library(XML)
library(rlist)
#library(RCurl)
#getwd()
setwd("C:/Users/LefMel/Documents/EVI Uth")

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
n.rows <- unlist(lapply(tables, function(t) dim(t)[1]))
View(data)
data = data[-1,1:12]
View(data)
str(data)
#str(data)
#data = as.numeric(data)
#View(data)





library(EVI)

# R_A 14 
deviant(as.numeric(data$`Respiratory Infections`), r_a = 14)
Res_R_A_14 = EVI_output 
EVI::evi.graphs(Res_R_A_14, ln=F)
EVI::evi.graphs(Res_R_A_14, ln=T)
EVI::evi.graphs(Res_R_A_14, graph = "PPV")
EVI::evi.graphs(Res_R_A_14, graph = "PPV", ln=F)

deviant(as.numeric(data$Pneumonia), r_a = 14)
Pne_R_A_14 = EVI_output 
EVI::evi.graphs(Pne_R_A_14, ln=F)
EVI::evi.graphs(Pne_R_A_14, ln=T)

deviant(as.numeric(data$`Attendance at Emergency Department`), r_a = 14)
Att_R_A_14 = EVI_output 
EVI::evi.graphs(Att_R_A_14, ln=F)
EVI::evi.graphs(Att_R_A_14, ln=T)


deviant(as.numeric(data$Total), r_a = 14)
Tot_R_A_14 = EVI_output 
EVI::evi.graphs(Tot_R_A_14, ln=F)
EVI::evi.graphs(Tot_R_A_14, ln=T)

deviant(as.numeric(data$`COVID-19 Cases`[62:219]), r_a = 14)
COV_R_A_14 = EVI_output 
EVI::evi.graphs(COV_R_A_14, ln=F)
EVI::evi.graphs(COV_R_A_14, ln=T)


deviant(as.numeric(data$Flu[206:219]), r_a = 1)

#APP DRAFT

#p("Figure 1. Daily confirmed cases of respiratory infections, presented on the original scale, with red dots corresponding to dates that, 
#  according to EVI, an early warning was issued."),

#p("Figure 2. Daily confirmed cases of pneumonia, presented on the original scale, with red dots corresponding to dates that, 
#  according to EVI, an early warning was issued."),

#p("Figure 3. Daily confirmed number of attendace at the emergency department, presented on the original scale, with red dots corresponding to dates that, 
#  according to EVI, an early warning was issued."),

