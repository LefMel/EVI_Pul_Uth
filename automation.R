library(EVI)
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
headers = c("Date",	"Resp_Inf", "Resp_Inf_cum",
            "Pneum", "Pneumon_cum", "Flu", "Flu_cum",
            "Attendance",
            "COVID-19",	"COVID-19 Cases_cum",	"Total","Total_cum")
colnames(data) = headers
#n.rows <- unlist(lapply(tables, function(t) dim(t)[1]))

data = data[-1,1:12]

getwd()
str(data)



# EVI_Res_Inf
if(!exists("EVI_Res_Inf"))
  load("EVI_Res_Inf")
EVI_output = EVI_Res_Inf

EVI_Res_Inf = deviant_update(as.numeric(as.character(data$Resp_Inf)), r_a = 14)
save(EVI_Res_Inf, file="EVI_Res_Inf")
load("EVI_Res_Inf")

# EVI_Pneum
if(!exists("EVI_Pneum"))
  load("EVI_Pneum")
EVI_output = EVI_Pneum

EVI_Pneum = deviant_update(as.numeric(as.character(data$Pneum), r_a=14))
save(EVI_Pneum, file="EVI_Pneum")
load("EVI_Pneum")

# EVI_Flu
if(!exists("EVI_Flu"))
  load("EVI_Flu")
EVI_output = EVI_Flu

EVI_Flu = deviant_update(as.numeric(as.character(data$Flu[206:length(data$Flu)])), r_a = 14)
save(EVI_Flu, file="EVI_Flu")
load("EVI_Flu")


# EVI_COVID
if(!exists("EVI_COVID"))
  load("EVI_COVID")
EVI_output = EVI_COVID

EVI_COVID = deviant_update(as.numeric(as.character(data[62:length(data$Flu),9])), r_a = 14)
save(EVI_COVID, file = "EVI_COVID")
load("EVI_COVID")
  
  
  