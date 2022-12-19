getwd()
setwd("C:/Users/LefMel/Documents/EVI Uth")
list.files()

library(openxlsx)

## function for runif
miss_uni = function(x,a,b){
  if(is.na(b))b=a
  
  if (a > b){
    y = floor((runif(x,b,a)))
  }
  else{
    y = floor(runif(x,a,b))
  }
  
  return (y)
}


deviant(draft$Respiratory.Infections, r_a=14)

draft = read.xlsx("Dataset Cases UTH_Official.xlsx", detectDates = TRUE)
View(draft)

data = draft[1:187,]
View(data)
str(data)

for (i in (seq((nrow(data))*2,4,-1))){
  
  if (i%%2==0){
  data[i,] = data[i/2,]
  }
  else{
  data[i,]$Date = as.Date(data[i+1,]$Date) - 1
  #data[i,]$Date = as.Date(as.numeric(data[i,]$Date), origin="1970-01-01")
  }
}

for (i in (4:nrow(data))){
  if (i%%2 != 0){
    data[i,]$Respiratory.Infections = miss_uni(1, a = as.numeric(data$Respiratory.Infections[i-1]), b = as.numeric(data$Respiratory.Infections[i+1]))
    data[i,]$Pneumonia = miss_uni(1, a = as.numeric(data$Pneumonia[i-1]), b = as.numeric(data$Pneumonia[i+1]))
    data[i,]$Attendance.at.Emergency.Department = miss_uni(1, a = as.numeric(data$Attendance.at.Emergency.Department[i-1]), b = as.numeric(data$Attendance.at.Emergency.Department[i+1]))
    data[i,]$Total = miss_uni(1, a = as.numeric(data$Total[i-1]), b = as.numeric(data$Total[i+1]))
  }
  else{
    data[i,]$Respiratory.Infections = as.numeric(data$Respiratory.Infections[i])
    data[i,]$Pneumonia = as.numeric(data$Pneumonia[i])
    data[i,]$Attendance.at.Emergency.Department = as.numeric(data$Attendance.at.Emergency.Department[i])
    data[i,]$Total = as.numeric(data$Total[i])
  }
#  data[4,]$Respiratory.Infections =  as.numeric(data$Respiratory.Infections[4])
#  data[i,]$Respiratory.Infections =  as.numeric(data$Respiratory.Infections[i-1]) + as.numeric(data$Respiratory.Infections[i])
  
#  data[4,]$Pneumonia =  as.numeric(data$Pneumonia[4])
#  data[i,]$Pneumonia =  as.numeric(data$Pneumonia[i-1]) + as.numeric(data$Pneumonia[i])
  
#  data[4,]$Total =  as.numeric(data$Total[4])
#  data[i,]$Total =  as.numeric(data$Total[i-1]) + as.numeric(data$Total[i])

}

View(data)
str(data)
EVI = data[c(1,4:374),]
str(EVI)
library(EVI)
# Respiratory infections
deviant(as.numeric(EVI$Respiratory.Infections), r_a = 14)
EVI::evi.graphs(EVI_output, ln=F)
EVI::evi.graphs(EVI_output, ln=T)

# Pneumonia
deviant(as.numeric(EVI$Pneumonia))
EVI::evi.graphs(EVI_output, ln=F)
EVI::evi.graphs(EVI_output, ln=T)

# Attendance at Emergency department
deviant(as.numeric(EVI$Attendance.at.Emergency.Department))
EVI::evi.graphs(EVI_output, ln=F)
EVI::evi.graphs(EVI_output, ln=T)

# Total
deviant(as.numeric(EVI$`Total.(Respiratory.inf.+.Pneumonia)`))
EVI::evi.graphs(EVI_output, ln=F)
EVI::evi.graphs(EVI_output, ln=T)


EVI[5,]$Date = as.Date(as.numeric(data[5,]$Date), origin="1970-01-01")

