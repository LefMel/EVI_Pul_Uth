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
new_data = data.frame(tables[1])
new_data = new_data[-1,-1]
headers = c("Date",	"Resp_Inf", "Resp_Inf_cum",
            "Pneum", "Pneumon_cum", "Flu", "Flu_cum",
            "Attendance",
            "COVID-19",	"COVID-19 Cases_cum",	"Total","Total_cum")
colnames(new_data) = headers
#n.rows <- unlist(lapply(tables, function(t) dim(t)[1]))

new_data = new_data[-1,1:12]
getwd()
str(data)
load("data")

deviant_update=function(new_cases, cum = FALSE, r_a=7, r=0.2, lag_max=30, origin){
  #source("mova.r")
  #source("medvol.r")
  #source("evi.r")
  #source("evifcut.r")
  #source("indic.r")
  #source("status.r")
  #source("rollsd.r")
  
  
  start_cases=14
  lag_1=7
  c_1=0.01
  w_s =7
  
  
  
  
  if (cum == TRUE) new_cases = c(new_cases[1], diff(new_cases))
  
  if (!exists("EVI_output"))
    stop("Please run the deviant function first")
  
  #calculate the moving average of new confrimed cases
  cases=mova(new_cases,r_a)
  
  roll=rollsd(cases[1:start_cases],lag_1)
  ev=evi(roll)
  ind=indic(ev,c_1, cases[1:start_cases])
  status=status(cases[1:start_cases],r)
  
  #initiate chain for positive predictive value
  ppv=rep(NA, length(cases))
  
  #initiate chain for negative predictive value
  npv=rep(NA, length(cases))
  
  lag_all=rep(NA, start_cases)
  c_all=rep(NA, start_cases)
  
  se_all=rep(NA, start_cases)
  sp_all=rep(NA, start_cases)
  
  
  lag_all[1:start_cases]=lag_1
  c_all[1:start_cases]=c_1
  
  diff= length(cases)-(nrow(EVI_output) +1)
  for (i in (nrow(EVI_output)+1): length(cases)){
    
    case_t=cases[1:i]
    #case_t=cases[max(1,(i-33)):i]
    #lag_s=7
    lag_s=seq(lag_1,min(lag_max,(length(case_t)-1)), 1)
    #lag_s=seq(lag_1,min(length(case_t),50), 1)
    c_s=seq(0.01,0.5, 0.01)
    #all_j=NA
    
    all_lag=NA
    all_cut=NA
    all_se=NA
    all_sp=NA
    
    
    
    for (j in lag_s){
      roll_t=rollsd(case_t,j)
      ev_t=evi(roll_t)
      for (l in c_s){
        evicut_t=evifcut(ev_t, case_t, l, r)
        new_j=j
        new_l=l
        new_se=evicut_t$sens
        new_sp=evicut_t$spec
        all_lag[[length(all_lag) + 1]] <- new_j
        all_cut[[length(all_cut) + 1]] <- new_l
        all_se[[length(all_se) + 1]] <- new_se
        all_sp[[length(all_sp) + 1]] <- new_sp
        
        
      }
    }
    
    
    
    sesp=as.data.frame(cbind(all_lag,all_cut,all_se,all_sp))
    
    
    
    
    #Select the row with the right window and cut
    index=which.max(sesp$all_se+sesp$all_sp-1)
    
    #index=sesp[which(sesp$all_sp>0.80),]
    #index=which.max(index$all_se)
    #index=which(sesp$all_se==1 & sesp$all_sp>=0.95),1)
    #if (i>40)
    #   {index1=sesp[which(sesp$all_sp>0.95),]
    #  index=which.max(index1$all_se)
    #   }
    #else
    #{index=which.max(sesp$all_se+sesp$all_sp-1)}
    
    
    #index=which(sesp$se>=0.6 & sesp$sp>0.9)
    print(i)
    print(sesp[index,])
    
    
    
    #estimate the parameters for the last observed case
    lag_n=sesp$all_lag[index]
    c_n=sesp$all_cut[index]
    
    roll_n=rollsd(cases[1:i],lag_n)
    
    ev_n=evi(roll_n)
    ind_n=indic(ev_n,c_n, case_t)
    evicut_n=evifcut(ev_n, case_t, c_n, r)
    
    roll=c(roll,roll_n[i])
    ev=c(ev,ev_n[i])
    ind=c(ind, ind_n[i])
    
    lag_all=c(lag_all,lag_n)
    c_all=c(c_all,c_n)
    
    se_all=c(se_all,all_se[index])
    sp_all=c(sp_all,all_sp[index])
    
    ppv[i]=evicut_n$prev*all_se[index]/
      (evicut_n$prev*all_se[index]+(1-evicut_n$prev)*(1-all_sp[index]))
    
    npv[i]=(1-evicut_n$prev)*all_sp[index]/
      ((1-evicut_n$prev)*all_sp[index]+evicut_n$prev*(1-all_se[index]))
    
    
    
  }
  
  
  Days=as.Date(2*((length(cases)-diff):length(cases))-2, origin=origin)
  EVI=ev[((length(ev)-diff):length(ev))]
  Cases=cases[((length(cases)-diff):length(cases))]
  Index=ind[((length(ind)-diff):length(ind))]
  ppv=ppv[((length(ppv)-diff):length(ppv))]
  npv=npv[((length(npv)-diff):length(npv))]
  lag_all=lag_all[((length(lag_all)-diff):length(lag_all))]
  c_all=c_all[((length(c_all)-diff):length(c_all))]
  se_all=se_all[((length(se_all)-diff):length(se_all))]
  sp_all=sp_all[((length(sp_all)-diff):length(sp_all))]
  
  
  EVI_out_add=data.frame(Days, EVI, Cases, Index, ppv, npv,
                                  lag_all, c_all, se_all, sp_all)
  
  EVI_output=rbind(EVI_output,EVI_out_add)
  
  EVI_output<<-(EVI_output)
  
  return(EVI_output)
  
}

if(identical(data, new_data) == TRUE){
  print("No Changes have been made")
  } else {
    
      # EVI_Res_Inf
      if(!exists("EVI_Res_Inf"))
        load("EVI_Res_Inf")
      EVI_output = EVI_Res_Inf
      
      EVI_Res_Inf = deviant_update(as.numeric(as.character(new_data$Resp_Inf)), r_a = 14, origin = "2021-10-02")
      save(EVI_Res_Inf, file="EVI_Res_Inf")
      load("EVI_Res_Inf")
      
      # EVI_Pneum
      if(!exists("EVI_Pneum"))
        load("EVI_Pneum")
      EVI_output = EVI_Pneum
      
      EVI_Pneum = deviant_update(as.numeric(as.character(data$Pneum), r_a=14), origin = "2021-10-02")
      save(EVI_Pneum, file="EVI_Pneum")
      load("EVI_Pneum")
      
      # EVI_Flu
      if(!exists("EVI_Flu"))
        load("EVI_Flu")
      EVI_output = EVI_Flu
      
      EVI_Flu = deviant_update(as.numeric(as.character(data$Flu[206:length(data$Flu)])), r_a = 14, origin = "2022-11-16")
      save(EVI_Flu, file="EVI_Flu")
      load("EVI_Flu")
      
      
      # EVI_COVID
      if(!exists("EVI_COVID"))
        load("EVI_COVID")
      EVI_output = EVI_COVID
      
      EVI_COVID = deviant_update(as.numeric(as.character(data[62:length(data$Flu),9])), r_a = 14,  origin = "2022-02-01")
      save(EVI_COVID, file = "EVI_COVID")
      load("EVI_COVID")
        
  }  

data = new_data
  