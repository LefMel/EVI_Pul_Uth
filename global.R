library(googledrive)
library(XML)
library(rlist)
library(EVI)

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
headers = c("Date",	"Resp_Inf", "Resp_Inf_cum",
            "Pneum", "Pneumon_cum", "Flu", "Flu_cum",
            "Attendance",
            "COVID-19",	"COVID-19 Cases_cum",	"Total","Total_cum")
colnames(data) = headers
#n.rows <- unlist(lapply(tables, function(t) dim(t)[1]))

data = data[-1,1:12]

getwd()
str(data)

deviant <-  function (new_cases, cum = FALSE, r_a = 7, r = 0.2, lag_max = 30, origin) {
  start_time = Sys.time()
  start_cases = 14
  lag_1 = 7
  c_1 = 0.01
  w_s = 7
  if (cum == TRUE) 
    new_cases = c(new_cases[1], diff(new_cases))
  cases = mova(new_cases, r_a)
  roll = rollsd(cases[1:start_cases], lag_1)
  ev = evi(roll)
  ind = indic(ev, c_1, cases[1:start_cases])
  status = status(cases[1:start_cases], r)
  ppv = rep(NA, length(cases))
  npv = rep(NA, length(cases))
  lag_all = rep(NA, start_cases)
  c_all = rep(NA, start_cases)
  se_all = rep(NA, start_cases)
  sp_all = rep(NA, start_cases)
  lag_all[1:start_cases] = lag_1
  c_all[1:start_cases] = c_1
  for (i in (start_cases + 1):length(cases)) {
    case_t = cases[1:i]
    lag_s = seq(lag_1, min(lag_max, (length(case_t) - 1)), 
                1)
    c_s = seq(0.01, 0.5, 0.01)
    all_lag = NA
    all_cut = NA
    all_se = NA
    all_sp = NA
    for (j in lag_s) {
      roll_t = rollsd(case_t, j)
      ev_t = evi(roll_t)
      for (l in c_s) {
        evicut_t = evifcut(ev_t, case_t, l, r)
        new_j = j
        new_l = l
        new_se = evicut_t$sens
        new_sp = evicut_t$spec
        all_lag[[length(all_lag) + 1]] <- new_j
        all_cut[[length(all_cut) + 1]] <- new_l
        all_se[[length(all_se) + 1]] <- new_se
        all_sp[[length(all_sp) + 1]] <- new_sp
      }
    }
    sesp = as.data.frame(cbind(all_lag, all_cut, all_se, 
                               all_sp))
    index = which.max(sesp$all_se + sesp$all_sp - 1)
    print(i)
    print(sesp[index, ])
    lag_n = sesp$all_lag[index]
    c_n = sesp$all_cut[index]
    roll_n = rollsd(cases[1:i], lag_n)
    ev_n = evi(roll_n)
    ind_n = indic(ev_n, c_n, case_t)
    evicut_n = evifcut(ev_n, case_t, c_n, r)
    roll = c(roll, roll_n[i])
    ev = c(ev, ev_n[i])
    ind = c(ind, ind_n[i])
    lag_all = c(lag_all, lag_n)
    c_all = c(c_all, c_n)
    se_all = c(se_all, all_se[index])
    sp_all = c(sp_all, all_sp[index])
    ppv[i] = evicut_n$prev * all_se[index]/(evicut_n$prev * 
                                              all_se[index] + (1 - evicut_n$prev) * (1 - all_sp[index]))
    npv[i] = (1 - evicut_n$prev) * all_sp[index]/((1 - evicut_n$prev) * 
                                                    all_sp[index] + evicut_n$prev * (1 - all_se[index]))
  }
  Days = (as.Date(seq(0, (2*(length(cases)-1)), 2), origin=origin)) # "2021-10-02" - Res, Pneum | "2022-02-01" - COVID | "2022-11-16" - Flu
  EVI = ev
  Cases = cases
  Index = ind
  EVI_out = data.frame(Days, EVI, Cases, Index, ppv, 
                                npv, lag_all, c_all, se_all, sp_all)
  EVI_output = EVI_out
  EVI_output <<- (EVI_output)
  end_time = Sys.time()
  time_elapsed = end_time - start_time
  print(time_elapsed)
  return(EVI_output)
}



#remotes::install_github("ku-awdc/EVI")

EVI_Res_Inf = deviant(as.numeric(as.character(data$Resp_Inf[1:243])), r_a = 14, origin = "2021-10-02")
save(EVI_Res_Inf, file="EVI_Res_Inf")
EVI_Pneum = deviant(as.numeric(as.character(data$Pneum[1:243]), r_a=14), origin = "2021-10-02")
save(EVI_Pneum, file="EVI_Pneum")
EVI_Flu = deviant(as.numeric(as.character(data$Flu[206:244])), r_a = 14, origin = "2022-11-16")
save(EVI_Flu, file="EVI_Flu")
EVI_COVID = deviant(as.numeric(as.character(data[62:243,9])), r_a = 14, origin = "2022-02-01")
save(EVI_COVID, file = "EVI_COVID")


list.files()

