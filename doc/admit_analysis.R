

admits <- read.csv("data/Inmate_Admissions_2020-05-10.csv")

as.Date(substr(levels(admits$admitted_dt)[admits$admitted_dt[1:10]],1,10),format='%m/%d/%Y')