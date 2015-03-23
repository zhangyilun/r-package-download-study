# setwd
setwd("Mine/Projects/R_package_download_stat/")

## ======================================================================
## Step 1: Download all log files
## ======================================================================

# Get all the URLs in R
start <- as.Date('2012-10-01')
today <- as.Date('2015-03-19')

all_days <- seq(start, today, by = 'day')

# set filenames
year <- as.POSIXlt(all_days)$year + 1900
urls <- paste0('http://cran-logs.rstudio.com/', year, '/', all_days, '.csv.gz')

# only download the files you don't have
missing_days <- setdiff(as.character(all_days), tools::file_path_sans_ext(dir("CRANlogs"), TRUE))

# download
dir.create("CRANlogs")
for (i in 1:length(missing_days)) {
    print(paste0(i, "/", length(missing_days)))
    download.file(urls[i], paste0('CRANlogs/', missing_days[i], '.csv.gz'))
}


##======================================================================
## Step 2: Load single data files into one big data.table
## ======================================================================

# get the list of files downloaded
file_list <- list.files("CRANlogs", full.names=TRUE)

# read files
# too large to read all files together..
# files get larger and larger as we get closer to nowadays (people download more!)

# logs1 <- list()
# logs2 <- list()
logs3 <- list()   # 731 - 807 (year 2015)
# for (file in file_list[1:400]) {
#     print(paste("Reading", file, "..."))
#     logs1[[file]] <- read.table(file, header = TRUE, sep = ",", quote = "\"",
#                                 dec = ".", fill = TRUE, comment.char = "", as.is=TRUE)
# }
# for (file in file_list[401:600]) {
#     print(paste("Reading", file, "..."))
#     logs2[[file]] <- read.table(file, header = TRUE, sep = ",", quote = "\"",
#                                 dec = ".", fill = TRUE, comment.char = "", as.is=TRUE)
# }
for (file in file_list[731:807]) {
    print(paste("Reading", file, "..."))
    logs3[[file]] <- read.table(file, header = TRUE, sep = ",", quote = "\"",
                                dec = ".", fill = TRUE, comment.char = "", as.is=TRUE)
}

# rbind together all files
library(data.table)
# dat1 <- rbindlist(logs1)
# dat2 <- rbindlist(logs2)
dat3 <- rbindlist(logs3)

# remove logs3 memory
rm(logs3)

# add keys and define variable types
dat3[, date:=as.Date(date)]
dat3[, package:=factor(package)]
dat3[, country:=factor(country)]
dat3[, weekday:=weekdays(date)]
dat3[, week:=strftime(as.POSIXlt(date),format="%Y-%W")]

setkey(dat3, package, date, week, country)

# remove unused columns
dat3_reduced <- dat3[,c("date","time","package","country","ip_id","weekday","week"),with=FALSE]
dat3 <- dat3_reduced

# save file
save(dat3, file="CRANlogs/CRANlogs2015.RData")

# for later analyses: load the saved data.table
# load("CRANlogs/CRANlogs2015.RData")

## ======================================================================
## Step 3: Analyze it!
## ======================================================================

library(ggplot2)
library(plyr)
library(data.table)

# load
load("CRANlogs/CRANlogs2015.RData")

# top 10 downloaded packages all time from: http://www.rdocumentation.org/
# ggplot2
# plyr
# Rcpp
# digest
# stringr
# RColorBrewer
# reshape2
# colorspace
# scales
# labeling
package_list <- c("ggplot2","plyr","Rcpp","digest","stringr","RColorBrewer",
                  "reshape2","colorspace","scales","labeling")

# Overall downloads of packages in 2015 by March 18th
d1 <- dat3[, length(week), by=package]
d1 <- d1[order(V1), ]
d1[package=="ggplot2", ]
d1[package=="plyr", ]

# write d1 (for future analysis)
# write.csv(d1,"All_Time_Download.csv",quote=F,row.names=F)
# to load
# read.csv("All_Time_Download.csv")

# create key (function in data.table package)
setkey(dat3, package, date, week, country)

# plot : Compare downloads of selected packages on a weekly basis
agg1 <- dat3[J(package_list), length(unique(ip_id)), by=c("week", "package")]

ggplot(agg1, aes(x=week, y=V1, color=package, group=package)) + 
    geom_line(lwd=1) + 
    ylab("Downloads") +
    theme_bw() + 
    theme(axis.text.x  = element_text(angle=90, size=8, vjust=0.5)) +
    scale_y_continuous(limits = c(0, 20000))

