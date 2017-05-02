fileUrl <- "ftp://ftp.ncdc.noaa.gov/pub/data/swdi/stormevents/csvfiles/StormEvents_details-ftp_v1.0_d2017_c20170419.csv.gz"


download.file(fileUrl, "details2017.csv.gz", method = "curl")


url <- "ftp://ftp.ncdc.noaa.gov/pub/data/swdi/stormevents/csvfiles/legacy/stormdata_2013.csv"

readLines(url, 2)

dados <- read.csv("details2017.csv.gz")