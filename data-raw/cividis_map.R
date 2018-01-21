cividis.map <- read.csv("data-raw/cividis.txt", sep = ";")

cividis.map$opt <- "V"

devtools::use_data(cividis.map, overwrite = TRUE)
