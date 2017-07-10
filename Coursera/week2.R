#load packages
library(pacman)
p_load(readxl, readr, dplyr, janitor, tidyr, stringr, ggplot2, utils, boot)

setwd("C:/Users/jules.liebster/Desktop/TNTP/NYCTF/R/Coursera")

specdata <- "C:/Users/jules.liebster/Desktop/TNTP/NYCTF/R/Coursera/specdata"

unzip("C:/Users/jules.liebster/Desktop/TNTP/NYCTF/R/Coursera/specdata", exdir = "specdata")

pollutantmean <- function(directory, pollutant, id = 1:332) {
    data = lapply(id, function(i) read.csv(paste(directory, "/", formatC(i, 
            width = 3, flag = "0"), ".csv", sep = ""))[[pollutant]])
    
    return(mean(unlist(data), na.rm = TRUE))
}

pollutantmean("specdata", "sulfate", 1:10)
pollutantmean("specdata", "sulfate", 34)
pollutantmean("specdata", "nitrate", 70:72)
pollutantmean("specdata", "nitrate")


# Write a function that reads a directory full of files and 
# reports the number of completely observed cases in each data file. 
# The function should return a data frame where the first column is the name of the file 
# and the second column is the number of complete cases. 
# need to use complete.cases and list.files with full.name = TRUE

complete <- function(directory, id = 1:332) {
    x = list.files(directory)
    y = x[match(id, as.numeric(sub(".csv","",x)))]
    z = file.path(directory, y)
    a = function(z) sum(complete.cases(read.csv(z)))
    data.frame(id = id, nobs = unlist(lapply(z,a)))
}

complete("specdata",1:332)

cc <- complete("specdata", c(6, 10, 20, 34, 100, 200, 310))
print(cc$nobs)

cc <- complete("specdata", 54)
print(cc$nobs)

set.seed(42)
cc <- complete("specdata", 332:1)
use <- sample(332, 10)
print(cc[use, "nobs"])

cr <- filter(!is.na("specdata"))
cr <- sort(cr)                
set.seed(868)                
out <- round(cr[sample(length(cr), 5)], 4)
print(out)

cr <- corr("specdata", 129)                
cr <- sort(cr)                
n <- length(cr)                
set.seed(197)                
out <- c(n, round(cr[sample(n, 5)], 4))
print(out)
