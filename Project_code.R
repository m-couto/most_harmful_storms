# Course Project 2

setwd("~/Desktop/Data Science course/5. Reproducible Research/Project 2")   # my local directory

# the data
download.file('https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2',
              './Storm Data.csv.bz2')


library(data.table)     # for faster reading

storm_data <- fread('Storm Data.csv.bz2')
storm_data <- as.data.frame(storm_data)

# system.time({
# data <- fread('Storm Data.csv.bz2')
# as.data.frame(data)
# })

# returned
# user  system elapsed 
# 19.824   1.738  24.222 


# system.time(read.csv('Storm Data.csv.bz2'))

# returned
# user  system elapsed 
# 44.215   0.434  44.732 


# I also unzipped doc by hand

# helpful info about the data
download.file('https://d396qusza40orc.cloudfront.net/repdata%2Fpeer2_doc%2Fpd01016005curr.pdf',
              './documentation.pdf')

