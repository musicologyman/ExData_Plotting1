library(data.table)
library(magrittr)     # so we can use the pipe operator %>%

# set parameters
wd <- "~/Repos/exploratory-data-analysis/data"
datafile <- "household_power_consumption.txt"

setwd(wd)

# First, estimate the size of the data set
# Get the number of lines in the data set
wc <- try(system2("wc", c("-l", datafile), stdout=T))
# Use a regular expression to find the numeric values
matchResult <- regexpr("\\d+", wc)
# Extract the number of lines from the result and convert to a numeric value
numrows <- substr(wc, matchResult, matchResult + attr(matchResult, "match.length")) %>% 
  as.integer()

# Now, read the first few rows of the data file in order to extract colClasses
toprows <- read.table(datafile, nrows = 20, header = T, sep=";", stringsAsFactors = F)
# Now, use the first row to get the column classes
classes <- lapply(toprows[1, ], class)

# We can use the list of classes as a basis for the colClasses argument when 
# we read the table.
colClasses <- unlist(classes, use.names =  FALSE)

# Once we know the column classes, we can re-read the first row to get the object sizes of each column
rowsize <- read.table(datafile, header=T, sep=";", colClasses = colClasses, nrows = 20) %>% 
  (function(x) sapply(x[1,], object.size)) %>%
  sum

#Now we can calculate the approximate size of the data in memory:
datasize <- rowsize * numrows

# Get the amount of memory on the system (for Mac OS X)
get_available_memory = function() {
  pattern <- "(?<=Primary memory available:\\s).+(?=\\sgigabytes)"
  
  # available memory in bytes:
  try(system2("hostinfo", stdout=TRUE)) %>%            
    (function(x) x[grepl(pattern, x, perl = TRUE)]) %>%
    (function(x) list(input = x, match = regexpr(pattern, x, perl=TRUE))) %>%
    (function(x) substr(x$input, x$match, attr(x$match, "match.length") + x$match )) %>%
    (function (x) as.numeric(x) * (1024 ^ 3))
}
  
