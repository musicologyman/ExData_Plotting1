library(data.table)
library(magrittr)     # so we can use the pipe operator %>%

# set parameters
wd <- "~/Repos/exploratory-data-analysis/data"
datafile <- "household_power_consumption.txt"
min_rows_to_browse <- 20
max_fraction_of_allowed_memory <- 0.5

setwd(wd)

# Get the number of rows in an input file (for Mac OS X and other Unix-based systems)
get_num_rows <- function(input_file) {
  try(system2("wc", c ("-l", input_file), stdout=T)) %>% 
    (function(x) list(input = x, match = regexpr("\\d+",x))) %>% 
    (function(x) substr(x$input, x$match, attr(x$match, "match.length") + x$match )) %>%
    as.integer()
}

num_rows <- get_num_rows(datafile)

if (num_rows < min_rows_to_browse) 
  stop(paste("The dataset does not include at least ", min_rows_to_browse, "rows."))

# Now, read the first few rows of the data file in order to extract colClasses
toprows <- read.table(datafile, nrows = min_rows_to_browse, header = T, sep=";", stringsAsFactors = F)
# Now, use the first row to get the column classes
classes <- lapply(toprows[1, ], class)

# We can use the list of classes as a basis for the colClasses argument when 
# we read the table.
colClasses <- unlist(classes, use.names =  FALSE)

# Once we know the column classes, we can re-read the first row to get the object sizes of each column
rowsize <- read.table(datafile, header=T, sep=";", colClasses = colClasses, nrows = min_rows_to_browse) %>% 
  (function(x) sapply(x[1,], object.size)) %>%
  sum

#Now we can calculate the approximate size of the data in memory:
datasize <- rowsize * num_rows

# Get the amount of memory on the system (for Mac OS X)
get_available_memory = function() {
  pattern <- "(?<=Primary memory available:\\s).+(?=\\sgigabytes)"
  
  # available memory in bytes:
  try(system2("hostinfo", stdout=TRUE)) %>%            
    (function(x) x[grepl(pattern, x, perl = TRUE)]) %>%
    (function(x) list(input = x, match = regexpr(pattern, x, perl=TRUE))) %>%
    (function(x) substr(x$input, x$match, attr(x$match, "match.length") + x$match )) %>%
    (function (x) as.integer(x) * (1024 ^ 3))
}

if ( datasize / get_available_memory() > max_fraction_of_allowed_memory )
  stop("Not enough memory to complete the operation")

# Since we're going to subset the dataset on the dates
# 2007-02-01 and 2007-02-02, let's first get a vector
# we can use to limit the number of rows we actually read.
get_date_filter <- function(){
  filter_dates <- c(as.Date("2007-02-01"), as.Date("2007-02-02"))
  
  read.table(datafile, header=TRUE, sep=";",
             colClasses=c("character", rep(NA, 8))) %>% 
    (function (x) as.Date(x$Date, "%d/%m/%Y")) %>%
    (function (x) x %in% filter_dates)
  }

# Now use the date filter to subset the entire data set
date_filter <- get_date_filter()

# Read the data table using fread. Setting column classes to "character" to
# prevent warning about rows in which a character is found in where its
# column class had been inferred to be something else.
dt <- fread(datafile, data.table = TRUE, colClasses = rep("character", 9))

# Filter by the dates of interest
# convert the first column to a Date
dt$Date <- as.Date(dt$Date, "%d/%m/%Y")
# set the filter dates
filter_dates <- c(as.Date("2007-02-01"), as.Date("2007-02-02"))
dt <- dt[dt$Date %in% filter_dates]
for (k in 3:9) {
  dt[[k]] <- as.numeric(dt[[k]])
}
dt[[2]] <- paste(dt[[1]] %>% as.character, dt[[2]]) %>% (function(s) strptime(s, "%Y-%m-%d %H:%M:%S"))

# Now, plot away!
# with(dt, plot(x = Time, y = Sub_metering_1, type = "l", 
#               col = "gray", 
#               xlab="", 
#               ylab="Energy sub metering"))
# lines(x = dt$Time, y = dt$Sub_metering_2, col="red")
# lines(x = dt$Time, y = dt$Sub_metering_3, col = "blue")
# legend(legend = c("Sub_metering_1", "Sub_metering_2", "Sub_Metering_3"), x = "topright", lty=1, col=c("gray", "red", "blue"))

par(mfrow=c(2,2), mar=c(1,1,1,1), pty="m", oma=c(1,1,1,1))
with(dt, { 
          par(mar=rep(4,4))
          plot(x = Time, 
                y = Global_active_power,
              type = "l",
              ylab="Global Active Power")
          plot(x = Time, y = Voltage, 
              type="l", ylab= "Voltage", xlab = "datetime")
          plot(x = Time, y = Sub_metering_1, type = "l", 
              col = "gray", 
              xlab="", 
              ylab="Energy sub metering")
          lines(x = dt$Time, y = dt$Sub_metering_2, col="red")
          lines(x = dt$Time, y = dt$Sub_metering_3, col = "blue")
          legend(legend = c("Sub_metering_1", "Sub_metering_2", "Sub_Metering_3"), x = "topright", lty=1, col=c("gray", "red", "blue"))
          plot(x = Time, y = Global_reactive_power, type="l",
               xlab = "datetime", ylab = "Global_reactive_power")
})

dev.copy(png, "../plot4.png")
dev.off()