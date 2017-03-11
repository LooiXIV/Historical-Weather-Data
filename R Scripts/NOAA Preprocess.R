###############################################################
# NOAA weather data Preprocessing #
###############################################################
# This code does some preprocessing on NOAA data to reformat
# data to be inputted to another piece of R code. The second
# piece of R code reformats the output of this data set 
# so that it can be read into Map Shed GWLF.
###############################################################
graphics.off();
rm(list=ls());
###############################################################
#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!#
#!            Data table header should be contain:           !#
#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!#
###############################################################
# This should be a csv file downloaded from NOAA. Input the 
# file's name or the number (usually NOAA acquired from NOAA
# will be sent through a link in an e-mail after specifying
# the data wanted. When downloaded the file is named with a 
# series of numbers. (392434.csv as an example).
###############################################################
# The input file should have the following column names (Also
# it doesn't necessarily matter the order):
#
# Figure 1.
# STATION   STATION_NAME   DATE     PRCP     TMAX     TMIN
# character character      numeric  numeric  numeric  numeric
#
# Below the column names I have posted the the data type 
# that each column should be when initially entered in. you
# need to have all these columns named as such in figure 1.
# If they are not formatted/entered in as in figure 1 then the
# code will not run.
# !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
# !!!!!!!!!!!!!!!!!!!CODE IS CASE SENSITIVE!!!!!!!!!!!!!!!!!!!!
# !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
###############################################################
#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!#
#!            Input the NOAA file to be processed            !#
#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!#
###############################################################
###############################################################
# Set the work directory (have these slashes: "/") #
###############################################################

Work.Dir = "C:/Users/Alex Looi/Desktop/Map Shed"

###############################################################
# Input file name/number below #
###############################################################

NOAA.File.Number = "551891"
###############################################################
#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!#
#!              DON'T EDIT CODE PASTE THIS LINE!             !#
#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!#
###############################################################
# set the work directory with the work directory you specified
# earlier.
setwd(Work.Dir)

# Find the particular file withing the work directory folder
# if the file isn't found the the code will not run. This is a
# safe guard to not unecessarily output a file.
NOAA.file = list.files(pattern = NOAA.File.Number)

# read in the actual weather data.
weather.data = read.table(NOAA.file, sep = ",", header = T)

#head(weather.data)

# find the number of rows in this read in data set.
tl = length(weather.data[,1]) 

# Create a new data table with the same number of rows and number of
# columns as the original NOAA data set. 
table.new = data.frame(rep(0,tl), rep(0,tl), rep(0,tl), 
                        rep(0,tl), rep(0,tl), rep(0,tl))

# Create the columns names that the main piece of R code can recognize. 
col.names = c("STATION","STATION_NAME", "DATE", "PRCP", "TMAX", "TMIN")

# Re-name the columns of the new data table so that they have the same
# ones as the NOAA data table, and more importantly the column names
# can now be read by the next piece of R code that turns the output of
# of this R code to a GWLF Map Shed input.
colnames(table.new) = col.names

# Transfer over the data that doesn't need to be reformatted
table.new$STATION = as.character(weather.data$STATION)
table.new$STATION_NAME = as.character(weather.data$STATION_NAME)
table.new$PRCP = weather.data$PRCP
table.new$TMAX = weather.data$TMAX
table.new$TMIN = weather.data$TMIN

# convert the dates from numeric to type character. So that it can 
# be reformatted using the function substr() to seperate Year, Month
# and Day, then converted to class type POSIXlt. 
table.new$DATE = as.character(weather.data$DATE)

# seperate out year, month, and day from the original NOAA
# .csv file.
year = substr(table.new$DATE, 1, 4)
month = substr(table.new$DATE, 5, 6)
day = substr(table.new$DATE, 7, 8)

# paste the year, month, and day together using dashes
new.date = paste(year,"-", month, "-", day, sep = "")

# Convert from class type character to class type POSIX
new.date = as.POSIXlt(new.date)

# Input the new date data to the table.
table.new$DATE = new.date

# Create a file name that relates to the station name.
output.name = paste(table.new$STATION_NAME[1],".csv", sep = "")

# Write out the new table with STATION_NAME" variable as the
# file name.
write.table(table.new, file = output.name, row.names = F, sep = ",")
