###########################################################################
# Weather Data Formatting Conversion Code: NOAA default to GWLF input
###########################################################################
# This R code takes standard NOAA weather formatted data and converts it
# to a format that the GIS GWLF plugin can read and use. More specifically
#
# SECTION 1:
#
# Turn this .csv format:
# STATION  STATION_NAME  DATE  PRCP  TMAX  TMIN
#__________________________________________________________________________
# The above format have two station name columns which are "char" variables
# the DATE column is a single column where each row represents a single day.
# The default of the data is type "char", however, when R reads in the data
# initially R interprets the data as type "factor". These data need to be
# converted to either type "POSIXlt" or type "char". The PRCP, TMAX, and 
# TMIN columns are precipitation, max. temperature, and min. temperature
# with each row representing a day of data. These data are in metric units
# and need to converted to imperial units. 
#__________________________________________________________________________
#
# SECTION 2:
#
# This code will convert the above format...
# To this:
# (No header)
# 9000  TMax  1990  JAN (Temperatures-->, no data = -999999)
# 9000  TMax  1990  FEB (Temperatures-->, no data = -999999)
# ..........................................................
# 9000  TMin  1990  JAN (Temperatures-->, no data = -999999)
#__________________________________________________________________________
# GWLF weather input is very different from the NOAA weather default format.
# Instead of each row representing a day of data. Each row in GWLF represents
# an entire month of data. The first 4 columns tell GWLF about what kind of 
# data is being read in. The weather station number, the type of data, The
# Year the data was collected in, and finally the month. 
# 
# The first column is the weather station number. This is is the weather 
# station that is closest to your watershed. The second column denotes the
# variable type. This column tells GWLF what kind of data or "data type" 
# the model is reading in. This can be either max/min temperature or 
# precipitation. The way the data needs to be formatted is to have all the 
# daily max temperature for all years first, then min. temperture next, and 
# daily precipitation last. It needs to be in this order or GWLF WILL crash. 
# Next to this column is the year column telling GWLF the year the data is 
# from. The years must be ascending, starting from earliest to the latest. 
# Full data set from each "data type" for all years must appear before the
# other "data types" can appear in the file. The next column denotes the 
# Month. This column shows the current month as the first three letters in
# all caps. Each row represents all the days of data form a specific data 
# type in a particular month and year. For example, in the above example,
# an entire row gives us the weather station number (9000), data type 
# (TMax), Year of the data (1990), the month (JAN), followed by the entire
# corresponding data set for the month. Columns beyond the "Month column" 
# in this case represent a day within the denoted month. In this format a 
# month has anywhere from 28 to 31 days of data. However, 31 total cells of 
# data need to be filled. Therefore any months with less than 31 days will 
# have extra "no data" fill with values of -999999 to bring the number of 
# cells to 31. Example: the month of November with 30 days in it will have 
# an extra cell at the end with the value -999999, A non-leap year February 
# will have an extra 3 cells with the values -999999 added to the end, and 
# January will have no extra cells.
#
# Lastly, this code will create a seperate folder to contain your newly 
# created files within your current/declared work directory.
#
# See Sample Document "Sta9999.csv" for an example of how data should be
# formatted.
#
# When looking over data make sure you check the formating in both excell
# and in notepad. 
#__________________________________________________________________________
# This code should have two outputs:
# 1) The correctly formatted GWLF data input. The data should be formatted 
# according to what was written in section 2.
#
# 2) A .txt files that shows all of the missing data dates, and values used
# to fill in missing days.
#
###########################################################################
graphics.off();
rm(list=ls())
###########################################################################
# Read in the data file to be fixed
###########################################################################
# Set work directory
dir.main = paste("/Users/Looi/Desktop/Back Up 8.30.2015/",
                 "Research GWLF and Bath Tub Model/Bath Tub Model/",
                 "Historical Weather Data", sep = "")

#setwd("C:/Users/LooiXIV/Desktop/Research GWLF and Bath Tub Model/Map Shed")
setwd(dir.main)
# get a list of all files and folders in the work directory
###########################################################################
# Determine what the files in the 
list.files()

# Read in the weather data
###########################################################################
# Name of Weather file to be edited entered here:

year.date.start = 1932
year.data.end = 1941

Weather_data_Name = paste("Watertown Weather ", 
                          year.date.start,"-", year.data.end, 
                          ".csv", sep = "")

Weather_data_Namefile = paste("Watertown Weather ", 
                              year.date.start,"-", year.data.end,
                              sep = "")
##########################################################################
#               #!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!#                 #
#               #! DO NOT EDIT CODE BEYOND THIS POINT !#                 #
#               #!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!#                 #
##########################################################################

W.data = read.table(Weather_data_Name, sep = ",", header = T)
D.length = length(W.data[,1])
# Set up a new date data frame
Dates = data.frame(rep(0, D.length), rep(0, D.length), rep(0, D.length))
colnames(Dates) = c("Day", "Month", "Year")
Date.old = as.character(W.data$DATE)
##########################################################################
# Convert the data to the proper units
# NOAA data temperature data is in tens of Celsius, convert to Fahrenheit
# Prcp is in tens of millimeters, convert to inches
##########################################################################
# Find which data points are not -9999
is.na.PRCP = which(W.data$PRCP <= -9999)
is.na.TMAX = which(W.data$TMAX <= -9999)
is.na.TMIN = which(W.data$TMIN <= -9999)
# Find data that is "na" or -9999
not.na.PRCP = which(W.data$PRCP >= -9999)
not.na.TMAX = which(W.data$TMAX >= -9999)
not.na.TMIN = which(W.data$TMIN >= -9999)

# Convert non-na values to correct units
W.data$PRCP[not.na.PRCP] = round(W.data$PRCP/254, digits = 2)
W.data$TMAX[not.na.TMAX] = round((W.data$TMAX/10)*(9/5)+32, digits = 0)
W.data$TMIN[not.na.TMIN] = round((W.data$TMIN/10)*(9/5)+32, digits = 0)

# Turn na values from -9999 to na's
W.data$PRCP[is.na.PRCP] = -999999
W.data$TMAX[is.na.TMAX] = -999999
W.data$TMIN[is.na.TMIN] = -999999

# Create seperate vectors for TMAX TMIN and PRCP to find NA cells and replace
# the cells with averages of the sandwiching days.
TMAX = W.data$TMAX
TMIN = W.data$TMIN
PRCP = W.data$PRCP

# NOTE: this portion of code will not run if the first value or the 
# very last value of the data set is an NA
# Create a vector of the GWLF variable names
var.name = c("TMAX", "TMIN", "PRCP")

for (var in 1:3){
  
  cur.var = paste("is.na.", var.name[var], sep = "")
  # Start looking for NA's at the beginning of the is.na.XXXX vector
  # Check to see if there are any NA's to evaluate. If there are none
  # then the following code will not need to run.
  
  if(length(get(cur.var)) != 0){
    
    # get the length of the NAs vector
    na.l = length(get(cur.var))
    
    # Find the beginning of the is.na.XXXX vector
    is.na.b = get(cur.var)[1]
    
    # Find the end of the is.na.XXXX vector 
    is.na.l = get(cur.var)[na.l]
    
    # Add on an additional cell to the vector.
    vec.eval = c(is.na.b-1, 
                 get(cur.var), 
                 -999999)
    
    # Find the beginning of the vector of continuous NA's and find a 
    # Also create a variable that finds the eventual end of the a
    # continuous set of adjacent NA cells
    start = get(cur.var)[1]
    end = get(cur.var)[1]
    
    # create a counter which counts the number of cells that are
    # "adjacent".
    ct = 0
    
    # Do this "for" look through all the NA cells to find which are adjacent
    for(l in 1:na.l){
      
      # look at the vector of NA's for a particular Variable and see if there are any
      # values that need to be replaced. If two adjacent vector cells in vec.eval are
      # sequential then the code had found the data that is sequential. Now the code
      # sets into these missing data values and estimates values for them. 
      if(vec.eval[l+1] != (vec.eval[l+2]-1)){
        
        # Find the end vector of missing data
        end = start + ct
        
        # Estimate all the missing data by averaging the the two days adjoining the
        # missing data vector. If the data is precipitation data then assume no
        # precipitation
        est.temp = round((get(var.name[var])[start-1]+get(var.name[var])[end+1])/2, 
                         digits = 0)
        
        # Assign it a unique variable for that specific data type
        paste.a = paste(var.name[1],".na.rep", sep = "")
        assign(paste.a, est.temp)
        
        # Determine if the missing data is part of either TMAX, TMIN, or PRCP
        # then add it to that data set. Where if var is 1 it's TMAX data, 2 TMIN 
        # data, or 3 PRCP data.
        if(var == 1){
          TMAX[start:end] = get(paste.a)
        } else if (var == 2){
          TMIN[start:end] = get(paste.a)
        } else {
          PRCP[start:end] = get(paste.a)
        }
        
        # reset the starting value of the NA vector to the next cell 
        # Also reset the vector. 
        start = get(cur.var)[l+1]
        ct = 0
      } else {
        # if the next cell is an NA value/part of a sequence then count up
        # the counter and continue the loop. 
        ct = ct + 1 
      }
    }
  }
}

# update the table values with the re-evaluated values.
W.data$TMAX = TMAX
W.data$TMIN = TMIN
W.data$PRCP = PRCP

# New Dates
Date.old = as.character(W.data$DATE)
Date.old = as.POSIXlt(as.Date(Date.old, format = "%m/%d/%Y"), tz = "EDT")
Dates$Year = as.numeric(format(Date.old, "%Y"))
Dates$Month = as.numeric(format(Date.old, "%m"))
Dates$Day = as.numeric(format(Date.old, "%d"))

# Create the new data frame
######################################################
# calculate the total number of years
num.years = Dates$Year[D.length] - Dates$Year[1] + 1

# number of rows each data type filles (TMax, TMin, Prcp)
S.month = Dates$Month[1]
S.year = Dates$Year[1]
E.year = Dates$Year[length(Dates$Month)]
E.month = Dates$Month[length(Dates$Month)]
num.row = ((num.years-1) * 12) + E.month

# need enough rows for TMax, TMin, and Prcp
row.tot = num.row * 3
num.col = 31 + 4 # number of days + other columns
Data.new = matrix(-999999, ncol = num.col, nrow = row.tot)

# Fill the data
# Create new data type designations for the reformatted data table. 
dataName = c("TMax", "TMin", "Prcp")

# For the GWLF Mapshed format all years of TMax are first, then TMin, and
# last PRCP 
Data.new[1:num.row, 2] = "TMax"
Data.new[(num.row+1):(num.row*2), 2] = "TMin"
Data.new[(num.row*2+1):(num.row*3), 2] = "Prcp"


Year.label = rep(0, num.row)
Month.num = rep(0, num.row)

Month.label = rep(0, num.row)
Month.name = c("JAN", "FEB", "MAR", "APR", "MAY", "JUN", 
               "JUL", "AUG", "SEP", "OCT", "NOV", "DEC")

# Create the year and month labels
# The code only needs to create enough year and month labels
# as there are number of years (obvious for the years a little
# more tricky for the month labels). This recreates the 
# Month.name vector the number of years. 
x = 1
year = S.year
for(y in 1:num.row){  
  
  if(x == 13){ x = 1; year = year + 1}
  Month.num[y] = x
  Month.label[y] = Month.name[x]
  Year.label[y] = year 
  x = x + 1
  
}

x = 1

# number of categories to fill TMax, TMin, and Prcp
for(i in 0:2){

   Data.new[(1+num.row*i):((i+1)*num.row), 2] = dataName[i+1]
   Data.new[(1+num.row*i):((i+1)*num.row), 3] = Year.label
   Data.new[(1+num.row*i):((i+1)*num.row), 4] = Month.label
}

prcp.w = which(Data.new[,2] == "Prcp")
TMax.w = which(Data.new[,2] == "TMax")
TMin.w = which(Data.new[,2] == "TMin")

prcp.s = prcp.w[1]
TMax.s = TMax.w[1]
TMin.s = TMin.w[1]


t = 2008
m = 7

for (t in S.year:E.year){
  
  y.w = which(Dates$Year == t)
  
  # Do the months
  for(m in 1:12){
    
    m.w = which(Dates$Month[y.w] == m)
    
    if(length(m.w) != 0){
      
      pos.v = y.w[m.w]

      M.N = Month.name[m]
    
      # Precipitation data
      prcp.v = W.data$PRCP[pos.v]
      Data.new[prcp.s, 5:(length(prcp.v)+4)] = prcp.v
      which(prcp.v < -1000)
      prcp.s = prcp.s + 1
     
      # TMax Data
      TMax.v = W.data$TMAX[pos.v]
      Data.new[TMax.s, 5:(length(TMax.v)+4)] = TMax.v
      which(TMax.v < -1000)
      
      TMax.s = TMax.s + 1
   
    
      # TMin Data
      TMin.v = W.data$TMIN[pos.v]
      Data.new[TMin.s, 5:(length(TMin.v)+4)] = TMin.v
      TMin.s = TMin.s + 1
    } else {
      TMin.s = TMin.s + 1
      TMax.s = TMax.s + 1
      prcp.s = prcp.s + 1
    }
  }
}

# Print out the data to a .csv file
#############################################
# Print out all the data to a .csv files with all the station names

# Get all the file names from the weather directory
#W.files = list.files("C:/Users/Alex Looi/Desktop/Map Shed/TIBS Watersheds/Weather Data/Weather")

sta.nums = seq(1000, 1011, by = 1)

#write.table(Data.new, file = "TEST.csv", col.names = F, row.names = F, sep = ",")

# Grab the station numbers from the list of weather data
# grabs everything between "sta" and ".csv" in sta####.csv, or just the station number
#sta.nums = sub(".*?sta(.*?).csv.*", "\\1", W.files)

# set a new work directory to a TIBS specific directory for the new weather files
#setwd("C:/Users/Alex Looi/Desktop/Map Shed/TIBS Watersheds/Weather Data/Weather TIBS")

# number of stations
num.sta = length(sta.nums)

# Create a seperate folder to contain the newly created GWLF weather files
dir.create(Weather_data_Namefile)

for (s in 1:num.sta){

  new.file = paste("sta", sta.nums[s], ".csv", sep = "")
  
  Data.new[,1] = sta.nums[s]

  write.table(Data.new, file = new.file, col.names = F, 
              row.names = F, sep = ",", quote = F)

}