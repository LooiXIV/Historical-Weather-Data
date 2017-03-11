graphics.off();
rm(list=ls())

# find the correct file with the historic data
setwd("C:/Users/Alex Looi/Desktop/Research GWLF and Bath Tub Model/Map Shed");

# list all the files and folders in the work directory
list.files()

# Type in the name of the historical weather data file
hist.W.name = "WATERTOWN REGIONAL AIRPORT SD US.csv"

# read in the historical weather data
hist.w = read.table(hist.W.name, sep = ",", header = T)

# Get month, day, and year vectors from the data
years = substr(hist.w$DATE, 1, 4)
months = substr(hist.w$DATE, 6, 7)
days = substr(hist.w$DATE, 9, 10)

# Create a character string of just the dates from the data
DATES = paste(years, "-", months, "-", days, sep = "")

# Select the years to be evaluated
s.y = 1900
e.y = 2014

# Select the time interval 
interval = 10

# calculate the total number of possible sequential "n" year 
# intervals there are
loop.len = (e.y-s.y-interval)

# Create a table with the number of loop lengths
num.nas = data.frame(rep(0, loop.len), 
                     rep(0, loop.len), rep(0, loop.len),
                     rep(0, loop.len), rep(0, loop.len),
                     rep(0, loop.len), rep(0, loop.len))

colnames(num.nas) = c("Years", 
                      "Tmin NA", "Tmin Seq", 
                      "Tmax NA", "Tmax Seq", 
                      "Prcp NA", "Prcp Seq")

i.year = s.y

for (years in 0:loop.len){
  
  f.year = i.year+interval
  
  # Create the start and end dates
  i.year.name = as.Date(paste(i.year, "-01-01", sep = ""))
  f.year.name = as.Date(paste(f.year, "-12-31", sep = ""))
  
  s.vec = which(i.year.name == DATES)
  f.vec = which(f.year.name == DATES)
  
  sec.data = data.frame(DATES[s.vec:f.vec], 
                        hist.w$PRCP[s.vec:f.vec],
                        hist.w$TMAX[s.vec:f.vec],
                        hist.w$TMIN[s.vec:f.vec])
  
  colnames(sec.data) = c("DATE", "PRCP", "TMAX", "TMIN")
  
  vec.prcp = which(sec.data$PRCP == -9999)
  if (length(vec.prcp) != 0) {
    count = 1
    prcp.cm = 1
    for(n in 1:(length(vec.prcp)-1)){
    
      vec.log = (vec.prcp[n] + 1) == vec.prcp[n+1] 
    
      if(vec.log == TRUE){
      
        count = count + 1
      
      } else if (vec.log == FALSE) {
      
        if(prcp.cm < count) prcp.cm = count
      
        count = 1
      }
    }
  }
  
  tot.prcp = length(vec.prcp)
  
  vec.tmax = which(sec.data$TMAX == -9999)
  
  if (length(vec.tmax) != 0) {
    count = 1
    tmax.cm = 1
    for(n in 1:(length(vec.tmax)-1)){
    
      vec.log = (vec.tmax[n] + 1) == vec.tmax[n+1] 
    
      if(vec.log == TRUE){
      
        count = count + 1
      
      } else if (vec.log == FALSE) {
      
        if(tmax.cm < count) tmax.cm = count
      
        count = 1
      }
    }
  }
  
  tot.tmax = length(vec.tmax)
  
  vec.tmin = which(sec.data$TMIN == -9999)
  
  if (length(vec.tmin) != 0) {
    count = 1
    tmin.cm = 1
    for(n in 1:(length(vec.tmin)-1)){
    
      vec.log = (vec.tmin[n] + 1) == vec.tmin[n+1] 
    
      if(vec.log == TRUE){
      
        count = count + 1
      
      } else if (vec.log == FALSE) {
      
        if(tmin.cm < count) tmin.cm = count
      
        count = 1
      
      }
    }
  }
  
  tot.tmin = length(vec.tmin)
  
  year.int = c(i.year, f.year)
  
  i.year = i.year + 1
  
  num.nas[years,1] = paste(i.year,"-", f.year)
  num.nas[years,2] = tot.tmin
  num.nas[years,3] = tmin.cm
  num.nas[years,4] = tot.tmax
  num.nas[years,5] = tmax.cm
  num.nas[years,6] = tot.prcp
  num.nas[years,7] = prcp.cm
}

write.table(num.nas, 
            file = "Missing Data over 15 year seqeuence.csv", 
            sep = ",", row.names = FALSE)
