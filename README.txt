Historical-Weather-Data
Processing weather data for GWLF and the Wetland Model
####################################################################

Within the main folder 'Historical-Weather-Data' there are three "Missing Data" files: 1) 'Missing Data 10 year ranges.csv', 
2) 'Missing Data Color Coded.xls', and 3) 'Missing Data.csv'. 

The last file is the 'WATERTOWN REGIONAL AIRPORT SD US.csv'. This is raw weather data taken from NOAA. It contains daily precipitation
data (in 10 x mm per day, i.e. a measurement of 100 is 10.0 mm), and daily max and min temperatures (measurements are in 10's of degrees 
C i.e. -100 is -10.0 C). This data was was used in modeling of wetland watersheds with GWLF, and in modeling the input of precipitation
to the wetlands themselves.

Additionally, there are two main folders within the main folder: 'R Scripts', and 'Figures Produced'.

In 'R Scripts' there are four different R scripts.
1) NOAA Preprocess.R: this R script pre-processes the Raw NOAA data downloaded from the NOAA data repository. Dates, are 
reformatted to be readable by R, excel, and GWLF. For specifics look in the 'NOAA Prepocess.R' file itself.

2) weather data conversion.R: this R script takes raw NOAA data, in the format of as downloaded from NOAA, and converts it to a 
format that can be understood by the MapShed GWLF model developed by Penn State (http://www.mapshed.psu.edu/, FYI this website 
appears to have lost their domain). For more specifics look in the 'weather data conversion.R' file itself.

3) Finding missing weather data.R: This R script finds missing daily precipitation, max temperature, and min temperature data 
in weather data obtained from NOAA. Mean, standard deviation, and longest run of number missing days for precipitation, 
min temp., and max temp, are calculated for user specified range of years. Additionally, this script outputs several figures 
summarizing the behavior of missing precipitation data within the downloaded NOAA data.


4) Finding missing data.R: This Script makes a table of missing daily precipitation, max temperature, and min temperature data 
in weather data obtained from NOAA.
