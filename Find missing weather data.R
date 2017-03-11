graphics.off()
rm(list = ls())

set.dir.main = paste("C:/Users/Alexander Looi/Google Drive/Dropbox/NOAA_Wetlands_Ceili-Alex/",
                     "Alex's Folder/Historical Weather Data", sep = "")

Start.year = 1900
End.year = 2014

data.range = 10

len.years = End.year - Start.year + 1 - (data.range - 1)

seq.year = seq(Start.year, End.year)

setwd(set.dir.main)

weather.file.name = paste("Watertown Airport Weather Data", ".csv", sep = "")

file.name = list.files(pattern = weather.file.name)

WD = read.table(file.name, header = T, sep = ",")

years = substr(WD$DATE, 1, 4)

months = substr(WD$DATE, 5, 6)

days = substr(WD$DATE, 7, 8)

all.ind.years = length(seq.year)

missing.data = data.frame(rep(0, len.years), rep(0, len.years), 
                          rep(0, len.years), rep(0, len.years),
                          rep(0, len.years), rep(0, len.years))

colnames(missing.data) = c("Time Period", "Missing Prcp", 
                           "Missing TMax", "Missing TMin",
                           "Mean Prcp (cm)", "Prcp SD")


se <- function(x) sqrt(var(x)/length(x))

# individual years analysis
all.years.data = data.frame(rep(0, all.ind.years), rep(0,all.ind.years),
                            rep(0, all.ind.years),rep(0, all.ind.years),
                            rep(0, all.ind.years),rep(0, all.ind.years))
colnames(all.years.data) = c("year", "M.Prcp", "M.TMax", 
                             "M.TMin", "Mean.Prcp",
                             "SD.Prcp")
for(y in 1:all.ind.years){
  
  year.vec = which(seq.year[y] == years)
  
  y.prcp = WD$PRCP[year.vec]
  y.TMin = WD$TMIN[year.vec]
  y.TMax = WD$TMAX[year.vec]
  
  y.prcp.na = which(y.prcp == -9999)
  y.TMax.na = which(y.TMax == -9999)
  y.TMin.na = which(y.TMin == -9999)
  
  y.TMax = y.TMax[-y.TMax.na]/10
  y.TMin = y.TMin[-y.TMin.na]/10
  
  if(length(y.prcp.na) != 0) y.prcp = y.prcp[-y.prcp.na]
  
  all.years.data[y,1] = seq.year[y]
  all.years.data[y,2] = length(y.prcp.na)
  all.years.data[y,3] = length(y.TMax.na)
  all.years.data[y,4] = length(y.TMin.na)
  all.years.data[y,5] = mean(y.prcp/100)
  all.years.data[y,6] = se(y.prcp/100)
  
}

vec.y.na = which(all.years.data[,2] != 0)
vec.n.na = which(all.years.data[,2] == 0)

prcp.y.na = all.years.data[vec.y.na,5]
prcp.n.na = all.years.data[vec.n.na,5]

prcp.x.y = seq.year[vec.y.na]
prcp.x.n = seq.year[vec.n.na]

#############################################################
png(filename = paste("1893-2014 yearly mean daily prcp.jpg", 
                     sep = ""),
    width = 1500, height = 900)
mag = 2.25
LW = 1.5
par(cex = mag, mar=c(4,4,0,0))
plot(seq.year, all.years.data[,5], type = "l", 
     ylab = "Mean Daily Prcp (cm)", 
     xlab = "year",
     ylim = c(0,0.4), lwd = LW,
     # main = "mean daily prcp for 15 year segments (1893-2014)",
     xaxt = "n")
axis(side = 1, at = seq(1900,2014, 4), labels = seq(1900,2014, 4), 
     las = 2)
points(prcp.x.n, prcp.n.na, col = 4, lwd = 3)
points(prcp.x.y, prcp.y.na, col = 2, lwd = 3)
lines(seq.year, all.years.data[,5]+all.years.data[,6], lwd = LW)
lines(seq.year, all.years.data[,5]-all.years.data[,6], lwd = LW)
abline(0.1525, 0, lwd = LW) # line marking the mean daily rainfall of 2000-2014
dev.off()
graphics.off()



# X # of year segments analysis
for(i in 1:len.years){
  
  years.obs = as.character(seq.year[i:(i+data.range-1)])
  
  y.vec = which(years %in% years.obs)
  
  time.p = paste(years.obs[1], "-", years.obs[length(years.obs)], sep = "")
  
  missing.data[i,1] = time.p
  
  data.prcp = WD$PRCP[y.vec]
  data.TMin = WD$TMIN[y.vec]
  data.TMax = WD$TMAX[y.vec]
  
  prcp.na = which(data.prcp == -9999)
  TMin.na = which(data.TMin == -9999)
  TMax.na = which(data.TMax == -9999)
  
  
  missing.data[i,2] = length(prcp.na)
  missing.data[i,3] = length(TMin.na)
  missing.data[i,4] = length(TMax.na)
  
  if(length(prcp.na) != 0)data.prcp = data.prcp[-prcp.na]

  ym.prcp = mean(data.prcp)
  ym.prcpsd = se(data.prcp/100)
  
  missing.data[i,5] = ym.prcp/100
  missing.data[i,6] = ym.prcpsd
  
}

years.len = length(seq.year)

year.data = data.frame(rep(0, years.len),rep(0, years.len),
                       rep(0, years.len))

colnames(year.data) = c("Year","missing.data", "avg.prcp")

for(y in 1:years.len){
  
  y.vec = which(as.character(seq.year)[y] == years)
  
  year.data[y,1] = seq.year[y]
  
  year.prcp = WD$PRCP[y.vec]
  
  prcp.miss = which(year.prcp == -9999)
  
  year.data[y,2] = length(prcp.miss)
  
  if(length(prcp.miss) != 0) {
    
    year.prcp = year.prcp[-prcp.miss]
  
  }
  
  year.data[y,3] = mean(year.prcp/100)
  
}

data.box = boxplot(missing.data[,5], main = "Quartiles of mean daily Prcp",
                   ylim = c(0.13, 0.17), 
                   ylab = "mean daily prcp (cm)")

for(l in 1:length(data.box$stats)){
  #abline(data.box$stats[l], 0, col = l)
  text(round(data.box$stats[l], digits = 4), x = 0.9, y = data.box$stats[l]+0.001)
}

# plot yearly data (15 year segments)
x.vals = c(1:length(missing.data[,5]))

p.na.vec = which(missing.data[,2] != 0)

p.da.vec = which(missing.data[,2] == 0)

na.p = missing.data[p.na.vec, 5]

da.p = missing.data[p.da.vec, 5]

# Boxplots mean daily prcp 15 year segments

png(filename = paste("Boxplots 1893-2014 mean daily prcp 15 year segments and annual mean.jpg", 
                     sep = ""),
    width = 1600, height = 1000)
layout(matrix(c(1,2), 1,2), height = c(1,1))
par(cex = 2, mar = c(4,4,1,0))
boxplot(list(da.p, na.p, missing.data[,5]), #lwd = 2,
        ylab = "daily mean rainfall (cm)", 
        names = c("No Missing Data", "Only Missing Data", "All Prcp Data"),
        # main = "Quartiles of mean daily prcp (15 year segments)",
        xlab = "*note: line marks mean daily prcp 2000-2014",
        ylim = c(0.,0.3))
abline(0.1525, 0) # line marking the mean daily rainfall of 2000-2014

par(cex = 2, mar = c(4,2,1,1))
# boxplots mean daily prcp
boxplot(list(prcp.n.na, prcp.y.na, all.years.data[,5]),
        # main = "Quartiles of mean daily Prcp (Yearly 1893-2014)",
        ylab = "", 
        names = c("No Missing Data", "Only Missing Data", "All Prcp Data"),
        xlab = "", yaxt = "n",
        ylim = c(0.,0.3))
abline(0.1525, 0) # line marking the mean daily rainfall of 2000-2014
dev.off()
graphics.off()
# 1893 to 2014 mean daily prcp for 15 year segments

png(filename = paste("prcp 15 year.png", sep = ""),
    width = 1500, height = 1500)

layout(matrix(c(1:3),3, 1), height = c(3, 3, 1))
par(cex = 2.5, mar = c(4, 4, 1, 1))
LW = 1.75

plot(x.vals, missing.data[,5], type = "l", 
     ylab = "Mean Daily Prcp (cm)", 
     xlab = "year segments (shows starting year)",
     ylim = c(0.125,0.175), lwd = LW,
     # main = "mean daily prcp for 15 year segments (1893-2014)",
     xaxt = "n")

start.years = as.numeric(substr(missing.data[,1],1, 4))

axis(side = 1, at = seq(x.vals[1],x.vals[length(x.vals)], 4), 
     labels = seq(start.years[1],start.years[length(start.years)], 4), 
     las = 2)
abline(0.1525, 0, lwd = LW) # line marking the mean daily rainfall of 2000-2014
# a time period already modelled
# apply color coded points. Green no missing prcp data
# red missing prcp data

points(p.na.vec, na.p, col = 2, lwd = 3)
points(p.da.vec, da.p, col = 4, lwd = 3)

# Apply SE as lines instead of bars
lines(x.vals, missing.data[,5]-missing.data[,6], lwd = LW) 
lines(x.vals, missing.data[,5]+missing.data[,6], lwd = LW)

#1893-2014 yearly mean daily prcp.jpg
plot(seq.year, all.years.data[,5], type = "l", 
     ylab = "Mean Daily Prcp (cm)", 
     xlab = "year",
     ylim = c(0,0.4), lwd = LW,
     # main = "mean daily prcp for 15 year segments (1893-2014)",
     xaxt = "n")
axis(side = 1, at = seq(1900,2014, 4), labels = seq(1900,2014, 4), 
     las = 2)
points(prcp.x.n, prcp.n.na, col = 4, lwd = 3)
points(prcp.x.y, prcp.y.na, col = 2, lwd = 3)
lines(seq.year, all.years.data[,5]+all.years.data[,6], lwd = LW)
lines(seq.year, all.years.data[,5]-all.years.data[,6], lwd = LW)
abline(0.1525, 0, lwd = LW) # line marking the mean daily rainfall of 2000-2014

plot.new()
par(cex = 2.5, mar = c(0,0,0,0))
legend("center", c("missing prcp", "no missing prcp"), 
       col = c(2,4), pch = c(1,1), lwd = 2, bty = "n",
       y.intersp = 3)

dev.off()
graphics.off()

write.table(missing.data, 
            file = paste("Missing Data ",data.range," years",".csv", sep=""), 
            quote = F, sep = ",", row.names = F)
