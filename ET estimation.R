# Site has 6 soil moisture sensors split among two substations; sensors 1:3 are North (N) and sensors 4:6 are South (S).

# Packages ----
library(readxl)
library(writexl)

# Data - Soil moisture data needs to be in a folder named "Soil moisture data" in the working directory of the code----
StationID=16 # Station ID number: needs to be inputted manually
data.path=file.path('Soil moisture data', paste0('Station ', StationID, '.xlsx'))
data=read_xlsx(data.path)
n=length(data$Date) # Number of days

# Plot daily soil moisture data - Verify that volumetric water content does not exceed field capacity (Max ~50%).
plot(data$Date, data$s1, type='l', xlab='', ylab='Volumetric water content (%)', ylim=c(0, 50), col='olivedrab', lwd=2)
lines(data$Date, data$s2, col='goldenrod', lwd=2)
lines(data$Date, data$s3, col='salmon', lwd=2)
legend('bottom', legend=c('Port 1', 'Port 2', 'Port 3'), inset=0.01, col=c('olivedrab', 'goldenrod', 'salmon'), lty=1, lwd=2, horiz=T, box.lty=0)

plot(data$Date, data$s4, type='l', xlab='', ylab='Volumetric water content (%)', ylim=c(0, 50), col='olivedrab', lwd=2)
lines(data$Date, data$s5, col='goldenrod', lwd=2)
lines(data$Date, data$s6, col='salmon', lwd=2)
legend('bottom', legend=c('Port 4', 'Port 5', 'Port 6'), inset=0.01, col=c('olivedrab', 'goldenrod', 'salmon'), lty=1, lwd=2, horiz=T, box.lty=0)

# Assign alpha value - Value was obtained while calibrating the model
alpha=0.42780695937849

# Calculate total soil moisture in the profile (mm) - Total soil moisture is calculated by multiplying the water content (%) by the depth of soil. ----

# Soil moisture sensor depths (inches) - Other than the station ID this is the only manual input into the code. If more than 3 sensors are used it needs to be modified.
d1=6   # 1st sensor is at 6" 
d2=18  # 2nd sensor is at 18"
d3=30  # 3rd sensor is at 30"

# Soil layer thickness (mm) represented by the sensors (1"=25.4mm) - Assuming that the sensor is representative of the soil half way to the next sensor. If soil horizon measurements are available this can be refined.
z1=(d1+d2)/2*25.4    # 1st sensor: [0, 1]ft
z2=(d3-d1)/2*25.4    # 2nd sensor: [1, 2]ft
z3=(d3-d2)*25.4      # 3rd sensor: [2, 3]ft

# Total water depth (mm) = Water content (%) * Soil depth (mm) - Verify that the water depth is smaller (Max ~50%) than the total depth soil depth represented by the sensors.
data$SM.N=c(NA) # create total SM column for the N sub-station
for (i in 1:n) {data$SM.N[i]=c(z1*data$s1[i]+
                               z2*data$s2[i]+ 
                               z3*data$s3[i])/100}
plot(data$Date, data$SM.N, type='l', xlab='', ylab='Water depth (mm)', col='royalblue', lwd=2, ylim=c(100, 0.5*(z1+z2+z3)))
abline(h=0.5*(z1+z2+z3), col='firebrick', lwd=2) # Red line represents is 50% of total soil depth

data$SM.S=c(NA) # create total SM column for the S sub-station
for (i in 1:n) {data$SM.S[i]=c(z1*data$s4[i]+
                               z2*data$s5[i]+ 
                               z3*data$s6[i])/100}
plot(data$Date, data$SM.S, type='l', xlab='', ylab='Water depth (mm)', col='royalblue', lwd=2, ylim=c(100, 0.5*(z1+z2+z3)))
abline(h=0.5*(z1+z2+z3), col='firebrick', lwd=2) # Red line represents is 50% of total soil depth

# Calculate daily change in SM - This daily change in SM is the basis of the ET estimation. Positive values indicate an increase in soil moisture due to irrigation (or rain). Verify that there are no excessive drops (larger than the irrigation spikes) in soil moisture as these may be caused by rocky soils or poor sensor adherence to the soil and they can cause a poor ET estimate. ----
data$delta.N=c(NA) # Create delta column for the N sub-station
for (i in (2:n)) {data$delta.N[i]=data$SM.N[i]-data$SM.N[i-1]}
plot(data$Date, data$delta.N, type='l', xlab='', ylab='Delta soil moisture (mm)', col='orchid', lwd=2)
abline(h=0)

data$delta.S=c(NA) # Create delta column for the S sub-station
for (i in (2:n)) {data$delta.S[i]=data$SM.S[i]-data$SM.S[i-1]}
plot(data$Date, data$delta.S, type='l', xlab='', ylab='Delta soil moisture (mm)', col='orchid', lwd=2)
abline(h=0)

# Calculate daily ET estimate - Daily ET estimates are not accurate and only used to calculate the cumulative value. Verify that the daily values don't have large spikes (Max ~10-12 mm or 1.2*ETr depending on the crop) ----
data$ET.N=c(NA) # create daily ET column for the N sub-station
for (i in (2:n)) {
  if (data$delta.N[i]<0) {data$ET.N[i]=alpha*(data$ETr[i]+abs(data$delta.N[i]))}
  else {data$ET.N[i]=2*alpha*data$ETr[i]}}

plot(data$Date, data$ET.N, type='l', col='forestgreen', ylab='mm/day', xlab='2020', ylim=c(0, 16), lwd=2)
lines(data$Date, 1.2*data$ETr, type='l', col='firebrick', lwd=2)
legend('bottom', legend=c('ET', 'ETr'), inset=0.01, col=c('forestgreen', 'firebrick'), lty=1, lwd=2, horiz=T, box.lty=0)

data$ET.S=c(NA) # create daily ET column for the S sub-station
for (i in (2:n)) {
  if (data$delta.S[i]<0) {data$ET.S[i]=alpha*(data$ETr[i]+abs(data$delta.S[i]))}
  else {data$ET.S[i]=2*alpha*data$ETr[i]}}

plot(data$Date, data$ET.S, type='l', col='forestgreen', ylab='mm/day', xlab='2020', ylim=c(0, 16), lwd=2)
lines(data$Date, 1.2*data$ETr, type='l', col='firebrick', lwd=2)
legend('bottom', legend=c('ET', 'ETr'), inset=0.01, col=c('forestgreen', 'firebrick'), lty=1, lwd=2, horiz=T, box.lty=0)

# Eliminate spikes in daily ET: ET>1.2ETr=1.2ETr - If there are no spikes in the daily ET this part of the code does nothing. The 1.2ETr threshold was established for corn and alfalfa from literature (FAO) and varies for different crops.
for (i in (2:n)) {
  if (data$ET.N[i]>1.2*data$ETr[i]) 
     {data$ET.N[i]=1.2*data$ETr[i]}}

for (i in (2:n)) {
  if (data$ET.S[i]>1.2*data$ETr[i]) 
     {data$ET.S[i]=1.2*data$ETr[i]}}

# Calculate seasonal cumulative ET - Make sure this value makes sense and that it spans the entire season. ----
data$ET.N[1]=0
data$ET_cum.N=cumsum(data$ET.N)
plot(data$Date, data$ET_cum.N, type='l', lwd=2, ylim=c(0,1000), ylab='ET (mm)', xlab='', col='slateblue')

data$ET.S[1]=0
data$ET_cum.S=cumsum(data$ET.S)
plot(data$Date, data$ET_cum.S, type='l', lwd=2, ylim=c(0,1000), ylab='ET (mm)', xlab='', col='slateblue')

# Export results to a spreadsheet - Creates an .xlsx file in the ET estimates folder (folder needs to be created manually in the code working directory) ----
results.path=file.path('ET estimates', paste0('Station ', StationID, ' ET estimate', '.xlsx'))
write_xlsx(data, results.path)
