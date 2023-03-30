# Site has 6 soil moisture sensors split among two sites;
# sensors 1:3 are North (N) and sensors 4:6 are South (S).

# Packages ----
library(readxl)
library(writexl)

# Data ----
data=read_xlsx('Soil moisture data/Station 16.xlsx')
n=length(data$Date) # Number of days

# Plot daily soil moisture data----
plot(data$Date, data$s1, type='l', xlab='', ylab='Volumetric water content (%)', ylim=c(0, 40), col='royalblue', lwd=2)
lines(data$Date, data$s2, col='goldenrod', lwd=2)
lines(data$Date, data$s3, col='salmon', lwd=2)

plot(data$Date, data$s4, type='l', xlab='', ylab='Volumetric water content (%)', ylim=c(0, 40), col='royalblue', lwd=2)
lines(data$Date, data$s5, col='goldenrod', lwd=2)
lines(data$Date, data$s6, col='salmon', lwd=2)

# Assign alpha value
alpha=0.42780695937849

# Soil layer thickness as represented by the sensors (1ft=304.8mm)
z1=((1+0.5)/2)*304.8          # 1st sensor: surface to .75ft
z2=((2+1)/2-(1+0.5)/2)*304.8  # 2nd sensor: .75ft to 1.5ft
z3=(2*(2-(2+1)/2))*304.8      # 3rd sensor: 1.5ft to 2.5ft

# Calculate total soil moisture in the profile (mm) ----
data$SM.N=c(NA) # create SM column
for (i in 1:n) {data$SM.N[i]=c(z1*data$s1[i]+
                               z2*data$s2[i]+ 
                               z3*data$s3[i])/100}
plot(data$Date, data$SM.N, type='l', xlab='', ylab='Water depth (mm)')

data$SM.S=c(NA) # create SM column
for (i in 1:n) {data$SM.S[i]=c(z1*data$s4[i]+
                               z2*data$s5[i]+ 
                               z3*data$s6[i])/100}
plot(data$Date, data$SM.S, type='l', xlab='', ylab='Water depth (mm)')

# Calculate daily change in SM ----
data$delta.N=c(NA)
for (i in (2:n)) {data$delta.N[i]=data$SM.N[i]-data$SM.N[i-1]}
plot(data$Date, data$delta.N, type='l', xlab='', ylab='Delta soil moisture (mm)')
abline(h=0)

data$delta.S=c(NA)
for (i in (2:n)) {data$delta.S[i]=data$SM.S[i]-data$SM.S[i-1]}
plot(data$Date, data$delta.S, type='l', xlab='', ylab='Delta soil moisture (mm)')
abline(h=0)

# Calculate daily ET estimate ----
data$ET.N=c(NA)
for (i in (2:n)) {
  if (data$delta.N[i]<0) {data$ET.N[i]=alpha*(data$ETr[i]+abs(data$delta.N[i]))}
  else {data$ET.N[i]=2*alpha*data$ETr[i]}}

plot(data$Date, data$ET.N, type='l', col='goldenrod', ylab='mm/day', xlab='2020')
lines(data$Date, 1.2*data$ETr, type='l')

data$ET.S=c(NA)
for (i in (2:n)) {
  if (data$delta.S[i]<0) {data$ET.S[i]=alpha*(data$ETr[i]+abs(data$delta.S[i]))}
  else {data$ET.S[i]=2*alpha*data$ETr[i]}}

plot(data$Date, data$ET.S, type='l', col='goldenrod', ylab='mm/day', xlab='2020')
lines(data$Date, 1.2*data$ETr, type='l')

# Eliminate spikes in daily ET: ET>1.2ETr=1.2ETr
for (i in (2:n)) {
  if (data$ET.N[i]>1.2*data$ETr[i]) 
     {data$ET.N[i]=1.2*data$ETr[i]}}

plot(data$Date, data$ET.N, type='l', col='goldenrod', ylab='mm/day', xlab='2020')
lines(data$Date, 1.2*data$ETr, type='l')

for (i in (2:n)) {
  if (data$ET.S[i]>1.2*data$ETr[i]) 
     {data$ET.S[i]=1.2*data$ETr[i]}}

plot(data$Date, data$ET.S, type='l', col='goldenrod', ylab='mm/day', xlab='2020')
lines(data$Date, 1.2*data$ETr, type='l')

# Calculate seasonal cumulative ET ----
data$ET.N[1]=0
data$ET_cum.N=cumsum(data$ET.N)

data$ET.S[1]=0
data$ET_cum.S=cumsum(data$ET.S)

# Plot seasonal cumulative ET
plot(data$Date, data$ET_cum.N, type='l', lwd=2, ylim=c(0,1000), ylab='mm', xlab='2020', col='goldenrod', cex.axis=1.5, cex.lab=1.5)
plot(data$Date, data$ET_cum.S, type='l', lwd=2, ylim=c(0,1000), ylab='mm', xlab='2020', col='goldenrod', cex.axis=1.5, cex.lab=1.5)

# Export results to a spreadsheet
write_xlsx(data, 'ET estimates/Station 16 ET estimate.xlsx')

