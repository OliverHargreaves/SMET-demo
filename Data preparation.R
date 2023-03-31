# Input data sheet preparation for running the SMET model

# Packages ----
library(readxl)
library(writexl)

# Upload data ----
StationID=16 # Station ID number: needs to be inputted manually
data.path=file.path('Raw data', paste0('Station ', StationID, '.xlsx'))
data=read_xlsx(data.path)

data=data[, 1:7] # Keep only volumetric water content data

# Rename columns
colnames(data)[1]='Date'                                
for(i in 1:6) {colnames(data)[i+1]=paste('s',i,sep="")}

# Select the year of interest if the data is from multiple years, otherwise delete/skip/ignore
data$Year=format(data$Date, "%y")       # Create a column with the year
data=data[data$Year=='21',]             # Keep only the 2021 values

# Extract the "midnight" value ----
data$Hour=format(data$Date, "%H")       # Create a column with the hour
data=data[data$Hour=='23',]             # Keep only the 23:xx values
data$Minute=format(data$Date, '%M')     # Create a minutes column
data=data[data$Minute=='45',]           # Keep only the xx:45 values

data=data[,-c(8,9,10)]                  # Eliminate year, hour, and minute columns

# Export the results
results.path=file.path('Soil moisture data', paste0('Station ', StationID, '.xlsx'))
write_xlsx(data, results.path)
