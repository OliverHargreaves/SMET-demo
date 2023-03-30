# Data sheet preparation for Wellsville alfalfa

# Packages ----
library(readxl)
library(writexl)

# Data preparation ----
data=read_xlsx('Raw data/Station 16.xlsx')

# Discard non-relevant rows and columns
data=data[, 1:7]  
# Rename columns
colnames(data)[1]='Date'                                
for(i in 1:6) {colnames(data)[i+1]=paste('s',i,sep="")}

# Filter data to have only one value per day 
data$Year=format(data$Date, "%y")
data=data[data$Year=='21',]
data$Hour=format(data$Date, "%H")       # Create a column with the hour
data=data[data$Hour=='23',]             # Keep only the 23:xx values
data$Minute=format(data$Date, '%M')     # Create a minutes column
data=data[data$Minute=='45',]           # Keep only the xx:45 values
data=data[,-c(8,9,10)]                  # Eliminate hour and minute columns

write_xlsx(data, 'Soil moisture data/Station 16.xlsx')
