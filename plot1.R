## ############################################################################
## File: plot1.R
## Version: 1.0
## Date: 20/03/2016
## 
## The function reads the "household_power_consumption.txt" file 
## stored in the "Data" folder and creates the the plot1.png file.
## 
## The data file stores these attributes:
## Date                   --> 16/12/2006
## Time                   --> 17:24:00
## Global_active_power    --> 4.216
## Global_reactive_power  --> 0.418
## Voltage                --> 234.840
## Global_intensity       --> 18.400
## Sub_metering_1         --> 0.000
## Sub_metering_2         --> 1.000
## Sub_metering_3         --> 17.000
## ############################################################################
plot1 <- function(){
  ## Delete data and values from the environment
  rm(list=ls())

  ## Load library
  library(dplyr)

  ## Load data
  powerConsumption <- read.table("./Data/household_power_consumption.txt",
                                 header = TRUE, 
                                 sep = ";",
                                 stringsAsFactors=FALSE,
                                 na.strings = "?")



  ## Convert the "Date" variable into a Date class.
  powerConsumption <- mutate(powerConsumption, Date = as.Date(Date,format='%d/%m/%Y'))


  d1<-as.Date("2007-02-01") ## Date object which represents the initial date  
  d2<-as.Date("2007-02-02") ## Date object which represents the final date  
  
  ## Filter tha data frame.
  ## Keep only the observations that belongs to Day1 or to Day2
  powerConsumption.f <- filter(powerConsumption, Date==d1 | Date==d2)


  # 1 Open a graphics device.
  # The default graphics device in R is your computer screen. To save a plot to an image file, you need to tell R to open a new type of device - in this case, a graphics file of a specific type, such as PNG, PDF, or JPG.
  # The R function to create a PNG device is png(). Similarly, you create a PDF device with pdf() and a JPG device with jpg().
  png(filename="plot1.png")
  
  # 2 Create the plot
  hist(powerConsumption.f$Global_active_power, 
       main="Global Active Power", 
       xlab="Global Active Power (kilowatts)",
       col="red")
  
  # 3 Close the graphics device
  dev.off()

}





