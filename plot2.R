## ############################################################################
## File: plot2.R
## Version: 1.0
## Date: 20/03/2016
## 
## The plot4()function reads the "household_power_consumption.txt" file 
## stored in the "Data" folder and creates the the plot2.png file.
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
plot2 <- function(){
  ## Delete data and values from the environment
  rm(list=ls())
  
  ## Load library
  library(dplyr)
  
  ## Load data
  powerConsumption <- read.table("./Data/household_power_consumption.txt",
                                 header = TRUE, 
                                 sep = ";",
                                 stringsAsFactors=FALSE,
                                 na.strings = "?",
                                 colClasses = c("character", 
                                                "character", 
                                                "numeric", 
                                                "NULL", 
                                                "NULL", 
                                                "NULL", 
                                                "NULL", 
                                                "NULL", 
                                                "NULL")                                 
                                 )
  
  ## Add the new variable "dateTime" as the union of "Date" and "Time"
  powerConsumption <- mutate(powerConsumption, 
                             dateTime = paste(Date, Time)) 
  ##dateTime: chr  "16/12/2006 17:24:00" "16/12/2006 17:25:00"

  ## Convert the new variable "dateTime" as POSIXct 
  powerConsumption <- mutate(powerConsumption, 
                             dateTime = as.POSIXct(dateTime, 
                                                   format="%d/%m/%Y %H:%M:%S")
                                                    )

  
  ## Convert the date variable into a Date class.
  powerConsumption <- mutate(powerConsumption, 
                             Date = as.Date(Date,format='%d/%m/%Y'))
  
  d1<-as.Date("2007-02-01") ## Date object which represents the initial date  
  d2<-as.Date("2007-02-02") ## Date object which represents the final date
  
  ## Filter tha data frame.
  ## Keep only the observations that belongs to Day1 or to Day2
  powerConsumption.f <- filter(powerConsumption, Date==d1 | Date==d2)
  
  # 1 Open a graphics device.
  # The default graphics device in R is your computer screen.
  # To save a plot to an image file, you need to tell R to open a new type of device
  # The R function to create a PNG device is png().
  # Similarly, you create a PDF device with pdf() and a JPG device with jpg().
  png(filename="plot2.png")


  # 2 Create the plot using blue points overlayed by a line 
  plot(powerConsumption.f$dateTime, 
       powerConsumption.f$Global_active_power,
       type="l", 
       xlab="",
       ylab="Global Active Power (kilowatts)")

  
  # 3 Close the graphics device
  dev.off()
}