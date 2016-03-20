## ############################################################################
## File: plot4.R
## Version: 1.0
## Date: 20/03/2016
## 
## The plot4()function reads the "household_power_consumption.txt" file 
## stored in the "Data" folder and creates the the plot4.png file.
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

plot4 <- function(){
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
                                          "numeric",  ## Global_active_power 
                                          "numeric",  ## Global_reactive_power
                                          "numeric",  ## Voltage
                                          "NULL", 
                                          "numeric",   ## Sub_metering_1
                                          "numeric",   ## Sub_metering_2
                                          "numeric"    ## Sub_metering_3
                                        )                                 
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

  
  ## Convert the "Date" variable into a Date class.
  powerConsumption <- mutate(powerConsumption, 
                             Date = as.Date(Date,format='%d/%m/%Y'))

  d1<-as.Date("2007-02-01") ## Date object which represents the initial date  
  d2<-as.Date("2007-02-02") ## Date object which represents the final date
  
  ## Filter tha data frame.
  ## Keep only the observations that belongs to Day1 or to Day2
  powerConsumption.f <- filter(powerConsumption, Date==d1 | Date==d2)


  # Remove from the data frame the observations without data
  # na.omit is nicer for just removing all NA's.
  # complete.cases allows partial selection by using part of the dataframe final[complete.cases(final[,5:6]),]
  powerConsumption.f.complete<-powerConsumption.f[complete.cases(powerConsumption.f),]

  # 1 Open a graphics device.
  # The default graphics device in R is your computer screen.
  # To save a plot to an image file, you need to tell R to open a new type of device
  # The R function to create a PNG device is png().
  # Similarly, you create a PDF device with pdf() and a JPG device with jpg().
  png(filename="plot4.png")
  
  
  # 2 Create a multi-paneled plotting window
  # 
  # mfrow: number of Multiple Figures
  
  # mfrow means to plot the figures in row 1 from left-to-right, 
  # followed by row 2 from left-to-right, etc.  
  
  # mfrow: a vector of length 2, where the first argument specifies 
  # the number of rows and the second the number of columns of plots
  
  par(mfrow=c(2,2))
  
  
  
  
  # 3.1 R1C1 - Create the plot using blue points overlayed by a line 
  plot(powerConsumption.f$dateTime, 
       powerConsumption.f$Global_active_power,
       type="l", 
       xlab="",
       ylab="Global Active Power")

  # 3.3 R1C2 - Create Line Chart
  plot(powerConsumption.f$dateTime, 
       powerConsumption.f$Voltage,
       type="l", 
       xlab="datetime",
       ylab="Voltage")
  
  
  # 3.2 R2C1 - Create Line Chart
  # get the range for the x and y axis
  xrange <- range(powerConsumption.f.complete$dateTime)

  
  Sub_metering_1.r = range(powerConsumption.f.complete$Sub_metering_1)
  Sub_metering_1.max = max(Sub_metering_1.r)
  Sub_metering_1.min = min(Sub_metering_1.r)
  
  Sub_metering_2.r = range(powerConsumption.f.complete$Sub_metering_2)  
  Sub_metering_2.max = max(Sub_metering_2.r)
  Sub_metering_2.min = min(Sub_metering_2.r)
  
  Sub_metering_3.r = range(powerConsumption.f.complete$Sub_metering_3)
  Sub_metering_3.max = max(Sub_metering_3.r)
  Sub_metering_3.min = min(Sub_metering_3.r)

  yrange <- range(c(Sub_metering_1.max,Sub_metering_1.min,
                    Sub_metering_2.max,Sub_metering_2.min,
                    Sub_metering_3.max,Sub_metering_3.min
                    ))
  

  # set up the plot
  plot(	xrange, 
        yrange,
        type="n", 
        xlab="",
        ylab="Energy sub metering"
  )
  
  # add lines
  lines(powerConsumption.f$dateTime, 
       powerConsumption.f$Sub_metering_1,
       type="l", 
       col="black" )  
  
  lines(powerConsumption.f$dateTime, 
        powerConsumption.f$Sub_metering_2,
        type="l", 
        col="red" )  
  
  lines(powerConsumption.f$dateTime, 
        powerConsumption.f$Sub_metering_3,
        type="l", 
        col="blue" )  

    legend( 
            # the x and y co-ordinates to be used to position the legend.
            # They can be specified by keyword or in any way which is accepted by xy.coords
            x="topright", 
            # y           

            # a character vector to appear in the legend
            legend=c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"),
            
            # The line types and widths for lines appearing in the legend.
            # One of these two must be specified for line drawing
            lty=1 ,    # gives the legend appropriate symbols (lines)
            # lwd 	
            
            # the color of points or lines appearing in the legend.
            col=c("black","blue","red"),
            
            # the type of box to be drawn around the legend.
            # The allowed values are "o" (the default) and "n".
            bty="n"  
                  
            )
 
    # 3.4 R2C2 - Create Line Chart
    plot(powerConsumption.f$dateTime, 
         powerConsumption.f$Global_reactive_power,
         type="l", 
         xlab="datetime",
         ylab="Global reactive power")
    
               
  # 4 Close the graphics device
  dev.off()
}