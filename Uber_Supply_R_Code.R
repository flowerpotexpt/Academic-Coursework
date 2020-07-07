#Load and name the Uber master data file
uber_masterdata <- read.csv("Uber Request Data (1).csv",stringsAsFactors = FALSE,na.strings = TRUE)

#Remove all NA values for (better analytics)
na.omit(uber_masterdata)

#Uniform formatting of date and time using lubridate package
library(lubridate)
uber_masterdata$Request.timestamp <- parse_date_time(uber_masterdata$Request.timestamp, c("%d/%m/%Y %H:%M", "%d-%m-%Y %H:%M:%S"))
uber_masterdata$Drop.timestamp <- parse_date_time(uber_masterdata$Drop.timestamp, c("%d/%m/%Y %H:%M", "%d-%m-%Y %H:%M:%S"))

#Separate date and time into separate columns (Date, Request_Time & Drop_Time)
request_dateA <- as.Date(uber_masterdata$Request.timestamp,format="%d/%m/%Y") 
request_dateB <- as.Date(uber_masterdata$Request.timestamp,format="%d-%m-%Y") 
request_dateA[is.na(request_dateA)] <- request_dateB[!is.na(request_dateB)]
Request_Time <- sapply(strsplit(as.character(uber_masterdata$Request.timestamp), " "), "[", 2)
Drop_Time <- sapply(strsplit(as.character(uber_masterdata$Drop.timestamp), " "), "[", 2)
uber_masterdata["Date"] <- request_dateA
uber_masterdata["Request_Time"] <- Request_Time
uber_masterdata["Drop_Time"] <- Drop_Time

#Remove original timestamp columns (Request.timestamp & Drop.timestamp)
uber_masterdata$Request.timestamp <- NULL
uber_masterdata$Drop.timestamp <- NULL

#The first 2 digits in the Request_Time column are extracted and saved in the column request_hour
uber_masterdata$request_hour <- sub(":.*","",uber_masterdata$Request_Time)
#Convert the digits to numeric data type
uber_masterdata$request_hour <- as.numeric(uber_masterdata$request_hour)

#Load the required packages 
require(dplyr)
require(ggplot2)
require(scales)

#Plot the number of cabs requested in all the weekdays 
#Airport is displayed in red where as city is displayed in blue
hourwise_request_count <- ggplot(uber_masterdata,aes(x=factor(request_hour),fill=factor(Pickup.point)))
#Add title and lables to the plot and save it as an object
plot1 <- hourwise_request_count+geom_bar(stat='count',position = "dodge")+
ggtitle("Hourly Demand for Uber Cabs")+
labs(x="Time in Hours", y="Number of Cabs Requested")+
labs(fill="Pickup Point")

#View the plot
plot1

#Generate a sequence of numbers from 0 to 23 and save it as a vector
request_hour <- c(0:23)
#Create a vector called time_slot1
Time_Slot1 <- c("Pre_Morning","Morning_Rush","Day_Time","Evening_Rush","Late_Night")
#Create a vector which represents the number of times time slots are to be repeated
times <- c(4,6,7,5,2)

#Repeat the time slots number of times required and save it as a vector
#The number of elements in this vector should correspond to 24
Time_Slot <- rep(Time_Slot1,times)
#Create a new dataframe with sequence of number generated and time slots
new_frame <- data.frame(Time_Slot,request_hour)
#Merge the main uber request dataframe with the new dataframe 
uber_masterdata <- merge(uber_masterdata,new_frame,by="request_hour",all.x=TRUE)
#Change the sequence of columns of dataframe
uber_masterdata <- uber_masterdata[,c(2,3,4,5,6,7,8,1,9)]

#Subset the master dataframe where row values are "Trip Completed"
trips_completed <- subset(uber_masterdata,uber_masterdata$Status=="Trip Completed")
#Plot a bar chart with time-slots on x axis and trips completed on Y-axis
Timeslot_bar <- ggplot(trips_completed,aes(x=Time_Slot))
plot2 <- Timeslot_bar+geom_bar(stat="count",col="black",fill="green")+
ggtitle("Trips completed during different Time Slots")+
labs(x="Time Slots",y="Trips Completed")+
geom_text(stat='count',aes(label=..count..),vjust=-1)+
guides(fill=FALSE)+
scale_x_discrete(limits=c("Morning_Rush","Evening_Rush","Day_Time",
                            "Late_Night","Pre_Morning"))

#View the plot
plot2

#Plot a bar chart with time slots on x-axis and request frequency on y-axis
#Show the status of requests in different colors, add title, axis labels 
#Save the plot as an object
timeslot_request_count <- ggplot(uber_masterdata,aes(x=factor(Time_Slot),fill=factor(Status)))
plot3 <- timeslot_request_count+geom_bar(stat="count",position = "stack",col="black")+
ggtitle("Trips during Different Time Slots")+
scale_x_discrete(limits=c("Evening_Rush","Morning_Rush","Day_Time",
                            "Late_Night","Pre_Morning"))+
labs(x="Time Slots",y="Number of Requests")+labs(fill="Trip Status")+
scale_fill_discrete(limits=c("Trip Completed","No Cars Available","Cancelled"))

#View the plot
plot3

#Problem 1. Large number of service requests got cancelled during the Morning_Rush Time slot
#Subset the Morning Rush time slot data for analysis
Problem_df <- subset(uber_masterdata,uber_masterdata$Time_Slot=="Morning_Rush")
#Plot the bargraph with status of request in x-axis and count in y-axis for Morning rush time slot
#Show the request from different pickup points in different colors
Problem1_count <- ggplot(Problem_df,aes(x=factor(Status),fill=factor(Pickup.point)))
plot4 <- Problem1_count+geom_bar(stat="count",position = "stack")+
ggtitle("Morning Rush Cab Status")+
labs(x="Status",y="Total count")+
labs(fill="Pickup Point")+scale_x_discrete(limits=c("Trip Completed","Cancelled","No Cars Available"))+
annotate("text", x=-Inf,y=Inf,label="Airport - 2.79% & City = 97.21%", hjust=-.1,vjust=1)

#View the plot
plot4

#Number of trips cancelled for the Morning rush time slot
total_trip_cancel <- length(which(Problem_df$Status=="Cancelled"))
#Number of trips cancelled from airport for Morning rush
airport_trip_cancel <- length(which((Problem_df$Pickup.point=="Airport") & (Problem_df$Status == "Cancelled")))
#Number of trips cancelled from city for Morning rush
city_trip_cancel <- length(which((Problem_df$Pickup.point=="City") & (Problem_df$Status == "Cancelled")))
#Percentage of trips cancelled from city out of total trips cancelled during morning rush
percent_trip_cancel_city <- (city_trip_cancel/total_trip_cancel*100)
#Percentage of trips cancelled from airport out of total trips cancelled during Morning rush
percent_trip_cancel_airport <- (airport_trip_cancel/total_trip_cancel*100)
#Number of trips requested from city to airport during morning rush
demand_trip_request_city <- length(which(Problem_df$Pickup.point=="City"))
#Number of trips completed from city to airport during morning rush
demand_trip_city_completed <- length(which((Problem_df$Pickup.point=="City")& (Problem_df$Status=="Trip Completed")))

#Problem 2
#Subset the data for Evening rush from dataframe for analysis
Problem2_df <- subset(subset(uber_masterdata,uber_masterdata$Time_Slot=="Evening_Rush"))
#Plot the bar graph with status of requests on x-axis and count in y-axis for evening rush time slot
#Show the request from different pickup points in different colors
Problem2_count <- ggplot(Problem2_df,aes(x=factor(Status),fill=factor(Pickup.point)))
plot5 <- Problem2_count+geom_bar(stat="count",position = "stack")+
ggtitle("Evening Rush Cabs Status")+
labs(x="Status",y="Total count")+
labs(fill="Pickup Point")+scale_x_discrete(limits=c("No Cars Available","Trip Completed","Cancelled"))+
annotate("text", x=-Inf,y=Inf,label="Airport - 94.9% & City = 5.1%", hjust=-.1,vjust=1)  

#View the plot
plot5

#Number of service requests with number cars available for evening rush time slot
total_nocar_available <- length(which(Problem2_df$Status=="No Cars Available"))
#Number of  service requests with number cars available from airport during evening rush
airport_nocar_available <- length(which((Problem2_df$Pickup.point=="Airport") & (Problem2_df$Status == "No Cars Available")))
#Number of service requests with Number cars available from city during evening rush
city_nocar_available <- length(which((Problem2_df$Pickup.point=="City") & (Problem2_df$Status == "No Cars Available")))
#Percentage of cars available status from city out of total cars available during evening rush
percent_city_nocar <- (city_nocar_available/total_nocar_available*100)
#Percentage of cars available status from airport out of total cars available during evening rush
percent_airport_nocar <- (airport_nocar_available/total_nocar_available*100)
#Number of service requests from airport to city during evening rush
demand_nocar_request_airport <- length(which(Problem2_df$Pickup.point=="Airport"))
#Number of trips completed from airport to city during evening rush
demand_nocar_request_airport_completed <- length(which((Problem2_df$Pickup.point=="Airport") & (Problem2_df$Status=="Trip Completed")))


