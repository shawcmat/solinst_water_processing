################################
#EXO Data Summaries and figures#
################################
###Written by McKenna Bristow###
################################

library(wlTools)
library(dplyr)
library(ggplot2)
library(data.table)
library(tidyr)
library(lubridate)

#Pull in data - the large QA'd and concatenated file - and get date/time variables in order
setwd("Z:/Davis/Thorne/Data Files/3 Projects/8 Water Quality/1 Site data/Dutch Slough/2 Processed data/EXO") #update with YOUR working directory***
data = read.csv("LittleDutchEXO_2021.05.21_2023.01.31.csv") #add file name you are going to work on***
exoname = "LittleDutchEXO 2021-2023" #Name the EXO and provide date range of data for figures***

#run the following, no inputs needed
colnames(data)[1]="Date" #fix column names for date and time for next step
colnames(data)[2]="Time"

data$date1=mdy(data$Date) #convert character into date format

data$datetime<-as.POSIXct(paste(data$date1, data$Time), format="%Y-%m-%d %H:%M:%S") #convert date and time into a POSIXct format datetime variable
str(data) #check data formats,make sure datetime variable populated correctly (not NAs)

#fill in all missing dates so there is continuous datetime data for the entire monitoring period, 
#change indexing values based on length of dataframe (e.g # of rows needs to be updated depending on the size fo the dataframe)
data=as.data.frame(complete(data, datetime = seq(as.POSIXct(datetime[1]), as.POSIXct(datetime[42491]), by = '15 mins'))) #fill in number of rows***


#####QAQC####
datatrim=data #to avoid touching original data, create a new dataframe called datatrim to QAQC
names(datatrim) #list of all column names in file, easy to copy paste variable names into next line

#plot data - update y = with variable you are looking at

p = ggplot(data = subset(datatrim, datetime > "2021-05-01 00:00:00" & datetime < "2023-02-01 00:00:00"), aes(x=datetime, y=SpC_Combined)) + #update variable name***
  geom_path(aes(x=datetime, y=SpC_Combined_.uS.cm.), linewidth=.75) + #update variable name***
  theme_bw() + theme(text = element_text(size = 14), 
                     axis.text.x = element_text(angle = 45, margin = margin(t=5),hjust=1)) +
  labs(x="Date",title=exoname) +
  scale_x_datetime(date_labels = "%b-%d-%y", date_breaks = "2 weeks") # + ylim(0, 400) #can use the hashtagged out ylim() here to zoom in if needed

p


#trim as needed, make notes in READ_ME file
#NOTE: This is different from QAQCing Solinst data in that each sensor is an individual unit, 
#so we ONLY trim data by the specific variable/sensor we are working with. Therefore, you need to verify 
#the rows and the column you enter as you QAQC (Can check this by viewing the data frame: >View(datatrim).
#If you are trimming for deployment start and end, you can NA all columns except Date and Time
datatrim[ROWS,COLUMNS]=NA #can use concatenate c() for ROWS to stitch disjointed outliers together for QAQC i.e. [c(1,23,435,467,889),11]


#CLEAR ABOVE once complete with QA step, do not save with your edits in this file

#Once trimming is complete, save trimmed data to QAQC'd folder (may have to update pathway)
write.csv(datatrim, row.names = T, "Z:/Data Files/3 Projects/8 Water Quality/1 Site data/Dutch Slough/3 QAQCed data/EXO/FILE.NAME.csv")

#Create final graphs
names(datatrim)
y = datatrim$VARIABLE #update with variable you want to plot***
varname = "VARIABLE.NAME.UNITS" #update with proper title with units***

#no touchy#
start="2021-10-19 05:30:00" #atmospheric river Oct 2021 start date
start=as.POSIXct(start)
end = "2021-10-26 17:00:00" #atmospheric river Oct 2021 end date
end=as.POSIXct(end)
atmorect = data.frame(xmin=start,xmax=end,ymin=-Inf,ymax=Inf) #makes a highlight bar for timeperiod on figure
start1 = "2021-11-06 12:00:00" #breach start date
start1 = as.POSIXct(start1)
end1 = "2021-11-14 23:00:00" #breach end date
end1=as.POSIXct(end1)
breachrect = data.frame(xmin=start1,xmax=end1,ymin=-Inf,ymax=Inf) #makes highlight bar for breach period on figure
xpos = as.POSIXct("2021-11-25 23:00:00") #x-axis position of "breach" label
xpos1 = as.POSIXct("2021-09-30 23:00:00") #x-axis position of "Atmospheric river" label

#run plot
fig = ggplot(data=datatrim, aes(x=datetime, y=as.numeric(y))) +
  geom_path(aes(x=datetime, y=y), linewidth=.75) +
  annotate("text",x = xpos,y=2000, label = "Breach") + #update y position as needed***
  annotate("text",x = xpos1,y=2000, label = "Atmospheric\nrivers") + #update y position as needed***
  geom_rect(data=atmorect,aes(xmin=xmin,xmax=xmax,ymin=ymin,ymax=ymax),
            color = "dodgerblue3",fill="grey60",alpha=0.5,inherit.aes=F)+
  geom_rect(data=breachrect,aes(xmin=xmin,xmax=xmax,ymin=ymin,ymax=ymax),
            color = "green4", fill = "grey60", alpha=0.5, inherit.aes = F) +
  theme_bw() + 
  theme(
    text = element_text(size = 14), 
    axis.text.x = element_text(angle = 45, margin = margin(t=5),hjust=1)) +
  labs(x="Date",y = varname, title=exoname) +
  scale_x_datetime(date_labels = "%b-%d-%y", date_breaks = "2 weeks")  # +ylim(-1, 500) #can use the hashtagged out ylim() here to zoom in if needed
fig
