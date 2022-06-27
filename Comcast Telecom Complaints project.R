

# Import data into R environment.
setwd("D:/Downloads/GDrive/SEU_Files/Training_files/IT499/R/project")
getwd()
file <- read.csv("Comcast Telecom Complaints data.csv", stringsAsFactors = TRUE)
library(dplyr)
library(stringi)
library(lubridate)
library(ggplot2)

str(file)
summary(file)
names(file)<- stri_replace_all(regex = "\\.",replacement = "",str =names(file))
head(file)


# - Provide the trend chart for the number of complaints at monthly and daily granularity levels.
file$Date<- dmy(file$Date)
head(file)
by_day <- summarise(group_by(file,Date),Count=n())
by_month <- arrange(summarise(group_by(file,month=as.integer(month(Date))),Count=n()),month)

ggplot(data = by_month,aes(month,Count,label = Count))+geom_line(color="red")+geom_text()+scale_x_continuous(breaks = by_month$month)+labs(title = "Tickets Count by Month",x= "Month",y ="Count")
ggplot(data = by_day,aes(as.POSIXct(Date),Count))+geom_line(color="blue")+theme(axis.text.x = element_text(angle = 90))+scale_x_datetime(breaks = "1 weeks",date_labels = "%d/%m")+labs(title = "Tickets Count by Day",x= "Day",y ="Count")


# - Provide a table with the frequency of complaint types.
technical <- contains(file$CustomerComplaint,match=("speed"),ignore.case = T)
technical2 <- contains(file$CustomerComplaint,match="internet",ignore.case = T)
technical3 <- contains(file$CustomerComplaint,match="network",ignore.case = T)
financial <- contains(file$CustomerComplaint,match=("Pay"),ignore.case = T)
financial2 <- contains(file$CustomerComplaint,match=("bill"),ignore.case = T)
financial3 <- contains(file$CustomerComplaint,match=("price"),ignore.case = T)
financial4 <- contains(file$CustomerComplaint,match=("charge"),ignore.case = T)
services <- contains(file$CustomerComplaint,match="service",ignore.case = T)

file$Complaint_type[technical]<-"technical"
file$Complaint_type[technical2]<-"technical"
file$Complaint_type[technical3]<-"technical"
file$Complaint_type[financial]<-"financial"
file$Complaint_type[financial2]<-"financial"
file$Complaint_type[financial3]<-"financial"
file$Complaint_type[financial4]<-"financial"
file$Complaint_type[services]<-"services"
file$Complaint_type[-c(technical, financial, services)]<-"others"

table(file$Complaint_type)


# Which complaint types are maximum i.e., around internet, network issues, or across any other domains.
#   *the table shows that the maximum complaints are of type "others"
#   *and the minimum are of type "financial"


# - Create a new categorical variable with value as Open and Closed. Open & Pending is to be categorized as Open and Closed & Solved is to be categorized as Closed.

file <- cbind(file,new_status= (ifelse(file$Status=="Open","Open",ifelse(file$Status=="Pending","Open","Closed"))))


# - Provide state wise status of complaints in a stacked bar chart. Use the categorized variable from Q3. Provide insights on:
by_state <- summarize(group_by(file,State,new_status), Count=n())
ggplot(by_state ,aes(State,Count))+geom_col(aes(fill = new_status),width = 0.75)+theme(axis.text.x = element_text(angle = 90))+labs(title = "Tickets Count by State",x = "State",y = "Count",fill= "Status")

  
#   Which state has the maximum complaints
by_state <- as.data.frame(by_state)
filter(by_state, Count ==max(Count))

# Which state has the highest percentage of unresolved complaints
filter(filter(by_state,new_status=="Open"),Count ==max(Count))

# - Provide the percentage of complaints resolved till date, which were received through the Internet and customer care calls.
resolved_tickets <- summarise(filter(file, new_status=="Closed"),count=n())
resolved_tickets
via_Internet <- summarise(filter(file,new_status=="Closed", ReceivedVia=="Internet"),count=n())
via_Calls <- summarise(filter(file,new_status=="Closed", ReceivedVia =="Customer Care Call"),count=n())

internet_tickets_percentage <- (via_Internet/resolved_tickets)*100
internet_tickets_percentage
call_tickets_percentage <- (via_Calls/resolved_tickets)*100
call_tickets_percentage
