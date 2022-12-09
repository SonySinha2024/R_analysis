## Analysis Tasks:
## Comcast is an American global telecommunication company. 
# The firm has been providing terrible customer service. 
# They continue to fall short despite repeated promises to improve. 
# Only last month (October 2016) the authority fined them a $2.3 million, after receiving over 1000 consumer complaints.
# The existing database will serve as a repository of public customer complaints filed against Comcast.
## Noting down what is wrong with Comcast's customer service.

## Geting and print current working directory.
print(getwd())

install.packages('lubridate')

## Libraries 
library(stringi)
library(lubridate)
library(dplyr)
library(ggplot2)
library(ggpubr)

## 1.Import data into R environment
## Importing Data 
data <- read.csv("C:/Users/nawin/Comcast_Telecom_Complaints_data.csv")
## Printing Dataset
print(data)

## Printing column names
colnames(data)

##  Printing rows and columns 
print(ncol(data))
## observation: There are 10 columns
print(nrow(data))
## observation: There are 2224 rows

#Manipulating column names
names(data)<- stri_replace_all(regex =  "\\.",replacement = "",str =names(data))
head(data)

## Finding the missing values
# find location of missing values
print("Position of missing values:")
which(is.na(data))
# Count total missing values 
print("Count of total missing values:")
sum(is.na(data))
## Observation: There are no missing values

## 2. Provide the trend chart for the number of complaints at monthly and daily granularity levels

## Extracting Monthly and Daily Ticket Count
data$Date<- dmy(data$Date)
monthly_count<- summarise(group_by(data,Month =as.integer(month(Date))),Count = n())
daily_count<- summarise(group_by(data,Date),Count =n())
monthly_count<-arrange(monthly_count,Month)


## Plotting monthly ticket count
ggplot(data = monthly_count,aes(Month,Count,label = Count))+
  geom_line()+
  geom_point(size = 0.8)+
  geom_text()+
  scale_x_continuous(breaks = monthly_count$Month)+
  labs(title = "Monthly Ticket Count",x= "Months",y ="No. of Tickets")+
  theme(plot.title = element_text(hjust = 0.5))
## Observation: June has maximum Monthly Ticket Count

## Daily Ticket Count
ggplot(data = daily_count,aes(as.POSIXct(Date),Count))+
  geom_line()+
  geom_point(size = 1)+
  scale_x_datetime(breaks = "1 weeks",date_labels = "%d/%m")+
  labs(title = "Daily Ticket Count",x= "Days",y ="No. of Tickets")+
  theme(axis.text.x = element_text(angle = 75),
        plot.title = element_text(hjust = 0.5))
## Observation: 21st June has maximum "Daily Ticket Count"

## 3.Provide a table with the frequency of complaint types.
data_date<-data %>% group_by(Date) %>% dplyr::summarise(frequency = n())
df <-data_date[order(-data_date$frequency),]
df1<-head(df)
df1


# Complaint Type Processing
network_tickets<- contains(data$CustomerComplaint,match = 'network',ignore.case = T)
internet_tickets<- contains(data$CustomerComplaint,match = 'internet',ignore.case = T)
billing_tickets<- contains(data$CustomerComplaint,match = 'bill',ignore.case = T)
email_tickets<- contains(data$CustomerComplaint,match = 'email',ignore.case = T)
charges_ticket<- contains(data$CustomerComplaint,match = 'charge',ignore.case = T)

## 4.Which complaint types are maximum i.e., around internet, network issues, or across any other domains
data$ComplaintType[internet_tickets]<- "Internet"
data$ComplaintType[network_tickets]<- "Network"
data$ComplaintType[billing_tickets]<- "Billing"
data$ComplaintType[email_tickets]<- "Email"
data$ComplaintType[charges_ticket]<- "Charges"
data$ComplaintType[-c(internet_tickets,network_tickets,
                      billing_tickets,charges_ticket,email_tickets)]<- "Others"
table(data$ComplaintType)
## Observation: 
##Billing  Charges    Email Internet  Network   Others 
#  363      139       16      472        1     1233 
## The Internet type complaints are maximum.


## 5. Create a new categorical variable with value as Open and Closed. Open & Pending is to be categorized as Open and Closed & Solved is to be categorized as Closed.
## Creating a new categorical variable for Complaint Status
open_complaints<-(data$Status == 'Open' | data$Status == 'Pending')
closed_complaints<-(data$Status == 'Closed' | data$Status == 'Solved')
data$ComplaintStatus[open_complaints]<-'Open'
data$ComplaintStatus[closed_complaints]<-'Closed'

## 6.Provide state wise status of complaints in a stacked bar chart. Use the categorized variable from Q3. Provide insights on:
## 7.Which state has the maximum complaints
state_max<-table(data$ComplaintStatus,data$State)
state_max
data<- group_by(data,state,ComplaintStatus)
chart_data<- summarise(data,Count = n())

## Plotting on stacked bar chart
ggplot(as.data.frame(chart_data) ,mapping = aes(State,Count))+
  geom_col(aes(fill = ComplaintStatus),width = 0.95)+
  theme(axis.text.x = element_text(angle = 90),
        axis.title.y = element_text(size = 15),
        axis.title.x = element_text(size = 15),
        title = element_text(size = 16,colour = "#0073C2FF"),
        plot.title = element_text(hjust =  0.5))+
  labs(title = "Ticket Status Stacked Bar Chart ",
       x = "States",y = "No of Tickets",
       fill= "Status")
## Observation: Georgia has maximum complaints.

## 8.Which state has the highest percentage of unresolved complaints
data %>% filter(ComplaintStatus=='Open') %>% group_by(State) %>% summarize(NumOfComplaints=n()) %>% arrange(desc(NumOfComplaints))
## Observation: State Georgia has maximum unresolved complaints(80)       

## 9.Provide the percentage of complaints resolved till date, which were received through theInternet and customer care calls.
total<-data %>% group_by(ComplaintStatus) %>% summarize(NumOfComplaints=n())
total
slices<-total$NumOfComplaints
pct<-round((slices/sum(slices)*100),2)
lbls<-paste(total$ComplaintStatus," ",pct,"%",sep="")

#Plotting pie chart
pie(slices,labels=lbls)
## Observation: There are total 76.75% Complaints resolved.

int<-data %>% filter(ReceivedVia=='Internet',ComplaintStatus=='Closed') %>% group_by(ReceivedVia,ComplaintStatus) %>% summarize(NumOfComplaints=n()) 
ccc<-data %>% filter(ReceivedVia=='Customer Care Call',ComplaintStatus=='Closed') %>% group_by(ReceivedVia,ComplaintStatus) %>% summarize(NumOfComplaints=n()) 

#Percentage of resolved internet Complaints
internetcom<-round(int$NumOfComplaints/sum(total$NumOfComplaints)*100,2)
internetcom
## Observation: There are 37.9% resolved internet Complaints

## Percentage of resolved Customer Care Call Complaints
custcalcom<-round(ccc$NumOfComplaints/sum(total$NumOfComplaints)*100,2)
custcalcom
## Observation: There are 38.85% resolved Customer Care Call Complaints

## Observation: Out of 76.75% resolved Complaints, 37.9% complaints are Internet type while 38.85% are Customer Care Call type.

