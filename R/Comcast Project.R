#Analysis of Comcast's Customer Service

#Importing data into R
telecom <- read.csv("Comcast Telecom Complaints data.csv", header = TRUE)
#viwing the first five rows of data
head(telecom,5)
#displaying internal structure of R object
str(telecom)
#diaplaying column names of dataframe
colnames(telecom)
#finding out the dimensions of dataframe
dim(telecom)



#importing dplyr package
library(dplyr)



#Finding out the unique values for states
unique(telecom$State)

#Since there are variations in the string 'District of Columbia', using gsub to provide uniformity
telecom$State<-sub("District Of Columbia", "District of Columbia", telecom$State)
unique(telecom$State)

#Importing lubridate package 
library(lubridate)

#Maintaining uniformity in Date format
telecom$DateUni <- as.Date(parse_date_time(telecom$Date,"dmy"))
View(telecom)

#Creating new columns and storing extracted year, month and day data
telecom$Year <- format(as.Date(telecom$DateUni, "%Y-%m-%d"), "%Y")
telecom$Month <- format(as.Date(telecom$DateUni, "%Y-%m-%d"), "%b")
telecom$Day <- format(as.Date(telecom$DateUni, "%Y-%m-%d"), "%d")
telecom$DayMonth <- format(as.Date(telecom$DateUni, "%Y-%m-%d"), "%d/%b")
View(telecom)

#Monthwise Complaints
MonthwiseComplaints <- summarise(group_by(telecom,Month), num_of_complaints=n())
print(MonthwiseComplaints)


#Importing ggplot library
library("ggplot2")

MonthwiseComplaints$Month <- factor(MonthwiseComplaints$Month, levels = c('Jan','Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec'))

ggplot(data=MonthwiseComplaints,aes(x=Month,y=num_of_complaints, group=1))+
  geom_line(color='blue')+geom_point()+
  geom_text(label=MonthwiseComplaints$num_of_complaints,nudge_x=0.5,nudge_y=0.5,check_overlap=TRUE)+
  labs(title="Trend chart for complaints at monthly level", x='Months', y='Number of Complaints')


#June month has the maximum numberof complaints(1046) followed by April and May


#Daywise Complaints
DaywiseComplaints <- summarise(group_by(telecom,DateUni), num_of_complaints=n())
DaywiseComplaints
DayMonthComplaints <- summarise(group_by(telecom,Year, Month, Day), num_of_complaints=n())
DayMonthComplaints

#Daywise trend chart
ggplot(data=DaywiseComplaints,aes(x=DateUni,y=num_of_complaints, group=1))+
  geom_line(color='green')+geom_point()+
  scale_x_date(date_breaks = "1 month", date_minor_breaks = "1 week",date_labels = "%d/%m")+
  labs(title="Trend chart for complaints at daily level", x='Date', y='Number of Complaints')

DaywiseComplaints[which.max(DaywiseComplaints$DateUni),]

#The fourth week of June 2015 has the maximum number of complaints compared to any other months.


#Importing necessary libraries to create wordcloud
library(wordcloud)
library("slam")
library(tm)

# To extract words from the text data, remove punctuations, white spaces, numbers and converting text to lowercase
com_text <- Corpus(VectorSource(telecom$Customer.Complaint))


com_text_clean <- tm_map(com_text, removePunctuation)

com_text_clean <- tm_map(com_text_clean, content_transformer(tolower))
com_text_clean <- tm_map(com_text_clean, removeNumbers)
com_text_clean <- tm_map(com_text_clean, stripWhitespace)

#Removing stopwords in English as well as the word 'comcast' as it may influence the result
head(stopwords("english"), n = 25)
com_text_clean <- tm_map(com_text_clean, removeWords, c('comcast',stopwords('english')))

#Creating the wordcloud
wordcloud(com_text_clean, scale = c(2, 1),min.freq= 25,colors = rainbow(30))

#To create a dataframe of words and their frequency
doc <- TermDocumentMatrix(com_text_clean)
mat_doc <- as.matrix(doc)
sorted_mat <- sort(rowSums(mat_doc),decreasing=TRUE)

#Table with the frequency of complaint types
table_freq <- data.frame(word= names(sorted_mat), freq = sorted_mat)
View(table_freq)

#Creating the wordcloud based on the word-frequency dataframe
par(mar=c(5,5,5,5))
wordcloud(words=table_freq$word, freq=table_freq$freq, min.freq = 1, max.words=30,scale=c(3,0.5),
          random.order=FALSE,rot.per=0.35,colors=brewer.pal(8,"Dark2"))


#Creating a barplot to view the most frequently used word
par(mar=c(5,5,5,5))
barplot(table_freq[1:10,]$freq, las = 1, names.arg = table_freq[1:10,]$word,
        col ="lightblue", main ="Most frequent words",
        ylab = "Word frequencies", las=2)

#The result shows that 'Internet Service' issues are the most frequent followed by 'billing' issues.


#Creating new categorical variable 'Open' and 'Closed'
unique(telecom$Status)
OpenComplaints <- (telecom$Status == 'Open' | telecom$Status == 'Pending')
ClosedComplaints <- (telecom$Status == 'Closed' | telecom$Status == 'Solved')
telecom$Cust_ComplaintStatus[OpenComplaints] <- 'Open'
telecom$Cust_ComplaintStatus[ClosedComplaints] <- 'Closed'
View(telecom)


#State having maximum complaints
StatewiseComplaintStatus <- summarise(group_by(telecom,State,Cust_ComplaintStatus), num_of_complaints=n())
StatewiseComplaintStatus[which.max(StatewiseComplaintStatus$num_of_complaints),]
View(StatewiseComplaintStatus)
StatewiseComplaintStatus[which.max(StatewiseComplaintStatus$num_of_complaints),]

#Stacked Barplot visualisation for Statewise Status of Complaints
par(mar=c(5,5,5,5))
ggplot(data=StatewiseComplaintStatus, aes(x=State, y=num_of_complaints,
                                          fill=Cust_ComplaintStatus))+
  geom_bar(stat='identity')+coord_flip()


#State having maximum complaints

StatewiseComplaints <- summarise(group_by(telecom,State), num_of_complaints=n())
StatewiseComplaints[which.max(StatewiseComplaints$num_of_complaints),]

#Georgia state is having a maximum of 288 complaints


#Barplot visualisation
par(mar=c(5,5,5,5))

plot_state=barplot(StatewiseComplaints$num_of_complaints, col=5:10, main='Statewise Complaints',
                   xlab="State", ylab="No. of Complaints", names.arg=StatewiseComplaints$State, las=2)


Max_complaints=arrange(summarise(group_by(telecom,State),No_of_complaints=n()),desc(No_of_complaints))
Max_complaints[1,]

Max_open_complaints=arrange(summarise(group_by(filter(telecom,Cust_ComplaintStatus == 'Open' ),State),No_of_complaints=n()),desc(No_of_complaints))
Max_open_complaints[1,]

Max_closed_complaints=arrange(summarise(group_by(filter(telecom,Cust_ComplaintStatus == 'Closed' ),State),No_of_complaints=n()),desc(No_of_complaints))
Max_closed_complaints[1,]

#State with highest percentage of unresolved Complaints
State_count <-merge(x=Max_complaints, y=Max_open_complaints, by='State', all.x=TRUE)
State_count <- mutate(State_count, Percent_unresolved = (No_of_complaints.y/No_of_complaints.x)*100)
State_count

#Barplot visualisation
par(mar=c(5,5,5,5))

plot_state=barplot(State_count$Percent_unresolved, col=2:10, main='Statewise unresolved Complaints',
                   xlab="State", ylab="Percent of unresolved Complaints", names.arg=State_count$State, las=2)


State_count[which.max(State_count$Percent_unresolved),]

#Kansas state have the highest percentage (50%) of unresolved complaints


#Percentage of complaints resolved (i.e. Closed Complaints) till date through internet and customer care calls
#Complaints received Via
unique(telecom$Received.Via)

StatusvsReceived <-table( telecom$Received.Via, telecom$Cust_ComplaintStatus)

StatusvsReceived <- cbind(StatusvsReceived, TotalbyReceived=rowSums(StatusvsReceived))

StatusvsReceived <- rbind(StatusvsReceived, TotalbyStatus=colSums(StatusvsReceived))

label_type=c('Customer Care Call','Internet','Totalbytype')
StatusvsReceived <- as.data.frame.matrix(StatusvsReceived,row.names=label_type) 
StatusvsReceived <- cbind(Receivedvia=label_type, StatusvsReceived)

StatusvsReceived <- mutate( StatusvsReceived , Percentbyclosed = (Closed/TotalbyReceived)*100, PercentbyOpen = (Open/TotalbyReceived)*100 )                            

View(StatusvsReceived)
StatusvsReceived

#77% of complaints are resolved through Customer care calls and 76% of complaints are resolved through Internet
