library("ggplot2")
library("dplyr")
setwd("C:/Users/PCPV/Desktop/prob and stst project")
DF<-read.csv("chip_dataset.csv")
DF<-as.data.frame(DF) # format tha data into a table
head(DF)

DF<-DF[, -c(1, 10, 12, 13, 14)] #delete unused column
head(DF)

#rename
names(DF)<-c("Product", "Type", "Date_release", "Process_size", "TDP", "Die_size", "Num_of_transistors", "Frequency", "Vendor")
head(DF)

#delete unused info
DF<-DF[DF$Type != "GPU",]
DF<-DF[DF$Date_release != "NaT",]

head(DF)
#set data type
DF[, c("Process_size", "TDP", "Die_size", "Num_of_transistors", "Frequency")]<-lapply(DF[, c("Process_size", "TDP", "Die_size", "Num_of_transistors", "Frequency")], as.numeric)
DF$Date_release <- as.Date(DF$Date_release) #yyyy-mm-dd
head(DF)

#sorted by date ascending
DF<-DF %>% arrange(Date_release)
head(DF, 10)

#cleaning
DF<-DF[complete.cases(DF),] #clear NA info
sum(is.na(DF)) # for check

duplicates<-duplicated(DF)
print(duplicates)
DF<-unique(DF) #remove dup
head(DF)

interaction.plot<-ggplot(data = DF, aes(x = Process_size)) + geom_histogram(aes(y = ..density..), color="black", fill="lightblue", binwidth=100)+geom_density(color="red")+xlab("Total Interaction")+ylab("Frequency")
#interaction.plot
summary(DF)
hist(DF$Process_size, xlab = "million", main = "Histogram of process size")

#linear regression
set.seed(1)



