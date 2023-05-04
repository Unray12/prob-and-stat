library("ggplot2")
library("dplyr")
library("car")
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
#DF<-DF[DF$Vendor != "AMD",]
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

#summary
summary(DF)

#split to 2 group Intel and AMD
intel<-DF[DF$Vendor == "Intel",]
head(intel)
AMD<-DF[DF$Vendor == "AMD",]
head(AMD)
#Q Q plot
par(mfrow=c(1,3))
qqnorm(DF$Die_size, main="Die_size")
qqline(DF$Die_size, col=6)

qqnorm(DF$Frequency, main="Frequency")
qqline(DF$Frequency, col=6)


qqnorm(DF$Process_size, main="Process_size")
qqline(DF$Process_size, col=6)

par(mfrow=c(1,2))

qqnorm(DF$TDP, main="TDP")
qqline(DF$TDP, col=6)

qqnorm(DF$Num_of_transistors, main="Num_of_transistors")
qqline(DF$Num_of_transistors, col=6)

par()
#test normality
p_test<-shapiro.test(DF$TDP)
p_test
p_test<-p_test$p.value
alpla<-0.05
is_normal<-p_test > alpla
is_normal

# test homogeneity of variance
leveneTest(DF$Process_size ~ factor(DF$Vendor))
leveneTest(DF$Frequency ~ factor(DF$Vendor))

#plotting distribution of data
Process_size.plot<-ggplot(data = DF, aes(x = Process_size)) + geom_histogram(aes(y = ..density..), color="black", fill="lightblue", binwidth=10) + geom_density(color="red") + xlab("Process size") + ylab("Frequency")
Process_size.plot

Frequency.plot<-ggplot(data = DF, aes(x = Frequency)) + geom_histogram(aes(y = ..density..), color="black", fill="lightblue", binwidth=100) + geom_density(color="red") + xlab("Frequency of CPU") + ylab("Frequency")
Frequency.plot

num_of_trans.plot<-ggplot(data = DF, aes(x = Num_of_transistors)) + geom_histogram(aes(y = ..density..), color="black", fill="lightblue", binwidth=500) + geom_density(color="red") + xlab("Number of transistors") + ylab("Frequency")
num_of_trans.plot


#Boxplot
table(DF$Vendor)

boxplot(DF$Frequency ~ DF$Vendor, data=DF, main="Frequency in CPU of each vendors", xlab="Vendors", ylab="Frequency of CPU")


boxplot(DF$Num_of_transistors ~ DF$Vendor, data=DF, main="", xlab="Vendors", ylab="Number of transistor in one CPU")


boxplot(DF$Process_size ~ DF$Vendor, data=DF, main="", xlab="Vendors", ylab="Process size of each CPU")

########## linear regression ##########
#split data
train_data<-DF %>% slice(1:ceiling(nrow(DF) * 0.8)) #80% first to train
train_data<-train_data[,c("Date_release", "Process_size", "Num_of_transistors", "Frequency")]
test_data<-dplyr::anti_join(DF, train_data, by = "Date_release") #20% to test
test_data<-test_data[,c("Date_release", "Process_size", "Num_of_transistors", "Frequency")]
head(train_data, 20)
head(test_data)

#scale data
#train_data$Num_of_transistors<-train_data$Num_of_transistors * 10^6
#train_data$Frequency<-train_data$Frequency * 10^6
head(train_data)

#training model
model<-lm(Frequency ~ Num_of_transistors, data = train_data)
summary(model)
residual<-resid(model)
residual_dis.plot<-ggplot(data = model, aes(x = residual)) + geom_histogram(aes(y = ..density..), color="black", fill="lightblue", binwidth=150) + geom_density(color="red") + xlab("Residual") + ylab("Frequency")
residual_dis.plot

#predict
predict_pro_size<-predict(model, newdata = test_data)
test_data$predictions<-predict_pro_size
#test_data$predictions<-test_data$predictions * (10^-6)
head(test_data, 100)
#plot prediction and reality
plot(test_data$Frequency, type="l", col="blue", xlab="X-label", ylab="Frequency")
plot(model, which = 2)

########## ANOVA ONE-WAY ##########
anova_model<-aov(Frequency ~ Vendor, data=DF) 
summary(anova_model)

#set significance
alpla<-0.05

#check P-values
p_values<-summary(anova_model)[[1]][,5]
p_values

#Check which p-values are significant
significant_p<-p_values < alpla
significant_p #true or false
