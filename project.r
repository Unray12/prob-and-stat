library("ggplot2")
library("dplyr")
library("car")
library("gridExtra") #for multi ggplot

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

#DF<-DF[DF$Frequency < 4500,]
DF<-DF[DF$Frequency > 1000,]

#add density of num
DF$Density_of_transistor<-((DF$Num_of_transistors * 10 ^6)/ (DF$Die_size))
head(DF)

#summary
summary(DF)

#split to 2 group Intel and AMD
intel<-DF[DF$Vendor == "Intel",]
# head(intel)
AMD<-DF[DF$Vendor == "AMD",]
# head(AMD)
#Q Q plot
par(mfrow=c(1,1))
# qqnorm(DF$Die_size, main="Die_size")
# qqline(DF$Die_size, col=6)

qqnorm(DF$Frequency, main="Frequency")
qqline(DF$Frequency, col=6)


# qqnorm(DF$Process_size, main="Process_size")
# qqline(DF$Process_size, col=6)

par(mfrow=c(1,2))

# qqnorm(DF$TDP, main="TDP")
# qqline(DF$TDP, col=6)

# qqnorm(DF$Num_of_transistors, main="Num_of_transistors")
# qqline(DF$Num_of_transistors, col=6)



#Boxplot
table(DF$Vendor)
par(mfrow=c(3, 2))
boxplot(DF$Frequency ~ DF$Vendor, data=DF, main="Frequency in CPU of each vendor", xlab="Vendors", ylab="Frequency of CPU")
boxplot(DF$Die_size ~ DF$Vendor, data=DF, main="Die size in CPU of each vendor", xlab="Vendors", ylab="Die size of CPU")
boxplot(DF$Process_size ~ DF$Vendor, data=DF, main="Process size in CPU of each vendor", xlab="Vendors", ylab="process size of CPU")
boxplot(DF$Num_of_transistors ~ DF$Vendor, data=DF, main="Number of transistor in CPU of each vendor", xlab="Vendors", ylab="Transistor of CPU")
boxplot(DF$TDP ~ DF$Vendor, data=DF, main="TDP in CPU of each vendor", xlab="Vendors", ylab="TDP of CPU")

#QQ plot 2 vendors
par(mfrow=c(1, 2), main = "Q-Q plot of frequency of 2 vendors")

qqnorm(intel$Frequency, main = "Normal Q-Q plot of intel frequency")
qqline(intel$Frequency, col=6)

qqnorm(AMD$Frequency, main = "Normal Q-Q plot of AMD frequency")
qqline(AMD$Frequency, col=6)

#distribution of freq of each vendor
AMD.plot<-ggplot(data = AMD, aes(x = Frequency)) + geom_histogram(aes(y = ..density..), color="black", fill="lightblue", binwidth=100) + geom_density(color="red") + xlab("Frequency of AMD") + ylab("Frequency")
AMD.plot

intel.plot<-ggplot(data = intel, aes(x = Frequency)) + geom_histogram(aes(y = ..density..), color="black", fill="lightblue", binwidth=100) + geom_density(color="red") + xlab("Frequency of Intel") + ylab("Frequency")
intel.plot

grid.arrange(AMD.plot, intel.plot, ncol(2), nrow(1))

#scatter of freq ~ multi
par(mfrow=c(3, 2), main = "Scatter plot of frequency by factors")
plot(Frequency ~ Die_size, data = DF)
plot(Frequency ~ TDP, data = DF)
plot(Frequency ~ Num_of_transistors, data = DF)
plot(Frequency ~ Process_size, data = DF)

########## linear regression ##########
#split data
train_data<-DF %>% slice(1:ceiling(nrow(DF) * 0.8)) #80% first to train

test_data<-dplyr::anti_join(DF, train_data, by = "Date_release") #20% to test

head(test_data)
head(train_data)

#training model
model<-lm(Frequency ~ Die_size + Process_size + TDP + Num_of_transistors, data = train_data)
summary(model)
residual<-resid(model)
par(mfrow=c(1, 1))
residual_dis.plot<-ggplot(data = model, aes(x = residual)) + geom_histogram(aes(y = ..density..), color="black", fill="lightblue", binwidth=150) + geom_density(color="red") + xlab("Residual") + ylab("Frequency")
residual_dis.plot

#test homogenity of variance and normal of LR
plot(model, which = 1)
plot(model, which = 2)


#predict
predict_pro_size<-predict(model, newdata = test_data)
test_data$predictions<-predict_pro_size
#test_data$predictions<-test_data$predictions * (10^-6)
head(test_data)

#ggplot
ggplot(data.frame(x=test_data$Density_of_transistor,y=test_data$Frequency), aes(x=x,y=y))+geom_point(color="blue") + geom_line(data=data.frame(x=test_data$Density_of_transistor,y = test_data$predictions), aes(x=x,y=y), color="red")+labs(x="Transistor density",y="Frequency of CPU", title = "reality vs prediction")
#plot prediction and reality
plot(test_data$Num_of_transistors, test_data$Frequency, type='l', xlab="Transistor density",y="Frequency of CPU", main="Compare prediction with reality", col="red")
lines(test_data$Density_of_transistor, test_data$predictions, col="blue")
plot(model, which = 2)


########## POLYNOMIAL LINEAR REGRESSION ##########
die_sizePoly<- poly(train_data$Die_size, 2)
num_transPoly<- poly(train_data$Num_of_transistors, 2)
process_sizePoly<- poly(train_data$Process_size, 2)
TDPPoly<- poly(train_data$TDP, 2)

head(die_sizePoly)

polyModel<-lm(Frequency ~ poly(Die_size, 1) + poly(Num_of_transistors, 2) + poly(Process_size, 2) + poly(TDP, 3) + poly(Num_of_transistors*Process_size, 2) + poly(Num_of_transistors*Die_size, 2), data = train_data)
summary(polyModel)


par(mfrow=c(1, 1))
#////////////////// test for anova //////////////////#
#test normality
p_test<-shapiro.test(DF$Frequency)
p_test
p_test<-p_test$p.value
alpla<-0.05
is_normal<-p_test > alpla
is_normal

#plotting distribution of data
# Process_size.plot<-ggplot(data = DF, aes(x = Process_size)) + geom_histogram(aes(y = ..density..), color="black", fill="lightblue", binwidth=15) + geom_density(color="red") + xlab("Process size") + ylab("Frequency")
# Process_size.plot 

Frequency.plot<-ggplot(data = DF, aes(x = Frequency)) + geom_histogram(aes(y = ..density..), color="black", fill="lightblue", binwidth=100) + geom_density(color="red") + xlab("Frequency of CPU") + ylab("Frequency")
Frequency.plot # Normal --> can use anova

# num_of_trans.plot<-ggplot(data = DF, aes(x = Num_of_transistors)) + geom_histogram(aes(y = ..density..), color="black", fill="lightblue", binwidth=800) + geom_density(color="red") + xlab("Number of transistors") + ylab("Frequency")
# num_of_trans.plot
#//////////////////////////////////////////////////#



########## ANOVA FOR FREQ ~ VENDORS ##########

#test for homogenity of variance#
leveneTest(DF$Frequency ~ factor(DF$Vendor))
anova_model<-aov(Frequency ~ Category, data=DF) 
summary(anova_model)

#set significance
alpla<-0.05

#check P-values
p_values<-summary(anova_model)[[1]][,5]
p_values

#Check which p-values are significant
significant_p<-p_values < alpla
significant_p #true or false
