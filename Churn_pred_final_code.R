######### IST 687 ##########
######### M004 #############
######## GROUP 1 ###########



############## LOADING LIBRARIES ###################
library(dplyr)
library(tidyverse)
library(Hmisc)
library(corrplot)
library(rjson)
library(RCurl)
library(jsonlite)
library(polycor)
library(data.table)
library(kernlab)
library(e1071)
library(caret)
library(ggplot2)
library(arules)
library(arulesViz)
library(ggpubr)
library(randomForest)
library(caTools)
library(arules)
library(arulesViz)


# Clear the graph window
dev.off()

# Clear the console
cat('\014')

# Clear all user objects from the environment
rm(list=ls())

# Setting the current working directory
setwd("E:/Grad Info/Fall Sem 19/IST 687/LAB/Project")

########## LOADING DATASET ################
df <- jsonlite::fromJSON("fall2019-survey-M04.json")

View(df)

############################ DATA MUNGING ####################################

# Replacing NA's of Flight time with values which have data for same places
df$Flight.time.in.minutes[14] <- 100
df$Flight.time.in.minutes[124] <- 130
df$Flight.time.in.minutes[359] <- 38
df$Flight.time.in.minutes[1167] <- 112
df$Flight.time.in.minutes[1493] <- 144
df$Flight.time.in.minutes[1541] <- 110
df$Flight.time.in.minutes[1918] <- 68
df$Flight.time.in.minutes[2302] <- 112
df$Flight.time.in.minutes[2672] <- 125
df$Flight.time.in.minutes[3559] <- 115
df$Flight.time.in.minutes[4303] <- 115
df$Flight.time.in.minutes[8431] <- 115
df$Flight.time.in.minutes[5540] <- 106
df$Flight.time.in.minutes[6170] <- 180
df$Flight.time.in.minutes[6718] <- 275
df$Flight.time.in.minutes[7328] <- 115
df$Flight.time.in.minutes[7500] <- 65
df$Flight.time.in.minutes[7864] <- 267
df$Flight.time.in.minutes[7881] <- 118
df$Flight.time.in.minutes[8119] <- 57
df$Flight.time.in.minutes[9208] <- 208
df$Flight.time.in.minutes[9235] <- 63

# Replacing NA's of Arrival delay time with values which have data for same places
df$Arrival.Delay.in.Minutes[(is.na(df$Arrival.Delay.in.Minutes) & df$Flight.cancelled=='No')] <- df$Departure.Delay.in.Minutes[(is.na(df$Arrival.Delay.in.Minutes) & df$Flight.cancelled=='No')]

# Removing all the Records where the Flight Cancelled is "Yes"
df <- df[df$Flight.cancelled=='No',]

# Converting the Likelihood to recommend to "Detractor,Passive and Promoter"

for (i in 1:length(df$Likelihood.to.recommend))
  {
    if (df$Likelihood.to.recommend[i] < 7) 
      {
        df$pr_pa_dt[i] <- "Detractor"
      } 
    else if (df$Likelihood.to.recommend[i] >= 7 & df$Likelihood.to.recommend[i] <= 8)
      {
        df$pr_pa_dt[i] <- "Passive"
      }
    else 
      {
        df$pr_pa_dt[i] <- "Promoter"
      }
  }

# Converting the Arrival.Delay.in.Minutes if Delay is greater than 5 mins to "Yes" or "No"

for (i in 1:length(df$Arrival.Delay.in.Minutes))
{
  if (df$Arrival.Delay.in.Minutes[i] > 5) 
  {
    df$ArrivalDelayof5min[i] <- "Yes"
  } 
  else 
  {
    df$ArrivalDelayof5min[i] <- "No"
  }
}

# Converting the Departure.Delay.in.Minutes if Delay is greater than 5 mins to "Yes" or "No"

for (i in 1:length(df$Departure.Delay.in.Minutes))
{
  if (df$Departure.Delay.in.Minutes[i] > 5) 
  {
    df$DepartDelayof5min[i] <- "Yes"
  } 
  else 
  {
    df$DepartDelayof5min[i] <- "No"
  }
}


# Coverting the columns to factors

converttostring <- function(vec)
{
  vec <- trimws(as.character(vec))
  vec <- as.factor(vec)
}

# Converting to Factors

df$Airline.Status <- converttostring(df$Airline.Status)
df$Gender <- converttostring(df$Gender)
df$Type.of.Travel <- converttostring(df$Type.of.Travel)
df$Class <- converttostring(df$Class)
df$Partner.Name <- converttostring(df$Partner.Name)
df$Flight.cancelled <- converttostring(df$Flight.cancelled)
df$Price.Sensitivity <- converttostring(df$Price.Sensitivity)
df$ArrivalDelayof5min <- converttostring(df$ArrivalDelayof5min)
df$DepartDelayof5min <- converttostring(df$DepartDelayof5min)
df$pr_pa_dt <- converttostring(df$pr_pa_dt)
df$Origin.State <- converttostring(df$Origin.State)
df$Destination.State <- converttostring (df$Destination.State)

colnames(df)

df1 <- df[,c(-1,-2,-15,-16,-17,-28:-32)] # 19,20

dfviz <- df1

################################# DATA VISUALIZATION #############################

dfviz <- df1

################# How age affecting the Likelihood to recommend? ################# 

dfviz$ageGroup[dfviz$Age < 30] <- "29 or younger"
dfviz$ageGroup[dfviz$Age > 29 & dfviz$Age < 56] <- "30-55"
dfviz$ageGroup[dfviz$Age > 55] <- "56 or above"

# age group vs promoter score
# subsetting
youngAge <- subset(dfviz, dfviz$ageGroup == "29 or younger")
medianAge <- subset(dfviz, dfviz$ageGroup == "30-55")
oldAge <- subset(dfviz, dfviz$ageGroup == "56 or above")

# replicate age group
replicateyoung <- replicate(200, mean(sample(youngAge$Likelihood.to.recommend, size = 10, replace = TRUE)), simplify = TRUE) # replicate the sample mean
replicatemedian <- replicate(200, mean(sample(medianAge$Likelihood.to.recommend, size = 10, replace = TRUE)), simplify = TRUE) # replicate the sample mean
replicateold <- replicate(200, mean(sample(oldAge$Likelihood.to.recommend, size = 10, replace = TRUE)), simplify = TRUE) # replicate the sample mean
# add to a data frame
replicateAge <- data.frame(replicateyoung, replicatemedian, replicateold)

# Histogram for 29 or younger
repliAgeYoungHist <- ggplot(replicateAge) +
  aes(x = replicateAge$replicateyoung) +
  geom_histogram(bins = 10, fill = "white", color = "black") +
  ggtitle("Histogram of 29 or younger") +
  xlab("Likelihood to Recommend")
repliAgeYoungHist

# histogram for 30-55
repliAgeMedHist <- ggplot(replicateAge) +
  aes(x = replicateAge$replicatemedian) +
  geom_histogram(bins = 10, fill = "white", color = "black") +
  ggtitle("Histogram of age group 30-55") +
  xlab("Likelihood to Recommend")
repliAgeMedHist

# histogram for 56 or above
repliAgeOldHist <- ggplot(replicateAge) +
  aes(x = replicateAge$replicateold) +
  geom_histogram(bins = 10, fill = "white", color = "black") +
  ggtitle("Histogram of Age Group 56 or above") +
  xlab("Likelihood to Recommend")
repliAgeOldHist

ggarrange(repliAgeYoungHist, repliAgeMedHist ,repliAgeOldHist ,labels = c("A", "B","C"),nrow = 2,ncol = 2)

# get the median value of each group to estimate the true mean
median(replicateyoung)
median(replicatemedian)
median(replicateold)
# replicatemedian > replicateyoung > replicateold

# age group vs likelihood to recommend
agegroupVsRecommend <- ggplot(dfviz) +
  aes(x = dfviz$pr_pa_dt, fill = dfviz$ageGroup) +
  geom_bar(position = "dodge") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  ggtitle("Age Group VS Likelihood to Recommend") +
  labs(x = "Likelihood to recommend", y = "Count", fill = "Age Group")
agegroupVsRecommend

# population comparison for different age group
popComp <- ggplot(dfviz) +
  aes(x = dfviz$ageGroup, fill = dfviz$ageGroup) +
  geom_bar(position = "dodge") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  ggtitle("Population for different age group") +
  labs(x = "Age Group", y = "Count", fill = "Age Group")
popComp

# score of different age group: boxplot
ageVsScore <- ggplot(dfviz) +
  aes(x = dfviz$ageGroup, y = dfviz$Likelihood.to.recommend, fill = dfviz$ageGroup) +
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  ggtitle("Age Group VS Likelihood to Recommend") +
  labs(x = "Age Group", y = "Likelihood to Recommend", fill = "Age Group")
ageVsScore

# stat summary of each age group
summary(dfviz$Likelihood.to.recommend[dfviz$ageGroup == "29 or younger"])
summary(dfviz$Likelihood.to.recommend[dfviz$ageGroup == "30-55"])
summary(dfviz$Likelihood.to.recommend[dfviz$ageGroup == "56 or above"])

################## How Gender is affecting the Likelihood to recommend?####################

genderVsRecommend <- ggplot(dfviz) +
  aes(x = dfviz$pr_pa_dt, fill = dfviz$Gender) +
  geom_bar(position = "dodge") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  ggtitle("Gender Count VS Detractor, Passive, Promoter") +
  labs(x = "Likelihood to recommend", y = "Count", fill = "Gender")
genderVsRecommend

# Gender vs liklihood to recommend (boxplot)
genderVsScore <- ggplot(dfviz) +
  aes(x = dfviz$Gender, y = dfviz$Likelihood.to.recommend, fill = dfviz$Gender) +
  geom_boxplot(position = "dodge") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  ggtitle("Gender VS Likelihood to Recommend") +
  labs(x = "Gender", y = "Likelihood to Recommend", fill = "Gender")
genderVsScore

ggarrange(genderVsRecommend, genderVsScore ,labels = c("A", "B"),ncol = 2)

# Gender by airline
genderAirline <- ggplot(dfviz) +
  aes(fill = dfviz$Gender,x = dfviz$Partner.Name) +
  geom_bar(position = "dodge") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
  ggtitle("Gender VS Airline") +
  labs(x = "Partner Name", y = "Count", fill = "Gender")
genderAirline

###################### How Arrival Delay is affecting the Likelihood to recommend? ####################

# bar plot for delay vs likelihood to recommend
delayRecommend <- ggplot(dfviz) +
  aes(x=dfviz$ArrivalDelayof5min, fill = dfviz$pr_pa_dt) +
  geom_bar(position = "dodge") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  ggtitle("Arrival Delay vs liklihood to recommend") +
  labs(x = "delay status", y = "Count", fill = "promoter/passive/detractor")
delayRecommend

###################### How does Price Sensitivity affect Likelihood to Recommend? ####################

priceSenVsRecommend <- ggplot(dfviz) +
  aes(x = dfviz$pr_pa_dt, fill = dfviz$Price.Sensitivity) +
  geom_bar(position = "dodge") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  ggtitle("Price Sensitivity VS Detractor, Passive, Promoter") +
  labs(x = "Likelihood to recommend", y = "Count", fill = "Price Sensitivity")
priceSenVsRecommend

# Gender vs liklihood to recommend (boxplot)
priceSenVsScore <- ggplot(dfviz) +
  aes(x = dfviz$Price.Sensitivity, y = dfviz$Likelihood.to.recommend, fill = dfviz$Price.Sensitivity) +
  geom_boxplot(position = "dodge") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  ggtitle("Price Sensitivity VS Likelihood to Recommend") +
  labs(x = "Price Sensitivity", y = "Likelihood to Recommend", fill = "Price Sensitivity")
priceSenVsScore

ggarrange(priceSenVsRecommend, priceSenVsScore ,labels = c("A", "B"),nrow = 2)

ggplot(df1,aes(x=Type.of.Travel)) + geom_histogram(stat="count",color="black",aes(fill=pr_pa_dt))+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


################# How Type of Travel is affecting the Likelihood to recommend? ###############

# type of travel vs likelihood to recommend
# hist
travelTypeRecommend <- ggplot(dfviz) +
  aes(x=dfviz$Type.of.Travel, fill = dfviz$pr_pa_dt) +
  geom_bar(position = "dodge") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  ggtitle("Travel Type vs liklihood to recommend") +
  labs(x = "Travel Type", y = "Count", fill = "promoter/passive/detractor")
travelTypeRecommend

# boxplot
travelTypeRecommendBox <- ggplot(dfviz) +
  aes(x = dfviz$Type.of.Travel, y = dfviz$Likelihood.to.recommend, fill = dfviz$Type.of.Travel) +
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  ggtitle("Type of Travel VS Likelihood to Recommend") +
  labs(x = "Type of Travel", y = "Likelihood to Recommend", fill = "Type of Travel")
travelTypeRecommendBox

ggarrange(travelTypeRecommend, travelTypeRecommendBox ,labels = c("A","B"),nrow = 2)


################# How Airline Status is affecting the Likelihood to recommend? ###############

# airline status vs likelihood to recommend
# hist
airStatusRecommend <- ggplot(dfviz) +
  aes(x=dfviz$Airline.Status, fill = dfviz$pr_pa_dt) +
  geom_bar(position = "dodge") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  ggtitle("Airline Status vs liklihood to recommend") +
  labs(x = "Airline Status", y = "Count", fill = "promoter/passive/detractor")
airStatusRecommend

#boxplot
airStatusRecommendBox <- ggplot(dfviz) +
  aes(x = dfviz$Airline.Status, y = dfviz$Likelihood.to.recommend, fill = dfviz$Airline.Status) +
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  ggtitle("Airline Status VS Likelihood to Recommend") +
  labs(x = "Airline Status", y = "Likelihood to Recommend", fill = "Airline Status")
airStatusRecommendBox

ggarrange(airStatusRecommend, airStatusRecommendBox ,labels = c("A","B"),nrow = 2)


# Nps Graph

count <- table(df1$Partner.Name,df1$pr_pa_dt)
nps_airline <- data.frame(Airlines = unique(rownames(count)),Detractor = count[,1],Passive = count[,2],Promoters = count[,3])
nps_airline$Nps <- ((nps_airline$Promoters - nps_airline$Detractor)/(nps_airline$Promoters + nps_airline$Detractor +
                                                                       nps_airline$Passive))*100
ggplot(nps_airline,aes(x=reorder(Airlines,Nps),y=Nps)) + geom_col(aes(fill=Nps))+ theme(axis.text.x = element_text(angle = 90, hjust = 1))


# Northwest business airlines NPS
View(count)
northwest_bus_air <- df[str_trim(df$Partner.Name)=="Northwest Business Airlines Inc.",]

# Plotting the data for gender and likelihood.to.recommend
ggplot(northwest_bus_air,aes(x=Gender)) + geom_histogram(stat="count",color="black",aes(fill=pr_pa_dt))


# Plotting the data for type.of.travel and likelihood.to.recommend
ggplot(northwest_bus_air,aes(x=Type.of.Travel)) + geom_histogram(stat="count",color="black",aes(fill=pr_pa_dt))


northwest_bus_air.nps <- group_by(northwest_bus_air,pr_pa_dt)
a.northwest_bus_air <- summarise(northwest_bus_air.nps,count=n())
northwest_nps <- (a.northwest_bus_air$count[3]-a.northwest_bus_air$count[1])*100/sum(a.northwest_bus_air$count)


# Going North airlines NPS

going_north_air <- df[str_trim(df$Partner.Name)=="GoingNorth Airlines Inc.",]

going_north_air.nps <- group_by(going_north_air,pr_pa_dt)
a.going_north_air <- summarise(going_north_air.nps,count=n())
goingnorth_nps <- (a.going_north_air$count[3]-a.going_north_air$count[1])*100/sum(a.going_north_air$count)


########################### FINDING Significant Variables #####################################

#-------------------------------------Correlation Matrix---------------------------------

library(data.table)
corr <- df1 %>% sapply(., as.numeric) %>% as.data.table()
corr <- cor(corr, use = 'pairwise.complete.obs')
corr[upper.tri(corr)] <- NA
corr <- melt(corr, na.rm = T) %>% as.data.table() %>% setorder(-value)
corr$text <- ifelse(abs(corr$value) >= .8 & corr$value != 1, round(corr$value, 2), '')

ggplot(data = corr, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile(color = 'white') +
  geom_text(aes(label = text)) +
  scale_fill_gradient2(low = 'blue', high = 'red', mid = 'white',
                       midpoint = 0, limit = c(-1, 1),
                       name = 'Pearson Correlation') +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = 'Correlation Matrix')

########################### APPLYING MODELS #############################

#-----------------------------------Linear Modelling--------------------------------------

df1_Linear <- lm(formula = Likelihood.to.recommend~Airline.Status+Gender+Loyalty+Eating.and.Drinking.at.Airport+
                 Flight.Distance+Age+Price.Sensitivity+Flights.Per.Year+Type.of.Travel+Origin.State+Destination.State+
                   Class+ArrivalDelayof5min,data = df1)

summary(df1_Linear)


# Northwest business airlines Linear Model

northwest_bus_air1 <- df1[str_trim(df$Partner.Name)=="Northwest Business Airlines Inc.",]

df1_Linear_north <- lm(formula = Likelihood.to.recommend~Airline.Status+Gender+Loyalty+Eating.and.Drinking.at.Airport+
                         Flight.Distance+Type.of.Travel+Age+Price.Sensitivity+Origin.State+Destination.State+
                         Flights.Per.Year+Class+ArrivalDelayof5min,data = northwest_bus_air1)
summary(df1_Linear_north)

# FlyFast Airways Linear Model

flyfast_air1 <- df1[str_trim(df$Partner.Name)=="FlyFast Airways Inc." & df$Flight.cancelled=='No',]

df1_Linear_flyfast <- lm(formula = Likelihood.to.recommend~Airline.Status+Gender+Loyalty+Eating.and.Drinking.at.Airport+
                         Flight.Distance+Type.of.Travel+Age+Price.Sensitivity+Flights.Per.Year+Class+Origin.State+
                          Destination.State+ArrivalDelayof5min,data = flyfast_air1)

summary(df1_Linear_flyfast)

########################### SUPPORT VECTOR MACHINE ##################################

#-----------------------------SVM for entire --------------

data2 <- df

# creating 3 levels for age
for (i in 1:length(data2$Age))
{
  if (data2$Age[i] >=15 & data2$Age[i] <= 29) 
  {
    data2$Age_group[i] <- "Age between 15 and 29"
  } 
  else if (data2$Age[i] >=30 & data2$Age[i] <= 54)
  {
    data2$Age_group[i] <- "Age between 30 and 54"
  }
  else 
  {
    data2$Age_group[i] <- "Age above 54"
  }
}
data2$Age_group <- as.factor(data2$Age_group)
# removing unwanted columns
data2 <- data2[,c(-1,-2,-4,-7,-15,-16,-17,-18,-21,-22,-23,-24,-25,-27,-28,-29,-30,-31,-32)]
data2 <- data2[,-8]
data2 <- data2[,-15]
colnames(data2)

# creating random sample for training and test dataset
randIndex1 <- sample(1:dim(data2)[1])
cutPoint2_3 <- floor(2 * dim(data2)[1]/3)
trainData_1 <- data2[randIndex1[1:cutPoint2_3],] 
testData_1 <- data2[randIndex1[(cutPoint2_3+1):dim(data2)[1]],]

# running support vector machine
svmOutput_1 <- ksvm(pr_pa_dt ~.,data=trainData_1,kernel="rbfdot",kpar="automatic",scale=FALSE,C = 5,cross = 3,prob.model = TRUE)
svmOutput_1

#performing prediction
svmPred_1 <- predict(svmOutput_1, testData_1)

# for accuracy
confusionMatrix(svmPred_1,testData_1$pr_pa_dt)


#-----------------------------------SVM for FLY FAST-----------------

flyfast <- data2[str_trim(df$Partner.Name)=="FlyFast Airways Inc." & df$Flight.cancelled=='No',]
randIndex2 <- sample(1:dim(flyfast)[1])
cutPoint2_3_2 <- floor(2 * dim(flyfast)[1]/3)
trainData_2 <- flyfast[randIndex2[1:cutPoint2_3_2],] 
testData_2 <- flyfast[randIndex2[(cutPoint2_3_2+1):dim(flyfast)[1]],]

# running support vector machine
svmOutput_2 <- ksvm(pr_pa_dt ~.,data=trainData_2,kernel="rbfdot",kpar="automatic",C = 5,cross = 3,prob.model = TRUE)
svmOutput_2

# performing prediction
svmPred_2 <- predict(svmOutput_2, testData_2)

# to get accuracy
confusionMatrix(svmPred_2,testData_2$pr_pa_dt)

#---------------------------------SVM for Northwest-----------------

northwest_bus <- data2[str_trim(df$Partner.Name)=="Northwest Business Airlines Inc."& df$Flight.cancelled=='No',]
randIndex3 <- sample(1:dim(northwest_bus)[1])
cutPoint2_3_3 <- floor(2 * dim(northwest_bus)[1]/3)
trainData_3 <- northwest_bus[randIndex3[1:cutPoint2_3_3],] 
testData_3 <- northwest_bus[randIndex3[(cutPoint2_3_3+1):dim(northwest_bus)[1]],]

# running support vector machine
svmOutput_3 <- ksvm(pr_pa_dt ~.,data=trainData_3,kernel="rbfdot",kpar="automatic",C = 5,cross = 3,prob.model = TRUE)
svmOutput_3

svmPred_3 <- predict(svmOutput_3, testData_3)
svmPred_3

# to get accuracy
confusionMatrix(svmPred_3,testData_3$pr_pa_dt)

####################### RANDOM FOREST ######################

#---------------------Randomforest for entire-----------------
# using randomforest and cat tools package to apply random forest model.
rf1<-randomForest(pr_pa_dt ~.,data=trainData_1)
rf1
predrf1 <- predict(rf1, testData_1)
predrf1
cm1 <- table(testData_1$pr_pa_dt,predrf1)
cm1
confusionMatrix(predrf1,testData_1$pr_pa_dt)

#---------------------randomforest for flyfast-----------------
rf2<-randomForest(pr_pa_dt ~.,data=trainData_2)
rf2
predrf2 <- predict(rf2, testData_2)
predrf2
cm2 <- table(testData_2$pr_pa_dt,predrf2)
cm2
confusionMatrix(predrf2,testData_2$pr_pa_dt)

#---------------------randomforest for northwest-----------------
rf3<-randomForest(pr_pa_dt ~.,data=trainData_3)
rf3
predrf3 <- predict(rf1, testData_3)
predrf3
cm3 <- table(testData_3$pr_pa_dt,predrf3)
cm3
confusionMatrix(predrf3,testData_3$pr_pa_dt)


###########Association Rules###############



#############For entire dataset###########

df7<-data2

#creating a separate column in the dataset which would contain the promoters, detractors and passive based on the likelihood to recommend score



#Creating a function where the values in that column below the 45th percentile are classified as low and above the 55th percentile as high.
creategroups<-function(vet)
{
  parts=quantile(vet,c(0.45,0.55))
  groupsp<-rep("Average",length(vet))
  groupsp[vet<=parts[1]]<-"Low"
  groupsp[vet>=parts[2]]<-"High"
  return(as.factor(groupsp))
}

#classifying the values in the loyalty, eating.and.Drinking.at.Airport, Flight.Distance, Total.Freq.Flyer.Accts, Price.Senitivity and Flights.Per.Year columns into low or high using the creategroups function

df7$Loyalty<-creategroups(df7$Loyalty)
df7$Eating.and.Drinking.at.Airport<-creategroups(df7$Eating.and.Drinking.at.Airport)
df7$Flight.Distance<-creategroups(df7$Flight.Distance)
df7$Total.Freq.Flyer.Accts<-creategroups(df7$Total.Freq.Flyer.Accts)
df7$Price.Sensitivity<-creategroups(df7$Price.Sensitivity)
df7$Flights.Per.Year<-creategroups(df7$Flights.Per.Year)

df7<-mutate_all(df7,as.factor) #converting the variables in the dataframe into factors

df4_x<-as(df7,"transactions") #converting the dataframe into transactions matrix

summary(df4_x)

inspect(df4_x) #inspecting the transactions matrix

itemFrequency(df4_x)

itemFrequencyPlot(df4_x)

#using the apriori function to create the association rules where the lhs is default and rhs is the likelihood to recommend column (Promoters)
ruleset<-apriori(df4_x,parameter=list(support=0.04,confidence=0.5),appearance = list(default="lhs",rhs=("pr_pa_dt=Promoter")))

#using the apriori function to create the association rules where the lhs is default and rhs is the likelihood to recommend column (Detractors)
rul<-apriori(df4_x,parameter=list(support=0.04,confidence=0.5),appearance = list(default="lhs",rhs=("pr_pa_dt=Detractor")))

#Inspecting the Associations for the promoters in rhs Interactively Using datatable
inspectDT(ruleset)

#Inspecting the Associations for the detractors in rhs Interactively Using datatable
inspectDT(rul)

#plotting graphs for the rules for the top 2 rules with highest lift
subrules<-head(ruleset,n=2,by="lift")
plot(subrules,method="graph")
plot(subrules,method = "paracoord")

subrules_d<-head(rul,n=2,by="lift")
plot(subrules_d,method="graph")
plot(subrules_d,method="paracoord")


##############Arules for NORTHWEST#####################


#creating a dataframe from the main datframe which contains data only for Northwest airlines
Northwest_df<-data2[str_trim(df$Partner.Name)=="Northwest Business Airlines Inc."& df$Flight.cancelled=='No',]

#classifying the values in the loyalty, eating.and.Drinking.at.Airport, Flight.Distance, Total.Freq.Flyer.Accts, Price.Senitivity and Flights.Per.Year columns into low or high using the creategroups function
Northwest_df$Loyalty<-creategroups(Northwest_df$Loyalty)
Northwest_df$Eating.and.Drinking.at.Airport<-creategroups(Northwest_df$Eating.and.Drinking.at.Airport)
Northwest_df$Flight.Distance<-creategroups(Northwest_df$Flight.Distance)
Northwest_df$Total.Freq.Flyer.Accts<-creategroups(Northwest_df$Total.Freq.Flyer.Accts)
Northwest_df$Price.Sensitivity<-as.numeric(Northwest_df$Price.Sensitivity)
Northwest_df$Price.Sensitivity<-creategroups(Northwest_df$Price.Sensitivity)
Northwest_df$Flights.Per.Year<-creategroups(Northwest_df$Flights.Per.Year)

#converting the variables in the dataframe into factors
Northwest_df<-mutate_all(Northwest_df,as.factor)

NWA<-as(Northwest_df,"transactions") #converting the dataframe into a transactions matrix

#using the apriori function to create the association rules where the lhs is default and rhs is the likelihood to recommend column (Detractors)
r<-apriori(NWA,parameter=list(support=0.06,confidence=0.5),appearance = list(default="lhs",rhs=("pr_pa_dt=Detractor")))

#Inspecting the Associations for the detractors in rhs Interactively Using datatable
inspectDT(r)

#plotting graphs for the rules for the top 2 rules with highest lift
sub_1<-head(r,n=2,by="lift")
plot(sub_1,method="graph")
plot(sub_1,method="paracoord")


############ ARULES for flyfast#############


#creating a dataframe from the main datframe which contains data only for Flyfast airways 
flyfast_df<-data2[str_trim(df$Partner.Name)=="FlyFast Airways Inc." & df$Flight.cancelled=='No',]

#classifying the values in the loyalty, eating.and.Drinking.at.Airport, Flight.Distance, Total.Freq.Flyer.Accts, Price.Senitivity and Flights.Per.Year columns into low or high using the creategroups function
flyfast_df$Loyalty<-creategroups(flyfast_df$Loyalty)
flyfast_df$Eating.and.Drinking.at.Airport<-creategroups(flyfast_df$Eating.and.Drinking.at.Airport)
flyfast_df$Flight.Distance<-creategroups(flyfast_df$Flight.Distance)
flyfast_df$Total.Freq.Flyer.Accts<-creategroups(flyfast_df$Total.Freq.Flyer.Accts)
flyfast_df$Price.Sensitivity<-as.numeric(flyfast_df$Price.Sensitivity)
flyfast_df$Price.Sensitivity<-creategroups(flyfast_df$Price.Sensitivity)
flyfast_df$Flights.Per.Year<-creategroups(flyfast_df$Flights.Per.Year)

#converting the variables in the dataframe into factors
flyfast_df<-mutate_all(flyfast_df,as.factor)

FF<-as(flyfast_df,"transactions") #converting the dataframe into a transactions matrix

#using the apriori function to create the association rules where the lhs is default and rhs is the likelihood to recommend column (Detractors)
r_f<-apriori(FF,parameter=list(support=0.06,confidence=0.5),appearance = list(default="lhs",rhs=("pr_pa_dt=Detractor")))

#Inspecting the Associations for the detractors in rhs Interactively Using datatable
inspectDT(r_f)

#plotting graphs for the rules for the top 2 rules with highest lift
sub_2<-head(r_f,n=2,by="lift")
plot(sub_2,method="graph")
plot(sub_2,method="paracoord")


############Sigma Airlines Inc.######################ARULES########


#creating a dataframe from the main datframe which contains data only for Sigma Airlines 
sigma_df<-data2[str_trim(df$Partner.Name)=="Sigma Airlines Inc." & df$Flight.cancelled=='No',]

#classifying the values in the loyalty, eating.and.Drinking.at.Airport, Flight.Distance, Total.Freq.Flyer.Accts, Price.Senitivity and Flights.Per.Year columns into low or high using the creategroups function
sigma_df$Loyalty<-creategroups(sigma_df$Loyalty)
sigma_df$Eating.and.Drinking.at.Airport<-creategroups(sigma_df$Eating.and.Drinking.at.Airport)
sigma_df$Flight.Distance<-creategroups(sigma_df$Flight.Distance)
sigma_df$Total.Freq.Flyer.Accts<-creategroups(sigma_df$Total.Freq.Flyer.Accts)
sigma_df$Price.Sensitivity<-as.numeric(sigma_df$Price.Sensitivity)
sigma_df$Price.Sensitivity<-creategroups(sigma_df$Price.Sensitivity)
sigma_df$Flights.Per.Year<-creategroups(sigma_df$Flights.Per.Year)

#converting the variables in the dataframe into factors
sigma_df<-mutate_all(sigma_df,as.factor)

#converting the dataframe into a transactions matrix
SS<-as(sigma_df,"transactions")

#using the apriori function to create the association rules where the lhs is default and rhs is the likelihood to recommend column (Promoters)
s_s<-apriori(SS,parameter=list(support=0.04,confidence=0.5),appearance = list(default="lhs",rhs=("pr_pa_dt=Promoter")))

#Inspecting the Associations for the detractors in rhs Interactively Using datatable
inspectDT(s_s)

#plotting graphs for the rules for the top 2 rules with highest lift
sub_3<-head(s_s,n=2,by="lift")
plot(sub_3,method="graph")
plot(sub_3,method="paracoord")

