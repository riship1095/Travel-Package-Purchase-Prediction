# Library ####
install.packages("rpart")
install.packages("rpart.plot")
install.packages("caret")
install.packages("pROC")
install.packages('epiDisplay')
install.packages("ggplot2")
install.packages("ROCR")
library(epiDisplay)
library(rpart)
library(rpart.plot)
library(caret)
library(pROC)
library(ggplot2)
library(ROCR)
library(randomForest)

#Reading the Data####

data = read.csv("C:\\Users\\DELL\\Downloads\\tour_package.csv")
head(data)

#Checking Null Values####

sapply(data,function(x) sum(is.na(x)))

#There are null values in Age, DurationOfPitch, NumberOfFollowups, PreferredPropertyStar,NumberOfTrips,
#NumberOfTrips, NumberOfChildrenVisiting, MonthlyIncome variables. 

#Checking Variable Distribution and Values####

tab1(data$TypeofContact, sort.group = "decreasing")
tab1(data$ProdTaken, sort.group = "decreasing")
tab1(data$CityTier, sort.group = "decreasing")
tab1(data$Occupation, sort.group = "decreasing")
tab1(data$ProductPitched, sort.group = "decreasing")
tab1(data$MaritalStatus, sort.group = "decreasing")
tab1(data$Passport, sort.group = "decreasing")
tab1(data$OwnCar, sort.group = "decreasing")
tab1(data$Designation, sort.group = "decreasing")
tab1(data$Gender, sort.group = "decreasing")

# The Gender variable has value Female value misspelled as 'Fe Male'. Need to replace all such values with
# Female value. 

data$Gender[data$Gender=='Fe Male'] = 'Female'
tab1(data$Gender, sort.group = "decreasing")

hist(data$Age,
     col='Purple',
     ylim=c(0,1200),
     xlim=c(10,70),
     xlab='Age')
hist(data$NumberOfPersonVisiting,
     breaks =5,
     col = 'light blue',
     ylim=c(0,2500),
     xlab='Number of Person Visiting')
hist(data$PreferredPropertyStar,
     breaks=3,
     col = 'maroon',
     xlab='Preferred Property Star')
hist(data$DurationOfPitch,
     breaks = 5,
     col='pink',
     ylim=c(0,3500),
     xlim=c(0,60),
     xlab='Duration of Pitch')
hist(data$NumberOfFollowups,
     breaks = 7,
     ylim=c(0,2500),
     xlab='Number of Followups',
     col = 'cyan')
hist(data$NumberOfTrips,
     breaks = 3,
     col = 'yellow',
     xlab='Number Of Trips',
     ylim=c(0,5000),
     xlim=c(0,15))
hist(data$PitchSatisfactionScore,
     breaks = 6,
     xlab='Pitch Satisfaction Score',
     col = 'brown')
hist(data$NumberOfChildrenVisiting,
     breaks = 5,
     xlab='Number of Children Visiting',
     col = 'light green')
hist(data$MonthlyIncome,
     xlim=c(15000,45000),
     xlab='Monthly Income',
     col = 'dark green',
     ylim=c(0,2500))

#Imputing Values####

#Imputing values of null values in Age, DurationOfPitch, NumberOfFollowups, PreferredPropertyStar,
#NumberOfTrips, NumberOfChildrenVisiting, MonthlyIncome variables. 

#Using Median values of variables to impute null values. 

data$Age[is.na(data$Age)] = mean(data$Age,na.rm=TRUE)
data$DurationOfPitch[is.na(data$DurationOfPitch)] = median(data$DurationOfPitch,na.rm=TRUE)

data$NumberOfFollowups[is.na(data$NumberOfFollowups)] = median(data$NumberOfFollowups,na.rm=TRUE)
data$PreferredPropertyStar[is.na(data$PreferredPropertyStar)] = mean(data$PreferredPropertyStar,na.rm=TRUE)
data$NumberOfTrips[is.na(data$NumberOfTrips)] = median(data$NumberOfTrips,na.rm=TRUE)
data$NumberOfChildrenVisiting[is.na(data$NumberOfChildrenVisiting)] = mean(data$NumberOfChildrenVisiting,na.rm=TRUE)
data$MonthlyIncome[is.na(data$MonthlyIncome)] = median(data$MonthlyIncome,na.rm=TRUE)
data$TypeofContact[data$TypeofContact==""] <- 'Self Enquiry'

sapply(data,function(x) sum(is.na(x)))
# There are no null values in the dataset anymore.

tab1(data$TypeofContact, sort.group = "decreasing")

hist(data$Age,
     col='Purple',
     ylim=c(0,1200),
     xlim=c(10,70),
     xlab='Age')

hist(data$DurationOfPitch,
     breaks = 5,
     col='pink',
     ylim=c(0,3500),
     xlim=c(0,60),
     xlab='Duration of Pitch')


hist(data$NumberOfFollowups,
     breaks = 7,
     ylim=c(0,2500),
     xlab='Number of Followups',
     col = 'cyan')

hist(data$NumberOfTrips,
     breaks = 3,
     col = 'yellow',
     xlab='Number Of Trips',
     ylim=c(0,5000),
     xlim=c(0,15))


hist(data$NumberOfChildrenVisiting,
     breaks = 5,
     xlab='Number of Children Visiting',
     col = 'light green')

hist(data$MonthlyIncome,
     xlim=c(15000,45000),
     xlab='Monthly Income',
     col = 'dark green',
     ylim=c(0,2500))

# Correlation ####

#plot(round(data.frame(cor(data[-c(1,4,8,7,11,13,19)])),2))
install.packages ('corrplot')
library(corrplot)
corrplot(cor(data))


palette = colorRampPalette(c("green", "white", "red")) (20)
heatmap(x = data.cor, col = palette, symm = TRUE)

#Exporting the DataSet ####
TravelPackage <- data[,-c(1)]
View(TravelPackage)
split.index <- seq(1,4888,by=1.66)
train.df <- TravelPackage[split.index,]
valid.df <- TravelPackage[-split.index,]

#create a classification tree
default.ct.train <- rpart(ProdTaken ~.,data = train.df, method = "class" )
rpart.plot(default.ct.train)
default.ct.pred.valid <- predict(default.ct.train,valid.df,type="class")
# generate confusion matrix for predicted data
confusionMatrix(as.factor(valid.df$ProdTaken),default.ct.pred.valid)

#Logistic regression
logit.reg.train = glm(ProdTaken ~ ., data= train.df, family ="binomial")
options(scipen =999)
summary(logit.reg.train)
logit.reg.pred.valid = predict(logit.reg.train, valid.df, type ="response")
# generate confusion matrix for predicted data
confusionMatrix(as.factor(valid.df$ProdTaken),as.factor(ifelse(logit.reg.pred.valid>0.5,1,0)),mode='everything')

#plot the variables by order of importance
v <- varImp(logit.reg.train)
ggplot(v, aes(x=reorder(rownames(v),Overall), y=Overall)) +
  geom_point( color="slateblue4", size=3, alpha=0.8)+
  geom_segment( aes(x=rownames(v), xend=rownames(v), y=0, yend=Overall), 
                color='turquoise4') +
  xlab('Variable')+
  ylab('Overall Importance')+
  theme_light() +
  coord_flip()

#Random forest classifier
ProdTaken <- as.factor(valid.df$ProdTaken)
rf <- randomForest(ProdTaken ~ ., data = train.df, 
                   ntree = 500, mtry = 4, nodesize = 1, importance = TRUE, sampsize = 500) 

#plot the variables by order of importance
varImpPlot(rf, type = 1)

#create a confusion matrix

rf.pred <- predict(rf, valid.df)
confusionMatrix(as.factor(ifelse(rf.pred>0.5,1,0)) ,as.factor(valid.df$ProdTaken), mode='everything')

t.df.rand <- data.frame("Predicted" = rf.pred, "Label" = as.factor(valid.df$ProdTaken))
t.df.rand

pred.random <- prediction(as.numeric(t.df.rand$Predicted), as.numeric(t.df.rand$Label))
perf.rand <- performance( pred.random, "tpr", "fpr" )


rdt <- roc(valid.df$ProdTaken,as.numeric(default.ct.pred.valid))
plot.roc(rdt)
auc(rdt)

rlr <- roc(valid.df$ProdTaken,logit.reg.pred.valid)
plot.roc(rlr)
auc(rlr)

rfr <- roc(valid.df$ProdTaken,rf.pred)
plot.roc(rfr)
auc(rfr)

#Bar Plot

new_data <- data[data$ProdTaken ==1,]
counts1 <- table(new_data$ProdTaken,new_data$NumberOfPersonVisiting)
barplot(counts1,xlab="Number of Persons Visiting",ylab="No of Purchases")

counts2 <- table(new_data$ProdTaken,new_data$Passport)
barplot(counts2,xlab="Passport",ylab="No of Purchases")

counts3 <- table(new_data$ProdTaken,new_data$Designation)
barplot(counts3,xlab="Designation",ylab="No of Purchases")

counts4 <- table(new_data$ProdTaken,new_data$ProductPitched)
barplot(counts4,xlab="Type of Package",ylab="No of Purchases")
