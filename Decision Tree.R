
install.packages("rpart",dependencies = TRUE)
install.packages("rpart.plot",dependencies = TRUE) # install package rpart.plot

library("rpart")
library("rpart.plot")

# Read the data
#setwd("c:/data/")
banktrain <- read.csv("bank-sample.csv")

class(banktrain)

# Make a simple decision tree by only keeping the categorical variables

fit <- rpart(subscribed ~ job + marital + education + default + housing + loan + contact + poutcome, 
             method="class", 
             data=banktrain,
             control=rpart.control(minsplit=2),
             parms=list(split='information'))
summary(fit)

# Plot the tree
rpart.plot(fit,type = 4,extra = 2)



# include a numeric variable "duration" into the model
fit <- rpart(subscribed ~ job + marital + education + default + housing + loan + contact + duration + poutcome, 
             method="class", 
             data=banktrain,
             control=rpart.control(minsplit=1),
             parms=list(split='information'))
summary(fit)
# Plot the tree
rpart.plot(fit, type=4, extra=2)
# Predict
newdata <- data.frame(job="management", 
                      marital="married", 
                      education="secondary",
                      default="no",
                      housing="yes",
                      loan="no",
                      contact = "cellular",
                      duration = 598,
                      poutcome="success")

newdata

predict(fit,newdata=newdata,type=c("class"))

# Read the data
play_decision <- read.table("DTdata.csv", header=TRUE, sep=",")
play_decision
summary(play_decision)

# build the decision tree
fit <- rpart(Play ~ Outlook + Temperature + Humidity + Wind,
             method="class",
             data=play_decision,
             control=rpart.control(minsplit=1),
             parms=list(split='information'))
summary(fit)

?rpart.plot
rpart.plot(fit, type=4, extra=1)

pdf("preditPlay.pdf")
rpart.plot(fit, type=4, extra=1)
dev.off()

newdata <- data.frame(Outlook="rainy", Temperature="mild",
                      Humidity="high", Wind=FALSE)

newdata

predict(fit,newdata=newdata,type="class")

####################################HR Prediction Problem#############################

employee<-read.csv("HR_comma_sep.csv")


employee

sample<-sample(1:nrow(employee),13000)

trainData<-employee[sample,]

testData <- employee[-sample,]

library(rpart)

fit<-rpart(left~.,data=trainData,method = "class")
fit

pdf("plot.pdf")
rpart.plot(fit,type=4, extra=2, clip.right.labs=FALSE, varlen=0, faclen=3)
dev.off()



