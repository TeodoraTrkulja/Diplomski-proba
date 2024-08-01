dataset<-read.csv("stroke_prediction_dataset.csv",stringsAsFactors = F)
str(dataset)

apply(dataset,MARGIN = 2,FUN = function(x) sum(is.na(x)))
apply(dataset,MARGIN = 2,FUN = function(x) sum(x==""))
#symptoms sadrži 2500 ""
apply(dataset,MARGIN = 2,FUN = function(x) sum(x==" "))
apply(dataset,MARGIN = 2,FUN = function(x) sum(x=="-"))
dataset$Patient.ID<-NULL
dataset$Patient.Name<-NULL
dataset$Symptoms<-NULL
dataset$Work.Type<-NULL
dataset$Dietary.Habits<-NULL
dataset$Gender<-as.factor(dataset$Gender)
dataset$Gender<-factor(dataset$Gender,levels = c("Male","Female"))
table(dataset$Marital.Status)
table(dataset$Work.Type)
table(dataset$Residence.Type)
table(dataset$Smoking.Status)
table(dataset$Alcohol.Intake)
table(dataset$Physical.Activity)
table(dataset$Family.History.of.Stroke)
table(dataset$Dietary.Habits)
table(dataset$Stress.Levels)

dataset$Stress.Levels<-NULL
dataset$Marital.Status<-as.factor(dataset$Marital.Status)
dataset$Residence.Type<-as.factor(dataset$Residence.Type)
dataset$Smoking.Status<-as.factor(dataset$Smoking.Status)
dataset$Alcohol.Intake<-as.factor(dataset$Alcohol.Intake)
dataset$Physical.Activity<-as.factor(dataset$Physical.Activity)
dataset$Family.History.of.Stroke<-as.factor(dataset$Family.History.of.Stroke)


dataset$Marital.Status<-factor(dataset$Marital.Status,levels = c("Single","Married","Divorced"))
dataset$Residence.Type<-factor(dataset$Residence.Type,levels = c("Rural","Urban"))
dataset$Smoking.Status<-factor(dataset$Smoking.Status,levels = c("Non-smoker","Currently Smokes","Formerly Smoked"))
dataset$Alcohol.Intake<-factor(dataset$Alcohol.Intake,levels = c("Never","Rarely","Social Drinker","Frequent Drinker"))
dataset$Physical.Activity<-factor(dataset$Physical.Activity,levels = c("Low","Moderate","High"))
dataset$Family.History.of.Stroke<-factor(dataset$Family.History.of.Stroke,levels = c("No","Yes"))

dataset$Diagnosis<-as.factor(dataset$Diagnosis)
dataset$Diagnosis<-factor(dataset$Diagnosis,levels = c("Stroke","No Stroke"))

str(dataset)

#library(caret)
#library(rpart)
#set.seed(10) 
#indexes <- createDataPartition(dataset$Diagnosis, p = 0.8, list = FALSE)
#trainSet<- dataset[indexes, ]
#testSet <- dataset[-indexes, ]

#tree1 <- rpart(Diagnosis ~ ., 
    #           data = trainSet,
     #          method = "class")

#tree1
#install.packages("rpart.plot")
#library(rpart.plot)
#rpart.plot(tree1, extra = 106)


#tree1.prediction<-predict(tree1,
 #                         newdata=testSet,
                #          type="class")

#tree1.cm<-table(true=testSet$Diagnosis,
  #              predicted=tree1.prediction)
#tree1.cm
