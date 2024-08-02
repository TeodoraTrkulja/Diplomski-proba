dataset<-read.csv("stroke_prediction_dataset.csv",stringsAsFactors = F)
str(dataset)

#Instaliranje paketa
install.packages("stringr")
install.packages("dplyr")
install.packages("ggplot2")

#provera nedostajućih vrednosti
apply(dataset,MARGIN = 2,FUN = function(x) sum(is.na(x)))
apply(dataset,MARGIN = 2,FUN = function(x) sum(x==""))
#symptoms sadrži 2500 ""
apply(dataset,MARGIN = 2,FUN = function(x) sum(x==" "))
apply(dataset,MARGIN = 2,FUN = function(x) sum(x=="-"))

#isključivanje opisnih varijabli i varijabli sa prevelikim brojem različitih vrednosti
dataset$Patient.ID<-NULL
dataset$Patient.Name<-NULL
dataset$Symptoms<-NULL

#provera vrednosti char i num promenljivih
summary(dataset$Marital.Status)
summary(dataset$Work.Type)
summary(dataset$Residence.Type)
summary(dataset$Smoking.Status)
summary(dataset$Alcohol.Intake)
summary(dataset$Physical.Activity)
summary(dataset$Family.History.of.Stroke)
summary(dataset$Dietary.Habits)
summary(dataset$Stress.Levels)

#transformisanje char varijable u factor
dataset$Gender<-as.factor(dataset$Gender)
dataset$Work.Type<-as.factor(dataset$Work.Type)
dataset$Dietary.Habits<-as.factor(dataset$Dietary.Habits)
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
dataset$Gender<-factor(dataset$Gender,levels = c("Male","Female"))
dataset$Work.Type<-factor(dataset$Work.Type,levels = c("Never Worked","Self-employed","Private","Government Job"))
dataset$Dietary.Habits<-factor(dataset$Dietary.Habits,levels = c("Pescatarian",
                                                                 "Vegan",
                                                                 "Vegetarian",
                                                                 "Paleo",
                                                                 "Keto",
                                                                 "Non-Vegetarian",
                                                                 "Gluten-Free"))
#transformacija indikatorskih numeričkih varijabli u factor
dataset$Hypertension<-as.factor(dataset$Hypertension)
dataset$Heart.Disease<-as.factor(dataset$Heart.Disease)
dataset$Stroke.History<-as.factor(dataset$Stroke.History)

#transformisanje izlazne varijable u factor
dataset$Diagnosis<-as.factor(dataset$Diagnosis)
dataset$Diagnosis<-factor(dataset$Diagnosis,levels = c("Stroke","No Stroke"))

#Kreiranje dve nove kolone na osnovu varijable Blood.Pressure.Levels, Systolic.pressure i Diastolic.pressure

library(stringr)
library(dplyr)

dataset <- dataset %>%
  mutate(
    Systolic.pressure = as.numeric(str_split_fixed(Blood.Pressure.Levels, "/", 2)[, 1]),
    Diastolic.pressure = as.numeric(str_split_fixed(Blood.Pressure.Levels, "/", 2)[, 2])
  )

dataset$Blood.Pressure.Levels<-NULL

#Kreiranje dve nove kolone na osnovu varijable Cholesterol.Levels, HDL i LDL
library(stringr)
library(dplyr)
dataset <- dataset %>%
  mutate(
    Cholesterol.Levels = str_replace_all(Cholesterol.Levels, "HDL: |LDL: ", ""),
    HDL = as.numeric(str_split_fixed(Cholesterol.Levels, ", ", 2)[, 1]),
    LDL = as.numeric(str_split_fixed(Cholesterol.Levels, ", ", 2)[, 2])
  )
dataset$Cholesterol.Levels<-NULL

set.seed(1010)
dataset1 <- dataset[sample(nrow(dataset), 150), ]
summary(dataset1$Diagnosis)

#Postavljanje izlazne varijable na kraj
dataset <- dataset[ , c(1:16, 18:21, 17)]   


#Ispitivanje značajnosti varijabli preko plotova
library(ggplot2)
library(dplyr)

# Scatter plot za numeričke varijable u odnosu na Diagnosis
numeric_vars <- c("Age", "Average.Glucose.Level", "Body.Mass.Index..BMI.", 
                  "Stress.Levels", "Systolic.pressure", "Diastolic.pressure", 
                  "HDL", "LDL")


for (var in numeric_vars) {
  print(
    ggplot(dataset1, aes_string(x = var, y = "Diagnosis", color = "Diagnosis")) +
      geom_point() +
      labs(title = paste("Scatter plot", var, "vs Diagnosis"),
           x = var,
           y = "Diagnosis",
           color = "Diagnosis")
  )
}
# Box plot za numeričke varijable u odnosu na Diagnosis
for (var in numeric_vars) {
  print(
    ggplot(dataset1, aes_string(x = "Diagnosis", y = var, fill = "Diagnosis")) +
      geom_boxplot() +
      labs(title = paste("Box plot", var, "u odnosu na Diagnosis"),
           x = "Diagnosis",
           y = var,
           fill = "Diagnosis")
  )
}
# Violin plot za numeričke varijable u odnosu na Diagnosis
for (var in numeric_vars) {
  print(
    ggplot(dataset, aes_string(x = "Diagnosis", y = var, fill = "Diagnosis")) +
      geom_violin() +
      labs(title = paste("Violin plot", var, "u odnosu na Diagnosis"),
           x = "Diagnosis",
           y = var,
           fill = "Diagnosis")
  )
}

# Bar plot za kategorijske varijable u odnosu na Diagnosis
categorical_vars <- c("Gender", "Hypertension", "Heart.Disease", "Marital.Status",
                      "Work.Type", "Residence.Type", "Smoking.Status", 
                      "Alcohol.Intake", "Physical.Activity", "Stroke.History", 
                      "Family.History.of.Stroke", "Dietary.Habits")

for (var in categorical_vars) {
  print(
    ggplot(dataset1, aes_string(x = var, fill = "Diagnosis")) +
      geom_bar(position = "dodge") +
      labs(title = paste("Bar plot", var, "u odnosu na Diagnosis"),
           x = var,
           y = "Count",
           fill = "Diagnosis")
  )
}
# Scatter plot za Systolic.pressure i Diastolic.pressure sa facet wrap za Diagnosis
ggplot(dataset1, aes(x = Systolic.pressure, y = Diastolic.pressure, color = Diagnosis)) +
  geom_point() +
  facet_wrap(~ Diagnosis) +
  labs(title = "Scatter plot Systolic vs Diastolic Pressure sa facet wrap za Diagnosis",
       x = "Systolic Pressure",
       y = "Diastolic Pressure",
       color = "Diagnosis")

str(dataset)





