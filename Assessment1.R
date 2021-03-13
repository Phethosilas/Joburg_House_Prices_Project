
library(ggplot2)
library(ggthemes)
library(tidyr)
library(readr)
library(stringr)
library(scales)
library(dplyr) 
library(mice)
library(randomForest) 
library(data.table)
library(gridExtra)
library(corrplot) 
library(GGally)
library(e1071)
library(taRifx)
library(stringi)
library(caret)
library(MASS)
library(lattice)
library(VIM)
library(ggpubr)



train=read.csv(file.choose())
test_data=read.csv(file.choose())
validation_data=read.csv(file.choose())
View(train)

######

#I decided to extract values from text,manipulate and clean the data,


###Data Structure/Preparation/Cleaning/Manipulation #######


#this function will be used to extract integers from text.                                  
numextract <- function(string){ 
  str_extract(string, "\\-*\\d+\\.*\\d*")
} 
#cleaning the area column
train$area=as.integer(numextract(train$area))

#cleaning bathroom columns
train$bathrooms=as.integer(numextract(train$bathrooms))

#cleaning the bedroom 2
train$Bedrooms_2=as.integer(numextract(train$Bedrooms_2))

#cleaning feature 2
train$Feature_2=as.integer(numextract(train$Feature_2))

##Cleaning Rent amount
train$rental_amount=numextract(str_replace_all(train$rental_amount, " ", ""))

train$rental_amount=as.integer(train$rental_amount) 

#dealing with area
train$area_jhb=as.factor(train$area_jhb)

str(train)
###Checking outliers and missing values and replacing them with anumeric placeholder

complete.cases(train)
summary(train)
Na_s=subset(train,is.na(train$Bedrooms_2))
Na_s2=subset(train,is.na(train$bathrooms))
Na_s3=subset(train,is.na(train$area))
Na_s4=subset(train,is.na(train$Feature_2))

# The percentage of data missing in train data
sum(is.na(train)) / (nrow(train) *ncol(train))

#dealing with Outliers
outlier_norm <- function(x){
  qntile <- quantile(x, probs=c(.25, .75))
  caps <- quantile(x, probs=c(.05, .95))
  H <- 1.5 * IQR(x, na.rm = T)
  x[x < (qntile[1] - H)] <- caps[1]
  x[x > (qntile[2] + H)] <- caps[2]
  return(x)
}
train$rental_amount=outlier_norm(train$Bedrooms_2)
train$Bedrooms_2=outlier_norm(train$Bedrooms_2)
train$area=outlier_norm(train$area)
train$Feature_2=outlier_norm(train$Feature_2)
####B. Data Imputation
# The percentage of data missing in train data
sum(is.na(train)) / (nrow(train) *ncol(train))

#About 18% of missing data and more than 25% of missing outliers
train$listing_agency[is.na(train$listing_agency)] <-0
mean(train$listing_agency)
kNN(train,k=5)
train=na.omit(train)


##########Data Visualisation 

ggscatter(train, x = "Bedrooms_2", y = "area", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
        xlab = "Miles/(US) gallon", ylab = "Weight (1000 lbs)")

ggscatter(train, x = "rental_amount", y = "listing_agency", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Miles/(US) gallon", ylab = "Weight (1000 lbs)")
ggscatter(train, x = "area", y = "bathrooms", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Miles/(US) gallon", ylab = "Weight (1000 lbs)")


#rental amount
ggqqplot(train$rental_amount, ylab = "rental_amount")

ggqqplot(train$Bedrooms_2, ylab = "Bedrooms_2")
train[nrow(train)+1904,] <- 0

initialmodel <- lm(rental_amount ~ train$area_jhb + train$Bedrooms_2
               + train$area + train$bathrooms , data = train )
summary(initialmodel)
###############

#validating the model again

finalmodel <- lm(rental_amount ~ train$Bedrooms_2
                  + train$area + train$bathrooms, data = train )
summary(finalmodel )

######
Prediction_M=predict(finalmodel,newdata=validation_data)
#Predicting the prices
(Prices=data.frame(validation_data$id,Prediction_M))

#############################
#The biggest problem with this data is that it was having a lot of missing values.Most valuable information were
#regarded as characters.The cleaning of the data involved extracting the rent prices and converting them into numeric or intergers.
#Another probles was the bedroom,area and feature.everything was scatterd.
#A lot of data manipulation had to be involved and this included coming up with the right imputation method for replacing missing values,
#Removing the outliers and unwated text of data in wrong columns.
#As it is my first time working with such a data,given that in varsity we worked with nice and clean data,well,there wasnt much cleaning to do.While with this data,
#What we can learn is that valuable information about the data is hidden and you need to find ways to get it,
#You have to consider different explanatory variables which are significant and how you will use them.
#well you might assume that since we're predicting house prices,feature like the location,whether johannesburg,durban etc
#,the number of bedrooms,the size or area.This are all important explonatory variables.
#Well we have to becareful of explanatory variables that are correlated.

#Honestly,i was really challened and forced tho think outside the box.I was stretched beyond capacity and for that i am greatful