#@Author: TS and MM
##Description: This R Script contains the code used in the Care sales report included in this file. The intention of this file is to illustrate the  techniques and procedures learned during Data anylitics I used in a real business application.


car_data = read.csv("I:/Analysis6100/Data Sets/Car-Data.csv")

attach(car_data)


hist(Age, xlab = "Age", main = "Histogram of Age")
hist(profit, xlab = "Profit", main= "Histogram of Profit")
summary(profit >= 6000)

hist(Odo)


#We create a profitability variable

car_data$profit <- MMRAretail - MMRAauction
profit

mean(profit)


#used large SUV small compact cars and vans were sold for 
#what % less profit so the 400$ divided by average profit 


#checking for any issues

colnames(car_data)
summary(car_data)

summary(Odo == "NA")
summary(Age=="NA")
summary(Make=="NA")
summary(profit=="NA")
summary(profit)


table(car_data$Age)
table(car_data$Make)


summary(WheelType=="NULL")

#We are going to remove
car_data$Auction = NULL
car_data$Color = NULL
car_data$WheelType=NULL

#split into testing and training

set.seed(6100)
num_obs <- nrow(car_data)
train_obs <- sample(num_obs,0.70*num_obs)
car_train <- car_data[train_obs,]
car_test <- car_data[-train_obs,]

#Now we are looking at profit and age in a linear regression model

car.age <- lm(profit ~ Age, data=car_train)
summary(car.age)

car.age_odo <- lm(profit~ Age + Odo, data = car_train)
summary(car.age_odo)

car.age_odo_make <- lm(profit ~ Age + Odo + Make, data=car_train)
summary(car.age_odo_make)


car.multi <- lm(Age ~Odo , data = car_train)
summary(car.multi)

odoage <- Odo/Age
car_data$odoage <- Odo/Age
odoage
summary(odoage)
car.odoage <- lm(profit ~ odoage + Make + Size, data=car_train)
summary(car.odoage)



#Function for RMSE
RMSE_calc <- function(predicted = NULL, actual = NULL)
{ RMSE = sqrt(mean((predicted-actual)^2))
return(RMSE)
}

#RMSE for car.age
car.age
car.preds <- predict(car.age,newdata=car_test)
RMSE1 <- RMSE_calc(car.preds,car_test$profit)
RMSE1

car.age_odo
car.age.preds <- predict(car.age_odo, newdata=car_test)
RMSE2 <- RMSE_calc(car.age.preds, car_test$profit)
RMSE2

car.age_odo_make
car.age.odo.preds <- predict(car.age_odo_make, newdata=car_test)
RMSE3 <- RMSE_calc(car.age.odo.preds,car_test$profit)
RMSE3

car.odoage
car.odoage.preds <- predict(car.odoage, newdata=car_test)
RMSE4<- RMSE_calc(car.odoage.preds, car_test$profit)
RMSE4

mean(odoage)

