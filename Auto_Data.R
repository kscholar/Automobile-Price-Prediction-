# Case study Automobile Dataset
rm(list = ls())

# Importing necessary libraries
library(naniar)
library(fastDummies)
library(caTools)
library(MASS)
library(car)
library(ggplot2)
library(caret)



#Setting the working directory
setwd("C:/Users/Kamaljeet Singh/Desktop/Ongoing Project/Automobile Data")
#Loading our data
auto_data<- read.csv("Automobile_data.csv", stringsAsFactors = F)
#Understanding our data
colnames(auto_data)
#checking the dimension of our data
dim(auto_data)
# Checking the data types and structure 
str(auto_data)

# correcting the datatypes of few variables
auto_data$normalized.losses<-as.numeric(auto_data$normalized.losses)
auto_data$horsepower<-as.numeric(auto_data$horsepower)
auto_data$peak.rpm<-as.numeric(auto_data$peak.rpm)
auto_data$bore<-as.numeric(auto_data$bore)
auto_data$stroke<-as.numeric(auto_data$stroke)
auto_data$price<-as.numeric(auto_data$price)
str(auto_data)

# Let's visualize the missing value in our data set
#we will need to install naniar package 
#install.packages(naniar)
gg_miss_var(auto_data)

#here we see that the variables which were in the form of character data type
#and were then converted to numeric data type are the ones that have missing values.
#variable  normalized.losses has highest number of missing values followed by stroke,price and bore
#and subsequently followed by peak.rpm and horsepower 

# Missing value imputation
attach(auto_data)
auto_data$normalized.losses[is.na(auto_data$normalized.losses)]<-median(auto_data$normalized.losses,na.rm=TRUE)
auto_data$stroke[is.na(auto_data$stroke)]<-median(auto_data$stroke,na.rm=TRUE)
auto_data$price[is.na(auto_data$price)]<-median(auto_data$price,na.rm=TRUE)
auto_data$bore[is.na(auto_data$bore)]<-median(auto_data$bore,na.rm=TRUE)
auto_data$peak.rpm[is.na(auto_data$peak.rpm)]<-median(auto_data$peak.rpm,na.rm=TRUE)
auto_data$horsepower[is.na(auto_data$horsepower)]<-median(auto_data$horsepower,na.rm=TRUE)

#visualising missing values now
gg_miss_var(auto_data)
# We can now see that there are no missing values


# Creating dummy Variables
auto_data<-dummy_cols(auto_data, select_columns = 'aspiration')
auto_data<-dummy_cols(auto_data, select_columns = 'fuel.type')
auto_data<-dummy_cols(auto_data, select_columns = 'num.of.doors')
auto_data<-dummy_cols(auto_data, select_columns = 'engine.location')
auto_data<-dummy_cols(auto_data, select_columns = 'drive.wheels')
auto_data<-dummy_cols(auto_data, select_columns = 'engine.type')
auto_data<-dummy_cols(auto_data, select_columns = 'num.of.cylinders')
auto_data<-dummy_cols(auto_data, select_columns = 'fuel.system')
auto_data<-dummy_cols(auto_data, select_columns = 'body.style')
auto_data<-dummy_cols(auto_data, select_columns = 'make')
colnames(auto_data)

#droping unnecessary variables
auto_data<- auto_data[,!(names(auto_data) %in% c("aspiration","fuel.type","num.of.doors","engine.location","drive.wheels","engine.type","num.of.cylinders","fuel.system","body.style","make"))]
                  
#Treating column 
str(auto_data)
auto_data$low_risk<-ifelse((symboling == -1 | symboling == -2) , 1, 0)
auto_data$medium_risk<- ifelse((symboling == 1 | symboling == 0) , 1 ,0)
auto_data$high_risk<- ifelse( symboling >1, 1,0)

# Checking our symboling column
#symbol_low_risk<-auto_data[,(names(auto_data)%in% c("symboling","low_risk"))]
#symbol_low_risk

auto_data<- auto_data[,!(names(auto_data) %in% c("symboling"))]

colnames(auto_data)

#final dataset for our modelling
auto_data<- auto_data[,!(names(auto_data) %in% c("num.of.doors_?","aspiration_std","fuel.type_diesel","make_volvo","make_subaru","make_porsche",
                                                 "make_peugot","make_chevrolet","body.style_wagon","fuel.system_spfi","fuel.system_4bbl","fuel.system_idi",
                                                 "engine.type_rotor","high_risk","num.of.cylinders_three","num.of.cylinders_twelve","num.of.cylinders_two",
                                                 "drive.wheels_rwd","engine.location_rear"))]


colnames((auto_data))
dim(auto_data)
# on fitting the model it was found that few variables has high collinearity hence removed "make_volvo","make_subaru","make_porsche",
#"make_peugot","make_chevrolet","body.style_wagon","fuel.system_spfi","fuel.system_4bbl","fuel.system_idi",
#"engine.type_rotor","high_risk","num.of.cylinders_three","num.of.cylinders_twelve","num.of.cylinders_two",
#"drive.wheels_rwd","engine.location_rear"


# Train test Split
set.seed(123)
split=sample.split(price, SplitRatio = 0.8)
train_auto<-subset(auto_data, split== TRUE)
test_auto<- subset(auto_data, split == FALSE)

# fitting Simple linear regressor on our training set
regressor_linear<- lm(formula = price ~ . , data = train_auto)
summary(regressor_linear)
sort(vif(regressor_linear))
importance<-varImp(regressor_linear, scale=FALSE)
print(importance)
plot(importance)


#Using stepAIC to remove insignificant variables
step<-stepAIC(regressor_linear,direction="both")


#2nd iteration of our model
regressor_linear_2<-lm(formula =price ~ normalized.losses + length + width + curb.weight + engine.size + 
                         stroke + compression.ratio + horsepower + peak.rpm + aspiration_turbo + 
                         fuel.type_gas + engine.location_front + drive.wheels_4wd + 
                         engine.type_dohc + engine.type_dohcv + engine.type_l + engine.type_ohcv + 
                         num.of.cylinders_five + num.of.cylinders_four + num.of.cylinders_six + 
                         fuel.system_2bbl + fuel.system_mpfi + body.style_sedan + 
                         `make_alfa-romero` + make_bmw + make_dodge + make_isuzu + 
                         make_jaguar + `make_mercedes-benz` + make_mitsubishi + make_plymouth + 
                         make_renault + make_saab + engine.type_ohcf , data = train_auto)
summary(regressor_linear_2)
sort(vif(regressor_linear_2))
importance<-varImp(regressor_linear_2, scale=FALSE)
print(importance)
plot(importance)


#3rd Iteration 
#step<-stepAIC(regressor_linear_2,direction = "both")
regressor_linear_3<-lm(formula=price ~ normalized.losses + length + width + curb.weight + engine.size + 
                         stroke +  peak.rpm + aspiration_turbo + 
                          engine.location_front + drive.wheels_4wd + 
                         engine.type_dohc + engine.type_dohcv + engine.type_l + engine.type_ohcv + 
                         num.of.cylinders_five + num.of.cylinders_four + num.of.cylinders_six + 
                         fuel.system_2bbl + fuel.system_mpfi + body.style_sedan + 
                         `make_alfa-romero` + make_bmw + make_dodge + make_isuzu + 
                         make_jaguar + `make_mercedes-benz` + make_mitsubishi + make_plymouth + 
                         make_renault + make_saab + engine.type_ohcf, data=train_auto)
summary(regressor_linear_3)
sort(vif(regressor_linear_3))
importance<-varImp(regressor_linear_3, scale=FALSE)
print(importance)
plot(importance)

# 4th Iteration removing length,drive.wheels_4wd,make_plymouth,make_renault,make_saab,
#step<-stepAIC(regressor_linear_2,direction = "both")
regressor_linear_4<-lm(formula=price ~ normalized.losses + width + curb.weight + engine.size + 
                         stroke +  peak.rpm + aspiration_turbo + 
                         engine.location_front + engine.type_dohc + engine.type_dohcv + engine.type_l + engine.type_ohcv + 
                         num.of.cylinders_five + num.of.cylinders_four + num.of.cylinders_six + 
                         fuel.system_2bbl + fuel.system_mpfi + body.style_sedan + 
                         `make_alfa-romero` + make_bmw + make_dodge + make_isuzu + 
                         make_jaguar + `make_mercedes-benz` + make_mitsubishi + engine.type_ohcf, data=train_auto)
summary(regressor_linear_4)
sort(vif(regressor_linear_4))

# 5th Iteration removing curb.weight ,engine.type_dohc, engine.type_l ,fuel.system_2bbl fuel.system_mpfi

regressor_linear_5<-lm(formula=price ~ normalized.losses + width + engine.size + 
                         stroke +  peak.rpm + aspiration_turbo + 
                         engine.location_front+ engine.type_dohcv + engine.type_ohcv + 
                         num.of.cylinders_five + num.of.cylinders_four + num.of.cylinders_six + 
                         body.style_sedan + 
                         `make_alfa-romero` + make_bmw + make_dodge + make_isuzu + 
                         make_jaguar + `make_mercedes-benz` + make_mitsubishi + engine.type_ohcf, data=train_auto)
summary(regressor_linear_5)
sort(vif(regressor_linear_5))

# 6th Iteration removing make_dodge,make_mitsubishi
regressor_linear_6<-lm(formula=price ~ normalized.losses + width + engine.size + 
                         stroke +  peak.rpm + aspiration_turbo + 
                         engine.location_front+ engine.type_dohcv + engine.type_ohcv + 
                         num.of.cylinders_five + num.of.cylinders_four + num.of.cylinders_six + 
                         body.style_sedan + 
                         `make_alfa-romero` + make_bmw+ make_isuzu + 
                         make_jaguar + `make_mercedes-benz` + engine.type_ohcf, data=train_auto)
summary(regressor_linear_6)
sort(vif(regressor_linear_6))



# Plotting our dataset
# we will plot the dependent variable i.e "Price" w.r.t other variables which have some statistical significance
attach(train_auto)
#price vs normalized.losses
ggplot(auto_data, aes(price,normalized.losses))+ geom_point()+stat_smooth()
# Price vs width 
ggplot(auto_data, aes(price,width))+ geom_point()+stat_smooth()
# Price vs engine.size
ggplot(auto_data, aes(price,engine.size))+ geom_point()+stat_smooth()
# price vs stroke
ggplot(auto_data, aes(price,stroke))+ geom_point()+stat_smooth()
#Price vs peak Rpm
ggplot(auto_data, aes(price,peak.rpm))+ geom_point()+stat_smooth()
#Price vs make
ggplot(auto_data,aes(price,make))+geom_point()+stat_smooth()
#Price vs engine_location
ggplot(auto_data,aes(price,engine.location)) + geom_point()+ stat_smooth()
# Price vs engine_type
ggplot(auto_data, aes(price,engine.type)) +geom_point()+ stat_smooth()
# Price vs number.of.cylinders
ggplot(auto_data,aes(price,num.of.cylinders))+ geom_point()+ stat_smooth()
# Price vs body.style
ggplot(auto_data,aes(price,body.style)) + geom_point() + stat_smooth()

# So for final inference we can conclude that the following variables are good predictors fro the price of cars
# normalized.losses, width, engine.size, stroke, peak.rpm, aspiration_turbo, engine.location_front engine.type_dohcv  engine.type_ohcv, num.of.cylinders_five, num.of.cylinders_four num.of.cylinders_six, 
# body.style_sedan, make_alfa-romero, make_bmw, make_isuzu,make_jaguar, make_mercedes-benz,  engine.type_ohcf



#Making predictions and checking for the r squared value of the model

# 1st Iterartion model 1
y_predict<-predict(regressor_linear,newdata = test_auto)
rsquared_1<-cor(test_auto$price,y_predict)

# 2nd iteration model 2
y_predict<-predict(regressor_linear_2,newdata = test_auto)
rsquared_2<-cor(test_auto$price,y_predict)

# 3rd iteration model 3
y_predict<-predict(regressor_linear_3,newdata = test_auto)
rsquared_3<-cor(test_auto$price,y_predict)


# 4rd iteration model 4
y_predict<-predict(regressor_linear_4,newdata = test_auto)
rsquared_4<-cor(test_auto$price,y_predict)

# 5th iteration model 5
y_predict<-predict(regressor_linear_5,newdata = test_auto)
rsquared_5<-cor(test_auto$price,y_predict)

# 6th iteration model 6
y_predict<-predict(regressor_linear_6,newdata = test_auto)
rsquared_6<-cor(test_auto$price,y_predict)


# Comparing all the models and the R squared value
R_Squared_table<- data.frame(
  R_square = c(rsquared_1,rsquared_2,rsquared_3,rsquared_4,rsquared_5,rsquared_6),
  model_num = c("Model 1", "Model_2", "Model_3","Model_4","Model_5", "Model_6")
)
R_Squared_table[order(R_Squared_table$R_square),]

## Here we see that model 6 has lowest R square value and hence we will select prediction made by model 6.

# --------------------- FINAL INFERENCE --------------------- #
# So for final inference we can conclude that the following variables are good predictors fro the price of cars
# normalized.losses, width, engine.size, stroke, peak.rpm, aspiration_turbo, engine.location_front engine.type_dohcv  engine.type_ohcv, num.of.cylinders_five, num.of.cylinders_four num.of.cylinders_six, 
# body.style_sedan, make_alfa-romero, make_bmw, make_isuzu,make_jaguar, make_mercedes-benz,  engine.type_ohcf


