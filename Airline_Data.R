# Forecast the CocaCola prices and Airlines Passengers data set. 
# Prepare a document for each model explaining 
# how many dummy variables you have created and RMSE value for each model. 
# Finally which model you will use for Forecasting.

library(readxl)

airline_data<-read_excel(file.choose()) 
View(airline_data) # Seasonality 12 months 
windows()
plot(airline_data$Passengers,type="o")

# So creating 12 dummy variables 

dummy_table<- data.frame(outer(rep(month.abb,length = 96), month.abb,"==") + 0 )
# Creating dummies for 12 months
View(dummy_table)

colnames(dummy_table)<-month.abb # Assigning month names 
View(dummy_table)
airlineData<-cbind(airline_data,dummy_table)
View(airlineData)

airlineData["t"]<- 1:96
View(airlineData)
airlineData["log_passenger"]<-log(airlineData["Passengers"])
airlineData["t_square"]<-airlineData["t"]*airlineData["t"]
attach(airlineData)

airline_train<-airlineData[1:80,]

airline_test<-airlineData[81:96,]

########################### LINEAR MODEL #############################

linear_model<-lm(Passengers~t,data=airline_train)
summary(linear_model)
linear_pred<-data.frame(predict(linear_model,interval='predict',newdata =airline_test))
View(linear_pred)
rmse_linear<-sqrt(mean((airline_test$Passengers-linear_pred$fit)^2,na.rm = T))
rmse_linear # 47.54262



######################### Exponential #################################

expo_model<-lm(log_passenger~t,data=airline_train)
summary(expo_model)
expo_pred<-data.frame(predict(expo_model,interval='predict',newdata=airline_test))
rmse_expo<-sqrt(mean((airline_test$Passengers-exp(expo_pred$fit))^2,na.rm = T))
rmse_expo # 43.79374

######################### Quadratic ####################################

Quad_model<-lm(Passengers~t+t_square,data=airline_train)
summary(Quad_model)
Quad_pred<-data.frame(predict(Quad_model,interval='predict',newdata=airline_test))
rmse_Quad<-sqrt(mean((airline_test$Passengers-Quad_pred$fit)^2,na.rm=T))
rmse_Quad # 43.6544

######################### Additive Seasonality #########################

sea_add_model<-lm(Passengers~Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov,data=airline_train)
summary(sea_add_model)
sea_add_pred<-data.frame(predict(sea_add_model,newdata=airline_test,interval='predict'))
rmse_sea_add<-sqrt(mean((airline_test$Passengers-sea_add_pred$fit)^2,na.rm = T))
rmse_sea_add # 129.2665

######################## Additive Seasonality with Linear #################

Add_sea_Linear_model<-lm(Passengers~t+Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov,data=airline_train)
summary(Add_sea_Linear_model)
Add_sea_Linear_pred<-data.frame(predict(Add_sea_Linear_model,interval='predict',newdata=airline_test))
rmse_Add_sea_Linear<-sqrt(mean((airline_test$Passengers-Add_sea_Linear_pred$fit)^2,na.rm=T))
rmse_Add_sea_Linear # 33.04571

######################## Additive Seasonality with Quadratic #################

Add_sea_Quad_model<-lm(Passengers~t+t_square+Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov,data=airline_train)
summary(Add_sea_Quad_model)
Add_sea_Quad_pred<-data.frame(predict(Add_sea_Quad_model,interval='predict',newdata=airline_test))
rmse_Add_sea_Quad<-sqrt(mean((airline_test$Passengers-Add_sea_Quad_pred$fit)^2,na.rm=T))
rmse_Add_sea_Quad # 23.91098

######################## Multiplicative Seasonality #########################

multi_sea_model<-lm(log_passenger~Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov,data = airline_train)
summary(multi_sea_model)
multi_sea_pred<-data.frame(predict(multi_sea_model,newdata=airline_test,interval='predict'))
rmse_multi_sea<-sqrt(mean((airline_test$Passengers-exp(multi_sea_pred$fit))^2,na.rm = T))
rmse_multi_sea # 135.3265

######################## Multiplicative Seasonality Linear trend ##########################

multi_add_sea_model<-lm(log_passenger~t+Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov,data = airline_train)
summary(multi_add_sea_model) 
multi_add_sea_pred<-data.frame(predict(multi_add_sea_model,newdata=airline_test,interval='predict'))
rmse_multi_add_sea<-sqrt(mean((airline_test$Passengers-exp(multi_add_sea_pred$fit))^2,na.rm = T))
rmse_multi_add_sea # 9.469

# Preparing table on model and it's RMSE values 

table_rmse<-data.frame(c("rmse_linear","rmse_expo","rmse_Quad","rmse_sea_add","rmse_Add_sea_Quad","rmse_multi_sea","rmse_multi_add_sea"),c(rmse_linear,rmse_expo,rmse_Quad,rmse_sea_add,rmse_Add_sea_Quad,rmse_multi_sea,rmse_multi_add_sea))
View(table_rmse)
colnames(table_rmse)<-c("model","RMSE")
View(table_rmse)

# Multiplicative Seasonality Linear trend has least RMSE value

new_model <- lm(log_passenger~t+Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov,data = airlineData)

new_model_pred<-data.frame(predict(new_model,newdata=airlineData,interval='predict'))
new_model_fin <- exp(new_model$fitted.values)

View(new_model_fin)

pred_res<- predict(arima(log_passenger,order=c(1,0,0)),n.ahead = 12)
Month <- as.data.frame(airline_data$Month)

Final_model <- as.data.frame(cbind(Month,airlineData$Passengers,new_model_fin))
colnames(Final_model) <-c("Month","Passengers","New_Pred_Value")
Final_model <- as.data.frame(Final_model)
View(Final_model)

### Summary of this problem :

# here we have created 12 dummy variables as sesonality is 12 months for this problem
# after creating all models, it has been observed that RMSE for Multiplicative Seasonality Linear trend 
# is less i.e. 9.469
# so we have build model on that and predicted new values 


