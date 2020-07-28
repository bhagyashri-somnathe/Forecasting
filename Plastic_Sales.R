# Forecast the CocaCola prices and Airlines Passengers data set. 
# Prepare a document for each model explaining 
# how many dummy variables you have created and RMSE value for each model. 
# Finally which model you will use for 
# Forecasting.


plastic_sale_data<-read.csv(file.choose()) 
View(plastic_sale_data) # Seasonality 12 months 
windows()
plot(plastic_sale_data$Sales,type="o")

# So creating 12 dummy variables 

dummy_table<- data.frame(outer(rep(month.abb,length = 60), month.abb,"==") + 0 )
# Creating dummies for 12 months
View(dummy_table)

colnames(dummy_table)<-month.abb # Assigning month names 
View(dummy_table)
plasticData<-cbind(plastic_sale_data,dummy_table)
View(airlineData)

plasticData["t"]<- 1:60
View(plasticData)
plasticData["log_sale"]<-log(plasticData["Sales"])
plasticData["t_square"]<-plasticData["t"]*plasticData["t"]
attach(plasticData)

plastic_sale_train<-plasticData[1:48,]

plastic_sale_test<-plasticData[49:60,]

########################### LINEAR MODEL #############################

linear_model<-lm(Sales~t,data=plastic_sale_train)
summary(linear_model)
linear_pred<-data.frame(predict(linear_model,interval='predict',newdata =plastic_sale_test))
View(linear_pred)
rmse_linear<-sqrt(mean((plastic_sale_test$Sales-linear_pred$fit)^2,na.rm = T))
rmse_linear #  260.9378



######################### Exponential #################################

expo_model<-lm(log_sale~t,data=plastic_sale_train)
summary(expo_model)
expo_pred<-data.frame(predict(expo_model,interval='predict',newdata=plastic_sale_test))
rmse_expo<-sqrt(mean((plastic_sale_test$Sales-exp(expo_pred$fit))^2,na.rm = T))
rmse_expo # 268.6938

######################### Quadratic ####################################

Quad_model<-lm(Sales~t+t_square,data=plastic_sale_train)
summary(Quad_model)
Quad_pred<-data.frame(predict(Quad_model,interval='predict',newdata=plastic_sale_test))
rmse_Quad<-sqrt(mean((plastic_sale_test$Sales-Quad_pred$fit)^2,na.rm=T))
rmse_Quad # 297.4067

######################### Additive Seasonality #########################

sea_add_model<-lm(Sales~Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov,data=plastic_sale_train)
summary(sea_add_model)
sea_add_pred<-data.frame(predict(sea_add_model,newdata=plastic_sale_test,interval='predict'))
rmse_sea_add<-sqrt(mean((plastic_sale_test$Sales-sea_add_pred$fit)^2,na.rm = T))
rmse_sea_add # 235.6027

######################## Additive Seasonality with Linear #################

Add_sea_Linear_model<-lm(Sales~t+Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov,data=plastic_sale_train)
summary(Add_sea_Linear_model)
Add_sea_Linear_pred<-data.frame(predict(Add_sea_Linear_model,interval='predict',newdata=plastic_sale_test))
rmse_Add_sea_Linear<-sqrt(mean((plastic_sale_test$Sales-Add_sea_Linear_pred$fit)^2,na.rm=T))
rmse_Add_sea_Linear # 135.5536

######################## Additive Seasonality with Quadratic #################

Add_sea_Quad_model<-lm(Sales~t+t_square+Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov,data=plastic_sale_train)
summary(Add_sea_Quad_model)
Add_sea_Quad_pred<-data.frame(predict(Add_sea_Quad_model,interval='predict',newdata=plastic_sale_test))
rmse_Add_sea_Quad<-sqrt(mean((plastic_sale_test$Sales-Add_sea_Quad_pred$fit)^2,na.rm=T))
rmse_Add_sea_Quad #  218.1939

######################## Multiplicative Seasonality #########################

multi_sea_model<-lm(log_sale~Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov,data = plastic_sale_train)
summary(multi_sea_model)
multi_sea_pred<-data.frame(predict(multi_sea_model,newdata=plastic_sale_test,interval='predict'))
rmse_multi_sea<-sqrt(mean((plastic_sale_test$Sales-exp(multi_sea_pred$fit))^2,na.rm = T))
rmse_multi_sea # 239.6543

######################## Multiplicative Seasonality Linear trend ##########################

multi_add_sea_model<-lm(log_sale~t+Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov,data = plastic_sale_train)
summary(multi_add_sea_model) 
multi_add_sea_pred<-data.frame(predict(multi_add_sea_model,newdata=plastic_sale_test,interval='predict'))
rmse_multi_add_sea<-sqrt(mean((plastic_sale_test$Sales-exp(multi_add_sea_pred$fit))^2,na.rm = T))
rmse_multi_add_sea # 160.6833

# Preparing table on model and it's RMSE values 

table_rmse<-data.frame(c("rmse_linear","rmse_expo","rmse_Quad","rmse_sea_add","rmse_Add_sea_Linear","rmse_Add_sea_Quad","rmse_multi_sea","rmse_multi_add_sea"),c(rmse_linear,rmse_expo,rmse_Quad,rmse_sea_add,rmse_Add_sea_Linear,rmse_Add_sea_Quad,rmse_multi_sea,rmse_multi_add_sea))
View(table_rmse)
colnames(table_rmse)<-c("model","RMSE")
View(table_rmse)

# Multiplicative Seasonality Linear trend has least RMSE value

new_model <- lm(log_sale~t+Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov,data = plasticData)

new_model_pred<-data.frame(predict(new_model,newdata=plasticData,interval='predict'))
new_model_fin <- exp(new_model$fitted.values)

View(new_model_fin)

pred_res<- predict(arima(log_sale,order=c(1,0,0)),n.ahead = 12)
Month <- as.data.frame(plastic_sale_data$Month)

Final_model <- as.data.frame(cbind(Month,plasticData$Sales,new_model_fin))
colnames(Final_model) <-c("Month","Sales","New_Pred_Value")
Final_model <- as.data.frame(Final_model)
View(Final_model)

plot(Final_model$Sales,main = "ActualGraph", xlab="Sales(Actual)", ylab="Months",
     col.axis="blue",type="o") 


plot(Final_model$New_Pred_Value, main = "PredictedGraph", xlab="Sales(Predicted)", ylab="Months",
     col.axis="Green",type="s")

### Summary of this problem :

# here we have created 12 dummy variables as sesonality is 12 months for this problem
# after creating all models, it has been observed that RMSE for Additive Seasonality with Linear 
# is less i.e. 160.6833 with 97 r squared value
# so we have build model on that and predicted new values 


