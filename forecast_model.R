require(jsonlite)
require(httr)
require(data.table)
library(tidyr)
library(dplyr)
library(fpp2)
library(zoo)
library(xts)
library(tidyverse)
library(caret)

get_token <- function(username, password, url_site){
  
  post_body = list(username=username,password=password)
  post_url_string = paste0(url_site,'/token/')
  result = POST(post_url_string, body = post_body)
  
  # error handling (wrong credentials)
  if(result$status_code==400){
    print('Check your credentials')
    return(0)
  }
  else if (result$status_code==201){
    output = content(result)
    token = output$key
  }
  
  return(token)
}

get_data <- function(start_date='2020-04-01', token, url_site){
  
  post_body = list(start_date=start_date,username=username,password=password)
  post_url_string = paste0(url_site,'/dataset/')
  
  header = add_headers(c(Authorization=paste('Token',token,sep=' ')))
  result = GET(post_url_string, header, body = post_body)
  output = content(result)
  data = data.table::rbindlist(output)
  data[,event_date:=as.Date(event_date)]
  data = data[order(product_content_id,event_date)]
  return(data)
}


send_submission <- function(predictions, token, url_site, submit_now=F){
  
  format_check=check_format(predictions)
  if(!format_check){
    return(FALSE)
  }
  
  post_string="list("
  for(i in 1:nrow(predictions)){
    post_string=sprintf("%s'%s'=%s",post_string,predictions$product_content_id[i],predictions$forecast[i])
    if(i<nrow(predictions)){
      post_string=sprintf("%s,",post_string)
    } else {
      post_string=sprintf("%s)",post_string)
    }
  }
  
  submission = eval(parse(text=post_string))
  json_body = jsonlite::toJSON(submission, auto_unbox = TRUE)
  submission=list(submission=json_body)
  
  print(submission)
  # {"31515569":2.4,"32939029":2.4,"4066298":2.4,"6676673":2.4,"7061886":2.4,"85004":2.4} 
  
  if(!submit_now){
    print("You did not submit.")
    return(FALSE)      
  }
  
  
  header = add_headers(c(Authorization=paste('Token',token,sep=' ')))
  post_url_string = paste0(url_site,'/submission/')
  result = POST(post_url_string, header, body=submission)
  
  if (result$status_code==201){
    print("Successfully submitted. Below you can see the details of your submission")
  } else {
    print("Could not submit. Please check the error message below, contact the assistant if needed.")
  }
  
  print(content(result))
  
}

check_format <- function(predictions){
  
  if(is.data.frame(predictions) | is.data.frame(predictions)){
    if(all(c('product_content_id','forecast') %in% names(predictions))){
      if(is.numeric(predictions$forecast)){
        print("Format OK")
        return(TRUE)
      } else {
        print("forecast information is not numeric")
        return(FALSE)                
      }
    } else {
      print("Wrong column names. Please provide 'product_content_id' and 'forecast' columns")
      return(FALSE)
    }
    
  } else {
    print("Wrong format. Please provide data.frame or data.table object")
    return(FALSE)
  }
  
}

# this part is main code
subm_url = 'http://167.172.183.67'
u_name = "Group22"
p_word = "KUopSFyzCNLlzc2A"
submit_now = FALSE
username = u_name
password = p_word
token = get_token(username=u_name, password=p_word, url=subm_url)
data = get_data(token=token,url=subm_url) %>% as.data.table()

id_list<-data$product_content_id %>% unique()
imputed_day=data[product_content_id==31515569 & event_date=="2020-05-16"]
imputed_day[,event_date:=as.Date("2020-05-17")]
data[product_content_id==31515569 & event_date=="2020-05-17"]=imputed_day[1,]
data[,"week"] = week(data$event_date)
data[,"dayoftheweek"] = weekdays(data$event_date)
forecast_day = as.Date(max(data$event_date)+2)
forecast_dayoftheweek = weekdays(forecast_day)

data_new<- data %>% as.data.table()
data_new[price==-1,price:=1000]
data_new[is.na(data_new)] <- 0
data_new[,forecast_date:=(event_date+2)]
data_new[,forecast_dayoftheweek := weekdays(forecast_date)]
data_new[,forecast_day_sales:=shift(sold_count,2,type="lead"),by=.(product_content_id)]
data_new[,last1:=(sold_count),by=.(product_content_id)]
data_new[,last2:=shift(sold_count,1),by=.(product_content_id)]
data_new[,last7:=rollmean(sold_count,7,na.pad = TRUE,align="right"),by=.(product_content_id)]
data_new[,last_4_week:=rollmean(forecast_day_sales,4,na.pad = TRUE,align="right",na.rm=TRUE),by=.(dayoftheweek)]
data_new[,diff:=c(NA,diff(sold_count,1)),by=.(product_content_id)]
data_new[,last_4_week_diff:=rollmean(diff,4,na.pad = TRUE,align="right",na.rm=TRUE),by=.(dayoftheweek)]
data_new$mean=rowMeans(data_new[,list(last7,last_4_week,last2,last1)],na.rm = TRUE)
data_new[is.na(data_new)] <- 0

data_wide_diff<-data_new %>% select(event_date,product_content_id,diff) %>% pivot_wider(names_from=product_content_id,values_from=diff) %>% as.data.table()
data_wide<-data %>% select(event_date,product_content_id,sold_count) %>% pivot_wider(names_from=product_content_id,values_from=sold_count)

starting_product_list=c(as.Date("2019-09-22"),as.Date("2019-11-23"),as.Date("2019-05-11"),as.Date("2019-09-09"),as.Date("2019-05-12")
,as.Date("2019-06-20"), as.Date("2019-07-27"), as.Date("2019-04-30"))

train_data<-data_new %>% filter(forecast_date<"2020-05-01")
test_data<-data_new %>% filter(forecast_date<forecast_day-1 & forecast_date>="2020-05-01") %>% as.data.table()
prediction_data<-data_new %>% filter(forecast_date>forecast_day-1) %>% as.data.table()
test_dates=test_data$forecast_date %>% unique()

#naive
test_data[,my_forecast:=(0.5*last1+0.5*mean)]
naive_check<-test_data %>% select(forecast_date,product_content_id,my_forecast,forecast_day_sales) %>% as.data.table()
naive_check[,AE:=abs(my_forecast-forecast_day_sales)]
naive_check[,APE:=abs(my_forecast-forecast_day_sales)/forecast_day_sales]
naive_check[,APE_mean:=mean(AE),by=.(product_content_id)]
naive_check[,mean_sales:=mean(forecast_day_sales),by=.(product_content_id)]
naive_summary<-naive_check[,c("product_content_id","APE_mean","mean_sales")]
error_comp<-naive_summary %>% mutate(error_spesiyal=APE_mean/mean_sales) %>% unique() %>% select(product_content_id,error_spesiyal)

# basic_arima_forecast
final_arima_forecast_list<-data.table()

AE_arima<-0
APE_arima<-0

for(i in 1:length(id_list)){
starting_date_temp=starting_product_list[i]
data_on_hand<-data_new %>% filter(event_date>=starting_date_temp & product_content_id==id_list[i])
for(j in 1:length(test_dates)){
  temp_list<-data.table()
  temp_date=test_dates[j]
  temp_list[,"test_date"]<-temp_date
  temp_list[,"product_content_id"]<-id_list[i]
  arima_fit = data_on_hand %>% filter(forecast_date<=temp_date) %>% select(sold_count) %>% ts() %>% auto.arima() %>%forecast(2)
  fact<-data_on_hand %>% filter(event_date==temp_date)%>% select(sold_count)
  forecast<-arima_fit$mean[2]
  AE_arima=as.numeric(abs(forecast-fact))+AE_arima
  APE_arima=as.numeric(abs((forecast-fact)/fact))+APE_arima
  temp_list[,"arima_prediction"]<-forecast
  temp_list[,"real_result"]<-fact
  final_arima_forecast_list<-rbind(final_arima_forecast_list,temp_list)
}
}
final_arima_forecast_list[,AE:=abs(arima_prediction-real_result)]
final_arima_forecast_list[,APE:=abs(arima_prediction-real_result)/real_result]
final_arima_forecast_list[,APE_mean:=mean(AE),by=.(product_content_id)]
final_arima_forecast_list[,mean_sales:=mean(real_result),by=.(product_content_id)]
final_summary<-final_arima_forecast_list[,c("product_content_id","APE_mean","mean_sales")]
error_comp<-final_summary %>% mutate(error_arima=APE_mean/mean_sales) %>% unique() %>% select(product_content_id,error_arima) %>% right_join(error_comp)

# diff_arima_forecast
final_diff_arima_forecast_list<-data.table()

AE_arima_diff<-0
APE_arima_diff<-0

for(i in 1:length(id_list)){
  starting_date_temp=starting_product_list[i]
  data_on_hand<-data_new %>% filter(event_date>=starting_date_temp & product_content_id==id_list[i])
  for(j in 1:length(test_dates)){
    temp_list<-data.table()
    temp_date=test_dates[j]
    temp_list[,"test_date"]<-temp_date
    temp_list[,"product_content_id"]<-id_list[i]
    arima_fit = data_on_hand %>% filter(forecast_date<=temp_date) %>% select(diff) %>% ts() %>% auto.arima()%>%forecast(2)
    fact<-data_on_hand %>% filter(forecast_date==temp_date+2) %>% select(sold_count)
    last_obs= data_on_hand %>% filter(forecast_date==temp_date) %>% select(sold_count)
    forecast<-arima_fit$mean[2]+arima_fit$mean[1]+last_obs
    AE_arima_diff=as.numeric(abs(forecast-fact))+AE_arima_diff
    APE_arima_diff=as.numeric(abs((forecast-fact)/fact))+APE_arima_diff
    temp_list[,"arima_diff_prediction"]<-forecast
    temp_list[,"real_result"]<-fact
    final_diff_arima_forecast_list<-rbind(final_diff_arima_forecast_list,temp_list)
  }
}
final_diff_arima_forecast_list[,AE:=abs(arima_diff_prediction-real_result)]
final_diff_arima_forecast_list[,APE:=abs(arima_diff_prediction-real_result)/real_result]
final_diff_arima_forecast_list[,APE_mean:=mean(AE),by=.(product_content_id)]
final_diff_arima_forecast_list[,mean_sales:=mean(real_result),by=.(product_content_id)]
final_diff_summary<-final_diff_arima_forecast_list[,c("product_content_id","APE_mean","mean_sales")]
error_comp<-final_diff_summary %>% mutate(error_diff_arima=APE_mean/mean_sales) %>% unique()%>%select(product_content_id,mean_sales,error_diff_arima) %>% right_join(error_comp)



# forecast_lr
final_lr_forecast_list<-data.table()

AE_lr<-0
APE_lr<-0

for(i in 1:length(id_list)){
  starting_date_temp=starting_product_list[i]
  data_on_hand<-data_new %>% filter(event_date>=starting_date_temp & product_content_id==id_list[i])
  for(j in 1:length(test_dates)){
    temp_list<-data.table()
    temp_date=test_dates[j]
    temp_list[,"test_date"]<-temp_date
    temp_list[,"product_content_id"]<-id_list[i]
    filtered_data<-data_on_hand %>% filter(forecast_date<temp_date)
    fitted<-lm(forecast_day_sales ~ last1+last2+last7+last_4_week_diff+favored_count,filtered_data)
    to_test<-data_on_hand %>% filter(forecast_date==temp_date)
    fact=to_test$forecast_day_sales
    forecast<-predict(fitted,to_test)
    AE_lr=as.numeric(abs(forecast-fact))+AE_lr
    APE_lr=as.numeric(abs((forecast-fact)/fact))+APE_lr
    temp_list[,"lr_prediction"]<-forecast
    temp_list[,"real_result"]<-fact
    final_lr_forecast_list<-rbind(final_lr_forecast_list,temp_list)
  }
}
final_lr_forecast_list[,AE:=abs(lr_prediction-real_result)]
final_lr_forecast_list[,APE:=abs(lr_prediction-real_result)/real_result]
final_lr_forecast_list[,APE_mean:=mean(AE),by=.(product_content_id)]
final_lr_forecast_list[,mean_sales:=mean(real_result),by=.(product_content_id)]
final_lr_summary<-final_lr_forecast_list[,c("product_content_id","APE_mean","mean_sales")] %>% unique()
error_comp<-final_lr_summary %>% mutate(error_lr=APE_mean/mean_sales) %>% unique()%>%select(product_content_id,mean_sales,error_lr) %>% right_join(error_comp)

# forecast ensemble
lr_forecast_short<-final_lr_forecast_list %>% select(test_date,product_content_id,lr_prediction,real_result)
diff_arima_forecast_short<-final_diff_arima_forecast_list %>% select(test_date,product_content_id,arima_diff_prediction,real_result) %>% right_join(lr_forecast_short)
arima_forecast_short<-final_arima_forecast_list %>% select(test_date,product_content_id,arima_prediction,real_result) %>% right_join(diff_arima_forecast_short)
naive_corrected<-naive_check %>% select(forecast_date,product_content_id,my_forecast)
colnames(naive_corrected)<-c("test_date","product_content_id","my_forecast")
ensemble_short<-naive_corrected %>% right_join(arima_forecast_short)

# group by product
final_overall_forecast<-data.table()
for(j in 0:11){
for(i in 1:length(id_list)){
  temp_ensemble_train<-ensemble_short %>% filter(product_content_id==id_list[i] & test_date<(as.Date("2020-05-20")+j))
  temp_ensemble_test<-ensemble_short %>% filter(product_content_id==id_list[i] & test_date==(as.Date("2020-05-20")+j))
  ensemble_fit<-lm(real_result ~ my_forecast+arima_prediction+lr_prediction-1,temp_ensemble_train)
  temp_ensemble_test[,"ensemble_forecast"]<-predict(ensemble_fit,temp_ensemble_test)
  final_overall_forecast<-rbind(final_overall_forecast,temp_ensemble_test)
}
}

final_overall_forecast[,AE_my:=abs(my_forecast-real_result)]
final_overall_forecast[,AE_arima:=abs(arima_prediction-real_result)]
final_overall_forecast[,AE_arima_diff:=abs(arima_diff_prediction-real_result)]
final_overall_forecast[,AE_lr:=abs(lr_prediction-real_result)]
final_overall_forecast[,AE_ensemble:=abs(ensemble_forecast-real_result)]

final_overall_forecast[,AE_my_mean:=mean(AE_my)/mean(real_result),by=.(product_content_id)]
final_overall_forecast[,AE_arima_mean:=mean(AE_arima)/mean(real_result),by=.(product_content_id)]
final_overall_forecast[,AE_arima_diff_mean:=mean(AE_arima_diff)/mean(real_result),by=.(product_content_id)]
final_overall_forecast[,AE_lr_mean:=mean(AE_lr)/mean(real_result),by=.(product_content_id)]
final_overall_forecast[,AE_ensemble_mean:=mean(AE_ensemble)/mean(real_result),by=.(product_content_id)]

final_overall_forecast[,mean_sales:=mean(real_result),by=.(product_content_id)]

model_comparison<-final_overall_forecast %>% select(mean_sales, AE_my_mean,AE_arima_mean,AE_arima_diff_mean,AE_lr_mean,AE_ensemble_mean) %>% unique() %>% cbind(id_list)




# prediction
prediction_data[,my_forecast:=(0.5*last1+0.5*mean)]
day_predictions<-prediction_data[,c("product_content_id","my_forecast","last1","last2","mean","favored_count")]

#linear regression
for(i in 1:length(id_list)){
  starting_date_temp=starting_product_list[i]
  data_on_hand<-data_new %>% filter(event_date>=starting_date_temp & product_content_id==id_list[i])
  fitted<-lm(forecast_day_sales ~ last1+last2+last7+last_4_week_diff+favored_count,data_on_hand)
  lr_f<-predict(fitted,prediction_data[product_content_id==id_list[i]])
  day_predictions[product_content_id==id_list[i],lr_forecast:=lr_f]
}
#arima
for(i in 1:length(id_list)){
  starting_date_temp=starting_product_list[i]
  data_on_hand<-data_new %>% filter(event_date>=starting_date_temp & product_content_id==id_list[i])
  arima.fit<-data_on_hand %>% select(sold_count) %>% auto.arima()
  arima_f<-forecast(arima.fit,2)
  day_predictions[product_content_id==id_list[i],arima_forecast:=arima_f$mean[2]]
}

predictions=unique(data[,list(product_content_id)])
#manual
forecast_1<-day_predictions[product_content_id==id_list[1],"my_forecast"]
predictions[product_content_id==id_list[1],forecast:=forecast_1]

forecast_2<-day_predictions[product_content_id==id_list[2],"my_forecast"]
predictions[product_content_id==id_list[2],forecast:=forecast_2]

predictions[product_content_id==id_list[3],forecast:=0]
forecast_4<-day_predictions[product_content_id==id_list[4],"my_forecast"]
predictions[product_content_id==id_list[4],forecast:=forecast_4]
predictions[product_content_id==id_list[5],forecast:=1]
#manual
forecast_6<-day_predictions[product_content_id==id_list[6],"my_forecast"]
predictions[product_content_id==id_list[6],forecast:=forecast_6]

forecast_7<-day_predictions[product_content_id==id_list[7],"my_forecast"]
predictions[product_content_id==id_list[7],forecast:=forecast_7]
forecast_8<-day_predictions[product_content_id==id_list[8],"lr_forecast"]
predictions[product_content_id==id_list[8],forecast:=forecast_8]

send_submission(predictions, token, url=subm_url, submit_now=T)


###
library(pracma)
for(j in 0:11){
  for(i in 1:length(id_list)){
    temp_ensemble_train<-ensemble_short %>% filter(product_content_id==id_list[i] & test_date<(as.Date("2020-05-20")+j))
    temp_ensemble_test<-ensemble_short %>% filter(product_content_id==id_list[i] & test_date==(as.Date("2020-05-20")+j))
    ensemble_fit<-lsqlincon(data.matrix(temp_ensemble_train[,c("my_forecast","arima_prediction","lr_prediction")]),d=temp_ensemble_train$real_result,,Aeq=c(1,1,1),beq=1)
    temp_ensemble_test[,"ensemble_forecast"]<-sum(temp_ensemble_test[,c("my_forecast","arima_prediction","lr_prediction")]*ensemble_fit)
    final_overall_forecast<-rbind(final_overall_forecast,temp_ensemble_test)
  }
}


