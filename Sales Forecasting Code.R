getwd()
setwd("E:/MBA BA3/R/retaildataset")
getwd()
stores<-read.csv("stores_dataset.csv")
stores_df<-stores
sales<-read.csv("sales_dataset.csv")
sales_df<-sales
features<-read.csv("Features_dataset.csv")
features_df<-features
df1<-merge(stores_df,sales_df,by=c("Store"))
final_df<-merge(df1,features_df,by=c("Store","Date","IsHoliday"))
head(df1)
dim(df1)

head(final_df)
dim(final_df)

str(final_df)
summary(final_df)

final_df$Date = as.Date(final_df$Date, format = "%d/%m/%Y")
final_df$Year<- strftime(final_df$Date, format = "%Y")
Month=strftime(final_df$Date, format = "%m")
final_df$MonthYear<-strftime(final_df$Date, format = "%m-%Y")
head(final_df)
dim(final_df)
Year
max(final_df$Year)
min(final_df$Date)
max(final_df$Date)

install.packages("ggplot2")
library(ggplot2)
install.packages("dplyr")
library(dplyr)

final_df %>% group_by(Store, Dept) %>% summarize(count_wk = n_distinct(Date)) %>% ggplot(aes(x = count_wk)) + geom_histogram()
ggplot(stores_df, aes(x = Size)) + geom_histogram(binwidth = 10000) + facet_grid(Type~.)
stores_df %>% group_by(Type) %>% summarize(n())
plot(final_df$Store,final_df$Weekly_Sales)


final_df$IsHoliday[final_df$IsHoliday=="true"]<-1
final_df$IsHoliday[final_df$IsHoliday=="false"]<-0
head(final_df)

final_df[is.na(final_df)]<-0
head(final_df)

summary(final_df$Weekly_Sales)
p<-which(final_df$Weekly_Sales>500000)
p
final_df<-final_df[-p,]
dim(final_df)
boxplot(final_df$Weekly_Sales)

head(final_df$Date)

subset1<-subset(final_df,select=c("Size","Weekly_Sales","Temperature","CPI","Unemployment","Fuel_Price","MarkDown1","MarkDown2","MarkDown3","MarkDown4","MarkDown5"))
cor1<-cor(subset1)
col<-colorRampPalette(c("blue","black","white"))((20))
heatmap(cor1,col=col,symm=TRUE)

subset2<-subset(final_df,select=c("Weekly_Sales","MarkDown1","MarkDown2","MarkDown3","MarkDown4","MarkDown5"))
cor2<-cor(subset2)
cor2
heatmap(cor2,symm=TRUE)

dim(is.na(final_df))
summary(final_df$Weekly_Sales<0)
table(final_df$IsHoliday)


Top_Store_byType<-aggregate(final_df$Weekly_Sales,by=list(Type=final_df$Type), FUN=sum)
Top_Store_byType
stores_sales<-aggregate(final_df$Weekly_Sales,by=list(Type=final_df$Store), FUN=mean)
stores_sales
plot(stores_sales,type="histogram")
qplot(final_df$IsHoliday,final_df$Weekly_Sales)


class(final_df$Date)
head(final_df)
install.packages("lubridate")
library(lubridate)
class(final_df$Date)
head(final_df)
final_df %>% group_by(Store) %>% summarize(avgsales=mean(Weekly_Sales))
final_df %>% group_by(Year) %>% summarize(avgsales= mean(Weekly_Sales))
final_df %>% group_by(Store, MonthYear) %>% summarize(avgsales= mean(Weekly_Sales)) %>% arrange(desc(avgsales))


head(final_df)

install.packages("forecast")
install.packages("curl")
install.packages("tseries")
library(curl)
library(tseries)
library(forecast)

min(final_df$Date)
forecast_data<-ts(final_df$Weekly_Sales,start=c(2010,2),end=c(2012,10),frequency=12)
kpss.test(forecast_data)
plot(forecast_data)
forecast_data
class(forecast_data)
start(forecast_data)
end(forecast_data)

hw<-HoltWinters(forecast_data)
plot(hw)

forecast1 <- forecast(hw, n.ahead = 12, prediction.interval = T, level = 0.95)
forecast1
forecast2 <- predict(hw, n.ahead = 12, prediction.interval = T, level = 0.95)
plot(hw, forecast2)
summary(hw)


AutoArimaModel=auto.arima(forecast_data)
AutoArimaModel
summary(AutoArimaModel)
arimapredict<-predict(AutoArimaModel,12)
arimapredict




install.packages("DMwR")
library(DMwR)

forecastmodel_input<-final_df[,c("Size","Weekly_Sales","Temperature","CPI","Unemployment","Fuel_Price","MarkDown1","MarkDown2","MarkDown3","MarkDown4","MarkDown5")]
head(model_input)
lmmodel1<-lm(Weekly_Sales~.,data=final_df)
print(lmmodel1)
accuracy(lmmodel1)

lmmodel2<-lm(Weekly_Sales~Size+Unemployment+Temperature+CPI+Fuel_Price+MarkDown1+MarkDown2+MarkDown3+MarkDown5,data=model_input)
print(lmmodel2)
accuracy(lmmodel2)
summary(lmmodel2)

lmmodel2predict<-predict(lmmodel2)
lmmodel2predict
x<-mean(lmmodel2predict)
x

