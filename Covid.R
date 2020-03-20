library(readxl)
library(tidyverse)
library(modlr)
library(downloader)
library(lubridate)
library(scales)
library(ggthemes)

url <- "https://data.humdata.org/hxlproxy/data/download/time_series-ncov-Confirmed.csv?dest=data_edit&filter01=explode&explode-header-att01=date&explode-value-att01=value&filter02=rename&rename-oldtag02=%23affected%2Bdate&rename-newtag02=%23date&rename-header02=Date&filter03=rename&rename-oldtag03=%23affected%2Bvalue&rename-newtag03=%23affected%2Binfected%2Bvalue%2Bnum&rename-header03=Value&filter04=clean&clean-date-tags04=%23date&filter05=sort&sort-tags05=%23date&sort-reverse05=on&filter06=sort&sort-tags06=%23country%2Bname%2C%23adm1%2Bname&tagger-match-all=on&tagger-default-tag=%23affected%2Blabel&tagger-01-header=province%2Fstate&tagger-01-tag=%23adm1%2Bname&tagger-02-header=country%2Fregion&tagger-02-tag=%23country%2Bname&tagger-03-header=lat&tagger-03-tag=%23geo%2Blat&tagger-04-header=long&tagger-04-tag=%23geo%2Blon&header-row=1&url=https%3A%2F%2Fraw.githubusercontent.com%2FCSSEGISandData%2FCOVID-19%2Fmaster%2Fcsse_covid_19_data%2Fcsse_covid_19_time_series%2Ftime_series_19-covid-Confirmed.csv"
download(url, dest="time_series-ncov-Confirmed.csv", mode="wb")
covid <- read_csv("time_series-ncov-Confirmed.csv")

covid <- covid[-1,]

covid$country <- covid$`Country/Region`
covid$`Country/Region` <- NULL
covid$state <- covid$`Province/State`
covid$`Province/State` <- NULL
covid$Value <- as.numeric(covid$Value)
covid$Date <- ymd(covid$Date)

covid_ctry <- covid %>% 
        filter(country==c("US", "China")) %>% 
        group_by(Date,country) %>% 
        summarize(Value=sum(Value)) %>% 
        arrange(country) %>% 
        mutate(lValue= log(Value))

qplot(data = covid_ctry, x = Date, y = (Value), color = country)

covid_CHN <- covid_ctry %>% 
        filter(country=="China")

covid_CHN <- covid_CHN[,3]

covid_USA <- covid_ctry %>% 
        filter(country=="US") 

covid_USA <- covid_USA[,3]

covid_USA_ts <- as.ts(covid_USA$Value)


(AR <- arima(covid_CHN, order = c(2,0,2)))
residuals <- AR$residuals

covid_CHN_ts <- as.ts(covid_CHN$Value)
acf(covid_CHN_ts)
pacf(covid_CHN_ts)

CHN_fitted <- covid_CHN_ts - residuals
ts.plot(covid_CHN_ts)
points(CHN_fitted, type = "l", col = 2, lty = 2)

forecast <- predict(AR, n.ahead=30)

ts.plot(covid_CHN_ts, CHN_fitted,forecast$pred, covid_USA_ts, 
        col=c("black","red", "red", "blue"))
abline(h=13677, col="grey")

residuals[1] <- 0
residuals_US <- residuals

USA_fitted <- as.numeric(covid_USA_ts-residuals_US)
USA_fitted[0:23] <- 0

fit1 <- as.numeric(CHN_fitted[8:29])

USA_fitted[30:51] <- fit1
USA_fitted <- as.ts(USA_fitted)

ts.plot(covid_CHN_ts, CHN_fitted,forecast$pred, covid_USA_ts, USA_fitted,
        col=c("black","red", "red", "blue", "green"))

fit1<- as.numeric(covid_CHN_ts)
fit1[23:70] <- NA
fit2 <- as.numeric(CHN_fitted)
fit2[29:70] <- last(fit2)
fit3 <- as.numeric(forecast$pred)
fit3[0:30] <- NA
fit3[60:70] <- last(forecast$pred)
fit4 <- as.numeric(covid_USA_ts)
fit4[30:70] <- NA
fit5 <- as.numeric(USA_fitted)
fit5[52:70] <- last(fit5)

date <- ymd("2020-01-22")

time <- 1:140
time <- time[c(TRUE, FALSE)]

final_df<- data.frame(fit1,fit2,fit3,fit4,fit5, days =time)



final_df$date <- ymd("2020-01-22")+ final_df$days
final_df$fit3 <- NULL

final_df2 <- final_df %>% 
        gather(key = type, value = value, -date, -days) %>% 
        mutate(group = ifelse(type=="fit1", "China Actual",
                      ifelse(type=="fit2", "China Forecast",
                             ifelse(type=="fit4", "US Actual",
                                    "US Forecast"))))


ggplot()+
        geom_line(data = final_df2[final_df2$type=="fit1",], aes(x = date, y = value, color = group), size = 5)+
        geom_line(data = final_df2[final_df2$type=="fit2",], aes(x = date, y = value, color = group), size = 2)+
        geom_line(data = final_df2[final_df2$type=="fit4",], aes(x = date, y = value, color = group), size = 5)+
        geom_line(data = final_df2[final_df2$type=="fit5",], aes(x = date, y = value, color = group), size = 2)+
        labs(title="Coronavirus Model - USA vs China", x = "Date", caption = "Source: Humanitarian Data Exchange",
             subtitle = "By JRP (2020/03/20)")+
        theme_economist()+
        scale_y_continuous(name="Confirmed Cases", labels = comma)+
        theme(legend.title=element_blank())

