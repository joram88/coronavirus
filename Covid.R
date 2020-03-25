library(readxl)
library(tidyverse)
library(downloader)
library(lubridate)
library(scales)
library(ggthemes)

today <- today()

#Update Date below for file

raw <- read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv")

covid <- gather(raw, key = "Date", value="Value", -c(`Province/State`,`Country/Region`,Lat,Long))

covid$Date <- mdy(covid$Date)

covid$country <- covid$`Country/Region`
covid$`Country/Region` <- NULL
covid$state <- covid$`Province/State`
covid$`Province/State` <- NULL
covid$Value <- as.numeric(covid$Value)

covid <- covid %>%  
        group_by(country, Date) %>% 
        summarize(Value=sum(Value)) %>% 
        select(country, Date, Value) 
        


CHN <- covid %>% 
        filter(country=="China") %>% 
        group_by(Date,country) %>% 
        summarize(Value=sum(Value))

ITA <- covid %>% 
        filter(country=="Italy") %>% 
        group_by(Date,country) %>% 
        summarize(Value=sum(Value))

USA <- covid %>% 
        filter(country=="US") %>% 
        group_by(Date,country) %>% 
        summarize(Value=sum(Value))

IRA <- covid %>% 
        filter(country=="Iran") %>% 
        group_by(Date,country) %>% 
        summarize(Value=sum(Value))

SKA <- covid %>% 
        filter(country=="Korea, South") %>% 
        group_by(Date,country) %>% 
        summarize(Value=sum(Value))

RUS <- covid %>% 
        filter(country=="Russia") %>% 
        group_by(Date,country) %>% 
        summarize(Value=sum(Value))



SPA <- covid %>% 
        filter(country=="Spain") %>% 
        group_by(Date,country) %>% 
        summarize(Value=sum(Value))


GER <- covid %>% 
        filter(country=="Germany") %>% 
        group_by(Date,country) %>% 
        summarize(Value=sum(Value))


FRA <- covid %>% 
        filter(country=="France") %>% 
        group_by(Date,country) %>% 
        summarize(Value=sum(Value))


SWI <- covid %>% 
        filter(country=="Switzerland") %>% 
        group_by(Date,country) %>% 
        summarize(Value=sum(Value))


CRU <- covid %>% 
        filter(country=="Cruise Ship") %>% 
        group_by(Date,country) %>% 
        summarize(Value=sum(Value))


UK <- covid %>% 
        filter(country=="United Kingdom") %>% 
        group_by(Date,country) %>% 
        summarize(Value=sum(Value))


NED <- covid %>% 
        filter(country=="Netherlands") %>% 
        group_by(Date,country) %>% 
        summarize(Value=sum(Value))


JAP <- covid %>% 
        filter(country=="Japan") %>% 
        group_by(Date,country) %>% 
        summarize(Value=sum(Value))


NOR <- covid %>% 
        filter(country=="Norway") %>% 
        group_by(Date,country) %>% 
        summarize(Value=sum(Value))


SWE <- covid %>% 
        filter(country=="Sweden") %>% 
        group_by(Date,country) %>% 
        summarize(Value=sum(Value))


BEL <- covid %>% 
        filter(country=="Belgiuim") %>% 
        group_by(Date,country) %>% 
        summarize(Value=sum(Value))


AUS <- covid %>% 
        filter(country=="Austria") %>% 
        group_by(Date,country) %>% 
        summarize(Value=sum(Value))


DEN <- covid %>% 
        filter(country=="Denmark") %>% 
        group_by(Date,country) %>% 
        summarize(Value=sum(Value))


MAL <- covid %>% 
        filter(country=="Malaysia") %>% 
        group_by(Date,country) %>% 
        summarize(Value=sum(Value))


SNG <- covid %>% 
        filter(country=="Singapore") %>% 
        group_by(Date,country) %>% 
        summarize(Value=sum(Value))

MEX <- covid %>% 
        filter(country=="Mexico") %>% 
        group_by(Date,country) %>% 
        summarize(Value=sum(Value))

covid_ctry <- rbind(CHN, ITA, USA, IRA, SKA, SPA, GER, FRA, SWI, CRU,
                    UK, NED, JAP, NOR, SWE, BEL, AUS, DEN, MAL, SNG, MEX)
covid_ctry <- covid_ctry %>% 
        mutate(lValue = log(Value)) %>% 
        group_by(country) %>% 
        mutate(rate = (Value-lag(Value))/lag(Value))
        

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


#Covid Maps

covid_ctry %>%
        
        filter(country==c("China", "Italy", "US", "Mexico")) %>% 
        
        
        ggplot(aes(x = Date, y = lValue, group_by(country), color = country))+
        geom_smooth()

covid_ctry %>%
        
        filter(country==c("China", "Italy", "US", "Mexico")) %>% 
        
        ggplot(aes(x = Date, y = Value, group_by(country), color = country))+
        geom_line(size = 1)

#Rate graph

covid_ctry %>%
        filter(rate<Inf & Date > "2020-03-01") %>% 
        filter(country %in% c("United Kingdom", "US", "Italy", "China", "Mexico")) %>% 
        ggplot(aes(x = Date, y = rate, group_by(country), color = country))+
        geom_point(size=3)+
        geom_line(size=1.1)+
        scale_y_continuous(name="Growth rate", labels = percent)+
        labs(title = "Growth Rate in Number of Confirmed Cases",
             subtitle = "As of 2020/03/21")


ttd <- covid_ctry %>% 
        group_by(country) %>% 
        filter(rate<Inf & rate>-Inf) %>% 
        arrange(desc(Date)) %>% 
        filter(row_number()==1) %>% 
        mutate(ttd = 72/(rate*100)) %>% 
        mutate(pred_case=(Value*(1+rate)^14)) %>% 
        mutate(pred_case3=(Value*(1+rate)^21))
        

ttd %>% 
        filter(country %in% c("United Kingdom", "US", "Mexico", "Switzerland" , 
                          "Germany", "Netherlands", "Austria" ,"France", "Italy", 
                          "Malaysia", "Sweden", "Spain")) %>% 
        ggplot(aes(x = reorder(country, ttd), y = ttd, group_by(country), color = country,))+
        geom_bar(stat = "identity", aes(fill=country))+
        theme(legend.position = "none")+
        theme(axis.text.x = element_text(face="bold", 
                                         size=10, angle=45))+
        labs(title = "Time for Confirmed Covid-19 Cases to Double (days)",
             subtitle = last(covid$Date))+
        xlab ("Country")+ ylab ("Time to double (days)")

#China is 621 days ttd currently

two_week_forecast <- ttd %>% 
        select(country, Value, pred_case) %>% 
        gather(key="type", value="value",-country) %>% 
        arrange(country)


two_week_forecast %>% 
        filter(country %in% c("United Kingdom", "US", "Mexico", "Switzerland" , 
                              "Germany", "Netherlands", "Austria" ,"France", "Italy", 
                              "Malaysia", "Sweden", "Spain", "China")) %>% 
        
        ggplot(aes(fill = type, x = reorder(country, value), y = value))+
                        geom_bar(position = "stack", stat = "identity")+
        theme(axis.text.x = element_text(face="bold", 
                                         size=10, angle=45))+
        labs(title = "Expected Cases in 14 Days at Current Infection Rates",
             subtitle = last(covid$Date))+
        xlab ("Country")+
        scale_fill_discrete(name = "Cases", labels = c("Predicted", "Current"))+
        scale_y_continuous(name="Number of cases", labels=comma)

three_week_forecast <- ttd %>% 
        select(country, Value, pred_case3) %>% 
        gather(key="type", value="value",-country) %>% 
        arrange(country)


three_week_forecast %>% 
        filter(country %in% c("United Kingdom", "US", "Mexico", "Switzerland" , 
                              "Germany", "Netherlands", "Austria" ,"France", "Italy", 
                              "Malaysia", "Sweden", "Spain", "China")) %>% 
        
        ggplot(aes(fill = type, x = reorder(country, value), y = value))+
        geom_bar(position = "stack", stat = "identity")+
        theme(axis.text.x = element_text(face="bold", 
                                         size=10, angle=45))+
        labs(title = "Expected Cases in 21 Days at Current Infection Rates",
             subtitle = last(covid$Date))+
        xlab ("Country")+
        scale_fill_discrete(name = "Cases", labels = c("Predicted", "Current"))+
        scale_y_continuous(name="Number of cases", labels=comma)

