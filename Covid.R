library(tidyverse)
library(lubridate)
library(scales)
library(ggthemes)

today <- today()

#Update Date below for file

raw <- read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv")
raw_death <- read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv")

covid <- gather(raw, key = "Date", value="Value", -c(`Province/State`,`Country/Region`,Lat,Long))
covid_death <- gather(raw_death, key = "Date", value="Value", -c(`Province/State`,`Country/Region`,Lat,Long))

covid$Date <- mdy(covid$Date)
covid_death$Date <- mdy(covid_death$Date)

covid$country <- covid$`Country/Region`
covid$`Country/Region` <- NULL
covid$state <- covid$`Province/State`
covid$`Province/State` <- NULL
covid$Value <- as.numeric(covid$Value)



covid_death$country <- covid_death$`Country/Region`
covid_death$`Country/Region` <- NULL
covid_death$state <- covid_death$`Province/State`
covid_death$`Province/State` <- NULL
covid_death$Value <- as.numeric(covid_death$Value)


covid <- covid %>%  
        group_by(country, Date) %>% 
        summarize(Value=sum(Value)) %>% 
        select(country, Date, Value) %>% 
        mutate(new_cases = Value - lag(Value))

covid$Week <- week(covid$Date)

covid_death <- covid_death %>%  
        group_by(country, Date) %>% 
        summarize(Value=sum(Value)) %>% 
        select(country, Date, Value) %>% 
        mutate(new_deaths = Value - lag(Value))

covid_death$Week <-  week(covid_death$Date)

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
        summarize(Value=sum(Value)) %>% 
        mutate(Week = week(Date))

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

#Create log of value and moving average for rate

ma <- function(x, n = 5){stats::filter(x, rep(1 / n, n), sides = 1)}

covid_ctry <- covid_ctry %>% 
        mutate(lValue = log(Value)) %>% 
        group_by(country) %>% 
        mutate(rate = (Value-lag(Value))/lag(Value)) %>% 
        mutate(mov_avg = ma(rate, n = 10))

#Covid Maps

covid_ctry %>%
        filter(country %in% c("China", "Italy", "US", "Mexico", "Spain", "Japan", "Iran", "Sweden", "Korea, South")) %>% 
        filter(rate<.4) %>% 
        ggplot(aes(x = Date, y = rate, group_by(country), color = country))+
        stat_smooth(se=FALSE, method = "loess", size = 1.5)+
        scale_y_continuous(name="Percent Change in cases (smoothed)", labels=percent)+
        labs(title = "Smoothed Rate of Change in Cases")

covid_ctry %>%
                filter(country %in% c("China", "Italy", "US", "Mexico", "Spain", "Japan", "Iran", "Sweden", "Korea, South")) %>% 
                ggplot(aes(x = Date, y = lValue, group_by(country), color = country))+
                geom_line(size =1.5)+
        scale_y_continuous(name="Logarithmic Confirmed Cases by Country", labels=comma)+
        labs(title = "Logarithmic Scaled Value ")


covid_ctry %>%
        filter(country %in% c("China", "Italy", "US", "Mexico", "Spain", "Japan", "Iran", "Sweden", "Korea, South")) %>% 
        ggplot(aes(x = Date, y = Value, group_by(country), color = country))+
        geom_line(size = 1.5)+
        scale_y_continuous(name = "Confirmed Cases", labels=comma)+
        labs(title = "Total Confirmed Cases")


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

#Using 10 day moving average now instead of just rate

ttd <- covid_ctry %>% 
        group_by(country) %>% 
        arrange(desc(Date)) %>% 
        filter(row_number()==1) %>% 
        mutate(ttd = 72/(mov_avg*100)) %>% 
        mutate(pred_case=(Value*(1+mov_avg)^14)) %>% 
        mutate(pred_case3=(Value*(1+mov_avg)^21)) 
        

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


us_dead <- covid_death %>% 
        filter(country %in% c("US"))


MEX_dead <- covid_death %>% 
        filter(country %in% c("Mexico"))

ggplot(data = us_dead, aes(x = Date, y = Value))+
        geom_line()+
        geom_point()+
        labs(title = "Deaths in the US")

covid_ctry %>% 
        filter(Date>"2020-03-01", country =="US") %>% 
        mutate(slope = (lValue - lag(lValue))/lag(lValue)) %>% 
        ggplot(aes(x = Date, y=slope))+
        geom_line()+
        geom_smooth()+
        labs(title = "Rate of Change in log(Value) for USA")

covid_ctry %>% 
        filter(Date>"2020-03-01", country =="Italy") %>% 
        mutate(slope = (lValue - lag(lValue))/lag(lValue)) %>% 
        ggplot(aes(x = Date, y=slope))+
        geom_line()+
        geom_smooth()+
        labs(title = "Rate of Change in log(Value) for Italy")

covid_ctry %>% 
        filter(Date>"2020-03-01", country =="Korea, South") %>% 
        mutate(slope = (lValue - lag(lValue))/lag(lValue)) %>% 
        ggplot(aes(x = Date, y=slope))+
        geom_line()+
        geom_smooth()+
        labs(title = "Rate of Change in log(Value) for South Korea")

#Next graphs: 

#New cases per week (Y axis) ~ Total confirmed cases (X axis)

weekly <- covid %>% 
        filter(country %in% c("US", "Mexico", "Korea, South")) %>% 
        group_by(country, Week) %>% 
        summarize(new_cases = sum(new_cases))

drop <- merge(covid, weekly, by = c("country", "Week"))

drop <- drop %>% 
        group_by(Week) %>% 
        mutate(Max = max(Value)) %>% 
        ungroup(Week) %>% 
        filter(Week != max(drop$Week)) 
#Won't consider most recent week until a new one has started

drop %>% 
        filter(log(Max)>3) %>% 
ggplot(aes(x = log(Max), y = log(new_cases.y), group = country, color = country))+
        geom_point()+
        labs(title = "First signs of a slow down in new cases the US")+
        xlab("New Cases by Week (log)")+
        ylab("Cumulative Confirmed Cases (log)")+
        geom_smooth()

covid %>% 
        filter(country %in% c("China", "Italy", "US", "Mexico", "Spain", "Japan", "Iran", "Sweden", "Korea, South")) %>% 
        ggplot(aes(x = Date,y = new_cases, color = country))+
        geom_line()+
        labs(title = "New daily cases")+
        ylab("New Daily Cases")

covid %>% 
        filter(country %in% c("China", "Italy", "US", "Mexico", "Spain", "Japan", "Iran", "Sweden", "Korea, South")) %>% 
        ggplot(aes(x = Date,y = new_cases, color = country))+
        geom_line()+
        labs(title = "New daily cases")+
        ylab("New Daily Cases")

covid %>% 
        filter(country == "US", Date > "2020-03-15") %>% 
        ggplot(aes(x = Date,y = new_cases))+
        geom_bar(stat="identity")+
        labs(title = "New daily cases in the US")+
        ylab("New Daily Cases")+
        geom_smooth(se = FALSE)


 us_dead %>% 
        filter(Date > "2020-03-15") %>%
        #filter(new_deaths < 2900) %>%  #Removes the two outliers due to NY adjustments
        ggplot(aes(x = Date,y = new_deaths))+
        geom_bar(stat="identity")+
        labs(title = "New daily deaths in the US")+
        ylab("New Daily Deaths")+
        geom_smooth(se = FALSE)
