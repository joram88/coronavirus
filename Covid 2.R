library(readxl)
library(tidyverse)
library(modlr)
library(downloader)
library(lubridate)
library(scales)
library(ggthemes)

today <- today()

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

worst <-head(covid %>% 
                     group_by(country) %>% 
                     summarize(Value=sum(Value)) %>% 
                     arrange(desc(Value)), 20)

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

FIN <- covid %>% 
        filter(country == "Finland") %>% 
        group_by(Date, country) %>% 
        summarize(Value=sum(Value))

covid_ctry <- rbind(CHN, ITA, USA, IRA, SKA, SPA, GER, FRA, SWI, CRU,
                    UK, NED, JAP, NOR, SWE, BEL, AUS, DEN, MAL, SNG, MEX, FIN)
covid_ctry <- covid_ctry %>% 
        mutate(lValue = log(Value)) %>% 
        group_by(country) %>% 
        mutate(rate = (Value-lag(Value))/lag(Value))
        

qplot(data = covid_ctry, x = Date, y = (Value), color = country)

date <- ymd("2020-01-22")

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
             subtitle = "As of 2020/03/21")+
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
             subtitle = "As of 2020/03/21")+
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
             subtitle = "As of 2020/03/21")+
        xlab ("Country")+
        scale_fill_discrete(name = "Cases", labels = c("Predicted", "Current"))+
        scale_y_continuous(name="Number of cases", labels=comma)

