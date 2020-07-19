library(tidyverse)
library(lubridate)
library(scales)
library(ggthemes)
library(RColorBrewer)
library(gapminder)
library(plotly)

today <- today()

#Update Date below for file

raw <- read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv")
raw_death <- read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv")
raw_usa <- read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_US.csv")
raw_usa_death <- read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_US.csv")
raw_usa_pop <- read_csv("http://www2.census.gov/programs-surveys/popest/datasets/2010-2019/national/totals/nst-est2019-popchg2010_2019.csv?#")

dcmetro <- c("24021", "24031", "11001", "24009", "24017", "24033",
             "51013", "51043", "51047", "51059", "51061", "51107",
             "51113", "51153", "51157", "51179", "51187", "51510",
             "51600", "51610", "51630", "51683", "51685", "54037")

raw_dcmetro <- raw_usa %>% 
        filter(FIPS %in% dcmetro )

covid_dc  <-  raw_dcmetro %>% 
        select(-c("UID", "iso2", "iso3", "code3", "FIPS", "Admin2", "Combined_Key", 
                  "Province_State", "Country_Region", "Lat", "Long_"))

raw_usa  <-  raw_usa %>% 
        select(-c("UID", "iso2", "iso3", "code3", "FIPS", "Admin2", "Combined_Key"))

gap <- gapminder %>% 
        filter(year == max(year)) %>% 
        select(country, pop, gdpPercap)

gap <- gap %>% 
        mutate(country = str_replace(country, "United States", "US")) %>% 
        mutate(country = str_replace(country, "Korea, Rep.", "Korea, South"))

russia_pop <- 144386830
russia_gdppc <- 11289

russia<-data.frame("Russia", 144386830, 11289)
names(russia)<-c("country","pop", "gdpPercap")

gap <- rbind(gap, russia)

usa_pop <- raw_usa_pop %>% 
        select("NAME", "POPESTIMATE2019") %>% 
        rename("state" = "NAME", "pop" = "POPESTIMATE2019")

covid <- gather(raw, key = "Date", value="Value", -c(`Province/State`,`Country/Region`,Lat,Long))
covid_death <- gather(raw_death, key = "Date", value="Value", -c(`Province/State`,`Country/Region`,Lat,Long))
covid_usa <- gather(raw_usa, key = "Date", value="Value", -c(`Province_State`,`Country_Region`,Lat,Long_)) %>% 
        rename(state = Province_State)
covid_dc <- gather(covid_dc, key = "Date", value="Value")
dc_counties <- gather(raw_dcmetro, key = "Date", value="Value", -c(Admin2, UID, iso2, iso3, code3, FIPS,
                                                                   Province_State, Country_Region, Lat,
                                                                   Long_, Combined_Key))

dc_counties <- dc_counties %>% 
        select(Admin2, Combined_Key, Date, Value) %>% 
        arrange(Admin2, Combined_Key)

covid$Date <- mdy(covid$Date)
covid_death$Date <- mdy(covid_death$Date)
covid_usa$Date <- mdy(covid_usa$Date)
covid_dc$Date <- mdy(covid_dc$Date)
dc_counties$Date <- mdy(dc_counties$Date)

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

covid <- merge(x = covid, y = gap, by = "country", all = TRUE)

covid_death <- covid_death %>%  
        group_by(country, Date) %>% 
        summarize(Value=sum(Value)) %>% 
        select(country, Date, Value) %>% 
        mutate(new_deaths = Value - lag(Value))

covid_death <- merge(x = covid_death, y = gap, by = "country", all = TRUE)

covid_death$Week <-  week(covid_death$Date)

covid_usa <- covid_usa %>%  
        group_by(state, Date) %>% 
        summarize(Value=sum(Value)) %>% 
        select(state, Date, Value) %>% 
        mutate(new_cases = Value - lag(Value))

covid_usa$Week <-  week(covid_usa$Date)

covid_dc <- covid_dc %>%  
        group_by(Date) %>% 
        summarize(Value=sum(Value)) %>% 
        select(Date, Value) %>% 
        mutate(new_cases = Value - lag(Value))

covid_dc$Week <- week(covid_dc$Date)

dc_counties <- dc_counties %>%  
        group_by(Admin2, Date) %>% 
        summarize(Value=sum(Value)) %>% 
        select(Admin2, Date, Value) %>% 
        mutate(new_cases = Value - lag(Value))

dc_counties$Week <- week(dc_counties$Date)

#Create log of value and moving average for rate

ma <- function(x, n = 5){stats::filter(x, rep(1 / n, n), sides = 1)}

covid <- covid %>% 
        mutate(lValue = log(Value)) %>% 
        group_by(country) %>% 
        mutate(rate = (Value-lag(Value))/lag(Value)) %>% 
        mutate(mov_avg = ma(rate, n = 10))

covid_usa <- covid_usa %>% 
        mutate(lValue = log(Value)) %>% 
        group_by(state) %>% 
        mutate(rate = (Value-lag(Value))/lag(Value)) %>% 
        mutate(mov_avg = ma(rate, n = 10))

#Covid Graphs

covid %>%
        filter(country %in% c("China", "Italy", "US", "Mexico", "Spain", "Japan", "Sweden", "Korea, South", "Russia", "Brazil")) %>%
        filter(Date > "2020-03-01") %>% 
        ggplot(aes(x = Date, y = (Value/(pop/1000000)), group_by(country), color = country))+
        geom_line(size = 1.5)+
        scale_y_continuous(name = "Confirmed Cases per capita per million inhabitants", labels=comma)+
        labs(title = "Total Confirmed Cases per capita")

covid %>%
        filter(country %in% c("China", "Italy", "US", "Mexico", "Spain", "Japan", "Sweden", "Korea, South", "Russia", "Brazil")) %>%
        filter(Date > "2020-03-01") %>% 
        ggplot(aes(x = Date, y = Value, group_by(country), color = country))+
        geom_line(size = 1.5)+
        scale_y_continuous(name = "Confirmed Cases", labels=comma)+
        labs(title = "Total Confirmed Cases")

us_dead <- covid_death %>% 
        filter(country %in% c("US"))

MEX_dead <- covid_death %>% 
        filter(country %in% c("Mexico"))


MEX_dead <- covid_death %>% 
        filter(country %in% c("Mexico"))

ggplot(data = us_dead, aes(x = Date, y = Value))+
        geom_line()+
        geom_point()+
        scale_y_continuous(labels = comma)+
        labs(title = "Deaths in the US")#Next graphs: 

#New cases per week (Y axis) ~ Total confirmed cases (X axis)

weekly <- covid %>% 
        filter(country %in% c("US", "Mexico", "Korea, South", "Sweden", "Brazil", "Italy",
                              "China", "Russia", "Japan")) %>% 
        group_by(country, Week) %>% 
        summarize(new_cases = sum(new_cases))

weekly_usa <- covid_usa %>% 
        filter(state %in% c("California", "Florida", "Texas", "Arizona", "District of Columbia",
                              "Virginia", "Maryland", "New York", "Colorado")) %>% 
        group_by(state, Week) %>% 
        summarize(new_cases = sum(new_cases))

drop <- merge(covid, weekly, by = c("country", "Week"))

drop <- drop %>% 
        group_by(Week) %>% 
        mutate(Max = max(Value)) %>% 
        ungroup(Week) %>% 
        filter(Week != max(drop$Week))

drop <- merge(drop, gap, by = "country")

drop_usa <- merge(covid_usa, weekly_usa, by = c("state", "Week"))

drop_usa <- drop_usa %>% 
        group_by(Week) %>% 
        mutate(Max = max(Value)) %>% 
        ungroup(Week) %>% 
        filter(Week != max(drop_usa$Week)) 

drop_usa <- merge(drop_usa, usa_pop, by = "state")

drop_dc <- dc_counties %>% 
        group_by(Week) %>% 
        mutate(Max = max(Value)) %>% 
        ungroup(Week) %>% 
        filter(Week != max(dc_counties$Week)) 


#Won't consider most recent week until a new one has started

drop %>% 
        filter(log(Max)>3) %>% 
        ggplot(aes(y = new_cases.y, x = (Value/1000), group = country, color = country))+
        labs(title = "Trend by Country")+
        scale_y_continuous(name="New Cases by Week", labels=comma)+
        xlab("Confirmed Cases (Thousands)")+
        geom_smooth(se = FALSE)+
        facet_wrap(~country, scales = "free")+
        theme(legend.position = "none")

drop %>% 
        filter(log(Max)>3) %>% 
        ggplot(aes(y = new_cases.y/(pop/1000000), x = Value/(pop/1000000), group = country, color = country))+
        labs(title = "New Cases by Country (per million inhabitants)")+
        scale_y_continuous(name="New Cases by Week", labels=comma)+
        xlab("Confirmed Cases (per million inhabitants)")+
        geom_smooth(se = FALSE)+
        scale_color_manual(values=c("darkgreen", "purple", "red",
                                   "cyan", "black", "#E69F00",
                                   "darkblue", "darkred", "seagreen3"))

drop_usa %>% 
        filter(log(Max)>3) %>% 
        ggplot(aes(y = new_cases.y, x = (Value/1000), group = state, color = state))+
        labs(title = "Trend by State")+
        scale_y_continuous(name="New Cases by Week", labels=comma)+
        xlab("Confirmed Cases (Thousands)")+
        geom_smooth(se = FALSE)+
        facet_wrap(~state, scales = "free")+
        theme(legend.position = "none")

drop_usa %>% 
        filter(log(Max)>3) %>% 
        ggplot(aes(y = ((new_cases.y)/(pop/1000000)), x = ((Value/1000)/(pop/1000000)), 
                   group = state, color = state))+
        labs(title = "New Cases by State per million inhabitants")+
        scale_y_continuous(name="New Cases by Week", labels = comma)+
        xlab("Confirmed Cases (per million inhabitants)")+
        geom_smooth(se = FALSE)

covid %>% 
        filter(country %in% c("China", "Italy", "US", "Mexico", "Spain", 
                              "Japan", "Iran", "Sweden", "Korea, South", "Brazil")) %>% 
        ggplot(aes(x = Date,y = new_cases, color = country))+
        geom_line()+
        labs(title = "New daily cases")+
        ylab("New Daily Cases")+
        scale_color_brewer(palette="Spectral")

covid %>% 
        filter(country == "US", Date > "2020-03-15") %>% 
        ggplot(aes(x = Date,y = new_cases))+
        geom_bar(stat="identity", color = "blue")+
        labs(title = "New daily cases in the US")+
        ylab("New Daily Cases")+
        geom_smooth(se = FALSE)


us_dead %>% 
        filter(Date > "2020-03-15") %>%
        #filter(new_deaths < 2900) %>%  #Removes the two outliers due to NY adjustments
        ggplot(aes(x = Date,y = new_deaths))+
        geom_bar(stat="identity", color = "blue")+
        labs(title = "New daily deaths in the US")+
        ylab("New Daily Deaths")+
        geom_smooth(se = FALSE)

covid %>% 
        filter(country == "Mexico", Date > "2020-03-15") %>% 
        ggplot(aes(x = Date,y = new_cases))+
        geom_bar(stat="identity", color = "green")+
        labs(title = "New daily cases in Mexico")+
        ylab("New Daily Cases")+
        geom_smooth(se = FALSE)


MEX_dead %>% 
        filter(Date > "2020-03-15") %>%
        #filter(new_deaths < 2900) %>%  #Removes the two outliers due to NY adjustments
        ggplot(aes(x = Date,y = new_deaths))+
        geom_bar(stat="identity", color = "green")+
        labs(title = "New daily deaths in Mexico")+
        ylab("New Daily Deaths")+
        geom_smooth(se = FALSE)

covid_usa %>% 
        filter(state == "District of Columbia", Date > "2020-03-15") %>% 
        ggplot(aes(x = Date,y = new_cases))+
        geom_bar(stat="identity", color = "seagreen")+
        labs(title = "New daily cases in DC")+
        ylab("New Daily Cases")+
        geom_smooth(se = FALSE)

covid_usa %>% 
        filter(state == "Maryland", Date > "2020-03-15") %>% 
        ggplot(aes(x = Date,y = new_cases))+
        geom_bar(stat="identity", color = "cyan")+
        labs(title = "New daily cases in MD")+
        ylab("New Daily Cases")+
        geom_smooth(se = FALSE)

covid_usa %>% 
        filter(state == "Virginia", Date > "2020-03-15") %>% 
        ggplot(aes(x = Date,y = new_cases))+
        geom_bar(stat="identity", color = "red")+
        labs(title = "New daily cases in VA")+
        ylab("New Daily Cases")+
        geom_smooth(se = FALSE)

covid_dc %>% 
        filter(Date > "2020-03-15") %>% 
        ggplot(aes(x = Date, y = new_cases))+
        geom_bar(stat = "identity", color = "purple")+
        labs(title = "New daily cases in the DC Metropolitan Area")+
        ylab("New Daily Cases")+
        geom_smooth(se = FALSE)

dc_big <- c("Alexandria", "District of Columbia", "Arlington", "Fairfax",
            "Montgomery", "Prince George's", "Prince William", "Loudoun", "Calvert")

dc_counties %>% 
        filter(Admin2 %in% dc_big) %>% 
        plot_ly(
                type = 'scatter',
                mode = 'markers',
                x = ~Date, 
                y = ~Value, 
                color = ~Admin2,
                text = ~paste("DC Counties Total Cases:", 
                             '<br>County:', Admin2, 'Value', Value)) 

dc_counties %>% 
        filter(Admin2 %in% dc_big) %>% 
        filter(Date>"2020-03-15") %>% 
        ggplot(aes(x = Date, y = new_cases, color = Admin2))+
        geom_line()+
        facet_wrap(~Admin2)+
        theme(legend.position = "none")+
        labs(title = "Daily New cases by DC Metro Area County")+
        xlab("Date")+
        ylab("New Cases")

drop_dc %>% 
        filter(log(Max)>3) %>% 
        filter(Admin2 %in% dc_big) %>% 
        ggplot(aes(y = new_cases, x = (Value), group = Admin2, color = Admin2))+
        labs(title = "Trend by DC County")+
        scale_y_continuous(name="New Cases by Week", labels=comma)+
        xlab("Confirmed Cases (Thousands)")+
        geom_smooth(se = FALSE)+
        facet_wrap(~Admin2, scales = "free")+
        theme(legend.position = "none")
