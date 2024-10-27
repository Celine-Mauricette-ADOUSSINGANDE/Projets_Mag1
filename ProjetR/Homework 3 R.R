##France (FRA)

#1 Preliminaries
library(tidyverse)
library(lubridate)  # for: day, year etc. 
library(aweek)      # for: get week of the year
library(gridExtra)  # for: grid.arrange
library(dplyr)
library(ggplot2)

#1.1 Date wrangling


#2 JHU and OWID

OWID_url = "https://raw.githubusercontent.com/owid/covid-19-data/master/public/data/jhu/new_deaths.csv"
OWID_url %>% 
  url() %>% 
  read_csv() %>% 
  select(date,France) %>%
  mutate(France = ifelse(is.na(France),0,France)) %>%  # cumulate daily incidence
  mutate(cdeath2 = cumsum(France)) %>%
  rename(deaths2 = France) -> OWID_France

# view(OWID_France)
# OWID_France_Initiale<-read.csv(url("https://raw.githubusercontent.com/owid/covid-19-data/master/public/data/jhu/new_deaths.csv"))

JHU_url = "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv"
JHU_url %>% 
  url() %>% 
  read_csv() %>%
  rename(Country=`Country/Region`) %>%
  filter(Country=="France") %>%
  select(-Lat, -Long) ->JHU_France
# Jhu_url_Initial<-read.csv(url("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv"))

# long to wide transform, aggregate mainland + overseas
JHU_France %>%
  pivot_longer(cols=-(1:2),names_to="date", values_to ="cdeaths") %>%
  mutate(date=as.Date(date,tryFormats = c("%m/%d/%Y"))) %>%
  mutate(date = date + dyears(2000) - ddays(15)) %>%
  group_by(date) %>%
  summarise(cdeaths=sum(cdeaths)) %>%
  ungroup() %>%                             # get daily incidence
  mutate(death =cdeaths - lag(cdeaths)) ->  
  JHU_France

# merging the two databases JHU_France and OWID_France
merge_data <-merge(JHU_France,OWID_France, by = "date")

#sort by date in descending order
merge_data <- merge_data %>%
  arrange(desc(year(date)), desc(month(date)), day(date))

#Delete the first three lines of the base merge_data
merge_data <- merge_data[-c(1:3), ]
head(merge_data)

#Graph of the cumulative death in France
ggplot() +
  geom_line(data = JHU_France, aes(x = date, y = cdeaths, color = "JHU")) +
  geom_line(data = OWID_France, aes(x = date, y = cdeath2, color = "OWID"), lwd = 2, linetype = "dashed") +
  scale_color_manual(values = c("JHU" = "black", "OWID" = "red")) +
  theme(legend.position = "bottom", legend.direction = "horizontal") +
  guides(color = guide_legend(title = NULL)) +
  labs(y = "death count") +
  ggtitle("cumulative death in France")


#3 Population and mortality data

# Import population data from Our World In Data
pop_url = "https://raw.githubusercontent.com/owid/covid-19-data/master/public/data/owid-covid-data.csv"

pop_url %>% 
  url() %>% 
  read_csv() %>%
  mutate(iso3c = iso_code) %>%
  filter(iso3c == "FRA") %>%
  select(population, iso3c, location) %>%
  unique() ->
  France_population

France_population

# Import global mortality data from World Mortality Dataset
# project page: https://github.com/akarlinsky/world_mortality
world_mort_url = "https://raw.githubusercontent.com/akarlinsky/world_mortality/main/world_mortality.csv"

world_mort_url %>% 
  url() %>% 
  read_csv() %>% 
  filter(iso3c == "FRA") ->
  mortality_France

mortality_France

mortality_France %>%
  select(-c(iso3c,country_name,time_unit)) %>%
  rename(mortality=deaths, week=time) ->
  mortality_France

## `summarise()` has grouped output by 'year'. You can override using the
## `.groups` argument.

OWID_France_weekly <- OWID_France %>%
  mutate(year = year(date),  # Extrait l'ann�e de la colonne "date"
         week = isoweek(date)) %>%  # Extrait le num�ro de semaine ISO de la colonne "date"
  select(year, week, deaths2) %>%  # S�lectionne les colonnes "year", "week" et "deaths"
  group_by(year, week) %>%  # Groupe par ann�e et num�ro de semaine
  summarize(deaths = sum(deaths2)) %>% # R�sum� en calculant la somme des d�c�s
  ungroup()
tail(OWID_France_weekly)


OWID_France_weekly <- inner_join(OWID_France_weekly,
                                 mortality_France, by=c("year", "week") )

# we also want a date, so take start of the week 
OWID_France_weekly %>%
  mutate(start_date = get_date(week=week,year=year)) ->
  OWID_France_weekly

# mortality: `deaths` are from all causes ! 
#            `weekly_deaths` are covid deaths
tail(OWID_France_weekly)

#Graph oh the France: weekly covid deaths per 100 thousand
plot1 <- ggplot() +
  geom_line(data = OWID_France_weekly, aes(x = start_date, y = deaths/(sum(deaths))*250), lwd = .5) +
  labs(x="date", y = "death count") +
  ggtitle("France: weekly covid deaths per 100 thousand")


#Graph of the France: weekly mortality(all causes
plot2 <- ggplot() +
  geom_line(data = OWID_France_weekly, aes(x = start_date, y = mortality), lwd = .5) +
  labs(x="date", y = "death count") +
  ggtitle("France: weekly mortality(all causes)")

# Layout organization in a 1x2 subplot
grid.arrange(plot1, plot2, ncol = 2)


# 4 Excess death and predicted death
# Transforming the mortality_france database into days while keeping the week variable
mortality_France %>%
  mutate(start_date = get_date(week=week,year=year)) ->
  mortality_France

# Step 1: Create a database for all data before 2020-03-01
data_before <- mortality_France %>%
  filter(start_date < as.Date("2020-03-01"))
model1 <- lm(mortality ~ year +factor(week), data = data_before)

#Step2: Create a database for all data after 2020-03-01
data_after <- mortality_France %>%
  filter(start_date >= as.Date("2020-03-01"))
model2 <- lm(mortality ~ year +factor(week), data = data_after)

#Step 3: Data_afeter prediction
data_after<- data_after %>%
  mutate(predicted_deaths = predict(model2, data = data_after))

#Add a new column predicted_deaths
data_after <- data_after %>%
mutate(predicted_deaths=predicted_deaths)
tail(data_after)

# Step 4:Add a new column excess_deaths and Calculate excess_deaths
data_after <- data_after %>%
  mutate(excess_deaths = mortality - predicted_deaths)
tail(data_after)

#Step 5: Select the required columns
data_after <- data_after %>%
  select(mortality, start_date, predicted_deaths, excess_deaths)

#Step 6: Keep data from 2020-01-02 to 2023-08-22
data_after1 <- data_after %>%
  filter(start_date < as.Date("2023-08-22"))
tail(data_after1)

OWID_France_weekly1 <- OWID_France_weekly %>%
  filter(start_date < as.Date("2023-08-22"))%>%
  filter(start_date > as.Date("2020-01-02"))
tail(OWID_France_weekly1)

#Graph France: covid registered v. excess deaths
ggplot() +
  geom_line(data = data_after1, aes(x = start_date, y = excess_deaths, color = "excess deaths")) +
  geom_line(data = OWID_France_weekly1, aes(x = start_date, y = deaths, color = "covid")) +
  scale_color_manual(values = c("covid" = "black", "excess deaths" = "red")) +
  theme(legend.position = "bottom", legend.direction = "horizontal") +
  guides(color = guide_legend(title = NULL)) +
  labs(x="date", y = "death count") +
  ggtitle("France: covid registered v. excess deaths")

#Graph France: covid registered v. excess deaths per 100 thousand
ggplot() +
  geom_line(data = data_after1, aes(x = start_date, y = excess_deaths/(sum(excess_deaths)), color = "excess deaths")) +
  geom_line(data = OWID_France_weekly1, aes(x = start_date, y = deaths/(sum(deaths))*250, color = "covid")) +
  scale_color_manual(values = c("covid" = "black", "excess deaths" = "red")) +
  theme(legend.position = "bottom", legend.direction = "horizontal") +
  guides(color = guide_legend(title = NULL)) +
  labs(x="date", y = "death count") +
  ggtitle("France: covid registered v. excess deaths per 100 thousand")


#Germany (Allemagne)

#1.1 Date wrangling

#2 JHU and OWID

OWID_url = "https://raw.githubusercontent.com/owid/covid-19-data/master/public/data/jhu/new_deaths.csv"
OWID_url %>% 
  url() %>% 
  read_csv() %>% 
  select(date,Germany) %>%
  mutate(Germany = ifelse(is.na(Germany),0,Germany)) %>%  # cumulate daily incidence
  mutate(cdeath2 = cumsum(Germany)) %>%
  rename(deaths2 = Germany) -> OWID_Germany

# view(OWID_France)
# OWID_France_Initiale<-read.csv(url("https://raw.githubusercontent.com/owid/covid-19-data/master/public/data/jhu/new_deaths.csv"))

JHU_url = "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv"
JHU_url %>% 
  url() %>% 
  read_csv() %>%
  rename(Country=`Country/Region`) %>%
  filter(Country=="Germany") %>%
  select(-Lat, -Long) ->JHU_Germany
# Jhu_url_Initial<-read.csv(url("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv"))

# long to wide transform, aggregate mainland + overseas
JHU_Germany %>%
  pivot_longer(cols=-(1:2),names_to="date", values_to ="cdeaths") %>%
  mutate(date=as.Date(date,tryFormats = c("%m/%d/%Y"))) %>%
  mutate(date = date + dyears(2000) - ddays(15)) %>%
  group_by(date) %>%
  summarise(cdeaths=sum(cdeaths)) %>%
  ungroup() %>%                             # get daily incidence
  mutate(death =cdeaths - lag(cdeaths)) ->  
  JHU_Germany

# merging the two databases JHU_France and OWID_France
merge_data <-merge(JHU_Germany,OWID_Germany, by = "date")

#sort by date in descending order
merge_data <- merge_data %>%
  arrange(desc(year(date)), desc(month(date)), day(date))

#Delete the first three lines of the base merge_data
merge_data <- merge_data[-c(1:3), ]
head(merge_data)

#Graph of the cumulative death in Germany
ggplot() +
  geom_line(data = JHU_Germany, aes(x = date, y = cdeaths, color = "JHU")) +
  geom_line(data = OWID_Germany, aes(x = date, y = cdeath2, color = "OWID"), lwd = 2, linetype = "dashed") +
  scale_color_manual(values = c("JHU" = "black", "OWID" = "red")) +
  theme(legend.position = "bottom", legend.direction = "horizontal") +
  guides(color = guide_legend(title = NULL)) +
  labs(y = "death count") +
  ggtitle("cumulative death in Germany")


#3 Population and mortality data

# Import population data from Our World In Data
pop_url = "https://raw.githubusercontent.com/owid/covid-19-data/master/public/data/owid-covid-data.csv"

pop_url %>% 
  url() %>% 
  read_csv() %>%
  mutate(iso3c = iso_code) %>%
  filter(iso3c == "DEU") %>%
  select(population, iso3c, location) %>%
  unique() ->
  Germany_population

Germany_population

# Import global mortality data from World Mortality Dataset
# project page: https://github.com/akarlinsky/world_mortality
world_mort_url = "https://raw.githubusercontent.com/akarlinsky/world_mortality/main/world_mortality.csv"

world_mort_url %>% 
  url() %>% 
  read_csv() %>% 
  filter(iso3c == "DEU") ->
  mortality_Germany

mortality_Germany

mortality_Germany %>%
  select(-c(iso3c,country_name,time_unit)) %>%
  rename(mortality=deaths, week=time) ->
  mortality_Germany

## `summarise()` has grouped output by 'year'. You can override using the
## `.groups` argument.

OWID_Germany_weekly <- OWID_Germany %>%
  mutate(year = year(date),  # Extrait l'ann�e de la colonne "date"
         week = isoweek(date)) %>%  # Extrait le num�ro de semaine ISO de la colonne "date"
  select(year, week, deaths2) %>%  # S�lectionne les colonnes "year", "week" et "deaths"
  group_by(year, week) %>%  # Groupe par ann�e et num�ro de semaine
  summarize(deaths = sum(deaths2)) %>% # R�sum� en calculant la somme des d�c�s
  ungroup()
tail(OWID_Germany_weekly)


OWID_Germany_weekly <- inner_join(OWID_Germany_weekly,
                                 mortality_Germany, by=c("year", "week") )

# we also want a date, so take start of the week 
OWID_Germany_weekly %>%
  mutate(start_date = get_date(week=week,year=year)) ->
  OWID_Germany_weekly

# mortality: `deaths` are from all causes ! 
#            `weekly_deaths` are covid deaths
tail(OWID_Germany_weekly)

#Graph oh the Germany: weekly covid deaths per 100 thousand
plot1G <- ggplot() +
  geom_line(data = OWID_France_weekly, aes(x = start_date, y = deaths/(sum(deaths))*250), lwd = .5) +
  labs(x="date", y = "death count") +
  ggtitle("Germany: weekly covid deaths per 100 thousand")


#Graph of the Germany: weekly mortality(all causes
plot2G <- ggplot() +
  geom_line(data = OWID_France_weekly, aes(x = start_date, y = mortality), lwd = .5) +
  labs(x="date", y = "death count") +
  ggtitle("Germany: weekly mortality(all causes)")

# Layout organization in a 1x2 subplot
grid.arrange(plot1G, plot2G, ncol = 2)

# 4 Excess death and predicted death
# Transforming the mortality_france database into days while keeping the week variable
mortality_Germany %>%
  mutate(start_date = get_date(week=week,year=year)) ->
  mortality_Germany

# Step 1: Create a database for all data before 2020-03-01
data_beforeG <- mortality_Germany %>%
  filter(start_date < as.Date("2020-03-01"))
model1G <- lm(mortality ~ year +factor(week), data = data_beforeG)

#Step2: Create a database for all data after 2020-03-01
data_afterG <- mortality_Germany %>%
  filter(start_date >= as.Date("2020-03-01"))
model2G <- lm(mortality ~ year +factor(week), data = data_afterG)

#Step 3: Data_afeter prediction
data_afterG<- data_afterG %>%
  mutate(predicted_deaths = predict(model2G, data = data_afterG))

#Add a new column predicted_deaths
data_afterG <- data_afterG %>%
  mutate(predicted_deaths=predicted_deaths)
tail(data_afterG)

# Step 4:Add a new column excess_deaths and Calculate excess_deaths
data_afterG <- data_afterG %>%
  mutate(excess_deaths = mortality - predicted_deaths)
tail(data_after)

#Step 5: Select the required columns
data_afterG <- data_afterG %>%
  select(mortality, start_date, predicted_deaths, excess_deaths)

#Step 6: Keep data from 2020-01-02 to 2023-08-22
data_after1G <- data_afterG %>%
  filter(start_date < as.Date("2023-08-22"))
tail(data_after1G)

OWID_Germany_weekly1G <- OWID_Germany_weekly %>%
  filter(start_date < as.Date("2023-08-22"))%>%
  filter(start_date > as.Date("2020-01-02"))
tail(OWID_Germany_weekly1G)

#Graph Germany: covid registered v. excess deaths
ggplot() +
  geom_line(data = data_after1G, aes(x = start_date, y = excess_deaths, color = "excess deaths")) +
  geom_line(data = OWID_Germany_weekly1G, aes(x = start_date, y = deaths, color = "covid")) +
  scale_color_manual(values = c("covid" = "black", "excess deaths" = "red")) +
  theme(legend.position = "bottom", legend.direction = "horizontal") +
  guides(color = guide_legend(title = NULL)) +
  labs(x="date", y = "death count") +
  ggtitle("Germany: covid registered v. excess deaths")

#Graph Germany: covid registered v. excess deaths per 100 thousand
ggplot() +
  geom_line(data = data_after1G, aes(x = start_date, y = excess_deaths/(sum(excess_deaths)), color = "excess deaths")) +
  geom_line(data = OWID_Germany_weekly1G, aes(x = start_date, y = deaths/(sum(deaths))*250, color = "covid")) +
  scale_color_manual(values = c("covid" = "black", "excess deaths" = "red")) +
  theme(legend.position = "bottom", legend.direction = "horizontal") +
  guides(color = guide_legend(title = NULL)) +
  labs(x="date", y = "death count") +
  ggtitle("Germany: covid registered v. excess deaths per 100 thousand")

# United kingdom(Uk)

#1.1 Date wrangling

#2 JHU and OWID

OWID_url = "https://raw.githubusercontent.com/owid/covid-19-data/master/public/data/jhu/new_deaths.csv"
OWID_url %>% 
  url() %>% 
  read_csv() %>% 
  select(date,"United Kingdom") %>%
  mutate(United_Kingdom = ifelse(is.na("United Kingdom"),0,"United Kingdom")) %>%  # cumulate daily incidence
  mutate(cdeath2 = cumsum(United_Kingdom)) %>%
  rename(deaths2 = United_Kingdom) -> OWID_United_Kingdom

# view(OWID_France)
# OWID_France_Initiale<-read.csv(url("https://raw.githubusercontent.com/owid/covid-19-data/master/public/data/jhu/new_deaths.csv"))

JHU_url = "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv"
JHU_url %>% 
  url() %>% 
  read_csv() %>%
  rename(Country=`Country/Region`) %>%
  filter(Country=="United Kingdom") %>%
  select(-Lat, -Long) ->JHU_United_Kingdom

# Jhu_url_Initial<-read.csv(url("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv"))`
# Jhu_url_Initial<-read.csv(url("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv"))
# long to wide transform, aggregate mainland + overseas

# long to wide transform, aggregate mainland + overseas
JHU_United_Kingdom %>%
  pivot_longer(cols=-(1:2),names_to="date", values_to ="cdeaths") %>%
  mutate(date=as.Date(date,tryFormats = c("%m/%d/%Y"))) %>%
  mutate(date = date + dyears(2000) - ddays(15)) %>%
  group_by(date) %>%
  summarise(cdeaths=sum(cdeaths)) %>%
  ungroup() %>%                             # get daily incidence
  mutate(death =cdeaths - lag(cdeaths)) ->  
  JHU_United_Kingdom

# merging the two databases JHU_France and OWID_France
merge_data <-merge(JHU_United_Kingdom,OWID_United_Kingdom, by = "date")

#sort by date in descending order
merge_data <- merge_data %>%
  arrange(desc(year(date)), desc(month(date)), day(date))

#Delete the first three lines of the base merge_data
merge_data <- merge_data[-c(1:3), ]
head(merge_data)

ggplot() +
  geom_line(data = JHU_United_Kingdom, aes(x = date, y = cdeaths, color = "JHU")) +
  geom_line(data = OWID_United_Kingdom, aes(x = date, y = cdeath2, color = "OWID"), lwd = 2, linetype = "dashed") +
  scale_color_manual(values = c("JHU" = "black", "OWID" = "red")) +
  theme(legend.position = "bottom", legend.direction = "horizontal") +
  guides(color = guide_legend(title = NULL)) +
  labs(y = "death count") +
  ggtitle("cumulative death in United_Kingdom")


#3 Population and mortality data

# Import population data from Our World In Data

pop_url = "https://raw.githubusercontent.com/owid/covid-19-data/master/public/data/owid-covid-data.csv"

pop_url %>% 
  url() %>% 
  read_csv() %>%
  mutate(iso3c = iso_code) %>%
  filter(iso3c == "Uk") %>%
  select(population, iso3c, location) %>%
  unique() ->
  United_Kingdom_population

United_Kingdom_population

# Import global mortality data from World Mortality Dataset
# project page: https://github.com/akarlinsky/world_mortality

world_mort_url = "https://raw.githubusercontent.com/akarlinsky/world_mortality/main/world_mortality.csv"

world_mort_url %>% 
  url() %>% 
  read_csv() %>% 
  filter(iso3c == "UK") ->
  mortality_United_Kingdom

mortality_United_Kingdom

mortality_United_Kingdom %>%
  select(-c(iso3c,country_name,time_unit)) %>%
  rename(mortality=deaths, week=time) ->
  mortality_United_Kingdom

OWID_United_Kingdom_weekly <- OWID_United_Kingdom %>%
  mutate(year = year(date),  # Extrait l'ann�e de la colonne "date"
         week = isoweek(date)) %>%  # Extrait le num�ro de semaine ISO de la colonne "date"
  select(year, week, deaths2) %>%  # S�lectionne les colonnes "year", "week" et "deaths"
  group_by(year, week) %>%  # Groupe par ann�e et num�ro de semaine
  summarize(deaths = sum(deaths2)) %>% # R�sum� en calculant la somme des d�c�s
  ungroup()
tail(OWID_United_Kingdom_weekly)

OWID_United_Kingdom_weekly <- inner_join(OWID_United_Kingdom_weekly,
                                         mortality_United_Kingdom, by=c("year", "week") )

# we also want a date, so take start of the week 
OWID_United_Kingdom_weekly %>%
  mutate(start_date = get_date(week=week,year=year)) ->
  OWID_United_Kingdom_weekly

# mortality: `deaths` are from all causes ! 
#            `weekly_deaths` are covid deaths
tail(OWID_United_Kingdom_weekly)


#Graph oh the United Kingdom: weekly covid deaths per 100 thousand
plot1 <- ggplot() +
  geom_line(data = OWID_United_Kingdom_weekly, aes(x = start_date, y = deaths/(sum(deaths))*250), lwd = .5) +
  labs(x="date", y = "death count") +
  ggtitle("United Kingdom: weekly covid deaths per 100 thousand")


#Graph of the United_Kingdom: weekly mortality(all causes
plot2 <- ggplot() +
  geom_line(data = OWID_United_Kingdom_weekly, aes(x = start_date, y = mortality), lwd = .5) +
  labs(x="date", y = "death count") +
  ggtitle("United Kingdom: weekly mortality(all causes)")

# Layout organization in a 1x2 subplot
grid.arrange(plot1, plot2, ncol = 2)


# 4 Excess death and predicted death
# Transforming the mortality_france database into days while keeping the week variable

OWID_url = "https://raw.githubusercontent.com/owid/covid-19-data/master/public/data/jhu/new_deaths.csv"
OWID_url %>% 
  url() %>% 
  read_csv() %>% 
  select(date,"United Kingdom") %>%
  mutate(United_Kingdom = ifelse(is.na("United Kingdom"),0,"United Kingdom")) %>%  # cumulate daily incidence
  mutate(cdeath2 = cumsum(United_Kingdom)) %>%
  rename(deaths2 = United_Kingdom) -> OWID_United_Kingdom

JHU_url = "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv"
JHU_url %>% 
  url() %>% 
  read_csv() %>%
  rename(Country=`Country/Region`) %>%
  filter(Country=="United Kingdom") %>%
  select(-Lat, -Long) ->JHU_United_Kingdom

JHU_United_Kingdom %>%
  pivot_longer(cols=-(1:2),names_to="date", values_to ="cdeaths") %>%
  mutate(date=as.Date(date,tryFormats = c("%m/%d/%Y"))) %>%
  mutate(date = date + dyears(2000) - ddays(15)) %>%
  group_by(date) %>%
  summarise(cdeaths=sum(cdeaths)) %>%
  ungroup() %>%                             # get daily incidence
  mutate(death =cdeaths - lag(cdeaths)) ->  
  JHU_United_Kingdom

# merging the two databases JHU_France and OWID_France
merge_data <-merge(JHU_United_Kingdom,OWID_United_Kingdom, by = "date")

#sort by date in descending order
merge_data <- merge_data %>%
  arrange(desc(year(date)), desc(month(date)), day(date))

#Delete the first three lines of the base merge_data
merge_data <- merge_data[-c(1:3), ]
head(merge_data)

ggplot() +
  geom_line(data = JHU_United_Kingdom, aes(x = date, y = cdeaths, color = "JHU")) +
  geom_line(data = OWID_United_Kingdom, aes(x = date, y = cdeath2, color = "OWID"), lwd = 2, linetype = "dashed") +
  scale_color_manual(values = c("JHU" = "black", "OWID" = "red")) +
  theme(legend.position = "bottom", legend.direction = "horizontal") +
  guides(color = guide_legend(title = NULL)) +
  labs(y = "death count") +
  ggtitle("cumulative death in United_Kingdom")

#5 Country-level analysis: Wrapping code into functions
#5.1 Preliminaries

# France

world_mort_url = "https://raw.githubusercontent.com/akarlinsky/world_mortality/main/world_mortality.csv"
world_mort_url %>%
  url() %>%
  read_csv()  %>%
  select(-c(country_name,time_unit)) %>%
  rename(mortality=deaths, week=time) %>%
  mutate(start_date = get_date(week=week,year=year)) ->
  mortality_global

#lm(mortality ~ year + factor(week), data=mortality_global) ->

# lm_res

#summary(lm_res)

lm.r= lm(formula = mortality ~ year + factor(week),
         data = mortality_global)
predict(lm.r)
mortality_global$pred = predict(lm.r, newdata = mortality_global)

#fonction pour calculer le predicted_death

predict_death <- function(mortality_country){
  typeof(mortality_country$start_date)
  mortality_country %>%
    filter(start_date <= as.POSIXct("2020-03-01")) ->
    mortality_before_covid
  lm.r= lm(formula = mortality ~ year + factor(week),
           data = mortality_before_covid)
  return(lm.r)}

#fonction pour l'analyse par pays

country_analysis <- function(country_name){
  mortality_global %>%
    filter(iso3c == country_name) ->
    mortality_country
  predict_death(mortality_country) ->
    lm.r
  mortality_country %>%
    filter(start_date >= as.POSIXct("2020-03-01"))->
    mortality_country
  mortality_country$pred = predict(lm.r, newdata = mortality_country)
  mortality_country$excess_death = mortality_country$mortality - mortality_country$pred
  return(mortality_country)}

country_analysis("FRA") ->
  France_covid
country_analysis("DEU")
country_analysis("GBR")


OWID_France_weekly %>%
  select(start_date, deaths) -> 
  #rename(start_date = date) ->
  OWID_France_covid

inner_join(OWID_France_covid, France_covid, by = c("start_date")) ->
  graph_covid

ggplot(graph_covid, aes(x=start_date)) +
  geom_line(aes(y=excess_death), color = "red")  +
  geom_line(aes(y=deaths))

# Germany

country_analysis("DEU") ->
  Germany_covid
country_analysis("FRA")
country_analysis("GBR")


OWID_Germany_weekly %>%
  select(start_date, deaths) -> 
  #rename(start_date = date) ->
  OWID_Germany_covid

inner_join(OWID_Germany_covid, Germany_covid, by = c("start_date")) ->
  graph_covid

ggplot(graph_covid, aes(x=start_date)) +
  geom_line(aes(y=excess_death), color = "red")  +
  geom_line(aes(y=deaths))


