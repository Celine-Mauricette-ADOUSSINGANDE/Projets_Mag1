
library(tidyverse)
library(lubridate)  # for: day, year etc. 
library(aweek)      # for: get week of the year
library(gridExtra)  # for: grid.arrange
library(dplyr)
library(ggplot2)

OWID_url = "https://raw.githubusercontent.com/owid/covid-19-data/master/public/data/jhu/new_deaths.csv"
OWID_url %>% 
  url() %>% 
  read_csv() %>% 
  select(date,Germany) %>%
  mutate(Germany = ifelse(is.na(Germany),0,Germany)) %>%  # cumulate daily incidence
  mutate(cdeath2 = cumsum(Germany)) %>%
  rename(deaths2 = Germany) -> OWID_Germany

# view(OWID_Germany)
# OWID_Germany_Initiale<-read.csv(url("https://raw.githubusercontent.com/owid/covid-19-data/master/public/data/jhu/new_deaths.csv"))

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

# fusion des deux bases de données
merge_data <-merge(JHU_Germany,OWID_Germany, by = "date")

#tri par ordre décroissant des dates 

merge_data <- merge_data %>%
  arrange(desc(year(date)), desc(month(date)), day(date))

merge_data <- merge_data[-c(1:3), ]
head(merge_data)

ggplot() +
  geom_line(data = JHU_Germany, aes(x = date, y = cdeaths, color = "JHU")) +
  geom_line(data = OWID_Germany, aes(x = date, y = cdeath2, color = "OWID"), lwd = 2, linetype = "dashed") +
  scale_color_manual(values = c("JHU" = "black", "OWID" = "red")) +
  theme(legend.position = "bottom", legend.direction = "horizontal") +
  guides(color = guide_legend(title = NULL)) +
  labs(y = "death count") +
  ggtitle("cumulative death in Germany")


#3 Donn�es sur la population et la mortalit�

# Import population data from Our World In Data
pop_url = "https://raw.githubusercontent.com/owid/covid-19-data/master/public/data/owid-covid-data.csv"

pop_url %>% 
  url() %>% 
  read_csv() %>%
  mutate(iso3c = iso_code) %>%
  filter(iso3c == "FRA") %>%
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
  filter(iso3c == "FRA") ->
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

plot1 <- ggplot() +
  geom_line(data = OWID_Germany_weekly, aes(x = start_date, y = deaths/(sum(deaths))*250), lwd = .5) +
  labs(x="date", y = "death count") +
  ggtitle("Germany: weekly covid deaths per 100 thousand")



plot2 <- ggplot() +
  geom_line(data = OWID_Germany_weekly, aes(x = start_date, y = mortality), lwd = .5) +
  labs(x="date", y = "death count") +
  ggtitle("Germany: weekly mortality(all causes)")

# Organiser les trac�s dans un subplot 1x2
grid.arrange(plot1, plot2, ncol = 2)


# 4 Mortalit� excessive et d�c�s pr�vu



# �tape 1 : Cr�er un mod�le pour pr�dire les d�c�s attendus
model <- lm(deaths ~ year + week, data = OWID_Germany_weekly %>% filter(year >= 2015 & year <= 2020))

df <- OWID_Germany_weekly %>%
  filter(start_date > as.Date("2020-01-01"))

df<- df %>%
  mutate(predicted_deaths = predict(model, newdata = .))


# �tape 3 : Calculer excess_deaths
df <- df %>%
  mutate(excess_deaths = deaths - predicted_deaths)

tail(df)

# �tape 4 : S�lectionner les colonnes requises
df <- df %>%
  select(mortality, start_date, predicted_deaths, excess_deaths)


ggplot() +
  geom_line(data = df, aes(x = start_date, y = excess_deaths, color = "excess deaths")) +
  geom_line(data = OWID_France_weekly, aes(x = start_date, y = deaths, color = "covid")) +
  scale_color_manual(values = c("covid" = "black", "excess deaths" = "red")) +
  theme(legend.position = "bottom", legend.direction = "horizontal") +
  guides(color = guide_legend(title = NULL)) +
  labs(x="date", y = "death count") +
  ggtitle("Germany: covid registered v. excess deaths")

ggplot() +
  geom_line(data = df, aes(x = start_date, y = excess_deaths/(sum(excess_deaths)), color = "excess deaths")) +
  geom_line(data = OWID_France_weekly, aes(x = start_date, y = deaths/(sum(deaths))*250, color = "covid")) +
  scale_color_manual(values = c("covid" = "black", "excess deaths" = "red")) +
  theme(legend.position = "bottom", legend.direction = "horizontal") +
  guides(color = guide_legend(title = NULL)) +
  labs(x="date", y = "death count") +
  ggtitle("Germany: covid registered v. excess deaths per 100 thousand")


#Estimer un modèle OLS avec des effets d'année et de semaine pour les données avant le 1er mars 2020 :
# Charger les packages
library(dplyr)
library(ggplot2)

# Charger les données à partir d'un fichier CSV
df <- OWID_Germany_weekly 

# Filtrer les données avant le 1er mars 2020
df <- data %>% filter(date < as.Date("2020-03-01"))

# Estimer le modèle OLS
model <- lm(mortality ~ year + factor(week), data = data_before)

# Filtrer les données après le 1er mars 2020
data_after <- data %>% filter(date >= as.Date("2020-03-01"))

# Prédire la mortalité avec le modèle
data_after <- data_after %>%
  mutate(predicted_deaths = predict(model, newdata = .))

# Calculer la surmortalité
data_after <- data_after %>%
  mutate(excess_deaths = mortality - predicted_deaths)

# Créer un graphique pour la surmortalité et les décès par COVID-19 hebdomadaires
data_plot <- data_after %>%
  group_by(week) %>%
  summarise(excess_deaths = sum(excess_deaths), covid_deaths = sum(covid_deaths), population = sum(population)) %>%
  mutate(excess_deaths_per_100k = (excess_deaths / population) * 100000, covid_deaths_per_100k = (covid_deaths / population) * 100000)

# Tracer les données
ggplot(data_plot, aes(x = week)) +
  geom_line(aes(y = excess_deaths_per_100k), color = "red", linetype = "solid") +
  geom_line(aes(y = covid_deaths_per_100k), color = "black", linetype = "dashed") +
  ylab("Décès par 100 000 habitants") +
  ggtitle("Surmortalité et Décès par COVID-19")





# 5 Country-level analysis: Wrapping code into functions
# 5.1 Preliminaries

world_mort_url = "https://raw.githubusercontent.com/akarlinsky/world_mortality/main/world_mortality.csv"

world_mort_url %>% 
  url() %>% 
  read_csv()  %>%
  select(-c(country_name,time_unit)) %>%
  rename(mortality=deaths, week=time) %>%
  mutate(start_date = get_date(week=week,year=year)) ->
  mortality_global

OWID_url = "https://raw.githubusercontent.com/owid/covid-19-data/master/public/data/jhu/new_deaths.csv"

# country is column, daily covid death incidence
OWID_url %>% 
  url() %>% 
  read_csv() ->
  OWID_global

# 5.2 Country-level analysis: France and Germany



