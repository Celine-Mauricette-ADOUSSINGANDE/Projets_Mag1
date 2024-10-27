library(haven)
library(tidyverse)
library(stargazer)
library(dplyr)
library(lmtest)
library(sandwich)
library(ggplot2)

# Définir le chemin vers le dossier contenant le fichier .dta
path <- "C:/Users/Mauricette/Desktop/"

# Ensuite, vous pouvez utiliser 'path' dans votre code
read_dta(paste0(path, "census_acs_2000.dta")) %>%
  select(year, age, labforce, wkswork2, incwage, classwkr, educd, race,
         marst, empstat, bpld, yrimmig, sex, serial, occ) ->
  acs2000

#Visualisation de la base de données avec les variables extraites 
view(acs2000) 

#****ANALYSE EXPLORATOIRE: PRELIMINAIRES******

#Délimitation de l'échantillon de travailleurs.ie la population active 
#celle agée de 18 à 65 ans

acs2000 %>%
  filter(age >=18 & age <=65 & labforce == 2,classwkr == 2) ->
  acs2000

#Visualisation de la nouvelle base
view(acs2000)


#Création et recodage  des variables : 

#**weeks**: Cette variable est calculée en multipliant différents multiplicateurs 
#par les catégories de la variable wkswork2.  

#**lnw*: Cette variable est calculée avec le logarithme naturel de la variable 
#*"incwage" divisée par la variable weeks

acs2000 %>%
  mutate(weeks = 7 * (wkswork2 == 1) + 20 * (wkswork2 == 2) +
           33 * (wkswork2 == 3) +
           43.5 * (wkswork2 == 4) + 48.5 * (wkswork2 == 5) +
           51 * (wkswork2 == 6)) %>%
  mutate(lnw = log(incwage / weeks)) %>%
  filter(!is.na(lnw), lnw != -Inf, lnw != Inf) ->
  acs2000 

#***foreign* : la nouvelle variable prendra la valeur "TRUE" si la valeur 
#de "bpld" est supérieure ou égale à 15000. 

#**immclass**: Ajoute une nouvelle variable  initialisée à NA puis 
#Modifie la valeur de la colonne "immclass" en 1 si la valeur de la colonne
#"foreign" est égale à 1 et si la différence entre les valeurs des colonnes 
#"year" et "yrimmig" est inférieure ou égale à 2.
#Modifie la valeur de la colonne "immclass" en 2 si la valeur de 
#la colonne "foreign" est égale à 1 et si la différence entre les valeurs 
#des colonnes "year" et "yrimmig" est supérieure à 2 et inférieure ou égale à 5, etc

acs2000 %>%
  mutate(foreign = (bpld>=15000),
         immclass = case_when(foreign==1 & year-yrimmig<=2 ~ 1,
                              foreign==1 & year-yrimmig> 2 & year-yrimmig<=5 ~ 2,
                              foreign==1 & year-yrimmig> 5 & year-yrimmig<=10 ~ 3,
                              foreign==1 & year-yrimmig> 10 & !is.na(year-yrimmig) ~ 4
         ),
         agecat= case_when(age >= 18 & age <= 25 ~ 1, 
                           age > 25 & age <= 35 ~ 2,
                           age > 35 & age <= 45 ~ 3,
                           age > 45 ~ 4, 
                           TRUE ~ NA_real_
         ))-> acs2000                 


#**Schooling**: La première mutation initialise la variable schooling à 0 
# par défaut.  Ensuite les codes qui suivent mettent à jour 
#la colonne "schooling" dans le tableau de données "acs2000" en 
#remplaçant les zéros  en fonction des valeurs de la colonne 
#"educd" dans chaque ligne



#**pe** : la différence entre 'age' et la somme de 6 et 'schooling', 
#et assigne cette différence à la variable 'pe'

acs2000 %>%    
  mutate(schooling = 0) %>%
  mutate(schooling = ifelse(educd == 10, 2, schooling)) %>%
  mutate(schooling = ifelse(educd == 13, 2.5, schooling)) %>%
  mutate(schooling = ifelse(educd == 14, 1, schooling)) %>%
  mutate(schooling = ifelse(educd == 15, 2, schooling)) %>%
  mutate(schooling = ifelse(educd == 16, 3, schooling)) %>%
  mutate(schooling = ifelse(educd == 17, 4, schooling)) %>%
  mutate(schooling = ifelse(educd == 20, 6.5,schooling)) %>%    
  mutate(schooling = ifelse(educd == 21, 5.5, schooling)) %>%
  mutate(schooling = ifelse(educd == 22, 5, schooling)) %>%
  mutate(schooling = ifelse(educd == 23, 6, schooling)) %>%
  mutate(schooling = ifelse(educd == 25, 7, schooling)) %>%
  mutate(schooling = ifelse(educd == 24, 7.5, schooling)) %>%    
  mutate(schooling = ifelse(educd == 26, 8, schooling)) %>%
  mutate(schooling = ifelse(educd == 30, 9, schooling)) %>%
  mutate(schooling = ifelse(educd == 40, 10, schooling)) %>%    
  mutate(schooling = ifelse(educd == 50 | educd == 61, 11, schooling)) %>%
  mutate(schooling = ifelse(educd == 60 | (educd >= 62 & educd <= 64), 12, schooling)) %>%
  mutate(schooling = ifelse(educd >= 65 & educd <= 71, 13, schooling)) %>%
  mutate(schooling = ifelse(educd >= 80 & educd <= 90, 14, schooling)) %>%
  mutate(schooling = ifelse(educd == 90, 15, schooling)) %>%
  mutate(schooling = ifelse(educd == 100 | educd == 101, 16, schooling)) %>%
  mutate(schooling = ifelse(educd == 110, 17, schooling)) %>%
  mutate(schooling = ifelse(educd == 111 | educd == 114, 18, schooling)) %>%
  mutate(schooling = ifelse(educd == 112, 19, schooling)) %>%
  mutate(schooling = ifelse(educd == 113 | educd > 114, 20, schooling)) %>%
  mutate(pe = as.integer(age - 6 - schooling))  ->
  acs2000


#L'instruction ci dessous filtre le df en gardant uniquement les 
#lignes où lnw n'est pas NA (non disponible) et où pe
#est supérieur ou égal à 1 et inférieur ou égal à 40.
#En d'autres termes, les lignes avec des valeurs manquantes 
#de lnw sont exclues et seules les lignes avec des valeurs de
#pe comprises entre 1 et 40 sont gardées

acs2000 %>%
  filter(!is.na(lnw), pe >= 1 & pe <= 40) ->
  acs2000

#affichage de la base définitive après l'analyse exploratoire 
#On obtient bien 146,577 observations et 22 colonnes
#y compris celles nouvellement créees

glimpse(acs2000) 


#***Tache de Replication***

# Charger les bibliothèques nécessaires


modf <-lnw ~ educd+age+I(age^2)+ sex+educd:age + foreign

reg2 <- lm(modf, data=acs2000, subset=(foreign==0 |immclass==1))
reg3 <- lm(modf, data=acs2000, subset=(foreign==0 |immclass==2))
reg4 <- lm(modf, data=acs2000, subset=(foreign==0 |immclass==3))
reg5 <- lm(modf, data=acs2000, subset=(foreign==0 |immclass==4))


stargazer(reg2,reg3,reg4,reg5,
          dep.var.caption="0-2 years 3-5 years 6-10 years more than 10 years",dep.var.labels="",
          omit.table.layout = "n", star.cutoffs = NA,
          keep.stat=c("n", "rsq"),no.space=TRUE,
          keep=c("foreignTRUE"),
          header=TRUE,
          title="Table replication",type="text"
)

acs2000$lnw_pred <- NA
acs2000$sigma <- NA

acs2000$in_reg <- (acs2000$sex == 1 & acs2000$foreign == 0)
mod <- lm(lnw ~ foreign + age + educd + educd*age,
          data=acs2000, subset=in_reg)
acs2000$sigma[acs2000$in_reg] <- (mod$residuals)^2
acs2000$lnw_pred[acs2000$sex == 1] <- predict(mod, acs2000[acs2000$sex == 1,])

# Supprimer les lignes avec les valeurs manquantes dans la colonne agecat
#acs2000 <-(acs2000[!is.na(acs2000$agecat),])

acs2000 %>% arrange(serial) -> acs2000
set.seed(1234)

for (i in min(acs2000$agecat):max(acs2000$agecat)) {
  for (k in 1:2) {
    for (j in min(acs2000$educd):max(acs2000$educd) ) {
      acs2000$in_reg <- (acs2000$agecat==i & acs2000$sex==k & acs2000$educd==j)
      N = sum(acs2000$in_reg)
      if (N>0) {
        sdS = sqrt(mean(acs2000$sigma[acs2000$in_reg], na.rm=TRUE))
        # cat(i, k, j, "N=",N, "  ", sdS, "\n")
        acs2000$lnw_pred[acs2000$in_reg] = acs2000$lnw_pred[acs2000$in_reg] +
          rnorm(N,0,sdS)
      }
    }
  }
}

acs2000 %>%
  filter(!is.na(lnw_pred)) %>%
  mutate(lnw_pred = ifelse(foreign==0, lnw,lnw_pred)) ->
  acs2000

#Question 2
#Calcul des rang centiles

acs2000 <- acs2000 %>% 
  mutate(x = (foreign==0)) %>% 
  arrange(lnw) %>% # sorting
  mutate(rank = cumsum(x) ) %>% # somme cumulée de l'indicateur qui prouve qu'on n'est pas immigrée
  mutate(rank = rank / max(rank)) %>% # normalisation to [0,1] 
  mutate(immpos = ifelse(log(rank/(1-rank)) < Inf, log(rank/(1-rank)), NA)) # première transformation des rangs par la transformation logarithmique


#estimation de la densité

dens_imm =density(acs2000$immpos[acs2000$foreign == 1])

#Retransformation  

dens_imm$x2 <- exp(dens_imm$x) / (1 + exp(dens_imm$x))


#Calcule des rangs centiles pour les salaires prédits 

#Il s'agit normalement de Calculer la variable  `Foreign predicted` et d'appliquer la meme pocédure comme pour pour `lnw_pred`.   Voici la procédure que nous proposons : 
  
  
# La partie des rangs centiles pour lnw_pred 

# Calculer  le Foreign_pred et appliquer la même procédure: 

# Construction du modèle de régression logistique
modele <- glm(foreign ~ bpld + immclass, data = acs2000, family = "binomial")

# Résumé du modèle
#summary(modele)

acs2000 <- acs2000 %>%
  mutate(foreign_pred = predict(modele, newdata = ., type = "response")) %>%
  mutate(foreign_pred = ifelse(is.na(foreign_pred), 0, foreign_pred)) %>%
  mutate(foreign_pred = as.logical(foreign_pred))

acs2000 <- acs2000 %>%
  mutate(y = (foreign_pred == 0)) %>%
  arrange(lnw_pred) %>% # sorting
  mutate(rank_pred = cumsum(y)) %>%
  mutate(rank_pred = rank_pred / max(rank_pred)) %>%
  mutate(immpos_pred = ifelse(log(rank_pred/(1-rank_pred)) < Inf, log(rank_pred/(1-rank_pred)), NA))


#Estimation de la densité : 

dens_imm_pred= density(acs2000$immpos_pred[acs2000$foreign_pred == 1])

# Puis la retransformation  

dens_imm_pred$y2 <- exp(dens_imm_pred$y) / (1 + exp(dens_imm_pred$y))


#Cependant en optant pour cette option nous avons rencontré dans le calcul de la variable `Foreign predicted`.
#Voici le code similaire que nous avons finalement utilisé  pour calculer les rangs centiles prédits: 
ecdf_lnwpr <- ecdf(acs2000$lnw)
centiles_predi <- ecdf_lnwpr(seq(min(acs2000$lnw_pred), max(acs2000$lnw_pred), length.out = 101))
centiles_predi = centiles_predi[1:100]
rang_p <- log(centiles_predi /(1-centiles_predi))

# Estimation de la densité de la distribution des rangs transformés :

dens_imm1 <- density(rang_p)

#Puis la rétransformation  

inv_transform <- function(x) {return (exp(x) / (1 +exp(x)))}

x1= inv_transform(dens_imm1$x)
dens1=density(x1)

plot(dens1,xlim=c(0,0.9))
lines(dens1)

# Le graphique  

# Réinitialisation des limites du clip après le graphique

# Tracer la première graphique
plot(dens_imm$x2,
     dens_imm$y /(dens_imm$x2 * (1-dens_imm$x2)), type = "l", lty = 2, #lwd=1,
     ylab="", xlab="rank", col = "green", xlim = c(0.05, 0.95))


# Tracer la deuxième graphique # A corriger
lines(dens1, type="l",
      ylab="", xlab="rank", col = "black")

# Tracer la troisième graphique
abline(h = 1, col = "grey")

axis(1, at = c(0.2, 0.4, 0.6, 0.8))
# Ajouter la légende
legend("topright", legend = c("Foreign predicted", "Foreign", "Natives"),
       col = c("black", "green", "grey"), lty = c(1, 2, 1))
#Refaire le même graphique avec l'axe des ordonnées  y=c((0.8, 1.0, 1.2, 1.4, 1.6)


# Tracer le graphique pour les immigrants travailleurs
 

# Arrival 3-10 years(immclass == 2 | immclass == 3)

acs2000 <- acs2000 %>% 
  mutate(x = (immclass == 2 | immclass == 3)) %>% 
  mutate(RAnk = cumsum(x) ) %>% # somme cumulée de l'indicateur qui prouve qu'on n'est pas immigrée
  mutate(RAnk = RAnk / max(RAnk)) %>% # normalisation to [0,1] 
  mutate(IMmpos = ifelse(log(RAnk/(1-RAnk)) < Inf, log(RAnk/(1-RAnk)), NA)) # première transformation des rangs par la transformation logarithmique

# Densité
DEns_imm =density(acs2000$immpos[acs2000$x])

#Retransformation  

DEns_imm$x2 <- exp(dens_imm$x) / (1 + exp(dens_imm$x))

plot(DEns_imm$x2,
     DEns_imm$y /(DEns_imm$x2 * (1-DEns_imm$x2)), type = "l", lty = 2, #lwd=1,
     ylab="", xlab="rank", col = "orange", xlim = c(0.02, 0.95))


#Arrival > 10 years
acs2000 <- acs2000 %>% 
  mutate(x = (immclass == 4)) %>% 
  mutate(rank3 = cumsum(x) ) %>% # somme cumulée de l'indicateur qui prouve qu'on n'est pas immigrée
  mutate(rank3 = rank3 / max(rank3)) %>% # normalisation to [0,1] 
  mutate(IMMpos = ifelse(log(rank3/(1-rank3)) < Inf, log(rank3/(1-rank3)), 0)) # première transformation des rangs par la transformation logarithmique

#Dansité
DENs_imm =density(acs2000$immpos[acs2000$x])

#Retransformation  

DENs_imm$x2 <- exp(dens_imm$x) / (1 + exp(dens_imm$x))

plot(DEns_imm$x2,
     DEns_imm$y /(DEns_imm$x2 * (1-DEns_imm$x2)), type = "l", lty = 2, #lwd=1,
     ylab="", xlab="rank", col = "violet", xlim = c(0.02, 0.95))

acs2000 <- acs2000 %>% 
  mutate(y = (immclass == 4)) %>% 
  mutate(rank3 = cumsum(y) ) %>% # somme cumulée de l'indicateur qui prouve qu'on n'est pas immigrée
  mutate(rank3 = rank3 / max(rank3)) %>% # normalisation to [0,1] 
  mutate(immpos3 = ifelse(log(rank3/(1-rank3)) < Inf, log(rank3/(1-rank3)), NA)) # première transformation des rangs par la transformation logarithmique


#estimation de la densité
acs2000$immpos3[acs2000$y] <- as.numeric(acs2000$immpos3[acs2000$y])

dens_imm3 =density(acs2000$immpos3[acs2000$y])

#Retransformation  

dens_imm3$x2 <- exp(dens_imm3$x) / (1 + exp(dens_imm3$x))


#Tracé des courbes
# Tracer la première courbe
plot(DEns_imm$x2,
     DEns_imm$y /(DEns_imm$x2 * (1-DEns_imm$x2)), type = "l", lty = 2, #lwd=1,
     ylab="", xlab="rank", col = "orange", xlim = c(0.02, 0.95))

# Ajouter la deuxième courbe
lines(dens_imm$x2,
      dens_imm$y /(dens_imm$x2 * (1-dens_imm$x2)), type = "l", lty = 2, #lwd=1,
      col = "green")

# Tracer la troisième graphique
abline(h = 1, col = "grey")

axis(1, at = c(0.2, 0.4, 0.6, 0.8))

#Tracé quatrième graphique
plot(dens_imm3$x2,
     dens_imm3$y /(dens_imm3$x2 * (1-dens_imm3$x2)), type = "l", lty = 2, #lwd=1,
     ylab="", xlab="rank", col = "violet", xlim = c(0.02, 0.95))

# Ajouter la légende
legend("topright", legend = c("Arrival <= 2 years", "Arrival 3-10 years", "Arrival > 10 years"),
       +        col = c("green", "orange","violet"), lty = c(1, 2,1))



#Question 3
#C:/Users/Mauricette/Desktop
#https://usa.ipums.org/usa-action/data_requests/download
# Installer et charger le package nécessaire

# Assurez-vous que le package ipumsr est installé
# install.packages('ipumsr')

# Charger le package ipumsr
library(haven)
library(tidyverse)
library(stargazer)
library(dplyr)
library(lmtest)
library(sandwich)
library(stargazer)
library(ggplot2)
library('ipumsr')

# Spécifiez le chemin d'accès au fichier .xml
path_xml <- "C:/Users/Mauricette/Desktop/usa_00003.xml"

# Lisez le fichier .xml
ddi <- read_ipums_ddi(path_xml)

# Spécifiez le chemin d'accès au fichier .dat
path_dat <- "C:/Users/Mauricette/Desktop/usa_00003.dat"

# Lisez les données
usa_00003 <- read_ipums_micro(ddi, data_file = path_dat)
usa_00003%>%
  select(YEAR, AGE, LABFORCE, WKSWORK2 , INCWAGE , CLASSWKR , EDUCD , RACE ,
      MARST , EMPSTAT , BPLD , YRIMMIG , SEX , SERIAL , OCC ) ->
  usa_00003

#****ANALYSE EXPLORATOIRE: PRELIMINAIRES******

#Délimitation de l'échantillon de travailleurs.ie la population active 
#celle agée de 18 à 65 ans

usa_00003 %>%
  filter(AGE >=18 & AGE <=65 & LABFORCE == 2,CLASSWKR == 2) ->
  usa_00003


#Création et recodage  des variables : 

#**weeks**: Cette variable est calculée en multipliant différents multiplicateurs 
#par les catégories de la variable wkswork2.  

#**lnw*: Cette variable est calculée avec le logarithme naturel de la variable 
#*"incwage" divisée par la variable weeks

usa_00003 %>%
  mutate(WEEKS  = 7 * (WKSWORK2 == 1) + 20 * (WKSWORK2 == 2) +
           33 * (WKSWORK2 == 3) +
           43.5 * (WKSWORK2 == 4) + 48.5 * (WKSWORK2 == 5) +
           51 * (WKSWORK2 == 6)) %>%
  mutate( LNW = log(INCWAGE / WEEKS)) %>%
  filter(!is.na(LNW), LNW != -Inf, LNW != Inf) ->
  usa_00003 

#***foreign* : la nouvelle variable prendra la valeur "TRUE" si la valeur 
#de "bpld" est supérieure ou égale à 15000. 

#**immclass**: Ajoute une nouvelle variable  initialisée à NA puis 
#Modifie la valeur de la colonne "immclass" en 1 si la valeur de la colonne
#"foreign" est égale à 1 et si la différence entre les valeurs des colonnes 
#"year" et "yrimmig" est inférieure ou égale à 2.
#Modifie la valeur de la colonne "immclass" en 2 si la valeur de 
#la colonne "foreign" est égale à 1 et si la différence entre les valeurs 
#des colonnes "year" et "yrimmig" est supérieure à 2 et inférieure ou égale à 5, etc

usa_00003 %>%
  mutate(FOREIGN  = (BPLD >=15000),
        IMMCLASS = case_when(FOREIGN==1 & YEAR-YRIMMIG<=2 ~ 1,
                              FOREIGN==1 & YEAR-YRIMMIG> 2 & YEAR-YRIMMIG<=5 ~ 2,
                              FOREIGN==1 & YEAR-YRIMMIG> 5 & YEAR-YRIMMIG<=10 ~ 3,
                              FOREIGN==1 & YEAR-YRIMMIG> 10 & !is.na(YEAR-YRIMMIG) ~ 4
         ),
         AGECAT  = case_when(AGE  >= 18 & AGE <= 25 ~ 1, 
                             AGE > 25 & AGE <= 35 ~ 2,
                             AGE > 35 & AGE <= 45 ~ 3,
                             AGE > 45 ~ 4, 
                           TRUE ~ NA_real_
         ))-> usa_00003                 


#**Schooling**: La première mutation initialise la variable schooling à 0 
# par défaut.  Ensuite les codes qui suivent mettent à jour 
#la colonne "schooling" dans le tableau de données "acs2000" en 
#remplaçant les zéros  en fonction des valeurs de la colonne 
#"educd" dans chaque ligne



#**pe** : la différence entre 'age' et la somme de 6 et 'schooling', 
#et assigne cette différence à la variable 'pe'

usa_00003 %>%    
  mutate(SCHOOLING  = 0) %>%
  mutate(SCHOOLING = ifelse(EDUCD == 10, 2, SCHOOLING)) %>%
  mutate(SCHOOLING = ifelse(EDUCD == 13, 2.5, SCHOOLING)) %>%
  mutate(SCHOOLING = ifelse(EDUCD == 14, 1, SCHOOLING)) %>%
  mutate(SCHOOLING = ifelse(EDUCD == 15, 2, SCHOOLING)) %>%
  mutate(SCHOOLING = ifelse(EDUCD == 16, 3, SCHOOLING)) %>%
  mutate(SCHOOLING = ifelse(EDUCD == 17, 4, SCHOOLING)) %>%
  mutate(SCHOOLING = ifelse(EDUCD == 20, 6.5,SCHOOLING)) %>%    
  mutate(SCHOOLING = ifelse(EDUCD == 21, 5.5, SCHOOLING)) %>%
  mutate(SCHOOLING = ifelse(EDUCD == 22, 5, SCHOOLING)) %>%
  mutate(SCHOOLING = ifelse(EDUCD == 23, 6, SCHOOLING)) %>%
  mutate(SCHOOLING = ifelse(EDUCD == 25, 7, SCHOOLING)) %>%
  mutate(SCHOOLING = ifelse(EDUCD == 24, 7.5, SCHOOLING)) %>%    
  mutate(SCHOOLING = ifelse(EDUCD == 26, 8, SCHOOLING)) %>%
  mutate(SCHOOLING = ifelse(EDUCD == 30, 9, SCHOOLING)) %>%
  mutate(SCHOOLING = ifelse(EDUCD == 40, 10, SCHOOLING)) %>%    
  mutate(SCHOOLING = ifelse(EDUCD == 50 | EDUCD == 61, 11, SCHOOLING)) %>%
  mutate(SCHOOLING = ifelse(EDUCD == 60 | (EDUCD >= 62 & EDUCD <= 64), 12, SCHOOLING)) %>%
  mutate(SCHOOLING = ifelse(EDUCD >= 65 & EDUCD <= 71, 13, SCHOOLING)) %>%
  mutate(SCHOOLING = ifelse(EDUCD >= 80 & EDUCD <= 90, 14, SCHOOLING)) %>%
  mutate(SCHOOLING = ifelse(EDUCD == 90, 15, SCHOOLING)) %>%
  mutate(SCHOOLING = ifelse(EDUCD == 100 | EDUCD == 101, 16, SCHOOLING)) %>%
  mutate(SCHOOLING = ifelse(EDUCD == 110, 17, SCHOOLING)) %>%
  mutate(SCHOOLING = ifelse(EDUCD == 111 | EDUCD == 114, 18, SCHOOLING)) %>%
  mutate(SCHOOLING = ifelse(EDUCD == 112, 19, SCHOOLING)) %>%
  mutate(SCHOOLING = ifelse(EDUCD == 113 | EDUCD > 114, 20, SCHOOLING)) %>%
  mutate(PE = as.integer(AGE - 6 - SCHOOLING))  ->
  usa_00003

#L'instruction ci dessous filtre le df en gardant uniquement les 
#lignes où lnw n'est pas NA (non disponible) et où pe
#est supérieur ou égal à 1 et inférieur ou égal à 40.
#En d'autres termes, les lignes avec des valeurs manquantes 
#de lnw sont exclues et seules les lignes avec des valeurs de
#pe comprises entre 1 et 40 sont gardées

usa_00003 %>%
  filter(!is.na(LNW), PE >= 1 & PE <= 40) ->
  usa_00003

#affichage de la base définitive après l'analyse exploratoire 
#On obtient bien 146,577 observations et 22 colonnes
#y compris celles nouvellement créees

glimpse(usa_00003) 

#***Tache de Replication***

# Charger les bibliothèques nécessaires


MODF <-LNW ~ EDUCD +AGE+I(AGE^2)+ SEX+EDUCD:AGE + FOREIGN 

REG2 <- lm(MODF, data=usa_00003, subset=(FOREIGN==0 |IMMCLASS==1))
REG3 <- lm(MODF, data=usa_00003, subset=(FOREIGN==0 |IMMCLASS==2))
REG4 <- lm(MODF, data=usa_00003, subset=(FOREIGN==0 |IMMCLASS==3))
REG5 <- lm(MODF, data=usa_00003, subset=(FOREIGN==0 |IMMCLASS==4))


stargazer(REG2,REG3,REG4,REG5,
          dep.var.caption="0-2 years 3-5 years 6-10 years more than 10 years",dep.var.labels="",
          omit.table.layout = "n", star.cutoffs = NA,
          keep.stat=c("n", "rsq"),no.space=TRUE,
          keep=c("foreignTRUE"),
          header=TRUE,
          title="Table replication",type="text"
)

usa_00003$LNW_PRED <- NA
usa_00003$SIGMA <- NA

usa_00003$IN_REG <- (usa_00003$SEX == 1 & usa_00003$FOREIGN == 0)
MOD <- lm(LNW ~ FOREIGN + AGE + EDUCD + EDUCD*AGE,
          data=usa_00003, subset=IN_REG)
usa_00003$SIGMA[usa_00003$IN_REG] <- (MOD$residuals)^2
usa_00003$LNW_PRED[usa_00003$SEX == 1] <- predict(MOD, usa_00003[usa_00003$SEX == 1,])

usa_00003 %>% arrange(SERIAL) -> usa_00003
set.seed(1234)

for (i in min(usa_00003$AGECAT):max(usa_00003$AGECAT)) {
  for (k in 1:2) {
    for (j in min(usa_00003$EDUCD):max(usa_00003$EDUCD) ) {
      usa_00003$IN_REG <- (usa_00003$AGECAT==i & usa_00003$SEX==k & usa_00003$EDUCD==j)
      N = sum(usa_00003$IN_REG)
      if (N>0) {
        sdS = sqrt(mean(usa_00003$SIGMA[usa_00003$IN_REG], na.rm=TRUE))
        # cat(i, k, j, "N=",N, "  ", sdS, "\n")
        usa_00003$LNW_PRED[usa_00003$IN_REG] = usa_00003$LNW_PRED[usa_00003$IN_REG] +
          rnorm(N,0,sdS)
      }
    }
  }
}

usa_00003 %>%
  filter(!is.na(LNW_PRED)) %>%
  mutate(LNW_PRED = ifelse(FOREIGN==0, LNW,LNW_PRED)) ->
  usa_00003

#Question 2
#Calcul des rang centiles

usa_00003 <- usa_00003 %>% 
  mutate(x = (FOREIGN==0)) %>% 
  arrange(LNW) %>% # sorting
  mutate(RANK = cumsum(x) ) %>% # somme cumulée de l'indicateur qui prouve qu'on n'est pas immigrée
  mutate(RANK = RANK / max(RANK)) %>% # normalisation to [0,1] 
  mutate(IMMPOS = ifelse(log(RANK/(1-RANK)) < Inf, log(RANK/(1-RANK)), NA)) # première transformation des rangs par la transformation logarithmique


#estimation de la densité

DENS_IMM =density(usa_00003$IMMPOS[usa_00003$FOREIGN == 1])

#Retransformation  

DENS_IMM$x2 <- exp(DENS_IMM$x) / (1 + exp(DENS_IMM$x))

#Calcule des rangs centiles pour les salaires prédits 

#Il s'agit normalement de Calculer la variable  `Foreign predicted` et d'appliquer la meme pocédure comme pour pour `lnw_pred`.   Voici la procédure que nous proposons : 


# La partie des rangs centiles pour lnw_pred 

# Calculer  le Foreign_pred et appliquer la même procédure: 

# Construction du modèle de régression logistique
MODELE <- glm(FOREIGN ~ BPLD + IMMCLASS, DATA = usa_00003, family = "binomial")

# Résumé du modèle
#summary(modele)

usa_00003 <- usa_00003 %>%
  mutate(FOREIGN_PRED = predict(MODELE, NEWDATA = ., type = "response")) %>%
  mutate(FOREIGN_PRED = ifelse(is.na(FOREIGN_PRED), 0, FOREIGN_PRED)) %>%
  mutate(FOREIGN_PRED = as.logical(FOREIGN_PRED))

usa_00003 <- usa_00003 %>%
  mutate(y = (FOREIGN_PRED == 0)) %>%
  arrange(LNW_PRED) %>% # sorting
  mutate(RANK_PRED = cumsum(y)) %>%
  mutate(RANK_PRED = RANK_PRED / max(RANK_PRED)) %>%
  mutate(IMMPOS_PRED = ifelse(log(RANK_PRED/(1-RANK_PRED)) < Inf, log(RANK_PRED/(1-RANK_PRED)), NA))


#Estimation de la densité : 

DENS_IMM_PRED= density(usa_00003$IMMPOS_PRED[usa_00003$FOREIGN_PRED == 1])

# Puis la retransformation  

DENS_IMM_PRED$y2 <- exp(DENS_IMM_PRED$y) / (1 + exp(DENS_IMM_PRED$y))


#Cependant en optant pour cette option nous avons rencontré dans le calcul de la variable `Foreign predicted`.
#Voici le code similaire que nous avons finalement utilisé  pour calculer les rangs centiles prédits: 
ecdf_LNWPR <- ecdf(usa_00003$LNW)
CENTILES_PREDI <- ecdf_LNWPR(seq(min(usa_00003$LNW_PRED), max(usa_00003$LNW_PRED), length.out = 101))
CENTILES_PREDI = CENTILES_PREDI[1:100]
RANG_P <- log(CENTILES_PREDI /(1-CENTILES_PREDI))

# Estimation de la densité de la distribution des rangs transformés :

DENS_IMM1 <- density(RANG_P)

#Puis la rétransformation  

INV_TRANSFORM <- function(x) {return (exp(x) / (1 +exp(x)))}

x3= INV_TRANSFORM(DENS_IMM1$x)
DENS1=density(x3)

plot(DENS1,xlim=c(0,0.9))
lines(DENS1)

# Le graphique  

# Réinitialisation des limites du clip après le graphique

# Tracer la première graphique
plot(DENS_IMM$x2,
     DENS_IMM$y /(DENS_IMM$x2 * (1-DENS_IMM$x2)), type = "l", lty = 2, #lwd=1,
     ylab="", xlab="rank", col = "green", xlim = c(0.05, 0.95))


# Tracer la deuxième graphique # A corriger
lines(DENS1, type="l",
      ylab="", xlab="rank", col = "black")

# Tracer la troisième graphique
abline(h = 1, col = "grey")

axis(1, at = c(0.2, 0.4, 0.6, 0.8))
# Ajouter la légende
legend("topright", legend = c("Foreign predicted", "Foreign", "Natives"),
       col = c("black", "green", "grey"), lty = c(1, 2, 1))



