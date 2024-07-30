library(ggplot2)
library(tidyverse)
library(dplyr)
library(tidyr)
library(stringr)

##### Adultes ----

### Yawzi_tektite ----

# Runer ligne 11 a 52 ensemble 
# Charger les données
cover <- read.csv("Data/raw/Biotique/Adultes/CSUN_USVI_yawzi_tektite_20210401.csv",
                  header = F,
                  skip = 1,
                  sep = ";",
                  col.names = c("date",     
                                "site",     
                                "transect",     
                                "quadrat",     
                                "percentCover_allCoral",     
                                "percentCover_macroalgae",     
                                "percentCover_CTB"),
                  check.names = TRUE)

# Convertir la colonne date en format Date
cover$date <- as.Date(cover$date, format = "%d/%m/%Y")

# Vérifier les types des colonnes et les convertir si nécessaire
cover$site <- as.factor(cover$site)
cover$transect <- as.numeric(as.character(cover$transect))
cover$quadrat <- as.factor(cover$quadrat)
cover$percentCover_allCoral <- as.numeric(as.character(cover$percentCover_allCoral))
cover$percentCover_macroalgae <- as.numeric(as.character(cover$percentCover_macroalgae))
cover$percentCover_CTB <- as.numeric(as.character(cover$percentCover_CTB))

# Convertir les valeurs manquantes en NA
cover$percentCover_allCoral <- ifelse(trimws(as.character(cover$percentCover_allCoral)) == "nd", NA, cover$percentCover_allCoral)
cover$percentCover_macroalgae <- ifelse(trimws(as.character(cover$percentCover_macroalgae)) == "nd", NA, cover$percentCover_macroalgae)
cover$percentCover_CTB <- ifelse(trimws(as.character(cover$percentCover_CTB)) == "nd", NA, cover$percentCover_CTB)

# Calculer la moyenne par date et site
cover_mean <- cover %>%
  group_by(date, site) %>%
  summarise(across(starts_with("percentCover"), mean, na.rm = TRUE))

# Renommer les colonnes
cover_mean <- cover_mean %>%
  rename(Coral = percentCover_allCoral, 
         Macroalgae = percentCover_macroalgae)

# Créer les graphiques
ggplot(data = cover_mean) +
  geom_path(aes(x = date, y = Coral, color = site)) +
  scale_x_date(breaks = scales::breaks_pretty(20)) +
  labs(title = "Variabilité du Recouvrement des Coraux",
       x = "Date",
       y = "Couverture de Corail (%)",
       color = "Site") +
  theme_minimal()

ggplot() +
  geom_path(mapping = aes(x = date, y = Macroalgae, color = site), 
             data = cover_mean)+
  scale_x_date(breaks = scales ::breaks_pretty(20)) +
  labs(title = "Variabilité du Recouvrement des Algues",
       x = "Date",
       y = "Couverture des algues (%)",
       color = "Site") +
  theme_minimal()

# Pivot pour ploter ensemble
cover_long <- pivot_longer(cover_mean, -(date : site) , names_to="Type", values_to="Cover")

ggplot() +
  geom_point(mapping = aes(x = date, y = Cover, color = Type, shape = site ), 
            data = cover_long)+
  geom_line(mapping = aes(x = date, y = Cover, color = Type), 
             data = cover_long)+
  scale_x_date(breaks = scales ::breaks_pretty(20)) +
  labs(title = "Variabilité du Recouvrement total",
       x = "Date",
       y = "Couverture (%)",
       color = "Type",
       shape = "Site") +
  theme_minimal()


### Random ----

species_ab <-read.csv("Data/raw/Biotique/Adultes/CSUN_USVI_random_20210401.csv",header=F 
               ,skip=1
               ,sep=";"  
               , col.names=c(
                 "site",     
                 "quadrat",     
                 "year",     
                 "Coral",     
                 "Macroalgae",     
                 "CTB",     
                 "Orbicella",     
                 "Montastrea_cavernosa",     
                 "Agaricia",     
                 "Colpophyllia",     
                 "Dendrogyra",     
                 "Dichocoenia",     
                 "Diploria",     
                 "Eusmilia",     
                 "Favia",     
                 "Madracis",     
                 "Meandrina",     
                 "Mussa",     
                 "Porites",     
                 "Stephanocoenia",     
                 "Siderastrea",     
                 "Manicina",     
                 "Mycetopohyllia",     
                 "Acropora",     
                 "Isophyllastrea",     
                 "Scolymia",
                 "Millepora"), check.names=TRUE)

# Fix any interval or ratio columns mistakenly read in as nominal and nominal columns read as numeric or dates read as strings
if (class(species_ab$site)!="factor") species_ab$site<- as.factor(species_ab$site)
if (class(species_ab$quadrat)=="factor") species_ab$quadrat <-as.numeric(levels(species_ab$quadrat))[as.integer(species_ab$quadrat) ]               
if (class(species_ab$quadrat)=="character") species_ab$quadrat <-as.numeric(species_ab$quadrat)
if (class(species_ab$percentCover_all)=="factor") species_ab$percentCover_all <-as.numeric(levels(species_ab$percentCover_all))[as.integer(species_ab$percentCover_all) ]               
if (class(species_ab$percentCover_all)=="character") species_ab$percentCover_all <-as.numeric(species_ab$percentCover_all)
if (class(species_ab$percentCover_macroalgae)=="factor") species_ab$percentCover_macroalgae <-as.numeric(levels(species_ab$percentCover_macroalgae))[as.integer(species_ab$percentCover_macroalgae) ]               
if (class(species_ab$percentCover_macroalgae)=="character") species_ab$percentCover_macroalgae <-as.numeric(species_ab$percentCover_macroalgae)
if (class(species_ab$percentCover_CTB)=="factor") species_ab$percentCover_CTB <-as.numeric(levels(species_ab$percentCover_CTB))[as.integer(species_ab$percentCover_CTB) ]               
if (class(species_ab$percentCover_CTB)=="character") species_ab$percentCover_CTB <-as.numeric(species_ab$percentCover_CTB)
if (class(species_ab$Orbicella)=="factor") species_ab$Orbicella <-as.numeric(levels(species_ab$Orbicella))[as.integer(species_ab$Orbicella) ]               
if (class(species_ab$Orbicella)=="character") species_ab$Orbicella <-as.numeric(species_ab$Orbicella)
if (class(species_ab$Montastrea_cavernosa)=="factor") species_ab$Montastrea_cavernosa <-as.numeric(levels(species_ab$Montastrea_cavernosa))[as.integer(species_ab$Montastrea_cavernosa) ]               
if (class(species_ab$Montastrea_cavernosa)=="character") species_ab$Montastrea_cavernosa <-as.numeric(species_ab$Montastrea_cavernosa)
if (class(species_ab$Agaricia)=="factor") species_ab$Agaricia <-as.numeric(levels(species_ab$Agaricia))[as.integer(species_ab$Agaricia) ]               
if (class(species_ab$Agaricia)=="character") species_ab$Agaricia <-as.numeric(species_ab$Agaricia)
if (class(species_ab$Colpophyllia)=="factor") species_ab$Colpophyllia <-as.numeric(levels(species_ab$Colpophyllia))[as.integer(species_ab$Colpophyllia) ]               
if (class(species_ab$Colpophyllia)=="character") species_ab$Colpophyllia <-as.numeric(species_ab$Colpophyllia)
if (class(species_ab$Dendrogyra)=="factor") species_ab$Dendrogyra <-as.numeric(levels(species_ab$Dendrogyra))[as.integer(species_ab$Dendrogyra) ]               
if (class(species_ab$Dendrogyra)=="character") species_ab$Dendrogyra <-as.numeric(species_ab$Dendrogyra)
if (class(species_ab$Dichocoenia)=="factor") species_ab$Dichocoenia <-as.numeric(levels(species_ab$Dichocoenia))[as.integer(species_ab$Dichocoenia) ]               
if (class(species_ab$Dichocoenia)=="character") species_ab$Dichocoenia <-as.numeric(species_ab$Dichocoenia)
if (class(species_ab$Diploria)=="factor") species_ab$Diploria <-as.numeric(levels(species_ab$Diploria))[as.integer(species_ab$Diploria) ]               
if (class(species_ab$Diploria)=="character") species_ab$Diploria <-as.numeric(species_ab$Diploria)
if (class(species_ab$Eusmilia)=="factor") species_ab$Eusmilia <-as.numeric(levels(species_ab$Eusmilia))[as.integer(species_ab$Eusmilia) ]               
if (class(species_ab$Eusmilia)=="character") species_ab$Eusmilia <-as.numeric(species_ab$Eusmilia)
if (class(species_ab$Favia)=="factor") species_ab$Favia <-as.numeric(levels(species_ab$Favia))[as.integer(species_ab$Favia) ]               
if (class(species_ab$Favia)=="character") species_ab$Favia <-as.numeric(species_ab$Favia)
if (class(species_ab$Madracis)=="factor") species_ab$Madracis <-as.numeric(levels(species_ab$Madracis))[as.integer(species_ab$Madracis) ]               
if (class(species_ab$Madracis)=="character") species_ab$Madracis <-as.numeric(species_ab$Madracis)
if (class(species_ab$Meandrina)=="factor") species_ab$Meandrina <-as.numeric(levels(species_ab$Meandrina))[as.integer(species_ab$Meandrina) ]               
if (class(species_ab$Meandrina)=="character") species_ab$Meandrina <-as.numeric(species_ab$Meandrina)
if (class(species_ab$Mussa)=="factor") species_ab$Mussa <-as.numeric(levels(species_ab$Mussa))[as.integer(species_ab$Mussa) ]               
if (class(species_ab$Mussa)=="character") species_ab$Mussa <-as.numeric(species_ab$Mussa)
if (class(species_ab$Porites)=="factor") species_ab$Porites <-as.numeric(levels(species_ab$Porites))[as.integer(species_ab$Porites) ]               
if (class(species_ab$Porites)=="character") species_ab$Porites <-as.numeric(species_ab$Porites)
if (class(species_ab$Stephanocoenia)=="factor") species_ab$Stephanocoenia <-as.numeric(levels(species_ab$Stephanocoenia))[as.integer(species_ab$Stephanocoenia) ]               
if (class(species_ab$Stephanocoenia)=="character") species_ab$Stephanocoenia <-as.numeric(species_ab$Stephanocoenia)
if (class(species_ab$Siderastrea)=="factor") species_ab$Siderastrea <-as.numeric(levels(species_ab$Siderastrea))[as.integer(species_ab$Siderastrea) ]               
if (class(species_ab$Siderastrea)=="character") species_ab$Siderastrea <-as.numeric(species_ab$Siderastrea)
if (class(species_ab$Manicina)=="factor") species_ab$Manicina <-as.numeric(levels(species_ab$Manicina))[as.integer(species_ab$Manicina) ]               
if (class(species_ab$Manicina)=="character") species_ab$Manicina <-as.numeric(species_ab$Manicina)
if (class(species_ab$Mycetopohyllia)=="factor") species_ab$Mycetopohyllia <-as.numeric(levels(species_ab$Mycetopohyllia))[as.integer(species_ab$Mycetopohyllia) ]               
if (class(species_ab$Mycetopohyllia)=="character") species_ab$Mycetopohyllia <-as.numeric(species_ab$Mycetopohyllia)
if (class(species_ab$Acropora)=="factor") species_ab$Acropora <-as.numeric(levels(species_ab$Acropora))[as.integer(species_ab$Acropora) ]               
if (class(species_ab$Acropora)=="character") species_ab$Acropora <-as.numeric(species_ab$Acropora)
if (class(species_ab$Isophyllastrea)=="factor") species_ab$Isophyllastrea <-as.numeric(levels(species_ab$Isophyllastrea))[as.integer(species_ab$Isophyllastrea) ]               
if (class(species_ab$Isophyllastrea)=="character") species_ab$Isophyllastrea <-as.numeric(species_ab$Isophyllastrea)
if (class(species_ab$Scolymia)=="factor") species_ab$Scolymia <-as.numeric(levels(species_ab$Scolymia))[as.integer(species_ab$Scolymia) ]               
if (class(species_ab$Scolymia)=="character") species_ab$Scolymia <-as.numeric(species_ab$Scolymia)

# Convert Missing Values to NA for non-dates
species_ab$quadrat <- ifelse((trimws(as.character(species_ab$quadrat))==trimws("nd")),NA,species_ab$quadrat)               
suppressWarnings(species_ab$quadrat <- ifelse(!is.na(as.numeric("nd")) & (trimws(as.character(species_ab$quadrat))==as.character(as.numeric("nd"))),NA,species_ab$quadrat))
species_ab$Orbicella <- ifelse((trimws(as.character(species_ab$Orbicella))==trimws("nd")),NA,species_ab$Orbicella)               
suppressWarnings(species_ab$Orbicella <- ifelse(!is.na(as.numeric("nd")) & (trimws(as.character(species_ab$Orbicella))==as.character(as.numeric("nd"))),NA,species_ab$Orbicella))
species_ab$Montastrea_cavernosa <- ifelse((trimws(as.character(species_ab$Montastrea_cavernosa))==trimws("nd")),NA,species_ab$Montastrea_cavernosa)               
suppressWarnings(species_ab$Montastrea_cavernosa <- ifelse(!is.na(as.numeric("nd")) & (trimws(as.character(species_ab$Montastrea_cavernosa))==as.character(as.numeric("nd"))),NA,species_ab$Montastrea_cavernosa))
species_ab$Agaricia <- ifelse((trimws(as.character(species_ab$Agaricia))==trimws("nd")),NA,species_ab$Agaricia)               
suppressWarnings(species_ab$Agaricia <- ifelse(!is.na(as.numeric("nd")) & (trimws(as.character(species_ab$Agaricia))==as.character(as.numeric("nd"))),NA,species_ab$Agaricia))
species_ab$Colpophyllia <- ifelse((trimws(as.character(species_ab$Colpophyllia))==trimws("nd")),NA,species_ab$Colpophyllia)               
suppressWarnings(species_ab$Colpophyllia <- ifelse(!is.na(as.numeric("nd")) & (trimws(as.character(species_ab$Colpophyllia))==as.character(as.numeric("nd"))),NA,species_ab$Colpophyllia))
species_ab$Dendrogyra <- ifelse((trimws(as.character(species_ab$Dendrogyra))==trimws("nd")),NA,species_ab$Dendrogyra)               
suppressWarnings(species_ab$Dendrogyra <- ifelse(!is.na(as.numeric("nd")) & (trimws(as.character(species_ab$Dendrogyra))==as.character(as.numeric("nd"))),NA,species_ab$Dendrogyra))
species_ab$Dichocoenia <- ifelse((trimws(as.character(species_ab$Dichocoenia))==trimws("nd")),NA,species_ab$Dichocoenia)               
suppressWarnings(species_ab$Dichocoenia <- ifelse(!is.na(as.numeric("nd")) & (trimws(as.character(species_ab$Dichocoenia))==as.character(as.numeric("nd"))),NA,species_ab$Dichocoenia))
species_ab$Diploria <- ifelse((trimws(as.character(species_ab$Diploria))==trimws("nd")),NA,species_ab$Diploria)               
suppressWarnings(species_ab$Diploria <- ifelse(!is.na(as.numeric("nd")) & (trimws(as.character(species_ab$Diploria))==as.character(as.numeric("nd"))),NA,species_ab$Diploria))
species_ab$Eusmilia <- ifelse((trimws(as.character(species_ab$Eusmilia))==trimws("nd")),NA,species_ab$Eusmilia)               
suppressWarnings(species_ab$Eusmilia <- ifelse(!is.na(as.numeric("nd")) & (trimws(as.character(species_ab$Eusmilia))==as.character(as.numeric("nd"))),NA,species_ab$Eusmilia))
species_ab$Favia <- ifelse((trimws(as.character(species_ab$Favia))==trimws("nd")),NA,species_ab$Favia)               
suppressWarnings(species_ab$Favia <- ifelse(!is.na(as.numeric("nd")) & (trimws(as.character(species_ab$Favia))==as.character(as.numeric("nd"))),NA,species_ab$Favia))
species_ab$Madracis <- ifelse((trimws(as.character(species_ab$Madracis))==trimws("nd")),NA,species_ab$Madracis)               
suppressWarnings(species_ab$Madracis <- ifelse(!is.na(as.numeric("nd")) & (trimws(as.character(species_ab$Madracis))==as.character(as.numeric("nd"))),NA,species_ab$Madracis))
species_ab$Meandrina <- ifelse((trimws(as.character(species_ab$Meandrina))==trimws("nd")),NA,species_ab$Meandrina)               
suppressWarnings(species_ab$Meandrina <- ifelse(!is.na(as.numeric("nd")) & (trimws(as.character(species_ab$Meandrina))==as.character(as.numeric("nd"))),NA,species_ab$Meandrina))
species_ab$Mussa <- ifelse((trimws(as.character(species_ab$Mussa))==trimws("nd")),NA,species_ab$Mussa)               
suppressWarnings(species_ab$Mussa <- ifelse(!is.na(as.numeric("nd")) & (trimws(as.character(species_ab$Mussa))==as.character(as.numeric("nd"))),NA,species_ab$Mussa))
species_ab$Porites <- ifelse((trimws(as.character(species_ab$Porites))==trimws("nd")),NA,species_ab$Porites)               
suppressWarnings(species_ab$Porites <- ifelse(!is.na(as.numeric("nd")) & (trimws(as.character(species_ab$Porites))==as.character(as.numeric("nd"))),NA,species_ab$Porites))
species_ab$Stephanocoenia <- ifelse((trimws(as.character(species_ab$Stephanocoenia))==trimws("nd")),NA,species_ab$Stephanocoenia)               
suppressWarnings(species_ab$Stephanocoenia <- ifelse(!is.na(as.numeric("nd")) & (trimws(as.character(species_ab$Stephanocoenia))==as.character(as.numeric("nd"))),NA,species_ab$Stephanocoenia))
species_ab$Siderastrea <- ifelse((trimws(as.character(species_ab$Siderastrea))==trimws("nd")),NA,species_ab$Siderastrea)               
suppressWarnings(species_ab$Siderastrea <- ifelse(!is.na(as.numeric("nd")) & (trimws(as.character(species_ab$Siderastrea))==as.character(as.numeric("nd"))),NA,species_ab$Siderastrea))
species_ab$Manicina <- ifelse((trimws(as.character(species_ab$Manicina))==trimws("nd")),NA,species_ab$Manicina)               
suppressWarnings(species_ab$Manicina <- ifelse(!is.na(as.numeric("nd")) & (trimws(as.character(species_ab$Manicina))==as.character(as.numeric("nd"))),NA,species_ab$Manicina))
species_ab$Mycetopohyllia <- ifelse((trimws(as.character(species_ab$Mycetopohyllia))==trimws("nd")),NA,species_ab$Mycetopohyllia)               
suppressWarnings(species_ab$Mycetopohyllia <- ifelse(!is.na(as.numeric("nd")) & (trimws(as.character(species_ab$Mycetopohyllia))==as.character(as.numeric("nd"))),NA,species_ab$Mycetopohyllia))
species_ab$Acropora <- ifelse((trimws(as.character(species_ab$Acropora))==trimws("nd")),NA,species_ab$Acropora)               
suppressWarnings(species_ab$Acropora <- ifelse(!is.na(as.numeric("nd")) & (trimws(as.character(species_ab$Acropora))==as.character(as.numeric("nd"))),NA,species_ab$Acropora))
species_ab$Isophyllastrea <- ifelse((trimws(as.character(species_ab$Isophyllastrea))==trimws("nd")),NA,species_ab$Isophyllastrea)               
suppressWarnings(species_ab$Isophyllastrea <- ifelse(!is.na(as.numeric("nd")) & (trimws(as.character(species_ab$Isophyllastrea))==as.character(as.numeric("nd"))),NA,species_ab$Isophyllastrea))
species_ab$Scolymia <- ifelse((trimws(as.character(species_ab$Scolymia))==trimws("nd")),NA,species_ab$Scolymia)               
suppressWarnings(species_ab$Scolymia <- ifelse(!is.na(as.numeric("nd")) & (trimws(as.character(species_ab$Scolymia))==as.character(as.numeric("nd"))),NA,species_ab$Scolymia))

# Selection des donnees
species <- select(species_ab,
              site,
              year,
              Orbicella : Scolymia)  %>%
  pivot_longer(Orbicella : Scolymia , names_to="genus", values_to="Cover")

# Particularité de 2017
species$year <- gsub("^11_(\\d{4})$", "\\1", species$year)
species$year <- gsub("^7_(\\d{4})$", "\\1", species$year)

# Plot
ggplot(species, aes(x = year, y = Cover, fill = genus)) +
  geom_bar(position = "stack", stat = "identity") +
  labs(title = "Variabilité du Recouvrement des Coraux selon les Genres",
       x = "Années",
       y = "Couverture des coraux (%)",
       fill = "Genres") +
  theme_minimal()


### Octo ----
Octocoralien <- read_delim("Data/raw/Biotique/Adultes/Octocoralien.csv", 
                           ";", escape_double = FALSE, col_types = cols(year = col_date(format = "%Y"), 
                                                                        count = col_number()), trim_ws = TRUE)

Octo <- Octocoralien %>%
  group_by(year,site) %>%
  summarise_at(vars(count),
               list(sum), na.rm = TRUE) 
colnames(Octo) <- c("Date","Site","Mean")


Octo <- read_delim("Data/raw/Biotique/Adultes/Octo-long.csv", 
                        ";", escape_double = FALSE, col_types = cols(Date = col_date(format = "%m/%d/%Y"), 
                                                                     Somme = col_number(), Mean = col_number()), 
                        locale = locale(decimal_mark = ","), 
                        trim_ws = TRUE)

ggplot(Octo, aes(x=Date, y=Mean, color = Site)) + 
  geom_line() +
  geom_point() +
  labs(title = "Evolution du nombre moyen d'octocoraliens par transect par années",
       x = "Années",
       y = "Nombre moyen d'octocoraliens par transect",
       color = "Site") +
  theme_minimal()

### Sclero ----
Scleractinien <- read_delim("Data/raw/Biotique/Adultes/CSUN_USVI_yawzi_tektite_20210401.csv", 
                     ";", escape_double = FALSE, col_types = cols(Date = col_date(format = "%d/%m/%Y")), 
                     trim_ws = TRUE)

Scleractinien$Date <- substr(Scleractinien$Date,1,4)

Scleractinien <- select(Scleractinien,
                 Date, 
                 Site = site,
                 Scleractinian = percentCover_allCoral,
                 Macroalgae = percentCover_macroalgae,
                 CTB =percentCover_CTB)

Scleractinien2 <- pivot_longer(Scleractinien, col = c("Scleractinian", "Macroalgae", "CTB"), 
             names_to = "Type", values_to = "Recouvrement" )

Sclero <- Scleractinien2 %>%
  group_by(Date,Site, Type) %>%
  summarise_at(vars(Recouvrement), list(mean, sd), na.rm = TRUE) 

colnames(Sclero) <- c("Date","Site", "Type", "Mean", "Sd")

Sclero <- read_delim("Data/raw/Biotique/Adultes/Sclero-long.csv", 
                          ";", escape_double = FALSE, col_types = cols(Date = col_date(format = "%Y"), 
                                                                       Mean = col_number(), Sd = col_number()), 
                          trim_ws = TRUE)

ggplot(Sclero, aes(x=Date, y=Mean, group = Type, color = Type)) + 
  geom_line() +
  geom_point() +
  labs(title = "Evolution du nombre moyen de colonies par transect par années",
       x = "Années",
       y = "Nombre moyen de colonies par transect",
       color = "Type de colonies") +
  theme_minimal()

### Jointure ----

all_biotique <- full_join(Sclero, Octo, by = c("Date", "Site", "Type", "Mean"))

Tektite <- all_biotique %>%
  filter(Site == "Tektite") %>%
  select(!Site)

Yawzi <- all_biotique %>%
  filter(Site == "Yawzi") %>%
  select(!Site)



### Plot ----
ggplot(Tektite, aes(x=Date, y=Mean, group=Type, color=Type)) + 
  geom_line() +
  geom_point()+
  geom_linerange(aes(ymin=Mean-Sd, ymax=Mean+Sd), alpha = 0.3)+
  scale_x_date(breaks = scales ::breaks_pretty(20)) +
  labs(title = "Evolution du Pourcentage de recouvrement par Abondance à Tektite",
       x = "Années",
       y = "Pourcentage de recouvrement/Abondance",
       color = "Type de colonies") +
  theme_minimal()



ggplot(Yawzi, aes(x=Date, y=Mean, group=Type, color=Type)) + 
  geom_line() +
  geom_point()+
  geom_linerange(aes(ymin=Mean-Sd, ymax=Mean+Sd), alpha = 0.3)+
  scale_x_date(breaks = scales ::breaks_pretty(20)) +
  labs(title = "Evolution du Pourcentage de recouvrement par Abondance à Yawzi",
       x = "Années",
       y = "Pourcentage de recouvrement/Abondance",
       color = "Type de colonies") +
  theme_minimal()


### Recru ----
Recru <- read_csv("Data/raw/Biotique/Recrutement/CSUN_USVI_spat_counts_20181007.csv", 
                  col_types = cols(Year = col_date(format = "%Y"), 
                                   Total = col_number()))
Recru <- Recru %>%
  select(Site, Year, Total)%>%
  rename(Recrutement = Total) %>%
  filter(Site == "Tektite" | Site =="Yawzi Point")%>%
  group_by(Year, Site) %>%
  summarize_at(vars(Recrutement), list(mean))

ggplot(Recru, aes(x=Year, y=Recrutement, group=Site, color=Site)) + 
  geom_line() +
  geom_point()+
  scale_x_date(breaks = scales ::breaks_pretty(10)) +
  labs(title = "Recrutement en fonction du temps",
       x = "Années",
       y = "Abondance moyenne de recrues par tuile",
       color = "Type de colonies") +
  theme_minimal()
