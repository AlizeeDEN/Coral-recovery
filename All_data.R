library(tidyr)
library(readr)
library(ggplot2)
library(dplyr)
library(stringr)
library(FactoMineR)
library(corrplot)
library(stats)
library(factoextra)

### Temperature ----


temp <- read_delim("Data/raw/Abiotique/CSUN_USVI_temperature_Daily_20190609.csv", 
                   ";", escape_double = FALSE, col_types = cols(Temperature = col_number(), 
                                                                Date = col_date(format = "%d/%m/%Y")), 
                   trim_ws = TRUE)
Temp_year <- temp
Temp_year$Date <- substr(Temp_year$Date,1,4)

Temp_year <- Temp_year %>%
  group_by(Date) %>%
  summarize_at(vars(Temperature), list(mean,min,max), na.rm = TRUE)

colnames(Temp_year) <- c("Date","Temp_mean", "Temp_min", "Temp_max")

Temp_year$Date <- as.numeric(Temp_year$Date)


### DHM ----
DHM <- read.csv("Data/raw/Abiotique/DHM.csv",
                      header=T, sep=";",
                      col.names=c("Date", "DHM"), check.names=TRUE)

### DHW ----
DHW <- read_delim("Data/raw/Abiotique/DHW.csv", 
                  ";", escape_double = FALSE, col_types = cols(Date = col_number(), 
                                                               DHW = col_number()), trim_ws = TRUE)

### Rainfall ----
rainfall <- read_csv("Data/raw/Abiotique/CSUN_USVI_rainfall_20190609.csv", 
                     col_types = cols(Rainfall = col_number()))

rainfall <- rainfall[-c(599:644),] # Enleve les dernères lignes vides

rainfall_mean <- rainfall[c(553:598),c(1,3)]
colnames(rainfall_mean) <- c("Date","Rainfall")
rainfall_mean$Date <- as.numeric(rainfall_mean$Date)

rainfall_final <- rainfall_mean[-c(1:17),]

### Anomalies ----
Anomalie <- read_delim("Data/raw/Abiotique/Anomalie.csv", 
                       ";", escape_double = FALSE, col_types = cols(Date = col_number(), 
                                                                    Temp_mean = col_number(), STA = col_number()), 
                       trim_ws = TRUE)

### Hurricanes ----
Hurricane <- read_delim("Data/raw/Abiotique/Hurricane.csv", 
                        ";", escape_double = FALSE, col_types = cols(Date = col_number(), 
                                                                     Hurricane = col_number()), trim_ws = TRUE)
Hurricane <- Hurricane[-31,]

### Light 5min ----

light_5min <-read.csv("Data/raw/Abiotique/CSUN_USVI_light_5min_20190609.csv",
                      header=F,
                      skip=1,
                      sep=";",
                      col.names=c(
                        "Date_time",     
                        "PAR_uE"    ), check.names=TRUE)


# Fix any interval or ratio columns mistakenly read in as nominal and nominal columns read as numeric or dates read as strings
if (class(light_5min$PAR_uE)=="factor") light_5min$PAR_uE <-as.numeric(levels(light_5min$PAR_uE))[as.integer(light_5min$PAR_uE) ]               
if (class(light_5min$PAR_uE)=="character") light_5min$PAR_uE <-as.numeric(light_5min$PAR_uE)

row.has.na <- apply(light_5min, 1, function(x){any(is.na(x))}) 

sum(row.has.na) 

l5_m <- light_5min[!row.has.na,] 

l5_m$Date_time <- substr(l5_m$Date_time,1,8)
l5_m$Date_time <- as.Date(l5_m$Date_time, format = "%m/%d/%y")
l5_m$Date_time <- substr(l5_m$Date_time,1,4)

l5_m <- l5_m %>%
  group_by(Date_time) %>%
  summarize_at(vars(PAR_uE), list(mean), na.rm = TRUE)

colnames(l5_m) <- c("Date","PAR_mean")

l5_m$Date <- as.numeric(l5_m$Date)

light_final <- l5_m[-10,]



### Sclero ----

# Runer ligne 11 a 57 ensemble 
cover <-read.csv("Data/raw/Biotique/Adultes/CSUN_USVI_yawzi_tektite_20210401.csv",
                 header=F,
                 skip=1,
                 sep=";",
                 col.names=c(
                   "Date",     
                   "site",     
                   "transect",     
                   "quadrat",     
                   "percentCover_allCoral",     
                   "percentCover_macroalgae",     
                   "percentCover_CTB"    ), check.names=TRUE)


# Fix any interval or ratio columns mistakenly read in as nominal and nominal columns read as numeric or dates read as strings

if (class(cover$site)!="factor") cover$site<- as.factor(cover$site)
if (class(cover$transect)=="factor") cover$transect <-as.numeric(levels(cover$transect))[as.integer(cover$transect) ]               
if (class(cover$transect)=="character") cover$transect <-as.numeric(cover$transect)
if (class(cover$quadrat)!="factor") cover$quadrat<- as.factor(cover$quadrat)
if (class(cover$percentCover_allCoral)=="factor") cover$percentCover_allCoral <-as.numeric(levels(cover$percentCover_allCoral))[as.integer(cover$percentCover_allCoral) ]               
if (class(cover$percentCover_allCoral)=="character") cover$percentCover_allCoral <-as.numeric(cover$percentCover_allCoral)
if (class(cover$percentCover_macroalgae)=="factor") cover$percentCover_macroalgae <-as.numeric(levels(cover$percentCover_macroalgae))[as.integer(cover$percentCover_macroalgae) ]               
if (class(cover$percentCover_macroalgae)=="character") cover$percentCover_macroalgae <-as.numeric(cover$percentCover_macroalgae)
if (class(cover$percentCover_CTB)=="factor") cover$percentCover_CTB <-as.numeric(levels(cover$percentCover_CTB))[as.integer(cover$percentCover_CTB) ]               
if (class(cover$percentCover_CTB)=="character") cover$percentCover_CTB <-as.numeric(cover$percentCover_CTB)

# Convert Missing Values to NA for non-dates

cover$percentCover_allCoral <- ifelse((trimws(as.character(cover$percentCover_allCoral))==trimws("nd")),NA,cover$percentCover_allCoral)               
suppressWarnings(cover$percentCover_allCoral <- ifelse(!is.na(as.numeric("nd")) & (trimws(as.character(cover$percentCover_allCoral))==as.character(as.numeric("nd"))),NA,cover$percentCover_allCoral))
cover$percentCover_macroalgae <- ifelse((trimws(as.character(cover$percentCover_macroalgae))==trimws("nd")),NA,cover$percentCover_macroalgae)               
suppressWarnings(cover$percentCover_macroalgae <- ifelse(!is.na(as.numeric("nd")) & (trimws(as.character(cover$percentCover_macroalgae))==as.character(as.numeric("nd"))),NA,cover$percentCover_macroalgae))
cover$percentCover_CTB <- ifelse((trimws(as.character(cover$percentCover_CTB))==trimws("nd")),NA,cover$percentCover_CTB)               
suppressWarnings(cover$percentCover_CTB <- ifelse(!is.na(as.numeric("nd")) & (trimws(as.character(cover$percentCover_CTB))==as.character(as.numeric("nd"))),NA,cover$percentCover_CTB))


cover_mean <- cover %>%
  group_by(Date,site) %>%
  summarise_at(vars(percentCover_allCoral, percentCover_macroalgae, percentCover_CTB),
               list(mean), na.rm = TRUE) 

cover_mean <- select(cover_mean,
                     Date, 
                     site,
                     Scleractinian = percentCover_allCoral,
                     Macroalgae = percentCover_macroalgae,
                     CTB =percentCover_CTB)

cover_mean$Date <- as.Date(cover_mean$Date, format = "%d/%m/%Y")
cover_mean$Date <- substr(cover_mean$Date,1,4)


sclero_Tektite <- cover_mean %>%
  filter(site == "Tektite")%>%
  group_by(Date) %>%
  summarize_at(vars(Scleractinian, Macroalgae, CTB), list(mean), na.rm = TRUE)
colnames(sclero_Tektite) <- c("Date","Scleractinian", "Macroalgae", "CTB")

sclero_Tektite$Date <- as.numeric(sclero_Tektite$Date)

sclero_Yawzi <- cover_mean %>%
  filter(site == "Yawzi") %>%
  group_by(Date) %>%
  summarize_at(vars(Scleractinian, Macroalgae, CTB), list(mean), na.rm = TRUE)
colnames(sclero_Yawzi) <- c("Date","Scleractinian", "Macroalgae", "CTB")

sclero_Yawzi$Date <- as.numeric(sclero_Yawzi$Date)


### Octo ----
Octo <- read_delim("Data/raw/Biotique/Adultes/Octo.csv", 
                   ";", escape_double = FALSE, col_types = cols(year = col_number(), 
                                                                count = col_number()), trim_ws = TRUE)

colnames(Octo) <- c("Date","site", "Octocoral")

octo_Tektite <- Octo %>%
  filter(site == "Tektite")%>%
  group_by(Date) %>%
  summarize_at(vars(Octocoral), list(mean), na.rm = TRUE)
colnames(octo_Tektite) <- c("Date","Octocoral", "Max", "Min")


octo_Yawzi <- Octo %>%
  filter(site == "Yawzi") %>%
  group_by(Date) %>%
  summarize_at(vars(Octocoral), list(mean), na.rm = TRUE)
colnames(octo_Yawzi) <- c("Date","Octocoral")

### Jointure Abiotique ----

all_abiotique <- full_join(Anomalie, DHM, by = "Date")
all_abiotique <- full_join(all_abiotique, rainfall_final, by = "Date")
all_abiotique <- full_join(all_abiotique, DHW, by = "Date")
all_abiotique <- full_join(all_abiotique, Hurricane, by = "Date")
all_abiotique <- all_abiotique[-c(29,30),]

### Jointure biotique ----

bio_Tektite <- full_join(octo_Tektite, sclero_Tektite, by = "Date")

bio_Yawzi <- full_join(octo_Yawzi, sclero_Yawzi, by = "Date")

### Jointure complete ----
Tektite <- full_join(all_abiotique, bio_Tektite, by = "Date")
Tektite <- Tektite[-c(29:34),]
Tektite_Date <- Tektite$Date
Tektite <- select(Tektite, -c(Date,STA, DHM))
rownames(Tektite) <- Tektite_Date


Yawzi <- full_join(all_abiotique, bio_Yawzi, by = "Date")
Yawzi <- Yawzi[-c(29:34),]
Yawzi_Date <- Yawzi$Date
Yawzi <- select(Yawzi, -c(Date,STA, DHM))
rownames(Yawzi) <- Yawzi_Date

#write.csv(Yawzi,"Data/processed/Yamzi.csv")
#write.csv(Tektite,"Data/processed/Tektite.csv")


### Correlations ----

cor_Tektite <- cor(Tektite, use	= "na.or.complete", method = "pearson")
corrplot(cor_Tektite , method="number")

cor_Yawzi <- cor(Yawzi, use	= "na.or.complete", method = "pearson")
corrplot(cor_Yawzi , method="number")


### ACP 1 ----
Tektite_pca <- PCA(Tektite)
plot.PCA(Tektite_pca, title = "Tektite")

Yawzi_pca <- PCA(Yawzi)
plot.PCA(Yawzi_pca, title = "Yawzi")

### ACP 2 et remplacement na par moyenne ----
Tektite <- Tektite %>%
  mutate(Octocoral=replace_na(Octocoral, mean(Octocoral, na.rm=TRUE)))

Tektite_pca2 <- prcomp(Tektite, scale=TRUE)
biplot(Tektite_pca2)


Yawzi <- Yawzi %>%
  mutate(Octocoral=replace_na(Octocoral, mean(Octocoral, na.rm=TRUE))) %>%
  mutate(Macroalgae=replace_na(Macroalgae, mean(Macroalgae, na.rm=TRUE))) %>%
  mutate(Scleractinian=replace_na(Scleractinian, mean(Scleractinian, na.rm=TRUE)))%>%
  mutate(CTB=replace_na(CTB, mean(CTB, na.rm=TRUE)))

Yawzi_pca2 <- prcomp(Yawzi, scale=TRUE)
biplot(Yawzi_pca2)

### ACP 3 ----
fviz_pca_biplot(Tektite_pca2, label = c("var","ind"), repel = TRUE, title = "Tektite",
                col.ind="gray20", col.var = "contrib") +
  scale_color_gradient2(low="brown1", mid="deepskyblue3",
                        high="deepskyblue3", midpoint=11, space ="Lab") +
  coord_cartesian(xlim = c(-4, 4), ylim = c(-4, 4)) +
  theme_minimal()



fviz_pca_biplot(Yawzi_pca2, label = c("var","ind"), repel = TRUE, title = "Yawzi",
                col.ind="gray20", col.var = "contrib") +
  scale_color_gradient2(low="brown1", mid="deepskyblue3",
                        high="deepskyblue3", midpoint=11, space ="Lab") +
  coord_cartesian(xlim = c(-4, 4), ylim = c(-4, 4)) +
  theme_minimal()