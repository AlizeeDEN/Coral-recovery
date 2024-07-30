library(tidyr)
library(readr)
library(ggplot2)
library(dplyr)
library(stringr)

### Temperature ----


temp <- read_delim("Data/raw/Abiotique/CSUN_USVI_temperature_Daily_20190609.csv", 
                   ";", escape_double = FALSE, col_types = cols(Temperature = col_number(), 
                                                                Date = col_date(format = "%d/%m/%Y")), 
                   trim_ws = TRUE)


ggplot() +
  geom_line(mapping = aes(x = Date, y = Temperature, color = -Temperature), 
            data = temp) + 
  scale_color_distiller(palette = "YlOrRd") +
  theme_classic()



temp$Date <- substr(temp$Date,1,4)

Temp_year <- temp %>%
  group_by(Date) %>%
  summarize_at(vars(Temperature), list(mean,sd), na.rm = TRUE)
colnames(Temp_year) <- c("Date","Mean", "Sd")
Temp_year$Date <- as.Date(Temp_year$Date, format = "%Y")
Temp_year <- Temp_year[-30,]


ggplot(Temp_year, aes(x=Date, y=Mean, color= -Sd)) + 
  geom_line(color = "orange") +
  geom_point()+
  geom_linerange(aes(ymin=Mean-Sd, ymax=Mean+Sd), alpha = 0.5)+
  scale_x_date(breaks = scales ::breaks_pretty(20)) +
  labs(title="Temperature moyenne au cours du temps", x ="Annees", y = "Temperature moyenne (eC)") +
  scale_color_distiller(palette = "YlOrRd") +
  theme_classic()


          
### Rainfall ----
rainfall <- read_csv("Data/raw/Abiotique/CSUN_USVI_rainfall_20190609.csv", 
                                        col_types = cols(Year = col_date(format = "%Y"), 
                                                         Rainfall = col_number()))



rainfall <- rainfall[-c(599:644),] # Enleve les dernieres lignes vides


# Evolution selon les mois
rainfall_wo_tot <- filter(rainfall, Month != "Total")
  
ggplot() +
  geom_path(mapping = aes(x = Year, y = Rainfall, color = Month), 
             data = rainfall_wo_tot) + # Essayer de faire un scale_colour_
  scale_colour_brewer(palette = "12-class Set3")

  
# Evolution totale
rainfall_tot <-  rainfall  %>%
  filter( Month == "Total") %>%
  select(-Month)


ggplot(rainfall_tot, aes(x=Year, y=Rainfall, color = -Rainfall)) + 
  geom_line() +
  geom_point()+
  scale_x_date(breaks = scales ::breaks_pretty(10)) +
  labs(title="Precipitations totales au cours du temps", x ="Annees", y = "Precipitations cumulees") +
  scale_color_distiller(palette = "Blues") +
  theme_classic()



### Moyenne de pluie selon les mois ----
rain_mean <- rainfall %>%
  group_by(Month) %>%
  summarise_at(vars(Rainfall),
               list(mean))

rain_mean_wo_tot <- rain_mean[-13,] # Enleve la dernere ligne tot

ggplot() +
  geom_bar(mapping = aes(x = Month, y = Rainfall), 
            data = rain_mean_wo_tot, stat = "identity")


# Evolution dans le temps 
rainfall2 <- rainfall
rainfall2$Month = replace(rainfall2$Month, which(rainfall2$Month == "January"), "01-01")
rainfall2$Month = replace(rainfall2$Month, which(rainfall2$Month == "February"), "01-02")
rainfall2$Month = replace(rainfall2$Month, which(rainfall2$Month == "March"), "01-03")
rainfall2$Month = replace(rainfall2$Month, which(rainfall2$Month == "April"), "01-04")
rainfall2$Month = replace(rainfall2$Month, which(rainfall2$Month == "May"), "01-05")
rainfall2$Month = replace(rainfall2$Month, which(rainfall2$Month == "June"), "01-06")
rainfall2$Month = replace(rainfall2$Month, which(rainfall2$Month == "July"), "01-07")
rainfall2$Month = replace(rainfall2$Month, which(rainfall2$Month == "August"), "01-08")
rainfall2$Month = replace(rainfall2$Month, which(rainfall2$Month == "September"), "01-09")
rainfall2$Month = replace(rainfall2$Month, which(rainfall2$Month == "October"), "01-10")
rainfall2$Month = replace(rainfall2$Month, which(rainfall2$Month == "November"), "01-11")
rainfall2$Month = replace(rainfall2$Month, which(rainfall2$Month == "December"), "01-12")


rainfall2 <-  rainfall2  %>%
  filter( Month != "Total")


rainfall2$Date <- paste(rainfall2$Month, rainfall2$Year,sep="-")

rainfall2$Date <- as.Date(rainfall2$Date, format="%d-%m-%Y")

ggplot() +
  geom_line(mapping = aes(x = Date, y = Rainfall), 
           data = rainfall2) +
  geom_point(mapping = aes(x = Date, y = Rainfall), 
             data = rainfall2) +
  scale_x_date(breaks = scales ::breaks_pretty(30))

ggplot(rainfall2, aes(x=Date, y=Rainfall, color = -Rainfall)) + 
  geom_line() +
  geom_point()+
  scale_x_date(breaks = scales ::breaks_pretty(10)) +
  labs(title="Precipitations au cours du temps", x ="Annees", y = "Precipitations mensuelles") +
  scale_color_distiller(palette = "Blues") +
  theme_classic()
