library(tidyverse)
library(readxl)

df = read_xlsx("Data/biomass_sargassum_monitoring_data_allstations.xlsx")

df |> 
  select(Station = Station_ID, date  = Day_UTC, biomass = `Sargassum_biomass [kg/m]`) |> 
  filter(biomass != -999) |> 
  ggplot(aes(x = date, y = biomass, color = Station))+
  geom_point()+
  geom_line()+
  theme_bw()

#Mean sargassum biomass timeline
df |> 
  select(Station = Station_ID, date  = Day_UTC, biomass = `Sargassum_biomass [kg/m]`) |> 
  filter(biomass != -999) |> 
  group_by(date) |> 
  summarise(MeanBio = mean(biomass), SDBio = sd(biomass)) |> 
  ggplot(aes(x = date, y = MeanBio))+
  geom_line(alpha = 0.6)+
  geom_point(size = 2, alpha = 0.8)+
  theme_bw()+
  labs(y = "Mean Sargassum Biomass (kg/m)", x = "Year")
  #geom_ribbon(aes(ymin =MeanBio - SDBio, ymax =  MeanBio + SDBio), alpha = 0.6)

#Mean sargassum per month and year
df_date <- df |> 
  select(Station = Station_ID, date  = Day_UTC, biomass = `Sargassum_biomass [kg/m]`) |> 
  filter(biomass != -999) |> 
  mutate(Month = month(date), Year = year(date))

#Month
df_date |> 
  group_by(Month) |> 
  summarise(MeanBio = mean(biomass), SDBio = sd(biomass)) |> 
  ggplot(aes(x = Month, y = MeanBio))+
  geom_point(size = 3)+
  geom_line()+
  theme_bw()+
  labs(y = "Mean Sargassum Biomass (kg/m)", x = "Month")+
  scale_x_continuous(breaks = seq(1,12,1))

df_date |> 
  group_by(Month, Station) |> 
  summarise(MeanBio = mean(biomass), SDBio = sd(biomass)) |> 
  ggplot(aes(x = Month, y = MeanBio, color = Station))+
  geom_point(size = 2)+
  geom_line(alpha = 0.6)+
  theme_bw()+
  labs(y = "Mean Sargassum Biomass (kg/m)", x = "Month")+
  scale_x_continuous(breaks = seq(1,12,1))


#Year
df_date |> 
  group_by(Year) |> 
  summarise(MeanBio = mean(biomass), SDBio = sd(biomass)) |> 
  ggplot(aes(x = Year, y = MeanBio))+
  geom_point(size = 2)+
  geom_line()+
  theme_bw()+
  labs(y = "Mean Sargassum Biomass (kg/m)", x = "Year")

df_date |> 
  group_by(Year, Station) |> 
  summarise(MeanBio = mean(biomass), SDBio = sd(biomass)) |> 
  ggplot(aes(x = Year, y = MeanBio, color = Station))+
  geom_point()+
  geom_line()+
  theme_bw()+
  labs(y = "Mean Sargassum Biomass (kg/m)", x = "Year")


