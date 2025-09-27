library(readr)
ruta  <- "C:\\Users\\luisf\\Documents\\trabajo-ventas-de-videojuegos\\data\\video_games_sales corregido.csv"
Data.informacion <- read_csv(ruta)

print(Data.informacion)


library(ggplot2)
library(dplyr)
library(tidyr)


### separamos los lugares de la ventas ###

Ventas_USA <- Data.informacion %>% select(Rank, Name, Platform, Year, Genre, Publisher, NA_Sales  )

Ventas_EU <- Data.informacion %>% select(Rank, Name, Platform, Year, Genre, Publisher, EU_Sales  )

Ventas_JP <- Data.informacion %>% select(Rank, Name, Platform, Year, Genre, Publisher, JP_Sales )

Ventas_Other <- Data.informacion %>% select(Rank, Name, Platform, Year, Genre, Publisher, Other_Sales )



### valores individuales de la data.infomacion ###


Data.informacion$Genre <- as.factor(Data.informacion$Genre)


frecuencias_genre <- summary(Data.informacion$Genre)

frame_genre <-  data.frame(frecuencias_genre)



Data.informacion$Publisher <- as.factor(Data.informacion$Publisher)

Lanzamientos <- summary(Data.informacion$Publisher) 


publicaciones_de_las_empresas <- data.frame(Lanzamientos) 
 




Data.informacion$Platform <- as.factor(Data.informacion$Platform)

Consolas <- summary(Data.informacion$Platform)

consolas_de_lanzamiento <- data.frame(Consolas) 


### agrupaciones de epocas en los continentes continentes ###

# estados Unidos 

Usa_siglo20 <- Ventas_USA %>% filter(Year < 2000)



Usa_2000 <- Ventas_USA %>% filter(Year >= 2000, Year < 2010 )



Usa_2020 <-  Ventas_USA %>% filter(Year  >= 2010 )



# Europa

EU_siglo20 <- Ventas_EU %>% filter(Year < 2000)



EU_2000 <- Ventas_EU %>% filter(Year >= 2000, Year < 2010 )



EU_2020 <-  Ventas_EU %>% filter(Year  >= 2010 )




# Japon

JP_siglo20 <- Ventas_JP %>% filter(Year < 2000)



JP_2000 <- Ventas_JP %>% filter(Year >= 2000, Year < 2010 )



JP_2020 <-  Ventas_JP %>% filter(Year  >= 2010 )



# Otros


Other_siglo20 <- Ventas_Other %>% filter(Year < 2000)



Other_2000 <- Ventas_Other %>% filter(Year >= 2000, Year < 2010 )



Other_2020 <-  Ventas_Other %>% filter(Year  >= 2010 )



### porcentaje de la frecuencia de los generos ###



porcentaje_genero  <- frame_genre  %>% 
  group_by(frecuencias_genre)  %>% 
  count()  %>% 
  ungroup()  %>% 
  mutate(probabilidades = (frecuencias_genre > 1200) /sum(frecuencias_genre))

