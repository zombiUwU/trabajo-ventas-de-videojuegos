library(readr)
library(ggplot2)
library(dplyr)
library(tidyr)
library(here)
library(gitcreds)
library(usethis)
use_git_config(
  user.name = "sebastian", # Cambia esto por tu nombre
  user.email = "valbuenasebas2209@gmail.com" # Usa el email asociado a tu cuenta de GitHub
)
create_github_token()

## Configuramos token
library(gitcreds)
gitcreds_set()

# ruta para que funcione en cualquier pc

here()
ruta  <- here("data", "video_games_sales_corregido.csv")

Data.informacion <- read_csv(ruta)

print(Data.informacion)

### separamos los lugares de ventas por pais/continentes ###

### Quite la columna rank y ordene las ventas de mayor a menor

Ventas_USA <- Data.informacion %>% arrange(desc(NA_Sales)) %>% select(Name, Platform, Year, Genre, Publisher, NA_Sales)

Ventas_EU <- Data.informacion %>% arrange(desc(EU_Sales)) %>% select(Name, Platform, Year, Genre, Publisher, EU_Sales)

Ventas_JP <- Data.informacion %>% arrange(desc(JP_Sales)) %>% select(Name, Platform, Year, Genre, Publisher, JP_Sales)

Ventas_Other <- Data.informacion %>% arrange(desc(Other_Sales)) %>% select(Name, Platform, Year, Genre, Publisher, Other_Sales )

### valores individuales de la data.infomacion ###

# cambio de variable de caracter a factor

Data.informacion$Genre <- as.factor(Data.informacion$Genre)

# agrupacion por genero

frecuencias_genre <- summary(Data.informacion$Genre)

frame_genre <-  data.frame(frecuencias_genre)

# cambio de variable a factor y agrupacion por publisher

Data.informacion$Publisher <- as.factor(Data.informacion$Publisher)

Lanzamientos <- summary(Data.informacion$Publisher)

publicaciones_de_las_empresas <- data.frame(Lanzamientos) 
 
publicaciones_de_las_empresas %>% arrange(desc(Lanzamientos))

# cambio de variable a factor y agrupacion por consola

Data.informacion$Platform <- as.factor(Data.informacion$Platform)

Consolas <- summary(Data.informacion$Platform)

consolas_de_lanzamiento <- data.frame(Consolas) 

consolas_de_lanzamiento %>% arrange(desc(Consolas))

### agrupaciones de epocas en los continentes ###

# estados Unidos 

Usa_milenio2 <- Ventas_USA %>% filter(Year < 2000)

Usa_2000 <- Ventas_USA %>% filter(Year >= 2000, Year < 2010 )

Usa_2020 <-  Ventas_USA %>% filter(Year  >= 2010 )

# Europa

EU_milenio2 <- Ventas_EU %>% filter(Year < 2000)

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

### creacion de columna generos ###

generos <- c("Acción",
             "Aventura",
             "Carreras",
             "Deportes",
             "Disparos",
             "Estrategia",
             "Juegos de rol",
             "Pelea",
             "Plataforma",
             "rompecabezas",
             "Simulacion",
             "Varios")

frame_genre <- frame_genre %>%
  mutate(genre = generos)

### porcentaje de la frecuencia de los generos de videojuegos ###

total_genre <- sum(frame_genre$frecuencias_genre)

porcentaje_genero  <- frame_genre  %>% 
  mutate(porcentajes = frecuencias_genre / total_genre * 100) %>% 
  select(genre, frecuencias_genre, porcentajes) %>%
  arrange(desc(porcentajes))

### Grafico de barras para el codigo anterior ###

grafico_genre_videojuegos <- ggplot(data = porcentaje_genero, aes(x = porcentajes, y = reorder(genre, porcentajes))) +
  geom_bar(stat = "identity",
           fill = "steelblue",
           width = 0.7) +
  geom_text(aes(label = paste0(round(porcentajes, 1), "%")), 
            hjust = -0.1, size = 3.5, color = "black") +  
  labs(title = "Porcentaje de Frecuencia por Género de Videojuegos",
       x = "Porcentaje",
       y = "Género") +
  theme_minimal() +
  theme(axis.text.y = element_text(size = 10),  
        plot.title = element_text(hjust = 0.5, size = 14, face = "bold")) + 
  scale_x_continuous(limits = c(0, max(porcentaje_genero$porcentajes) * 1.1)) 

print(grafico_genre_videojuegos)

### creacion de columna publishers ###

Publishers <- c("Electronic Arts",
                "Activision" ,
                "Namco Bandai Games",
                "Ubisoft", 
                "Konami Digital Entertainment",
                "THQ", 
                "Nintendo",
                "Sony Computer Entertainment", 
                "Sega",
                "Take-Two Interactive", 
                "Capcom",
                "Atari", 
                "Tecmo Koei",
                "Square Enix", 
                "Warner Bros. Interactive Entertainment",
                "Disney Interactive Studios", 
                "Unknown",
                "Eidos Interactive", 
                "Midway Games",
                "505 Games", 
                "Microsoft Game Studios",
                "Acclaim Entertainment",
                "D3Publisher",
                "Vivendi Games", 
                "Codemasters",
                "Idea Factory", 
                "Deep Silver",
                "Nippon Ichi Software", 
                "Zoo Digital Publishing",
                "Majesco Entertainment", 
                "LucasArts",
                "Rising Star Games", 
                "Hudson Soft",
                "Banpresto", 
                "Bethesda Softworks",
                "Crave Entertainment", 
                "Atlus",
                "Infogrames", 
                "Virgin Interactive",
                "5pb", 
                "Ignition Entertainment",
                "Focus Home Interactive",
                "Desconocidas",
                "Marvelous Interactive", 
                "Empire Interactive",
                "SquareSoft",
                "Kadokawa Shoten",
                "Destineer",
                "DTP Entertainment",
                "GT Interactive", 
                "Alchemist",
                "MTV Games", 
                "Global Star",
                "PQube", 
                "SouthPeak Games",
                "Spike", 
                "Takara Tomy",
                "3DO", 
                "TDK Mediactive",
                "BAM! Entertainment", 
                "Nordic Games",
                "Black Bean Games", 
                "Zoo Games",
                "Game Factory", 
                "Mindscape",
                "Psygnosis", 
                "Enix Corporation",
                "Interplay", 
                "Activision Value",
                "Kalypso Media", 
                "FuRyu",
                "Level 5", 
                "Prototype",
                "Arc System Works", 
                "Avanquest",
                "Little Orbit", 
                "Telltale Games",
                "Midas Interactive Entertainment", 
                "Aqua Plus",
                "Jaleco", 
                "Paradox Interactive",
                "Universal Interactive", 
                "Broccoli",
                "JoWood Productions", 
                "Oxygen Interactive",
                "SNK", 
                "Kemco",
                "ASCII Entertainment",
                "Compile Heart",
                "City Interactive",
                "Storm City Games",
                "Success",
                "Taito",
                "Titus",
                "ChunSoft",
                "SNK Playmore",
                "Tomy Corporation",
                "Zushi Games", 
                "DreamCatcher Interactive",
                ("Otros"))

publicaciones_de_las_empresas <- publicaciones_de_las_empresas %>%
  mutate(Empresas = Publishers)

### Porcetaje de juegos publicados por cada empresa ###

total_publishers <- sum(publicaciones_de_las_empresas$Lanzamientos)

porcentaje_publishers  <- publicaciones_de_las_empresas %>% 
  mutate(porcentajes = Lanzamientos / total_publishers * 100) %>% 
  select(Empresas, Lanzamientos, porcentajes) %>%
  arrange(desc(porcentajes))

### Top 15 empresas de videojuegos ###

porcentaje_top15 <- porcentaje_publishers %>%
  slice_max(order_by = porcentajes, n = 15) %>%
  arrange(desc(porcentajes))

grafico_publishers_videojuegos <- ggplot(data = porcentaje_top15,
                                         aes(x = porcentajes,
                                             y = reorder(Empresas,
                                                         porcentajes))) +
  geom_bar(stat = "identity",
           fill = "#A1D99B",
           width = 0.7) +
  geom_text(aes(label = paste0(round(porcentajes, 1), "%")), 
            hjust = -0.1, size = 3.5, color = "black") +  
  labs(title = "Top 15 Publishers por Porcentaje de Juegos Publicados",
       x = "Porcentaje",
       y = "Empresa") +
  theme_classic() +
  theme(axis.text.y = element_text(size = 10),  
        plot.title = element_text(hjust = 0.5, size = 14, face = "bold")) + 
  scale_x_continuous(limits = c(0, max(porcentaje_top15$porcentajes) * 1.1))  

print(grafico_publishers_videojuegos)
