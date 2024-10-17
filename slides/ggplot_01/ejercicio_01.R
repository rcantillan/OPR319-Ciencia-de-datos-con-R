


# Actividad: Análisis de la Relación entre Expectativa de Vida y PIB per cápita

# Instalar y cargar los paquetes necesarios
if (!require(WDI)) install.packages("WDI")
if (!require(ggplot2)) install.packages("ggplot2")
if (!require(dplyr)) install.packages("dplyr")

library(WDI)
library(ggplot2)
library(dplyr)

# tarea 1: Obtener los datos
# Descargamos datos de expectativa de vida y PIB per cápita para el año 2020
datos <- WDI(indicator = c("SP.DYN.LE00.IN", "NY.GDP.PCAP.PP.KD"), 
             start = 2020, end = 2020)

# tarea 2: Limpiar y preparar los datos
datos_limpios <- datos %>%
  rename(expectativa_vida = SP.DYN.LE00.IN,
         pib_per_capita = NY.GDP.PCAP.PP.KD) %>%
  filter(!is.na(expectativa_vida) & !is.na(pib_per_capita))

# tarea 3: Crear un gráfico de dispersión básico
ggplot(datos_limpios, aes(x = , y = )) +
  geom_point() +
  labs(title = "Relación entre PIB per cápita y Expectativa de Vida (2020)",
       x = "PIB per cápita (USD)",
       y = "Expectativa de Vida (años)")

# tarea 4: Mejorar el gráfico
ggplot(datos_limpios, aes(x = , y = )) +
  geom_point(aes(color = region), alpha = 0.7) +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  scale_x_log10(labels = scales::dollar_format()) +
  labs(title = "Relación entre PIB per cápita y Expectativa de Vida (2020)",
       subtitle = "Escala logarítmica para PIB per cápita",
       x = "PIB per cápita (USD, escala log)",
       y = "Expectativa de Vida (años)",
       color = "Región") +
  theme_minimal()

# tarea 5: Crear un gráfico de barras para las 10 expectativas de vida más altas
## filtrar puntajes más altos y crear sub tabla (crear)


## plotear
ggplot(top_10_ev, aes(x = reorder(country, expectativa_vida), y = expectativa_vida)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  coord_flip() +
  labs(title = "Top 10 Países con Mayor Expectativa de Vida (2020)",
       x = "País",
       y = "Expectativa de Vida (años)") +
  theme_minimal()

# Desafío extra: Crear un gráfico que muestre la relación entre 
# expectativa de vida y PIB per cápita, pero con el tamaño de los puntos 
# proporcional a la población del país.

# Primero, obtenemos los datos de población
datos_poblacion <- WDI(indicator = c("SP.DYN.LE00.IN", "NY.GDP.PCAP.PP.KD", "SP.POP.TOTL"), 
                       start = 2020, end = 2020)

datos_completos <- datos_poblacion %>%
  rename(expectativa_vida = SP.DYN.LE00.IN,
         pib_per_capita = NY.GDP.PCAP.PP.KD,
         poblacion = SP.POP.TOTL) %>%
  filter(!is.na(expectativa_vida) & !is.na(pib_per_capita) & !is.na(poblacion))

# ploteamos
