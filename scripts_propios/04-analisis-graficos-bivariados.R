# Instalo los paquetes necesarios
install.packages("readxl")
install.packages("tidyverse")
install.packages("ggplot2")
install.packages("janitor")
install.packages("scales")
install.packages("forcats")

# Cargo los paquetes que voy a usar
library(readxl)
library(tidyverse)
library(ggplot2)
library(janitor)
library(scales)
library(forcats)

# Calcular porcentajes por grupo de presión
df_porcentajes <- datos_limpios |>
  filter(!is.na(calidad_presion_del_agua) & !is.na(hay_plagas_en_su_vivienda_y_alrededores)) |>
  group_by(calidad_presion_del_agua, hay_plagas_en_su_vivienda_y_alrededores) |>
  summarise(Frecuencia = n(), .groups = "drop") |>
  group_by(calidad_presion_del_agua) |>
  mutate(Porcentaje = Frecuencia / sum(Frecuencia) * 100)

# Gráfico de barras agrupadas porcentuales
ggplot(df_porcentajes, aes(x = calidad_presion_del_agua, y = Porcentaje, fill = hay_plagas_en_su_vivienda_y_alrededores)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(
    title = "Porcentaje de viviendas con plagas según presión del agua",
    x = "Presión del Agua",
    y = "Porcentaje (%)",
    fill = "¿Hay plagas?"
  ) +
  theme_minimal()
table(df_porcentajes$Porcentaje)

# Boxplot de personas por dormitorio según tipo de baño
# Primero, creamos un dataframe con los datos necesarios
df_banio_personas <- datos_limpios |>
  select(cantidad_MAX_de_personas_en_un_dormitorio, posee_descarga_de_agua_del_baño) |>
  filter(!is.na(posee_descarga_de_agua_del_baño) & !is.na(cantidad_MAX_de_personas_en_un_dormitorio))

# Calcular límites para excluir valores extremos (usando un método menos estricto)
limites <- df_banio_personas |>
  group_by(posee_descarga_de_agua_del_baño) |>
  summarise(
    Q1 = quantile(cantidad_MAX_de_personas_en_un_dormitorio, 0.25),
    Q3 = quantile(cantidad_MAX_de_personas_en_un_dormitorio, 0.75),
    IQR = Q3 - Q1,
    Limite_Inferior = Q1 - 0.75 * IQR,  # Reducido de 1.5 a 0.75
    Limite_Superior = Q3 + 0.75 * IQR   # Reducido de 1.5 a 0.75
  )

# Filtrar valores extremos
df_banio_personas_filtrado <- df_banio_personas |>
  left_join(limites, by = "Posee descarga de agua del baño") |>
  filter(
    cantidad_MAX_de_personas_en_un_dormitorio >= Limite_Inferior,
    cantidad_MAX_de_personas_en_un_dormitorio <= Limite_Superior
  )

# Crear el boxplot
ggplot(df_banio_personas_filtrado, aes(x = posee_descarga_de_agua_del_baño, y = cantidad_MAX_de_personas_en_un_dormitorio, fill = posee_descarga_de_agua_del_baño)) +
  geom_boxplot(alpha = 0.7, outlier.shape = NA) +
  labs(
    title = "Distribución de personas por dormitorio según descarga de agua del baño\n(sin valores extremos)",
    x = NULL,  # Quitamos la etiqueta del eje X
    y = "Cantidad máxima de personas por dormitorio",
    fill = "Descarga de agua del baño"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.text.x = element_blank(),  # Quitamos las etiquetas del eje X
    axis.ticks.x = element_blank()  # Quitamos las marcas del eje X
  ) +
  scale_fill_brewer(palette = "Set2") 
  
#-----------------

#################################################
# Barras: Presion de agua y presencia de plagas #
#################################################
datos_limpios %>%
  filter(!is.na(calidad_presion_del_agua),
         !is.na(hay_plagas_en_su_vivienda_y_alrededores)) %>%
  ggplot(aes(x = calidad_presion_del_agua,
             fill = hay_plagas_en_su_vivienda_y_alrededores)) +
  
  geom_bar(position = "fill", color = "black") +  # fill => porcentual
  
  scale_y_continuous(labels = percent_format(accuracy = 1),
                     expand = expansion(mult = c(0.01, 0.05))) +
  
  scale_fill_brewer(palette = "Paired") +
  
  labs(x = "Calidad de la presión del agua",
       y = "Porcentaje de viviendas",
       fill = "¿Hay plagas?",
       title = "Distribución porcentual de viviendas con/sin plagas según presión del agua") +
  
  theme_classic(base_size = 12) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
    plot.title.position = "plot",
    plot.margin = margin(t = 20, r = 20, b = 10, l = 20),
    axis.title.y = element_text(margin = margin(r = 20)),
    panel.grid.major.y = element_line(color = "gray90", linetype = "solid"),
    axis.title = element_text(size = 14)
  )

##################################################
# Barras: Tipo de descarga y presencia de plagas #
##################################################
datos_limpios %>%
  filter(!is.na(posee_descarga_de_agua_del_baño),
         !is.na(hay_plagas_en_su_vivienda_y_alrededores)) %>%
  mutate(posee_descarga_de_agua_del_baño = fct_infreq(posee_descarga_de_agua_del_baño)) %>%  # Reordenar por frecuencia
  
  ggplot(aes(x = posee_descarga_de_agua_del_baño,
             fill = hay_plagas_en_su_vivienda_y_alrededores)) +
  
  geom_bar(position = "fill", color = "black") +
  
  scale_y_continuous(labels = percent_format(accuracy = 1),
                     expand = expansion(mult = c(0.01, 0.05))) +
  
  scale_fill_brewer(palette = "Paired") +
  
  labs(x = "",
       y = "Porcentaje de viviendas",
       fill = "¿Hay plagas?",
       title = "Distribución porcentual de viviendas con/sin plagas según el tipo de descarga del baño") +
  
  theme_classic(base_size = 12) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
    plot.title.position = "plot",
    plot.margin = margin(t = 20, r = 20, b = 10, l = 20),
    axis.title.y = element_text(margin = margin(r = 20)),
    panel.grid.major.y = element_line(color = "gray90", linetype = "solid"),
    axis.title = element_text(size = 14)
  )

########################################################
# Bastones: Tipo de descarga y Personas por dormitorio #
########################################################
datos_limpios %>%
  filter(!is.na(posee_descarga_de_agua_del_baño),
         !is.na(cantidad_MAX_de_personas_en_un_dormitorio)) %>%
  mutate(posee_descarga_de_agua_del_baño = fct_infreq(posee_descarga_de_agua_del_baño)) %>%
  group_by(cantidad_MAX_de_personas_en_un_dormitorio, posee_descarga_de_agua_del_baño) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(cantidad_MAX_de_personas_en_un_dormitorio) %>%
  mutate(prop = n / sum(n)) %>%
  ungroup() %>%
  
  ggplot(aes(x = factor(cantidad_MAX_de_personas_en_un_dormitorio),
             y = prop,
             fill = posee_descarga_de_agua_del_baño)) +
  
  geom_bar(stat = "identity", position = "fill", color = "black", alpha = 0.7, width = 0.30) +
  
  scale_y_continuous(labels = percent_format(accuracy = 1),
                     breaks = seq(0, 1, 0.1),
                     expand = expansion(mult = c(0.01, 0.05))) +
  
  scale_fill_brewer(palette = "Dark2") +
  
  labs(x = "Máxima cantidad de personas en un mismo dormitorio",
       y = "Porcentaje dentro del grupo",
       fill = "¿Posee descarga del baño?",
       title = "Composición porcentual por cantidad de personas en un mismo dormitorio según el tipo de descarga") +
  
  theme_classic(base_size = 12) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
    plot.title.position = "plot",
    plot.margin = margin(t = 20, r = 20, b = 10, l = 20),
    axis.title.y = element_text(margin = margin(r = 20)),
    panel.grid.major.y = element_line(color = "gray90"),
    axis.title = element_text(size = 14)
  )

###########################################################
# Bastones: Personas por dormitorio y presencia de plagas #
###########################################################
datos_limpios %>%
  filter(!is.na(hay_plagas_en_su_vivienda_y_alrededores),
         !is.na(cantidad_MAX_de_personas_en_un_dormitorio)) %>%
  mutate(hay_plagas_en_su_vivienda_y_alrededores = fct_infreq(hay_plagas_en_su_vivienda_y_alrededores)) %>%
  group_by(cantidad_MAX_de_personas_en_un_dormitorio, hay_plagas_en_su_vivienda_y_alrededores) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(cantidad_MAX_de_personas_en_un_dormitorio) %>%
  mutate(prop = n / sum(n)) %>%
  ungroup() %>%
  
  ggplot(aes(x = factor(cantidad_MAX_de_personas_en_un_dormitorio),
             y = prop,
             fill = hay_plagas_en_su_vivienda_y_alrededores)) +
  
  geom_bar(stat = "identity", position = "fill", color = "black", alpha = 0.7, width = 0.30) +
  
  scale_y_continuous(labels = percent_format(accuracy = 1),
                     breaks = seq(0, 1, 0.1),
                     expand = expansion(mult = c(0.01, 0.05))) +
  
  scale_fill_brewer(palette = "Paired") +
  
  labs(x = "Máxima cantidad de personas en un mismo dormitorio",
       y = "Porcentaje dentro del grupo",
       fill = "¿Hay plagas?",
       title = "Composición porcentual de cantidad de personas en un mismo dormitorio según presencia de plagas") +
  
  theme_classic(base_size = 12) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 15),
    plot.title.position = "plot",
    plot.margin = margin(t = 20, r = 20, b = 10, l = 20),
    axis.title.y = element_text(margin = margin(r = 20)),
    panel.grid.major.y = element_line(color = "gray90"),
    axis.title = element_text(size = 14)
  )

##############################################################
# Dispersión: Personas por dormitorio y Tiempo de residencia #
##############################################################
# Filtrar valores atípicos
datos_filtrados <- datos_limpios %>%
  filter(tiempo_en_vivienda <= 71)
ggplot(datos_filtrados, aes(x = factor(cantidad_MAX_de_personas_en_un_dormitorio),
                            y = tiempo_en_vivienda)) +
  stat_summary(fun = mean, geom = "point", color = "red", size = 3, shape = 18) +
  
  geom_jitter(width = 0.2,
              height = 0,
              alpha = 0.2,
              color = "steelblue") +
  
  scale_x_discrete(
    name = "Máxima cantidad de personas en un dormitorio"
  ) +
  
  scale_y_continuous(
    name = "Tiempo de residencia (años)",
    breaks = seq(0, 75, by = 5),
    expand = expansion(mult = c(0.01, 0.05))
  ) +
  
  labs(
    title = "Distribución del tiempo de residencia según cantidad de personas por dormitorio",
    caption = "Nota: se excluyó 1 valor atípico (5 personas y 122 años de residencia)"
  ) +
  
  theme_classic(base_size = 12) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
    axis.title = element_text(size = 13),
    plot.caption = element_text(size = 9, hjust = 0),
    plot.margin = margin(t = 20, r = 20, b = 10, l = 20)
  )


#############################################
# HeatMap: Provincias y presencia de plagas #
#############################################
install.packages("sf")
install.packages("geodata")

library(sf)
library(geodata)

# Obtener mapa de provincias argentinas
mapa_provincias <- geodata::gadm(country = "ARG", level = 1, path = tempdir())

datos_provincias <- datos_limpios |>
  filter(!is.na(hay_plagas_en_su_vivienda_y_alrededores)) |>
  group_by(provincia) |>
  summarise(porcentaje_plagas = mean(hay_plagas_en_su_vivienda_y_alrededores == "Sí") * 100)

# Convertir factores a character y corregir nombres
datos_provincias <- datos_provincias %>%
  mutate(provincia = as.character(provincia)) %>%
  mutate(provincia = recode(provincia,
                            "CABA" = "Ciudad de Buenos Aires",
                            "Santiago" = "Santiago del Estero",
                            .default = provincia
  ))

# Asegurate de que tu mapa ya esté en formato sf
mapa_provincias_sf <- sf::st_as_sf(mapa_provincias)

# Join
mapa_datos <- mapa_provincias_sf %>%
  left_join(datos_provincias, by = c("NAME_1" = "provincia"))

ggplot(mapa_datos) +
  geom_sf(aes(fill = porcentaje_plagas), color = "white", size = 0.2) +
  scale_fill_viridis_c(option = "magma", direction = -1, na.value = "grey90") +
  labs(
    title = "Porcentaje de viviendas con plagas por provincia",
    fill = "% con plagas"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.text = element_blank(),
    axis.ticks = element_blank()
  )
