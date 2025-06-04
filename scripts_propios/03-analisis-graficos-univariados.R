# Instalo los paquetes necesarios
install.packages("readxl")
install.packages("tidyverse")
install.packages("ggplot2")
install.packages("janitor")
install.packages("scales")
install.packages("stringr")

# Cargo los paquetes que voy a usar
library(readxl)
library(tidyverse)
library(ggplot2)
library(janitor)
library(scales)
library(stringr)

#GRAFICOS PROPIOS SCRIPTS
# Gráfico de pie chart para plagas
# Crear una tabla de frecuencias
tabla_plagas_pie <- table(Datos_LP_limpios$`Hay plagas en su vivienda y alrededores`)

# Convertir la tabla en un dataframe para ggplot2
df_plagas_pie <- as.data.frame(tabla_plagas_pie)
colnames(df_plagas_pie) <- c("Plagas", "Frecuencia")

# Calcular los porcentajes
df_plagas_pie$Porcentaje <- df_plagas_pie$Frecuencia / sum(df_plagas_pie$Frecuencia)

# Crear el pie chart
ggplot(df_plagas_pie, aes(x = "", y = Porcentaje, fill = Plagas)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +
  geom_text(aes(label = paste0(round(Porcentaje * 100), "%")), 
            position = position_stack(vjust = 0.5), 
            size = 4, 
            color = "black") +
  labs(title = "¿Presencia de plagas en la vivienda y alrededores?", fill = "Plagas") +
  theme_void() + # Eliminar elementos del tema por defecto
  theme(legend.position = "bottom") # Colocar la leyenda abajo

# Pie chart para descarga de agua del baño
# Crear una tabla de frecuencias
tabla_descarga_pie <- table(Datos_LP_limpios$`Posee descarga de agua del baño`)

# Convertir la tabla en un dataframe para ggplot2
df_descarga_pie <- as.data.frame(tabla_descarga_pie)
colnames(df_descarga_pie) <- c("Descarga", "Frecuencia")

# Calcular los porcentajes
df_descarga_pie$Porcentaje <- df_descarga_pie$Frecuencia / sum(df_descarga_pie$Frecuencia)

# Crear el pie chart
ggplot(df_descarga_pie, aes(x = "", y = Porcentaje, fill = Descarga)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +
  geom_text(aes(label = paste0(round(Porcentaje * 100), "%")), 
            position = position_stack(vjust = 0.5), 
            size = 4, 
            color = "black") +
  labs(title = "¿Posee descarga de agua del baño?", fill = "Descarga") +
  theme_void() + # Eliminar elementos del tema por defecto
  theme(legend.position = "bottom") # Colocar la leyenda abajo

# Asegurémonos de que la columna Provincia sea factor
datos_limpios$provincia <- as.factor(datos_limpios$provincia)

# Función para crear un gráfico de barras de presencia de una plaga por provincia
crear_grafico_plaga_provincia <- function(plaga_columna, nombre_plaga) {
  ggplot(datos_limpios, aes(x = Provincia, fill = .data[[plaga_columna]])) +
    geom_bar(position = "dodge") +
    labs(
      title = paste("Presencia de", nombre_plaga, "por Provincia"),
      x = "Provincia",
      y = "Número de Viviendas",
      fill = "¿Presente?"
    ) +
    scale_fill_discrete(na.value = "lightgray", labels = c("Sí", "No")) + # Asumimos NA es No
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
}

# Crear gráficos para cada tipo de plaga
grafico_cucarachas <- crear_grafico_plaga_provincia("Cucarachas", "Cucarachas")
grafico_mosquitos <- crear_grafico_plaga_provincia("Mosquitos", "Mosquitos")
grafico_ratas <- crear_grafico_plaga_provincia("Ratas", "Ratas")



# Calcular frecuencias y porcentajes para cada tipo de plaga
total_viviendas <- nrow(datos_limpios)
frecuencias_plagas <- tibble(
  Tipo_Plaga = c("Cucarachas", "Mosquitos", "Ratas"),
  Frecuencia = c(
    sum(datos_limpios$cucarachas == "Cucarachas", na.rm = TRUE),
    sum(datos_limpios$mosquitos == "Mosquitos", na.rm = TRUE),
    sum(datos_limpios$ratas == "Ratas", na.rm = TRUE)
  )
) %>%
  mutate(
    Porcentaje = (Frecuencia / total_viviendas) * 100
  ) %>%
  arrange(Porcentaje)

# Crear y mostrar el gráfico de porcentajes
grafico_porcentajes_plagas <- ggplot(frecuencias_plagas, aes(x = reorder(Tipo_Plaga, Porcentaje), y = Porcentaje, fill = Tipo_Plaga)) +
  geom_bar(stat = "identity", width = 0.6) +
  labs(
    title = "Porcentaje de viviendas con presencia de plagas",
    x = "Tipo de Plaga",
    y = "Porcentaje de Viviendas (%)",
    fill = "Tipo de Plaga"
  ) +
  theme_minimal() +
  theme(legend.position = "none") +
  geom_text(aes(label = sprintf("%.1f%%", Porcentaje)), vjust = -0.5, size = 5) +
  scale_fill_manual(values = c(
    "Cucarachas" = "#FFA07A",
    "Mosquitos" = "#20B2AA",
    "Ratas" = "#778899"
  )) +
  scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, 25))

# Imprimir el gráfico
print(grafico_porcentajes_plagas)

# Función para crear gráfico de bastones de personas por dormitorio
crear_grafico_personas_dormitorio <- function(datos) {
  # Convertir la variable a factor y asegurar que esté ordenada
  datos$cantidad_MAX_de_personas_en_un_dormitorio <- 
    as.factor(datos$cantidad_MAX_de_personas_en_un_dormitorio)
  
  # Calcular porcentajes
  datos_porcentual <- datos %>%
    group_by(cantidad_MAX_de_personas_en_un_dormitorio) %>%
    summarise(
      Cantidad = n(),
      Porcentaje = (Cantidad / nrow(datos)) * 100
    )
  
  # Crear el gráfico de bastones
  ggplot(datos_porcentual, aes(x = cantidad_MAX_de_personas_en_un_dormitorio, y = Porcentaje)) +
    geom_bar(stat = "identity", fill = "steelblue", color = "black") +
    labs(
      title = "Frecuencia del Número de Personas por Dormitorio",
      x = "Número de Personas por Dormitorio",
      y = "Porcentaje de Viviendas (%)"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold"),
      axis.text.x = element_text(angle = 45, hjust = 1)
    ) +
    geom_text(aes(label = sprintf("%.1f%%", Porcentaje)), vjust = -0.5, size = 4) +
    scale_y_continuous(limits = c(0, max(datos_porcentual$Porcentaje) * 1.1))
}

# Mostrar el gráfico
grafico_personas_dormitorio <- crear_grafico_personas_dormitorio(datos_limpios)
print(grafico_personas_dormitorio)

# Calcular estadísticas (limitando a 60 años)
estadisticas <- datos_limpios %>%
  filter(tiempo_en_vivienda <= 60) %>%
  summarise(
    media = mean(tiempo_en_vivienda, na.rm = TRUE),
    mediana = median(tiempo_en_vivienda, na.rm = TRUE),
    q1 = quantile(tiempo_en_vivienda, 0.25, na.rm = TRUE),
    q3 = quantile(tiempo_en_vivienda, 0.75, na.rm = TRUE),
    sd = sd(tiempo_en_vivienda, na.rm = TRUE),
    min = min(tiempo_en_vivienda, na.rm = TRUE),
    max = max(tiempo_en_vivienda, na.rm = TRUE)
  )

print(estadisticas)

# Histograma con líneas descriptivas (limitando a 60 años)
ggplot(datos_limpios %>% filter(tiempo_en_vivienda <= 60), 
       aes(x = tiempo_en_vivienda)) +
  geom_histogram(binwidth = 2, fill = "#ADD8E6", color = "black") +
  geom_vline(aes(xintercept = estadisticas$media, color = "Media"), 
             linetype = "dashed", size = 1) +
  geom_vline(aes(xintercept = estadisticas$mediana, color = "Mediana"), 
             linetype = "solid", size = 1) +
 
  labs(
    title = "Distribución del tiempo vivido en el hogar (años)",
    x = "Años en el hogar",
    y = "Cantidad de viviendas",
    color = "Medidas de tendencia central"
  ) +
  scale_color_manual(
    values = c(
      "Media" = "red",
      "Mediana" = "black"
    
    ),
    labels = c(
      "Media" = paste("Media:", round(estadisticas$media, 1), "años"),
      "Mediana" = paste("Mediana:", round(estadisticas$mediana, 1), "años")
      
    )
  ) +
  theme_minimal() +
  scale_x_continuous(limits = c(0, 60), breaks = seq(0, 60, 5)) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "bottom",
    legend.title = element_text(face = "bold")
  )


# Calcular porcentajes
df_presion <- datos_limpios %>%
  filter(!is.na(calidad_presion_del_agua)) %>%
  count(calidad_presion_del_agua) %>%
  mutate(Porcentaje = n / sum(n) * 100) %>%
  arrange(Porcentaje)  # Ordenar por porcentaje ascendente

# Graficar
ggplot(df_presion, aes(x = reorder(calidad_presion_del_agua, Porcentaje), y = Porcentaje)) +
  geom_bar(stat = "identity", fill = "#4682B4") +
  geom_text(aes(label = sprintf("%.1f%%", Porcentaje)), vjust = -0.5) +
  labs(
    title = "Distribución porcentual de la presión del agua",
    x = "Presión del Agua",
    y = "Porcentaje (%)"
  ) +
  theme_minimal() +
  scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, 10)) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(hjust = 0.5, face = "bold")
  )

#------------------

####################
# PieChart: Plagas #
####################
# Crear una tabla de frecuencias
tabla_plagas_pie <- table(datos_limpios$hay_plagas_en_su_vivienda_y_alrededores)

# Convertir la tabla en un dataframe para ggplot2
df_plagas_pie <- as.data.frame(tabla_plagas_pie)
colnames(df_plagas_pie) <- c("Plagas", "Frecuencia")

# Calcular los porcentajes
df_plagas_pie$Porcentaje <- df_plagas_pie$Frecuencia / sum(df_plagas_pie$Frecuencia)

# Crear el pie chart
ggplot(df_plagas_pie, aes(x = "", y = Porcentaje, fill = Plagas)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +
  geom_text(aes(label = paste0(round(Porcentaje * 100), "%")), 
            position = position_stack(vjust = 0.5), 
            size = 8, 
            color = "black") +
  labs(title = "Presencia de plagas en la vivienda y alrededores", fill = "¿Hay plagas?") +
  theme_void() + # Eliminar elementos del tema por defecto
  theme(legend.position = "bottom",
        plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
        plot.title.position = "plot",
        plot.margin = margin(t = 20, r = 20, b = 10, l = 20), # reduce espacio superior
        legend.margin = margin(t = -20, r = 0, b = 10, l = 0)) + 
  scale_fill_brewer(palette = "Paired")


############################
# Barras: Tipo de descarga #
############################
datos_limpios %>%
  drop_na(posee_descarga_de_agua_del_baño) %>% # Eliminar valores NA si los hay
  ggplot() +
  
  aes(x = reorder(posee_descarga_de_agua_del_baño, 
                  posee_descarga_de_agua_del_baño, 
                  function(x) -length(x)), 
      y = ..count.. / sum(..count..)) + # Calcular proporción (porcentaje)
  
  geom_bar(width = 0.75,
           fill = 'steelblue',
           col = "black",
           alpha = 0.6) +
  
  scale_y_continuous(labels = scales::percent, 
                     breaks = seq(0, 1, 0.1),  # Cada 10%
                     expand = expansion(mult = c(0.01, 0.05))) + # Pequeño espacio arriba
  
  labs(y = "Porcentaje de viviendas",
       x = "") +
  
  ggtitle("Distribución porcentual del tipo de descarga de agua del baño") +
  
  theme_classic(base_size = 12) +
  
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16), # Título centrado y bold
    plot.title.position = "plot",
    plot.margin = margin(t = 20, r = 20, b = 10, l = 20),
    axis.title.y = element_text(margin = margin(r = 20)),              # Más margen izquierdo (desde gráfico al label Y)
    panel.grid.major.y = element_line(color = "gray90", linetype = "solid"), # Líneas verticales suaves
    axis.title = element_text(size = 14)                               # Fuente más grande para títulos de ejes
  )


############################
# Barras: Presion del agua #
############################
datos_limpios %>%
  drop_na(calidad_presion_del_agua) %>% # Eliminar valores NA si los hay
  ggplot() +
  
  aes(x = reorder(calidad_presion_del_agua, 
                  calidad_presion_del_agua, 
                  function(x) -length(x)), 
      y = ..count.. / sum(..count..)) + # Calcular proporción (porcentaje)
  
  geom_bar(width = 0.75,
           fill = '#4e79a7',  # Azul grisáceo profesional
           col = "black",
           alpha = 0.6) +
  
  scale_y_continuous(labels = scales::percent, 
                     breaks = seq(0, 1, 0.1),  # Cada 10%
                     expand = expansion(mult = c(0.01, 0.05))) + # Pequeño espacio arriba
  
  labs(y = "Porcentaje de viviendas (%)",
       x = "") +
  
  ggtitle("Distribución porcentual de la calidad de la presión del agua en la vivienda") +
  
  theme_classic(base_size = 12) +
  
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16), # Título centrado y bold
    plot.title.position = "plot",
    plot.margin = margin(t = 20, r = 20, b = 10, l = 20),
    axis.title.y = element_text(margin = margin(r = 20)),              # Más margen izquierdo (desde gráfico al label Y)
    panel.grid.major.y = element_line(color = "gray90", linetype = "solid"), # Líneas verticales suaves
    axis.title = element_text(size = 14)                               # Fuente más grande para títulos de ejes
  )


############################################
# Gráfico de bastones: personas por cuarto #
############################################
datos_limpios %>%
  drop_na(cantidad_MAX_de_personas_en_un_dormitorio) %>%
  ggplot() +
  
  aes(x = cantidad_MAX_de_personas_en_un_dormitorio, 
      y = ..count.. / sum(..count..)) +
  
  geom_bar(stat = "count",
           width = 0.10,  # Bastones delgados
           fill = "steelblue",
           color = "black",
           alpha = 0.6) +
  
  scale_x_continuous(breaks = 1:10) +  # Saltos de 1 en 1 hasta 10
  
  scale_y_continuous(labels = scales::percent,
                     breaks = seq(0, 1, 0.1),
                     expand = expansion(mult = c(0.01, 0.05))) +
  
  labs(y = "Porcentaje de viviendas (%)",
       x = "Cantidad máxima de personas en un mismo dormitorio") +
  
  ggtitle("Distribución porcentual de personas en un mismo dormitorio por vivienda") +
  
  theme_classic(base_size = 12) +
  
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
    plot.title.position = "plot",
    plot.margin = margin(t = 20, r = 20, b = 10, l = 20),
    axis.title.y = element_text(margin = margin(r = 20)),
    panel.grid.major.y = element_line(color = "gray90", linetype = "solid"),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12)
  )

############################################
# Histograma: Años viviendo en la vivienda #
############################################
datos_limpios %>%
  drop_na(tiempo_en_vivienda) %>%
  ggplot() +
  aes(x = tiempo_en_vivienda, y = ..count.. / sum(..count..)) +
  
  geom_histogram(fill = "lightblue", 
                 color = "black",
                 breaks = seq(0, 120, 10)) +
  
  scale_x_continuous(breaks = seq(0, max(datos_limpios$tiempo_en_vivienda, na.rm = TRUE) + 10, 10)) +
  
  scale_y_continuous(labels = scales::percent,
                     breaks = seq(0, 1, 0.1),
                     expand = expansion(mult = c(0.01, 0.05))) +
  
  labs(
    x = "Años viviendo en la vivienda",
    y = "Porcentaje de viviendas (%)"
  ) +
  
  ggtitle("Distribución del tiempo vivido en la vivienda") +
  
  theme_classic(base_size = 12) +
  
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
    plot.title.position = "plot",
    plot.margin = margin(t = 20, r = 20, b = 10, l = 20),
    axis.title.y = element_text(margin = margin(r = 20)),
    panel.grid.major.y = element_line(color = "gray90", linetype = "solid"),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12)
  )

# Calcular número total de viviendas con plagas
total_viviendas_con_plagas <- datos_limpios %>%
  filter(hay_plagas_en_su_vivienda_y_alrededores == "Sí") %>%
  filter(!is.na(cuales_plagas)) %>%
  nrow()

# Crear gráfico con porcentajes sobre total de viviendas con plagas
datos_limpios %>%
  filter(hay_plagas_en_su_vivienda_y_alrededores == "Sí") %>%
  filter(!is.na(cuales_plagas)) %>%
  mutate(cuales_plagas = str_split(cuales_plagas, pattern = ",\\s*")) %>%
  unnest(cuales_plagas) %>%
  mutate(cuales_plagas = str_trim(str_to_title(cuales_plagas))) %>%
  count(cuales_plagas, name = "n") %>%
  mutate(porcentaje = n / total_viviendas_con_plagas) %>%
  ggplot(aes(x = reorder(cuales_plagas, porcentaje), 
             y = porcentaje)) +
  
  geom_bar(stat = "identity", 
           width = 0.7, 
           fill = "lightblue", 
           color = "black", 
           alpha = 0.7) +
  
  coord_flip() +
  
  scale_y_continuous(labels = percent_format(accuracy = 1), 
                     breaks = seq(0, 1.2, 0.1), 
                     expand = expansion(mult = c(0.01, 0.05))) +
  
  labs(y = "Porcentaje de viviendas con dicha plaga", 
       x = "", 
       title = "Porcentaje de viviendas con cada tipo de plaga") +
  
  theme_classic(base_size = 12) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
    plot.title.position = "plot",
    plot.margin = margin(t = 20, r = 20, b = 10, l = 0),
    axis.title.y = element_text(margin = margin(r = 20)),
    panel.grid.major.x = element_line(color = "gray90", linetype = "solid"),
    axis.title = element_text(size = 14)
  )
