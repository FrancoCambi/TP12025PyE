# Instalo los paquetes necesarios
install.packages("readxl")
install.packages("tidyverse")
install.packages("ggplot2")
install.packages("janitor")

# Cargo los paquetes que voy a usar
library(readxl)
library(tidyverse)
library(ggplot2)
library(janitor)


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
Datos_LP_limpios$Provincia <- as.factor(Datos_LP_limpios$Provincia)

# Función para crear un gráfico de barras de presencia de una plaga por provincia
crear_grafico_plaga_provincia <- function(plaga_columna, nombre_plaga) {
  ggplot(Datos_LP_limpios, aes(x = Provincia, fill = .data[[plaga_columna]])) +
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
total_viviendas <- nrow(Datos_LP_limpios)
frecuencias_plagas <- tibble(
  Tipo_Plaga = c("Cucarachas", "Mosquitos", "Ratas"),
  Frecuencia = c(
    sum(Datos_LP_limpios$Cucarachas == "Cucarachas", na.rm = TRUE),
    sum(Datos_LP_limpios$Mosquitos == "Mosquitos", na.rm = TRUE),
    sum(Datos_LP_limpios$Ratas == "Ratas", na.rm = TRUE)
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
  datos$`Cant MAX Personas en un dormitorio` <- 
    as.factor(datos$`Cant MAX Personas en un dormitorio`)
  
  # Calcular porcentajes
  datos_porcentual <- datos %>%
    group_by(`Cant MAX Personas en un dormitorio`) %>%
    summarise(
      Cantidad = n(),
      Porcentaje = (Cantidad / nrow(datos)) * 100
    )
  
  # Crear el gráfico de bastones
  ggplot(datos_porcentual, aes(x = `Cant MAX Personas en un dormitorio`, y = Porcentaje)) +
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
grafico_personas_dormitorio <- crear_grafico_personas_dormitorio(Datos_LP_limpios)
print(grafico_personas_dormitorio)

# Calcular estadísticas (limitando a 60 años)
estadisticas <- Datos_LP_limpios %>%
  filter(`Tiempo en vivienda` <= 60) %>%
  summarise(
    media = mean(`Tiempo en vivienda`, na.rm = TRUE),
    mediana = median(`Tiempo en vivienda`, na.rm = TRUE),
    q1 = quantile(`Tiempo en vivienda`, 0.25, na.rm = TRUE),
    q3 = quantile(`Tiempo en vivienda`, 0.75, na.rm = TRUE),
    sd = sd(`Tiempo en vivienda`, na.rm = TRUE),
    min = min(`Tiempo en vivienda`, na.rm = TRUE),
    max = max(`Tiempo en vivienda`, na.rm = TRUE)
  )

print(estadisticas)

# Histograma con líneas descriptivas (limitando a 60 años)
ggplot(Datos_LP_limpios %>% filter(`Tiempo en vivienda` <= 60), 
       aes(x = `Tiempo en vivienda`)) +
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
df_presion <- Datos_LP_limpios %>%
  filter(!is.na(`Presion del Agua`)) %>%
  count(`Presion del Agua`) %>%
  mutate(Porcentaje = n / sum(n) * 100) %>%
  arrange(Porcentaje)  # Ordenar por porcentaje ascendente

# Graficar
ggplot(df_presion, aes(x = reorder(`Presion del Agua`, Porcentaje), y = Porcentaje)) +
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




