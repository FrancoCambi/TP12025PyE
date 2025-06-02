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

# Calcular porcentajes por grupo de presión
df_porcentajes <- Datos_LP_limpios |>
  filter(!is.na(`Presion del Agua`) & !is.na(`Hay plagas en su vivienda y alrededores`)) |>
  group_by(`Presion del Agua`, `Hay plagas en su vivienda y alrededores`) |>
  summarise(Frecuencia = n(), .groups = "drop") |>
  group_by(`Presion del Agua`) |>
  mutate(Porcentaje = Frecuencia / sum(Frecuencia) * 100)

# Gráfico de barras agrupadas porcentuales
ggplot(df_porcentajes, aes(x = `Presion del Agua`, y = Porcentaje, fill = `Hay plagas en su vivienda y alrededores`)) +
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
df_banio_personas <- Datos_LP_limpios |>
  select(`Cant MAX Personas en un dormitorio`, `Posee descarga de agua del baño`) |>
  filter(!is.na(`Posee descarga de agua del baño`) & !is.na(`Cant MAX Personas en un dormitorio`))

# Calcular límites para excluir valores extremos (usando un método menos estricto)
limites <- df_banio_personas |>
  group_by(`Posee descarga de agua del baño`) |>
  summarise(
    Q1 = quantile(`Cant MAX Personas en un dormitorio`, 0.25),
    Q3 = quantile(`Cant MAX Personas en un dormitorio`, 0.75),
    IQR = Q3 - Q1,
    Limite_Inferior = Q1 - 0.75 * IQR,  # Reducido de 1.5 a 0.75
    Limite_Superior = Q3 + 0.75 * IQR   # Reducido de 1.5 a 0.75
  )

# Filtrar valores extremos
df_banio_personas_filtrado <- df_banio_personas |>
  left_join(limites, by = "Posee descarga de agua del baño") |>
  filter(
    `Cant MAX Personas en un dormitorio` >= Limite_Inferior,
    `Cant MAX Personas en un dormitorio` <= Limite_Superior
  )

# Crear el boxplot
ggplot(df_banio_personas_filtrado, aes(x = `Posee descarga de agua del baño`, y = `Cant MAX Personas en un dormitorio`, fill = `Posee descarga de agua del baño`)) +
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
  

