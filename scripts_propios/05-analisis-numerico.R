summary(datos_limpios[, c("tiempo_en_vivienda", "cantidad_MAX_de_personas_en_un_dormitorio", "cantida_litros_de_agua_almacenada")])

# Por variable, más detallado:
library(janitor)

# Medidas para cantidad_MAX_de_personas_en_un_dormitorio
resumen_dormitorio <- datos_limpios %>%
  summarise(
    min_personas = min(cantidad_MAX_de_personas_en_un_dormitorio, na.rm = TRUE),
    q1_personas = quantile(cantidad_MAX_de_personas_en_un_dormitorio, 0.25, na.rm = TRUE),
    mediana_personas = median(cantidad_MAX_de_personas_en_un_dormitorio, na.rm = TRUE),
    promedio_personas = mean(cantidad_MAX_de_personas_en_un_dormitorio, na.rm = TRUE),
    q3_personas = quantile(cantidad_MAX_de_personas_en_un_dormitorio, 0.75, na.rm = TRUE),
    max_personas = max(cantidad_MAX_de_personas_en_un_dormitorio, na.rm = TRUE)
  )

# Medidas para tiempo_en_vivienda
resumen_tiempo <- datos_limpios %>%
  summarise(
    min_tiempo = min(tiempo_en_vivienda, na.rm = TRUE),
    q1_tiempo = quantile(tiempo_en_vivienda, 0.25, na.rm = TRUE),
    mediana_tiempo = median(tiempo_en_vivienda, na.rm = TRUE),
    promedio_tiempo = mean(tiempo_en_vivienda, na.rm = TRUE),
    q3_tiempo = quantile(tiempo_en_vivienda, 0.75, na.rm = TRUE),
    max_tiempo = max(tiempo_en_vivienda, na.rm = TRUE),
    desvio_tiempo = sd(tiempo_en_vivienda, na.rm = TRUE)
  )

porcentaje_plagas <- datos_limpios %>%
  group_by(as.character(provincia)) %>%
  summarise(
    total = n(),
    con_plagas = sum(as.character(hay_plagas_en_su_vivienda_y_alrededores) == "Sí", na.rm = TRUE),
    porcentaje_plagas = round(100 * con_plagas / total, 1)
  ) %>%
  arrange(desc(porcentaje_plagas))
