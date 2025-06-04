# Instalo los paquetes necesarios (si aún no los tengo instalados)
# install.packages("tidyverse")

# Cargo los paquetes que voy a usar
library(tidyverse)

# Fijo el dataset
attach(datos)

#######################
# Selección de campos #
#######################

# Selecciono columnas por posición
datos_limpios <- datos[, c(2, 5, 14, 26, 28, 32, 92:95)]

###########################
# Renombramiento de campos#
###########################

colnames(datos_limpios) <- c("provincia",
                             "tiempo_en_vivienda",
                             "cantidad_MAX_de_personas_en_un_dormitorio",
                             "calidad_presion_del_agua",
                             "cantida_litros_de_agua_almacenada",
                             "posee_descarga_de_agua_del_baño",
                             "hay_plagas_en_su_vivienda_y_alrededores",
                             "cucarachas",
                             "mosquitos",
                             "ratas")

########################
# Modificación de datos#
########################

datos_limpios <- datos_limpios %>%
  
  # Creo nueva variable uniendo plagas presentes
  mutate(
    cuales_plagas = apply(select(., cucarachas, mosquitos, ratas), 1, function(x) {
      plagas <- na.omit(x)
      if (length(plagas) == 0) {
        return(NA)
      } else {
        return(paste(plagas, collapse = ", "))
      }
    })
  ) %>%
  
  # Transformo variables de texto en factores
  mutate(across(where(is.character), as.factor)) 
  
# Veo estructura del dataset limpio
str(datos_limpios)
