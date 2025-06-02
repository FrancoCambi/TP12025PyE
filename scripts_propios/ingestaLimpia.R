# install.packages("readxl")
library(readxl)

Datos_LP <- read_excel("Datos_LP.xlsx", 
                       skip = 2)

columnas_a_conservar = c(2, 14, 26, 28, 32, 92:95,5)

Datos_LP_limpios <- Datos_LP[, columnas_a_conservar]

names(Datos_LP_limpios)[names(Datos_LP_limpios) == "...2"] <- "Provincia"
names(Datos_LP_limpios)[names(Datos_LP_limpios) == "¿Cuál es el número MÁXIMO de personas que duermen en estos dormitorios usualmente?"] <- "Cant MAX Personas en un dormitorio"
names(Datos_LP_limpios)[names(Datos_LP_limpios) == "¿Cómo es la presión del agua?"] <- "Presion del Agua"
names(Datos_LP_limpios)[names(Datos_LP_limpios) == "¿Cuántos litros de almacenamiento de agua posee?"] <- "Litros de agua almacenada"
names(Datos_LP_limpios)[names(Datos_LP_limpios) == "¿Su baño/letrina posee descarga de agua?"] <- "Posee descarga de agua del baño"
names(Datos_LP_limpios)[names(Datos_LP_limpios) == "¿Hay plagas (cucarachas, mosquitos, ratas, etc) en su vivienda y en los alrededores de la misma?"] <- "Hay plagas en su vivienda y alrededores"
names(Datos_LP_limpios)[names(Datos_LP_limpios) == "¿Cuáles plagas?"] <- "Cucarachas"
names(Datos_LP_limpios)[names(Datos_LP_limpios) == "...94"] <- "Mosquitos"
names(Datos_LP_limpios)[names(Datos_LP_limpios) == "...95"] <- "Ratas"
names(Datos_LP_limpios)[names(Datos_LP_limpios) == "Tiempo de residencia en la vivienda actual (en años)"] <- "Tiempo en vivienda"

Datos_LP_limpios$`Cuales plagas` <- apply(Datos_LP_limpios[, c("Cucarachas", "Mosquitos", "Ratas")], 1, function(x) {
  plagas <- na.omit(x)
  if (length(plagas) == 0) {
    return(NA)
  } else {
    return(paste(plagas, collapse = ", "))
  }
})

Datos_LP_limpios <- Datos_LP_limpios |> dplyr::mutate(across(where(is.character), as.factor))

   rm(columnas_a_conservar)
   