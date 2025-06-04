# Instalo los paquetes necesarios (si aún no los tengo instalados)
# install.packages("googledrive")
# install.packages("readxl")

# Descargo el archivo mediante su id de google drive
# El link de los archivos de drive tiene esta forma:
# https://docs.google.com/spreadsheets/d/1IRhvzOQkvuspQF3TAsBCI-68i8ya0_hy
# El id de esta hoja de cálculo es "1IRhvzOQkvuspQF3TAsBCI-68i8ya0_hy"
googledrive::drive_download(as_id("1IRhvzOQkvuspQF3TAsBCI-68i8ya0_hy"), 
                            overwrite = T)

# Cargo el archivo como .xlsx
datos <- readxl::read_excel("Datos_LP.xlsx", 
                            col_names = FALSE, 
                            skip = 3)

# Veo la estructura del dataset
str(datos)
