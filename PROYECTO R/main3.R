# Instalar paquetes si no están instalados
install.packages(c("tidyverse", "ggplot2", "sf", "viridis"))

# Cargar las librerías
library(tidyverse)
library(ggplot2)
library(sf)
library(viridis)
library(dplyr)

#Leer datos
datos2 = read.csv('./Data_DeportistasBeneficiadosPAD_2.2.csv', fileEncoding = "latin1")


#Verificar si hay datos vacios
colSums(is.na(datos2))

# Determinar el tipo de dato de cada columna
tipos_de_dato <- sapply(datos2, class)
print(tipos_de_dato)



# Eliminar colummnas innecesarias
datos2 <- select(datos2, -FECHA_CORTE, -FECHA_PUBLICACION, -ANIO, -ITEM, -UBIGEO)

# CREAMOS UNA COLUMNA FECHA CON FORMATO DATE USANDO LA INFO DE COLUMNA MES
datos2$FECHA <- as.Date(paste0(datos2$'MES', "01"), format = "%Y%m%d")

# MOVEMOS LA COLUMNA FECHA A LA POSICION DE MES
datos2 <- relocate(datos2, FECHA, .after = 1)

# ELIMINAMOS LA COLUMNA MES QUE YA NO NECESITAMOS
datos2 <- select(datos2, -MES)




#VERIFICAMOS SI HAY DATOS ATIPICOS EN LA FECHA
ggplot(datos2, aes(y = FECHA)) +
  geom_boxplot(fill = "lightblue", color = "darkblue") +
  labs(title = "Boxplot para identificar datos atípicos en las fechas",
       y = "Fechas")

ggplot(datos2, aes(y = MONTO)) +
  geom_boxplot(fill = "lightblue", color = "darkblue") +
  labs(title = "Boxplot para identificar datos atípicos en los montos",
       y = "Monto en soles")
  

ggplot(datos2, aes(x = MONTO)) +
  geom_histogram(binwidth = 5, fill = "black", color = "brown") +
  ggtitle("Distribución de MONTO") +
  xlab("MONTO") +
  ylab("Cantidad de deportistas")




# CONSTRUYENDO LOS MODELOS


#Haciendo un mapa de Lima para saber cual distrito tiene más deportistas beneficiados del programa

# Cargar el shapefile
shp_lima_provincia <- st_read("./Distrital INEI 2023 geogpsperu SuyoPomalia/Distrital INEI 2023 geogpsperu SuyoPomalia.shp")

# Filtrar el shapefile para que solo contenga distritos de Lima
shp_lima_distritos <- shp_lima_provincia %>%
  filter(PROVINCIA == "LIMA")  


# Contar deportistas por distrito en la provincia de Lima
datos_lima <- datos2 %>%
  filter(PROVINCIA == "LIMA") %>%
  count(DISTRITO, name = "num_deportistas")  # Cuenta deportistas por distrito

# Unir la data con el shapefile filtrado de Lima
mapa_lima <- shp_lima_distritos %>%
  left_join(datos_lima, by = c("DISTRITO" = "DISTRITO")) 

# Graficar el mapa con colores según el número de deportistas
ggplot(mapa_lima) +
  geom_sf(aes(fill = num_deportistas), color = "black") +
  scale_fill_viridis_c(option = "Blues",  direction = 1,na.value = "gray80") +  # Escala de colores
  theme_minimal() +
  labs(title = "Diversificación de Deportistas por Distrito en Lima",
       fill = "N° Deportistas") +
  coord_sf(datum = NA)  # Elimina las coordenadas





# 1. Identificar los 5 distritos con más deportistas
top_distritos <- datos2 %>% 
  filter(PROVINCIA == "LIMA") %>%
  group_by(DISTRITO) %>%
  summarise(num_deportistas = n()) %>%
  arrange(desc(num_deportistas)) %>%
  slice_head(n = 5) %>%
  pull(DISTRITO)

# 2. Identificar las 5 federaciones con más deportistas
top_federaciones <- datos2 %>% 
  group_by(FEDERACION) %>%
  summarise(num_deportistas = n()) %>%
  arrange(desc(num_deportistas)) %>%
  slice_head(n = 5) %>%
  pull(FEDERACION)

datos_sum <- datos2 %>%
  mutate(
    DISTRITO = ifelse(DISTRITO %in% top_distritos, DISTRITO, "Otros Distritos"),
    FEDERACION = ifelse(FEDERACION %in% top_federaciones, FEDERACION, "Otros Federaciones")
  ) %>%
  group_by(DISTRITO, FEDERACION) %>%
  summarise(num_deportistas = n()) %>%
  ungroup()

# Graficar cantidad de deportistas por distrito y federación
ggplot(datos_sum, aes(x = DISTRITO, y = num_deportistas, fill = FEDERACION)) +
  geom_col(position = position_dodge()) +
  theme_minimal() +
  labs(title = "Cantidad de Deportistas por Distrito y Federación",
       x = "Distrito",
       y = "Cantidad de Deportistas",
       fill = "Federación") +
  scale_y_continuous(labels = comma) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


print(datos_sum, n = 50)



#Haciendo un grafico por ver cual federacion o deportes tiene más representantes beneficiados del programa


# Contar deportistas por federación
deportistas_por_federacion <- count(datos2, FEDERACION)

# Graficar barras horizontales
ggplot(deportistas_por_federacion, aes(x = FEDERACION, y = n)) +  # 'x' es la federación y 'y' el conteo
  geom_bar(stat = "identity", fill = "skyblue", color = "black") +  # Barras con color
  ggtitle("Número de Deportistas por Federación") +
  theme_minimal() +
  coord_flip()  # Esto voltea las barras para hacerlas horizontales











# UTILITARIOS

#calcular la cantidad de valores unicos en cada columna
valores_df <- data.frame(Columna = names(valores_unicos), Unicos = valores_unicos)
print(valores_df, row.names = FALSE)


print(unique(datos2$PAD))

# EXPORTAR DATA MEJORADA COMO CSV
write.csv(datos2, "dato2s.csv", row.names = FALSE)
