library(tidyverse)
library(sf)

# 0

list.files('data',recursive = T, full.names = T) [
  list.files('data',recursive = T, full.names = T) %>% str_ends('.shp')]

subte_estaciones <- st_read('data/subte-estaciones/estaciones-de-subte.shp')
subte_lineas <- st_read('data/subte-lineas/lineas-subte.shp')
barrios <- st_read('data/barrios/barrios_badata_wgs84.shp')
ferias <- st_read("data/ferias/ferias.shp" )

for(i in ls()){
  print(st_crs(get(i))[[1]])
}

# 1

ggplot()+
  geom_sf(data = barrios)+
  geom_sf(data = subte_estaciones, aes(col = LINEA))+
  geom_sf(data = subte_lineas)+
  geom_sf(data = ferias)+
  theme_bw()



# 2

barrios_est <- barrios %>% 
  st_join(subte_estaciones) %>% 
  mutate(tiene_estacion = !is.na(ESTACION)) %>% 
  group_by(BARRIO) %>% 
  summarise(estaciones = sum(tiene_estacion,na.rm = T))

ggplot()+
  geom_sf(data = barrios_est, aes(fill = estaciones)) + 
  theme_bw()+
  scale_fill_viridis_c()


# 3
buffers_lineas <-  subte_lineas %>% 
  group_by(LINEASUB) %>% 
  summarise() %>% 
  st_buffer(dist = 50)

buffers_ferias <- ferias %>% 
  st_buffer(dist = 500)


intersects <- buffers_ferias %>% 
  st_intersects(buffers_lineas,sparse = F) %>%
  rowSums()

buffers_ferias$intersects <- if_else(intersects >0,1,0)  


g <- ggplot()+
  geom_sf(data = barrios)+
  geom_sf(data = buffers_lineas, fill = 'orange')+
  geom_sf(data = buffers_ferias, aes(fill = as.factor(intersects)), alpha = .2)+
  theme_bw()+
  scale_fill_manual(values = c('yellow', '#2D882D'))+
  labs(fill = 'Cerca del Subte', title = 'Ferias de la ciudad y líneas de Subte')


# 4


intersects <- subte_estaciones %>% 
  st_intersects(buffers_ferias,sparse = F) %>%
  rowSums()


estaciones_intersections <- subte_estaciones %>% 
  filter(as.logical(intersects))

g+
  geom_sf(data = estaciones_intersections, col = 'red')
