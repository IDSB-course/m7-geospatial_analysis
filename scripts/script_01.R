library(tidyverse)
library(sf)
library(spData)

#armamos geometrías

st_point(c(5,2)) %>%
  plot(axes = TRUE)


st_linestring(rbind(c(1, 5),
                    c(4, 4),
                    c(4, 1),
                    c(2, 2),
                    c(3, 2))) %>%
  plot(axes = TRUE)


st_polygon(list(rbind(c(1, 5),
                      c(2, 2),
                      c(4, 1),
                      c(4, 4),
                      c(1, 5)))) %>% 
  plot(axes = TRUE, col = 'gray')


#juntamos todo

st_geometrycollection(list(st_point(c(5, 2)),
                           st_linestring(rbind(
                             c(1, 5),
                             c(4, 4),
                             c(4, 1),
                             c(2, 2),
                             c(3, 2)
                           )),
                           st_polygon(list(
                             rbind(c(1, 5),
                                   c(2, 2),
                                   c(4, 1),
                                   c(4, 4),
                                   c(1, 5))
                           )))) %>% 
  plot(axes = TRUE)

#importamos comunas

comunas <- st_read('data/comunas/comunas_wgs84.shp') %>% st_transform(5343)
barrios <- st_read('data/barrios/barrios_badata_wgs84.shp') %>% st_transform(5343)
rus <- st_read("data/RUS/relevamiento-usos-suelo-2017.shp") %>% st_transform(5343)

plot(st_geometry(comunas))
plot(st_geometry(barrios))
plot(st_geometry(rus %>% sample_n(50)))

# EDA

glimpse(barrios)
glimpse(comunas)


glimpse(rus)

set.seed(0)
rus_sample_5000 <- rus %>% sample_n(5000)


#plot sample RUS
ggplot()+
  geom_sf(data = rus_sample_5000)


ggplot()+
  geom_sf(data = comunas)+
  geom_sf(data = rus_sample_5000, alpha = .1, col = 'steelblue')

library(ggthemes)

ggplot()+
  geom_sf(data = comunas %>% st_union())+
  geom_sf(data = rus_sample_5000, alpha = .1, col = 'steelblue')+
  theme_bw()



#ferias de la ciudad
ferias <- st_read("data/ferias/ferias.shp") %>% st_transform(5343)

ggplot()+
  geom_sf(data = barrios, fill = 'steelblue')+
  geom_sf(data = ferias, col = 'red')+
  theme_bw()

# Geo Wrangling

world

world_summ <- world %>% 
  group_by(continent) %>% 
  summarise(
    area_km2 = sum(area_km2,na.rm = T),
    pop = sum(pop, na.rm = T),
    lifeExp = mean(lifeExp, na.rm = T),
    gdpPercap = mean(gdpPercap, na.rm = T)
  ) %>% 
  filter(pop >0) %>% 
  mutate(pop_density = pop/area_km2)


ggplot(world_summ)+
  geom_sf(aes(fill = pop))+
  scale_fill_viridis_c()+
  theme_bw()+
  ggtitle('Fill by pop')

ggplot(world_summ)+
  geom_sf(aes(fill = pop_density))+
  scale_fill_viridis_c()+
  theme_bw()+
  ggtitle('Fill by pop density')



# Spatial subsetting

rus$RAMA %>% 
  table() %>% 
  data.frame() %>% 
  arrange(desc(Freq))

rus_H_G <- rus %>% 
  filter(RAMA == 'HOTELERIA Y GASTRONOMIA')

ggplot(rus_H_G) +
  geom_sf(alpha = .05)+
  theme_bw()

palermo_flores <- barrios %>% 
  filter(BARRIO %in% c('PALERMO', 'FLORES'))

ggplot(palermo_flores)+
  geom_sf(fill = 'steelblue', col = 'red', lwd = 1)+
  geom_sf_label(aes(label = BARRIO), size = 3)+
  theme_bw()


rus_H_G_PF <- rus_H_G[palermo_flores,]


ggplot()+
  geom_sf(data = comunas %>% st_union(), fill = 'white')+
  geom_sf(data = palermo_flores)+
  geom_sf(data = rus_H_G_PF, col = 'steelblue', alpha = .1)+
  ggtitle("Gastronomía y hoteles en Palermo y Chacarita") +
  theme_bw()


ggplot()+
  geom_sf(data = comunas %>% st_union(), fill = 'white')+
  geom_sf(data = palermo_flores)+
  geom_sf(data = rus_H_G_PF, col = 'steelblue', alpha = .1)+
  ggtitle("Gastronomía y hoteles en Palermo y Chacarita") +
  theme_bw()


# spatial join



summ_hgpf <- palermo_flores %>% 
  st_join(
    rus %>% filter(RAMA == 'HOTELERIA Y GASTRONOMIA') %>% select(RAMA)
    ) %>% 
  group_by(BARRIO) %>% 
  summarise(gh = n()) %>% 
  mutate(gh_norm = 10000 * gh/st_area(.))


ggplot(summ_hgpf)+
  geom_sf(aes(fill = gh))+
  scale_fill_viridis_c()+
  theme_bw()
  
ggplot(summ_hgpf)+
  geom_sf(aes(fill = as.numeric(gh_norm)))+
  scale_fill_viridis_c()+
  theme_bw()

# centroids

centroides_comunas <-  st_centroid(comunas)

ggplot()+
  geom_sf(data= comunas)+
  geom_sf(data = centroides_comunas, col = 'steelblue')+
  ggtitle('Comunas y sus centroides')+
  theme_bw()

# buffers

buffers_comunas <- st_buffer(centroides_comunas,dist = 1000)

ggplot()+
  geom_sf(data= comunas)+
  geom_sf(data= buffers_comunas, fill = 'yellow', alpha = .3)+
  geom_sf(data = centroides_comunas, col = 'steelblue')+
  ggtitle('Comunas, centroides y buffers de 1km')+
  theme_bw()



# ex

ferias$withinBuffer <- ferias %>% 
  st_within(buffers_comunas,sparse = F) %>% 
  rowSums()

ggplot()+
  geom_sf(data= comunas)+
  geom_sf(data= buffers_comunas, fill = 'yellow', alpha = .3)+
  geom_sf(data = ferias, aes(color = as.factor(withinBuffer))) +
  ggtitle('Comunas, buffers y ferias')+
  theme_bw()
  
