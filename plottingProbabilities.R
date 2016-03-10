# setwd('C:/Users/Brian/Desktop/gits/RUBL')




rInd <- rasterToPoints(indLogisticMean)

df <- data.frame(rInd)

bind_rows(indLogisticMean %>% aggregate(2) %>% rasterToPoints %>% data.frame %>%
            mutate(flockClass = 'Individual'),
          sfLogisticMean %>% aggregate(2) %>% rasterToPoints %>% data.frame %>%
            mutate(flockClass = 'Small'),
          lfLogisticMean %>% aggregate(2) %>% rasterToPoints %>% data.frame %>%
            mutate(flockClass = 'Large')) %>%
  mutate(flockClass = factor(flockClass, levels = c('Individual', 'Small', 'Large')),
         layer = ifelse(layer < 0.01, NA, layer)) %>%
  ggplot(aes(x, y)) + 
  geom_raster(aes(fill = layer)) +
  facet_grid(. ~ flockClass) +
  theme_bw() +
  scale_fill_gradientn(colours=c('navyblue', 'darkblue', 'blue' , 'yellow', 'red', 'darkred'), 
                       na.value = 'gray70')

bind_rows((sfLogisticMean-indLogisticMean) %>% aggregate(2) %>% rasterToPoints %>% data.frame %>%
            mutate(d = 'IndSF'),
          (lfLogisticMean-indLogisticMean) %>% aggregate(2) %>% rasterToPoints %>% data.frame %>%
            mutate(d = 'IndLF')) %>%
  mutate(d = factor(d, levels = c('IndSF', 'IndLF'), 
                    labels = c('Small flocks - individuals)', 'Large flocks - individuals')),
         layer = ifelse(layer < 0.0001 & layer > - 0.0001, NA, layer)) %>%
  ggplot(aes(x, y)) + 
  geom_raster(aes(fill = layer)) +
  facet_grid(. ~ d) +
  theme_bw() + 
  labs(x = 'Latitude', y = 'Longitude') +
  theme(axis.title.y = element_text(vjust = 1.5, size = rel(1.75)),
        axis.title.x = element_text(vjust = -0.25, size = rel(1.75)),
        axis.text = element_text(size = rel(1.25)),
        strip.text = element_text(size = rel(2), face = 'bold')) + 
  scale_fill_gradientn(colours=c('navyblue', 'darkblue', 'blue' ,'blue', 'gray50', 'yellow', 'red', 'darkred'), 
                       na.value = 'gray20')

e <- extent(c(-93,-85,28.49803,37))

bind_rows((crop(sfLogisticMean,e)) %>%
            rasterToPoints %>% data.frame %>%
            mutate(d = 'sf'),
          (crop(lfLogisticMean,e)) %>%
            rasterToPoints %>% data.frame %>%
            mutate(d = 'lf')) %>%
  filter(d!= 'LFSF') %>%
  mutate(d = factor(d, levels = c('sf', 'lf'), labels = c('Small flock', 'Large flock')),
         layer = ifelse(layer < 0.0001 & layer > - 0.0001, NA, layer)) %>%
  ggplot(aes(x, y)) + 
  geom_raster(aes(fill = layer)) +
  labs(x = 'Latitude', y = 'Longitude') +
  facet_grid(. ~ d) +
  theme_bw() + 
  theme(axis.title.y = element_text(vjust = 1.5, size = rel(1.75)),
        axis.title.x = element_text(vjust = -0.25, size = rel(1.75)),
        axis.text = element_text(size = rel(1.25)),
        strip.text = element_text(size = rel(2), face = 'bold')) + 
  scale_fill_gradientn(colours=c('navyblue', 'darkblue', 'blue' ,'blue', 'gray50', 'yellow', 'red', 'darkred'), 
                       na.value = 'gray20')
