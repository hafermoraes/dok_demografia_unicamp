
library(readxl)
library(ggplot2)
library(dplyr)
library(maps)
library(mapproj)
library(stringr)

raw <- read_excel(
    path = '../raw/Dados_Plotagem_Corrigido_v2.xlsx'
   ,sheet = 'Dados_plotagem'
) %>%
    filter( !is.na(lat) ) %>% 
    mutate(
        lat = as.numeric(lat)
       ,long = as.numeric(long)
       ,phase = factor(
            phase
           ,levels = paste('Phase', c('1','1/2','2','2/3','3','4'))
           ,order = TRUE
        )
       ,plataforma_reclass = case_when(
            str_detect( string = plataforma, pattern = 'Viral vec'   ) ~ 'Viral vector/Virus like particle'
           ,str_detect( string = plataforma, pattern = 'Virus like'  ) ~ 'Viral vector/Virus like particle'
           ,str_detect( string = plataforma, pattern = 'Inactivated' ) ~ 'Inactivated/Live attenuated virus'
           ,str_detect( string = plataforma, pattern = 'attenu'      ) ~ 'Inactivated/Live attenuated virus'
           ,TRUE ~ as.character( plataforma )
        )
    ) %>%
    na.omit()

## raw %>% count(plataforma, plataforma_reclass)
## raw %>% arrange( desc(lat) ) %>% head() %>% View


## https://r-graph-gallery.com/330-bubble-map-with-ggplot2.html

world <- map_data('world')

ggplot() +
    geom_map(
        data = world, map = world,
        aes(long, lat, map_id = region),
        color = "darkgrey", fill = "lightgrey", size = 0.1, alpha = 0.3
    ) +
    geom_point(
        data = raw
       ,aes(
            x=long
           ,y=lat
           ,size = as.numeric( phase )
           ,color = plataforma_reclass
        )
    ) +
    labs(
        title = 'Mapa da distribuição global do desenvolvimento de vacinas para Covid-19 em fase clínica'
       ,caption = 'Fonte: Autoria própria a partir de OMS (2022)'
       ,size = 'Estágio clínico'
       ,color = 'Plataforma'
    ) +
    theme_void() +
    theme( legend.position = 'left') +
    scale_size_continuous( labels = levels( raw$phase ) ) +
    coord_quickmap() 


