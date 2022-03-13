##############################################################################
# Graficacion animada de las Ciudades mas pobladas de Mexico                 #
# (c) Angel Gonzalez Espinosa 2019                                           #
# aglezespinosa@gmail.com                                                    #
##############################################################################


library(tidyverse)
library(janitor)
library(gganimate)
# https://www.datanovia.com/en/blog/ggplot-themes-gallery/

poblacion <- read_csv("ciudadesmaspobladasmx.csv")

pob_formatted <- poblacion %>%
  group_by(Anio) %>%
  # The * 1 makes it possible to have non-integer ranks while sliding
  mutate(rank = rank(-value),
         Value_rel = value/value[rank==1],
         #Value_lbl = paste0(" ",round(value/1e9))) %>%
         Value_lbl = paste0(" ",round(value))) %>%
  group_by(Ciudad) %>% 
  filter(rank <=10) %>%
  ungroup()


# Inicia la graficación

anim <- ggplot(pob_formatted, aes(rank, group = Ciudad, 
                                  fill = as.factor(Ciudad), color = as.factor(Ciudad))) +
  geom_tile(aes(y = value/2,
                height = value,
                width = 0.9), alpha = 0.8, color = NA) +
  #geom_text(aes(y = 0, label = paste(Ciudad, " ")), vjust = 0.2, hjust = 1) +
  geom_text(aes(y = 0, label = paste(Ciudad, " ")), vjust = 0.6, hjust = 1) +
  geom_text(aes(y=value,label = Value_lbl, hjust=0)) +
  coord_flip(clip = "off", expand = FALSE) +
  scale_y_continuous(labels = scales::comma) +
  scale_x_reverse() +
  guides(color = FALSE, fill = FALSE) +
  theme(axis.line=element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        legend.position="none",
        panel.background=element_blank(),
        panel.border=element_blank(),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        panel.grid.major.x = element_line( size=.1, color="grey" ),
        panel.grid.minor.x = element_line( size=.1, color="grey" ),
        plot.title=element_text(size=25, hjust=0.5, face="bold", colour="red", vjust=-1),
        plot.subtitle=element_text(size=18, hjust=0.5, face="italic", color="black"),
        plot.caption =element_text(size=18, hjust=0.5, face="italic", color="red"),
        plot.background=element_blank(),
        plot.margin = margin(2,2, 2, 4, "cm")) +
  
  transition_states(Anio, transition_length = 4, state_length = 1) +
  view_follow(fixed_x = TRUE)  +
  labs(title = 'Poblacion por Año : {closest_state}',  
       subtitle  =  "10 Principales Ciudades Mexico",
       caption  = "Fuente de Datos: INEGI") 

# Se genera el GIF Animado
animate(anim, 200, fps = 30,  width = 1200, height = 1000, 
        renderer = gifski_renderer("poblacion10.gif"))

# Este codigo genera mejores graficas

anim2 <- poblacion %>%
  group_by(Anio) %>%
  # The * 1 makes it possible to have non-integer ranks while sliding
  mutate(rank = rank(-value),
         Value_rel = value/value[rank==1],
         #Value_lbl = paste0(" ",round(value/1e9))) %>%
         Value_lbl = paste0(" ",round(value))) %>%
  group_by(Ciudad) %>% 
  filter(rank <=10) %>%
  
  
  # plot
  ggplot(aes(-rank,Value_rel, fill = Ciudad)) +
  geom_col(width = 0.8, position="identity") +
  coord_flip() + 
  geom_text(aes(-rank,y=0,label = Ciudad,hjust=0)) +       #country label
  geom_text(aes(-rank,y=Value_rel,label = Value_lbl, hjust=0)) + # value label
  
  coord_flip(clip = "off", expand = FALSE) +
  scale_y_continuous(labels = scales::comma) +
  scale_x_reverse() +
  guides(color = FALSE, fill = FALSE) +
  
  theme_minimal() +
  theme(legend.position = "none",axis.title = element_blank()) +
  theme(axis.line=element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        legend.position="none",
        panel.background=element_blank(),
        panel.border=element_blank(),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        panel.grid.major.x = element_line( size=.1, color="grey" ),
        panel.grid.minor.x = element_line( size=.1, color="grey" ),
        plot.title=element_text(size=25, hjust=0.5, face="bold", colour="red", vjust=-1),
        plot.subtitle=element_text(size=18, hjust=0.5, face="italic", color="black"),
        plot.caption =element_text(size=18, hjust=0.5, face="italic", color="red"),
        plot.background=element_blank(),
        plot.margin = margin(2,2, 2, 4, "cm")) +
  
  
  # animate along Year
  #transition_states(year,4,1)
  transition_states(Anio, transition_length = 4, state_length = 1) +
  view_follow(fixed_x = TRUE)  +
  labs(title = 'Poblacion por Año : {closest_state}',  
       subtitle  =  "10 Principales Ciudades Mexico",
       caption  = "Fuente de Datos: INEGI") 


#animate(p, 100, fps = 25, duration = 20, width = 800, height = 600)
# animate(plot, nframes, fps, duration, detail, renderer,
#device, ref_frame, start_pause, end_pause, rewind, ...)

animate(anim2, 200, fps = 20,  duration = 20, width = 800, height = 600, 
        renderer = gifski_renderer("poblacion2.gif"))
