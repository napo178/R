#Sobre----------------------------------------------------------------
#Anderson Ara, PhD
#R version 4.0.3

#Pacotes necessários -------------------------------------------------
require(gganimate)
require(ggplot2)
require(RColorBrewer)
require(gifski)

#Número de pontos ----------------------------------------------------
K=15
#Cores básicas -------------------------------------------------------
cores=c('#fff9c4','#18ffff','#ffc107')

#Função fogos --------------------------------------------------------

fogos = function(size=1,centro=c(0,0),n=K){
  ns=1:n
  ds=data.frame(x1 = rep(centro[1], length(ns)),
                x2 = size*cos(ns)  + centro[1],
                y1 = rep(centro[2], length(ns)),
                y2 = size*sin(ns) + centro[2]
  ) 
  return(ds)
}

# Dataset com dois 'fogos' -------------------------------------------
dd=rbind(data.frame(size=0.2,
                    fogos(size=0.2),
                    fogos(size=0.1,centro=c(0.5,0.5))),
         data.frame(size=0.5,
                    fogos(size=0.5),
                    fogos(size=0.2,centro=c(0.5,0.5))),
         data.frame(size=0.7,
                    fogos(size=0.7),
                    fogos(size=0.5,centro=c(0.5,0.5))))


# Construção do plot geral -------------------------------------------

g <- 
  ggplot()+
  geom_point(aes(x = x2, y = y2, color=size), 
             data = dd, 
             shape = 8, 
             size=rgeom(K*3,0.2)+1)+
  geom_point(aes(x = x2.1, y = y2.1, color=size), 
             data = dd, 
             shape = 8, 
             size=rgeom(K*3,0.2)+1)+
  scale_colour_gradientn(colours = 
                           colorRampPalette(cores)(4))+
  #  annotate("text",label="@led.ufba",x=0.7,y=-0.5,col="#ffd54f")+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.line=element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        legend.position = "none",
        panel.background = element_rect(fill = '#01021e',colour='#01021e'),
        plot.background = element_rect(fill = '#01021e', colour='#01021e'))

# Animação e Renderização ---------------------------------------------


g2 <- g + transition_time(size) +
  labs(title = "")+
  shadow_wake(wake_length = 0.25, alpha = T)+
  enter_fade() 

animate(g2, renderer = gifski_renderer(),duration=4)
anim_save("fireworks2021.gif", width = 1000, height = 1000) 