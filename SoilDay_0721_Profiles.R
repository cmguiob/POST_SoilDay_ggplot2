
library(aqp)
library(ggplot2)
library(ggrepel)
library(colorspace)
library(tidyverse)
library(showtext)

knitr::opts_chunk$set(echo = TRUE, message = FALSE,  warning = FALSE, fig.showtext = T, fig.retina = 1, fig.align = 'center', dpi = 300, out.width = "80%")

showtext_auto()
#Following the advice from here: https://cran.rstudio.com/web/packages/showtext/vignettes/introduction.html



horizons <- readr::read_csv('https://raw.githubusercontent.com/cmguiob/TCI_CerroSeco_git/main/Datos/Suelos_CS_Horiz.csv')

site <- readr::read_csv('https://raw.githubusercontent.com/cmguiob/TCI_CerroSeco_git/main/Datos/Suelos_CS_Sitio.csv')

#Select four profiles and relevant properties for plot
hz4 <- horizons %>%
  dplyr::filter(ID %in% c("CS01", "CS02","CS03","CS04")) %>%
  dplyr::select(ID, BASE, TOPE, ESP, HZ, CON_POR, MX_H, MX_V, MX_C, CON_H, CON_V, CON_C )

head(hz4, 10)



# Create color variables | crear variables de color
hz4$RGBmx <- munsell2rgb(hz4$MX_H, hz4$MX_V, hz4$MX_C)
hz4$RGBco <-munsell2rgb(hz4$CON_H,hz4$CON_V , hz4$CON_C)


# Create factor variable with ID and HZ | Crear variable factor con ID y HZ
hz_bdf <- hz4 %>%
  dplyr::select(ID, BASE, TOPE, ESP,HZ, CON_POR, RGBmx, RGBco)%>%
  dplyr::mutate(ID_HZ = paste(ID, HZ),
         ID_HZ2 = factor(ID_HZ, ID_HZ))

head(hz_bdf, 5)


ggplot(hz_bdf, aes(x = ID, y = ESP, fill = ID_HZ2)) + 
  geom_bar(position="dodge", stat="identity") 



profiles <- ggplot(hz_bdf, aes(x = ID, y = ESP, fill = ID_HZ2)) + 
  geom_bar(position="stack", stat="identity") +
  # Adds colors from the RGBmx variable
  scale_fill_manual(values = hz_bdf$RGBmx,
                    # Don't plot the fill legend | no grafique leyenda de relleno
                    guide = "none") 

profiles



profiles2 <- ggplot(hz_bdf, 
                    aes(x = ID, y = ESP, fill = forcats::fct_rev(ID_HZ2))) + 
  geom_bar(position="stack", stat="identity", width = 0.4) +
  # Adds colors from the RGBmx variable
  scale_fill_manual(values = rev(hz_bdf$RGBmx),
                    # Don't plot the fill legend | no grafique leyenda de relleno
                    guide = "none") +
  # Adds horizon labels | agrega etiquetas a horizontes
  ggrepel::geom_text_repel( data = hz_bdf,   
                   aes(y = BASE - (ESP/2), label = HZ),
                   color = darken(hz_bdf$RGBmx, .2, space = "HCL"),
                   size = 3,
                   family = "robotoc",
                   hjust = 0,
                   direction = "y",
                   nudge_x = 0.3,
                   segment.size = .5,
                   segment.linetype = "dotted",
                   segment.square = TRUE,
                   segment.curvature = 0.1,
                   segment.angle = 30,
                   segment.alpha = 0.5,
                   box.padding = 0.3) +
  # Reverse y axis scale | Invierte la escala del eje y
  scale_y_reverse(breaks = c(0,100,200,300,400,500), 
                  labels=c("0", "100", "200", "300", "400", "500"))+
  scale_x_discrete(position = "top")
  
profiles2



# New data frame with random points | Nuevo data frame con puntos aleatorios
hz_jdf <-  hz4 %>%
  dplyr::select(ID, BASE, TOPE, ESP,HZ, CON_POR, RGBmx, RGBco)%>%
  dplyr::mutate(ID = factor(ID),
         ID_HZ = paste(ID, HZ),
         ID_HZ2 = factor(ID_HZ, ID_HZ))%>%
  dplyr::mutate(CON_POR = ifelse(CON_POR == 0, 1, CON_POR),
         n = 5*ESP*CON_POR / 100,
         mean = 0.5*(BASE - TOPE),
         sd = 0.1*ESP)%>%
  dplyr::mutate(samples = purrr::pmap(.[11:13], rnorm))%>%
  tidyr::unnest(samples)

head(hz_jdf, 20)



profiles3 <- profiles2 +
#location from where jitter spreads out vertically
  geom_jitter(data = hz_jdf, aes(x = ID, y = BASE - (ESP/2)),  
              width = 0.18, 
              # how far jitter spreads out to each side
              height = hz_jdf$ESP*0.5,
              size = 0.3,
              col = hz_jdf$RGBco,
              shape = 16)+
  scale_y_reverse(breaks = c(0,100,200,300,400,500), 
                  labels=c("0", "100", "200", "300", "400", "500\ncm"))+
  scale_x_discrete(position = "top")

profiles3



# Obtener fuentes
font_add_google(name = "Roboto Condensed", family= "robotoc")
font_add_google(name = "Roboto", family= "roboto")


# Definir theme
theme_set(theme_minimal(base_family = "roboto"))

theme_update(panel.grid = element_blank(),
             axis.text = element_text(family = "robotoc",
                                        color = "#c3beb8"),
             axis.title = element_blank(),
             axis.text.x = element_text(family = "robotoc",
                           colour = c('#56B4E9', '#009E73',"#E69F00", "#D55E00"),
                           face = "bold"),
               axis.ticks.x =  element_blank(),
        panel.grid.major.y = element_line(color = "#c3beb8", size = .4, linetype = c("13")))


profiles_vf <-profiles3 +
  geom_hline(yintercept = 0, col = '#f2d29b') +
  theme() +
  coord_cartesian(clip = "off")

profiles_vf




ggsave(file = "Profiles.png", plot = profiles_vf, device = "png", type = "cairo", path = here::here(), dpi = 300, width = 8, height = 5.5)


