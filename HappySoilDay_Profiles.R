#RPubs / R journal
#Plotting soil profiles with ggplot 2: an alternative to aqp

#Why?
# Compatibility with ggplot functions and framework, e.g. usage with patchwork
# AQP functions are not as structured and transparent as ggplot
# Aestetic freedom: fonts, placing of elements...
# Need to encode other soil elements: a ggsoil?

#For a paper: encoding soil information with the grammar of graphics.
# expand on encoding and decoding theory
# expand on encoding elements in soils
# expand on the grammar of graphics
# encoding structure: lines and modifying contours (check cedric drawing penguins)
# encoding laterla variation: using geom_area
# encoding proportions
# Publish:
# https://www.springer.com/journal/41651
# https://journal.r-project.org/
# https://journals.sagepub.com/home/ivi
# https://www.springer.com/journal/12650
# https://www.sciencedirect.com/journal/journal-of-visual-languages-and-computing 
# https://www.sciencedirect.com/journal/environmental-modelling-and-software/about/aims-and-scope
# https://www.journals.elsevier.com/computers-and-geosciences


#Inspiration: beaudette, the flower guy , cedric scherer (combining jitter with other)

library(aqp)
library(tidyverse)
library(ggplot2)
library(colorspace)


#DATA
ruta <- "C:/Users/cguio/Documents/Terrae/TCI_Cerro Seco_GIT/"

# Cargar datos de perfiles
hz <- read.csv(paste(ruta,"Datos/Suelos_CS_Horiz.csv", sep = ""))
sitio <- read.csv(paste(ruta,"Datos/Suelos_CS_Sitio.csv", sep = "")) 
hz4 <- hz[1:23,]
head(hz4, 5)


#SETUP
# Definir theme
theme_set(theme_minimal(base_family = "roboto"))

theme_update(panel.grid = element_blank(),
             axis.text = element_text(family = "robotoc",
                                      color = "#c3beb8",
                                      face = "bold"),
             axis.title = element_blank(),
             axis.ticks.x =  element_line(color = "#c3beb8", size = .7),
             axis.ticks.y.right =  element_line(color = "#c3beb8", size = .7),
             legend.position = c(0,0.85),
             legend.direction = "vertical", 
             legend.box = "horizontal",
             legend.title = element_text(size = 13, 
                                         face = "bold", 
                                         color = "grey20", 
                                         family = "roboto"),
             legend.text = element_text(size = 10, 
                                        color = "#c3beb8", 
                                        family = "robotoc",
                                        face = "bold"),
             legend.key.size = unit(0.8, "cm"))

#PREP
# Create color variables
hz4$RGBmx <- munsell2rgb(hz4$MX_H, hz4$MX_V, hz4$MX_C)
hz4$RGBco <-munsell2rgb(hz4$CON_H,hz4$CON_V , hz4$CON_C)


#Factor horizons to order
hz_bdf <- hz4 %>%
  dplyr::select(ID, BASE, TOPE, ESP,HZ, CON_POR, RGBmx, RGBco)%>%
  dplyr::mutate(ID_HZ = paste(ID, HZ),
         ID_HZ2 = factor(ID_HZ, ID_HZ))

hz_jdf <-  hz4 %>%
  dplyr::select(ID, BASE, TOPE, ESP,HZ, CON_POR, RGBmx, RGBco)%>%
  dplyr::mutate(ID = factor(ID),
         ID_HZ = paste(ID, HZ),
         ID_HZ2 = factor(ID_HZ, ID_HZ))%>%
  dplyr::mutate(CON_POR = ifelse(CON_POR == 0, 1, CON_POR),
         n = 5*ESP*CON_POR / 100,
         mean = 0.5*(BASE - TOPE),
         sd = 0.1*ESP)%>%
  dplyr::mutate(samples = pmap(.[11:13], rnorm))%>%
  unnest(samples)

# Points for jitter:
# mean: BASE - TOPE/2, sd = x*ESP, n = 5*CON_POR*ESP/100
# The problem with n: calculated with a multiple three rule, assuming that 1cm 
# which is 100% saturated has 5 concentrations, i.e. the size of concentrations is 2mm

#Stacked bar chart

ggplot(hz_bdf, aes(x = reorder(ID, desc(ID)), y = ESP, fill = forcats::fct_rev(ID_HZ2))) + 
  geom_bar(position="stack", stat="identity", width = 0.4) +
  scale_fill_manual(values = rev(hz_bdf$RGBmx),
                    guide = FALSE) +
  geom_text_repel( data = hz_bdf,   
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
  scale_y_reverse(breaks = c(0,100,200,300,400,500), 
                  labels=c("0", "100", "200", "300", "400", "500"))+
  scale_x_discrete(position = "top")
