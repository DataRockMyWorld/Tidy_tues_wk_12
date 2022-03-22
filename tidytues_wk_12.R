## Setting Up Environment----
library(tidyverse)
library(showtext)
library(ggthemes)
library(patchwork)
library(tidytuesdayR)


font_add_google("El Messiri","El Messiri")
font_add_google("Marck Script","Marck Script")
font_add_google("Merienda One","Merienda One")
font_add_google("Playfair Display","Playfair Display")
font_add_google("Bangers","Bangers")

showtext_auto()

## Importing the Data
tidytues <- tt_load("2022-03-22")

applicants <- tidytues$applicants
babynames <- tidytues$babynames
births <- tidytues$births
lifetables <- tidytues$lifetables
moarinames <- tidytues$maorinames
nz_births <- tidytues$nz_births
nz_lifetables <- tidytues$nz_lifetables
nz_names <- tidytues$nz_names



## Explore the babynames dataset

skimr::skim(babynames)

## I want to look at the variations of the popular Islamic name "Mohammed" over the years

islam <- c("Mohammed","Mohamed","Muhammad")

is_babyname <- babynames %>% 
  filter(name %in% islam) %>%
  group_by(year,name) %>% 
  summarise(prop = sum(n))




plot <- is_babyname %>% 
  ggplot(aes(year,prop, color = name)) +
  geom_rect(aes(xmin = 2001, ymin = 0, xmax =2004, ymax = Inf), alpha = 0.5, fill = "#d3eaf2", color = "transparent") +
  annotate(geom = "curve", x = 1980, y = 500, xend = 2001, yend = 375, curvature = 0.4, arrow = arrow(length = unit(2,"mm")), color ="black") +
  annotate(geom = "label", x = 1980, y = 500, label = "A decline in the use of the\nname between 2001-2003", color = "black", fill = "#d3eaf2", size = 4, family = "Playfair Display") +
  scale_x_continuous(limits = c(1960,NA),breaks = c(1960,1970,1980,1990,2000,2010,2020)) +
  geom_line() +
  scale_color_manual(values = c("darkolivegreen4","darkorchid3","firebrick")) +
  labs( title = "Muhammad on the UP ?",
        subtitle = "Use of variations of the arabic name 'Mohammed' from 1960 t0 2017",
        caption = "Data from Babynames R Package | plot by : Juelzgh",
        x ="Year",
        y = "Number of babies") +
  theme_minimal() 


## Adding thmes

mohammed <- plot + theme(
  plot.title = element_text(size = 20, face = "bold",family = "Bangers", color = "black"),
  plot.subtitle = element_text(size = 12, face = "italic", colour = "black", family = "El Messiri",lineheight = .5),
  plot.title.position = "plot",
  axis.ticks = element_blank(),
  axis.title = element_text(face = "bold", family = "Playfair Display"),
  axis.title.y = element_text(face = "bold", family = "Playfair Display"),
  panel.grid.minor =element_blank(),
  legend.position = c(0.5,0.94),
  legend.background = element_rect(fill = "transparent", colour = "transparent"),
  legend.text = element_text(size = 10, family = "Merienda One", face = "bold")) +
  
  guides(color = guide_legend(direction = "horizontal",
                              title = NULL,
                              override.aes = list(size = 4)))


ggsave("mohammed.png")


         