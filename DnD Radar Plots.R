##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                                                                            ~~
##                        ARBITERS DATA VISUALIZATION                       ----
##                                                                            ~~
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
library(ARTofR)
library(data.table)
library(ggplot2)
library(ggradar)
library(ggimage)
library(lightslateblue404)
library(geomtextpath)
library(tidyverse)
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                          Getting Data and Images                         ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


name = c("Elovar Olara","Geralynn Axebrand","Hannah Greenwood",
                   "Hiltrix Ravenswood","Mithuphas","Ryuko","Valac Marx")
str = c(15,24,10,19,23,12,8)
dex = c(10,18,17,20,13,21,13)
con = c(10,19,14,20,17,15,12)
int = c(12,9,12,15,14,10,20)
wis = c(20,9,10,13,12,15,10)
chr = c(16,10,18,11,8,10,17)
class = c("Cleric", "Barbarian", "Sorcerer", "Rogue", "Fighter", "Monk", "Wizard")


url = c("https://raw.githubusercontent.com/hannahmfg/Arbiters/main/Character_Profiles/Elovar%20Olara.png",
        "https://raw.githubusercontent.com/hannahmfg/Arbiters/main/Character_Profiles/Geralynn%20Axebrand.png",
        "https://raw.githubusercontent.com/hannahmfg/Arbiters/main/Character_Profiles/Hannah%20Greenwood.png",
        "https://raw.githubusercontent.com/hannahmfg/Arbiters/main/Character_Profiles/Hiltrix%20Ravenswood.png",
        "https://raw.githubusercontent.com/hannahmfg/Arbiters/main/Character_Profiles/Mithuphas.png",
        "https://raw.githubusercontent.com/hannahmfg/Arbiters/main/Character_Profiles/Ryuko.png",
        "https://raw.githubusercontent.com/hannahmfg/Arbiters/main/Character_Profiles/Valac%20Marx.png")



arbiters = data.table(name,class,str,dex,con,int,wis,chr,url)

arbiters = arbiters%>%
     select(name, class,str, dex, con, int, wis, chr, url)%>%
     pivot_longer(str:chr, names_to = "ability", values_to = "stat")
# saveRDS(arbiters, "Arbiters_Stats.rds")
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                         Building Basic Radar Plots                       ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
labels = data_frame(y = c(0, 5, 10, 15, 20, 25),
                    x = c(0.25, 0.25, 0.25, 0.25))
segements = data.frame( x1 = rep(0, 6),
                        x2 = rep(5.5, 6),
                        y1 = c(0, 5, 10, 15, 20, 25),
                        y2 = c(0, 5, 10, 15, 20, 25))
##~~~~~~~~~~~~~~~~~~~~~~
##  ~ Elovar Olara  ----
##~~~~~~~~~~~~~~~~~~~~~~

elovar = arbiters%>%filter(name=="Elovar Olara")
elovar_plot = ggplot(elovar, aes(x=ability, y=stat, fill=ability))+
     coord_polar()+
     theme_void()+
     theme(plot.background = element_rect(fill = "#f9f9ff"))+
     geom_textpath(inherit.aes=FALSE,
                   mapping=aes(x=ability, label=ability, y=30),
                   fontface="bold", family = "Copperplate",
                   upright=TRUE, text_only=TRUE, size=4)+
     geom_segment(inherit.aes=FALSE,
                  data = segments,
                  mapping=aes(x=x1,xend=x2,y=y1,yend=y2), size=0.35)+
     geom_col(width=.8, show.legend = FALSE)+
     scale_y_continuous(limits=c(-9,30))+
     geom_textsegment(inherit.aes=FALSE,
                      data=labels,
                      mapping=aes(x=5.5, xend=6.5, y= y, yend=y, label=y),
                      linewidth=0.35, size=2.5, family = "Copperplate")+
     geom_image(mapping=aes(y=-9,x=0), image = elovar$url[1], size = 0.42) +
     scale_fill_404_d(option = "empirecity", direction = -1)+
     labs(title="ELOVAR OLARA",
          subtitle="Ability Stats, Eryindon Campaign",
          caption = "Made with love by Hannah Guthrie") +
     theme(
          plot.title=element_text(face="bold", family = "Copperplate", hjust=0.5, size=18, vjust=-2),
          plot.subtitle=element_text(hjust=0.5, vjust=-5, family = "Copperplate"),
     )
elovar_plot
# ggsave("Elovar_Stat_Plot.png", dpi = 600)
##~~~~~~~~~~~~~~~~~~~~~~~~~~~
##  ~ Geralynn Axebrand  ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~
geralynn = arbiters%>%filter(name=="Geralynn Axebrand")
geralynn_plot = ggplot(geralynn, aes(x=ability, y=stat, fill=ability))+
     coord_polar()+
     theme_void()+
     theme(plot.background = element_rect(fill = "#f1f3f5"))+
     geom_textpath(inherit.aes=FALSE,
                   mapping=aes(x=ability, label=ability, y=30),
                   fontface="bold", family = "Copperplate",
                   upright=TRUE, text_only=TRUE, size=4)+
     geom_segment(inherit.aes=FALSE,
                  data = segments,
                  mapping=aes(x=x1,xend=x2,y=y1,yend=y2), size=0.35)+
     geom_col(width=.8, show.legend = FALSE)+
     scale_y_continuous(limits=c(-9,30))+
     geom_textsegment(inherit.aes=FALSE,
                      data=labels,
                      mapping=aes(x=5.5, xend=6.5, y= y, yend=y, label=y),
                      linewidth=0.35, size=2.5, family = "Copperplate")+
     geom_image(mapping=aes(y =-9,x = 0), image = geralynn$url[1], size = 0.45) +
     scale_fill_404_d(option = "okcomputer")+
     labs(title="Geralynn Axebrand",
          subtitle="Ability Statistics, Eryindon Campaign",
          caption = "Made with love by Hannah Guthrie") +
     theme(
          plot.title=element_text(face="bold", family = "Copperplate", 
                                  hjust=0.5, size=18, vjust=-2),
          plot.subtitle=element_text(hjust=0.5, vjust=-5, family = "Copperplate"),
     )
geralynn_plot 
# ggsave("Geralynn_Stat_Plot.png", dpi = 600)
##~~~~~~~~~~~~~~~~~~~~~~~~~~
##  ~ Hannah Greenwood  ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~
hannah = arbiters%>%filter(name=="Hannah Greenwood")
hannah_plot = ggplot(hannah, aes(x=ability, y=stat, fill=ability))+
     coord_polar()+
     theme_void()+
     geom_textpath(inherit.aes=FALSE,
                   mapping=aes(x=ability, label=ability, y=30),
                   fontface="bold", family = "Copperplate",
                   upright=TRUE, text_only=TRUE, size=4)+
     geom_segment(inherit.aes=FALSE,
                  data = segments,
                  mapping=aes(x=x1,xend=x2,y=y1,yend=y2), size=0.35)+
     geom_col(width=.8, show.legend = FALSE)+
     scale_y_continuous(limits=c(-9,30))+
     geom_textsegment(inherit.aes=FALSE,
                      data=labels,
                      mapping=aes(x=5.5, xend=6.5, y= y, yend=y, label=y),
                      linewidth=0.35, size=2.5, family = "Copperplate")+
     geom_image(mapping=aes(y =-8,x = 0), image = hannah$url[1], size = 0.42) +
     scale_fill_404_d(option = "fire")+
     labs(title="Hannah Greenwood",
          subtitle="Ability Statistics, Eryindon Campaign",
          caption = "Made with love by Hannah Guthrie") +
     theme(
          plot.title=element_text(face="bold", family = "Copperplate", 
                                  hjust=0.5, size=18, vjust=-2),
          plot.subtitle=element_text(hjust=0.5, vjust=-5, family = "Copperplate"),
          plot.background = element_rect(fill = "#fffff1")
     )
hannah_plot 
# ggsave("Hannah_Stats_Plot.png", dpi = 600)
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##  ~ Hiltrix Ravenswood  ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~
hiltrix = arbiters%>%filter(name=="Hiltrix Ravenswood")
hiltrix_plot = ggplot(hiltrix, aes(x=ability, y=stat, fill=ability))+
     coord_polar()+
     theme_void()+
     geom_textpath(inherit.aes=FALSE,
                   mapping=aes(x=ability, label=ability, y=30),
                   fontface="bold", family = "Copperplate",
                   upright=TRUE, text_only=TRUE, size = 4)+
     geom_segment(inherit.aes=FALSE,
                  data = segments,
                  mapping=aes(x=x1,xend=x2,y=y1,yend=y2), size=0.35)+
     geom_col(width=.8, show.legend = FALSE)+
     scale_y_continuous(limits=c(-9,30))+
     geom_textsegment(inherit.aes=FALSE,
                      data=labels,
                      mapping=aes(x=5.5, xend=6.5, y= y, yend=y, label=y),
                      linewidth=0.35, size=2.5, family = "Copperplate")+
     geom_image(mapping=aes(y =-9,x = 0), image = hiltrix$url[1], size = 0.45) +
     scale_fill_404_d(option = "dune")+
     labs(title="Hiltrix Ravenswood",
          subtitle="Ability Statistics, Eryindon Campaign",
          caption = "Made with love by Hannah Guthrie") +
     theme(
          plot.title=element_text(face="bold", family = "Copperplate", 
                                  hjust=0.5, size=18, vjust=-2),
          plot.subtitle=element_text(hjust=0.5, vjust=-5, family = "Copperplate"),
          plot.background = element_rect(fill = "#f5f5f5"),
     )
hiltrix_plot 
# ggsave("Hiltrix_Stats_Plot.png", dpi = 600)
##~~~~~~~~~~~~~~~~~~~
##  ~ Mithuphas  ----
##~~~~~~~~~~~~~~~~~~~
mithuphas = arbiters%>%filter(name=="Mithuphas")
mith_plot = ggplot(mithuphas, aes(x=ability, y=stat, fill=ability))+
     coord_polar()+
     theme_void()+
     geom_textpath(inherit.aes=FALSE,
                   mapping=aes(x=ability, label=ability, y=30),
                   fontface="bold", family = "Copperplate",
                   upright=TRUE, text_only=TRUE, size=4)+
     geom_segment(inherit.aes=FALSE,
                  data = segments,
                  mapping=aes(x=x1,xend=x2,y=y1,yend=y2), size=0.35)+
     geom_col(width=.8, show.legend = FALSE)+
     scale_y_continuous(limits=c(-9,30))+
     geom_textsegment(inherit.aes=FALSE,
                      data=labels,
                      mapping=aes(x=5.5, xend=6.5, y= y, yend=y, label=y),
                      linewidth=0.35, size=2.5, family = "Copperplate")+
     geom_image(mapping=aes(y =-9,x = 0), image = mithuphas$url[1], size = 0.45) +
     scale_fill_404_d(option = "stuntdouble")+
     labs(title="Mithuphas",
          subtitle="Ability Statistics, Eryindon Campaign",
          caption = "Made with love by Hannah Guthrie") +
     theme(
          plot.title=element_text(face="bold", family = "Copperplate", 
                                  hjust=0.5, size=18, vjust=-2),
          plot.subtitle=element_text(hjust=0.5, vjust=-5, family = "Copperplate"),
          plot.background = element_rect(fill = "#fffff2")
     )
mith_plot 
# ggsave("Mithuphas_Stats_Plot.png", dpi = 600)
##~~~~~~~~~~~~~~~
##  ~ Ryuko  ----
##~~~~~~~~~~~~~~~
ryuko = arbiters%>%filter(name=="Ryuko")
ryuko_plot = ggplot(ryuko, aes(x=ability, y=stat, fill=ability))+
     coord_polar()+
     theme_void()+
     geom_textpath(inherit.aes=FALSE,
                   mapping=aes(x=ability, label=ability, y=30),
                   fontface="bold", family = "Copperplate",
                   upright=TRUE, text_only=TRUE, size=4)+
     geom_segment(inherit.aes=FALSE,
                  data = segments,
                  mapping=aes(x=x1,xend=x2,y=y1,yend=y2), size=0.35)+
     geom_col(width=.8, show.legend = FALSE)+
     scale_y_continuous(limits=c(-9,30))+
     geom_textsegment(inherit.aes=FALSE,
                      data=labels,
                      mapping=aes(x=5.5, xend=6.5, y= y, yend=y, label=y),
                      linewidth=0.35, size=2.5, family = "Copperplate")+
     geom_image(mapping=aes(y =-9,x = 0), image = ryuko$url[1], size = 0.45) +
     scale_fill_404_d(option = "goldbloom")+
     labs(title="ryuko",
          subtitle="Ability Statistics, Eryindon Campaign",
          caption = "Made with love by Hannah Guthrie") +
     theme(
          plot.title=element_text(face="bold", family = "Copperplate", 
                                  hjust=0.5, size=18, vjust=-2),
          plot.subtitle=element_text(hjust=0.5, vjust=-5, family = "Copperplate"),
          plot.background = element_rect(fill = "#faf2ef")
     )
ryuko_plot 
# ggsave("Ryuko_Stats_Plot.png", dpi = 600)
##~~~~~~~~~~~~~~~~~~~~
##  ~ Valac Marx  ----
##~~~~~~~~~~~~~~~~~~~~
valac = arbiters%>%filter(name=="Valac Marx")
valac_plot = ggplot(valac, aes(x=ability, y=stat, fill=ability))+
     coord_polar()+
     theme_void()+
     geom_textpath(inherit.aes=FALSE,
                   mapping=aes(x=ability, label=ability, y=30),
                   fontface="bold", family = "Copperplate",
                   upright=TRUE, text_only=TRUE, size= 4, colour = "white")+
     geom_segment(inherit.aes=FALSE,
                  data = segments,
                  mapping=aes(x=x1,xend=x2,y=y1,yend=y2), size=0.35, colour = "white")+
     geom_col(width=.8, show.legend = FALSE)+
     scale_y_continuous(limits=c(-9,30))+
     geom_textsegment(inherit.aes=FALSE,
                      data=labels,
                      mapping=aes(x=5.5, xend=6.5, y= y, yend=y, label=y),
                      linewidth=0.35, size=2.5, family = "Copperplate", colour = "white")+
     geom_image(mapping=aes(y =-9,x = 0), image = valac$url[1], size = 0.45) +
     scale_fill_404_d(option = "sunset")+
     labs(title="Valac Marx",
          subtitle="Ability Statistics, Eryindon Campaign",
          caption = "Made with love by Hannah Guthrie") +
     theme(
          plot.title=element_text(face="bold", family = "Copperplate", 
                                  hjust=0.5, size=18, vjust=-2, colour = "white"),
          plot.subtitle=element_text(hjust=0.5, vjust=-5, family = "Copperplate", colour = "white"),
          plot.background = element_rect(fill = "#341d5c" )
     )
valac_plot 
# ggsave("Valac_Stats_Plot.png", dpi = 600)

##~~~~~~~~~~~~~~~~~
##  ~ Favreau  ----
##~~~~~~~~~~~~~~~~~

favreau = data.table( name = "Favreaux Gilmont",
                      str = 9,
                      dex = 18,
                      con = 12,
                      int = 10,
                      wis = 11,
                      chr = 18,
                      url = "https://raw.githubusercontent.com/hannahmfg/Arbiters/main/Character_Profiles/Favreau.png")
favreaux = favreau %>%
     select(name,str, dex, con, int, wis, chr, url)%>%
     pivot_longer(str:chr, names_to = "ability", values_to = "stat")

fav_plot = ggplot(favreaux, aes(x=ability, y=stat, fill=ability))+
     coord_polar()+
     theme_void()+
     geom_textpath(inherit.aes=FALSE,
                   mapping=aes(x=ability, label=ability, y=30),
                   fontface="bold", family = "Copperplate",
                   upright=TRUE, text_only=TRUE, size= 5, colour = "black")+
     geom_segment(inherit.aes=FALSE,
                  data = segments,
                  mapping=aes(x=x1,xend=x2,y=y1,yend=y2), size=0.5, colour = "black")+
     geom_col(width=.8, show.legend = FALSE)+
     scale_y_continuous(limits=c(-9,30))+
     geom_textsegment(inherit.aes=FALSE,
                      data=labels,
                      mapping=aes(x=5.5, xend=6.5, y= y, yend=y, label=y),
                      linewidth=0.35, size=2.5, family = "Copperplate", colour = "black")+
     geom_image(mapping=aes(y =-9,x = 0), image = favreaux$url[1], size = 0.35) +
     scale_fill_404_d(option = "hitchhike", direction = -1)+
     labs(title="Favreaux Gilmont",
          subtitle="Ability Statistics, Eryindon Campaign",
          caption = "Made with love by Hannah Guthrie") +
     theme(
          plot.title=element_text(face="bold", family = "Copperplate", 
                                  hjust=0.5, size=18, vjust=-2, colour = "black"),
          plot.subtitle=element_text(hjust=0.5, vjust=-5, family = "Copperplate", colour = "black"),
          plot.background = element_rect(fill = "#faf2ef" )
     )
fav_plot
# ggsave("Favreaux_Plot.png", dpi = 600)

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                            All Arbiters Together                         ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
pal_font= lightslateblue404_palettes$moonset[1]
pal_bg  = "#d8d7ce"
pal_line = lightslateblue404_palettes$moonset[1]

all_plot = ggplot(arbiters, aes(x=ability, y=stat, fill=ability))+
     # Text for y-axis #
     geom_textpath(inherit.aes=FALSE,
                   mapping=aes(x=ability, label=ability, y=30),
                   fontface="bold", upright=TRUE, text_only=TRUE, 
                   size=3, color=pal_font, family = "Copperplate") + 
     geom_image(mapping=aes(y=-9,x=0,image= url), size=0.47) +
     coord_curvedpolar()+
     # Line Segments for Panel Gridlines #
     geom_segment(inherit.aes = FALSE,
                  data = segments,
                  mapping = aes(x = x1,xend = x2, y = y1,yend = y2),
                  size=0.45, color=pal_line)+
     geom_col(show.legend = FALSE, width=.8) +
     # Text for Panel Gridlines #
     geom_textsegment(inherit.aes=FALSE,
                      data=labels,
                      mapping=aes(x=5.5, xend=6.5, y= y, yend=y, label=y),
                      color = pal_line, textcolour= pal_font, linewidth=0.45, size=2.5)+
     scale_fill_404_d(option = "sunglow")+
     scale_y_continuous(limits=c(-9,30))+
     # Iterate for each Party Member #
     facet_wrap(~toupper(name))+
     # Labels, Theme, Aesthetics #
     labs(title="THE ARBITERS OF ASCENDENCY",
          subtitle="Abiltiy Stats by Party Member",
          caption="Made with Love | Hannah Guthrie")+
     theme_minimal()+
     theme(text=element_text(color=pal_font),
           plot.background = element_rect(fill=pal_bg),
           plot.title=element_text(face="bold", family = "Copperplate", hjust=0.5, size=18, margin=margin(t=5)),
           plot.subtitle=element_text(hjust=0.5, family= "Copperplate", margin=margin(t=5, b=20)),
           plot.caption = element_text(margin=margin(t=15)),
           axis.title=element_blank(),
           panel.grid = element_blank(),
           plot.margin=margin(t=10,b=5,l=10,r=10),
           axis.text=element_blank(),
           axis.ticks = element_blank(),
           strip.text=element_text(face="bold", family = "Copperplate", color=pal_font, size=12, vjust=-0.5))
all_plot
# ggsave("Arbiters_Plot.png", dpi = 800)

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                  Table of Averages and Standard Deviations               ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
library(gt)
stat_arb = data.table(Name = name, 
                      Strength = str, 
                      Dexterity = dex,
                      Constitution = con, 
                      Intelligence = int, 
                      Wisdom = wis, 
                      Charisma = chr)
averages = lapply(stat_arb[,2:7], mean, 2)
sds = lapply(stat_arb[,2:7], sd, 2)
ability = c("Strength", "Dexterity","Constitution",
            "Intelligence","Wisdom","Charisma")
arbs = data.table(averages = as.numeric(averages), sds = as.numeric(sds), ability = ability)
arbs$sds = round(arbs$sds, digits = 2)
setcolorder(arbs, neworder = c(3,1,2))
gt(arbs)
gt_arbs = gt(arbs) %>% cols_label(ability = "Ability", averages = "Average Score",
                                  sds = "Standard Deviation") %>% 
     tab_header(title = "Arbiters of Ascendency",
                subtitle = "Summary Statistics of Abilities") %>%
     tab_style(locations = cells_title(groups = "subtitle"), 
               style = list(cell_text(align = "center",weight = "bold", size = 24, font = "Copperplate",
                                      color = lightslateblue404_palettes$moonset[1]))) %>%
     tab_style( locations = cells_column_labels(columns = everything()),
                style = list( cell_borders(sides = "bottom",color = lightslateblue404_palettes$moonset[1], weight = px(2)),
                              cell_text(weight = "bold", font = "Copperplate", color = lightslateblue404_palettes$moonset[1])
                )) %>% 
     tab_style(locations = cells_title(groups = "title"),
               style= list(cell_text(align = "center",weight = "bold", size = 24, font = "Copperplate",
                                     color = lightslateblue404_palettes$moonset[1]),
                           cell_borders(sides = "bottom", 
                                        color = lightslateblue404_palettes$moonset[1], 
                                        weight = px(2)))) %>%
     tab_style( locations = cells_body(columns = everything(), rows = everything()),
                style = list( cell_text( font = "Copperplate", color = lightslateblue404_palettes$moonset[1])))
gt_arbs
