library(tidyverse)
library(ggplot2)
library(kableExtra)
library(modelsummary)
library(here)
library(ggpubr)
library(grid)

palette <- list("white" = "#FAFAFA",
                "light_grey" = "#d9d9d9",
                "dark" = "#0c2230",
                "red" = "#ed195a",
                "blue" = "#1c86ee",
                "green" = "#7CAE7A",
                "dark_green" = "#496F5D",
                "gold" = "#DAA520")

file_dir <- "C:/Users/garci/Dropbox/eab_chicago_data"

results_dir <- here::here("results")

cleaned_dir <- here::here("cleaned")

fig_dir <- here::here("figs")

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#### Grid unit of analysis
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

eab_panel <- readRDS(paste0(cleaned_dir, "/eab_panel_grid3km.rds"))%>%
  mutate_at(vars(place_first_detected), as.numeric)

eab_cross <- eab_panel %>%
  filter(year == 2005)

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#### School unit of analysis
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

eab_panel_school <- readRDS(paste0(cleaned_dir, "/eab_panel_school2mi.rds"))

eab_school_cross <- eab_panel_school %>%
  filter(year == 2005)
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#### How is tree cover correlated with test scores

p1 <- ggplot(eab_school_cross, aes(x = ISAT_composite, y = canopy))+
  xlab("% students meeting or exceeding ISAT threshold (2005)") + 
  geom_point(color = palette$dark, shape = 21)+
  geom_smooth(method='lm', formula= y~x, color = palette$blue,
              size = 1.5
  )+
  theme_classic()+
  ylab("Mean canopy cover")

p2 <- ggplot(eab_school_cross, aes(x = lowinc_pct, y = canopy))+
  xlab("% low-income students (2005)") + 
  geom_point(color = palette$dark, shape = 21)+
  geom_smooth(method='lm', formula= y~x, color = palette$blue,
              size = 1.5
              )+
  theme_classic()

p2$labels$y <- ""

ggarrange(p1, p2, ncol=2, nrow=1, common.legend = FALSE,
          labels = c("A", "B"))
ggsave(paste0(fig_dir, "/descriptive_canopy_associations.png"), width = 10, height = 5)

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#### Year of arrival vs. test score, low-income, canopy cover

eab_school_cross <- eab_school_cross %>%
  mutate(ever_detected = as.character(ifelse(first_exposed > 0, "Detected", "Never detected")))

point_size = 3

p1 <- ggplot(eab_school_cross, aes(x=ever_detected, color=ever_detected,
                             y=canopy)) + 
  geom_violin(fill = palette$light_grey, alpha = .75, size = 0.8)+
  stat_summary(fun.data="mean_sdl", geom="point", 
               size=point_size,
               color = palette$red)+
  theme_classic()+
  theme(legend.position = "none",
        axis.title.x = element_blank())+
  scale_color_manual(values=c(palette$blue, palette$dark))+
  ylim(0,100)+ 
  ylab("Mean canopy cover")

p2 <- ggplot(eab_school_cross, aes(x=ever_detected, color=ever_detected,
                             y=ISAT_composite)) + 
  geom_violin(fill = palette$light_grey, alpha = .75, size = 0.8)+
  stat_summary(fun.data="mean_sdl", geom="point", 
               size=point_size,
               color = palette$red)+
  theme_classic()+
  theme(legend.position = "none",
        axis.title.x = element_blank())+
  scale_color_manual(values=c(palette$blue, palette$dark))+
  ylim(0,100)+
  ylab("% students meeting or exceeding ISAT threshold (2005)")


p3 <- ggplot(eab_school_cross, aes(x=ever_detected, color=ever_detected,
                             y=lowinc_pct)) + 
  geom_violin(fill = palette$light_grey, alpha = .75, size = 0.8)+
  stat_summary(fun.data="mean_sdl", geom="point", 
               size=point_size,
              color = palette$red)+
  theme_classic()+
  theme(legend.position = "none",
        axis.title.x = element_blank())+
  scale_color_manual(values=c(palette$blue, palette$dark))+
  ylim(0,100)+
   ylab("% low-income students (2005)")


annotate_figure(ggarrange(p1, p2, p3, ncol=3, nrow=1, common.legend = FALSE,
                          labels = c("A", "B", "C")
                          ),# legend="bottom"),
                bottom = textGrob("Ash borer status (prior to 2016)")
)
ggsave(paste0(fig_dir, "/descriptive_schoolexposed_violin.png"), width = 10, height = 5)

