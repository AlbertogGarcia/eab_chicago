library(tidyverse)
library(ggplot2)
library(kableExtra)
library(modelsummary)
library(here)
library(ggpubr)
library(grid)

palette <- list("white" = "#FAFAFA",
                "light_grey" = "#d9d9d9",
                "dark_grey" = "grey30",
                "dark" = "#0c2230",
                "red" = "#ed195a",
                "blue" = "#1c86ee",
                "green" = "#7CAE7A",
                "dark_green" = "#496F5D",
                "gold" = "#DAA520")

results_dir <- here::here("analysis", "results")

clean_data_dir <- here::here("cleaned")

fig_dir <- here::here("figs")

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#### Grid unit of analysis
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

eab_panel <- readRDS(paste0(clean_data_dir, "/eab_panel_grid5km.rds"))%>%
  mutate_at(vars(place_first_detected), as.numeric)

eab_cross <- eab_panel %>%
  filter(year == 2005)

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#### School unit of analysis
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

eab_panel_school <- readRDS(paste0(clean_data_dir, "/eab_panel_school2mi.rds"))

eab_school_cross <- eab_panel_school %>%
  filter(year == 2005)%>%
  mutate(ever_detected = as.character(ifelse(first_exposed > 0, "Detected", "Never detected")))


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#### Expansion of ash borer exposure across schools

exposure_by_year <- eab_panel_school %>%
  group_by(RCDS)%>%
  slice_head()%>%
  ungroup%>%
  group_by(first_exposed)%>%
  count()%>%
  filter(first_exposed != 0)%>%
  ungroup %>%
  mutate(cum_exposed = cumsum(n))

coeff <- 4

ggplot(exposure_by_year, aes(x=first_exposed)) +
  geom_col( aes(y=n), fill=palette$blue, color = palette$dark_grey, alpha = 0.75, width = 0.75) + 
  geom_line( aes(y= cum_exposed / coeff ), linewidth=2, color=palette$dark) +
  scale_y_continuous(
    # Features of the first axis
    name = "Newly exposed schools",
    # Add a second axis and specify its features
    sec.axis = sec_axis(~.*coeff, name="Cumulative number of treated schools")
  ) + 
  theme_classic() +
  theme(
    axis.title.y = element_text(color = palette$dark_grey),
    axis.text.y = element_text(color = palette$blue),
    axis.text.y.right = element_text(color = palette$dark),
    axis.title.y.right = element_text(color = palette$dark)
  ) +
  scale_x_continuous("Year", breaks = seq(2006, 2014, 2), labels = seq(2006, 2014, 2))
ggsave(paste0(fig_dir, "/cumulative_school_exposure.png"), width = 6, height = 4)



ggplot(eab_school_cross, aes(x = canopy, y = first_exposed))+
  xlab("% students meeting or exceeding ISAT threshold (2005)") + 
  geom_point(color = palette$dark, shape = 21)+
  geom_smooth(method='lm', formula= y~x, color = palette$blue,
              size = 1.5
  )+
  theme_classic()+
  ylab("Mean canopy cover")

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#### How is tree cover correlated with test scores

p1 <- ggplot(eab_school_cross, aes(x = canopy, y = ISAT_composite))+
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

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
### Detected vs. never-detected table
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

describe_vars <- c(
  "hispanic_pct",
  "asian_pct",
  "black_pct",
  "white_pct",
  "lowinc_pct",
  "chronic truants rate school pct",
  "all_attendance rate school pct",
  "all_tests",
  "ISAT_composite",
  "canopy"
)

summary <- data.frame()
for(d in describe_vars){
  
  eab_school_describe <- eab_school_cross %>%
    select(ever_detected, d)%>%
    rename(this_var = d)%>%
    mutate(this_var = as.numeric(this_var))
  
  ttest <- t.test(this_var~ever_detected, data=eab_school_describe)
  sd_detected <- sd(subset(eab_school_describe, ever_detected == "Detected")$this_var, na.rm = T)
  sd_nvr_detected <- sd(subset(eab_school_describe, ever_detected != "Detected")$this_var, na.rm = T)
  
  summary <- data.frame(
    "variable" = d,
    "detected_mean" = round(ttest$estimate[1], digits = 3),
    "detected_sd" = round(sd_detected, digits = 3),
    "nvr_detected_mean" = round(ttest$estimate[2], digits = 3),
    "nvr_detected_sd" = round(sd_nvr_detected, digits = 3),
    "p_val" = ttest$p.value
  )%>%
  mutate(p_val = ifelse(p_val < 0.001, "< 0.001", round(p_val, digits = 3))) %>%
    rbind(summary)
  
}

named_vars <- c(
  "canopy" = "Canopy (mean probability)",
  "lowinc_pct" = "Low-income (%)",
  "ISAT_composite" = "Meet/exceed ISAT (%)",
  "all_tests" = "Meet/exceed all tests (%)",
  "all_attendance rate school pct" = "Attendance rate (%)",
  "chronic truants rate school pct" = "Chronic truants rate (%)",
  "white_pct" = "White (%)",
  "black_pct" = "Black (%)",
  "hispanic_pct" = "Hispanic (%)",
  "asian_pct" = "Asian (%)"
)
summary <- summary %>% 
  mutate(variable = named_vars[variable])

kbl(summary, 
    format = "latex",
    row.names = F,
    col.names = c("Variable", "Mean", "SD", "Mean", "SD", "t-test p-value"), 
    align = c("l", "c", "c", "c", "c", "r"), 
    caption = "Descriptive comparison of schools in the vicinity of confirmed ash borer infestation at some point within the study period versus those never exposed.", 
    label = "descriptive-table",
    booktabs = T)%>%
  kable_styling(latex_options = c("hold_position"))%>%
  add_header_above(c(" " = 1, "Detected" = 2, "Never detected" = 2, " " = 1))%>%
  save_kable(paste0(results_dir, "/descriptive_table.tex"))

