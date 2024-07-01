library(tidyverse)
library(ggplot2)
library(kableExtra)
library(modelsummary)
library(here)
library(ggpubr)
library(grid)
library(Hmisc)

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
  mutate(ever_detected = as.character(ifelse(first_detected > 0, "Detected", "Never detected")))%>%
  mutate_at(vars(place_first_detected), as.numeric)

eab_cross <- eab_panel %>%
  filter(year == 2005)

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#### Raw canopy measures through time
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

eab_panel_plot <- eab_panel %>%
  group_by(year, ever_detected)%>%
  summarise(canopy = mean(canopy, na.rm = T),
            loss = mean(loss, na.rm = T),
            gain = mean(gain, na.rm = T))

trends_loss <- ggplot(eab_panel_plot, aes(y = loss, x = year, color = ever_detected))+
  geom_vline(xintercept = 2006 - 0.25, linetype = "dashed")+
  geom_point()+
  geom_line()+
  scale_color_manual(values = c(palette$red, palette$blue), name = element_blank())+
  scale_x_continuous(breaks = seq(2000, 2012, 3), name = "Year")+
  scale_y_continuous(breaks = seq(0, 16, 4), name = "Tree loss (Ha/Year)")+
  theme_classic()

trends_canopy <- ggplot(eab_panel_plot, aes(y = canopy, x = year, color = ever_detected))+
  geom_vline(xintercept = 2006 - 0.25, linetype = "dashed")+
  geom_point()+
  geom_line()+
  scale_color_manual(values = c(palette$red, palette$blue), name = element_blank())+
  scale_x_continuous(breaks = seq(2000, 2012, 3), name = "Year")+
  scale_y_continuous(breaks = seq(0, 100, 20), name = "Canopy (mean probability)")+
  theme_classic()

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#### School unit of analysis
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

eab_panel_school <- readRDS(paste0(clean_data_dir, "/eab_panel_school2mi.rds"))

eab_school_cross <- eab_panel_school %>%
  filter(year == 2005)%>%
  mutate(ever_detected = as.character(ifelse(first_exposed > 0, "Detected", "Never detected")))

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#### Education measures through time
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

eab_panel_school_plot <- eab_panel_school %>%
  mutate(ever_detected = as.character(ifelse(first_exposed > 0, "Detected", "Never detected")))%>%
  mutate(median_lowinc = median(lowinc_pct, na.rm = T),
         above_median_lowinc = as.character(ifelse(lowinc_pct >= median_lowinc, "More low-income", "Less low-income")))%>%
  ungroup %>%
  group_by(year, above_median_lowinc)%>%
  summarise(ISAT_composite = mean(ISAT_composite, na.rm = T),
            all_attend = mean(as.numeric(`all_attendance rate school pct`), na.rm = T))

trends_ISAT <- ggplot(eab_panel_school_plot, aes(y = ISAT_composite, x = year, color = above_median_lowinc))+
  geom_vline(xintercept = 2006 - 0.25, linetype = "dashed")+
  geom_point()+
  geom_line()+
  scale_color_manual(values = c(palette$gold, palette$green), name = "Above/below median low-income:")+
  scale_x_continuous(breaks = seq(2003, 2012, 3), name = "Year")+
  scale_y_continuous(breaks = seq(0, 100, 20), name = "% meet or exceed\nISAT cutoff (composite)")+
  theme_classic()

ggarrange(trends_loss, trends_canopy, trends_ISAT,
          ncol=2, nrow=2, 
          common.legend = TRUE, legend = "right",
          labels = c("A", "B", "C"))

blank <- ggplot() + theme_void()

ggarrange(
  ggarrange(trends_loss, trends_canopy, 
            nrow = 1, widths = c(1, 1), common.legend = T, legend = "bottom",
            labels = c("A", "B")),
  ggarrange(blank, trends_ISAT, blank, 
            nrow = 1, widths = c(0.5, 1, 0.5), legend = "bottom",
            labels = c(" ", "C", " ")),
  nrow = 2, legend = "none"
)
ggsave(paste0(fig_dir, "/raw_trends.png"), width = 9, height = 7)
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
  "ISAT_composite",
  "all_attend",
  "enrollment",
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
  "ever_detected" = "P(detection)",
  "first_exposed" = "Year detected (given detection)",
  "canopy" = "Canopy (mean probability)",
  "lowinc_pct" = "Low-income (%)",
  "ISAT_composite" = "Meet/exceed ISAT (%)",
  "all_tests" = "Meet/exceed all tests (%)",
  "all_attend" = "Attendance rate (%)",
  "enrollment" = "Enrollment",
  "white_pct" = "White (%)",
  "black_pct" = "Black (%)",
  "hispanic_pct" = "Hispanic (%)",
  "asian_pct" = "Asian (%)"
)
summary_detect <- summary %>% 
  mutate(variable = named_vars[variable])

kbl(summary_detect, 
    row.names = F,
    col.names = c("Variable", "Mean", "SD", "Mean", "SD", "t-test p-value"), 
    align = c("l", "c", "c", "c", "c", "r"), 
    caption = "Descriptive comparison of schools in the vicinity of confirmed ash borer infestation at some point within the study period versus those never exposed.", 
    label = "descriptive-table-detect",
    booktabs = T)%>%
  kable_styling(latex_options = c("hold_position"))%>%
  add_header_above(c(" " = 1, "Detected" = 2, "Never detected" = 2, " " = 1))%>%
  save_kable(paste0(results_dir, "/descriptive_table_detect.html"))

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
### Above/below median income table
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
median_lowinc = median(eab_school_cross$lowinc_pct)

eab_school_cross <- eab_school_cross %>%
  mutate(above_med_lowinc = (lowinc_pct >= median_lowinc)*1)

describe_vars <- c(
  "first_exposed",
  "ever_detected",
  "hispanic_pct",
  "asian_pct",
  "black_pct",
  "white_pct",
  "lowinc_pct",
  "all_attend",
  "enrollment",
  "ISAT_composite",
  "canopy"
)

summary <- data.frame()
for(d in describe_vars){
  
  if(d == "first_exposed"){
    eab_school_describe <- eab_school_cross %>%
      filter(ever_detected == "Detected")%>%
      select(above_med_lowinc, d)%>%
      rename(this_var = d)%>%
      mutate(this_var = as.numeric(this_var))
    
  } else{
    
    eab_school_describe <- eab_school_cross %>%
      select(above_med_lowinc, d)%>%
      rename(this_var = d)
    
    if(d == "ever_detected"){
    eab_school_describe <- eab_school_describe %>%
      mutate(this_var = ifelse(this_var == "Detected", 1, 0)
             )
    }
    
  }
  
  ttest <- t.test(this_var~above_med_lowinc, data=eab_school_describe)
  
  above_mean <- mean(subset(eab_school_describe, above_med_lowinc == 1)$this_var, na.rm = T)
  below_mean <- mean(subset(eab_school_describe, above_med_lowinc == 0)$this_var, na.rm = T)
  
  sd_above <- sd(subset(eab_school_describe, above_med_lowinc == 1)$this_var, na.rm = T)
  sd_below <- sd(subset(eab_school_describe, above_med_lowinc == 0)$this_var, na.rm = T)
  
  summary <- data.frame(
    "variable" = d,
    "above_mean" = round(above_mean, digits = 3),
    "above_sd" = round(sd_above, digits = 3),
    "below_mean" = round(below_mean, digits = 3),
    "below_sd" = round(sd_below, digits = 3),
    "p_val" = ttest$p.value
  )%>%
    mutate(p_val = ifelse(p_val < 0.001, "< 0.001", round(p_val, digits = 3))) %>%
    rbind(summary)
  
}

summary_lowinc <- summary %>% 
  mutate(variable = named_vars[variable])

kbl(summary_lowinc, 
    row.names = F,
    col.names = c("Variable", "Mean", "SD", "Mean", "SD", "t-test p-value"), 
    align = c("l", "c", "c", "c", "c", "r"), 
    caption = "Descriptive comparison of schools above and below the median percentage of students classified as low-income (33.4%).", 
    label = "descriptive-table-lowinc",
    booktabs = T
    )%>%
 # kable_styling(latex_options = c("hold_position"))%>%
  add_header_above(c(" " = 1, "More than 33.4% low-income students" = 2, "Less than 33.4% low-income students" = 2, " " = 1))%>%
  save_kable(paste0(results_dir, "/descriptive_table_lowinc.html"))


rename_summary_rows <- function(x){
  
  x <- x %>%
    rename(a_mean = 2,
           a_sd = 3,
           b_mean = 4,
           b_sd = 5)
  
}

summary_combo <- rbind(rename_summary_rows(summary_lowinc), 
                       rename_summary_rows(summary_detect))%>%
  kbl(format = "html",
      row.names = F,
      col.names = c("Variable", "Mean", "SD", "Mean", "SD", "t-test p-value"), 
      align = c("l", "c", "c", "c", "c", "r"), 
      caption = "Descriptive comparison of schools in the vicinity of confirmed ash borer infestation at some point within the study period versus those never exposed.", 
      label = "descriptive-table") %>%
  kable_paper("striped", full_width = F) %>%
  pack_rows("Above median vs. below median low income representation", 1, nrow(summary_lowinc))%>%
  pack_rows("Ever vs. never detected schools", (nrow(summary_lowinc) + 1), nrow(summary_detect), indent = F)

summary_combo #%>%
  #save_kable(paste0(results_dir, "/descriptive_table.png"))

