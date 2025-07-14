
# Before running this script, first run:
# 1. Setup_File-Run_This_First.R <-- This loads in the actual ABM and many functions for handling the data that it produces
# 2. Sensitivity_Analysis.R <-- This runs a parameter sweep, producing model output data from a range of parameter values -- including the parameter values in the main analysis. 

# This script begins by reading in a subset of the data from the parameter sweep in the sensitivity analysis.







#### Main Results ####


#### Load in the Surviving Cohort data ####
survivors_rmr1 <- read_in_living_sweep(mortality_regime = CoaleDemenyWestF5, 
                   lesion_rates = 0.05, 
                   reps = 100,
                   rmr = 1)
survivors_rmr2 <- read_in_living_sweep(mortality_regime = CoaleDemenyWestF5, 
                                       lesion_rates = 0.05, 
                                       reps = 100,
                                       rmr = 2)



#### Load in the Cemetery data ####

#### RISK-FREE SKELETAL LESIONS #### 
risk_free <- read_sweep(mortality_regime = CoaleDemenyWestF5, 
                        lesion_rates = 0.05, 
                        reps = 100,
                        rmr = 1 # lesion-associated (proportional) relative mortality risk (rmr) = 1
                        )
risk_free$rmr <- "Risk-free"


#### RISK-DOUBLING SKELETAL LESIONS #### 
risk_doubled <- read_sweep(mortality_regime = CoaleDemenyWestF5, 
                        lesion_rates = 0.05, 
                        reps = 100,
                        rmr = 2 # lesion-associated (proportional) relative mortality risk (rmr) = 2. 
                        ## The only difference between these simulation scenarios ##
)

risk_doubled$rmr <- "Risk-doubled"



# Match specific ages to relevant age categories and midpoint of each age category (for plotting)
age_lookup <- data.frame(
  Age_Interval = factor(
    c("0-1","2-5","6-9","10-14","15-19","20-29","30-39","40-49","50-59","60+"),
    levels = c("0-1","2-5","6-9","10-14","15-19","20-29","30-39","40-49","50-59","60+")
  ),
  interval_midpoint = c(0.5, 3.5, 7.5, 12.5, 17.5, 25, 35, 45, 55, 75) # 75 because that is halfway between age 60 and age 89, the oldest age at death in this cohort. 
)


# Combine all Cemeteries into a single, plottable, data frame. 
both_worlds <- rbind(risk_free, risk_doubled) %>%
# Create age intervals
  mutate(Age_Interval = factor(case_when(
    Age < 2 ~ "0-1",
    Age >= 2 & Age < 6 ~ "2-5",
    Age >= 6 & Age < 10 ~ "6-9",
    Age >= 10 & Age < 15 ~ "10-14",
    Age >= 15 & Age < 20 ~ "15-19",
    Age >= 20 & Age < 30 ~ "20-29",
    Age >= 30 & Age < 40 ~ "30-39",
    Age >= 40 & Age < 50 ~ "40-49",
    Age >= 50 & Age < 60 ~ "50-59",
    Age >= 60 ~ "60+"
  ), levels = levels(age_lookup$Age_Interval)),
  mortality = factor(mortality, levels = c("CDW3", "CDW5", "CDW11", "CDW15", "CDW17", "CDW21"))) %>%
  mutate(mortality = recode(mortality,
                            "CDW3" = "CDW Level 3",
                            "CDW5" = "CDW Level 5",
                            "CDW11" = "CDW Level 11",
                            "CDW15" = "CDW Level 15",
                            "CDW17" = "CDW Level 17",
                            "CDW21" = "CDW Level 21"
  ),
  rmr = as.factor(rmr))







#### PLOTS ####

#### Plot Age-at-Death Distribution for both scenarios ####

# set plot colors
plot_colors <- c('magenta', "black")

# Bar plot (more familiar for archaeologists)
barplot_sample_age_distributions <- both_worlds %>%
  group_by(rmr, Age_Interval) %>%
  summarise(prop = n() / 100000, .groups = "keep") %>%
ggplot(aes(x = Age_Interval, y = prop)) +
  geom_col(aes(group = rmr, fill = rmr), position = "dodge") +
  scale_fill_manual(values = plot_colors) +
  scale_y_continuous(labels = scales::percent, limits = c(0,.3), expand = c(0,0) ) +
  labs(x = "Age at Death (years)", y = "% of Cemetery", fill = "Lesion-associated Mortality Risk") +
  theme_bw() +
  theme(legend.position = "bottom") +
  ggtitle("Age-at-Death Distributions")

legend <- get_legend(barplot_sample_age_distributions)

barplot_sample_age_distributions <- barplot_sample_age_distributions + 
  theme(legend.position = "none")


#### Plot skeletal lesion frequency in both scenarios ####

# convert individual-level cemetery data to % of individuals with skeletal lesions in each age category
plot_data <- lesions_to_percents(both_worlds, group_vars = c("rmr", "Age_Interval", "rep"))
plot_data_pooled <- lesions_to_percents(both_worlds, group_vars = c("rmr", "Age_Interval"))
#plot_data$Status <- "Dead"
#plot_data_pooled$Status <- "Dead"


barplot_lesion_frequency_comparison <- ggplot(plot_data_pooled, aes(x = Age_Interval, y = Lesion_Percent)) +
  geom_col(aes(group = rmr, fill = rmr), position = "dodge") +
  scale_fill_manual(values = plot_colors) +
  scale_y_continuous(limits = c(0,60), expand = c(0,0) ) +
  labs(x = "Age at Death (years)", y = "% with Skeletal Lesion") +
  theme_bw() +
  theme(legend.position = "none") +
  ggtitle("Age Distributions of Skeletal Lesions in Cemeteries")


barplots <- plot_grid(barplot_sample_age_distributions, barplot_lesion_frequency_comparison, 
                      ncol = 1, labels = "AUTO")


ggsave(filename = "./Known Unknowns/Figures/Age_and_Lesion_frequency_distribution_barplots.pdf", 
  plot = plot_grid(barplots, legend, ncol = 1, rel_heights = c(1, 0.05)), 
  width = 5,
  height = 7,
  units = "in",
  dpi = 1200)


# make plot
output_plot <- ggplot(plot_data, aes(x = interval_midpoint, y = Lesion_Percent)) +
  geom_line(aes(group = interaction(rmr, rep), color = rmr), alpha = 0.2) +
  geom_line(data = plot_data_pooled, aes(x = interval_midpoint, y = Lesion_Percent,
                                         group = rmr, color = rmr), linewidth = 1.5) +
  scale_color_manual(values = plot_colors) +
  labs(x = "Age at Death (years)", y = "Percent with Skeletal Lesions", color = "Lesion-related Risk") +
  theme_bw() +
  theme(legend.position = "bottom") +
  ggtitle("Lesion Frequency in the Cemeteries")

# save plot.
ggsave(filename = "./Known Unknowns/Figures/Figure_3_Lesion_frequency_lineplot.pdf", 
       plot = output_plot,
       width = 7,
       height = 5,
       units = "in",
       dpi = 1200)
                                  



# plot lesion frequency in survivors
survivors_rmr1$rmr <- "Risk-free"
survivors_rmr2$rmr <- "Risk-doubled"
survivors <- rbind(survivors_rmr1, survivors_rmr2)
survivors$rmr <- factor(survivors$rmr, levels = c("Risk-free", "Risk-doubled"))
survivors$Status <- "Alive"
plot_data$Status <-"Dead"

survivors_pooled <- survivors %>%
  group_by(Age, rmr) %>%
  summarise(Lesion_Percent = mean(Lesion_perc), .groups = "keep")


# Cemetery and survivors: Risk-free Lesions
linetypes = c("dashed", "solid")
over_and_underworld_plot1 <- ggplot(survivors %>% filter(rmr == "Risk-free"),
                                    aes(x = Age, y = Lesion_perc)) +
  geom_line(aes(group = interaction(rmr, rep), linetype = Status), color = "black", alpha = 0.1) +
  geom_line(data = plot_data %>% filter(rmr == "Risk-free")
            , aes(x = interval_midpoint, y = Lesion_Percent, 
                      group = interaction(rmr, rep), linetype = Status), , color = "black", alpha = 0.1) +
  geom_line(data = plot_data_pooled %>% filter(rmr == "Risk-free")
            , aes(x = interval_midpoint, y = Lesion_Percent,
                                         group = rmr), color = "black", linewidth = 1.5) +
  geom_line(data = survivors_pooled %>% filter(rmr == "Risk-free"),
            aes(x = Age, y = Lesion_Percent,
                                         group = rmr), color = "black", linetype = "dashed", linewidth = 1.5) +
  scale_linetype_manual(values = linetypes) +
  labs(linetype = " ", y = "% of individuals with skeletal lesions", x = "Age (years)") +
  theme_bw() +
  guides(linetype = guide_legend(override.aes = list(alpha = 1))) +
  theme(legend.position = "bottom") +
  ggtitle("Risk-free Lesions") +
  scale_y_continuous(limits = c(0,85), breaks = c(0, 25, 50, 75))

# Cemetery and survivors: Risk-doubling Lesions
over_and_underworld_plot2 <- ggplot(survivors %>% filter(rmr == "Risk-doubled"),
                                    aes(x = Age, y = Lesion_perc)) +
  geom_line(aes(group = interaction(rmr, rep), linetype = Status), color = "magenta", alpha = 0.1) +
  geom_line(data = plot_data %>% filter(rmr == "Risk-doubled")
            , aes(x = interval_midpoint, y = Lesion_Percent, 
                  group = interaction(rmr, rep), linetype = Status), , color = "magenta", alpha = 0.1) +
  geom_line(data = plot_data_pooled %>% filter(rmr == "Risk-doubled")
            , aes(x = interval_midpoint, y = Lesion_Percent,
                  group = rmr), color = "magenta", linewidth = 1.5) +
  geom_line(data = survivors_pooled %>% filter(rmr == "Risk-doubled"),
            aes(x = Age, y = Lesion_Percent,
                group = rmr), color = "magenta", linetype = "dashed", linewidth = 1.5) +
  scale_linetype_manual(values = linetypes) +
  labs(linetype = " ", y = "% of individuals with skeletal lesions", x = "Age (years)") +
  theme_bw() +
  guides(linetype = guide_legend(override.aes = list(alpha = 1))) +
  theme(legend.position = "bottom") +
  ggtitle("Risk-doubling Lesions") +
  scale_y_continuous(limits = c(0,85), breaks = c(0, 25, 50, 75))


over_and_underworld_plots <- plot_grid(over_and_underworld_plot1, over_and_underworld_plot2,
          ncol = 2, labels = "AUTO")


ggsave(filename = "./Known Unknowns/Figures/Overworld_Underworld_Lesion_frequency_lineplot.pdf", 
       plot = over_and_underworld_plots,
       width = 7,
       height = 5,
       units = "in",
       dpi = 1200)


over_under <- plot_grid(over_and_underworld_plots, 
          output_plot,
          ncol = 1,
          labels = c('', "C"))

ggsave(filename = "./Known Unknowns/Figures/Supplemental Figures/Compound_overworld_underworld.pdf", 
       plot = over_under,
       width = 7,
       height = 7.75,
       units = "in",
       dpi = 1200)



### Same plots as above, but with truncated x-axis to avoid over-focusing on the stochasticity in the upper age ranges, for Supplement. 
over_and_underworld_plots <- plot_grid(over_and_underworld_plot1 +
                                         scale_x_continuous(limits = c(0,75), expand = c(0,0)), 
                                       over_and_underworld_plot2 +
                                         scale_x_continuous(limits = c(0,75), expand = c(0,0)),
                                       ncol = 2, labels = "AUTO")



over_under <- plot_grid(over_and_underworld_plots, 
                        output_plot +
                          scale_x_continuous(limits = c(0,75), expand = c(0,0)),
                        ncol = 1,
                        labels = c('', "C"))

ggsave(filename = "./Known Unknowns/Figures/Figure_3_Compound_overworld_underworld.pdf", 
       plot = over_under,
       width = 7,
       height = 7.75,
       units = "in",
       dpi = 1200)





###################################################################################################
#### Chi-Square Test ####

#### How often does each scenario return a significant Chi-square test? ####
chi_risk_free <- run_chi_test(risk_free)
chi_risk_free$rmr <- 1
chi_risk_doubled <- run_chi_test(risk_doubled)
chi_risk_doubled$rmr <- 2

chi_both_worlds <- rbind(chi_risk_free, chi_risk_doubled) 

chi_both_worlds %>%
  group_by(rmr) %>%
  summarise(sig_chi_percent = sum(p.value < 0.05))
# both scenarios produce only cemetery data sets with significant chi-square tests


ggplot(chi_both_worlds, aes(x = chi.sq)) +
  geom_density(aes(group = rmr, fill = as.factor(rmr)), alpha = 0.6) +
  geom_vline(xintercept = 3.841, linetype = 5, linewidth = 1)




#################################################################################################

#### Survival Curves #### 

# plot survival curves from a single run of each scenario
# What % of runs return a significant log-rank test?

survival_data1 <- extract_survival_data(risk_free %>% filter(rep == 1))
survival_data2 <- extract_survival_data(risk_doubled %>% filter(rep == 1))

rmr1_survival_curve <- ggplot(survival_data1, aes(x = time, y = survival)) +
  geom_line(aes(group = group, color = group), linewidth = 1) +
  scale_color_manual(values = c('black', 'grey'), labels = c("Lesion Absent", "Lesion Present")) +
    labs(color = "", x = "Age (years)", y = "Proportion of group surviving") +
    theme_bw() +
  theme(legend.position = "top") +
  ggtitle(label = "Risk-Free Skeletal Lesions: All Ages",
          subtitle = "Includes ages within the lesion's developmental window")


rmr2_survival_curve <- ggplot(survival_data2, aes(x = time, y = survival)) +
  geom_line(aes(group = group, color = group), linewidth = 1) +
  scale_color_manual(values = c('#AA0144', '#FF64DC'), labels = c("Lesion Absent", "Lesion Present")) +
  labs(color = "", x = "Age (years)", y = "Proportion of group surviving") +
  theme_bw() +
  theme(legend.position = "top") +
  ggtitle(label = "Risk-Doubling Skeletal Lesions: All Ages",
                   subtitle = "Includes ages within the lesion's developmental window") 



survival_curve_comparison <- plot_grid(rmr1_survival_curve, rmr2_survival_curve, 
          ncol = 1,
          labels = "AUTO")

ggsave(filename = "./Known Unknowns/Figures/Survival_curve_comparison_All-Ages.pdf", 
       plot = survival_curve_comparison,
       width = 4,
       height = 6,
       units = "in",
       dpi = 1200)



# Survival curves, adults only:
survival_adults1 <- extract_survival_data(risk_free %>% filter(rep == 1, Age >= 10))
survival_adults2 <- extract_survival_data(risk_doubled %>% filter(rep == 1, Age >= 10))

rmr1_adults_curve <- ggplot(survival_adults1, aes(x = time, y = survival)) +
  geom_line(aes(group = group, color = group), linewidth = 1) +
  scale_color_manual(values = c('black', 'grey'), labels = c("Lesion Absent", "Lesion Present")) +
  labs(color = "", x = "Age (years)", y = "Proportion of group surviving") +
  theme_bw() +
  theme(legend.position = "top") +
  ggtitle(label = "Risk-Free Skeletal Lesions: Ages 10+",
          subtitle = "Excludes ages within the lesion's developmental window")

rmr2_adults_curve <- ggplot(survival_adults2, aes(x = time, y = survival)) +
  geom_line(aes(group = group, color = group), linewidth = 1) +
  scale_color_manual(values = c('#AA0144', '#FF64DC'), labels = c("Lesion Absent", "Lesion Present")) +
  labs(color = "", x = "Age (years)", y = "Proportion of group surviving") +
  theme_bw() +
  theme(legend.position = "top") +
  ggtitle(label = "Risk-Doubling Skeletal Lesions: Ages 10+",
          subtitle = "Excludes ages within the lesion's developmental window") 


ggsave(filename = "./Known Unknowns/Figures/survival_curves_10+.pdf",
  plot = plot_grid(rmr1_adults_curve, rmr2_adults_curve, 
          ncol = 1,
          labels = "AUTO"),
  width = 4,
  height = 6,
  units = "in",
  dpi = 1200)
  
legend1 <- get_legend(rmr1_survival_curve)
legend2 <- get_legend(rmr2_survival_curve)
risk_free_survival <- plot_grid(rmr1_survival_curve + theme(legend.position = "none"),
            rmr1_adults_curve + theme(legend.position = "none"), 
            ncol = 2, labels = "AUTO")
risk_free_survival <- plot_grid(risk_free_survival, legend1, ncol = 1,
                                rel_heights = c(1, 0.1))
risk_2x_survival <- plot_grid(rmr2_survival_curve + theme(legend.position = "none"),
                                rmr2_adults_curve + theme(legend.position = "none"), 
                                ncol = 2, labels = c("C", "D"))
risk_2x_survival <- plot_grid(risk_2x_survival, legend2, ncol = 1,
                                rel_heights = c(1, 0.1))

ggsave(filename = "./Known Unknowns/Figures/survival_curves_age_comparison.pdf",
  plot = plot_grid(risk_free_survival,
          risk_2x_survival, ncol = 1),
  width = 9,
  height = 7, 
  units = "in", 
  dpi = 1200)





#### Log-rank Test ####
# The code below generates Table 1: Median survival time, 95% CI,and % of each scenario's 100 runs that produce data that return a significant log-rank test. 

logrank1_main <- survival_sweep_rmr1$logrank_results %>%
  filter(mortality == "CDW5", lesion_formation_rate == 0.05) %>%
  mutate(significant = if_else(p_value < 0.05, 1, 0)) %>%
  summarise(percent_significant = sum(significant, na.rm = T))
logrank2_main <- survival_sweep_rmr2$logrank_results %>%
  filter(mortality == "CDW5", lesion_formation_rate == 0.05) %>%
  mutate(significant = if_else(p_value < 0.05, 1, 0)) %>%
  summarise(percent_significant = sum(significant, na.rm = T))


survival_rmr1_adults <- run_survival_analysis(risk_free %>% filter(Age >= 15))
logrank1_adults <- survival_rmr1_adults$logrank_results %>%
  mutate(significant = if_else(p_value < 0.05, 1, 0)) %>%
  summarise(percent_significant = sum(significant, na.rm = T))
survival_rmr2_adults <- run_survival_analysis(risk_doubled %>% filter(Age >= 15))
logrank2_adults <- survival_rmr2_adults$logrank_results %>%
  mutate(significant = if_else(p_value < 0.05, 1, 0)) %>%
  summarise(percent_significant = sum(significant, na.rm = T))




# median survival time and 95% CI
median_survival_rmr1 <- risk_free %>%
  group_by(rep, Lesion) %>%
  summarise(median_survival_time = median(Age), .groups = "keep") %>%
  ungroup() %>%
  group_by(Lesion) %>%
  summarise(Scenario = "Risk-free",
            Ages = "All Ages",
            Median = median(median_survival_time),
            lower_CI = round(quantile(median_survival_time, probs = 0.025), 1),
            upper_CI = round(quantile(median_survival_time, probs = 0.0975), 1)
  )

median_adults_rmr1 <- risk_free %>%
  filter(Age >=15) %>%
  group_by(rep, Lesion) %>%
  summarise(median_survival_time = median(Age), .groups = "keep") %>%
  ungroup() %>%
  group_by(Lesion) %>%
  summarise(Scenario = "Risk-free",
            Ages = "15+",
            Median = median(median_survival_time),
            lower_CI = round(quantile(median_survival_time, probs = 0.025), 1),
            upper_CI = round(quantile(median_survival_time, probs = 0.0975), 1)
  )

median_survival_rmr2 <- risk_doubled %>%
  group_by(rep, Lesion) %>%
  summarise(median_survival_time = median(Age), .groups = "keep") %>%
  ungroup() %>%
  group_by(Lesion) %>%
  summarise(Scenario = "Risk-doubled",
            Ages = "All Ages",
            Median = median(median_survival_time),
            lower_CI = round(quantile(median_survival_time, probs = 0.025), 1),
            upper_CI = round(quantile(median_survival_time, probs = 0.0975), 1)
  )

median_adults_rmr2 <- risk_doubled %>%
  filter(Age >=15) %>%
  group_by(rep, Lesion) %>%
  summarise(median_survival_time = median(Age), .groups = "keep") %>%
  ungroup() %>%
  group_by(Lesion) %>%
  summarise(Scenario = "Risk-doubled",
            Ages = "15+",
            Median = median(median_survival_time),
            lower_CI = round(quantile(median_survival_time, probs = 0.025), 1),
            upper_CI = round(quantile(median_survival_time, probs = 0.0975), 1)
  )

median_list <- list(median_survival_rmr1, 
                    median_survival_rmr2, 
                    median_adults_rmr1,
                    median_adults_rmr2)


survival_time_table <- rbindlist(median_list) %>%
  mutate(`Skeletal Lesion` = if_else(Lesion == 1, "Present", "Absent"),
         `Median Survival Time (95% CI)` = paste(Median, " (", lower_CI, " ,", upper_CI, ")")) %>%
  select(Scenario, Ages, `Skeletal Lesion`, `Median Survival Time (95% CI)`)
write.csv(survival_time_table,
          file = "./Known Unknowns/Tables/Median_survival_times.csv")







