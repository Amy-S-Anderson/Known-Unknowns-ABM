

#### Known Unknowns: Sensitivity Analysis ####

#### This script 
# 1. runs a parameter sweep: Sweeping across lesion exposure rate (1-10% annually in each of the first ten years) and mortality regime (Coale and Demeny West model life tables), for (1) risk-free lesions and (2) risk-doubling lesions
# 2. plots the results of the sweep in a series of multi-panel line plots
# 3. 



### Probability of exposure to lesion-causing conditions (% annual probability of exposure in each of the first ten years) ###
# - 1, 2, 3, 4, 5, 6, 7, 8, 9, 10.

### Mortality curve (Coale & Demeny West model life tables for females) ###
# - CDW3, 5, 7, 9, 11, 13, 15






#### FUNCTIONS ####

# Function to generate root output directory name of folder in which to store data produced from a single set of model parameter values.
generate_output_directory <- function(params) {
  mortality_regime_number <- gregexpr("[0-9]+", params[["mortality_regime"]]$name)
  mortality_regime_abbrev <- paste0("CDW", regmatches(params[["mortality_regime"]]$name, mortality_regime_number))
  paste0("./Known Unknowns/Output/RMR=", params[["relative_mortality_risk"]], "/", mortality_regime_abbrev)
}



#### SWEEP FUNCTIONS ####

run_sweep <- function(mortality_regime, lesion_rates, reps, rmr) {
  params <- get_default_params()
  params[["mortality_regime"]] <- mortality_regime
  params[["relative_mortality_risk"]] <- rmr
  
  out_dir <- generate_output_directory(params)
  
  run_model_sweep(
    base_params = params,
    root_output_directory = out_dir,
    target_param = "lesion_formation_rate",
    target_param_values = lesion_rates,
    numreps = reps
  )
}


read_sweep <- function(mortality_regime, lesion_rates, reps, rmr) {
  params <- get_default_params()
  params[["mortality_regime"]] <- mortality_regime
  params[["relative_mortality_risk"]] <- rmr
  
  out_dir <- generate_output_directory(params)
  sweep_data <- read_model_sweep(
    root_output_directory = out_dir,
    target_param = "lesion_formation_rate",
    target_param_values = lesion_rates
  )
  
  sweep_df <- data_to_data_frame(sweep_data, target_param_values = lesion_rates)
  
  # Annotate with mortality regime name for downstream grouping
  sweep_df$mortality <- paste0("CDW", regmatches(mortality_regime$name, gregexpr("[0-9]+", mortality_regime$name))[[1]])
  
  return(sweep_df)
}




library(dplyr)
#### PLOTTING FUNCTIONS ####
# Make a line plot for this lesion_exposure parameter sweep:
sweep_lineplot <- function(plot_data, plot_color){
  ggplot(data = plot_data, aes(x = interval_midpoint, y = Lesion_Percent)) +
    geom_line(aes(group = rep), color = plot_color, alpha = 0.05) +
    facet_grid(
      rows = vars(mortality), 
      cols = vars(lesion_formation_rate), 
      labeller = labeller(
        lesion_formation_rate = function(x) paste("Exposure Rate:", x)
      )
    ) +
    labs(
      title = "Lesion Percent by Age, Across Mortality Regimes and Exposure Rates",
      x = "Age (years)",
      y = "Percent with Lesions",
      caption = "Each line = one simulation replicate"
    ) +
    theme_bw() +
    theme(
      strip.text = element_text(size = 8.5),
      plot.title = element_text(hjust = 0.5),
      axis.text.x = element_text(angle = 45, hjust = 1)
    )
}



# Plot AVERAGE OUTCOMES for each combination scenario of lesion exposure and mortality
scenario_lineplot <- function(plot_summary_data){
  ggplot(data = plot_summary_data, aes(x = Age_Interval, y = Lesion_Percent)) +
    geom_line(aes( group = lesion_formation_rate,
                   color = as.factor(lesion_formation_rate))) +
    facet_wrap(~ mortality, ncol = 2) +  # One panel per mortality regime
    labs(
      title = "Lesion Percent by Age, Colored by Exposure Rate",
      x = "Age (years)",
      y = "Percent with Lesions",
      color = "Exposure Rate",
      caption = "Each line = one simulation replicate"
    ) +
    theme_bw() +
    theme(
      strip.text = element_text(size = 10),
      plot.title = element_text(hjust = 0.5),
      axis.text.x = element_text(angle = 45, hjust = 1)
    ) +
    guides(colour = guide_legend(override.aes = list(alpha = 1)))
  
}





#### SETUP ####

# Define default parameters
params <- get_default_params()

# Sweep configuration
reps <- 100
lesion_exposure_rates <- c(0.01, 0.02, 0.03, 0.04, 0.05, 0.06, 0.07, 0.08, 0.09, 0.1)
mortality_regimes <- list(
  CoaleDemenyWestF3,
  CoaleDemenyWestF5,
  CoaleDemenyWestF11,
  CoaleDemenyWestF15,
  CoaleDemenyWestF17,
  CoaleDemenyWestF21
)



 


 # #### RUN AND SAVE SWEEPS ####
## These loops will take some time to execute. 

 for (regime in mortality_regimes) {
   regime_name <- regime$name
   sweep_results2[[regime_name]] <- run_sweep(
     mortality_regime = regime,
     lesion_rates = lesion_exposure_rates,
     reps = reps,
     rmr = 1
   )
 }

 for (regime in mortality_regimes) {
   regime_name <- regime$name
   sweep_results2[[regime_name]] <- run_sweep(
     mortality_regime = regime,
     lesion_rates = lesion_exposure_rates,
     reps = reps,
     rmr = 2
   )
 }

 

 
#### READ IN SWEEPS DATA ####
# Master lists to hold results
sweep_results <- list()
sweep_results2 <- list()

# Running these loops will take several minutes. 
# RMR = 1
for (regime in mortality_regimes) {
  regime_name <- regime$name
  sweep_results[[regime_name]] <- read_sweep(
    mortality_regime = regime,
    lesion_rates = lesion_exposure_rates,
    reps = reps,
    rmr = 1
  )
}

# RMR = 2
for (regime in mortality_regimes) {
  regime_name <- regime$name
  sweep_results2[[regime_name]] <- read_sweep(
    mortality_regime = regime,
    lesion_rates = lesion_exposure_rates,
    reps = reps,
    rmr = 2
  )
}




#### COMBINE & PLOT ####

# Combine all results into a single data frame
sweep_data1 <- do.call(rbind, sweep_results)
sweep_data1$rmr <- 1

sweep_data2 <- do.call(rbind, sweep_results2)
sweep_data2$rmr <- 2

all_data <- rbind(sweep_data1, sweep_data2)



# Convert to summary data
plot_data1 <- lesions_to_percents(sweep_data1, group_vars = c("mortality", "lesion_formation_rate", "rep"))
plot_data1$rmr <- 1
plot_data2 <- lesions_to_percents(sweep_data2, group_vars = c("mortality", "lesion_formation_rate", "rep"))
plot_data2$rmr <- 2





# Generate plots

full_lineplot1 <- sweep_lineplot(plot_data1, "black")
full_lineplot2 <- sweep_lineplot(plot_data2, "magenta")

# combined plot (same as above, just superimposed on the same grid)

all_lineplot <- ggplot(data = plot_data1, aes(x = interval_midpoint, y = Lesion_Percent)) +
    geom_line(aes(group = rep), color = 'black', alpha = 0.05) +
    geom_line(data = plot_data2, aes(group = rep), color = 'magenta', alpha = 0.05) +
    facet_grid(
      rows = vars(mortality), 
      cols = vars(lesion_formation_rate), 
      labeller = labeller(
        lesion_formation_rate = function(x) paste("Exposure Rate:", x)
      )
    ) +
    labs(
      title = "Lesion Percent by Age, Across Mortality Regimes and Exposure Rates",
      x = "Age (years)",
      y = "Percent with Lesions",
      caption = "Each line = one simulation replicate"
    ) +
    theme_bw() +
    theme(
      strip.text = element_text(size = 8.5),
      plot.title = element_text(hjust = 0.5),
      axis.text.x = element_text(angle = 45, hjust = 1)
    )






# image size: full page, landscape, with 1/2 inch for image caption text
ggsave(filename = "./Known Unknowns/Figures/Supplemental Figures/sensitivity_sweep_all_runs_rmr1.pdf", 
       plot = full_lineplot1,
       width = 13.5,
       height = 8,
       units = "in",
       dpi = 1200)

ggsave(filename = "./Known Unknowns/Figures/Supplemental Figures/sensitivity_sweep_all_runs_rmr2.pdf", 
       plot = full_lineplot2,
       width = 13.5,
       height = 8,
       units = "in",
       dpi = 1200)

ggsave(filename = "./Known Unknowns/Figures/Supplemental Figures/sensitivity_sweep_all_runs_all.pdf", 
       plot = all_lineplot,
       width = 13.5,
       height = 8,
       units = "in",
       dpi = 1200)




lineplot1 <- ggplot(data = plot_data1, aes(x = interval_midpoint, y = Lesion_Percent)) +
  geom_line(aes(group = interaction(rep, lesion_formation_rate), 
                color = as.factor(lesion_formation_rate)), 
            alpha = 0.05) +
  facet_wrap(~ mortality, ncol = 2) +  # One panel per mortality regime
  labs(
    title = "Lesion Percent by Age, Colored by Exposure Rate",
    x = "Age (years)",
    y = "Percent with Lesions",
    color = "Exposure Rate"
  ) +
  theme_bw() +
  theme(
    strip.text = element_text(size = 10),
    plot.title = element_text(hjust = 0.5),
    axis.text.x = element_text(angle = 45, hjust = 1)
  ) +
  guides(colour = guide_legend(override.aes = list(alpha = 1))) +
  theme(legend.position = "none")



lineplot2 <- ggplot(data = plot_data2, aes(x = interval_midpoint, y = Lesion_Percent)) +
  geom_line(aes(group = interaction(rep, lesion_formation_rate), 
                color = as.factor(lesion_formation_rate)), 
            alpha = 0.05) +
  facet_wrap(~ mortality, ncol = 2) +  # One panel per mortality regime
  labs(
    x = "Age (years)",
    y = "Percent with Lesions",
    color = "Exposure Rate",
    caption = "Each line = one simulation replicate"
  ) +
  theme_bw() +
  theme(
    strip.text = element_text(size = 10),
    plot.title = element_text(hjust = 0.5),
    axis.text.x = element_text(angle = 45, hjust = 1)
  ) +
  guides(colour = guide_legend(override.aes = list(alpha = 1))) +
  theme(legend.position = "bottom")

legend <- get_legend(lineplot2)
lineplot2 <- lineplot2 +
  theme(legend.position = "none")



ggsave(filename = "./Known Unknowns/Figures/Supplemental Figures/sensitivity_sweep_lesion_rate_by_mortality_regime.pdf", 
       plot = plot_grid(lineplot1,
                        lineplot2,
                        legend,
                        ncol = 1,
                        rel_heights = c(1,1, 0.1),
                        labels = c("A", "B")),
       width = 7,
       height = 10,
       units = "in",
       dpi = 1200)


############################################################################################################

# Summary tables of Mortality Schedules

life_expectancies <- sweep_data1 %>%
  group_by(mortality) %>%
  mutate(life_expectancy_at_birth = round(mean(Age), 1),
         adult = if_else(Age > 15, 1, 0),
         adult_life_expectancy = round(mean()))
  
  
e0 = sweep_data1 %>%
  group_by(mortality) %>%
  summarise(life_expectancy_at_birth = round(mean(Age),1))
juvenile_mortality = sweep_data1 %>%
  group_by(mortality) %>%
  summarise(juvenile_mortality = round(sum(Age < 15) / n() * 100, 1))
# Adult life expectancy (mean adult age at death)
mean_adult_age_at_death = sweep_data1 %>%
  filter(Age >= 15) %>%
  group_by(mortality) %>%
  summarise(adult_life_expectancy = round(mean(Age),1))


life_expectancy = merge(e0, juvenile_mortality) %>%
  left_join(mean_adult_age_at_death)
life_expectancy$mortality <- factor(life_expectancy$mortality, levels = c("CDW3", "CDW5", "CDW11", "CDW15", "CDW17", "CDW21"))
life_expectancy <- life_expectancy %>%
  arrange(mortality)
  
names(life_expectancy) <- c("Mortality regime", "Life expectancy at birth", "Juvenile mortality (%)", "Mean adult age at death")


write.csv(life_expectancy, file = "./Known Unknowns/Tables/CDW_life_expectancies_table.csv")


############################################################################################################



#### CHI SQUARE TEST ####

run_chi_test <- function(sweep_data){
# Initialize an empty dataframe to store all chi-square results
chi_results <- data.frame()

# Add 'survivors' columns to 'all_output'
sweep_data <- sweep_data %>%
  mutate(
    survivors15 = ifelse(Age < 15, "Nonadults", "Adults")
  )

# Loop over each mortality regime
for (regime_name in unique(sweep_data$mortality)) {
  
  # Loop over each sample size
  for (rate in unique(sweep_data$lesion_formation_rate)) {
    
    # Filter data for the current regime and sample size
    scenario_data <- sweep_data %>%
      filter(mortality == regime_name, lesion_formation_rate == rate)
    
    # Loop over each replication within the current regime and sample size
    for (rep in unique(scenario_data$rep)) {
      
      d <- scenario_data %>% filter(rep == !!rep)
      
      if (nrow(d) == 0) {
        warning(paste("No data for regime:", regime_name, "rate:", rate, "rep:", rep))
        next
      }
      
      # Initialize row with NAs
      d_results <- data.frame(
        chi.sq = NA, p.value = NA,
        nonadults_15 = NA, adults_15 = NA,
        rep = rep,
        lesion_formation_rate = rate,
        mortality = regime_name,
        rmr = unique(sweep_data$rmr)
      )
      
      lesion_table <- table(d$survivors15, d$Lesion)
      
      if (all(lesion_table > 5)) {
        chi_test <- chisq.test(lesion_table)
        d_results$chi.sq <- chi_test$statistic
        d_results$p.value <- chi_test$p.value
        prop_table <- prop.table(lesion_table, margin = 1)
        
        # Only assign proportions if labels are present
        if (all(c("Nonadults", "Adults") %in% rownames(prop_table)) &&
            "1" %in% colnames(prop_table)) {
          d_results$nonadults_15 <- prop_table["Nonadults", "1"] * 100
          d_results$adults_15 <- prop_table["Adults", "1"] * 100
        }
      }
      # Append the results of the current replication to the overall results dataframe
      chi_results <- bind_rows(chi_results, d_results)
    }
  }
}
return(chi_results)
}




# run chi.sq on cemeteries
chi.sq_rmr1 <- run_chi_test(sweep_data1)
chi.sq_rmr1$rmr <- as.factor(1)

chi.sq_rmr2 <- run_chi_test(sweep_data2)
chi.sq_rmr2$rmr <- as.factor(2)


# Plot the number of model runs that produce sufficient data to run a chi.sq test
chi.sq1_summary <- chi.sq_rmr1 %>%
  group_by(mortality, lesion_formation_rate) %>%
  summarise(na_chi = sum(!is.na(chi.sq))) %>%
  mutate(mortality = factor(mortality, levels = c("CDW3", "CDW5", "CDW11", "CDW15", "CDW17", "CDW21")))

chi_sensitivity <- ggplot(chi.sq1_summary, aes(x = lesion_formation_rate, y = na_chi)) +
  geom_line(aes(color = mortality)) +
  scale_x_continuous(breaks = c(0.01, 0.02, 0.03, 0.04, 0.05, 0.06, 0.07, 0.08, 0.09, 0.1)) +
  labs(x = "Exposure rate", y = "Model Runs with Sufficient Data for Chi Square", color = "Mortality Schedule",
       title = "Risk-free Lesions") +
  theme_bw() +
  theme(legend.position = "bottom")

legend <- get_legend(chi_sensitivity)

chi_sensitivity <- chi_sensitivity +
  theme(legend.position = "none")

chi.sq2_summary <- chi.sq_rmr2 %>%
  group_by(mortality, lesion_formation_rate) %>%
  summarise(na_chi = sum(!is.na(chi.sq))) %>%
  mutate(mortality = factor(mortality, levels = c("CDW3", "CDW5", "CDW11", "CDW15", "CDW17", "CDW21")))

chi_sensitivity_2 <- ggplot(chi.sq2_summary, aes(x = lesion_formation_rate, y = na_chi)) +
  geom_line(aes(color = mortality)) +
  scale_x_continuous(breaks = c(0.01, 0.02, 0.03, 0.04, 0.05, 0.06, 0.07, 0.08, 0.09, 0.1)) +
  labs(x = "Exposure rate", y = "Model Runs with Sufficient Data for Chi Square", color = "Mortality Schedule",
       title = "Risk-doubling Lesions") +
  theme_bw() +
  theme(legend.position = "none")

sensitivity_plots <- plot_grid(chi_sensitivity, chi_sensitivity_2, ncol = 2, labels = "AUTO")


ggsave(filename = "./Known Unknowns/Figures/Supplemental Figures/chi_square_data_sufficiency.pdf", 
       plot = plot_grid(sensitivity_plots, legend, ncol = 1, rel_heights = c(1, 0.1)),
       width = 7,
       height = 6,
       units = "in",
       dpi = 1200)


# Plot the % of model runs with enough data to run a chi.sq test that return significant test results. 
chi.sq_all <- rbind(chi.sq_rmr1, chi.sq_rmr2) %>%
  mutate(mortality = factor(mortality, levels = c("CDW3", "CDW5", "CDW11", "CDW15", "CDW17", "CDW21")),
         mortality = recode(mortality,
                            "CDW3" = "CDW Level 3",
                            "CDW5" = "CDW Level 5",
                            "CDW11" = "CDW Level 11",
                            "CDW15" = "CDW Level 15",
                            "CDW17" = "CDW Level 17",
                            "CDW21" = "CDW Level 21"))


chi_plot_data <- chi.sq_all %>%
  mutate(chi_sig = if_else(p.value < 0.05, 1, 0)) %>%
  group_by(mortality, rmr, lesion_formation_rate) %>%
  summarise(chi_runs = sum(!is.na(chi.sq)),
            chi_sig_percent = if_else(chi_runs > 0, (sum(chi_sig, na.rm = T) / chi_runs) * 100, NA), .groups = "keep")
            

chi_plot_data <- chi.sq_all %>%
  filter(!is.na(chi.sq)) %>%
  mutate(chi_sig = if_else(p.value < 0.05, 1, 0)) %>%
  group_by(mortality, rmr, lesion_formation_rate) %>%
  summarise(chi_runs = sum(!is.na(chi.sq)),
            chi_sig_percent = if_else(chi_runs > 0, (sum(chi_sig, na.rm = T) / chi_runs) * 100, NA), .groups = "keep")


rmr_labeller <- function(variable,value){
  return(rmr[value])
}
rmr <- list("Risk-Free Skeletal Lesions",
            "Risk-Doubling Skeletal Lesions")

chi_significance <- ggplot(chi_plot_data, aes(x = lesion_formation_rate, y = chi_sig_percent)) +
  geom_line(aes(color = mortality)) +
  facet_wrap(~ rmr, labeller = rmr_labeller) +
  theme_bw() +
  labs(x = "Exposure Rate", y = "% of runs with Significant Chi-square test", color = "Mortality") +
  scale_x_continuous(breaks = c(0.01, 0.03, 0.05,0.07, 0.09))
 

ggsave(filename = "./Known Unknowns/Figures/Supplemental Figures/sweep_chi_square_significance.pdf", 
       plot = chi_significance,
       width = 7,
       height = 5,
       units = "in",
       dpi = 1200)





###############################################################################################################


#### SURVIVAL ANALYSES ####



extract_survival_data <- function(sweep_data) {
  # Ensure the 'Dead' column exists and is set to 1 for all entries
  sweep_data <- sweep_data %>% mutate(Dead = 1)
  
  # Identify unique combinations of the grouping variables
  unique_combinations <- sweep_data %>%
    distinct(mortality, lesion_formation_rate, rep)
  
  # Initialize a list to store survival data frames
  survival_data_list <- list()
  
  # Iterate over each unique combination
  for (i in seq_len(nrow(unique_combinations))) {
    combo <- unique_combinations[i, ]
    
    # Filter data for the current combination
    d <- sweep_data %>%
      filter(
        mortality == combo$mortality,
        lesion_formation_rate == combo$lesion_formation_rate,
        rep == combo$rep
      )
    
    # Check if necessary columns exist
    if (!all(c("Age", "Dead", "Lesion") %in% colnames(d))) {
      warning(paste("Missing required columns in data for combination:", 
                    paste(combo, collapse = ", ")))
      next
    }
    
    # Fit the survival model
    surv_obj <- Surv(time = d$Age, event = d$Dead)
    fit <- survfit(surv_obj ~ Lesion, data = d)
    
    # Obtain the summary of the survival fit
    surv_summary <- summary(fit)
    
    # Extract survival times and estimates
    time_points <- surv_summary$time
    survival_probs <- surv_summary$surv
    
    # Extract stratum labels for each time point
    group_labels <- as.character(surv_summary$strata)
    
    # Construct the data frame with survival information
    surv_df <- data.frame(
      time = time_points,
      survival = survival_probs,
      group = group_labels,
      mortality = combo$mortality,
      lesion_formation_rate = combo$lesion_formation_rate,
      rep = combo$rep
    )
    
    # Append to the list
    survival_data_list[[i]] <- surv_df
  }
  
  # Combine all data frames into one
  survival_data <- bind_rows(survival_data_list)
  
  return(survival_data)
}


survival_data1 <- extract_survival_data(sweep_data1 %>% filter(rep == 1)) %>%
  mutate(mortality = factor(mortality, levels = c("CDW3", "CDW5", "CDW11", "CDW15", "CDW17", "CDW21")),
         mortality = recode(mortality,
                          "CDW3" = "CDW Level 3",
                          "CDW5" = "CDW Level 5",
                          "CDW11" = "CDW Level 11",
                          "CDW15" = "CDW Level 15",
                          "CDW17" = "CDW Level 17",
                          "CDW21" = "CDW Level 21"))

survival_data2 <- extract_survival_data(sweep_data2 %>% filter(rep == 1)) %>%
  mutate(mortality = factor(mortality, levels = c("CDW3", "CDW5", "CDW11", "CDW15", "CDW17", "CDW21")),
         mortality = recode(mortality,
                            "CDW3" = "CDW Level 3",
                            "CDW5" = "CDW Level 5",
                            "CDW11" = "CDW Level 11",
                            "CDW15" = "CDW Level 15",
                            "CDW17" = "CDW Level 17",
                            "CDW21" = "CDW Level 21"))

plot_survival_curves <- function(survival_data){
lesion_colors = c("black", '#b9c28d')
plots <- ggplot(survival_data, aes(x = time, y = survival)) +
  geom_line(aes(group = group, color = group)) +
  scale_color_manual(values = lesion_colors, labels = c("Absent", "Present")) +
  facet_grid(mortality ~ lesion_formation_rate,
             labeller = labeller(
               lesion_formation_rate = function(x) paste("Exposure Rate:", x))) +
  labs(color = "Skeletal Lesions") +
  theme_bw()

return(plots)
}

rmr1_survival_plots <- plot_survival_curves(survival_data1)
rmr2_survival_plots <- plot_survival_curves(survival_data2)


ggsave(filename = "./Known Unknowns/Figures/Supplemental Figures/sensitivity_sweep_survival_curves_rep1_rmr1.pdf", 
       plot = rmr1_survival_plots,
       width = 15,
       height = 8,
       units = "in",
       dpi = 1200)

ggsave(filename = "./Known Unknowns/Figures/Supplemental Figures/sensitivity_sweep_survival_curves_rep1_rmr2.pdf", 
       plot = rmr2_survival_plots,
       width = 15,
       height = 8,
       units = "in",
       dpi = 1200)





run_survival_analysis <- function(sweep_data) {
  
  # Create 'status' column indicating that everyone has experienced the outcome of interest (death)
  sweep_data$Dead <- 1
  
  # Initialize a list to store survfit objects
  survfit_list <- list()
  
  # Initialize a dataframe to store log-rank test results
  logrank_results <- data.frame()
  
  # Loop over unique combinations
  unique_combinations <- sweep_data %>%
    distinct(mortality, lesion_formation_rate, rep)
  
  for (i in seq_len(nrow(unique_combinations))) {
    combo <- unique_combinations[i, ]
    
    # Filter data for the current combination
    d <- sweep_data %>%
      filter(
        mortality == combo$mortality,
        lesion_formation_rate == combo$lesion_formation_rate,
        rep == combo$rep
      )
    
    # Check if necessary columns exist
    if (!all(c("Age", "Dead", "Lesion") %in% colnames(d))) {
      warning(paste("Missing required columns in data for combination:", 
                    paste(combo, collapse = ", ")))
      next
    }
    
    # Fit survival curve
    surv_obj <- Surv(time = d$Age, event = d$Dead)
    fit <- survfit(surv_obj ~ Lesion, data = d)
    
    # Perform log-rank test
    logrank_test <- survdiff(surv_obj ~ Lesion, data = d)
    p_value <- 1 - pchisq(logrank_test$chisq, df = length(logrank_test$n) - 1)
    
    # Store survfit object
    key <- paste(combo$mortality, combo$lesion_formation_rate, combo$rep, sep = "_")
    survfit_list[[key]] <- fit
    
    # Store log-rank test result
    logrank_results <- rbind(logrank_results, data.frame(
      mortality = combo$mortality,
      lesion_formation_rate = combo$lesion_formation_rate,
      rep = combo$rep,
      p_value = p_value
    ))
  }
  
  return(list(survfit_list = survfit_list, logrank_results = logrank_results))
  #return(logrank_results)
}




# sweep survival analyses
survival_sweep_rmr1 <- run_survival_analysis(sweep_data1)
survival_sweep_rmr2 <- run_survival_analysis(sweep_data2)

# summarise significant logrank tests for risk-free lesion cohorts
logrank_rmr1 <- survival_sweep_rmr1$logrank_results %>%
  mutate(significant = if_else(p_value < 0.05, 1, 0)) %>%
  group_by(mortality, lesion_formation_rate) %>%
  summarise(percent_significant = sum(significant, na.rm = T), .groups = "keep") %>%
  mutate(mortality = factor(mortality, levels = c("CDW3", "CDW5", "CDW11", "CDW15", "CDW17", "CDW21")),
         mortality = recode(mortality,
                            "CDW3" = "CDW Level 3",
                            "CDW5" = "CDW Level 5",
                            "CDW11" = "CDW Level 11",
                            "CDW15" = "CDW Level 15",
                            "CDW17" = "CDW Level 17",
                            "CDW21" = "CDW Level 21"))
# plot the spread
logrank_rmr1_plot <- ggplot(logrank_rmr1, aes(x = lesion_formation_rate, y = percent_significant)) +
  geom_line(aes(group = mortality, color = mortality)) +
  labs(x = "Exposure Rate", y = "% of runs with Significant Log-Rank test", color = "Mortality") +
  theme_bw() +
  scale_x_continuous(limits = c(0.005,.1), expand = c(0,0), 
                     breaks = c(0.01, 0.03, 0.05, 0.07, 0.09)) +
  theme(legend.position = "none")
  

# summarise significant logrank tests for risk-DOUBLING lesion cohorts
logrank_rmr2 <- survival_sweep_rmr2$logrank_results %>%
  mutate(significant = if_else(p_value < 0.05, 1, 0)) %>%
  group_by(mortality, lesion_formation_rate) %>%
  summarise(percent_significant = sum(significant, na.rm = T), .groups = "keep") %>%
  mutate(mortality = factor(mortality, levels = c("CDW3", "CDW5", "CDW11", "CDW15", "CDW17", "CDW21")),
         mortality = recode(mortality,
                            "CDW3" = "CDW Level 3",
                            "CDW5" = "CDW Level 5",
                            "CDW11" = "CDW Level 11",
                            "CDW15" = "CDW Level 15",
                            "CDW17" = "CDW Level 17",
                            "CDW21" = "CDW Level 21"))
logrank_rmr2_plot <- ggplot(logrank_rmr2, aes(x = lesion_formation_rate, y = percent_significant)) +
  geom_line(aes(group = mortality, color = mortality)) +
  labs(x = "Exposure Rate", y = "% of runs with Significant Log-Rank test", color = "Mortality") +
  theme_bw() +
  scale_x_continuous(limits = c(0.005,.1), expand = c(0,0), 
                     breaks = c(0.01, 0.03, 0.05, 0.07, 0.09)) +
  theme(legend.position = "bottom")

legend <- get_legend(logrank_rmr2_plot)
logrank_rmr2_plot <- logrank_rmr2_plot +
  theme(legend.position = "none")

logrank_significance_plot <- plot_grid(logrank_rmr1_plot, 
          logrank_rmr2_plot, 
          legend, 
          ncol = 1,
          rel_heights = c(1,1, 0.2)) +
  theme(plot.margin = unit(c(0.5,0.5,0.5,0.5), "cm"))


ggsave(filename = "./Known Unknowns/Figures/Supplemental Figures/logrank_significance.pdf", 
       plot = logrank_significance_plot,
       width = 5,
       height = 7,
       units = "in",
       dpi = 1200)




###############################################################################################################






