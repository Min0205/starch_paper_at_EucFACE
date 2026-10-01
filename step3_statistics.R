###### CO2 effect for NSC
nsc_stats <- ssall
nsc_stats$position <- factor(nsc_stats$position, levels=c("DBH","DBH + 50","BoC","Branch","Twig","old","new"))
levels(nsc_stats$position) <- c("Base","Base","BoC","Branch","Twig", "Old","New")

# test <- subset(nsc_stats, flush == "pre-flush" & position == "New")
# model <- lmer(tnc ~ treatment + (1|Ring), data = test)
# car::Anova(model)

##### twig and leaf
nsc_tl <- subset(nsc_stats, position %in% c("Twig", "New", "Old"))
# Function to run lmer and extract p-value using car::Anova
run_lmer_Anova_tl <- function(data, response, pos, fl) {
  data_sub <- data %>%
    filter(position == pos, flush == fl) %>%
    dplyr::select(Ring, treatment, all_of(response)) %>%
    rename(value = !!sym(response)) %>%
    filter(!is.na(value))
  
  # Check for enough data
  if (n_distinct(data_sub$treatment) < 2 || nrow(data_sub) < 6) {
    return(tibble(response = response, position = pos, flush = fl, p.value = NA))
  }
  
  model <- try(lmer(value ~ treatment + (1 | Ring), data = data_sub), silent = TRUE)
  
  if (inherits(model, "try-error")) {
    return(tibble(response = response, position = pos, flush = fl, p.value = NA))
  }
  
  # Run car::Anova
  aov_res <- try(Anova(model, type = "II"), silent = TRUE)
  if (inherits(aov_res, "try-error")) {
    return(tibble(response = response, position = pos, flush = fl, p.value = NA))
  }
  
  pval <- aov_res["treatment", "Pr(>Chisq)"]
  
  tibble(response = response, position = pos, flush = fl, p.value = pval)
}
# Variables to loop over
variables <- c("sugar", "starch", "tnc")
positions <- unique(nsc_tl$position)
flushes <- unique(nsc_tl$flush)
# Run the model across all combinations
results_tl <- expand.grid(response = variables,
                          position = positions,
                          flush = flushes,
                          stringsAsFactors = FALSE) %>%
  pmap_dfr(~run_lmer_Anova_tl(nsc_tl, ..1, ..2, ..3))
# View results
results_tl <- results_tl %>%
  arrange(response, position, flush) %>%
  mutate(component = case_when(
    position == "Twig" ~ "twig",
    position %in% c("New", "Old") ~ "foliage")) %>%
  dplyr::select(response, position, flush, component, p.value) 

##### stems
nsc_s <- subset(nsc_stats, position %in% c("Base", "BoC", "Branch"))
# Function to run lmer and extract p-value using car::Anova
run_lmer_Anova_s <- function(data, response, pos, fl, comp) {
  data_sub <- data %>%
    filter(position == pos, flush == fl, component == comp) %>%
    dplyr::select(Ring, treatment, all_of(response)) %>%
    rename(value = !!sym(response)) %>%
    filter(!is.na(value))
  
  if (n_distinct(data_sub$treatment) < 2 || nrow(data_sub) < 6) {
    return(tibble(response = response, position = pos, flush = fl,
                  component = comp, p.value = NA))
  }
  
  model <- try(lmer(value ~ treatment + (1 | Ring), data = data_sub), silent = TRUE)
  if (inherits(model, "try-error")) {
    return(tibble(response = response, position = pos, flush = fl,
                  component = comp, p.value = NA))
  }
  
  aov_res <- try(Anova(model, type = "II"), silent = TRUE)
  if (inherits(aov_res, "try-error")) {
    return(tibble(response = response, position = pos, flush = fl,
                  component = comp, p.value = NA))
  }
  
  pval <- aov_res["treatment", "Pr(>Chisq)"]
  
  tibble(response = response, position = pos, flush = fl,
         component = comp, p.value = pval)
}
# Set up combinations
variables <- c("sugar", "starch", "tnc")
positions <- unique(nsc_s$position)
flushes <- unique(nsc_s$flush)
# Define which positions have which components
component_map <- list(
  Base = c("wood", "bark"),
  BoC = c("wood", "bark"),
  Branch = c("wood", "bark"))
# Create full list of combinations
combo_list <- expand.grid(
  response = variables,
  position = names(component_map),
  flush = flushes,
  stringsAsFactors = FALSE
) %>%
  mutate(component = map(position, ~ component_map[[.x]])) %>%
  unnest(component)
# Run the model across all combinations
results_s <- pmap_dfr(combo_list, function(response, position, flush, component) {
  run_lmer_Anova_s(nsc_s, response, position, flush, component)
})
# View result
results_s %>%
  arrange(response, position, component, flush)
results <- rbind(results_tl, results_s)

write.csv(results, "output/nsc_stats.csv")


###### interactive effect of CO2 and flush for NSC
### wood
ss_base_wood <- subset(ssall, position %in% c("DBH", "DBH + 50") & component == "wood")
ss_boc_wood <- subset(ssall, position %in% c("BoC") & component == "wood")
ss_branch_wood <- subset(ssall, position %in% c("Branch") & component == "wood")

model_tnc_wood <- lmer(
  tnc ~ flush * treatment + (1 | Ring),
  data = ss_branch_wood)
car::Anova(model_tnc_wood)

### bark
ss_base_bark <- subset(ssall, position %in% c("DBH", "DBH + 50") & component == "bark")
ss_boc_bark <- subset(ssall, position %in% c("BoC") & component == "bark")
ss_branch_bark <- subset(ssall, position %in% c("Branch") & component == "bark")

model_tnc_bark <- lmer(
  tnc ~ flush * treatment + (1 | Ring),
  data = ss_branch_bark)
car::Anova(model_tnc_bark)

### twig
ss_twig <- subset(ssall, position %in% c("Twig"))
model_twig <- lmer(
  tnc ~ flush * treatment + (1 | Ring),
  data = ss_twig)
car::Anova(model_twig)

### foliage
ss_foliage <- subset(ssall, position %in% c("new"))
model_foliage <- lmer(
  tnc ~ flush * treatment + (1 | Ring),
  data = ss_foliage)
car::Anova(model_foliage)



###### lai and greenness
##### read and process
laidf <- lai
### filter gcc data with Fdiff >= 0.98
gccdf <- subset(gcc, Fdiff >= 0.98)
### calculate gcc daily averages
gccd <- doBy::summaryBy(GCC~Date+Ring, data=gccdf, FUN=mean, keep.names=T, na.rm=T)
### subset Oct to Dec
laisub <- subset(laidf,Date > "2015-09-30" & Date < "2016-01-01")
gccsub <- subset(gccd,Date > "2015-09-30" & Date < "2016-01-01")
### mutate treatment column
laisub$Trt[laisub$Ring %in% c("R1", "R4", "R5")] <- "eCO2"
gccsub$Trt[gccsub$Ring %in% c("1", "4", "5")] <- "eCO2"
laisub$Trt[laisub$Ring %in% c("R2", "R3", "R6")] <- "aCO2"
gccsub$Trt[gccsub$Ring %in% c("2", "3", "6")] <- "aCO2"
laisub$Trt <- as.factor(laisub$Trt)
gccsub$Trt <- as.factor(gccsub$Trt)
##### models for lai
laisub$Date0 <- as.numeric(laisub$Date - min(laisub$Date))
gccsub$Date0 <- as.numeric(gccsub$Date - min(gccsub$Date))
fit.lai <- lme(LAI ~ Date0 + Date0:Trt, random=~1|Ring, 
                 method="ML",
                 data=laisub)
pander(car::Anova(fit.lai))
slopes <- emtrends(fit.lai, specs = ~ Trt, var = "Date0")
slopes_df <- as.data.frame(slopes)
### get percent increase and 95% CI
slope_aCO2 <- slopes_df$Date0.trend[slopes_df$Trt == "aCO2"]
slope_eCO2 <- slopes_df$Date0.trend[slopes_df$Trt == "eCO2"]
se_aCO2 <- slopes_df$SE[slopes_df$Trt == "aCO2"]
se_eCO2 <- slopes_df$SE[slopes_df$Trt == "eCO2"]

percent_diff <- (slope_eCO2 - slope_aCO2) / abs(slope_aCO2) * 100
delta_slope <- slope_eCO2 - slope_aCO2
se_delta <- sqrt(se_aCO2^2 + se_eCO2^2) 
# 95% CI
ci_lower <- delta_slope - 1.96 * se_delta
ci_upper <- delta_slope + 1.96 * se_delta
percent_lower <- ci_lower / abs(slope_aCO2) * 100
percent_upper <- ci_upper / abs(slope_aCO2) * 100
##### models for gcc
fit.gcc <- lme(GCC ~ Date0 + Date0:Trt, random=~1|Ring, 
                 method="ML",
                 data=gccsub)
pander(car::Anova(fit.gcc))
summary(fit.gcc)
slopes <- emtrends(fit.gcc, specs = ~ Trt, var = "Date0")
slopes_df <- as.data.frame(slopes)
### get percent increase and 95% CI
slope_aCO2 <- slopes_df$Date0.trend[slopes_df$Trt == "aCO2"]
slope_eCO2 <- slopes_df$Date0.trend[slopes_df$Trt == "eCO2"]
se_aCO2 <- slopes_df$SE[slopes_df$Trt == "aCO2"]
se_eCO2 <- slopes_df$SE[slopes_df$Trt == "eCO2"]
  
percent_diff <- (slope_eCO2 - slope_aCO2) / abs(slope_aCO2) * 100
delta_slope <- slope_eCO2 - slope_aCO2
se_delta <- sqrt(se_aCO2^2 + se_eCO2^2)  # 独立假设
# 95% CI
ci_lower <- delta_slope - 1.96 * se_delta
ci_upper <- delta_slope + 1.96 * se_delta
percent_lower <- ci_lower / abs(slope_aCO2) * 100
percent_upper <- ci_upper / abs(slope_aCO2) * 100
  







##### distribution of residuals
hist(residuals(fit.lai), main = "Histogram of residuals")
qqnorm(residuals(fit.lai)); qqline(residuals(fit.lai))
shapiro.test(residuals(fit.lai)) # Shapiro-Wilk test for normality

res <- residuals(fit.lai, type = "normalized")  # or type = "pearson"
group <- interaction(laisub$Trt, laisub$Date0)
leveneTest(res ~ group)

hist(residuals(fit.gcc), main = "Histogram of residuals")
qqnorm(residuals(fit.gcc)); qqline(residuals(fit.gcc))
shapiro.test(residuals(fit.gcc)) # Shapiro-Wilk test for normality
leveneTest(fit.gcc)
res <- residuals(fit.gcc)
group <- laisub$treatment
leveneTest(res ~ group)


res <- residuals(fit.gcc, type = "normalized")  # or type = "pearson"
group <- interaction(gccsub$Trt, gccsub$Date0)
leveneTest(res ~ group)




