## Set working directory ----
setwd("C:\\Users\\John Karuitha\\OneDrive - University of Witwatersrand\\Documents\\My Thesis\\Karuitha and Ojah Data\\THESIS\\Dissertation\\Objective 1 Binary\\Chapters in Progress\\Objective1_financing structure_determinants")

## Download required packages ----


## Load required packages ----
library(tidyverse)
library(plm)
library(broom)
library(corrplot)
library(skimr)
library(GGally)
library(gridExtra)
library(ggthemes)
library(stargazer)
library(lmtest)
library(bbplot)
library(robustHD)
library(gplots)
library(pcse)
library(car)

## Load dataset and attach ----
data <- read.csv("amelia.csv")

###############################################################################
## Visualization of the variables ----
data %>% ggplot(aes(x = debt_to_equity_ratio,
  fill = currentlegalstatus)) + 
  geom_density(position = "stack", alpha = 1) + 
  scale_x_log10() +
  labs(x = "Debt to Equity Ratio (Log Scale)", 
  title = "Debt to Equity Ratio") + theme_economist()
##need to log this in regression

data %>% ggplot(aes(x = deposits_to_total_assets,
  fill = currentlegalstatus)) + 
  geom_density(position = "stack", alpha = 1) + 
  scale_x_log10() + 
  labs(x = "Deposits to Total Assets (Log Scale)", 
  title = "Deposits to Total Assets") + 
  theme_bw() 
##need to log this in regression

data %>% ggplot(aes(x = capital_.asset_ratio,
  fill = currentlegalstatus)) + 
  geom_density(position = "stack", alpha = 1) + 
  scale_x_log10() + 
  labs(x = "Capital to Assets Ratio (Log Scale)", 
  title = "Capital to Assets Ratio") + 
  theme_bw() 

##need to log this in regression

data %>% ggplot(aes(x = assets,
  fill = currentlegalstatus)) + 
  geom_density(position = "stack", alpha = 1) + 
  scale_x_log10() + 
  labs(x = "Assets (Log Scale)", 
  title = "Assets") + 
  theme_bw() 

##already logged this in regression

data %>% ggplot(aes(x = asset_structure,
  fill = currentlegalstatus)) + 
  geom_density(position = "stack", alpha = 1) + 
  scale_x_log10() + 
  labs(x = "Assets Structure (Log Scale)", 
  title = "Assets Structure") + 
  theme_bw() 

##need to log this in regression

data %>% ggplot(aes(x = profit_margin,
  fill = currentlegalstatus)) + 
  geom_density(position = "stack", alpha = 1) + 
  scale_x_log10() + 
  labs(x = "Profit Margin (Log Scale)", 
  title = "Profit margin") + 
  theme_bw() 


data %>% ggplot(aes(x = donations_assets_ratio,
  fill = currentlegalstatus)) + 
  geom_density(position = "stack", alpha = 1) + 
  scale_x_log10() + 
  labs(x = "Donations to Assets Ratio (Log Scale)", 
       title = "Donations to Assets") + 
  theme_bw() 

##need to log this in regression
###########################################################################
data[which.min(data$profit_margin), c(1:5, 40)]
data[data$mfiid == 149278, c(1:5, 40)]
data$profit_margin <- ifelse(data$profit_margin <= 35000, 
                             data$profit_margin/10000, data$profit_margin)

data$capital_.asset_ratio <- ifelse(data$capital_.asset_ratio > 1, 1, 
                                    data$capital_.asset_ratio)

data$capital_.asset_ratio <- ifelse(data$capital_.asset_ratio < 0, 0, 
                                    data$capital_.asset_ratio)

data$donations_assets_ratio <- ifelse(data$donations_assets_ratio > 1, 1, 
                                      data$donations_assets_ratio)

data$donations_assets_ratio <- ifelse(data$donations_assets_ratio < 0, 0, 
                                      data$donations_assets_ratio)

data$deposits_to_total_assets <- ifelse(data$deposits_to_total_assets > 1, 1, 
                                      data$deposits_to_total_assets)

data$deposits_to_total_assets <- ifelse(data$deposits_to_total_assets < 0, 0, 
                                        data$deposits_to_total_assets)

data$debt_to_equity_ratio <- ifelse(data$debt_to_equity_ratio < 0, 0, 
                                        data$debt_to_equity_ratio)



###########################################################################

data %>% ggplot(aes(x = kkm,
  fill = legal_tradition)) + 
  geom_density(position = "stack", alpha = 1) + 
  scale_x_log10() + 
  labs(x = "KKM - Institutional Quality (Log Scale)", 
  title = "KKM - Institutional Quality") + 
  theme_bw() 

##need to log this in regression

data %>% ggplot(aes(x = education,
  fill = legal_tradition)) + 
  geom_density(position = "stack", alpha = 1) + 
  scale_x_log10() + 
  labs(x = "Education (Log Scale)", 
  title = "Education") + 
  theme_bw() 

##need to log this in regression

data %>% ggplot(aes(x = fdev,
  fill = legal_tradition)) + 
  geom_density(position = "stack", alpha = 1) + 
  scale_x_log10() + 
  labs(x = "Financial Development (Log Scale)", 
  title = "Financial Development") + 
  theme_bw() 

##need to difference this in regression

######################################################################################
# Visualize dependent variables by outreac, age, and legal tradition ----
# Debt to equity ratio  ----
data[which(data$debt_to_equity_ratio <= -0.01), 
     c("mfiname", "debt_to_equity_ratio")] %>% head(20)
data[which.min(data$debt_to_equity_ratio), c(1:5, 35)]

data[data$mfiname == "RECEC/FD", c("mfiname", "debt_to_equity_ratio")]

data$debt_to_equity_ratio <- ifelse(data$mfiname == "RECEC/FD" & 
                            data$debt_to_equity_ratio <= -3567, 
                            data$debt_to_equity_ratio/10, data$debt_to_equity_ratio)

median_n <- function(x){median(x, na.rm = TRUE)}
mean_n <- function(x){mean(x, na.rm = TRUE)}

plotls1 <- data %>% ggplot(aes(x = reorder(currentlegalstatus, 
                                           debt_to_equity_ratio, median_n), 
                    y = debt_to_equity_ratio, fill = currentlegalstatus)) + 
  geom_violin(draw_quantiles = c(0.25, 0.5, 0.75), 
              scale = "count") + scale_y_log10() + 
  labs(x = "", y = "", 
       title = "Debt to Equity Ratio") + theme_minimal() +
  stat_summary(fun = mean, geom = "point", size = 1, color = "red") + 
  theme(axis.text.x = element_blank()) + 
  theme(legend.position = "bottom") + theme(legend.key.size = unit(0.4, "cm")) + 
  theme(legend.title = element_blank())

plotls1


data %>% ggplot(aes(x = reorder(outreach, debt_to_equity_ratio, median_n), 
        y = debt_to_equity_ratio, fill = outreach)) + 
  geom_violin(draw_quantiles = c(0.25, 0.5, 0.75), 
              scale = "count") + scale_y_log10() + 
        labs(x = "", y = "Debt to Equity Ratio", 
             title = "",
             caption = "Inside the bins, the black horizontal lines
             are the first, second (median), and third quatiles. 
             The red point is the mean") + 
  stat_summary(fun = mean, geom = "point", size = 1, color = "red")

plotage1 <- data %>% ggplot(aes(x = reorder(age, debt_to_equity_ratio, median_n), 
  y = debt_to_equity_ratio, fill = age)) + 
  geom_violin(draw_quantiles = c(0.25, 0.5, 0.75), scale = "count") + 
  scale_fill_manual(values=c("orange", "white", "grey")) +
  scale_y_log10()+ labs(x = "", y = "", 
                    title = "") + theme_minimal() +
  stat_summary(fun = mean, geom = "point", size = 1, color = "red") + 
  theme(legend.position = "none")+ theme(axis.text.x = element_blank())

plotage1

data %>% ggplot(aes(x = reorder(legal_tradition, debt_to_equity_ratio, median_n), 
                    y = debt_to_equity_ratio, fill = legal_tradition)) + 
  geom_violin(draw_quantiles = c(0.25, 0.5, 0.75), scale = "count") + 
  scale_y_log10()+ 
  labs(x = "", y = "Debt to Equity Ratio", 
       title = "", 
       caption = "Inside the bins, the black horizontal lines
             are the first, second (median), and third quatiles. 
             The red point is the mean") + 
  stat_summary(fun = mean, geom = "point", size = 1, color = "red")

## Deposits to total assets ----
plotls2 <- data %>% ggplot(aes(x = reorder(currentlegalstatus, 
                                           deposits_to_total_assets, median_n), 
                    y = deposits_to_total_assets, fill = currentlegalstatus)) + 
  geom_violin(draw_quantiles = c(0.25, 0.5, 0.75), 
              scale = "count") + scale_y_log10() + 
  labs(x = "", y = "", 
       title = "Deposits to Total Assets Ratio") + theme_minimal()+
  stat_summary(fun = mean, geom = "point", size = 1, color = "red") +
  theme(legend.position = "none") + theme(axis.text.x = element_blank()) 

plotls2


data %>% ggplot(aes(x = reorder(outreach, deposits_to_total_assets, median_n), 
                    y = deposits_to_total_assets, fill = outreach)) + 
  geom_violin(draw_quantiles = c(0.25, 0.5, 0.75), scale = "count") + 
  scale_y_log10()+ 
                   labs(x = "", y = "Deposits to Total Assets", 
                   title = "",
                   caption = "Inside the bins, the black horizontal lines
             are the first, second (median), and third quatiles. 
             The red point is the mean") + stat_summary(fun = mean, 
              geom = "point", size = 1, color = "red")

plotage2 <- data %>% ggplot(aes(x = reorder(age, deposits_to_total_assets, median_n), 
                    y = deposits_to_total_assets, fill = age)) + 
  geom_violin(draw_quantiles = c(0.25, 0.5, 0.75), scale = "count") + 
  scale_fill_manual(values=c("orange", "white", "grey")) +
  scale_y_log10()+ labs(x = "", y = "", 
                  title = "") + theme_minimal() +
                  stat_summary(fun = mean, geom = "point", size = 1, color = "red") +
  theme(legend.position = "none")+ theme(axis.text.x = element_blank())

plotage2


data %>% ggplot(aes(x = reorder(legal_tradition, deposits_to_total_assets, median_n), 
                    y = deposits_to_total_assets, fill = legal_tradition)) + 
  geom_violin(draw_quantiles = c(0.25, 0.5, 0.75), scale = "count") + 
  scale_y_log10()+ labs(x = "", y = "Deposits to Total Assets", 
  title = "", 
  caption = "Inside the bins, the black horizontal lines
             are the first, second (median), and third quatiles. 
             The red point is the mean") + stat_summary(fun = mean, 
              geom = "point", size = 1, color = "red")

  

# Capital to assets ratio ----
plotls3 <- data %>% ggplot(aes(x = reorder(currentlegalstatus, 
                                           capital_.asset_ratio, median_n), 
                    y = capital_.asset_ratio, fill = currentlegalstatus)) + 
  geom_violin(draw_quantiles = c(0.25, 0.5, 0.75), 
              scale = "count") + scale_y_log10() + 
  labs(x = "", y = "", 
       title = "Capital to Assets Ratio") + theme_minimal() +
  stat_summary(fun = mean, geom = "point", size = 1, color = "red") + 
  theme(axis.text.x = element_blank()) + 
  theme(legend.title = element_blank()) + theme(legend.position = "none")

plotls3


data %>% ggplot(aes(x = reorder(outreach, capital_.asset_ratio, median_n), 
                    y = capital_.asset_ratio, fill = outreach)) + 
  geom_violin(draw_quantiles = c(0.25, 0.5, 0.75), scale = "count") + 
  scale_y_log10()+ 
                    labs(x = "", y = "Capital Asset Ratio", 
                    title = "", 
                    caption = "Inside the bins, the black horizontal lines
             are the first, second (median), and third quatiles. 
             The red point is the mean") + stat_summary(fun = mean, 
              geom = "point", size = 1, color = "red")


plotage3 <- data %>% ggplot(aes(x = reorder(age, capital_.asset_ratio, median_n), 
                    y = capital_.asset_ratio, fill = age)) + 
  geom_violin(draw_quantiles = c(0.25, 0.5, 0.75), scale = "count") + 
  scale_fill_manual(values=c("orange", "white", "grey")) +
  scale_y_log10()+ 
                    labs(x = "", y = "", 
                     title = "") + stat_summary(fun = mean, 
              geom = "point", size = 1, color = "red") + theme_minimal() +
  theme(axis.text.x = element_blank()) + theme(legend.position = "bottom") +
  theme(legend.key.size = unit(0.4, "cm")) + theme(legend.title = element_blank())

plotage3

data %>% ggplot(aes(x = reorder(legal_tradition, capital_.asset_ratio, median_n), 
                    y = capital_.asset_ratio, fill = legal_tradition)) + 
  geom_violin(draw_quantiles = c(0.25, 0.5, 0.75), scale = "count") + 
  scale_y_log10()+ 
                    labs(x = "", y = "Capital Asset Ratio", 
                    title = "",
                    caption = "Inside the bins, the black horizontal lines
             are the first, second (median), and third quatiles. 
             The red point is the mean") + stat_summary(fun = mean, 
              geom = "point", size = 1, color = "red")

grid.arrange(plotls1, plotls2, plotls3, plotage1, plotage2, plotage3, ncol = 3, nrow = 2)

##################################################################################
plotdonations1 <- data %>% ggplot(aes(x = reorder(currentlegalstatus, 
  donations_assets_ratio, median_n), 
  y = donations_assets_ratio, fill = currentlegalstatus)) + 
  geom_violin(draw_quantiles = c(0.25, 0.5, 0.75), 
  scale = "count") + scale_y_log10() + 
  labs(x = "", y = "", 
  title = "Donations to Assets Ratio by MFI Legal Status") + theme_minimal() +
  stat_summary(fun = mean_n, geom = "point", size = 1, color = "red") + 
  theme(axis.text.x = element_blank()) + 
  theme(legend.title = element_blank()) + theme(legend.position = "bottom") +
  theme(legend.key.size = unit(0.4, "cm"))

plotdonations1

plotdonations2 <- data %>% ggplot(aes(x = reorder(age, 
  donations_assets_ratio, median_n), 
  y = donations_assets_ratio, fill = age)) + 
  geom_violin(draw_quantiles = c(0.25, 0.5, 0.75), 
              scale = "count") + scale_y_log10() + 
  labs(x = "", y = "", 
       title = "Donations to Assets Ratio by MFI Age") + theme_minimal() +
  stat_summary(fun = mean, geom = "point", size = 1, color = "red") + 
  theme(axis.text.x = element_blank()) + 
  theme(legend.title = element_blank()) + theme(legend.position = "bottom") + 
  theme(legend.key.size = unit(0.4, "cm"))

plotdonations2

################################################################################
###############################################################################
var_cor <- data %>% select(debt_to_equity_ratio, 
                                deposits_to_total_assets, 
                                capital_.asset_ratio,
                                lassets, kkm, education, fdev, 
                                asset_structure, profit_margin, 
                                donations_assets_ratio) %>% 
                                na.omit() %>% cor()
var_cor

corrplot(var_cor, method = "circle", type = "lower", col = "black")

plots_numeric <- data %>% ungroup() %>% select(debt_to_equity_ratio, 
                           deposits_to_total_assets, 
                           capital_.asset_ratio, donations_assets_ratio,
                           assets, kkm, education, fdev, 
                           asset_structure, profit_margin) %>% 
  sapply(., function(x) {x + 1}) %>% log10() %>% as.data.frame() %>%
                           na.omit()  %>% 
  ggpairs(columnLabels = c("DebtEquity", "DepositsAssets", "CapitalAsset", 
                           "DonationsAsset",
                          "Assets", "KKM", "Education", "FDEV", 
                          "AssetStruc", "ProfitMargin"), 
                          lower = list(continuous = wrap("points", alpha = 0.2, 
                                      color = "blue"), 
                                       combo = wrap("dot_no_facet", alpha = 0.1, size = 0.1)))

plots_numeric
###############################################################################
## Clean profit margin ----
stats_summary <- data %>% ungroup() %>% select(debt_to_equity_ratio, 
        deposits_to_total_assets, 
        capital_.asset_ratio, donations_assets_ratio,
        lassets, kkm, education, fdev, 
        asset_structure, profit_margin) %>% skim() %>% tibble()

stats_summary
write.csv(stats_summary, "summary.csv")

################################################################################
## Convert country and year to dummies 
data$year <- as.factor(data$year)
data$country <- as.factor(data$year)
data$age <- factor(data$age, levels = c("Mature", "Young", "New"))
data$currentlegalstatus <- factor(data$currentlegalstatus, 
                                  levels = c("NGO", "Bank", 
                                             "Credit Union/ Cooperative", 
                                             "NBFI", "Rural Bank"))

###############################################################################
## Create d_fdev variable by taking first difference of fdev with groups ----
data <- data %>%
  arrange(year) %>% 
  group_by(mfiid) %>% 
  mutate(d_fdev = fdev - dplyr::lag(fdev))


###############################################################################
## Ken Mwai helps ----
##data_sub <- data %>% 
  #select(mfiid,year,fdev) %>% 
  #arrange(year) %>% 
  #group_by(mfiid) %>% 
  #mutate(d_fdev = fdev - dplyr::lag(fdev))

data$d_fdev

#################################################################################
## Plot means for panel data using gplots package ----
plotmeans1 <- plotmeans(debt_to_equity_ratio ~ mfiid, 
          main = "Debt- Equity Ratio Variation by Microfinance Institution", 
          ylab = "Debt-Equity Ratio", 
          xlab = "MFIID", data = data)

plotmneans2 <- plotmeans(capital_.asset_ratio ~ mfiid, 
          main = "Capital Asset Ratio Variation by Microfinance Institution", 
          ylab = "Capital-Asset Ratio", 
          xlab = "MFIID", data = data)

plotmeans3 <- plotmeans(deposits_to_total_assets ~ mfiid, 
          main = "Deposits To Total Assets Variation by Microfinance Institution",
          ylab = "Deposits to Total Assets Ratio", 
          xlab = "MFIID",
          data = data)

plotmeans4 <- plotmeans(debt_to_equity_ratio ~ year, 
          main = "Debt to Equity Ratio Trends Over Time", 
          ylab = "Debt-Equity Ratio", 
          xlab = "Year", data = data)



plotmeans5 <- plotmeans(capital_.asset_ratio ~ year, 
          main = "Capital Asset Ratio Trends Over Time", 
          ylab = "Capital to Total Assets Ratio", 
          xlab = "Year",
          data = data)

plotmeans6 <- plotmeans(deposits_to_total_assets ~ year, 
          ylab = "Deposits to Total Assets Ratio", 
          xlab = "Year",
          main = "Deposit Asset Ratio Trends Over Time", data = data)

plotmeans7 <- plotmeans(donations_assets_ratio ~ year, 
                        main = "Donations to Asset Ratio Trends Over Time", 
                        ylab = "Donations to Assets Ratio", 
                        xlab = "Year",
                        data = data)

plotmeans8 <- plotmeans(donations_assets_ratio ~ mfiid, 
                        main = "Donations to Assets_ratio Variation by Microfinance Institution",
                        ylab = "Donations to Assets Ratio", 
                        xlab = "MFIID",
                        data = data)

##################################################################################
## Define a Regression function ----
regression_fn <- function(depvar, my_data, my_model) {
  plm(depvar ~ lassets + log10(kkm) + log10(education) + d_fdev + 
        log10(asset_structure + 1) + 
        log10(profit_margin) + 
        currentlegalstatus + age +  year, 
      data = my_data, model = my_model, 
      index = c("mfiid", "year"))
}

##################################################################################
# Define a Hausamnn Test Function 
hausmann_fn <- function(fixed, random){
  phtest(fixed, random) %>% 
    broom::tidy()
}

##################################################################################
## Run Hausmann test for fixed vs Random effects 
# Debt to equity ratio ----
data <- data %>% ungroup()
fixed_hausmann1 <- plm(log10(debt_to_equity_ratio + 1) ~ 
                         lassets + 
                         log10(kkm) + log10(education) + d_fdev + 
                         log10(asset_structure + 1) + 
                         log10(profit_margin) + 
                         currentlegalstatus + age + year, 
                       data = data, model = "within", 
                       index = c("mfiid", "year"))
summary(fixed_hausmann1)

de_model_fe <- coeftest(fixed_hausmann1, vcov.=function(x) vcovBK(x, method = "arellano", 
                                type="HC1", cluster = "group"))


summary(fixed_hausmann1)
fde1 <- broom::tidy(fixed_hausmann1)
fde1

random_hausmann1 <- plm(log10(debt_to_equity_ratio + 1) ~ 
                          lassets + log10(kkm) + log10(education) 
                          + d_fdev + 
                          log10(asset_structure + 1) + 
                          log10(profit_margin) + 
                          currentlegalstatus + age +  year, 
                        data = data, model = "random", 
                        index = c("mfiid", "year"))

summary(random_hausmann1)
de_model_re <- coeftest(random_hausmann1, vcov.=function(x) vcovBK(x, method = "arellano", 
                          type="HC1", cluster = "group"))

rde1 <- broom::tidy(random_hausmann1)
rde1

h1 <- broom::tidy(phtest(fixed_hausmann1, random_hausmann1))
h1 ## Choose the random effects model ******************************************

################################################################################
pooling1 <- plm(log10(debt_to_equity_ratio + 1) ~ 
                  lassets + 
                  log10(kkm) + log10(education) + d_fdev + 
                  log10(asset_structure + 1) + 
                  log10(profit_margin) + 
                  currentlegalstatus + age +  year, 
                data = data, model = "pooling", 
                index = c("mfiid", "year"))

de_model_pooling <- coeftest(pooling1, vcov.=function(x) vcovBK(x, method = "arellano", 
                        type="HC1", cluster = "group"))

summary(pooling1)
pde1 <- broom::tidy(pooling1)
pde1

#######################################################################################
# Run regressions for 3 or more years ----
data3years <- data %>% group_by(mfiid) %>% filter(n() > 3) %>% ungroup()
######################################################################################

debt_eq3 <- plm(log10(debt_to_equity_ratio + 1) ~ 
            lassets + log10(kkm) + log10(education) + d_fdev + 
            log10(asset_structure + 1) + 
            log10(profit_margin) + 
            currentlegalstatus + age +  year,
            data = data3years, model = "random", 
            index = c("mfiid", "year"))

de_model_re3 <- coeftest(debt_eq3, vcov.=function(x) vcovBK(x, method = "arellano", 
                    type="HC1", cluster = "group"))

summary(debt_eq3)
rde2 <- broom::tidy(debt_eq3)
rde2

##############################################################################
# Run regressions 5 or more years 
data5years <- data %>% group_by(mfiid) %>% filter(n() > 5) %>% ungroup()
#############################################################################
debt_eq5 <- plm(log10(debt_to_equity_ratio + 1) ~ 
                  lassets + log10(kkm) + log10(education) + d_fdev + 
                  log10(asset_structure + 1) + 
                  log10(profit_margin) + 
                  currentlegalstatus + age +  year, 
                  data = data5years, model = "random", 
                  index = c("mfiid", "year"))

de_model_re_5 <- coeftest(debt_eq5, vcov.=function(x) vcovBK(x, method = "arellano", 
                          type="HC1", cluster = "group"))

summary(debt_eq5)
summary(debt_eq5)
rde3 <- broom::tidy(debt_eq5)
rde3

####################################################################################
##Stargazer Outreg model debt-equity
final_de_model <- stargazer(de_model_re, de_model_re_5, de_model_re3, 
                             de_model_fe, de_model_pooling, debt_eq_wins1,
                            title= "Regression FE D/E Ratio", 
                             out = "DE-FE_Model.html")

final_de_model_uncorrected <- stargazer(random_hausmann1, debt_eq5, debt_eq3, 
                                        fixed_hausmann1, pooling1, 
                                        debt_eq_wins1_unadj,
                                        title= "Regression FE D/E Ratio- uncorrected", 
                                        out = "DE-FE_Model_unadjusted.html")

######################################################################################
# Deposits to total assets ----
fixed_hausmann2_unadj <- plm(log10(deposits_to_total_assets + 1) ~ 
                         lassets + log10(kkm) + log10(education) + d_fdev + 
                         log10(asset_structure + 1) + 
                         log10(profit_margin) + 
                         currentlegalstatus + age +  year, 
                       data = data, model = "within", 
                       index = c("mfiid", "year"))

fixed_hausmann2_adj <- coeftest(fixed_hausmann2_unadj, vcov.=function(x) vcovBK(x, method = "arellano", 
                            type="HC1", cluster = "group"))

summary(fixed_hausmann2_adj)
rdta0 <- broom::tidy(fixed_hausmann2)
rdta0
broom::tidy(fixed_hausmann2)

random_hausmann2_unadj <- plm(log10(deposits_to_total_assets + 1) ~ 
                          lassets + log10(kkm) + log10(education) + d_fdev + 
                          log10(asset_structure + 1) + 
                          log10(profit_margin) + 
                          currentlegalstatus + age +  year, 
                        data = data, model = "random", 
                        index = c("mfiid", "year"))

random_hausmann2_adj <- coeftest(random_hausmann2_unadj, vcov.=function(x) vcovBK(x, method = "arellano", 
                          type="HC1", cluster = "group"))

summary(random_hausmann2)
rdta1 <- broom::tidy(random_hausmann2)
rdta1
h2 <- broom::tidy(phtest(fixed_hausmann2, random_hausmann2))
h2 ## Choose the random effects model******************************************

##############################################################################
dep_assets_eq2_unadj <- plm(log10(deposits_to_total_assets + 1) ~ 
                        lassets + log10(kkm) + log10(education) + d_fdev + 
                        log10(asset_structure + 1) + 
                        log10(profit_margin) + 
                        currentlegalstatus + age +  year, 
                      data = data3years, model = "random", 
                      index = c("mfiid", "year"))

dep_assets_eq2_adj <- coeftest(dep_assets_eq2_unadj, vcov.=function(x) vcovBK(x, method = "arellano", 
                          type="HC1", cluster = "group"))

summary(dep_assets_eq2)
rdta2 <- broom::tidy(dep_assets_eq2)


dep_assets_eq3_adj <- coeftest(dep_assets_eq3_unadj, vcov.=function(x) vcovBK(x, method = "arellano", 
                        type="HC1", cluster = "group"))

summary(dep_assets_eq3)
rdta3 <- broom::tidy(dep_assets_eq3)
rdta3
summary(dep_assets_eq3)

pooling2_unadj <- plm(log10(deposits_to_total_assets + 1) ~ 
                  lassets + 
                  log10(kkm) + log10(education) + d_fdev + 
                  log10(asset_structure + 1) + 
                  log10(profit_margin) + 
                  currentlegalstatus + age +  year, 
                data = data, model = "pooling", 
                index = c("mfiid", "year"))

pooling2_adj <- coeftest(pooling2_unadj, vcov.=function(x) vcovBK(x, method = "arellano", 
                        type="HC1", cluster = "group"))

summary(pooling2)
pde2 <- broom::tidy(pooling2)
pde2

###################################################################################
final_deposits_model <- stargazer(random_hausmann2_adj, dep_assets_eq3_adj, 
                                  dep_assets_eq2_adj, fixed_hausmann2_adj, 
                                  pooling2_adj, deposits_to_total_assets1_adj,
                                  title= "Regression Deposits Ratio", 
                                  out = "Deposits_Model.html")


final_deposits_model_unadj <- stargazer(random_hausmann2_unadj, dep_assets_eq3_unadj, 
                                  dep_assets_eq2_unadj, fixed_hausmann2_unadj, 
                                  pooling2_unadj, deposits_to_total_assets1,
                                  title= "Regression Deposits Ratio- Unadjusted", 
                                  out = "Deposits_Model_unadjusted.html")
###################################################################################
# Capital assets ratio ----
data <- data %>% ungroup()

fixed_hausmann3_unadj <- plm(log10(capital_.asset_ratio + 1) ~ 
                         lassets + 
                         log10(kkm) + log10(education) + d_fdev + 
                         log10(asset_structure + 1) + 
                         log10(profit_margin) + 
                         currentlegalstatus + age +  year, 
                       data = data, model = "within", 
                       index = c("mfiid", "year"))

fixed_hausmann3_adj <- coeftest(fixed_hausmann3_unadj, vcov.=function(x) vcovBK(x, method = "arellano", 
                          type="HC1", cluster = "group"))

summary(fixed_hausmann3_unadj)
broom::tidy(fixed_hausmann3)

random_hausmann3_unadj <- plm(log10(capital_.asset_ratio + 1) ~ 
                          lassets + 
                          log10(kkm) + log10(education) + d_fdev + 
                          log10(asset_structure + 1) + 
                          log10(profit_margin) + 
                          currentlegalstatus + age +  year, 
                        data = data, model = "random", 
                        index = c("mfiid", "year"))

random_hausmann3_adj <- coeftest(random_hausmann3_unadj, vcov.=function(x) vcovBK(x, method = "arellano", 
                          type="HC1", cluster = "group"))

summary(random_hausmann1)
rcar1 <- broom::tidy(random_hausmann3)
rcar1

h3 <- broom::tidy(phtest(fixed_hausmann3, random_hausmann3))
h3 ## Choose the fixed effects model*******************************************


h4 <- rbind(h1, h2, h3) ## Hausmann test final table ----
h4
write.csv(h4, "Hausman_Test.csv")
################################################################################
data <- data %>% ungroup()

cap_assets_eq3_unadj <- plm(log10(capital_.asset_ratio + 1) ~ 
                        lassets + 
                        log10(kkm) + log10(education) + d_fdev + 
                        log10(asset_structure + 1) + 
                        log10(profit_margin) + 
                        currentlegalstatus + age +  year, 
                      data = data, model = "within", 
                      index = c("mfiid", "year"))

cap_assets_eq3_adj <- coeftest(cap_assets_eq3_unadj, vcov.=function(x) vcovBK(x, method = "arellano", 
                        type="HC1", cluster = "group"))

summary(cap_assets_eq3)
rcar3 <- broom::tidy(cap_assets_eq3)
rcar3

cap_assets_eq2_unadj <- plm(log10(capital_.asset_ratio + 1) ~ 
                        lassets + 
                        log10(kkm) + log10(education) + d_fdev + 
                        log10(asset_structure + 1) + 
                        log10(profit_margin) + 
                        currentlegalstatus + age +  year, 
                      data = data3years, model = "within", 
                      index = c("mfiid", "year"))

cap_assets_eq2_adj <- coeftest(cap_assets_eq2_unadj, vcov.=function(x) vcovBK(x, method = "arellano", 
                  type="HC1", cluster = "group"))

summary(cap_assets_eq2)
rcar2 <- broom::tidy(cap_assets_eq2)
rcar2

cap_assets_eq4_unadj <- plm(log10(capital_.asset_ratio + 1) ~ 
                        lassets + 
                        log10(kkm) + log10(education) + d_fdev + 
                        log10(asset_structure + 1) + 
                        log10(profit_margin) + 
                        currentlegalstatus + age +  year, 
                      data = data5years, model = "within", 
                      index = c("mfiid", "year"))

cap_assets_eq4_adj <- coeftest(cap_assets_eq4_unadj, vcov.=function(x) vcovBK(x, method = "arellano", 
                            type="HC1", cluster = "group"))

summary(cap_assets_eq4)
rcar4 <- broom::tidy(cap_assets_eq4)
rcar4

pooling3_unadj <- plm(log10(capital_.asset_ratio + 1) ~ 
                  lassets + 
                  log10(kkm) + log10(education) + d_fdev + 
                  log10(asset_structure + 1) + 
                  log10(profit_margin) + 
                  currentlegalstatus + age +  year, 
                data = data, model = "pooling", 
                index = c("mfiid", "year"))

pooling3_adj <- coeftest(pooling3_unadj, vcov.=function(x) vcovBK(x, method = "arellano", 
                              type="HC1", cluster = "group"))

summary(pooling3)
pde3 <- broom::tidy(pooling3)
pde3

##################################################################################
##Stargazer for capital assets ratio 
final_deposits_model <- stargazer(cap_assets_eq3_adj, cap_assets_eq2_adj, 
                                  cap_assets_eq4_adj, random_hausmann3_adj, 
                                  pooling3_adj, cap_assets_wins1_adj,
                                  title= "Regression Capital_Assets Ratio", 
                                  out = "Capital_Assets_Model.html")

final_deposits_model_unadj <- stargazer(cap_assets_eq3_unadj, cap_assets_eq2_unadj, 
                                  cap_assets_eq4_unadj, random_hausmann3_unadj, 
                                  pooling3_unadj, cap_assets_wins1_unadj,
                                  title= "Regression Capital_Assets_Ratio_unadj", 
                                  out = "Capital_Assets_Model_unadj.html")


#################################################################################
### Donations to assets ratio ----
donations_assets_fixed_unadj <- regression_fn(depvar = log10(data$donations_assets_ratio + 1), 
                                        my_data = data, my_model = "within")

summary(donations_assets_fixed)

donations_assets_fixed_adj <- coeftest(donations_assets_fixed_unadj, 
                              vcov.=function(x) vcovBK(x, method = "arellano", 
                                type="HC1", cluster = "group"))

donations_assets_random_unadj <- regression_fn(depvar = log10(data$donations_assets_ratio + 1), 
              my_data = data, my_model = "random")

summary(donations_assets_random)

hausmann_fn(donations_assets_fixed_unadj, donations_assets_random)

donations_assets_random_adj <- coeftest(donations_assets_random_unadj, 
  vcov.=function(x) vcovBK(x, method = "arellano", 
  type="HC1", cluster = "group"))

donations_assets_pooling_unadj <- regression_fn(depvar = log10(data$donations_assets_ratio + 1), 
              my_data = data, my_model = "pooling")

donations_assets_pooling_adj <- coeftest(donations_assets_pooling_unadj, 
              vcov.=function(x) vcovBK(x, method = "arellano", 
              type="HC1", cluster = "group")) 

summary(donations_assets_pooling_adj)

donations_assets_random3years_unadj <- regression_fn(depvar = log10(data3years$donations_assets_ratio + 1), 
  my_data = data3years, my_model = "random") 

donations_assets_random3years_adj <- 
  coeftest(donations_assets_random3years_unadj, 
  vcov.=function(x) vcovBK(x, method = "arellano", 
  type="HC1", cluster = "group"))

summary(donations_assets_random3years_unadj)

donations_assets_random5years_unadj <- regression_fn(depvar = log10(data5years$donations_assets_ratio + 1), 
  my_data = data5years, my_model = "random") 

donations_assets_random5years_adj <- 
  coeftest(donations_assets_random5years_unadj, 
  vcov.=function(x) vcovBK(x, method = "arellano", 
  type="HC1", cluster = "group"))

summary(donations_assets_random5years_adj)



######################################################################################
## Stargazer donations to assets ratio
final_donations_model_adjusted <- stargazer(donations_assets_random_adj, 
                                  donations_assets_random5years_adj, 
                                  donations_assets_random3years_adj, 
                                  donations_assets_fixed_adj, 
                                  donations_assets_pooling_adj, 
                                  donations_assets_random_wins1,
                                  title= "Regression Donations Ratio Adjusted", 
                                  out = "Donations_Model_adjusted.html")


final_donations_model_unadj <- stargazer(donations_assets_random_unadj, 
                                   donations_assets_random5years_unadj, 
                                   donations_assets_random3years_unadj, 
                                   donations_assets_fixed_unadj, 
                                   donations_assets_pooling_unadj, 
                                   donations_assets_random_wins,
                                   title= "Regression Donations Ratio Unadjusted", 
                                   out = "Donations_Model_unadjusted.html")


#################################################################################

## Output regression tables in Ms Word----
# Debt equity ratio 
output1 <- cbind(rde1$term, rde1$estimate, rde1$std.error, rde1$p.value, 
                     rde2$estimate, rde2$std.error, rde2$p.value,
                     rde3$estimate, rde3$std.error, rde3$p.value,
                     pde1$estimate, pde1$std.error, pde1$p.value) %>% 
                     data.frame()


output1
write.csv(output1, "Routput1.csv")

output2 <- cbind(fde1$term, fde1$estimate, fde1$std.error,
                 fde1$p.value) %>% data.frame()
output2
write.csv(output2, "Routput10.csv")

## Capital asset ratio ----
output3 <- cbind(rcar1$term, rcar1$estimate, rcar1$std.error, 
                 rcar1$p.value, pde1$estimate, pde1$std.error, 
                 pde1$p.value) %>% data.frame()

output3
write.csv(output3, "foutput20.csv")

output4 <- rbind(rcar3$term, rcar3$estimate, rcar3$std.error,
                 rcar3$p.value, rcar2$estimate, rcar2$std.error,
                 rcar2$p.value, rcar4$std.error, 
                 rcar4$estimate, rcar4$p.value) %>% t() %>% data.frame()
output4
write.csv(output4, "foutput2.csv")

# Deposits to Total Assets Ratio 
output5 <- cbind(rdta1$term, rdta1$estimate, rdta1$std.error,
                 rdta1$p.value, rdta2$estimate, rdta1$std.error, 
                 rdta2$p.value, rdta3$estimate, rdta3$std.error, 
                 rdta3$p.value, pde2$estimate, pde2$std.error, 
                 pde2$p.value) %>% data.frame()


output5

write.csv(output5, "Routput2.csv")

output6 <- cbind(rdta0$term, rdta0$estimate, rdta0$std.error, 
                 rdta0$p.value) %>% data.frame()
output6
write.csv(output6, "Routput20.csv")

###########################################################################
# Winsorize data ---
data_winsorized0 <- data %>% na.omit()
data_winsorized1 <- sapply(data_winsorized0[,c(18:64, 67)], 
  function(x){winsorize(x, minval = NULL, 
  maxval = NULL, probs = c(0.10, 0.90),
              na.rm = FALSE, type = 7)}) %>% as.data.frame()
data_winsorized1 <- cbind(data_winsorized0[,1:17], data_winsorized1)

###########################################################################
# Run random, fixed and pooled OLS 
## Debt to equity ratio ----
debt_eq_wins1_unadj <- plm(log10(debt_to_equity_ratio + 1) ~ 
                  lassets + log10(kkm) + log10(education) + d_fdev + 
                  log10(asset_structure + 1) + 
                  log10(profit_margin) + 
                  currentlegalstatus + age +  year,
                data = data_winsorized1, model = "random", 
                index = c("mfiid", "year"))

debt_eq_wins1 <- coeftest(debt_eq_wins1, vcov.=function(x) vcovBK(x, method = "arellano", 
                  type="HC1", cluster = "group"))

wins1 <- broom::tidy(debt_eq_wins1)
summary(debt_eq_wins1)

debt_eq_wins2 <- plm(log10(debt_to_equity_ratio + 1) ~ 
                       lassets + log10(kkm) + log10(education) + d_fdev + 
                       log10(asset_structure + 1) + 
                       log10(profit_margin) + 
                       currentlegalstatus + age +  year,
                     data = data_winsorized1, model = "pooling", 
                     index = c("mfiid", "year"))

coeftest(debt_eq_wins2, vcov.=function(x) vcovBK(x, method = "arellano", 
              type="HC1", cluster = "group"))

wins2 <- broom::tidy(debt_eq_wins2)
summary(debt_eq_wins2)

debt_eq_wins3 <- plm(log10(debt_to_equity_ratio + 1) ~ 
                       lassets + log10(kkm) + log10(education) + d_fdev + 
                       log10(asset_structure + 1) + 
                       log10(profit_margin) + 
                       currentlegalstatus + age +  year,
                     data = data_winsorized1, model = "within", 
                     index = c("mfiid", "year"))

coeftest(debt_eq_wins3, vcov.=function(x) vcovBK(x, method = "arellano", 
        type="HC1", cluster = "group"))

wins3 <- broom::tidy(debt_eq_wins3)
summary(debt_eq_wins3)


#################################################################################
winsorized_regressions_adj <- stargazer(debt_eq_wins1, 
                                        cap_assets_wins1_adj,
                                        
                                         
                                         title= "Regression Donations Ratio Unadjusted", 
                                         out = "Donations_Model_unadjusted.html")



winsorized_regressions_unadj <- stargazer(debt_eq_wins1_unadj, 
                                          cap_assets_wins1_unadj,
                                          
                                        
                                        title= "Regression Donations Ratio Unadjusted", 
                                        out = "Donations_Model_unadjusted.html")


#################################################################################


# Capital to assets ratio----
cap_assets_wins1_unadj <- plm(log10(capital_.asset_ratio + 1) ~ 
                        lassets + 
                        log10(kkm) + log10(education) + d_fdev + 
                        log10(asset_structure + 1) + 
                        log10(profit_margin) + 
                        currentlegalstatus + age +  year, 
                      data = data_winsorized1, model = "random", 
                      index = c("mfiid", "year"))

cap_assets_wins1_adj <- coeftest(cap_assets_wins1_unadj, vcov.=function(x) vcovBK(x, method = "arellano", 
                            type="HC1", cluster = "group"))

wins4 <- broom::tidy(cap_assets_wins1)
summary(cap_assets_wins1)

cap_assets_wins2 <- plm(log10(capital_.asset_ratio + 1) ~ 
                          lassets + 
                          log10(kkm) + log10(education) + d_fdev + 
                          log10(asset_structure + 1) + 
                          log10(profit_margin) + 
                          currentlegalstatus + age +  year, 
                        data = data_winsorized1, model = "pooling", 
                        index = c("mfiid", "year"))

coeftest(cap_assets_wins2, vcov.=function(x) vcovBK(x, method = "arellano", 
                    type="HC1", cluster = "group"))

wins5 <- broom::tidy(cap_assets_wins2)
summary(cap_assets_wins2)

cap_assets_wins3 <- plm(log10(capital_.asset_ratio + 1) ~ 
                          lassets + 
                          log10(kkm) + log10(education) + d_fdev + 
                          log10(asset_structure + 1) + 
                          log10(profit_margin) + 
                          currentlegalstatus + age +  year, 
                        data = data_winsorized1, model = "within", 
                        index = c("mfiid", "year"))

coeftest(cap_assets_wins3, vcov.=function(x) vcovBK(x, method = "arellano", 
                  type="HC1", cluster = "group"))

wins6 <- broom::tidy(cap_assets_wins3)
summary(cap_assets_wins3)

# Deposits to assets ratio ----
deposits_to_total_assets1 <- plm(log10(deposits_to_total_assets + 1) ~ 
                          lassets + 
                          log10(kkm) + log10(education) + d_fdev + 
                          log10(asset_structure + 1) + 
                          log10(profit_margin) + 
                          currentlegalstatus + age +  year, 
                        data = data_winsorized1, model = "random", 
                        index = c("mfiid", "year"))

deposits_to_total_assets1_adj <- coeftest(deposits_to_total_assets1, vcov.=function(x) vcovBK(x, method = "arellano", 
                        type="HC1", cluster = "group"))

wins7 <- broom::tidy(deposits_to_total_assets1)
summary(deposits_to_total_assets1)

deposits_to_total_assets2 <- plm(log10(deposits_to_total_assets + 1) ~ 
                                   lassets + 
                                   log10(kkm) + log10(education) + d_fdev + 
                                   log10(asset_structure + 1) + 
                                   log10(profit_margin) + 
                                   currentlegalstatus + age +  year, 
                                 data = data_winsorized1, model = "pooling", 
                                 index = c("mfiid", "year"))

coeftest(deposits_to_total_assets2, vcov.=function(x) vcovBK(x, method = "arellano", 
                    type="HC1", cluster = "group"))

wins8 <- broom::tidy(deposits_to_total_assets2)
summary(deposits_to_total_assets2)

deposits_to_total_assets3 <- plm(log10(deposits_to_total_assets + 1) ~ 
                                   lassets + 
                                   log10(kkm) + log10(education) + d_fdev + 
                                   log10(asset_structure + 1) + 
                                   log10(profit_margin) + 
                                   currentlegalstatus + age +  year, 
                                 data = data_winsorized1, model = "within", 
                                 index = c("mfiid", "year"))

coeftest(deposits_to_total_assets3, vcov.=function(x) vcovBK(x, method = "arellano", 
                type="HC1", cluster = "group"))

wins9 <- broom::tidy(deposits_to_total_assets3)
summary(deposits_to_total_assets3)

## Donations to assets ratio  ----
# random effects 
donations_assets_fixed_wins <- regression_fn(data_winsorized1$donations_assets_ratio, 
                                              my_data = data_winsorized1, my_model = "within")

summary(donations_assets_fixed_wins)

donations_assets_fixed_wins <- coeftest(donations_assets_fixed_wins, vcov.=function(x) vcovBK(x, method = "arellano", 
                                                             type="HC1", cluster = "group")) %>% 
  broom::tidy()

## pooled OLs
donations_assets_pooled_wins <- regression_fn(data_winsorized1$donations_assets_ratio, 
                                              my_data = data_winsorized1, my_model = "pooling")

summary(donations_assets_pooled_wins)

donations_assets_pooled_wins1 <- coeftest(donations_assets_pooled_wins, vcov.=function(x) vcovBK(x, method = "arellano", 
                                                                type="HC1", cluster = "group")) %>% 
  broom::tidy()

# Random effects ----
donations_assets_random_wins <- regression_fn(data_winsorized1$donations_assets_ratio, 
                                              my_data = data_winsorized1, my_model = "random")

summary(donations_assets_random_wins)

donations_assets_random_wins1 <- coeftest(donations_assets_random_wins, 
                                         vcov.=function(x) vcovBK(x, method = "arellano", 
                                          type="HC1", cluster = "group")) 


################################################################################
winsorized_regressions <- cbind(wins3$term, wins3$estimate, wins3$std.error,
                                wins3$p.value, wins6$estimate, wins6$std.error,
                                wins6$p.value, wins9$estimate, wins9$std.error,
                                wins9$p.value, donations_assets_fixed_wins$estimate, 
                                donations_assets_fixed_wins$std.error, 
                                donations_assets_fixed_wins$p.value)

write.csv(winsorized_regressions, "winsorized_regressions.csv")

winsorized_regressions_2 <- cbind(wins1$term, wins1$estimate, wins1$std.error, 
                                  wins1$p.value, 
                                  wins2$estimate, wins2$std.error, wins2$p.value, 
                                  wins4$estimate, wins4$std.error, wins4$p.value, 
                                  wins5$estimate, wins5$std.error, wins5$p.value,
                                  wins7$estimate, wins7$std.error, wins7$p.value, 
                                  wins8$estimate, wins8$std.error, wins8$p.value, 
                                  donations_assets_random_wins$estimate, 
                                  donations_assets_random_wins$std.error, 
                                  donations_assets_random_wins$p.value,
                                  donations_assets_pooled_wins$estimate, 
                                  donations_assets_pooled_wins$std.error, 
                                  donations_assets_pooled_wins$p.value)
                                  
write.csv(winsorized_regressions_2, "winsorized_regressions_2.csv")


## Model diagnostics ----
# Random vs Pooled OLS
plm_test1 <- broom::tidy(plmtest(random_hausmann1, type = c("bp")))
plm_test2 <- broom::tidy(plmtest(random_hausmann2, type = c("bp")))
plm_test3 <- broom::tidy(plmtest(donations_assets_random, type = c("bp")))
plm_test2
LM_test <- rbind(plm_test1, plm_test2)
LM_test
write.csv(LM_test, "LM_test.csv")

## Fixed vs pooled OLS
broom::tidy(pFtest(fixed_hausmann3, pooling3))

# Serial correlation ----
pcd1 <- broom::tidy(pcdtest(random_hausmann1, test = c("lm"))) # debt equity 
pcd2 <- broom::tidy(pcdtest(fixed_hausmann3, test = c("lm"))) # capital assets ratio
pcd3 <- broom::tidy(pcdtest(random_hausmann2, test = c("lm"))) # deposits to assets ratio
pcd4 <- broom::tidy(pcdtest(donations_assets_random, test = c("lm")))
autocorrelation <- rbind(pcd1, pcd2, pcd3)
write_csv(autocorrelation, "autocorrelation.csv")

# Cross sectional dependence ## Presence of cross sectional dependence 
pcd5 <- broom::tidy(pcdtest(random_hausmann1, test = c("cd")))# debt equity 
pcd6 <- broom::tidy(pcdtest(fixed_hausmann3, test = c("cd"))) # capital assets ratio
pcd7 <- broom::tidy(pcdtest(random_hausmann2, test = c("cd"))) # deposits to assets ratio
pcd7 <- broom::tidy(pcdtest(donations_assets_random, test = c("cd")))
cross_sectional_dep <- rbind(pcd4, pcd5, pcd6)
write_csv(cross_sectional_dep, "cross_sectional_dep.csv")

## Within and between estimators for all dep variables ----
between_debt_eq <- plm(log10(data$debt_to_equity_ratio + 1) ~ 
                        lassets + 
                        log10(kkm) + log10(education) + d_fdev + 
                        log10(asset_structure + 1) + 
                        log10(profit_margin) + 
                        currentlegalstatus + age +  year, 
                      data = data, model = "between", 
                      index = c("mfiid", "year"))

broom::tidy(between_debt_eq)
summary(between_debt_eq)

between_debt_eq1 <- plm(log10(data$debt_to_equity_ratio + 1) ~ 
                         lassets + 
                         log10(kkm) + log10(education) + d_fdev + 
                         log10(asset_structure + 1) + 
                         log10(profit_margin) + 
                         currentlegalstatus + age +  year, 
                       data = data, model = "fd", 
                       index = c("mfiid", "year"))

broom::tidy(between_debt_eq1)
summary(between_debt_eq1)


between_cap_ass <- plm(log10(data$capital_.asset_ratio + 1) ~ 
                         lassets + 
                         log10(kkm) + log10(education) + d_fdev + 
                         log10(asset_structure + 1) + 
                         log10(profit_margin) + 
                         currentlegalstatus + age +  year, 
                       data = data, model = "between", 
                       index = c("mfiid", "year"))

broom::tidy(between_cap_ass)
summary(between_cap_ass)

between_cap_ass1 <- plm(log10(data$capital_.asset_ratio + 1) ~ 
                         lassets + 
                         log10(kkm) + log10(education) + d_fdev + 
                         log10(asset_structure + 1) + 
                         log10(profit_margin) + 
                         currentlegalstatus + age +  year, 
                       data = data, model = "fd", 
                       index = c("mfiid", "year"))

broom::tidy(between_cap_ass1)
summary(between_cap_ass1)


between_dep_ass <- plm(log10(data$deposits_to_total_assets + 1) ~ 
                         lassets + 
                         log10(kkm) + log10(education) + d_fdev + 
                         log10(asset_structure + 1) + 
                         log10(profit_margin) + 
                         currentlegalstatus + age +  year, 
                       data = data, model = "between", 
                       index = c("mfiid", "year"))

broom::tidy(between_dep_ass)
summary(between_dep_ass)

between_dep_ass1 <- plm(log10(data$deposits_to_total_assets + 1) ~ 
                         lassets + 
                         log10(kkm) + log10(education) + d_fdev + 
                         log10(asset_structure + 1) + 
                         log10(profit_margin) + 
                         currentlegalstatus + age +  year, 
                       data = data, model = "fd", 
                       index = c("mfiid", "year"))

broom::tidy(between_dep_ass1)
summary(between_dep_ass1)

## Check for variance inflation factors ----
vif1 <- car::vif(pooling1) %>% tibble()
vif2 <- car::vif(pooling2) %>% tibble()
vif3 <- car::vif(pooling3) %>% tibble()
vif_final <- cbind(vif1, vif2, vif3)
vif_final
write.csv(vif_final, "vif_final.csv")

## Normality of residuals  random effects models ----
# Debt to equity ratio random effects model 
qqnorm(residuals(random_hausmann1), ylab = 'Residuals', col = "red", 
       main = "Debt to Equity Ratio Normal QQ-Plot- Random Effects Model")
qqline(residuals(random_hausmann1))

# Capital to assets ratio random effects model 
qqnorm(residuals(random_hausmann3), ylab = 'Residuals', col = "red", 
       main = "Capital Assets Ratio Normal QQ-Plot- Random Effects Model")
qqline(residuals(random_hausmann3))

# Deposits to assets ratio random effects model 
qqnorm(residuals(random_hausmann2), ylab = 'Residuals', col = "red", 
       main = "Deposits to Assets Ratio Normal QQ-Plot- Random Effects Model")
qqline(residuals(random_hausmann2))

## Normality of residuals fixed effects models ----
qqnorm(residuals(fixed_hausmann1), ylab = 'Residuals', col = "red", 
       main = "Debt to Equity Ratio Normal QQ-Plot- Fixed Effects Model") 
qqline(residuals(fixed_hausmann1))

# Capital to assets ratio random effects model 
qqnorm(residuals(fixed_hausmann3), ylab = 'Residuals', col = "red", 
       main = "Capital Assets Ratio Normal QQ-Plot- Fixed Effects Model")
qqline(residuals(fixed_hausmann3))

# Deposits to assets ratio random effects model 
qqnorm(residuals(fixed_hausmann2), ylab = 'Residuals', col = "red", 
       main = "Deposits to Assets Ratio Normal QQ-Plot- Fixed Effects Model")
qqline(residuals(fixed_hausmann2))

# Donations to assets ratio fixed effects model 
qqnorm(residuals(donations_assets_fixed), ylab = 'Residuals', col = "red", 
       main = "Donations to Assets Ratio Normal QQ-Plot- Fixed Effects Model")

qqline(residuals(donations_assets_fixed))


qqnorm(residuals(donations_assets_random), ylab = 'Residuals', col = "red", 
       main = "Donations to Assets Ratio Normal QQ-Plot- Random Effects Model")

qqline(residuals(donations_assets_random))

#Winsorozed data ----
qqnorm(residuals(donations_assets_fixed_wins), ylab = 'Residuals', col = "red", 
       main = "Donations to Assets Ratio Normal QQ-Plot- Fixed Effects Model")

qqline(residuals(donations_assets_fixed_wins))


qqnorm(residuals(donations_assets_random), ylab = 'Residuals', col = "red", 
       main = "Donations to Assets Ratio Normal QQ-Plot- Random Effects Model")

qqline(residuals(donations_assets_random))


## Plot comprehensive ggally graphs for all variables ----
ggally_data <- data %>% ungroup() %>% select(mfiid, year, debt_to_equity_ratio, 
                capital_.asset_ratio, 
                deposits_to_total_assets, assets, kkm, 
                education, d_fdev, asset_structure, 
                profit_margin, currentlegalstatus, age) %>% 
                mutate(deposits_to_total_assets = 
                deposits_to_total_assets + 1, asset_structure = 
                  asset_structure + 1)

ggally_data$debt_to_equity_ratio <- log10(ggally_data$debt_to_equity_ratio)
ggally_data$capital_.asset_ratio <- log10(ggally_data$capital_.asset_ratio)
ggally_data$deposits_to_total_assets <- log10(ggally_data$deposits_to_total_assets)
ggally_data$assets <- log10(ggally_data$assets)
ggally_data$kkm <- log10(ggally_data$kkm)
ggally_data$education <- log10(ggally_data$education)
ggally_data$asset_structure <- log10(ggally_data$asset_structure)
ggally_data$profit_margin <- log10(ggally_data$profit_margin)

ggally_data %>% select(-mfiid, -year) %>% 
  ggpairs(aes(color = currentlegalstatus))

## Summary statistics of raw data of capital 
# donations, debt/equity, capital, deposits/assets 
dep_var_raw_data_summary <- data %>% group_by(currentlegalstatus) %>% 
  skim(debt_to_equity_ratio, capital_.asset_ratio, 
       deposits_to_total_assets, donations_assets_ratio) %>% 
  select(-c(1, 4, 5, 13))


write.csv(dep_var_raw_data_summary, 
          "dep_var_raw_data_summary.csv")





data %>% select(currentlegalstatus, age, donations_assets_ratio) %>% 
  na.omit() %>% ggplot(aes(x = reorder(currentlegalstatus, 
  donations_assets_ratio, median_n), y = donations_assets_ratio, 
  fill = currentlegalstatus)) + geom_violin(draw_quantiles = c(0.25, 0.5, 0.75), 
  scale = "count") + scale_y_log10() + 
  labs(x = "", y = "", 
       title = "Donations to Assets Ratio by MFI Legal Status") + theme_minimal() +
  stat_summary(fun = mean_n, geom = "point", size = 1, color = "red") + 
  theme(axis.text.x = element_blank()) + 
  theme(legend.title = element_blank()) + theme(legend.position = "bottom") +
  theme(legend.key.size = unit(0.4, "cm"))


length(which(data$donations_assets_ratio > 1))
length(which(data$deposits_to_total_assets > 1))
length(which(data$capital_.asset_ratio > 1))
length(which(data$debt_to_equity_ratio > 1))

data[which(data$donations_assets_ratio >= 1), 1:7]











