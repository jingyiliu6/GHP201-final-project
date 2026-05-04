
#health benefit calculation and disaggregation by quintile 
#cases of DR detected and cases of blindness averted due to intervention
rm(list = ls())
input <- read.csv("input table new.csv")
input$Parameters <- trimws(input$Parameters)
input$Value <- as.numeric(gsub(",", "", input$Value)) 


get_val <- function(name) input$Value[input$Parameters == name]
quintiles <- 1:5
scale_to_target_pop <- get_val("target_pop") / get_val("pop")
plain_number <- function(x) format(round(x, 0), scientific = FALSE, trim = TRUE)

#section 1: Additional DR cases detected due to intervention  
#1.1 estimating the cases of DR detected before the screening intervention in each income quintile

dr_baseline <- sapply(quintiles, function(i) {
  get_val(paste0("pop_", i)) *
    get_val(paste0("dr_", i)) *
    get_val(paste0("pcu_", i)) *
    get_val("cov_b") *
    get_val("sens_b")})

#1.2 estimating the cases of DR detected after the screening intervention in each income quintile
dr_inv <- sapply(quintiles, function(i) {
  get_val(paste0("pop_", i)) *
    get_val(paste0("dr_", i)) *
    get_val(paste0("pcu_", i)) *
    get_val("cov_inv") *
    get_val("sens_inv")})

#1.3 estimating the difference in number of DR cases detected per quintile and plot 

dr_detected <- dr_inv - dr_baseline
dr_detected_target <- dr_detected * scale_to_target_pop

library(ggplot2)
plot_dr <- data.frame(
  quintile = factor(c("I","II","III","IV","V"), levels=c("I","II","III","IV","V")),
  dr_detected = dr_detected_target)

ggplot(plot_dr, aes(x=quintile, y=dr_detected)) +
  geom_col(fill="orangered4", width=0.6)+
  geom_text(aes(label = plain_number(dr_detected)), vjust = -0.4, size = 3) +
  scale_y_continuous(labels = plain_number,
                     expand = expansion(mult = c(0, 0.1))) +
  labs(title="Additional DR Cases Detected Due to Intervention",
       x="Income Quintile (Poorest to Richest)",
       y="Additional DR Cases Detected") +
  theme_classic()+
  theme(plot.title=element_text(hjust=0.5, face="bold"))

#section 2: Number of blindness cases averted due to intervention
#2.1 estimating blindness cases due to DR before the screening intervention in each income quintile

blind_baseline <-  sapply(quintiles, function(i) {
  get_val(paste0("pop_", i)) *
    get_val(paste0("dr_", i)) *
    get_val(paste0("pcu_", i)) *
    get_val("cov_b") *
    get_val("sens_b") *
    (1 - get_val("lfu_referral")) *
    (1 - get_val("lfu_treat")) *
    get_val(paste0("vtdr_dr_", i)) *
    get_val("eff_blind")})

#2.2 estimating blindness cases due to DR after the screening intervention in each income quintile
blind_inv <-  sapply(quintiles, function(i) {
  get_val(paste0("pop_", i)) *
    get_val(paste0("dr_", i)) *
    get_val(paste0("pcu_", i)) *
    get_val("cov_inv") *
    get_val("sens_inv") *
    (1 - get_val("lfu_referral")) *
    (1 - get_val("lfu_treat")) *
    get_val(paste0("vtdr_dr_", i)) *
    (1 - get_val("vtdr_reduction"))*
    get_val("eff_blind")})

#1.3 estimating blindness cases averted per quintile and plot
blind_averted <- blind_inv - blind_baseline
blind_averted_target <- blind_averted * scale_to_target_pop

plot_blind <- data.frame(
  quintile = factor(c("I","II","III","IV","V"), levels=c("I","II","III","IV","V")),
  blind_averted = blind_averted_target)

ggplot(plot_blind, aes(x=quintile, y=blind_averted)) +
  geom_col(fill="orangered", width=0.6)+
  geom_text(aes(label = plain_number(blind_averted)), vjust = -0.4, size = 3) +
  scale_y_continuous(labels = plain_number,
                     expand = expansion(mult = c(0, 0.1))) +
  labs(title="Blindness Cases Averted Due to Intervention",
       x="Income Quintile (Poorest to Richest)",
       y="Blindness Cases Averted") +
  theme_classic()+
  theme(plot.title=element_text(hjust=0.5, face="bold"))

#dashboard

health_benefit_dash <- as.data.frame(rbind(
  round(dr_detected_target, 0),
  round(blind_averted_target, 0)
))
colnames(health_benefit_dash) <- c("I","II","III","IV","V")
rownames(health_benefit_dash) <- c("Additional DR Cases Detected (scaled to target population)",
                                   "Blindness Cases Averted (scaled to target population)")
print(health_benefit_dash)
