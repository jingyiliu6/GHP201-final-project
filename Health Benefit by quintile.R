
#health benefit calculation and disaggregation by quintile 
#cases of DR detected and cases of blindness averted due to intervention

input <- read.csv("input table new.csv")
input$Value <- as.numeric(gsub(",", "", input$Value)) 



get_val <- function(name) input$Value[input$Parameters == name]
quintiles <- 1:5

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

library(ggplot2)
plot_dr <- data.frame(
  quintile = factor(c("I","II","III","IV","V"), levels=c("I","II","III","IV","V")),
  dr_detected = dr_detected)

ggplot(plot_dr, aes(x=quintile, y=dr_detected)) +
  geom_col(fill="darkturquoise", width=0.6)+
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

plot_blind <- data.frame(
  quintile = factor(c("I","II","III","IV","V"), levels=c("I","II","III","IV","V")),
  blind_averted = blind_averted)

ggplot(plot_blind, aes(x=quintile, y=blind_averted)) +
  geom_col(fill="darkturquoise", width=0.6)+
  labs(title="Blindness Cases Averted Due to Intervention",
       x="Income Quintile (Poorest to Richest)",
       y="Blindness Cases Averted") +
  theme_classic()+
  theme(plot.title=element_text(hjust=0.5, face="bold"))

#dashboard

health_benefit_dash <- as.data.frame(rbind(
  round(dr_detected, 1),
  round(blind_averted, 3)
))
colnames(health_benefit_dash) <- c("I","II","III","IV","V")
rownames(health_benefit_dash) <- c("Additional DR Cases Detected (per 100,000)",
                                   "Blindness Cases Averted (per 100,000)")
print(health_benefit_dash)