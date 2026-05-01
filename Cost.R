#cost: additional OOP incurred to patients by quintile and by rural urban 
#catastrophic health expenditure cases 


input <- read.csv("input table new.csv")
input$Value <- as.numeric(gsub(",", "", input$Value)) 

get_val <- function(name) input$Value[input$Parameters == name]
quintiles <- 1:5

#section 2 OOP cost due to intervention 
#2.1 Direct medical OOP
#number of DR patients gets treated at baseline
treated_baseline <- sapply(quintiles, function(i) {
  get_val(paste0("pop_", i)) *
    get_val(paste0("dr_", i)) *
    get_val(paste0("pcu_", i)) *
    get_val("cov_b") *
    get_val("sens_b") *
    (1 - get_val("lfu_referral")) *
    (1 - get_val("lfu_treat"))})

#number of DR patients gets treated after intervention 
treated_inv <- sapply(quintiles, function(i) {
  get_val(paste0("pop_", i)) *
    get_val(paste0("dr_", i)) *
    get_val(paste0("pcu_", i)) *
    get_val("cov_inv") *
    get_val("sens_inv") *
    (1 - get_val("lfu_referral")) *
    (1 - get_val("lfu_treat"))})

#direct medical OOP at baseline per quintile 
direct_med_OOP_baseline <- treated_baseline*sapply(quintiles, function(i) {
  get_val(paste0("oop_med_dr_", i))*
    (1-get_val(paste0("vtdr_dr_", i)))+
    get_val(paste0("oop_med_vtdr_", i))*
    get_val(paste0("vtdr_dr_", i))})

#direct medical OOP after intervention per quintile
direct_med_OOP_inv <- treated_inv*sapply(quintiles, function(i) {
  get_val(paste0("oop_med_dr_", i))*
    (1-get_val(paste0("vtdr_dr_", i))*(1-get_val("vtdr_reduction")))+
    get_val(paste0("oop_med_vtdr_", i))*
    get_val(paste0("vtdr_dr_", i))*
    (1 - get_val("vtdr_reduction"))})

#Additional direct medical OOP due to intervention
OOP_med_additional <- direct_med_OOP_inv-direct_med_OOP_baseline

#2.2 ALL OOP (medical + non-medical)
#direct non-medical OOP at baseline 
direct_nonmed_OOP_baseline <- treated_baseline*sapply(quintiles, function(i) {
  get_val(paste0("oop_nonmed_dr_", i))*
    (1-get_val(paste0("vtdr_dr_", i)))+
    get_val(paste0("oop_nonmed_vtdr_", i))*
    get_val(paste0("vtdr_dr_", i))})

#direct non-medical OOP after intervention  
direct_nonmed_OOP_inv <- treated_inv*sapply(quintiles, function(i) {
  get_val(paste0("oop_nonmed_dr_", i))*
    (1-get_val(paste0("vtdr_dr_", i))*(1-get_val("vtdr_reduction")))+
    get_val(paste0("oop_nonmed_vtdr_", i))*
    get_val(paste0("vtdr_dr_", i))*
    (1 - get_val("vtdr_reduction"))})

#Additional total OOP due to intervention
OOP_total_additional <- (direct_med_OOP_inv+direct_nonmed_OOP_inv)-
  (direct_med_OOP_baseline+direct_nonmed_OOP_baseline)

#Plots
library(ggplot2)
plot_dr <- data.frame(
  quintile = factor(c("I","II","III","IV","V"), levels=c("I","II","III","IV","V")),
  dr_detected = dr_detected)

#plot of additional direct medical OOP due to intervention
ggplot(plot_dr, aes(x=quintile, y=OOP_med_additional)) +
  geom_col(fill="coral1", width=0.6)+
  labs(title="Additional Direct Medical OOP Due to Intervention",
       x="Income Quintile (Poorest to Richest)",
       y="Additional Direct Medical OOP (INR)") +
  theme_classic()+
  theme(plot.title=element_text(hjust=0.5, face="bold"))

#plot of additional total OOP due to intervention
ggplot(plot_dr, aes(x=quintile, y=OOP_total_additional)) +
  geom_col(fill="coral1", width=0.6)+
  labs(title="Additional Total OOP Due to Intervention",
       x="Income Quintile (Poorest to Richest)",
       y="Additional Medical and Non-medical OOP (INR)") +
  theme_classic()+
  theme(plot.title=element_text(hjust=0.5, face="bold"))

#2.3 Catastrophic Health Expenditure 

# Per-person total OOP — baseline 
per_person_oop_baseline <- sapply(quintiles, function(i) {
  vtdr_b <- get_val(paste0("vtdr_dr_", i))
  
  med    <- get_val(paste0("oop_med_dr_", i))    * (1 - vtdr_b) +
    get_val(paste0("oop_med_vtdr_", i))  * vtdr_b
  nonmed <- get_val(paste0("oop_nonmed_dr_", i)) * (1 - vtdr_b) +
    get_val(paste0("oop_nonmed_vtdr_", i)) * vtdr_b
  
  med + nonmed
})

# Per-person total OOP — intervention (VTDR proportion reduced by vtdr_reduction)
per_person_oop_inv <- sapply(quintiles, function(i) {
  vtdr_inv <- get_val(paste0("vtdr_dr_", i)) * (1 - get_val("vtdr_reduction"))
  
  med    <- get_val(paste0("oop_med_dr_", i))    * (1 - vtdr_inv) +
    get_val(paste0("oop_med_vtdr_", i))  * vtdr_inv
  nonmed <- get_val(paste0("oop_nonmed_dr_", i)) * (1 - vtdr_inv) +
    get_val(paste0("oop_nonmed_vtdr_", i)) * vtdr_inv
  
  med + nonmed
})

#Annunal consumption
annual_consumption_q <- sapply(quintiles, function(i) {
  get_val(paste0("mcpe_", i)) * 12
  })

#CHE thresholds
th1 <- get_val("th1")   # 0.10
th2 <- get_val("th2")   # 0.25

#CHE cases per quintile at baseline and after intervention 
che_10_cases_baseline   <- as.integer(per_person_oop_baseline > th1 * annual_consumption_q)
che_25_cases_baseline   <- as.integer(per_person_oop_baseline > th2 * annual_consumption_q)
che_10_cases_inv <- as.integer(per_person_oop_inv > th1 * annual_consumption_q)
che_25_cases_inv <- as.integer(per_person_oop_inv > th2 * annual_consumption_q)

# Total CHE cases = number treated × indicator
che_10_baseline <- treated_baseline * che_10_cases_baseline 
che_25_baseline <- treated_baseline * che_25_cases_baseline 
che_10_inv      <- treated_inv * che_10_cases_inv
che_25_inv      <- treated_inv * che_25_cases_inv

# CHE averted
che_10_additional <- che_10_inv - che_10_baseline
che_25_additional <- che_25_inv - che_25_baseline

#graphs
ggplot(plot_dr, aes(x=quintile, y=che_10_additional)) +
  geom_col(fill="red2", width=0.6)+
  labs(title="Additional CHE Cases (10% threshold) Due to Intervention",
       x="Income Quintile (Poorest to Richest)",
       y="Additional CHE Cases") +
  theme_classic()+
  theme(plot.title=element_text(hjust=0.5, face="bold"))

ggplot(plot_dr, aes(x=quintile, y=che_25_additional)) +
  geom_col(fill="red2", width=0.6)+
  labs(title="Additional CHE Cases (25% threshold) Due to Intervention",
       x="Income Quintile (Poorest to Richest)",
       y="Additional CHE Cases") +
  theme_classic()+
  theme(plot.title=element_text(hjust=0.5, face="bold"))

#section 2.3 impoverishment due to medical expense 
#annual poverty 
pov_annual <- get_val("pov") * 365

#consumption after OOP
post_oop_consumption_baseline <- annual_consumption_q - per_person_oop_baseline
post_oop_consumption_inv      <- annual_consumption_q - per_person_oop_inv

#impoverishment cases at baseline and after intervention 
impov_cases_baseline   <- as.integer(annual_consumption_q >= pov_annual &           # was above
                                     post_oop_consumption_baseline < pov_annual   # now below  
                                     )
impov_cases_inv  <- as.integer(annual_consumption_q >= pov_annual &           # was above
                               post_oop_consumption_inv < pov_annual)   # now below  

#total impoverishment cases per quintile 
impov_baseline <- treated_baseline * impov_cases_baseline
impov_inv      <- treated_inv * impov_cases_inv

impov_additional <- impov_inv - impov_baseline

ggplot(plot_dr, aes(x=quintile, y=impov_additional)) +
  geom_col(fill="red2", width=0.6)+
  labs(title="Additional Impoverishment Cases Due to Intervention",
       x="Income Quintile (Poorest to Richest)",
       y="Additional Impoverishment Cases") +
  theme_classic()+
  theme(plot.title=element_text(hjust=0.5, face="bold"))

