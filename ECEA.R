
#disease burden calculation and disaggregation 
#cases of DR detected and cases of blindness averted due to intervention
rm(list=ls()) 
input <- read.csv("Input table.csv")
input$Value <- as.numeric(gsub(",", "", input$Value)) 

set.seed(123)

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
  geom_col(fill="red2", width=0.6)+
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
    get_val("eff_blind")})

#1.3 estimating blindness cases averted per quintile and plot
blind_averted <- blind_inv - blind_baseline

plot_blind <- data.frame(
  quintile = factor(c("I","II","III","IV","V"), levels=c("I","II","III","IV","V")),
  blind_averted = blind_averted)

ggplot(plot_blind, aes(x=quintile, y=blind_averted)) +
  geom_col(fill="red2", width=0.6)+
  labs(title="Blindness Cases Averted Due to Intervention",
       x="Income Quintile (Poorest to Richest)",
       y="Blindness Cases Averted") +
  theme_classic()+
  theme(plot.title=element_text(hjust=0.5, face="bold"))

###############################################################################
################################################################################
#3. FRP benefits per income quintile

#3.1 Simulating incomes from income distribution and assigning to individuals seeking DR care
gini       <- get_val("gini")
ave_income <- get_val("mean_income")

phi  <- 1 / gini
beta <- (1 / phi) * ave_income

incomes <- rgamma(n = 1000000, shape = phi, rate = 1 / beta)

qt <- quantile(incomes, probs = seq(0, 1, by = 0.20), na.rm = TRUE)
# extracting income quintile cutoffs

# generating incomes for each individual per quintile
pop_total <- get_val("pop")

pop_incomes <- matrix(NA, nrow = 5, ncol = pop_total / 5)

for (i in 1:5) {
  pool <- incomes[incomes >= qt[i] & incomes < qt[i + 1]]
  pop_incomes[i, ] <- sample(pool, pop_total / 5, replace = TRUE)
}
# replace = TRUE guards against edge cases where pool is slightly smaller than needed

# generating incomes for individuals per quintile who have DR and utilize care
pop_incomes_u <- lapply(quintiles, function(i) {
  n_seekers <- round(get_val(paste0("pop_", i)) *
                       get_val(paste0("dr_",  i)) *   # DR prevalence among diabetics
                       get_val(paste0("pcu_",  i)))     # utilization/care-seeking rate
  sample(pop_incomes[i, ], n_seekers, replace = TRUE)
})

#3.2 Estimating catastrophic health expenditures (CHE)

th1 <- get_val("th1")
th2 <- get_val("th2")

# Baseline: OOP without DR screening intervention (late-stage treatment costs)
cata_baseline <- sapply(quintiles, function(i) {
  sum(get_val(paste0("oop_b_", i)) > th1 * pop_incomes_u[[i]])
})

# Intervention: OOP with DR screening 
cata_inv <- sapply(quintiles, function(i) {
  sum(get_val(paste0("oop_tr_", i)) > th1 * pop_incomes_u[[i]])
})

# CHE cases averted by intervention
cata_averted <- cata_baseline - cata_inv

# Plot
df_che <- data.frame(
  quintile     = factor(c("I","II","III","IV","V"), levels = c("I","II","III","IV","V")),
  cata_averted = cata_averted
)

ggplot(df_che, aes(x = quintile, y = cata_averted)) +
  geom_col(fill = "red2", width = 0.6) +
  labs(title = "DR-Related CHE Cases Averted by Screening Intervention",
       x = "Income Quintile (Poorest to Richest)",
       y = "CHE Cases Averted") +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))