rm(list = ls())
input <- read.csv("Input table.csv")
input$Value <- as.numeric(gsub(",", "", input$Value))

set.seed(123)
library(ggplot2)
library(data.table)

get_val <- function(name) input$Value[input$Parameters == name]
quintiles <- 1:5

#Simulate income distribution
gini       <- get_val("gini")
ave_income <- get_val("mean_income")
pop_total  <- get_val("pop")
phi  <- 1 / gini
beta <- (1 / phi) * ave_income
all_incomes <- rgamma(n = pop_total, shape = phi, rate = 1 / beta)

qt_cuts <- quantile(all_incomes, probs = seq(0, 1, by = 0.20))

#2. Build individual-level table (one row per diabetic person)
rows <- list()

for (i in quintiles) {
  n_i        <- get_val(paste0("pop_", i))           # diabetic pop in quintile
  dr_prev_i  <- get_val(paste0("dr_", i))            # P(DR | diabetic)
  vtdr_dr_i  <- get_val(paste0("vtdr_dr_", i))       # P(VTDR | DR)
  pcu_i      <- get_val(paste0("pcu_", i))           # primary-care utilisation
  oop_b_i    <- get_val(paste0("oop_b_", i))         # OOP baseline (late-stage)
  oop_tr_i   <- get_val(paste0("oop_tr_", i))        # OOP intervention (early tx)
  
  # draw incomes from the quintile pool
  pool <- all_incomes[all_incomes >= qt_cuts[i] & all_incomes < qt_cuts[i + 1]]
  inc  <- sample(pool, n_i, replace = TRUE)
  
  # Health pathway draws (Bernoulli for every individual) ----
  has_dr   <- rbinom(n_i, 1, dr_prev_i)
  has_vtdr <- rbinom(n_i, 1, vtdr_dr_i) * has_dr     # VTDR only if DR
  
  seeks_care <- rbinom(n_i, 1, pcu_i)                 # utilises primary care
  
  # Baseline arm
  screened_b  <- rbinom(n_i, 1, get_val("cov_b"))   * seeks_care
  detected_b  <- rbinom(n_i, 1, get_val("sens_b"))  * screened_b * has_dr
  referred_b  <- rbinom(n_i, 1, 1 - get_val("lfu_referral")) * detected_b
  treated_b   <- rbinom(n_i, 1, 1 - get_val("lfu_treat"))    * referred_b
  
  # among treated VTDR cases, blindness prevented
  blind_prevented_b <- rbinom(n_i, 1, get_val("eff_blind")) * treated_b * has_vtdr
  
  # Intervention arm
  screened_inv  <- rbinom(n_i, 1, get_val("cov_inv"))   * seeks_care
  detected_inv  <- rbinom(n_i, 1, get_val("sens_inv"))  * screened_inv * has_dr
  referred_inv  <- rbinom(n_i, 1, 1 - get_val("lfu_referral")) * detected_inv
  treated_inv   <- rbinom(n_i, 1, 1 - get_val("lfu_treat"))    * referred_inv
  blind_prevented_inv <- rbinom(n_i, 1, get_val("eff_blind")) * treated_inv * has_vtdr
  
  # OOP & CHE
  # baseline: detected cases face late-stage OOP;
  # intervention: detected cases face early-treatment OOP
  oop_paid_b   <- detected_b   * oop_b_i
  oop_paid_inv <- detected_inv * oop_tr_i
  
  # CHE at 10 % threshold
  che_10_b   <- as.integer(oop_paid_b   > get_val("th1") * inc)
  che_10_inv <- as.integer(oop_paid_inv > get_val("th1") * inc)
  
  # CHE at 25 % threshold
  che_25_b   <- as.integer(oop_paid_b   > get_val("th2") * inc)
  che_25_inv <- as.integer(oop_paid_inv > get_val("th2") * inc)
  
  # impoverishment: income after OOP drops below quintile floor
  q_floor <- qt_cuts[i]
  impov_b   <- as.integer((inc - oop_paid_b)   < q_floor & oop_paid_b   > 0)
  impov_inv <- as.integer((inc - oop_paid_inv) < q_floor & oop_paid_inv > 0)
  
  rows[[i]] <- data.table(
    id              = seq_len(n_i) + (i - 1) * n_i,
    quintile        = i,
    income          = inc,
    has_dr          = has_dr,
    has_vtdr        = has_vtdr,
    seeks_care      = seeks_care,
    
    # baseline arm
    screened_b      = screened_b,
    detected_b      = detected_b,
    referred_b      = referred_b,
    treated_b       = treated_b,
    blind_prev_b    = blind_prevented_b,
    oop_paid_b      = oop_paid_b,
    che_10_b        = che_10_b,
    che_25_b        = che_25_b,
    impov_b         = impov_b,
    
    # intervention arm
    screened_inv     = screened_inv,
    detected_inv     = detected_inv,
    referred_inv     = referred_inv,
    treated_inv      = treated_inv,
    blind_prev_inv   = blind_prevented_inv,
    oop_paid_inv     = oop_paid_inv,
    che_10_inv       = che_10_inv,
    che_25_inv       = che_25_inv,
    impov_inv        = impov_inv
  )
}

cohort <- rbindlist(rows)

# Quintile-level summary
summary_q <- cohort[, .(
  n                    = .N,
  dr_cases             = sum(has_dr),
  vtdr_cases           = sum(has_vtdr),
  mean_income          = mean(income),
  
  detected_b           = sum(detected_b),
  detected_inv         = sum(detected_inv),
  addl_detected        = sum(detected_inv) - sum(detected_b),
  
  blind_prev_b         = sum(blind_prev_b),
  blind_prev_inv       = sum(blind_prev_inv),
  addl_blind_averted   = sum(blind_prev_inv) - sum(blind_prev_b),
  
  total_oop_b          = sum(oop_paid_b),
  total_oop_inv        = sum(oop_paid_inv),
  
  che10_b              = sum(che_10_b),
  che10_inv            = sum(che_10_inv),
  che10_averted        = sum(che_10_b) - sum(che_10_inv),
  
  che25_b              = sum(che_25_b),
  che25_inv            = sum(che_25_inv),
  che25_averted        = sum(che_25_b) - sum(che_25_inv),
  
  impov_b              = sum(impov_b),
  impov_inv            = sum(impov_inv),
  impov_averted        = sum(impov_b) - sum(impov_inv)
), by = quintile][order(quintile)]

print(summary_q)

# Plots 
q_labels <- c("I","II","III","IV","V")

# 5a. Additional DR detected
ggplot(summary_q, aes(x = factor(quintile, labels = q_labels), y = addl_detected)) +
  geom_col(fill = "red2", width = 0.6) +
  labs(title = "Additional DR Cases Detected Due to Intervention",
       x = "Income Quintile (Poorest → Richest)", y = "Additional DR Cases") +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

# 5b. Blindness cases averted
ggplot(summary_q, aes(x = factor(quintile, labels = q_labels), y = addl_blind_averted)) +
  geom_col(fill = "red2", width = 0.6) +
  labs(title = "Blindness Cases Averted Due to Intervention",
       x = "Income Quintile (Poorest → Richest)", y = "Cases Averted") +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

# 5c. CHE (10 %) averted
ggplot(summary_q, aes(x = factor(quintile, labels = q_labels), y = che10_averted)) +
  geom_col(fill = "red2", width = 0.6) +
  labs(title = "DR-Related CHE Cases Averted (10% threshold)",
       x = "Income Quintile (Poorest → Richest)", y = "CHE Cases Averted") +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

# 5c2. CHE (25 %) averted
ggplot(summary_q, aes(x = factor(quintile, labels = q_labels), y = che25_averted)) +
  geom_col(fill = "red2", width = 0.6) +
  labs(title = "DR-Related CHE Cases Averted (25% threshold)",
       x = "Income Quintile (Poorest → Richest)", y = "CHE Cases Averted") +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

# 5d. Impoverishment averted
ggplot(summary_q, aes(x = factor(quintile, labels = q_labels), y = impov_averted)) +
  geom_col(fill = "red2", width = 0.6) +
  labs(title = "Impoverishment Cases Averted by Screening",
       x = "Income Quintile (Poorest → Richest)", y = "Cases Averted") +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))