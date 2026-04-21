rm(list = ls())
input <- read.csv("Input table.csv")
input$Value <- as.numeric(gsub(",", "", input$Value))

set.seed(123)
library(ggplot2)
library(data.table)

get_val <- function(name) input$Value[input$Parameters == name]
quintiles <- 1:5

n_treat_visits        <- get_val("n_treat_visits")
days_lost_screen      <- get_val("days_lost_screen")
days_lost_referral    <- get_val("days_lost_referral")
days_lost_treat       <- get_val("days_lost_treat")
late_stage_multiplier <- get_val("late_stage_multiplier")
poverty_line_val      <- get_val("poverty_line")

# ═══════════════════════════════════════════════════════════════════════════════
# INCOME SIMULATION
gini       <- get_val("gini")
ave_income <- get_val("mean_income")
pop_total  <- get_val("pop")

phi  <- 1 / gini
beta <- (1 / phi) * ave_income
all_incomes <- rgamma(n = pop_total, shape = phi, rate = 1 / beta)
qt_cuts     <- quantile(all_incomes, probs = seq(0, 1, by = 0.20))

#### Poverty line: top of Q1 (bottom 20%)???
poverty_line <- qt_cuts[2]
#poverty_line <- poverty_line_val

# ═══════════════════════════════════════════════════════════════════════════════
# MICROSIMULATION
rows <- list()

for (i in quintiles) {
  # Read all quintile-specific parameters from input table
  n_i          <- get_val(paste0("pop_", i))
  dr_prev_i    <- get_val(paste0("dr_", i))
  vtdr_dr_i    <- get_val(paste0("vtdr_dr_", i))
  pcu_i        <- get_val(paste0("pcu_", i))
  oop_treat_i  <- get_val(paste0("oop_tr_", i))
  nonmed_sc_i  <- get_val(paste0("nonmed_sc_", i))
  nonmed_ref_i <- get_val(paste0("nonmed_ref_", i))
  daily_wage_i <- get_val(paste0("daily_wage_", i))
  
  # ── Derived costs from input parameters ───────────────────────────────────
  # Non-medical cost per treatment visit ≈ referral-level travel
  nonmed_treat_i <- nonmed_ref_i * n_treat_visits
  
  # Indirect costs (wage loss)
  indirect_screen_i   <- daily_wage_i * days_lost_screen
  indirect_referral_i <- daily_wage_i * days_lost_referral
  indirect_treat_i    <- daily_wage_i * days_lost_treat
  
  # Draw incomes
  pool <- all_incomes[all_incomes >= qt_cuts[i] & all_incomes < qt_cuts[i + 1]]
  inc  <- sample(pool, n_i, replace = TRUE)
  
  # Health status draws
  has_dr     <- rbinom(n_i, 1, dr_prev_i)
  has_vtdr   <- rbinom(n_i, 1, vtdr_dr_i) * has_dr
  seeks_care <- rbinom(n_i, 1, pcu_i)
  
  # ═════════════════════════════════════════════════════════════════════════
  # BASELINE ARM (usual care)
  screened_b   <- rbinom(n_i, 1, get_val("cov_b"))              * seeks_care
  detected_b   <- rbinom(n_i, 1, get_val("sens_b"))             * screened_b * has_dr
  referred_b   <- rbinom(n_i, 1, 1 - get_val("lfu_referral"))   * detected_b
  treated_b    <- rbinom(n_i, 1, 1 - get_val("lfu_treat"))      * referred_b
  blind_prev_b <- rbinom(n_i, 1, get_val("eff_blind"))          * treated_b * has_vtdr
  
  # OOP baseline: late-stage → multiplied medical + treatment non-med
  oop_med_b      <- treated_b * oop_treat_i * late_stage_multiplier
  oop_nonmed_b   <- (detected_b * nonmed_sc_i +
                       referred_b * nonmed_ref_i +
                       treated_b  * nonmed_treat_i * late_stage_multiplier)
  oop_indirect_b <- (detected_b * indirect_screen_i +
                       referred_b * indirect_referral_i +
                       treated_b  * indirect_treat_i)
  oop_total_b    <- oop_med_b + oop_nonmed_b + oop_indirect_b
  
  # ═════════════════════════════════════════════════════════════════════════
  # INTERVENTION ARM (AI screening)
  screened_inv   <- rbinom(n_i, 1, get_val("cov_inv"))            * seeks_care
  detected_inv   <- rbinom(n_i, 1, get_val("sens_inv"))           * screened_inv * has_dr
  referred_inv   <- rbinom(n_i, 1, 1 - get_val("lfu_referral"))   * detected_inv
  treated_inv    <- rbinom(n_i, 1, 1 - get_val("lfu_treat"))      * referred_inv
  blind_prev_inv <- rbinom(n_i, 1, get_val("eff_blind"))          * treated_inv * has_vtdr
  
  # OOP intervention: early-stage → base costs, screening is FREE
  oop_med_inv      <- treated_inv * oop_treat_i
  oop_nonmed_inv   <- (detected_inv * nonmed_sc_i +
                         referred_inv * nonmed_ref_i +
                         treated_inv  * nonmed_treat_i)
  oop_indirect_inv <- (detected_inv * indirect_screen_i +
                         referred_inv * indirect_referral_i +
                         treated_inv  * indirect_treat_i)
  oop_total_inv    <- oop_med_inv + oop_nonmed_inv + oop_indirect_inv
  
  # ═════════════════════════════════════════════════════════════════════════
  # CHE & IMPOVERISHMENT
  th1 <- get_val("th1")
  th2 <- get_val("th2")
  
  che_10_b   <- as.integer(oop_total_b   > th1 * inc)
  che_10_inv <- as.integer(oop_total_inv > th1 * inc)
  che_25_b   <- as.integer(oop_total_b   > th2 * inc)
  che_25_inv <- as.integer(oop_total_inv > th2 * inc)
  
  impov_b   <- as.integer((inc - oop_total_b)   < poverty_line & oop_total_b   > 0)
  impov_inv <- as.integer((inc - oop_total_inv) < poverty_line & oop_total_inv > 0)
  
  #
  rows[[i]] <- data.table(
    id               = seq_len(n_i) + (i - 1) * n_i,
    quintile         = i,
    income           = round(inc, 0),
    has_dr           = has_dr,
    has_vtdr         = has_vtdr,
    seeks_care       = seeks_care,
    
    # Baseline disease
    screened_b       = screened_b,
    detected_b       = detected_b,
    referred_b       = referred_b,
    treated_b        = treated_b,
    blind_prev_b     = blind_prev_b,
    
    # baseline cost
    oop_med_b        = oop_med_b,
    oop_nonmed_b     = oop_nonmed_b,
    oop_indirect_b   = oop_indirect_b,
    oop_total_b      = oop_total_b,
    che_10_b         = che_10_b,
    che_25_b         = che_25_b,
    impov_b          = impov_b,
    
    # Intervention disease
    screened_inv     = screened_inv,
    detected_inv     = detected_inv,
    referred_inv     = referred_inv,
    treated_inv      = treated_inv,
    blind_prev_inv   = blind_prev_inv,
    
    # Intervention costs
    oop_med_inv      = oop_med_inv,
    oop_nonmed_inv   = oop_nonmed_inv,
    oop_indirect_inv = oop_indirect_inv,
    oop_total_inv    = oop_total_inv,
    che_10_inv       = che_10_inv,
    che_25_inv       = che_25_inv,
    impov_inv        = impov_inv
  )
}

cohort <- rbindlist(rows)


# ═══════════════════════════════════════════════════════════════════════════════
# QUINTILE-LEVEL SUMMARY
summary_q <- cohort[, .(
  n                  = .N,
  dr_cases           = sum(has_dr),
  mean_income        = round(mean(income)),
  
  detected_b         = sum(detected_b),
  detected_inv       = sum(detected_inv),
  addl_detected      = sum(detected_inv) - sum(detected_b),
  
  blind_prev_b       = sum(blind_prev_b),
  blind_prev_inv     = sum(blind_prev_inv),
  addl_blind_averted = sum(blind_prev_inv) - sum(blind_prev_b),
  
  mean_oop_b         = round(mean(oop_total_b[treated_b == 1]), 0),
  mean_oop_inv       = round(mean(oop_total_inv[treated_inv == 1]), 0),
  
  che10_b            = sum(che_10_b),
  che10_inv          = sum(che_10_inv),
  che10_averted      = sum(che_10_b) - sum(che_10_inv),
  
  che25_b            = sum(che_25_b),
  che25_inv          = sum(che_25_inv),
  che25_averted      = sum(che_25_b) - sum(che_25_inv),
  
  impov_b            = sum(impov_b),
  impov_inv          = sum(impov_inv),
  impov_averted      = sum(impov_b) - sum(impov_inv)
), by = quintile][order(quintile)]

print(summary_q)


# ═══════════════════════════════════════════════════════════════════════════════
# PLOTS
q_labels <- c("I", "II", "III", "IV", "V")

# A) Additional DR detected
ggplot(summary_q, aes(x = factor(quintile, labels = q_labels), y = addl_detected)) +
  geom_col(fill = "red2", width = 0.6) +
  labs(title = "Additional DR Cases Detected Due to Screening",
       x = "Income Quintile", y = "Additional Cases") +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

# B) Blindness averted
ggplot(summary_q, aes(x = factor(quintile, labels = q_labels), y = addl_blind_averted)) +
  geom_col(fill = "red2", width = 0.6) +
  labs(title = "Blindness Cases Averted Due to Screening",
       x = "Income Quintile", y = "Cases Averted") +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

# C) Mean OOP comparison (grouped bar)
df_oop <- melt(summary_q[, .(quintile, Baseline = mean_oop_b, Intervention = mean_oop_inv)],
               id.vars = "quintile", variable.name = "Arm", value.name = "Mean_OOP")

ggplot(df_oop, aes(x = factor(quintile, labels = q_labels), y = Mean_OOP, fill = Arm)) +
  geom_col(position = "dodge", width = 0.6) +
  scale_fill_manual(values = c("Baseline" = "grey50", "Intervention" = "red2")) +
  labs(title = "Mean OOP Among Treated, by Arm",
       x = "Income Quintile", y = "Mean OOP (THB)") +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

# D) CHE averted (10%)
ggplot(summary_q, aes(x = factor(quintile, labels = q_labels), y = che10_averted)) +
  geom_col(fill = "red2", width = 0.6) +
  labs(title = "CHE Cases Averted (10% Threshold)",
       x = "Income Quintile", y = "CHE Cases Averted") +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

# E) CHE averted (25%)
ggplot(summary_q, aes(x = factor(quintile, labels = q_labels), y = che25_averted)) +
  geom_col(fill = "red2", width = 0.6) +
  labs(title = "CHE Cases Averted (25% Threshold)",
       x = "Income Quintile", y = "CHE Cases Averted") +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

# F) Impoverishment averted
ggplot(summary_q, aes(x = factor(quintile, labels = q_labels), y = impov_averted)) +
  geom_col(fill = "red2", width = 0.6) +
  labs(title = "Impoverishment Cases Averted by Screening",
       x = "Income Quintile", y = "Cases Averted") +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))