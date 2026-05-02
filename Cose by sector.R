
input <- read.csv("input table new.csv")
input$Value <- as.numeric(gsub(",", "", input$Value)) 
get_val <- function(name) input$Value[input$Parameters == name]

sectors <- c("u", "r")   # u = urban, r = rural
sector_labels <- c("Urban", "Rural")

# ═══════════════════════════════════════════════════════════════════════════════
# COST CALCULATION — URBAN/RURAL DISAGGREGATION
# ═══════════════════════════════════════════════════════════════════════════════

# ─── 2.1 Direct medical OOP ────────────────────────────────────────────────────

# Number of DR patients treated at baseline
treated_baseline_ur <- sapply(sectors, function(s) {
  get_val(paste0("pop_", s)) *
    get_val(paste0("dr_", s)) *
    get_val(paste0("pcu_", s)) *
    get_val("cov_b") *
    get_val("sens_b") *
    (1 - get_val("lfu_referral")) *
    (1 - get_val("lfu_treat"))
})

# Number of DR patients treated after intervention
treated_inv_ur <- sapply(sectors, function(s) {
  get_val(paste0("pop_", s)) *
    get_val(paste0("dr_", s)) *
    get_val(paste0("pcu_", s)) *
    get_val("cov_inv") *
    get_val("sens_inv") *
    (1 - get_val("lfu_referral")) *
    (1 - get_val("lfu_treat"))
})

# Direct medical OOP at baseline by sector
direct_med_OOP_baseline_ur <- treated_baseline_ur * sapply(sectors, function(s) {
  get_val(paste0("oop_med_dr_", s))   * (1 - get_val(paste0("vtdr_dr_", s))) +
    get_val(paste0("oop_med_vtdr_", s)) * get_val(paste0("vtdr_dr_", s))
})

# Direct medical OOP after intervention by sector
direct_med_OOP_inv_ur <- treated_inv_ur * sapply(sectors, function(s) {
  vtdr_inv <- get_val(paste0("vtdr_dr_", s)) * (1 - get_val("vtdr_reduction"))
  get_val(paste0("oop_med_dr_", s))   * (1 - vtdr_inv) +
    get_val(paste0("oop_med_vtdr_", s)) * vtdr_inv
})

OOP_med_additional_ur <- direct_med_OOP_inv_ur - direct_med_OOP_baseline_ur

# ─── 2.2 All OOP (medical + non-medical) ───────────────────────────────────────

# Direct non-medical OOP at baseline
direct_nonmed_OOP_baseline_ur <- treated_baseline_ur * sapply(sectors, function(s) {
  get_val(paste0("oop_nonmed_dr_", s))   * (1 - get_val(paste0("vtdr_dr_", s))) +
    get_val(paste0("oop_nonmed_vtdr_", s)) * get_val(paste0("vtdr_dr_", s))
})

# Direct non-medical OOP after intervention
direct_nonmed_OOP_inv_ur <- treated_inv_ur * sapply(sectors, function(s) {
  vtdr_inv <- get_val(paste0("vtdr_dr_", s)) * (1 - get_val("vtdr_reduction"))
  get_val(paste0("oop_nonmed_dr_", s))   * (1 - vtdr_inv) +
    get_val(paste0("oop_nonmed_vtdr_", s)) * vtdr_inv
})

OOP_total_additional_ur <- (direct_med_OOP_inv_ur + direct_nonmed_OOP_inv_ur) -
  (direct_med_OOP_baseline_ur + direct_nonmed_OOP_baseline_ur)

# ─── Cost plots ────────────────────────────────────────────────────────────────

plot_cost_ur <- data.frame(
  sector = factor(sector_labels, levels = sector_labels),
  OOP_med_additional = OOP_med_additional_ur,
  OOP_total_additional = OOP_total_additional_ur
)

ggplot(plot_cost_ur, aes(x = sector, y = OOP_med_additional_ur)) +
  geom_col(fill = "coral1", width = 0.6) +
  labs(title = "Additional Direct Medical OOP Due to Intervention",
       x = "Place of Residence",
       y = "Additional Direct Medical OOP (INR)") +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

ggplot(plot_cost_ur, aes(x = sector, y = OOP_total_additional_ur)) +
  geom_col(fill = "coral1", width = 0.6) +
  labs(title = "Additional Total OOP Due to Intervention",
       x = "Place of Residence",
       y = "Additional Medical and Non-medical OOP (INR)") +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

# ─── 2.3 Catastrophic Health Expenditure ───────────────────────────────────────

# Per-person total OOP — baseline
per_person_oop_baseline_ur <- sapply(sectors, function(s) {
  vtdr_b <- get_val(paste0("vtdr_dr_", s))
  med    <- get_val(paste0("oop_med_dr_", s))    * (1 - vtdr_b) +
    get_val(paste0("oop_med_vtdr_", s))  * vtdr_b
  nonmed <- get_val(paste0("oop_nonmed_dr_", s)) * (1 - vtdr_b) +
    get_val(paste0("oop_nonmed_vtdr_", s)) * vtdr_b
  med + nonmed
})

# Per-person total OOP — intervention
per_person_oop_inv_ur <- sapply(sectors, function(s) {
  vtdr_inv <- get_val(paste0("vtdr_dr_", s)) * (1 - get_val("vtdr_reduction"))
  med    <- get_val(paste0("oop_med_dr_", s))    * (1 - vtdr_inv) +
    get_val(paste0("oop_med_vtdr_", s))  * vtdr_inv
  nonmed <- get_val(paste0("oop_nonmed_dr_", s)) * (1 - vtdr_inv) +
    get_val(paste0("oop_nonmed_vtdr_", s)) * vtdr_inv
  med + nonmed
})

# Annual consumption by sector
annual_consumption_ur <- sapply(sectors, function(s) {
  get_val(paste0("mcpe_", s)) * 12
})

# CHE thresholds
th1 <- get_val("th1")   # 0.10
th2 <- get_val("th2")   # 0.25

# CHE indicators per sector
che_10_indicator_baseline_ur <- as.integer(per_person_oop_baseline_ur > th1 * annual_consumption_ur)
che_25_indicator_baseline_ur <- as.integer(per_person_oop_baseline_ur > th2 * annual_consumption_ur)
che_10_indicator_inv_ur      <- as.integer(per_person_oop_inv_ur > th1 * annual_consumption_ur)
che_25_indicator_inv_ur      <- as.integer(per_person_oop_inv_ur > th2 * annual_consumption_ur)

# Total CHE cases = treated × indicator
che_10_baseline_ur <- treated_baseline_ur * che_10_indicator_baseline_ur
che_25_baseline_ur <- treated_baseline_ur * che_25_indicator_baseline_ur
che_10_inv_ur      <- treated_inv_ur * che_10_indicator_inv_ur
che_25_inv_ur      <- treated_inv_ur * che_25_indicator_inv_ur

# CHE additional (intervention - baseline)
che_10_additional_ur <- che_10_inv_ur - che_10_baseline_ur
che_25_additional_ur <- che_25_inv_ur - che_25_baseline_ur

# CHE plots
plot_che_ur <- data.frame(
  sector = factor(sector_labels, levels = sector_labels),
  che_10_additional = che_10_additional_ur,
  che_25_additional = che_25_additional_ur
)

ggplot(plot_che_ur, aes(x = sector, y = che_10_additional_ur)) +
  geom_col(fill = "coral1", width = 0.6) +
  labs(title = "Additional CHE Cases (10% threshold) Due to Intervention",
       x = "Place of Residence",
       y = "Additional CHE Cases") +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

ggplot(plot_che_ur, aes(x = sector, y = che_25_additional_ur)) +
  geom_col(fill = "coral1", width = 0.6) +
  labs(title = "Additional CHE Cases (25% threshold) Due to Intervention",
       x = "Place of Residence",
       y = "Additional CHE Cases") +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

# ─── 2.4 Impoverishment due to medical expense ─────────────────────────────────

# Annual poverty line (assuming pov is daily — adjust × 12 if monthly)
pov_annual <- 180 * 365

# Consumption after OOP
post_oop_consumption_baseline_ur <- annual_consumption_ur - per_person_oop_baseline_ur
post_oop_consumption_inv_ur      <- annual_consumption_ur - per_person_oop_inv_ur

# Impoverishment indicators (was above pov, now below)
impov_indicator_baseline_ur <- as.integer(
  annual_consumption_ur >= pov_annual &
    post_oop_consumption_baseline_ur < pov_annual
)
impov_indicator_inv_ur <- as.integer(
  annual_consumption_ur >= pov_annual &
    post_oop_consumption_inv_ur < pov_annual
)

# Total impoverishment cases per sector
impov_baseline_ur <- treated_baseline_ur * impov_indicator_baseline_ur
impov_inv_ur      <- treated_inv_ur * impov_indicator_inv_ur

impov_additional_ur <- impov_inv_ur - impov_baseline_ur

# Impoverishment plot
plot_impov_ur <- data.frame(
  sector = factor(sector_labels, levels = sector_labels),
  impov_additional = impov_additional_ur
)

ggplot(plot_impov_ur, aes(x = sector, y = impov_additional_ur)) +
  geom_col(fill = "coral1", width = 0.6) +
  labs(title = "Additional Impoverishment Cases Due to Intervention",
       x = "Place of Residence",
       y = "Additional Impoverishment Cases") +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

#dashboard

health_benefit_dash_ur <- as.data.frame(rbind(
  round(OOP_med_additional_ur, 1),
  round(OOP_total_additional_ur, 1),
  round(che_10_additional_ur, 1),
  round(che_25_additional_ur, 1),
  round(impov_additional_ur, 1)
))
colnames(health_benefit_dash_ur) <- c("Urban", "Rural")
rownames(health_benefit_dash_ur) <- c(
  "Additional Direct Medical OOP due to Intervention",
  "Additional Total OOP due to Intervention",
  "Additional CHE Cases (10% threshold)",
  "Additional CHE Cases (25% threshold)",
  "Additional Impoverishment Cases"
)
print(health_benefit_dash_ur)