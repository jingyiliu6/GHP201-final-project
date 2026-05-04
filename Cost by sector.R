rm(list = ls())
input <- read.csv("input table new.csv")
input$Parameters <- trimws(input$Parameters)
input$Value <- as.numeric(gsub(",", "", input$Value)) 
get_val <- function(name) input$Value[input$Parameters == name]
scale_to_target_pop <- get_val("target_pop") / get_val("pop")
plain_number <- function(x) format(round(x, 0), scientific = FALSE, trim = TRUE)
comma_number <- function(x) format(round(x, 0), scientific = FALSE, trim = TRUE, big.mark = ",")
burden_label <- function(value, unit) {
  ifelse(unit == "INR",
         paste0("INR ", round(value / 1000000, 0), "M"),
         comma_number(value))
}

sectors <- c("u", "r")   # u = urban, r = rural
sector_labels <- c("Urban", "Rural")
get_pcu_sector <- function(s) get_val(paste0("pcu_", s)) / 100
get_insurance_coverage_sector <- function(s) get_val(paste0("ins_cov_", s))
get_hh_size_sector <- function(s) {
  if (s == "u") {
    get_val("hh_size_urban")
  } else {
    get_val("hh_size_rural")
  }
}
fractiles <- c("0_5", "5_10", "10_20", "20_30", "30_40", "40_50",
               "50_60", "60_70", "70_80", "80_90", "90_95", "95_100")
fractile_shares <- sapply(fractiles, function(f) get_val(paste0("frac_share_", f)))
get_mpce_fractiles <- function(s) {
  sapply(fractiles, function(f) get_val(paste0("mpce_", s, "_", f)))
}
weighted_threshold_rate <- function(oop, annual_consumption, threshold) {
  sum(fractile_shares * as.integer(oop > threshold * annual_consumption))
}
weighted_impov_rate <- function(oop, annual_consumption, poverty_line) {
  sum(fractile_shares *
        as.integer(annual_consumption >= poverty_line &
                     annual_consumption - oop < poverty_line))
}
insured_med_oop <- function(oop_dr, oop_vtdr, vtdr_share,
                            insurance_coverage,
                            coverage_med_dr,
                            coverage_med_vtdr) {
  oop_dr * (1 - vtdr_share) * (1 - insurance_coverage * coverage_med_dr) +
    oop_vtdr * vtdr_share * (1 - insurance_coverage * coverage_med_vtdr)
}

# ═══════════════════════════════════════════════════════════════════════════════
# COST CALCULATION — URBAN/RURAL DISAGGREGATION
# ═══════════════════════════════════════════════════════════════════════════════

# ─── 2.1 Direct medical OOP ────────────────────────────────────────────────────

# Number of DR patients treated at baseline
treated_baseline_ur <- sapply(sectors, function(s) {
  get_val(paste0("pop_", s)) *
    get_val(paste0("dr_", s)) *
    get_pcu_sector(s) *
    get_val("cov_b") *
    get_val("sens_b") *
    (1 - get_val("lfu_referral")) *
    (1 - get_val("lfu_treat"))
})

# Number of DR patients treated after intervention
treated_inv_ur <- sapply(sectors, function(s) {
  get_val(paste0("pop_", s)) *
    get_val(paste0("dr_", s)) *
    get_pcu_sector(s) *
    get_val("cov_inv") *
    get_val("sens_inv") *
    (1 - get_val("lfu_referral")) *
    (1 - get_val("lfu_treat"))
})

# Direct medical OOP at baseline by sector
direct_med_OOP_baseline_ur <- treated_baseline_ur * sapply(sectors, function(s) {
  insured_med_oop(
    get_val(paste0("oop_med_dr_", s)),
    get_val(paste0("oop_med_vtdr_", s)),
    get_val(paste0("vtdr_dr_", s)),
    get_insurance_coverage_sector(s),
    get_val("coverage_med_dr_insured"),
    get_val("coverage_med_vtdr_insured")
  )
})

# Direct medical OOP after intervention by sector
direct_med_OOP_inv_ur <- treated_inv_ur * sapply(sectors, function(s) {
  vtdr_inv <- get_val(paste0("vtdr_dr_", s)) * (1 - get_val("vtdr_reduction"))
  insured_med_oop(
    get_val(paste0("oop_med_dr_", s)),
    get_val(paste0("oop_med_vtdr_", s)),
    vtdr_inv,
    get_insurance_coverage_sector(s),
    get_val("coverage_med_dr_insured"),
    get_val("coverage_med_vtdr_insured")
  )
})

OOP_med_additional_ur <- direct_med_OOP_inv_ur - direct_med_OOP_baseline_ur
OOP_med_additional_ur_target <- OOP_med_additional_ur * scale_to_target_pop

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
OOP_total_additional_ur_target <- OOP_total_additional_ur * scale_to_target_pop

# ─── Cost plots ────────────────────────────────────────────────────────────────

library(ggplot2)
plot_cost_ur <- data.frame(
  sector = factor(sector_labels, levels = sector_labels),
  OOP_med_additional = OOP_med_additional_ur_target,
  OOP_total_additional = OOP_total_additional_ur_target
)

ggplot(plot_cost_ur, aes(x = sector, y = OOP_med_additional)) +
  geom_col(fill = "red", width = 0.6) +
  geom_text(aes(label = plain_number(OOP_med_additional)), vjust = -0.4, size = 3) +
  scale_y_continuous(labels = plain_number,
                     expand = expansion(mult = c(0, 0.1))) +
  labs(title = "Additional Direct Medical OOP Due to Intervention",
       x = "Place of Residence",
       y = "Additional Direct Medical OOP (INR)") +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

ggplot(plot_cost_ur, aes(x = sector, y = OOP_total_additional)) +
  geom_col(fill = "red", width = 0.6) +
  geom_text(aes(label = plain_number(OOP_total_additional)), vjust = -0.4, size = 3) +
  scale_y_continuous(labels = plain_number,
                     expand = expansion(mult = c(0, 0.1))) +
  labs(title = "Additional Total OOP Due to Intervention",
       x = "Place of Residence",
       y = "Additional Medical and Non-medical OOP (INR)") +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

# ─── 2.3 Catastrophic Health Expenditure ───────────────────────────────────────

# Per-person total OOP — baseline
per_person_oop_baseline_ur <- sapply(sectors, function(s) {
  vtdr_b <- get_val(paste0("vtdr_dr_", s))
  med    <- insured_med_oop(
    get_val(paste0("oop_med_dr_", s)),
    get_val(paste0("oop_med_vtdr_", s)),
    vtdr_b,
    get_insurance_coverage_sector(s),
    get_val("coverage_med_dr_insured"),
    get_val("coverage_med_vtdr_insured")
  )
  nonmed <- get_val(paste0("oop_nonmed_dr_", s)) * (1 - vtdr_b) +
    get_val(paste0("oop_nonmed_vtdr_", s)) * vtdr_b
  med + nonmed
})

# Per-person total OOP — intervention
per_person_oop_inv_ur <- sapply(sectors, function(s) {
  vtdr_inv <- get_val(paste0("vtdr_dr_", s)) * (1 - get_val("vtdr_reduction"))
  med    <- insured_med_oop(
    get_val(paste0("oop_med_dr_", s)),
    get_val(paste0("oop_med_vtdr_", s)),
    vtdr_inv,
    get_insurance_coverage_sector(s),
    get_val("coverage_med_dr_insured"),
    get_val("coverage_med_vtdr_insured")
  )
  nonmed <- get_val(paste0("oop_nonmed_dr_", s)) * (1 - vtdr_inv) +
    get_val(paste0("oop_nonmed_vtdr_", s)) * vtdr_inv
  med + nonmed
})

# Annual household consumption by sector fractile
annual_consumption_ur <- lapply(sectors, function(s) {
  get_mpce_fractiles(s) * 12 * get_hh_size_sector(s)
})
names(annual_consumption_ur) <- sectors

# CHE thresholds
th1 <- get_val("th1")   # 0.10
th2 <- get_val("th2")   # 0.25

# CHE rates per sector, weighted across MPCE fractiles
che_10_rate_baseline_ur <- sapply(sectors, function(s) {
  weighted_threshold_rate(per_person_oop_baseline_ur[s], annual_consumption_ur[[s]], th1)
})
che_25_rate_baseline_ur <- sapply(sectors, function(s) {
  weighted_threshold_rate(per_person_oop_baseline_ur[s], annual_consumption_ur[[s]], th2)
})
che_10_rate_inv_ur <- sapply(sectors, function(s) {
  weighted_threshold_rate(per_person_oop_inv_ur[s], annual_consumption_ur[[s]], th1)
})
che_25_rate_inv_ur <- sapply(sectors, function(s) {
  weighted_threshold_rate(per_person_oop_inv_ur[s], annual_consumption_ur[[s]], th2)
})

# Total CHE cases = treated patients x weighted fractile CHE rate
che_10_baseline_ur <- treated_baseline_ur * che_10_rate_baseline_ur
che_25_baseline_ur <- treated_baseline_ur * che_25_rate_baseline_ur
che_10_inv_ur      <- treated_inv_ur * che_10_rate_inv_ur
che_25_inv_ur      <- treated_inv_ur * che_25_rate_inv_ur

# CHE additional (intervention - baseline)
che_10_additional_ur <- che_10_inv_ur - che_10_baseline_ur
che_25_additional_ur <- che_25_inv_ur - che_25_baseline_ur
che_10_additional_ur_target <- che_10_additional_ur * scale_to_target_pop
che_25_additional_ur_target <- che_25_additional_ur * scale_to_target_pop

# CHE plots
plot_che_ur <- data.frame(
  sector = factor(sector_labels, levels = sector_labels),
  che_10_additional = che_10_additional_ur_target,
  che_25_additional = che_25_additional_ur_target
)

ggplot(plot_che_ur, aes(x = sector, y = che_10_additional)) +
  geom_col(fill = "red", width = 0.6) +
  geom_text(aes(label = plain_number(che_10_additional)), vjust = -0.4, size = 3) +
  scale_y_continuous(labels = plain_number,
                     expand = expansion(mult = c(0, 0.1))) +
  labs(title = "Additional CHE Cases (10% threshold) Due to Intervention",
       x = "Place of Residence",
       y = "Additional CHE Cases") +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

ggplot(plot_che_ur, aes(x = sector, y = che_25_additional)) +
  geom_col(fill = "red", width = 0.6) +
  geom_text(aes(label = plain_number(che_25_additional)), vjust = -0.4, size = 3) +
  scale_y_continuous(labels = plain_number,
                     expand = expansion(mult = c(0, 0.1))) +
  labs(title = "Additional CHE Cases (25% threshold) Due to Intervention",
       x = "Place of Residence",
       y = "Additional CHE Cases") +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

# ─── 2.4 Impoverishment due to medical expense ─────────────────────────────────

# Annual household poverty line from a daily household poverty line
pov_annual <- get_val("pov") * 365

# Impoverishment rates per sector, weighted across MPCE fractiles
impov_rate_baseline_ur <- sapply(sectors, function(s) {
  weighted_impov_rate(per_person_oop_baseline_ur[s], annual_consumption_ur[[s]], pov_annual)
})
impov_rate_inv_ur <- sapply(sectors, function(s) {
  weighted_impov_rate(per_person_oop_inv_ur[s], annual_consumption_ur[[s]], pov_annual)
})

# Total impoverishment cases per sector = treated patients x weighted fractile rate
impov_baseline_ur <- treated_baseline_ur * impov_rate_baseline_ur
impov_inv_ur      <- treated_inv_ur * impov_rate_inv_ur

impov_additional_ur <- impov_inv_ur - impov_baseline_ur
impov_additional_ur_target <- impov_additional_ur * scale_to_target_pop

# Impoverishment plot
plot_impov_ur <- data.frame(
  sector = factor(sector_labels, levels = sector_labels),
  impov_additional = impov_additional_ur_target
)

ggplot(plot_impov_ur, aes(x = sector, y = impov_additional)) +
  geom_col(fill = "red", width = 0.6) +
  geom_text(aes(label = plain_number(impov_additional)), vjust = -0.4, size = 3) +
  scale_y_continuous(labels = plain_number,
                     expand = expansion(mult = c(0, 0.1))) +
  labs(title = "Change in Impoverishment Cases Due to Intervention",
       x = "Place of Residence",
       y = "Change in Impoverishment Cases") +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

# Combined Urban/Rural burden plot. Each metric gets its own y-scale so the
# urban/rural comparison stays visible despite mixing INR totals and case counts.
plot_burden_ur <- data.frame(
  category = factor(
    rep(c("Direct Medical OOP\n(INR)",
          "Total OOP\n(INR)",
          "CHE cases\n(Number)",
          "Impoverishment\n(Number)"),
        each = 2),
    levels = c("Direct Medical OOP\n(INR)",
               "Total OOP\n(INR)",
               "CHE cases\n(Number)",
               "Impoverishment\n(Number)")
  ),
  sector = factor(rep(sector_labels, times = 4), levels = sector_labels),
  value = c(
    OOP_med_additional_ur_target,
    OOP_total_additional_ur_target,
    che_10_additional_ur_target,
    impov_additional_ur_target
  ),
  unit = rep(c("INR", "INR", "Number", "Number"), each = 2)
)
plot_burden_ur$plot_value <- pmax(plot_burden_ur$value, 1)
plot_burden_ur$label <- burden_label(plot_burden_ur$value, plot_burden_ur$unit)

ggplot(plot_burden_ur, aes(x = sector, y = plot_value, fill = sector)) +
  geom_col(width = 0.62) +
  geom_text(aes(label = label),
            vjust = -0.35,
            size = 3) +
  scale_fill_manual(values = c("Urban" = "#0067B9", "Rural" = "lightcoral")) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.18))) +
  facet_wrap(~ category, scales = "free_y", nrow = 1) +
  labs(title = "Rural vs Urban Burden",
       x = NULL,
       y = NULL,
       fill = NULL) +
  theme_classic() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    legend.position = "top",
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank(),
    strip.background = element_blank(),
    strip.text = element_text(size = 10)
  )

#dashboard

health_benefit_dash_ur <- as.data.frame(rbind(
  round(OOP_med_additional_ur_target, 0),
  round(OOP_total_additional_ur_target, 0),
  round(che_10_additional_ur_target, 0),
  round(che_25_additional_ur_target, 0),
  round(impov_additional_ur_target, 0)
))
colnames(health_benefit_dash_ur) <- c("Urban", "Rural")
rownames(health_benefit_dash_ur) <- c(
  "Additional Direct Medical OOP due to Intervention (scaled to target population)",
  "Additional Total OOP due to Intervention (scaled to target population)",
  "Additional CHE Cases (10% threshold, scaled to target population)",
  "Additional CHE Cases (25% threshold, scaled to target population)",
  "Change in Impoverishment Cases (scaled to target population)"
)
print(health_benefit_dash_ur)

# Sensitivity analysis for medical insurance coverage assumptions. No graphs are
# generated; results summarize total changes across urban and rural groups.
run_insurance_sensitivity_sector <- function(
    coverage_med_dr_values = c(0.30, 0.50, 0.70),
    coverage_med_vtdr_values = c(0.40, 0.70, 0.90)) {
  scenarios <- expand.grid(
    coverage_med_dr_insured = coverage_med_dr_values,
    coverage_med_vtdr_insured = coverage_med_vtdr_values
  )
  
  results <- lapply(seq_len(nrow(scenarios)), function(row_id) {
    cov_dr <- scenarios$coverage_med_dr_insured[row_id]
    cov_vtdr <- scenarios$coverage_med_vtdr_insured[row_id]
    
    med_baseline <- treated_baseline_ur * sapply(sectors, function(s) {
      insured_med_oop(
        get_val(paste0("oop_med_dr_", s)),
        get_val(paste0("oop_med_vtdr_", s)),
        get_val(paste0("vtdr_dr_", s)),
        get_insurance_coverage_sector(s),
        cov_dr,
        cov_vtdr
      )
    })
    med_inv <- treated_inv_ur * sapply(sectors, function(s) {
      insured_med_oop(
        get_val(paste0("oop_med_dr_", s)),
        get_val(paste0("oop_med_vtdr_", s)),
        get_val(paste0("vtdr_dr_", s)) * (1 - get_val("vtdr_reduction")),
        get_insurance_coverage_sector(s),
        cov_dr,
        cov_vtdr
      )
    })
    
    oop_baseline <- sapply(sectors, function(s) {
      vtdr_b <- get_val(paste0("vtdr_dr_", s))
      med <- insured_med_oop(
        get_val(paste0("oop_med_dr_", s)),
        get_val(paste0("oop_med_vtdr_", s)),
        vtdr_b,
        get_insurance_coverage_sector(s),
        cov_dr,
        cov_vtdr
      )
      nonmed <- get_val(paste0("oop_nonmed_dr_", s)) * (1 - vtdr_b) +
        get_val(paste0("oop_nonmed_vtdr_", s)) * vtdr_b
      med + nonmed
    })
    oop_inv <- sapply(sectors, function(s) {
      vtdr_inv <- get_val(paste0("vtdr_dr_", s)) * (1 - get_val("vtdr_reduction"))
      med <- insured_med_oop(
        get_val(paste0("oop_med_dr_", s)),
        get_val(paste0("oop_med_vtdr_", s)),
        vtdr_inv,
        get_insurance_coverage_sector(s),
        cov_dr,
        cov_vtdr
      )
      nonmed <- get_val(paste0("oop_nonmed_dr_", s)) * (1 - vtdr_inv) +
        get_val(paste0("oop_nonmed_vtdr_", s)) * vtdr_inv
      med + nonmed
    })
    
    che_10_b <- sapply(sectors, function(s) weighted_threshold_rate(oop_baseline[s], annual_consumption_ur[[s]], th1))
    che_25_b <- sapply(sectors, function(s) weighted_threshold_rate(oop_baseline[s], annual_consumption_ur[[s]], th2))
    che_10_i <- sapply(sectors, function(s) weighted_threshold_rate(oop_inv[s], annual_consumption_ur[[s]], th1))
    che_25_i <- sapply(sectors, function(s) weighted_threshold_rate(oop_inv[s], annual_consumption_ur[[s]], th2))
    impov_b <- sapply(sectors, function(s) weighted_impov_rate(oop_baseline[s], annual_consumption_ur[[s]], pov_annual))
    impov_i <- sapply(sectors, function(s) weighted_impov_rate(oop_inv[s], annual_consumption_ur[[s]], pov_annual))
    
    data.frame(
      coverage_med_dr_insured = cov_dr,
      coverage_med_vtdr_insured = cov_vtdr,
      additional_direct_med_oop = round(sum((med_inv - med_baseline) * scale_to_target_pop), 0),
      additional_total_oop = round(sum(((med_inv + direct_nonmed_OOP_inv_ur) -
                                          (med_baseline + direct_nonmed_OOP_baseline_ur)) *
                                         scale_to_target_pop), 0),
      additional_che_10_cases = round(sum((treated_inv_ur * che_10_i -
                                             treated_baseline_ur * che_10_b) *
                                            scale_to_target_pop), 0),
      additional_che_25_cases = round(sum((treated_inv_ur * che_25_i -
                                             treated_baseline_ur * che_25_b) *
                                            scale_to_target_pop), 0),
      change_impoverishment_cases = round(sum((treated_inv_ur * impov_i -
                                                treated_baseline_ur * impov_b) *
                                               scale_to_target_pop), 0)
    )
  })
  
  do.call(rbind, results)
}

insurance_sensitivity_sector <- run_insurance_sensitivity_sector()
print(insurance_sensitivity_sector)
