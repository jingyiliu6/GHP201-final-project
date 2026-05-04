#cost: additional OOP incurred to patients by quintile and by rural urban 
#catastrophic health expenditure cases 
rm(list = ls())

input <- read.csv("input table new.csv")
input$Parameters <- trimws(input$Parameters)
input$Value <- as.numeric(gsub(",", "", input$Value)) 
hces <- read.csv("hces_simulated_100k_households.csv", stringsAsFactors = FALSE)

get_val <- function(name) input$Value[input$Parameters == name]
quintiles <- 1:5
scale_to_target_pop <- get_val("target_pop") / get_val("pop")
plain_number <- function(x) format(round(x, 0), scientific = FALSE, trim = TRUE)
cost_label <- function(x) paste0("INR ", round(x / 1000000, 0), "M")
required_hces_cols <- c("sector", "hh_size", "hh_usual_cons_exp_mnth", "household_quintile")
missing_hces_cols <- setdiff(required_hces_cols, names(hces))
if (length(missing_hces_cols) > 0) {
  stop("Missing required columns in hces_simulated_100k_households.csv: ",
       paste(missing_hces_cols, collapse = ", "))
}
hces$sector_raw <- trimws(as.character(hces$sector))
hces$sector_code <- ifelse(tolower(hces$sector_raw) %in% c("1", "r", "rural"),
                           "r",
                           ifelse(tolower(hces$sector_raw) %in% c("2", "u", "urban"),
                                  "u",
                                  NA))
hces$hh_consumption_annual <- as.numeric(hces$hh_usual_cons_exp_mnth) * 12
hces$household_quintile <- as.integer(hces$household_quintile)
hces <- hces[!is.na(hces$sector_code) &
               !is.na(hces$hh_consumption_annual) &
               hces$hh_consumption_annual > 0 &
               hces$household_quintile %in% quintiles, ]
get_hces_quintile <- function(i) {
  households <- hces[hces$household_quintile == i, ]
  if (nrow(households) == 0) {
    stop("No simulated households found for household_quintile = ", i)
  }
  households
}
household_threshold_rate <- function(oop, households, threshold) {
  mean(oop > threshold * households$hh_consumption_annual)
}
household_impov_rate <- function(oop, households, poverty_line) {
  mean(households$hh_consumption_annual >= poverty_line &
         households$hh_consumption_annual - oop < poverty_line)
}
insured_med_oop <- function(oop_dr, oop_vtdr, vtdr_share,
                            insurance_coverage,
                            coverage_med_dr,
                            coverage_med_vtdr) {
  oop_dr * (1 - vtdr_share) * (1 - insurance_coverage * coverage_med_dr) +
    oop_vtdr * vtdr_share * (1 - insurance_coverage * coverage_med_vtdr)
}
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
  insured_med_oop(
    get_val(paste0("oop_med_dr_", i)),
    get_val(paste0("oop_med_vtdr_", i)),
    get_val(paste0("vtdr_dr_", i)),
    get_val(paste0("ins_cov_", i)),
    get_val("coverage_med_dr_insured"),
    get_val("coverage_med_vtdr_insured")
  )
})

#direct medical OOP after intervention per quintile
direct_med_OOP_inv <- treated_inv*sapply(quintiles, function(i) {
  insured_med_oop(
    get_val(paste0("oop_med_dr_", i)),
    get_val(paste0("oop_med_vtdr_", i)),
    get_val(paste0("vtdr_dr_", i)) * (1 - get_val("vtdr_reduction")),
    get_val(paste0("ins_cov_", i)),
    get_val("coverage_med_dr_insured"),
    get_val("coverage_med_vtdr_insured")
  )
})

#Additional direct medical OOP due to intervention
OOP_med_additional <- direct_med_OOP_inv-direct_med_OOP_baseline
OOP_med_additional_target <- OOP_med_additional * scale_to_target_pop

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
OOP_total_additional_target <- OOP_total_additional * scale_to_target_pop

#Plots
library(ggplot2)
plot_dr <- data.frame(
  quintile = factor(c("I","II","III","IV","V"), levels=c("I","II","III","IV","V")))

#combined plot of additional direct medical OOP and total OOP due to intervention
plot_oop_q <- data.frame(
  quintile = rep(plot_dr$quintile, times = 2),
  outcome = factor(
    rep(c("Direct Medical OOP\n(INR)", "Total OOP\n(INR)"), each = length(quintiles)),
    levels = c("Direct Medical OOP\n(INR)", "Total OOP\n(INR)")
  ),
  value = c(OOP_med_additional_target, OOP_total_additional_target)
)
plot_oop_q$label <- cost_label(plot_oop_q$value)

ggplot(plot_oop_q, aes(x = quintile, y = value, fill = outcome)) +
  geom_col(position = position_dodge(width = 0.75), width = 0.65) +
  geom_text(aes(label = label),
            position = position_dodge(width = 0.75),
            vjust = -0.35,
            size = 3) +
  scale_fill_manual(values = c("Direct Medical OOP\n(INR)" = "orangered",
                               "Total OOP\n(INR)" = "orangered4"),
                    labels = c("Direct Medical OOP", "Total OOP")) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.18))) +
  labs(title = "Additional OOP Due to Intervention by Quintile",
       x = "Income Quintile (Poorest to Richest)",
       y = NULL,
       fill = NULL) +
  theme_classic() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    legend.position = "top",
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank()
  )

#2.3 Catastrophic Health Expenditure 

# Per-person total OOP — baseline 
per_person_oop_baseline <- sapply(quintiles, function(i) {
  vtdr_b <- get_val(paste0("vtdr_dr_", i))
  
  med    <- insured_med_oop(
    get_val(paste0("oop_med_dr_", i)),
    get_val(paste0("oop_med_vtdr_", i)),
    vtdr_b,
    get_val(paste0("ins_cov_", i)),
    get_val("coverage_med_dr_insured"),
    get_val("coverage_med_vtdr_insured")
  )
  nonmed <- get_val(paste0("oop_nonmed_dr_", i)) * (1 - vtdr_b) +
    get_val(paste0("oop_nonmed_vtdr_", i)) * vtdr_b
  
  med + nonmed
})

# Per-person total OOP — intervention (VTDR proportion reduced by vtdr_reduction)
per_person_oop_inv <- sapply(quintiles, function(i) {
  vtdr_inv <- get_val(paste0("vtdr_dr_", i)) * (1 - get_val("vtdr_reduction"))
  
  med    <- insured_med_oop(
    get_val(paste0("oop_med_dr_", i)),
    get_val(paste0("oop_med_vtdr_", i)),
    vtdr_inv,
    get_val(paste0("ins_cov_", i)),
    get_val("coverage_med_dr_insured"),
    get_val("coverage_med_vtdr_insured")
  )
  nonmed <- get_val(paste0("oop_nonmed_dr_", i)) * (1 - vtdr_inv) +
    get_val(paste0("oop_nonmed_vtdr_", i)) * vtdr_inv
  
  med + nonmed
})

# Simulated household consumption distributions by sector-specific household
# quintile. Rural and urban households in the same quintile number are pooled
# according to their simulated household shares.
households_q <- lapply(quintiles, get_hces_quintile)

#CHE thresholds
th1 <- get_val("th1")   # 0.10
th2 <- get_val("th2")   # 0.25

# CHE rates per quintile, calculated across simulated households
che_10_rate_baseline <- sapply(quintiles, function(i) {
  household_threshold_rate(per_person_oop_baseline[i], households_q[[i]], th1)
})
che_25_rate_baseline <- sapply(quintiles, function(i) {
  household_threshold_rate(per_person_oop_baseline[i], households_q[[i]], th2)
})
che_10_rate_inv <- sapply(quintiles, function(i) {
  household_threshold_rate(per_person_oop_inv[i], households_q[[i]], th1)
})
che_25_rate_inv <- sapply(quintiles, function(i) {
  household_threshold_rate(per_person_oop_inv[i], households_q[[i]], th2)
})

# Total CHE cases = number treated x simulated-household CHE rate
che_10_baseline <- treated_baseline * che_10_rate_baseline
che_25_baseline <- treated_baseline * che_25_rate_baseline
che_10_inv      <- treated_inv * che_10_rate_inv
che_25_inv      <- treated_inv * che_25_rate_inv

# CHE averted
che_10_additional <- che_10_inv - che_10_baseline
che_25_additional <- che_25_inv - che_25_baseline
che_10_additional_target <- che_10_additional * scale_to_target_pop
che_25_additional_target <- che_25_additional * scale_to_target_pop

#graphs
ggplot(plot_dr, aes(x=quintile, y=che_10_additional_target)) +
  geom_col(fill="coral1", width=0.6)+
  geom_text(aes(label = plain_number(che_10_additional_target)), vjust = -0.4, size = 3) +
  scale_y_continuous(labels = plain_number,
                     expand = expansion(mult = c(0, 0.1))) +
  labs(title="Additional CHE Cases (10% threshold) Due to Intervention",
       x="Income Quintile (Poorest to Richest)",
       y="Additional CHE Cases") +
  theme_classic()+
  theme(plot.title=element_text(hjust=0.5, face="bold"))

ggplot(plot_dr, aes(x=quintile, y=che_25_additional_target)) +
  geom_col(fill="coral1", width=0.6)+
  geom_text(aes(label = plain_number(che_25_additional_target)), vjust = -0.4, size = 3) +
  scale_y_continuous(labels = plain_number,
                     expand = expansion(mult = c(0, 0.1))) +
  labs(title="Additional CHE Cases (25% threshold) Due to Intervention",
       x="Income Quintile (Poorest to Richest)",
       y="Additional CHE Cases") +
  theme_classic()+
  theme(plot.title=element_text(hjust=0.5, face="bold"))

#section 2.3 impoverishment due to medical expense 
# Annual household poverty line from a daily household poverty line
pov_annual <- get_val("pov") * 365

# Impoverishment rates per quintile, calculated across simulated households
impov_rate_baseline <- sapply(quintiles, function(i) {
  household_impov_rate(per_person_oop_baseline[i], households_q[[i]], pov_annual)
})
impov_rate_inv <- sapply(quintiles, function(i) {
  household_impov_rate(per_person_oop_inv[i], households_q[[i]], pov_annual)
})

# Total impoverishment cases per quintile = treated patients x simulated-household rate
impov_baseline <- treated_baseline * impov_rate_baseline
impov_inv      <- treated_inv * impov_rate_inv

impov_additional <- impov_inv - impov_baseline
impov_additional_target <- impov_additional * scale_to_target_pop

ggplot(plot_dr, aes(x=quintile, y=impov_additional_target)) +
  geom_col(fill="coral1", width=0.6)+
  geom_text(aes(label = plain_number(impov_additional_target)), vjust = -0.4, size = 3) +
  scale_y_continuous(labels = plain_number,
                     expand = expansion(mult = c(0, 0.1))) +
  labs(title="Change in Impoverishment Cases Due to Intervention",
       x="Income Quintile (Poorest to Richest)",
       y="Change in Impoverishment Cases") +
  theme_classic()+
  theme(plot.title=element_text(hjust=0.5, face="bold"))

#dashboard
health_benefit_dash <- as.data.frame(rbind(
  round(OOP_med_additional_target, 0),
  round(OOP_total_additional_target, 0),
  round(che_10_additional_target, 0),
  round(che_25_additional_target, 0),
  round(impov_additional_target, 0)
))
colnames(health_benefit_dash) <- c("I","II","III","IV","V")
rownames(health_benefit_dash) <- c(
  "Additional Direct Medical OOP due to Intervention (scaled to target population)",
  "Additional Total OOP due to Intervention (scaled to target population)",
  "Additional CHE Cases (10% threshold, scaled to target population)",
  "Additional CHE Cases (25% threshold, scaled to target population)",
  "Change in Impoverishment Cases (scaled to target population)"
)
print(health_benefit_dash)

# Sensitivity analysis for medical insurance coverage assumptions.
run_insurance_sensitivity_quintile <- function(
    coverage_med_dr_values = c(0.30, 0.50, 0.70),
    coverage_med_vtdr_values = c(0.40, 0.70, 0.90)) {
  scenarios <- expand.grid(
    coverage_med_dr_insured = coverage_med_dr_values,
    coverage_med_vtdr_insured = coverage_med_vtdr_values
  )
  
  results <- lapply(seq_len(nrow(scenarios)), function(row_id) {
    cov_dr <- scenarios$coverage_med_dr_insured[row_id]
    cov_vtdr <- scenarios$coverage_med_vtdr_insured[row_id]
    
    med_baseline <- treated_baseline * sapply(quintiles, function(i) {
      insured_med_oop(
        get_val(paste0("oop_med_dr_", i)),
        get_val(paste0("oop_med_vtdr_", i)),
        get_val(paste0("vtdr_dr_", i)),
        get_val(paste0("ins_cov_", i)),
        cov_dr,
        cov_vtdr
      )
    })
    med_inv <- treated_inv * sapply(quintiles, function(i) {
      insured_med_oop(
        get_val(paste0("oop_med_dr_", i)),
        get_val(paste0("oop_med_vtdr_", i)),
        get_val(paste0("vtdr_dr_", i)) * (1 - get_val("vtdr_reduction")),
        get_val(paste0("ins_cov_", i)),
        cov_dr,
        cov_vtdr
      )
    })
    
    oop_baseline <- sapply(quintiles, function(i) {
      vtdr_b <- get_val(paste0("vtdr_dr_", i))
      med <- insured_med_oop(
        get_val(paste0("oop_med_dr_", i)),
        get_val(paste0("oop_med_vtdr_", i)),
        vtdr_b,
        get_val(paste0("ins_cov_", i)),
        cov_dr,
        cov_vtdr
      )
      nonmed <- get_val(paste0("oop_nonmed_dr_", i)) * (1 - vtdr_b) +
        get_val(paste0("oop_nonmed_vtdr_", i)) * vtdr_b
      med + nonmed
    })
    oop_inv <- sapply(quintiles, function(i) {
      vtdr_inv <- get_val(paste0("vtdr_dr_", i)) * (1 - get_val("vtdr_reduction"))
      med <- insured_med_oop(
        get_val(paste0("oop_med_dr_", i)),
        get_val(paste0("oop_med_vtdr_", i)),
        vtdr_inv,
        get_val(paste0("ins_cov_", i)),
        cov_dr,
        cov_vtdr
      )
      nonmed <- get_val(paste0("oop_nonmed_dr_", i)) * (1 - vtdr_inv) +
        get_val(paste0("oop_nonmed_vtdr_", i)) * vtdr_inv
      med + nonmed
    })
    
    che_10_b <- sapply(quintiles, function(i) household_threshold_rate(oop_baseline[i], households_q[[i]], th1))
    che_25_b <- sapply(quintiles, function(i) household_threshold_rate(oop_baseline[i], households_q[[i]], th2))
    che_10_i <- sapply(quintiles, function(i) household_threshold_rate(oop_inv[i], households_q[[i]], th1))
    che_25_i <- sapply(quintiles, function(i) household_threshold_rate(oop_inv[i], households_q[[i]], th2))
    impov_b <- sapply(quintiles, function(i) household_impov_rate(oop_baseline[i], households_q[[i]], pov_annual))
    impov_i <- sapply(quintiles, function(i) household_impov_rate(oop_inv[i], households_q[[i]], pov_annual))
    
    data.frame(
      coverage_med_dr_insured = cov_dr,
      coverage_med_vtdr_insured = cov_vtdr,
      additional_direct_med_oop = round(sum((med_inv - med_baseline) * scale_to_target_pop), 0),
      additional_total_oop = round(sum(((med_inv + direct_nonmed_OOP_inv) -
                                          (med_baseline + direct_nonmed_OOP_baseline)) *
                                         scale_to_target_pop), 0),
      additional_che_10_cases = round(sum((treated_inv * che_10_i -
                                             treated_baseline * che_10_b) *
                                            scale_to_target_pop), 0),
      additional_che_25_cases = round(sum((treated_inv * che_25_i -
                                             treated_baseline * che_25_b) *
                                            scale_to_target_pop), 0),
      change_impoverishment_cases = round(sum((treated_inv * impov_i -
                                                treated_baseline * impov_b) *
                                               scale_to_target_pop), 0)
    )
  })
  
  do.call(rbind, results)
}

insurance_sensitivity_quintile <- run_insurance_sensitivity_quintile()
print(insurance_sensitivity_quintile)
