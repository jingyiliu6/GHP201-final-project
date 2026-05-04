#cost: additional OOP incurred to patients by quintile
#catastrophic health expenditure cases
rm(list = ls())

input <- read.csv("input table new.csv")
input$Parameters <- trimws(input$Parameters)
input$Value <- as.numeric(gsub(",", "", input$Value))
hces <- read.csv("hces_simulated_100k_households.csv", stringsAsFactors = FALSE)

get_val <- function(name) input$Value[input$Parameters == name]
quintiles <- 1:5
quintile_labels <- c("I", "II", "III", "IV", "V")
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
hces$sector_label <- factor(ifelse(hces$sector_code == "u", "Urban", "Rural"),
                            levels = c("Urban", "Rural"))
hces$hh_size <- as.numeric(hces$hh_size)
hces$hh_usual_cons_exp_mnth <- as.numeric(hces$hh_usual_cons_exp_mnth)
hces$hh_consumption_annual <- hces$hh_usual_cons_exp_mnth * 12
hces$household_quintile <- as.integer(hces$household_quintile)
hces <- hces[!is.na(hces$sector_code) &
               !is.na(hces$hh_size) &
               hces$hh_size > 0 &
               !is.na(hces$hh_consumption_annual) &
               hces$hh_consumption_annual > 0 &
               hces$household_quintile %in% quintiles, ]

if (nrow(hces) == 0) {
  stop("No valid simulated HCES households remain after cleaning.")
}

summarize_hces <- function(data, group_var, group_name) {
  grouped <- split(data, data[[group_var]])
  summary <- do.call(rbind, lapply(names(grouped), function(group) {
    households <- grouped[[group]]
    data.frame(
      group = group,
      households = nrow(households),
      household_share = round(nrow(households) / nrow(data), 4),
      mean_hh_size = round(mean(households$hh_size), 2),
      mean_monthly_consumption = round(mean(households$hh_usual_cons_exp_mnth), 0),
      median_monthly_consumption = round(median(households$hh_usual_cons_exp_mnth), 0),
      mean_annual_consumption = round(mean(households$hh_consumption_annual), 0),
      median_annual_consumption = round(median(households$hh_consumption_annual), 0)
    )
  }))
  rownames(summary) <- NULL
  names(summary)[1] <- group_name
  summary
}

hces_summary_quintile <- summarize_hces(hces, "household_quintile", "household_quintile")
hces_summary_urbanicity <- summarize_hces(hces, "sector_label", "urbanicity")
print(hces_summary_quintile)
print(hces_summary_urbanicity)

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

# Number of DR patients treated at baseline and after intervention.
treated_baseline <- sapply(quintiles, function(i) {
  get_val(paste0("pop_", i)) *
    get_val(paste0("dr_", i)) *
    get_val(paste0("pcu_", i)) *
    get_val("cov_b") *
    get_val("sens_b") *
    (1 - get_val("lfu_referral")) *
    (1 - get_val("lfu_treat"))
})

treated_inv <- sapply(quintiles, function(i) {
  get_val(paste0("pop_", i)) *
    get_val(paste0("dr_", i)) *
    get_val(paste0("pcu_", i)) *
    get_val("cov_inv") *
    get_val("sens_inv") *
    (1 - get_val("lfu_referral")) *
    (1 - get_val("lfu_treat"))
})

direct_nonmed_OOP_baseline <- treated_baseline * sapply(quintiles, function(i) {
  get_val(paste0("oop_nonmed_dr_", i)) * (1 - get_val(paste0("vtdr_dr_", i))) +
    get_val(paste0("oop_nonmed_vtdr_", i)) * get_val(paste0("vtdr_dr_", i))
})

direct_nonmed_OOP_inv <- treated_inv * sapply(quintiles, function(i) {
  vtdr_inv <- get_val(paste0("vtdr_dr_", i)) * (1 - get_val("vtdr_reduction"))
  get_val(paste0("oop_nonmed_dr_", i)) * (1 - vtdr_inv) +
    get_val(paste0("oop_nonmed_vtdr_", i)) * vtdr_inv
})

households_q <- lapply(quintiles, get_hces_quintile)
th1 <- get_val("th1")
pov_annual <- get_val("pov") * 365

calculate_quintile_results <- function(coverage_med_dr_insured,
                                       coverage_med_vtdr_insured,
                                       scenario_label) {
  direct_med_OOP_baseline <- treated_baseline * sapply(quintiles, function(i) {
    insured_med_oop(
      get_val(paste0("oop_med_dr_", i)),
      get_val(paste0("oop_med_vtdr_", i)),
      get_val(paste0("vtdr_dr_", i)),
      get_val(paste0("ins_cov_", i)),
      coverage_med_dr_insured,
      coverage_med_vtdr_insured
    )
  })
  
  direct_med_OOP_inv <- treated_inv * sapply(quintiles, function(i) {
    vtdr_inv <- get_val(paste0("vtdr_dr_", i)) * (1 - get_val("vtdr_reduction"))
    insured_med_oop(
      get_val(paste0("oop_med_dr_", i)),
      get_val(paste0("oop_med_vtdr_", i)),
      vtdr_inv,
      get_val(paste0("ins_cov_", i)),
      coverage_med_dr_insured,
      coverage_med_vtdr_insured
    )
  })
  
  per_person_oop_baseline <- sapply(quintiles, function(i) {
    vtdr_b <- get_val(paste0("vtdr_dr_", i))
    med <- insured_med_oop(
      get_val(paste0("oop_med_dr_", i)),
      get_val(paste0("oop_med_vtdr_", i)),
      vtdr_b,
      get_val(paste0("ins_cov_", i)),
      coverage_med_dr_insured,
      coverage_med_vtdr_insured
    )
    nonmed <- get_val(paste0("oop_nonmed_dr_", i)) * (1 - vtdr_b) +
      get_val(paste0("oop_nonmed_vtdr_", i)) * vtdr_b
    med + nonmed
  })
  
  per_person_oop_inv <- sapply(quintiles, function(i) {
    vtdr_inv <- get_val(paste0("vtdr_dr_", i)) * (1 - get_val("vtdr_reduction"))
    med <- insured_med_oop(
      get_val(paste0("oop_med_dr_", i)),
      get_val(paste0("oop_med_vtdr_", i)),
      vtdr_inv,
      get_val(paste0("ins_cov_", i)),
      coverage_med_dr_insured,
      coverage_med_vtdr_insured
    )
    nonmed <- get_val(paste0("oop_nonmed_dr_", i)) * (1 - vtdr_inv) +
      get_val(paste0("oop_nonmed_vtdr_", i)) * vtdr_inv
    med + nonmed
  })
  
  OOP_med_additional <- direct_med_OOP_inv - direct_med_OOP_baseline
  OOP_total_additional <- (direct_med_OOP_inv + direct_nonmed_OOP_inv) -
    (direct_med_OOP_baseline + direct_nonmed_OOP_baseline)
  
  che_10_rate_baseline <- sapply(quintiles, function(i) {
    household_threshold_rate(per_person_oop_baseline[i], households_q[[i]], th1)
  })
  che_10_rate_inv <- sapply(quintiles, function(i) {
    household_threshold_rate(per_person_oop_inv[i], households_q[[i]], th1)
  })
  che_10_additional <- treated_inv * che_10_rate_inv -
    treated_baseline * che_10_rate_baseline
  
  impov_rate_baseline <- sapply(quintiles, function(i) {
    household_impov_rate(per_person_oop_baseline[i], households_q[[i]], pov_annual)
  })
  impov_rate_inv <- sapply(quintiles, function(i) {
    household_impov_rate(per_person_oop_inv[i], households_q[[i]], pov_annual)
  })
  impov_additional <- treated_inv * impov_rate_inv -
    treated_baseline * impov_rate_baseline
  
  list(
    scenario_label = scenario_label,
    OOP_med_additional = OOP_med_additional * scale_to_target_pop,
    OOP_total_additional = OOP_total_additional * scale_to_target_pop,
    che_10_additional = che_10_additional * scale_to_target_pop,
    impov_additional = impov_additional * scale_to_target_pop
  )
}

make_oop_plot <- function(results) {
  plot_oop_q <- data.frame(
    quintile = factor(rep(quintile_labels, times = 2), levels = quintile_labels),
    outcome = factor(
      rep(c("Direct Medical OOP", "Total OOP"), each = length(quintiles)),
      levels = c("Direct Medical OOP", "Total OOP")
    ),
    value = c(results$OOP_med_additional, results$OOP_total_additional)
  )
  plot_oop_q$label <- cost_label(plot_oop_q$value)
  plot_oop_q$label_vjust <- ifelse(plot_oop_q$value >= 0, -0.35, 1.25)
  
  ggplot(plot_oop_q, aes(x = quintile, y = value, fill = outcome)) +
    geom_col(position = position_dodge(width = 0.75), width = 0.65) +
    geom_text(aes(label = label, vjust = label_vjust),
              position = position_dodge(width = 0.75),
              size = 3) +
    scale_fill_manual(values = c("Direct Medical OOP" = "#f58220",
                                 "Total OOP" = "#b74616")) +
    scale_y_continuous(expand = expansion(mult = c(0.18, 0.18))) +
    labs(title = paste0("Additional OOP Due to Intervention by Quintile: ",
                        results$scenario_label),
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
}

make_che_plot <- function(results) {
  plot_che_q <- data.frame(
    quintile = factor(quintile_labels, levels = quintile_labels),
    value = results$che_10_additional
  )
  plot_che_q$label <- plain_number(plot_che_q$value)
  plot_che_q$label_vjust <- ifelse(plot_che_q$value >= 0, -0.35, 1.25)
  
  ggplot(plot_che_q, aes(x = quintile, y = value)) +
    geom_col(fill = "#f58220", width = 0.6) +
    geom_text(aes(label = label, vjust = label_vjust), size = 3) +
    scale_y_continuous(expand = expansion(mult = c(0.18, 0.18))) +
    labs(title = paste0("Additional CHE Cases (10%): ",
                        results$scenario_label),
         x = "Income Quintile (Poorest to Richest)",
         y = "Additional CHE Cases") +
    theme_classic() +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold"),
      axis.text.y = element_blank(),
      axis.ticks.y = element_blank()
    )
}

make_ihe_plot <- function(results) {
  plot_ihe_q <- data.frame(
    quintile = factor(quintile_labels, levels = quintile_labels),
    value = results$impov_additional
  )
  plot_ihe_q$label <- plain_number(plot_ihe_q$value)
  plot_ihe_q$label_vjust <- ifelse(plot_ihe_q$value >= 0, -0.35, 1.25)
  
  ggplot(plot_ihe_q, aes(x = quintile, y = value)) +
    geom_col(fill = "#ffcc00", width = 0.6) +
    geom_text(aes(label = label, vjust = label_vjust), size = 3) +
    scale_y_continuous(expand = expansion(mult = c(0.18, 0.18))) +
    labs(title = paste0("Change in Impoverishment Cases: ",
                        results$scenario_label),
         x = "Income Quintile (Poorest to Richest)",
         y = "Change in Impoverishment Cases") +
    theme_classic() +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold"),
      axis.text.y = element_blank(),
      axis.ticks.y = element_blank()
    )
}

make_dashboard <- function(results) {
  dash <- as.data.frame(rbind(
    round(results$OOP_med_additional, 0),
    round(results$OOP_total_additional, 0),
    round(results$che_10_additional, 0),
    round(results$impov_additional, 0)
  ))
  colnames(dash) <- quintile_labels
  rownames(dash) <- c(
    paste0("Additional Direct Medical OOP - ", results$scenario_label),
    paste0("Additional Total OOP - ", results$scenario_label),
    paste0("Additional CHE Cases (10%) - ", results$scenario_label),
    paste0("Change in Impoverishment Cases - ", results$scenario_label)
  )
  dash
}

library(ggplot2)

results_0 <- calculate_quintile_results(
  coverage_med_dr_insured = 0,
  coverage_med_vtdr_insured = 0,
  scenario_label = "0% Reimbersement"
)
results_100 <- calculate_quintile_results(
  coverage_med_dr_insured = 1,
  coverage_med_vtdr_insured = 1,
  scenario_label = "100% Reimbersement"
)

print(make_dashboard(results_0))
make_oop_plot(results_0)
make_che_plot(results_0)
make_ihe_plot(results_0)

print(make_dashboard(results_100))
make_oop_plot(results_100)
make_che_plot(results_100)
make_ihe_plot(results_100)
