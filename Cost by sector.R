rm(list = ls())

input <- read.csv("input table new.csv")
input$Parameters <- trimws(input$Parameters)
input$Value <- as.numeric(gsub(",", "", input$Value))
hces_file_candidates <- c("hces_simulated_1m_households.csv",
                          "hces_simulated_100k_households.csv")
hces_file <- hces_file_candidates[file.exists(hces_file_candidates)][1]
if (is.na(hces_file)) {
  stop("Could not find a simulated HCES household CSV file.")
}
hces <- read.csv(hces_file, stringsAsFactors = FALSE)
cat("Using simulated HCES household file:", hces_file, "\n")
cat("Simulated household rows loaded:", nrow(hces), "\n")

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

required_hces_cols <- c("sector", "hh_size", "hh_usual_cons_exp_mnth")
missing_hces_cols <- setdiff(required_hces_cols, names(hces))
if (length(missing_hces_cols) > 0) {
  stop("Missing required columns in ", hces_file, ": ",
       paste(missing_hces_cols, collapse = ", "))
}
assign_equiv_quintile <- function(data) {
  sort_order <- order(data$hh_equiv_cons_exp_mnth, seq_len(nrow(data)))
  sorted_midpoint <- (seq_len(nrow(data)) - 0.5) / nrow(data)
  quintile <- integer(nrow(data))
  quintile[sort_order] <- floor(5 * sorted_midpoint) + 1
  pmin(pmax(quintile, 1), 5)
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
if ("hh_equiv_cons_exp_mnth" %in% names(hces)) {
  hces$hh_equiv_cons_exp_mnth <- as.numeric(hces$hh_equiv_cons_exp_mnth)
} else {
  hces$hh_equiv_cons_exp_mnth <- hces$hh_usual_cons_exp_mnth / sqrt(hces$hh_size)
}
# Always derive the household poverty line from the current input table so the
# model does not use a stale value stored in a previously generated cohort file.
hces$hh_poverty_line_annual <- get_val("pov") * hces$hh_size * 365
hces <- hces[!is.na(hces$sector_code) &
               !is.na(hces$hh_size) &
               hces$hh_size > 0 &
               !is.na(hces$hh_consumption_annual) &
               hces$hh_consumption_annual > 0 &
               !is.na(hces$hh_equiv_cons_exp_mnth) &
               hces$hh_equiv_cons_exp_mnth > 0 &
               !is.na(hces$hh_poverty_line_annual) &
               hces$hh_poverty_line_annual > 0, ]
hces$household_quintile <- assign_equiv_quintile(hces)

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
      mean_equiv_monthly_consumption = round(mean(households$hh_equiv_cons_exp_mnth), 0),
      median_equiv_monthly_consumption = round(median(households$hh_equiv_cons_exp_mnth), 0),
      mean_annual_consumption = round(mean(households$hh_consumption_annual), 0),
      median_annual_consumption = round(median(households$hh_consumption_annual), 0),
      mean_annual_poverty_line = round(mean(households$hh_poverty_line_annual), 0),
      median_annual_poverty_line = round(median(households$hh_poverty_line_annual), 0)
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

get_hces_sector <- function(s) {
  households <- hces[hces$sector_code == s, ]
  if (nrow(households) == 0) {
    stop("No simulated households found for sector = ", s)
  }
  households
}

household_threshold_rate <- function(oop, households, threshold) {
  mean(oop > threshold * households$hh_consumption_annual)
}

household_impov_rate <- function(oop, households) {
  mean(households$hh_consumption_annual >= households$hh_poverty_line_annual &
         households$hh_consumption_annual - oop < households$hh_poverty_line_annual)
}

insured_med_oop <- function(oop_dr, oop_vtdr, vtdr_share,
                            insurance_coverage,
                            coverage_med_dr,
                            coverage_med_vtdr) {
  oop_dr * (1 - vtdr_share) * (1 - insurance_coverage * coverage_med_dr) +
    oop_vtdr * vtdr_share * (1 - insurance_coverage * coverage_med_vtdr)
}

# Number of DR patients treated at baseline and after intervention.
treated_baseline_ur <- sapply(sectors, function(s) {
  get_val(paste0("pop_", s)) *
    get_val(paste0("dr_", s)) *
    get_pcu_sector(s) *
    get_val("cov_b") *
    get_val("sens_b") *
    (1 - get_val("lfu_referral")) *
    (1 - get_val("lfu_treat"))
})

treated_inv_ur <- sapply(sectors, function(s) {
  get_val(paste0("pop_", s)) *
    get_val(paste0("dr_", s)) *
    get_pcu_sector(s) *
    get_val("cov_inv") *
    get_val("sens_inv") *
    (1 - get_val("lfu_referral")) *
    (1 - get_val("lfu_treat"))
})

direct_nonmed_OOP_baseline_ur <- treated_baseline_ur * sapply(sectors, function(s) {
  get_val(paste0("oop_nonmed_dr_", s)) * (1 - get_val(paste0("vtdr_dr_", s))) +
    get_val(paste0("oop_nonmed_vtdr_", s)) * get_val(paste0("vtdr_dr_", s))
})

direct_nonmed_OOP_inv_ur <- treated_inv_ur * sapply(sectors, function(s) {
  vtdr_inv <- get_val(paste0("vtdr_dr_", s)) * (1 - get_val("vtdr_reduction"))
  get_val(paste0("oop_nonmed_dr_", s)) * (1 - vtdr_inv) +
    get_val(paste0("oop_nonmed_vtdr_", s)) * vtdr_inv
})

households_ur <- lapply(sectors, get_hces_sector)
names(households_ur) <- sectors

th1 <- get_val("th1")

calculate_sector_results <- function(coverage_med_dr_insured,
                                     coverage_med_vtdr_insured,
                                     scenario_label) {
  direct_med_OOP_baseline_ur <- treated_baseline_ur * sapply(sectors, function(s) {
    insured_med_oop(
      get_val(paste0("oop_med_dr_", s)),
      get_val(paste0("oop_med_vtdr_", s)),
      get_val(paste0("vtdr_dr_", s)),
      get_insurance_coverage_sector(s),
      coverage_med_dr_insured,
      coverage_med_vtdr_insured
    )
  })
  
  direct_med_OOP_inv_ur <- treated_inv_ur * sapply(sectors, function(s) {
    vtdr_inv <- get_val(paste0("vtdr_dr_", s)) * (1 - get_val("vtdr_reduction"))
    insured_med_oop(
      get_val(paste0("oop_med_dr_", s)),
      get_val(paste0("oop_med_vtdr_", s)),
      vtdr_inv,
      get_insurance_coverage_sector(s),
      coverage_med_dr_insured,
      coverage_med_vtdr_insured
    )
  })
  
  per_person_oop_baseline_ur <- sapply(sectors, function(s) {
    vtdr_b <- get_val(paste0("vtdr_dr_", s))
    med <- insured_med_oop(
      get_val(paste0("oop_med_dr_", s)),
      get_val(paste0("oop_med_vtdr_", s)),
      vtdr_b,
      get_insurance_coverage_sector(s),
      coverage_med_dr_insured,
      coverage_med_vtdr_insured
    )
    nonmed <- get_val(paste0("oop_nonmed_dr_", s)) * (1 - vtdr_b) +
      get_val(paste0("oop_nonmed_vtdr_", s)) * vtdr_b
    med + nonmed
  })
  
  per_person_oop_inv_ur <- sapply(sectors, function(s) {
    vtdr_inv <- get_val(paste0("vtdr_dr_", s)) * (1 - get_val("vtdr_reduction"))
    med <- insured_med_oop(
      get_val(paste0("oop_med_dr_", s)),
      get_val(paste0("oop_med_vtdr_", s)),
      vtdr_inv,
      get_insurance_coverage_sector(s),
      coverage_med_dr_insured,
      coverage_med_vtdr_insured
    )
    nonmed <- get_val(paste0("oop_nonmed_dr_", s)) * (1 - vtdr_inv) +
      get_val(paste0("oop_nonmed_vtdr_", s)) * vtdr_inv
    med + nonmed
  })
  
  OOP_med_additional_ur <- direct_med_OOP_inv_ur - direct_med_OOP_baseline_ur
  OOP_total_additional_ur <- (direct_med_OOP_inv_ur + direct_nonmed_OOP_inv_ur) -
    (direct_med_OOP_baseline_ur + direct_nonmed_OOP_baseline_ur)
  
  che_10_rate_baseline_ur <- sapply(sectors, function(s) {
    household_threshold_rate(per_person_oop_baseline_ur[s], households_ur[[s]], th1)
  })
  che_10_rate_inv_ur <- sapply(sectors, function(s) {
    household_threshold_rate(per_person_oop_inv_ur[s], households_ur[[s]], th1)
  })
  che_10_additional_ur <- treated_inv_ur * che_10_rate_inv_ur -
    treated_baseline_ur * che_10_rate_baseline_ur
  
  impov_rate_baseline_ur <- sapply(sectors, function(s) {
    household_impov_rate(per_person_oop_baseline_ur[s], households_ur[[s]])
  })
  impov_rate_inv_ur <- sapply(sectors, function(s) {
    household_impov_rate(per_person_oop_inv_ur[s], households_ur[[s]])
  })
  impov_additional_ur <- treated_inv_ur * impov_rate_inv_ur -
    treated_baseline_ur * impov_rate_baseline_ur
  
  list(
    scenario_label = scenario_label,
    OOP_med_additional = OOP_med_additional_ur * scale_to_target_pop,
    OOP_total_additional = OOP_total_additional_ur * scale_to_target_pop,
    che_10_additional = che_10_additional_ur * scale_to_target_pop,
    impov_additional = impov_additional_ur * scale_to_target_pop
  )
}

make_burden_plot <- function(results) {
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
      results$OOP_med_additional,
      results$OOP_total_additional,
      results$che_10_additional,
      results$impov_additional
    ),
    unit = rep(c("INR", "INR", "Number", "Number"), each = 2)
  )
  plot_burden_ur$label <- burden_label(plot_burden_ur$value, plot_burden_ur$unit)
  plot_burden_ur$label_vjust <- ifelse(plot_burden_ur$value >= 0, -0.35, 1.25)
  
  ggplot(plot_burden_ur, aes(x = sector, y = value, fill = sector)) +
    geom_col(width = 0.62) +
    geom_text(aes(label = label, vjust = label_vjust), size = 3) +
    scale_fill_manual(values = c("Urban" = "#f58220", "Rural" = "#c75301")) + 
    scale_y_continuous(expand = expansion(mult = c(0.18, 0.18))) +
    facet_wrap(~ category, scales = "free_y", nrow = 1) +
    labs(title = paste0("Rural vs Urban Burden: ", results$scenario_label),
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
}

make_dashboard <- function(results) {
  dash <- as.data.frame(rbind(
    round(results$OOP_med_additional, 0),
    round(results$OOP_total_additional, 0),
    round(results$che_10_additional, 0),
    round(results$impov_additional, 0)
  ))
  colnames(dash) <- c("Urban", "Rural")
  rownames(dash) <- c(
    paste0("Additional Direct Medical OOP due to Intervention - ", results$scenario_label),
    paste0("Additional Total OOP due to Intervention - ", results$scenario_label),
    paste0("Additional CHE Cases (10% threshold) - ", results$scenario_label),
    paste0("Change in Impoverishment Cases - ", results$scenario_label)
  )
  dash
}

library(ggplot2)

results_0 <- calculate_sector_results(
  coverage_med_dr_insured = 0,
  coverage_med_vtdr_insured = 0,
  scenario_label = "0% DR/VTDR Treatment Coverage"
)
results_100 <- calculate_sector_results(
  coverage_med_dr_insured = 1,
  coverage_med_vtdr_insured = 1,
  scenario_label = "100% DR/VTDR Treatment Coverage"
)

print(make_dashboard(results_0))
make_burden_plot(results_0)

print(make_dashboard(results_100))
make_burden_plot(results_100)
