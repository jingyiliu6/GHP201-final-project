#health benefit calculation and disaggregation by urban/rural sector 
#cases of DR detected and cases of blindness averted due to intervention

input <- read.csv("input table new.csv")
input$Value <- as.numeric(gsub(",", "", input$Value)) 
get_val <- function(name) input$Value[input$Parameters == name]

sectors <- c("u", "r")   # u = urban, r = rural
sector_labels <- c("Urban", "Rural")

#section 1: Additional DR cases detected due to intervention  

# 1.1 DR cases detected before intervention by sector
dr_baseline_ur <- sapply(sectors, function(s) {
  get_val(paste0("pop_", s)) *
    get_val(paste0("dr_", s)) *
    get_val(paste0("pcu_", s)) *
    get_val("cov_b") *
    get_val("sens_b")
})

# 1.2 DR cases detected after intervention by sector
dr_inv_ur <- sapply(sectors, function(s) {
  get_val(paste0("pop_", s)) *
    get_val(paste0("dr_", s)) *
    get_val(paste0("pcu_", s)) *
    get_val("cov_inv") *
    get_val("sens_inv")
})

# 1.3 Additional DR cases detected
dr_detected_ur <- dr_inv_ur - dr_baseline_ur

library(ggplot2)
plot_dr_ur <- data.frame(
  sector = factor(sector_labels, levels = sector_labels),
  dr_detected = dr_detected_ur
)

ggplot(plot_dr_ur, aes(x = sector, y = dr_detected)) +
  geom_col(fill = "darkturquoise", width = 0.6) +
  labs(title = "Additional DR Cases Detected Due to Intervention",
       x = "Place of Residence",
       y = "Additional DR Cases Detected") +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))


# ─── Section 2: Blindness cases averted due to intervention ────────────────────

# 2.1 Blindness cases due to DR before intervention by sector
blind_baseline_ur <- sapply(sectors, function(s) {
  get_val(paste0("pop_", s)) *
    get_val(paste0("dr_", s)) *
    get_val(paste0("pcu_", s)) *
    get_val("cov_b") *
    get_val("sens_b") *
    (1 - get_val("lfu_referral")) *
    (1 - get_val("lfu_treat")) *
    get_val(paste0("vtdr_dr_", s)) *
    get_val("eff_blind")
})

# 2.2 Blindness cases due to DR after intervention by sector (with VTDR reduction)
blind_inv_ur <- sapply(sectors, function(s) {
  get_val(paste0("pop_", s)) *
    get_val(paste0("dr_", s)) *
    get_val(paste0("pcu_", s)) *
    get_val("cov_inv") *
    get_val("sens_inv") *
    (1 - get_val("lfu_referral")) *
    (1 - get_val("lfu_treat")) *
    get_val(paste0("vtdr_dr_", s)) *
    (1 - get_val("vtdr_reduction")) *
    get_val("eff_blind")
})

# 2.3 Blindness cases averted
blind_averted_ur <- blind_inv_ur - blind_baseline_ur

plot_blind_ur <- data.frame(
  sector = factor(sector_labels, levels = sector_labels),
  blind_averted = blind_averted_ur
)

ggplot(plot_blind_ur, aes(x = sector, y = blind_averted)) +
  geom_col(fill = "darkturquoise", width = 0.6) +
  labs(title = "Blindness Cases Averted Due to Intervention",
       x = "Place of Residence",
       y = "Blindness Cases Averted") +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

#dashboard

health_benefit_dash_ur <- as.data.frame(rbind(
  round(dr_detected_ur, 1),
  round(blind_averted_ur, 3)))
  colnames(health_benefit_dash_ur) <- c("Urban", "Rural")
  rownames(health_benefit_dash_ur) <- c(
    "Additional DR Cases Detected",
    "Blindness Cases Averted")

print(health_benefit_dash_ur)