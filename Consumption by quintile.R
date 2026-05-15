# Monthly household consumption by combined rural-urban household-weighted quintile

library(ggplot2)

plain_number <- function(x) format(round(x, 0), scientific = FALSE, trim = TRUE, big.mark = ",")

quintile_labels <- c("I", "II", "III", "IV", "V")

plot_consumption_q <- data.frame(
  quintile = factor(rep(quintile_labels, times = 2), levels = quintile_labels),
  sector = factor(
    rep(c("Rural", "Urban"), each = length(quintile_labels)),
    levels = c("Urban", "Rural")
  ),
  monthly_consumption = c(
    4886.782, 8282.951, 10986.73, 14497.38, 21750.32,
    4982.803, 8416.013, 11120.83, 14863.98, 25502.76
  )
)
plot_consumption_q$label <- plain_number(plot_consumption_q$monthly_consumption)

ggplot(plot_consumption_q,
       aes(x = quintile, y = monthly_consumption, fill = sector)) +
  geom_col(position = position_dodge(width = 0.75),
           width = 0.65,
           color = "black") +
  geom_text(aes(label = label),
            position = position_dodge(width = 0.75),
            vjust = -0.35,
            size = 3) +
  scale_fill_manual(values = c("Urban" = "#AD002A99",
                               "Rural" = "#FDAF9199")) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.18))) +
  labs(title = "Monthly Household Consumption by Quintile",
       x = "Consumption Quintile (Poorest to Richest)",
       y = "Monthly Household Consumption",
       fill = NULL) +
  theme_classic() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    legend.position = "top",
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank()
  )
# Monthly household consumption by combined rural-urban household-weighted quintile

library(ggplot2)

plain_number <- function(x) format(round(x, 0), scientific = FALSE, trim = TRUE, big.mark = ",")

quintile_labels <- c("I", "II", "III", "IV", "V")

plot_consumption_q <- data.frame(
  quintile = factor(rep(quintile_labels, times = 2), levels = quintile_labels),
  sector = factor(
    rep(c("Rural", "Urban"), each = length(quintile_labels)),
    levels = c("Urban", "Rural")
  ),
  monthly_consumption = c(
    4886.782, 8282.951, 10986.73, 14497.38, 21750.32,
    4982.803, 8416.013, 11120.83, 14863.98, 25502.76
  )
)
plot_consumption_q$label <- plain_number(plot_consumption_q$monthly_consumption)

ggplot(plot_consumption_q,
       aes(x = quintile, y = monthly_consumption, fill = sector)) +
  geom_col(position = position_dodge(width = 0.75),
           width = 0.65,
           color = "black") +
  geom_text(aes(label = label),
            position = position_dodge(width = 0.75),
            vjust = -0.35,
            size = 3) +
  scale_fill_manual(values = c("Urban" = "#AD002A99",
                               "Rural" = "#FDAF9199")) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.18))) +
  labs(title = "Monthly Household Consumption by Quintile",
       x = "Consumption Quintile (Poorest to Richest)",
       y = "Monthly Household Consumption",
       fill = NULL) +
  theme_classic() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    legend.position = "top",
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank()
  )
# Monthly household consumption by combined rural-urban household-weighted quintile

library(ggplot2)

plain_number <- function(x) format(round(x, 0), scientific = FALSE, trim = TRUE, big.mark = ",")

quintile_labels <- c("I", "II", "III", "IV", "V")

plot_consumption_q <- data.frame(
  quintile = factor(rep(quintile_labels, times = 2), levels = quintile_labels),
  sector = factor(
    rep(c("Rural", "Urban"), each = length(quintile_labels)),
    levels = c("Urban", "Rural")
  ),
  monthly_consumption = c(
    4886.782, 8282.951, 10986.73, 14497.38, 21750.32,
    4982.803, 8416.013, 11120.83, 14863.98, 25502.76
  )
)
plot_consumption_q$label <- plain_number(plot_consumption_q$monthly_consumption)

ggplot(plot_consumption_q,
       aes(x = quintile, y = monthly_consumption, fill = sector)) +
  geom_col(position = position_dodge(width = 0.75),
           width = 0.65,
           color = "black") +
  geom_text(aes(label = label),
            position = position_dodge(width = 0.75),
            vjust = -0.35,
            size = 3) +
  scale_fill_manual(values = c("Urban" = "#AD002A99",
                               "Rural" = "#FDAF9199")) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.18))) +
  labs(title = "Monthly Household Consumption by Quintile",
       x = "Consumption Quintile (Poorest to Richest)",
       y = "Monthly Household Consumption",
       fill = NULL) +
  theme_classic() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    legend.position = "top",
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank()
  )
# Monthly household consumption by combined rural-urban household-weighted quintile

library(ggplot2)

plain_number <- function(x) format(round(x, 0), scientific = FALSE, trim = TRUE, big.mark = ",")

quintile_labels <- c("I", "II", "III", "IV", "V")

plot_consumption_q <- data.frame(
  quintile = factor(rep(quintile_labels, times = 2), levels = quintile_labels),
  sector = factor(
    rep(c("Rural", "Urban"), each = length(quintile_labels)),
    levels = c("Urban", "Rural")
  ),
  monthly_consumption = c(
    4886.782, 8282.951, 10986.73, 14497.38, 21750.32,
    4982.803, 8416.013, 11120.83, 14863.98, 25502.76
  )
)
plot_consumption_q$label <- plain_number(plot_consumption_q$monthly_consumption)

ggplot(plot_consumption_q,
       aes(x = quintile, y = monthly_consumption, fill = sector)) +
  geom_col(position = position_dodge(width = 0.75),
           width = 0.65,
           color = "black") +
  geom_text(aes(label = label),
            position = position_dodge(width = 0.75),
            vjust = -0.35,
            size = 3) +
  scale_fill_manual(values = c("Urban" = "#AD002A99",
                               "Rural" = "#FDAF9199")) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.18))) +
  labs(title = "Monthly Household Consumption by Quintile",
       x = "Consumption Quintile (Poorest to Richest)",
       y = "Monthly Household Consumption",
       fill = NULL) +
  theme_classic() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    legend.position = "top",
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank()
  )
# Monthly household consumption by combined rural-urban household-weighted quintile

library(ggplot2)

plain_number <- function(x) format(round(x, 0), scientific = FALSE, trim = TRUE, big.mark = ",")

quintile_labels <- c("I", "II", "III", "IV", "V")

plot_consumption_q <- data.frame(
  quintile = factor(rep(quintile_labels, times = 2), levels = quintile_labels),
  sector = factor(
    rep(c("Rural", "Urban"), each = length(quintile_labels)),
    levels = c("Urban", "Rural")
  ),
  monthly_consumption = c(
    4886.782, 8282.951, 10986.73, 14497.38, 21750.32,
    4982.803, 8416.013, 11120.83, 14863.98, 25502.76
  )
)
plot_consumption_q$label <- plain_number(plot_consumption_q$monthly_consumption)

ggplot(plot_consumption_q,
       aes(x = quintile, y = monthly_consumption, fill = sector)) +
  geom_col(position = position_dodge(width = 0.75),
           width = 0.65,
           color = "black") +
  geom_text(aes(label = label),
            position = position_dodge(width = 0.75),
            vjust = -0.35,
            size = 3) +
  scale_fill_manual(values = c("Urban" = "#AD002A99",
                               "Rural" = "#FDAF9199")) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.18))) +
  labs(title = "Monthly Household Consumption by Quintile",
       x = "Consumption Quintile (Poorest to Richest)",
       y = "Monthly Household Consumption",
       fill = NULL) +
  theme_classic() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    legend.position = "top",
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank()
  )
# Monthly household consumption by combined rural-urban household-weighted quintile

library(ggplot2)

plain_number <- function(x) format(round(x, 0), scientific = FALSE, trim = TRUE, big.mark = ",")

quintile_labels <- c("I", "II", "III", "IV", "V")

plot_consumption_q <- data.frame(
  quintile = factor(rep(quintile_labels, times = 2), levels = quintile_labels),
  sector = factor(
    rep(c("Rural", "Urban"), each = length(quintile_labels)),
    levels = c("Urban", "Rural")
  ),
  monthly_consumption = c(
    4886.782, 8282.951, 10986.73, 14497.38, 21750.32,
    4982.803, 8416.013, 11120.83, 14863.98, 25502.76
  )
)
plot_consumption_q$label <- plain_number(plot_consumption_q$monthly_consumption)

ggplot(plot_consumption_q,
       aes(x = quintile, y = monthly_consumption, fill = sector)) +
  geom_col(position = position_dodge(width = 0.75),
           width = 0.65,
           color = "black") +
  geom_text(aes(label = label),
            position = position_dodge(width = 0.75),
            vjust = -0.35,
            size = 3) +
  scale_fill_manual(values = c("Urban" = "#AD002A99",
                               "Rural" = "#FDAF9199")) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.18))) +
  labs(title = "Monthly Household Consumption by Quintile",
       x = "Consumption Quintile (Poorest to Richest)",
       y = "Monthly Household Consumption",
       fill = NULL) +
  theme_classic() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    legend.position = "top",
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank()
  )
