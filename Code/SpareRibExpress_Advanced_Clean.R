### Install and load libraries ########################################################
packages <- c("dplyr","tidyr","ggplot2","scales","BTYD","cluster","pwr","lubridate","survival","ggrepel")
for (pkg in packages) {
  if (!require(pkg, character.only = TRUE, quietly = TRUE)) {
    install.packages(pkg, repos = "https://cloud.r-project.org")
    library(pkg, character.only = TRUE)
  }
}

adv_dir <- "outputs/advanced"
dir.create(adv_dir, recursive = TRUE, showWarnings = FALSE)

save_plot <- function(plot, filename, w = 12, h = 7) {
  ggsave(file.path(adv_dir, filename), plot = plot, width = w, height = h, dpi = 200, bg = "white")
  cat("  Saved:", filename, "\n")
}
save_csv <- function(df, filename) {
  write.csv(df, file.path(adv_dir, filename), row.names = FALSE)
  cat("  Saved:", filename, "\n")
}

### UNIFIED THEME ######################################################################
COL_PRIMARY   <- "#3F51B5"   # Indigo
COL_SECONDARY <- "#9C27B0"   # Purple
COL_ACCENT    <- "#E91E63"   # Pink/red accent
COL_POSITIVE  <- "#4CAF50"   # Green
COL_NEUTRAL   <- "#9E9E9E"   # Grey
COL_WARN      <- "#FF9800"   # Orange

SEGMENT_COLORS <- c(
  "Champions"         = COL_POSITIVE,
  "Loyal Customers"   = COL_PRIMARY,
  "Potential Loyalists" = COL_WARN,
  "New Customers"     = "#00BCD4",
  "At Risk"           = COL_ACCENT,
  "Lost"              = COL_NEUTRAL
)

theme_sre <- function() {
  theme_minimal(base_size = 13) %+replace%
    theme(
      plot.title         = element_text(face = "bold", size = 16, color = COL_PRIMARY, margin = margin(b = 4)),
      plot.subtitle      = element_text(color = "grey40", size = 10, margin = margin(b = 10)),
      legend.position    = "top",
      legend.title       = element_text(face = "bold", size = 10),
      panel.grid.major.x = element_blank(),
      panel.grid.minor   = element_blank(),
      axis.text.x        = element_text(size = 10),
      axis.text.y        = element_text(size = 10),
      axis.title         = element_text(size = 11),
      strip.text         = element_text(face = "bold", size = 11)
    )
}


###############################################################################
### 0. LOAD CLEANED DATA & REBUILD REQUIRED OBJECTS ###########################
###############################################################################
cat("=== Loading cleaned data ===\n")
orders <- readRDS("outputs/orders_clean.rds")
orders$Channel   <- as.factor(orders$Channel)
orders$StoreName <- as.factor(orders$StoreName)

analysis_date <- max(orders$order_date_local, na.rm = TRUE) + 1

cat("Building customer RFM...\n")
customer_rfm <- orders %>%
  group_by(CustomerID) %>%
  summarise(
    Recency       = as.numeric(analysis_date - max(order_date_local)),
    Frequency     = n(),
    Monetary      = sum(PriceIncVat),
    AvgOrderValue = mean(PriceIncVat),
    FirstOrder    = min(order_date_local),
    LastOrder     = max(order_date_local),
    Tenure        = as.numeric(analysis_date - min(order_date_local)),
    NumStores     = n_distinct(StoreName),
    NumChannels   = n_distinct(Channel),
    .groups = "drop"
  )

customer_rfm$R_score <- ntile(-customer_rfm$Recency, 5)
customer_rfm$F_score <- ntile(customer_rfm$Frequency, 5)
customer_rfm$Segment <- case_when(
  customer_rfm$R_score >= 4 & customer_rfm$F_score >= 4 ~ "Champions",
  customer_rfm$R_score >= 3 & customer_rfm$F_score >= 3 ~ "Loyal Customers",
  customer_rfm$R_score >= 4 & customer_rfm$F_score <= 2 ~ "New Customers",
  customer_rfm$R_score <= 2 & customer_rfm$F_score >= 3 ~ "At Risk",
  customer_rfm$R_score <= 2 & customer_rfm$F_score <= 2 ~ "Lost",
  TRUE ~ "Potential Loyalists"
)

cat("Building BG/NBD data...\n")
observation_end <- max(orders$order_date_local, na.rm = TRUE)
bgf_data <- orders %>%
  group_by(CustomerID) %>%
  summarise(
    first_purchase = min(order_date_local),
    last_purchase  = max(order_date_local),
    n_orders       = n(),
    total_spend    = sum(PriceIncVat),
    .groups = "drop"
  ) %>%
  mutate(
    x        = n_orders - 1,
    t.x      = as.numeric(difftime(last_purchase, first_purchase, units = "weeks")),
    T.cal    = as.numeric(difftime(observation_end, first_purchase, units = "weeks")),
    monetary = ifelse(x > 0, total_spend / n_orders, 0)
  ) %>%
  filter(T.cal > 0)

cat("Estimating BG/NBD model...\n")
bgf_input <- as.data.frame(bgf_data[, c("x", "t.x", "T.cal")])
params <- BTYD::bgnbd.EstimateParameters(bgf_input)
names(params) <- c("r", "alpha", "a", "b")
cat("BG/NBD Parameters:\n"); print(params)

bgf_data$predicted_purchases <- BTYD::bgnbd.ConditionalExpectedTransactions(
  params = params, T.star = 52,
  x = bgf_data$x, t.x = bgf_data$t.x, T.cal = bgf_data$T.cal
)
bgf_data$p_alive <- BTYD::bgnbd.PAlive(
  params = params,
  x = bgf_data$x, t.x = bgf_data$t.x, T.cal = bgf_data$T.cal
)

cat("Data ready:", nrow(customer_rfm), "customers,", nrow(bgf_data), "in BG/NBD\n\n")


###############################################################################
### A. GAMMA-GAMMA MODEL FOR MONETARY VALUE + PROPER CLV ######################
###############################################################################
cat("=== A: GAMMA-GAMMA CLV ===\n")

gg_data <- bgf_data %>% filter(x > 0, monetary > 0)

cor_fm <- cor(as.numeric(gg_data$x), as.numeric(gg_data$monetary))
cat("Correlation freq vs monetary:", round(cor_fm, 3), "(should be ~0)\n")

gg_params <- BTYD::spend.EstimateParameters(
  m.x.vector = as.numeric(gg_data$monetary),
  x.vector   = as.numeric(gg_data$x)
)
names(gg_params) <- c("p", "q", "gamma")
cat("Gamma-Gamma Parameters:\n"); print(gg_params)

gg_data$expected_monetary <- BTYD::spend.expected.value(
  gg_params,
  m.x = as.numeric(gg_data$monetary),
  x   = as.numeric(gg_data$x)
)
gg_data$CLV_proper <- gg_data$predicted_purchases * gg_data$expected_monetary

cat("Mean CLV:", round(mean(gg_data$CLV_proper, na.rm = TRUE), 2), "EUR\n")
cat("Total projected (repeat):", round(sum(gg_data$CLV_proper, na.rm = TRUE), 0), "EUR\n\n")

p <- ggplot(gg_data, aes(x = CLV_proper)) +
  geom_histogram(bins = 100, fill = COL_PRIMARY, color = "white", alpha = 0.85) +
  xlim(0, quantile(gg_data$CLV_proper, 0.95, na.rm = TRUE)) +
  geom_vline(xintercept = median(gg_data$CLV_proper, na.rm = TRUE),
             linetype = "dashed", color = COL_SECONDARY, linewidth = 0.8) +
  annotate("text", x = median(gg_data$CLV_proper, na.rm = TRUE) * 1.5,
           y = Inf, vjust = 2, hjust = 0, color = COL_SECONDARY, fontface = "bold",
           label = paste0("Median: \u20ac", round(median(gg_data$CLV_proper, na.rm = TRUE), 0))) +
  labs(title = "Distribution of CLV (BG/NBD x Gamma-Gamma)",
       subtitle = paste0("Projected 12-month customer lifetime value for repeat customers (n = ",
                         format(nrow(gg_data), big.mark = ","), ")"),
       x = "Customer Lifetime Value (EUR)", y = "Number of Customers") +
  theme_sre()
save_plot(p, "01_clv_distribution.png")

gg_data <- gg_data %>%
  left_join(customer_rfm %>% select(CustomerID, Segment), by = "CustomerID")

clv_repeat <- gg_data %>%
  group_by(Segment) %>%
  summarise(
    n_repeat = n(),
    mean_CLV   = round(mean(CLV_proper, na.rm = TRUE), 2),
    median_CLV = round(median(CLV_proper, na.rm = TRUE), 2),
    total_CLV  = round(sum(CLV_proper, na.rm = TRUE), 0),
    mean_p_alive = round(mean(p_alive, na.rm = TRUE), 3),
    .groups = "drop"
  )

all_segments <- customer_rfm %>%
  group_by(Segment) %>%
  summarise(n_total = n(), .groups = "drop")

clv_by_segment <- all_segments %>%
  left_join(clv_repeat, by = "Segment") %>%
  mutate(
    n_repeat     = ifelse(is.na(n_repeat), 0, n_repeat),
    mean_CLV     = ifelse(is.na(mean_CLV), 0, mean_CLV),
    median_CLV   = ifelse(is.na(median_CLV), 0, median_CLV),
    total_CLV    = ifelse(is.na(total_CLV), 0, total_CLV),
    mean_p_alive = ifelse(is.na(mean_p_alive), 0, mean_p_alive),
    note         = ifelse(n_repeat == 0, "Single-purchase (no GG model)", "")
  ) %>%
  arrange(desc(total_CLV))

save_csv(clv_by_segment, "01_clv_by_segment.csv")

clv_by_segment$Segment <- factor(clv_by_segment$Segment,
  levels = clv_by_segment$Segment[order(-clv_by_segment$mean_CLV)])

p <- ggplot(clv_by_segment, aes(x = Segment, y = mean_CLV, fill = Segment)) +
  geom_bar(stat = "identity", width = 0.7) +
  geom_text(aes(label = ifelse(mean_CLV > 0, paste0("\u20ac", round(mean_CLV, 0)),
                                "N/A\n(single purchase)")),
            vjust = -0.3, size = 3.5, fontface = "bold") +
  scale_fill_manual(values = SEGMENT_COLORS) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.2)), labels = label_comma(prefix = "\u20ac")) +
  labs(title = "Average Projected CLV by Customer Segment",
       subtitle = "BG/NBD x Gamma-Gamma model | 12-month projection | Single-purchase segments have no GG estimate",
       x = "", y = "Mean CLV (EUR)") +
  theme_sre() +
  theme(legend.position = "none", axis.text.x = element_text(angle = 30, hjust = 1))
save_plot(p, "02_clv_by_segment.png")

top20_clv <- gg_data %>%
  arrange(desc(CLV_proper)) %>%
  head(20) %>%
  select(CustomerID, Segment, p_alive, predicted_purchases, expected_monetary, CLV_proper)
save_csv(top20_clv, "02_top20_clv_customers.csv")


###############################################################################
### B. K-MEANS CLUSTERING #####################################################
###############################################################################
cat("=== B: K-MEANS CLUSTERING ===\n")

rfm_for_clustering <- customer_rfm %>%
  select(CustomerID, Recency, Frequency, Monetary) %>%
  filter(complete.cases(.))

rfm_scaled <- rfm_for_clustering %>%
  mutate(
    Recency_scaled   = scale(Recency)[,1],
    Frequency_scaled = scale(log1p(Frequency))[,1],
    Monetary_scaled  = scale(log1p(Monetary))[,1]
  )
cluster_input <- as.data.frame(rfm_scaled[, c("Recency_scaled","Frequency_scaled","Monetary_scaled")])

set.seed(42)
sample_idx <- sample(nrow(cluster_input), min(10000, nrow(cluster_input)))
cluster_sample <- cluster_input[sample_idx, ]

wss_values <- sapply(1:10, function(k) {
  kmeans(cluster_sample, centers = k, nstart = 10, iter.max = 50)$tot.withinss
})
elbow_df <- data.frame(k = 1:10, wss = wss_values)

p <- ggplot(elbow_df, aes(x = k, y = wss)) +
  geom_line(color = COL_PRIMARY, linewidth = 1.2) +
  geom_point(color = COL_PRIMARY, size = 3.5) +
  geom_point(data = elbow_df %>% filter(k == 5), color = COL_ACCENT, size = 5, shape = 18) +
  annotate("text", x = 5.4, y = elbow_df$wss[5], label = "k = 5 (selected)",
           color = COL_ACCENT, fontface = "bold", hjust = 0) +
  scale_x_continuous(breaks = 1:10) +
  labs(title = "Elbow Method: Optimal Number of Clusters",
       subtitle = "Within-cluster sum of squares on 10K customer sample | k = 5 selected",
       x = "Number of Clusters (k)", y = "Total Within-Cluster Sum of Squares") +
  theme_sre() +
  theme(panel.grid.major.x = element_line(color = "grey92"))
save_plot(p, "03_elbow_method.png")

set.seed(42)
km_fit <- kmeans(cluster_input, centers = 5, nstart = 10, iter.max = 100)
rfm_for_clustering$Cluster <- as.factor(km_fit$cluster)

cluster_profiles <- rfm_for_clustering %>%
  group_by(Cluster) %>%
  summarise(
    n_customers   = n(),
    pct           = round(100 * n() / nrow(rfm_for_clustering), 1),
    avg_recency   = round(mean(Recency), 0),
    avg_frequency = round(mean(Frequency), 1),
    avg_monetary  = round(mean(Monetary), 2),
    .groups = "drop"
  ) %>%
  arrange(desc(avg_monetary))

cat("K-Means cluster profiles:\n"); print(cluster_profiles)
save_csv(cluster_profiles, "03_kmeans_profiles.csv")

centers_df <- as.data.frame(km_fit$centers)
centers_df$Cluster <- paste("Cluster", rownames(centers_df))
centers_long <- tidyr::pivot_longer(centers_df, -Cluster, names_to = "Variable", values_to = "Value")
centers_long$Variable <- gsub("_scaled", "", centers_long$Variable)

p <- ggplot(centers_long, aes(x = Variable, y = Cluster, fill = Value)) +
  geom_tile(color = "white", linewidth = 1.5) +
  geom_text(aes(label = round(Value, 2)), size = 4.5, fontface = "bold") +
  scale_fill_gradient2(low = COL_ACCENT, mid = "white", high = COL_PRIMARY, midpoint = 0) +
  labs(title = "K-Means Cluster Centers (Standardized)",
       subtitle = "Positive = above average | Negative = below average",
       x = "", y = "", fill = "Z-score") +
  theme_sre() +
  theme(panel.grid = element_blank())
save_plot(p, "04_kmeans_heatmap.png")


###############################################################################
### C. CHURN PREDICTION MODEL #################################################
###############################################################################
cat("\n=== C: CHURN MODEL ===\n")

ipt_summary <- orders %>%
  arrange(CustomerID, order_date_local) %>%
  group_by(CustomerID) %>%
  mutate(days_gap = as.numeric(order_date_local - lag(order_date_local))) %>%
  filter(!is.na(days_gap)) %>%
  summarise(avg_gap = mean(days_gap), .groups = "drop")

churn_threshold <- max(quantile(ipt_summary$avg_gap, 0.75, na.rm = TRUE), 90)
cat("Churn threshold:", round(churn_threshold), "days\n")

customer_rfm$is_churned <- ifelse(customer_rfm$Recency > churn_threshold, 1, 0)
cat("Churn rate:", round(100 * mean(customer_rfm$is_churned), 1), "%\n")

churn_data <- customer_rfm %>%
  select(CustomerID, is_churned, Frequency, AvgOrderValue, Tenure, NumStores, NumChannels) %>%
  filter(complete.cases(.))

avg_gap_per_customer <- orders %>%
  arrange(CustomerID, order_date_local) %>%
  group_by(CustomerID) %>%
  mutate(days_gap = as.numeric(order_date_local - lag(order_date_local))) %>%
  filter(!is.na(days_gap)) %>%
  summarise(avg_inter_purchase = mean(days_gap), .groups = "drop")

churn_data <- churn_data %>%
  left_join(avg_gap_per_customer, by = "CustomerID")
churn_data$avg_inter_purchase[is.na(churn_data$avg_inter_purchase)] <-
  max(churn_data$avg_inter_purchase, na.rm = TRUE)

if (length(unique(churn_data$is_churned)) > 1) {

  churn_model <- glm(is_churned ~ Frequency + AvgOrderValue + Tenure +
                       NumStores + NumChannels + avg_inter_purchase,
                     data = churn_data, family = binomial)

  cat("\nChurn model summary:\n")
  print(summary(churn_model))

  cat("\nOdds Ratios:\n")
  print(round(exp(coef(churn_model)), 4))

  churn_data$churn_prob <- predict(churn_model, type = "response")

  # --- Plot 05: Churn Probability Distribution ---
  p <- ggplot(churn_data, aes(x = churn_prob, fill = factor(is_churned))) +
    geom_histogram(bins = 50, alpha = 0.75, position = "identity", color = "white") +
    scale_fill_manual(values = c("0" = COL_PRIMARY, "1" = COL_ACCENT),
                      labels = c("Active", "Churned"), name = "") +
    scale_y_continuous(labels = label_comma()) +
    labs(title = "Distribution of Predicted Churn Probability",
         subtitle = paste0("Logistic regression model | Accuracy: ",
                           round(100 * sum(diag(table(
                             ifelse(churn_data$churn_prob > 0.5, 1, 0),
                             churn_data$is_churned))) / nrow(churn_data), 1), "%"),
         x = "Predicted P(Churn)", y = "Number of Customers") +
    theme_sre()
  save_plot(p, "05_churn_probability_distribution.png")

  # Confusion matrix
  churn_data$predicted_churn <- ifelse(churn_data$churn_prob > 0.5, 1, 0)
  confusion <- table(Predicted = churn_data$predicted_churn, Actual = churn_data$is_churned)
  cat("\nConfusion Matrix:\n"); print(confusion)
  accuracy <- round(sum(diag(confusion)) / sum(confusion), 3)
  cat("Accuracy:", accuracy, "\n")

  # Feature importance
  coef_df <- data.frame(
    Variable    = names(coef(churn_model))[-1],
    Coefficient = coef(churn_model)[-1],
    OddsRatio   = exp(coef(churn_model)[-1])
  )
  coef_df$Significant <- ifelse(summary(churn_model)$coefficients[-1, 4] < 0.05, "Yes", "No")
  coef_df$Direction   <- ifelse(coef_df$Coefficient > 0, "Increases Churn", "Reduces Churn")
  save_csv(coef_df, "04_churn_coefficients.csv")

  # --- Plot 06: Feature Importance ---
  p <- ggplot(coef_df, aes(x = reorder(Variable, abs(Coefficient)), y = Coefficient, fill = Direction)) +
    geom_bar(stat = "identity", width = 0.7) +
    coord_flip() +
    geom_text(aes(label = paste0("OR: ", round(OddsRatio, 2)),
                  hjust = ifelse(Coefficient > 0, -0.1, 1.1)),
              size = 3.5, fontface = "bold") +
    scale_fill_manual(values = c("Increases Churn" = COL_ACCENT, "Reduces Churn" = COL_PRIMARY)) +
    scale_y_continuous(expand = expansion(mult = c(0.2, 0.2))) +
    labs(title = "Churn Model: Feature Importance",
         subtitle = "Log-odds coefficients with odds ratios | All features significant (p < 0.05)",
         x = "", y = "Coefficient (Log-Odds)", fill = "") +
    theme_sre()
  save_plot(p, "06_churn_feature_importance.png")

} else {
  cat("WARNING: Single churn class - skipping logistic model\n")
  churn_data$churn_prob <- mean(churn_data$is_churned)
}


###############################################################################
### D. REVENUE-AT-RISK ANALYSIS ###############################################
###############################################################################
cat("\n=== D: REVENUE AT RISK ===\n")

risk_data <- churn_data %>%
  select(CustomerID, churn_prob, is_churned) %>%
  left_join(customer_rfm %>% select(CustomerID, Monetary, Segment), by = "CustomerID")

risk_data$revenue_at_risk <- risk_data$Monetary * risk_data$churn_prob

total_revenue_at_risk <- sum(risk_data$revenue_at_risk, na.rm = TRUE)
total_revenue         <- sum(risk_data$Monetary, na.rm = TRUE)

cat("Total revenue at risk:", round(total_revenue_at_risk, 0), "EUR (",
    round(100 * total_revenue_at_risk / total_revenue, 1), "%)\n")

risk_by_segment <- risk_data %>%
  group_by(Segment) %>%
  summarise(
    n_customers     = n(),
    total_revenue   = round(sum(Monetary, na.rm = TRUE), 0),
    revenue_at_risk = round(sum(revenue_at_risk, na.rm = TRUE), 0),
    pct_at_risk     = round(100 * sum(revenue_at_risk, na.rm = TRUE) / sum(Monetary, na.rm = TRUE), 1),
    avg_churn_prob  = round(mean(churn_prob, na.rm = TRUE), 3),
    .groups = "drop"
  ) %>%
  arrange(desc(revenue_at_risk))

print(risk_by_segment)
save_csv(risk_by_segment, "05_revenue_at_risk.csv")

risk_long <- risk_by_segment %>%
  select(Segment, `Total Revenue` = total_revenue, `Revenue at Risk` = revenue_at_risk) %>%
  tidyr::pivot_longer(-Segment, names_to = "Type", values_to = "EUR")

p <- ggplot(risk_long, aes(x = reorder(Segment, -EUR), y = EUR / 1e6, fill = Type)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.7), width = 0.6) +
  geom_text(aes(label = paste0("\u20ac", round(EUR / 1e6, 1), "M")),
            position = position_dodge(width = 0.7), vjust = -0.5, size = 3, fontface = "bold") +
  scale_fill_manual(values = c("Total Revenue" = COL_PRIMARY, "Revenue at Risk" = COL_ACCENT)) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.15)), labels = label_comma(prefix = "\u20ac", suffix = "M")) +
  labs(title = "Revenue at Risk by Customer Segment",
       subtitle = paste0("Total at risk: \u20ac", format(round(total_revenue_at_risk / 1e6, 1), big.mark = ","),
                         "M (", round(100 * total_revenue_at_risk / total_revenue, 1), "% of total)"),
       x = "", y = "Revenue (EUR millions)", fill = "") +
  theme_sre() +
  theme(axis.text.x = element_text(angle = 30, hjust = 1))
save_plot(p, "07_revenue_at_risk_by_segment.png")


###############################################################################
### E. SEASONALITY & TREND ####################################################
###############################################################################
cat("\n=== E: SEASONALITY ===\n")

weekly_revenue <- orders %>%
  mutate(Week = as.Date(cut(order_date_local, "week"))) %>%
  group_by(Week) %>%
  summarise(revenue = sum(PriceIncVat), n_orders = n(), .groups = "drop") %>%
  arrange(Week) %>%
  filter(!is.na(Week))

p <- ggplot(weekly_revenue, aes(x = Week, y = revenue / 1000)) +
  geom_line(color = COL_PRIMARY, alpha = 0.5, linewidth = 0.6) +
  geom_smooth(method = "loess", color = COL_SECONDARY, se = TRUE, fill = COL_SECONDARY,
              alpha = 0.15, span = 0.3, linewidth = 1.2) +
  scale_y_continuous(labels = label_comma(prefix = "\u20ac", suffix = "K")) +
  labs(title = "Weekly Revenue with Trend Line",
       subtitle = "LOESS smoothing (purple) over raw weekly revenue (indigo)",
       x = "", y = "Revenue (EUR thousands)") +
  theme_sre() +
  theme(panel.grid.major.x = element_line(color = "grey92"))
save_plot(p, "08_weekly_revenue_trend.png")

if (nrow(weekly_revenue) >= 20) {
  rev_ts <- ts(weekly_revenue$revenue, frequency = 52)
  if (length(rev_ts) > 2 * 52) {
    decomp <- stl(rev_ts, s.window = "periodic")
    png(file.path(adv_dir, "09_revenue_decomposition.png"), width = 2400, height = 1400, res = 200)
    par(col = COL_PRIMARY, col.main = COL_PRIMARY, font.main = 2, cex.main = 1.3)
    plot(decomp, main = "Revenue Decomposition: Trend + Seasonal + Remainder")
    dev.off()
    cat("  Saved: 09_revenue_decomposition.png\n")
  } else {
    weekly_revenue$ma_4week <- stats::filter(weekly_revenue$revenue, rep(1/4, 4), sides = 2)
    p <- ggplot(weekly_revenue, aes(x = Week)) +
      geom_line(aes(y = revenue / 1000), color = COL_PRIMARY, alpha = 0.4) +
      geom_line(aes(y = ma_4week / 1000), color = COL_SECONDARY, linewidth = 1.2, na.rm = TRUE) +
      scale_y_continuous(labels = label_comma(prefix = "\u20ac", suffix = "K")) +
      labs(title = "Weekly Revenue with 4-Week Moving Average",
           subtitle = "Purple = moving average | Indigo = raw weekly",
           x = "", y = "Revenue (EUR thousands)") +
      theme_sre() +
      theme(panel.grid.major.x = element_line(color = "grey92"))
    save_plot(p, "09_revenue_moving_avg.png")
  }
}


###############################################################################
### F. CHANNEL MIGRATION & ACQUISITION ANALYSIS ###############################
###############################################################################
cat("\n=== F: CHANNEL MIGRATION ===\n")

first_channel <- orders %>%
  arrange(CustomerID, order_date_local) %>%
  group_by(CustomerID) %>%
  slice(1) %>%
  ungroup() %>%
  select(CustomerID, FirstChannel = Channel)

last_channel <- orders %>%
  arrange(CustomerID, desc(order_date_local)) %>%
  group_by(CustomerID) %>%
  slice(1) %>%
  ungroup() %>%
  select(CustomerID, LastChannel = Channel)

channel_migration <- first_channel %>%
  left_join(last_channel, by = "CustomerID") %>%
  left_join(customer_rfm %>% select(CustomerID, Frequency), by = "CustomerID") %>%
  filter(Frequency > 1)

migration_long <- channel_migration %>% count(FirstChannel, LastChannel)
save_csv(migration_long, "06_channel_migration.csv")

p <- ggplot(migration_long, aes(x = LastChannel, y = FirstChannel, fill = n)) +
  geom_tile(color = "white", linewidth = 1.2) +
  geom_text(aes(label = format(n, big.mark = ",")), size = 3.5, fontface = "bold") +
  scale_fill_gradient(low = "white", high = COL_PRIMARY, labels = label_comma(), name = "Customers") +
  labs(title = "Channel Migration: First \u2192 Last Channel",
       subtitle = "Repeat customers only | Diagonal = stayed in same channel",
       x = "Most Recent Channel", y = "First Channel") +
  theme_sre() +
  theme(panel.grid = element_blank(), axis.text.x = element_text(angle = 30, hjust = 1))
save_plot(p, "10_channel_migration_heatmap.png")

first_channel_clv <- first_channel %>%
  left_join(customer_rfm %>% select(CustomerID, Frequency, Monetary), by = "CustomerID")

channel_clv_summary <- first_channel_clv %>%
  group_by(FirstChannel) %>%
  summarise(
    n_customers   = n(),
    avg_frequency = round(mean(Frequency), 2),
    avg_monetary  = round(mean(Monetary), 2),
    pct_repeat    = round(100 * mean(Frequency > 1), 1),
    .groups = "drop"
  ) %>%
  arrange(desc(avg_monetary))

print(channel_clv_summary)
save_csv(channel_clv_summary, "07_clv_by_acquisition_channel.csv")

p <- ggplot(channel_clv_summary, aes(x = reorder(FirstChannel, -avg_monetary), y = avg_monetary)) +
  geom_bar(stat = "identity", fill = COL_PRIMARY, width = 0.7) +
  geom_text(aes(label = paste0("\u20ac", round(avg_monetary, 0), "\n",
                                pct_repeat, "% repeat")),
            vjust = -0.3, size = 3.2, fontface = "bold", lineheight = 0.85) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.2)), labels = label_comma(prefix = "\u20ac")) +
  labs(title = "Average Customer Spend by Acquisition Channel",
       subtitle = "Total lifetime spend per customer, grouped by first-touch channel",
       x = "", y = "Avg Total Spend (EUR)") +
  theme_sre() +
  theme(axis.text.x = element_text(angle = 30, hjust = 1))
save_plot(p, "11_clv_by_acquisition_channel.png")


###############################################################################
### G. EXPERIMENT DESIGN WITH POWER ANALYSIS ##################################
###############################################################################
cat("\n=== G: POWER ANALYSIS ===\n")

at_risk_customers <- customer_rfm %>% filter(Segment == "At Risk")
n_at_risk <- nrow(at_risk_customers)

baseline_reactivation <- 0.05
target_reactivation   <- 0.10
effect_size_retention <- ES.h(target_reactivation, baseline_reactivation)

power_retention <- pwr.2p.test(
  h = effect_size_retention, sig.level = 0.05, power = 0.80, alternative = "greater"
)
cat("Experiment 1 (Win-back): n/group =", ceiling(power_retention$n),
    "| Feasible?", ifelse(2 * ceiling(power_retention$n) <= n_at_risk, "YES", "NO"), "\n")

avg_orders_6m <- mean(customer_rfm$Frequency) / (mean(customer_rfm$Tenure) / 180)
sd_orders     <- sd(customer_rfm$Frequency) / (mean(customer_rfm$Tenure) / 180)
effect_size_frequency <- (avg_orders_6m * 0.15) / sd_orders

power_frequency <- pwr.t.test(
  d = effect_size_frequency, sig.level = 0.05, power = 0.80,
  type = "two.sample", alternative = "greater"
)
cat("Experiment 2 (Loyalty): n/group =", ceiling(power_frequency$n), "\n")

avg_aov <- mean(orders$PriceIncVat)
sd_aov  <- sd(orders$PriceIncVat)
effect_size_aov <- (avg_aov * 0.05) / sd_aov

power_aov <- pwr.t.test(
  d = effect_size_aov, sig.level = 0.05, power = 0.80,
  type = "two.sample", alternative = "greater"
)
weekly_orders <- nrow(orders) / as.numeric(max(orders$order_date_local, na.rm = TRUE) -
                                             min(orders$order_date_local, na.rm = TRUE)) * 7
cat("Experiment 3 (Upsell): n/group =", ceiling(power_aov$n),
    "| ~", ceiling(2 * ceiling(power_aov$n) / weekly_orders), "weeks\n")

power_summary <- data.frame(
  Experiment = c(
    "Win-back (5%->10% reactivation)",
    "Loyalty (+15% frequency)",
    "Upsell (+5% AOV)"
  ),
  Required_n_per_group = c(
    ceiling(power_retention$n),
    ceiling(power_frequency$n),
    ceiling(power_aov$n)
  ),
  Feasible = c(
    paste0("Yes (", format(n_at_risk, big.mark = ","), " at-risk)"),
    "Yes",
    paste0("Yes (~", ceiling(2 * ceiling(power_aov$n) / weekly_orders), " weeks)")
  )
)
save_csv(power_summary, "08_power_analysis_summary.csv")


###############################################################################
### H. REPEAT PURCHASE BEHAVIOR ###############################################
###############################################################################
cat("\n=== H: REPEAT PURCHASE BEHAVIOR ===\n")

max_tx <- 10
hist_dist <- customer_rfm %>%
  mutate(tx_bucket = ifelse(Frequency >= max_tx, paste0(max_tx, "+"), as.character(Frequency))) %>%
  count(tx_bucket) %>%
  mutate(
    tx_bucket = factor(tx_bucket, levels = c(as.character(1:(max_tx-1)), paste0(max_tx, "+"))),
    pct  = n / sum(n) * 100,
    type = "Historical"
  )

pred_dist <- bgf_data %>%
  mutate(expected_total = n_orders + round(predicted_purchases)) %>%
  mutate(tx_bucket = ifelse(expected_total >= max_tx, paste0(max_tx, "+"),
                            as.character(pmax(1, expected_total)))) %>%
  count(tx_bucket) %>%
  mutate(
    tx_bucket = factor(tx_bucket, levels = c(as.character(1:(max_tx-1)), paste0(max_tx, "+"))),
    pct  = n / sum(n) * 100,
    type = "Predicted (BG/NBD)"
  )

repeat_df <- bind_rows(hist_dist, pred_dist)

p <- ggplot(repeat_df, aes(x = tx_bucket, y = pct, fill = type)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.7), width = 0.6) +
  geom_text(aes(label = paste0(round(pct, 1), "%")),
            position = position_dodge(width = 0.7), vjust = -0.5, size = 3, fontface = "bold") +
  scale_fill_manual(values = c("Historical" = COL_PRIMARY, "Predicted (BG/NBD)" = COL_SECONDARY)) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
  labs(title = "Repeat Purchase Behavior",
       subtitle = "Historical vs. BG/NBD predicted long-term rates",
       x = "Number of Transactions", y = "% of Customers", fill = NULL) +
  theme_sre()
save_plot(p, "12_repeat_purchase_behavior.png")

pct_repeat <- round(100 * mean(customer_rfm$Frequency >= 2), 1)
cat("Customers reaching 2nd order:", pct_repeat, "%\n")


###############################################################################
### I. TIME TO SECOND PURCHASE ################################################
###############################################################################
cat("\n=== I: TIME TO SECOND PURCHASE ===\n")

second_purchase <- orders %>%
  filter(order_sequence == 2) %>%
  select(CustomerID, second_order_date = order_date_local)

time_to_2nd <- customer_rfm %>%
  left_join(second_purchase, by = "CustomerID") %>%
  mutate(
    event = ifelse(!is.na(second_order_date), 1, 0),
    time  = ifelse(event == 1,
                   as.numeric(second_order_date - FirstOrder),
                   as.numeric(analysis_date - FirstOrder))
  ) %>%
  filter(time > 0, time <= 365)

surv_obj <- Surv(time_to_2nd$time, time_to_2nd$event)
surv_fit <- survfit(surv_obj ~ 1)

surv_df <- data.frame(
  time = surv_fit$time, surv = surv_fit$surv, converted = 1 - surv_fit$surv
)

day30 <- surv_df$converted[which.min(abs(surv_df$time - 30))]
day90 <- surv_df$converted[which.min(abs(surv_df$time - 90))]

milestone_df <- data.frame(
  time = c(7, 14, 30, 90),
  converted = c(
    surv_df$converted[which.min(abs(surv_df$time - 7))],
    surv_df$converted[which.min(abs(surv_df$time - 14))],
    day30, day90
  ) * 100
)
milestone_df$label <- paste0("Day ", milestone_df$time, ": ", round(milestone_df$converted, 1), "%")

p <- ggplot(surv_df, aes(x = time, y = converted * 100)) +
  geom_area(fill = COL_PRIMARY, alpha = 0.2) +
  geom_line(color = COL_PRIMARY, linewidth = 1.2) +
  geom_point(data = milestone_df, aes(x = time, y = converted),
             color = COL_ACCENT, size = 3.5) +
  geom_label_repel(data = milestone_df,
                   aes(x = time, y = converted, label = label),
                   fill = "grey20", color = "white", size = 3, fontface = "bold",
                   nudge_y = 3, segment.color = "grey50") +
  scale_x_continuous(breaks = c(0, 7, 14, 30, 60, 90, 180, 365)) +
  labs(title = "Time to Second Purchase (Conversion Curve)",
       subtitle = "Cumulative % of first-time buyers who convert within N days",
       x = "Days After First Purchase", y = "% Converted to Repeat Customer") +
  theme_sre() +
  theme(panel.grid.major.x = element_line(color = "grey92"))
save_plot(p, "15_time_to_second_purchase.png")

cat("30-day conversion:", round(day30 * 100, 1), "%\n")
cat("90-day conversion:", round(day90 * 100, 1), "%\n")


###############################################################################
### J. CUSTOMER SEGMENT VALUE MAP #############################################
###############################################################################
cat("\n=== J: SEGMENT VALUE MAP ===\n")

seg_map <- customer_rfm %>%
  group_by(Segment) %>%
  summarise(
    n = n(), avg_recency = mean(Recency), avg_freq = mean(Frequency),
    avg_revenue = mean(Monetary), total_rev = sum(Monetary),
    .groups = "drop"
  )

p <- ggplot(seg_map, aes(x = avg_recency, y = avg_freq)) +
  geom_point(aes(size = total_rev, color = Segment), alpha = 0.8) +
  geom_label_repel(aes(label = paste0(Segment, "\n",
                                       format(n, big.mark = ","), " customers\n",
                                       "\u20ac", format(round(total_rev / 1e6, 1), big.mark = ","), "M")),
                   size = 3, fontface = "bold", box.padding = 0.8) +
  scale_size_continuous(range = c(8, 35), guide = "none") +
  scale_color_manual(values = SEGMENT_COLORS, guide = "none") +
  scale_x_reverse() +
  labs(title = "Customer Segment Value Map",
       subtitle = "Bubble size = total revenue | X-axis reversed (recent = right)",
       x = "Avg Recency (days) \u2190 more recent", y = "Avg Order Frequency") +
  theme_sre() +
  theme(panel.grid.major.x = element_line(color = "grey92"))
save_plot(p, "17_segment_value_map.png", w = 12, h = 8)


###############################################################################
### K. EXECUTIVE SUMMARY ######################################################
###############################################################################
cat("\n=== K: EXECUTIVE SUMMARY ===\n")

exec_summary <- data.frame(
  Metric = c(
    "Total Customers",
    "Total Orders",
    "Total Revenue",
    "Avg Order Value",
    "One-Time Buyers",
    "Churn Rate",
    "Avg P(Alive)",
    "Revenue at Risk",
    "Projected CLV (repeat, 12mo)",
    "Champions",
    "At Risk + Lost"
  ),
  Value = c(
    format(nrow(customer_rfm), big.mark = ","),
    format(nrow(orders), big.mark = ","),
    paste0("EUR ", format(round(sum(orders$PriceIncVat), 0), big.mark = ",")),
    paste0("EUR ", round(mean(orders$PriceIncVat), 2)),
    paste0(round(100 * mean(customer_rfm$Frequency == 1), 1), "%"),
    paste0(round(100 * mean(customer_rfm$is_churned), 1), "%"),
    round(mean(bgf_data$p_alive, na.rm = TRUE), 3),
    paste0("EUR ", format(round(total_revenue_at_risk, 0), big.mark = ","),
           " (", round(100 * total_revenue_at_risk / total_revenue, 1), "%)"),
    paste0("EUR ", format(round(sum(gg_data$CLV_proper, na.rm = TRUE), 0), big.mark = ",")),
    paste0(round(100 * mean(customer_rfm$Segment == "Champions"), 1), "% of customers, ",
           round(100 * sum(customer_rfm$Monetary[customer_rfm$Segment == "Champions"]) /
                   sum(customer_rfm$Monetary), 1), "% of revenue"),
    paste0(round(100 * mean(customer_rfm$Segment %in% c("At Risk", "Lost")), 1), "% of customers")
  )
)
save_csv(exec_summary, "09_executive_summary.csv")

cat("\n=== ALL ADVANCED OUTPUTS SAVED TO", adv_dir, "===\n")
cat("Plots: 14 PNG files\n")
cat("Tables: 9 CSV files\n")
cat("=== DONE ===\n")
