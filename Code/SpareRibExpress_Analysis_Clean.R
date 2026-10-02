### Install and load libraries ########################################################
packages_to_install <- c(
  "dplyr",
  "tidyr",
  "ggplot2",
  "ggpubr",
  "gridExtra",
  "scales",
  "data.table",
  "stargazer",
  "BTYD",
  "lubridate"
)
for (pkg in packages_to_install) {
  if (!require(pkg, character.only = TRUE, quietly = TRUE)) {
    install.packages(pkg)
    library(pkg, character.only = TRUE)
  }
}

save_plot <- function(plot, filename, w = 10, h = 6) {
  ggsave(filename = file.path("outputs", filename), plot = plot,
         width = w, height = h, dpi = 200, bg = "white")
  cat("  Saved:", filename, "\n")
}

save_csv <- function(df, filename) {
  write.csv(df, file.path("outputs", filename), row.names = FALSE)
  cat("  Saved:", filename, "\n")
}

### 1. IMPORTING THE CLEANED DATA #####################################################
orders <- readRDS("orders_clean.rds")
customer_level <- readRDS("customer_level.rds")

orders$DeliveryDayOfWeek <- factor(
  weekdays(orders$order_date_local),
  levels = c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday")
)

orders$Channel   <- as.factor(orders$Channel)
orders$StoreName <- as.factor(orders$StoreName)
orders$City      <- as.factor(orders$City)

str(orders)
summary(orders)
colSums(is.na(orders))

cat("Date range:", as.character(min(orders$order_date_local, na.rm = TRUE)), "to",
    as.character(max(orders$order_date_local, na.rm = TRUE)), "\n")
cat("Number of unique customers:", length(unique(orders$CustomerID)), "\n")
cat("Number of unique orders:", length(unique(orders$OrderID)), "\n")
cat("Number of unique stores:", length(unique(orders$StoreName)), "\n")
cat("Number of unique channels:", length(unique(orders$Channel)), "\n")

### 2. DATA EXPLORATION ################################################################
cat("\n=== SECTION 2: DATA EXPLORATION ===\n")

## 2.1 Revenue distribution
p <- ggplot(orders, aes(x = PriceIncVat)) +
  geom_histogram(bins = 100, fill = "lightblue", color = "black") +
  xlim(0, quantile(orders$PriceIncVat, 0.99)) +
  labs(title = "Distribution of Order Values (incl. VAT)",
       x = "Order Value (EUR)", y = "Count") +
  theme_minimal()
save_plot(p, "01_order_value_distribution.png")

cat("Mean order value:", round(mean(orders$PriceIncVat), 2), "EUR\n")
cat("Median order value:", round(median(orders$PriceIncVat), 2), "EUR\n")

## 2.2 Orders over timeorders_per_month <- orders %>%
  group_by(order_year_month) %>%
  summarise(
    n_orders        = n(),
    total_revenue   = sum(PriceIncVat),
    avg_order_value = mean(PriceIncVat),
    n_customers     = n_distinct(CustomerID),
    .groups = "drop"
  ) %>%
  arrange(order_year_month)

save_csv(orders_per_month, "01_orders_per_month.csv")

p <- ggplot(orders_per_month, aes(x = order_year_month, y = n_orders, group = 1)) +
  geom_line(color = "steelblue", linewidth = 1) +
  geom_point(color = "steelblue") +
  labs(title = "Number of Orders Over Time", x = "Month", y = "Number of Orders") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
save_plot(p, "02_orders_over_time.png")

p <- ggplot(orders_per_month, aes(x = order_year_month, y = total_revenue, group = 1)) +
  geom_line(color = "darkgreen", linewidth = 1) +
  geom_point(color = "darkgreen") +
  labs(title = "Total Revenue Over Time", x = "Month", y = "Revenue (EUR)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
save_plot(p, "03_revenue_over_time.png")

p <- ggplot(orders_per_month, aes(x = order_year_month, y = n_customers, group = 1)) +
  geom_line(color = "purple", linewidth = 1) +
  geom_point(color = "purple") +
  labs(title = "Active Customers Over Time", x = "Month", y = "Unique Customers") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
save_plot(p, "04_active_customers_over_time.png")

## 2.3 Orders by day of week and hour -------------------------------------------------
orders_by_day <- orders %>%
  group_by(DeliveryDayOfWeek) %>%
  summarise(n_orders = n(), avg_revenue = mean(PriceIncVat), .groups = "drop")

p <- ggplot(orders_by_day, aes(x = DeliveryDayOfWeek, y = n_orders)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(title = "Orders by Day of Week", x = "Day", y = "Number of Orders") +
  theme_minimal()
save_plot(p, "05_orders_by_day_of_week.png")

orders_by_hour <- orders %>%
  group_by(order_hour) %>%
  summarise(n_orders = n(), avg_revenue = mean(PriceIncVat), .groups = "drop")

p <- ggplot(orders_by_hour, aes(x = order_hour, y = n_orders)) +
  geom_bar(stat = "identity", fill = "coral") +
  labs(title = "Orders by Hour of Day", x = "Hour", y = "Number of Orders") +
  theme_minimal()
save_plot(p, "06_orders_by_hour.png")

## 2.4 Channel analysischannel_summary <- orders %>%
  group_by(Channel) %>%
  summarise(
    n_orders        = n(),
    total_revenue   = sum(PriceIncVat),
    avg_order_value = mean(PriceIncVat),
    n_customers     = n_distinct(CustomerID),
    .groups = "drop"
  ) %>%
  arrange(desc(n_orders))

print(channel_summary)
save_csv(channel_summary, "02_channel_summary.csv")

p <- ggplot(channel_summary, aes(x = reorder(Channel, -n_orders), y = n_orders)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(title = "Orders by Channel", x = "Channel", y = "Number of Orders") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
save_plot(p, "07_orders_by_channel.png")

p <- ggplot(channel_summary, aes(x = reorder(Channel, -avg_order_value), y = avg_order_value)) +
  geom_bar(stat = "identity", fill = "darkgreen") +
  labs(title = "Average Order Value by Channel", x = "Channel", y = "Avg Order Value (EUR)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
save_plot(p, "08_avg_order_value_by_channel.png")

## 2.5 Store analysisstore_summary <- orders %>%
  group_by(StoreName) %>%
  summarise(
    n_orders        = n(),
    total_revenue   = sum(PriceIncVat),
    avg_order_value = mean(PriceIncVat),
    n_customers     = n_distinct(CustomerID),
    .groups = "drop"
  ) %>%
  arrange(desc(total_revenue))

print(store_summary, n = 50)
save_csv(store_summary, "03_store_summary.csv")

p <- ggplot(head(store_summary, 20), aes(x = reorder(StoreName, total_revenue), y = total_revenue)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +
  labs(title = "Top 20 Stores by Revenue", x = "Store", y = "Total Revenue (EUR)") +
  theme_minimal()
save_plot(p, "09_top20_stores_revenue.png")

### 3. CUSTOMER-LEVEL ANALYSIS (RFM) ##################################################
analysis_date <- max(orders$order_date_local, na.rm = TRUE) + 1

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

cat("Number of customers:", nrow(customer_rfm), "\n")
summary(customer_rfm[, c("Recency", "Frequency", "Monetary", "AvgOrderValue", "Tenure")])

## 3.1 Frequency distribution
p <- ggplot(customer_rfm, aes(x = Frequency)) +
  geom_histogram(bins = 50, fill = "lightblue", color = "black") +
  xlim(0, quantile(customer_rfm$Frequency, 0.99)) +
  labs(title = "Distribution of Customer Purchase Frequency",
       x = "Number of Orders", y = "Number of Customers") +
  theme_minimal()
save_plot(p, "10_frequency_distribution.png")

cat("% single-purchase customers:", round(100 * mean(customer_rfm$Frequency == 1), 1), "%\n")
cat("% customers with 2+ orders:", round(100 * mean(customer_rfm$Frequency >= 2), 1), "%\n")
cat("% customers with 5+ orders:", round(100 * mean(customer_rfm$Frequency >= 5), 1), "%\n")

## 3.2 Recency distribution
p <- ggplot(customer_rfm, aes(x = Recency)) +
  geom_histogram(bins = 50, fill = "salmon", color = "black") +
  labs(title = "Distribution of Recency (Days Since Last Order)",
       x = "Days Since Last Order", y = "Number of Customers") +
  theme_minimal()
save_plot(p, "11_recency_distribution.png")

## 3.3 Monetary distributionp <- ggplot(customer_rfm, aes(x = Monetary)) +
  geom_histogram(bins = 100, fill = "lightgreen", color = "black") +
  xlim(0, quantile(customer_rfm$Monetary, 0.99)) +
  labs(title = "Distribution of Total Customer Spend",
       x = "Total Spend (EUR)", y = "Number of Customers") +
  theme_minimal()
save_plot(p, "12_monetary_distribution.png")

## 3.4 Revenue Concentration (Lorenz Curve)
customer_rfm <- customer_rfm %>% arrange(Monetary)
customer_rfm$cum_customers <- (1:nrow(customer_rfm)) / nrow(customer_rfm)
customer_rfm$cum_revenue   <- cumsum(customer_rfm$Monetary) / sum(customer_rfm$Monetary)

gini <- 1 - 2 * sum(customer_rfm$cum_revenue) / nrow(customer_rfm)
pct_revenue_top20 <- 1 - customer_rfm$cum_revenue[round(0.80 * nrow(customer_rfm))]

p <- ggplot(customer_rfm, aes(x = cum_customers, y = cum_revenue)) +
  geom_line(color = "darkblue", linewidth = 1.2) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "grey") +
  geom_segment(aes(x = 0.80, xend = 0.80, y = 0, yend = 1 - pct_revenue_top20),
               linetype = "dotted", color = "red") +
  annotate("text", x = 0.35, y = 0.85,
           label = paste0("Gini = ", round(gini, 3), "\n",
                          "Top 20% = ", round(100 * pct_revenue_top20, 1), "% of revenue"),
           size = 4.5, fontface = "bold", color = "darkblue") +
  labs(title = "Revenue Concentration Curve (Lorenz)",
       x = "% of Customers (ranked by spend)", y = "% of Total Revenue") +
  scale_x_continuous(labels = scales::percent) +
  scale_y_continuous(labels = scales::percent) +
  theme_minimal()
save_plot(p, "13_pareto_curve.png")

cat("Top 20% of customers account for", round(100 * pct_revenue_top20, 1), "% of revenue\n")
cat("Gini coefficient:", round(gini, 3), "\n")

### 4. CUSTOMER SEGMENTATION BASED ON CLV ##############################################
cat("\n=== SECTION 4: SEGMENTATION ===\n")

## 4.1 RFM Score-based segmentation (6 segments) ---------------------------------------------------
customer_rfm$R_score   <- ntile(-customer_rfm$Recency, 5)
customer_rfm$F_score   <- ntile(customer_rfm$Frequency, 5)

customer_rfm$Segment <- case_when(
  customer_rfm$R_score >= 4 & customer_rfm$F_score >= 4 ~ "Champions",
  customer_rfm$R_score >= 3 & customer_rfm$F_score >= 3 ~ "Loyal Customers",
  customer_rfm$R_score >= 4 & customer_rfm$F_score <= 2 ~ "New Customers",
  customer_rfm$R_score <= 2 & customer_rfm$F_score >= 3 ~ "At Risk",
  customer_rfm$R_score <= 2 & customer_rfm$F_score <= 2 ~ "Lost",
  TRUE ~ "Potential Loyalists"
)

segment_summary <- customer_rfm %>%
  group_by(Segment) %>%
  summarise(
    n_customers   = n(),
    pct_customers = round(100 * n() / nrow(customer_rfm), 1),
    avg_recency   = round(mean(Recency), 0),
    avg_frequency = round(mean(Frequency), 1),
    avg_monetary  = round(mean(Monetary), 2),
    total_revenue = round(sum(Monetary), 0),
    .groups = "drop"
  ) %>%
  arrange(desc(total_revenue))

print(segment_summary)
save_csv(segment_summary, "04_segment_summary.csv")

p <- ggplot(segment_summary, aes(x = reorder(Segment, -n_customers), y = n_customers, fill = Segment)) +
  geom_bar(stat = "identity") +
  labs(title = "Customer Segments - Number of Customers", x = "Segment", y = "Number of Customers") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "none")
save_plot(p, "14_segments_size.png")

p <- ggplot(segment_summary, aes(x = reorder(Segment, -total_revenue), y = total_revenue, fill = Segment)) +
  geom_bar(stat = "identity") +
  labs(title = "Customer Segments - Total Revenue", x = "Segment", y = "Total Revenue (EUR)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "none")
save_plot(p, "15_segments_revenue.png")

## 4.2 RFM Scatter: Recency vs Frequency colored by Monetaryp <- ggplot(customer_rfm, aes(x = Recency, y = Frequency, color = log(Monetary))) +
  geom_point(alpha = 0.3, size = 0.5) +
  scale_color_viridis_c() +
  labs(title = "Customer Map: Recency vs Frequency",
       x = "Recency (days)", y = "Frequency (orders)", color = "log(Total Spend)") +
  theme_minimal()
save_plot(p, "16_rfm_scatter.png")

## 4.3 Segment profiles by channel ----------------------------------------------------
segment_channel <- orders %>%
  left_join(customer_rfm %>% select(CustomerID, Segment), by = "CustomerID") %>%
  group_by(Segment, Channel) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(Segment) %>%
  mutate(pct = n / sum(n))

p <- ggplot(segment_channel, aes(x = Segment, y = pct, fill = Channel)) +
  geom_bar(stat = "identity") +
  labs(title = "Channel Mix by Customer Segment", x = "Segment", y = "Proportion of Orders") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
save_plot(p, "17_channel_mix_by_segment.png")

## 4.4 RFM Segment Matrix (6 segments)
library(forcats)
library(gridExtra)
library(grid)

total_customers <- nrow(customer_rfm)

grid_stats <- customer_rfm %>%
  group_by(R_score, F_score, Segment) %>%
  summarise(n_cust = n(), .groups = "drop") %>%
  mutate(pct = round(100 * n_cust / total_customers, 1))

seg_summary <- customer_rfm %>%
  group_by(Segment) %>%
  summarise(
    customers    = n(),
    pct          = round(100 * n() / total_customers, 1),
    avg_orders   = round(mean(Frequency), 1),
    avg_monetary = round(mean(Monetary), 0),
    .groups = "drop"
  )

seg_centers <- grid_stats %>%
  group_by(Segment) %>%
  summarise(x_center = mean(R_score), y_center = mean(F_score), .groups = "drop") %>%
  left_join(seg_summary, by = "Segment")

segment_order <- c("Champions", "Loyal Customers", "Potential Loyalists",
                    "New Customers", "At Risk", "Lost")

segment_colors_grid <- c(
  "Champions"          = "#00BFA5",
  "Loyal Customers"    = "#26C6DA",
  "Potential Loyalists" = "#66BB6A",
  "New Customers"      = "#AED581",
  "At Risk"            = "#EC407A",
  "Lost"               = "#5C6BC0"
)

grid_stats$Segment <- factor(grid_stats$Segment, levels = segment_order)

p_grid <- ggplot(grid_stats, aes(x = R_score, y = F_score, fill = Segment)) +
  geom_tile(color = "white", linewidth = 2.5) +
  geom_label(data = seg_centers,
             aes(x = x_center, y = y_center,
                 label = paste0(Segment, "\n", pct, "%"),
                 fill = Segment),
             color = "white", fontface = "bold", size = 5,
             lineheight = 1.1, linewidth = 0, label.padding = unit(5, "pt"),
             show.legend = FALSE) +
  scale_fill_manual(values = segment_colors_grid, breaks = segment_order, drop = FALSE) +
  scale_x_continuous(breaks = 1:5, expand = c(0, 0)) +
  scale_y_continuous(breaks = 1:5, expand = c(0, 0)) +
  labs(x = "\nRecency Score \u2192", y = "Frequency Score \u2192\n") +
  coord_equal() +
  theme_minimal(base_size = 14) +
  theme(
    panel.grid      = element_blank(),
    legend.position = "none",
    axis.ticks      = element_line(color = "grey60"),
    axis.text       = element_text(size = 12, face = "bold"),
    axis.title      = element_text(size = 12),
    plot.margin     = margin(10, 10, 10, 10)
  )

legend_df <- seg_summary %>%
  mutate(
    Segment = factor(Segment, levels = segment_order),
    customers_fmt    = format(customers, big.mark = ","),
    avg_orders_fmt   = sprintf("%.1f", avg_orders),
    avg_monetary_fmt = paste0("\u20ac", format(avg_monetary, big.mark = ","))
  ) %>%
  arrange(Segment)

p_legend <- ggplot(legend_df, aes(y = factor(Segment, levels = rev(segment_order)))) +
  geom_tile(aes(x = 0, fill = Segment), width = 0.35, height = 0.8) +
  geom_text(aes(x = 0.65, label = Segment), hjust = 0, size = 4, fontface = "bold") +
  geom_text(aes(x = 3.5, label = paste0(pct, "%")), hjust = 1, size = 4, color = "grey30") +
  geom_text(aes(x = 4.5, label = customers_fmt), hjust = 1, size = 3.8, color = "grey40") +
  geom_text(aes(x = 5.5, label = avg_orders_fmt), hjust = 1, size = 3.8, color = "grey40") +
  geom_text(aes(x = 6.5, label = avg_monetary_fmt), hjust = 1, size = 3.8, color = "grey40") +
  annotate("text", x = 3.5, y = 7.3, label = "%", size = 3.2, fontface = "bold", hjust = 1) +
  annotate("text", x = 4.5, y = 7.3, label = "customers", size = 3.2, fontface = "bold", hjust = 1) +
  annotate("text", x = 5.5, y = 7.3, label = "avg orders", size = 3.2, fontface = "bold", hjust = 1) +
  annotate("text", x = 6.5, y = 7.3, label = "avg spend", size = 3.2, fontface = "bold", hjust = 1) +
  scale_fill_manual(values = segment_colors_grid, drop = FALSE) +
  scale_x_continuous(limits = c(-0.2, 6.8), expand = c(0, 0)) +
  scale_y_discrete(drop = FALSE, expand = expansion(add = c(0.5, 1.5))) +
  theme_void() +
  theme(legend.position = "none")

title_grob <- textGrob(
  label = "RFM Customer Segments",
  gp    = gpar(fontsize = 22, fontface = "bold"),
  just  = "left", x = 0.02
)
subtitle_grob <- textGrob(
  label = paste0(
    format(total_customers, big.mark = ","), " customers | ",
    "6 segments based on Recency & Frequency quintile scores"
  ),
  gp   = gpar(fontsize = 11, col = "grey40"),
  just = "left", x = 0.02
)

final_rfm_plot <- arrangeGrob(
  arrangeGrob(title_grob, subtitle_grob, ncol = 1, heights = c(1, 0.8)),
  arrangeGrob(p_grid, p_legend, ncol = 2, widths = c(1.2, 1)),
  nrow    = 2,
  heights = c(0.7, 4)
)

ggsave(
  filename = file.path("outputs", "rfm_segment_matrix.png"),
  plot = final_rfm_plot,
  width = 16, height = 9, dpi = 200, bg = "white"
)
cat("Saved: outputs/rfm_segment_matrix.png\n")

write.csv(
  seg_summary %>% select(Segment, customers, pct, avg_orders, avg_monetary) %>% arrange(desc(customers)),
  file.path("outputs", "rfm_segment_summary.csv"), row.names = FALSE
)
cat("Saved: outputs/rfm_segment_summary.csv\n")

### 5. COHORT ANALYSIS AND RETENTION ##################################################
cat("\n=== SECTION 5: COHORT RETENTION ===\n")

## 5.1 Define cohorts by first purchase month
customer_rfm$CohortMonth <- format(customer_rfm$FirstOrder, "%Y-%m")

cohort_sizes <- customer_rfm %>%
  group_by(CohortMonth) %>%
  summarise(n_new_customers = n(), .groups = "drop") %>%
  arrange(CohortMonth)

p <- ggplot(cohort_sizes, aes(x = CohortMonth, y = n_new_customers, group = 1)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(title = "New Customer Acquisitions by Month", x = "Cohort Month", y = "New Customers") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
save_plot(p, "18_cohort_acquisitions.png")

## 5.2 Cohort retention table
orders_cohort <- orders %>%
  left_join(customer_rfm %>% select(CustomerID, FirstOrder), by = "CustomerID") %>%
  mutate(
    CohortMonth  = format(FirstOrder, "%Y-%m"),
    OrderMonth   = order_year_month,
    CohortPeriod = (as.numeric(format(order_date_local, "%Y")) -
                      as.numeric(format(FirstOrder, "%Y"))) * 12 +
                   (as.numeric(format(order_date_local, "%m")) -
                      as.numeric(format(FirstOrder, "%m")))
  )

cohort_data <- orders_cohort %>%
  filter(!is.na(CohortMonth), !is.na(CohortPeriod)) %>%
  group_by(CohortMonth, CohortPeriod) %>%
  summarise(n_customers = n_distinct(CustomerID), .groups = "drop")

cohort_data <- cohort_data %>%
  left_join(cohort_sizes, by = "CohortMonth") %>%
  mutate(retention_rate = n_customers / n_new_customers)

retention_matrix <- cohort_data %>%
  select(CohortMonth, CohortPeriod, retention_rate) %>%
  filter(CohortPeriod <= 12) %>%
  tidyr::pivot_wider(names_from = CohortPeriod, values_from = retention_rate)

print(as.data.frame(retention_matrix), row.names = FALSE)
save_csv(as.data.frame(retention_matrix), "05_retention_matrix.csv")

# Heatmap
cohort_heatmap_data <- cohort_data %>% filter(CohortPeriod <= 12)

p <- ggplot(cohort_heatmap_data, aes(x = CohortPeriod, y = CohortMonth, fill = retention_rate)) +
  geom_tile(color = "white") +
  geom_text(aes(label = paste0(round(100 * retention_rate, 0), "%")), size = 2.5) +
  scale_fill_gradient(low = "white", high = "steelblue", labels = scales::percent) +
  labs(title = "Cohort Retention Heatmap",
       x = "Months Since First Purchase", y = "Cohort", fill = "Retention") +
  theme_minimal()
save_plot(p, "19_cohort_retention_heatmap.png", w = 12, h = 8)

## 5.3 Average retention curveavg_retention <- cohort_data %>%
  filter(CohortPeriod <= 12) %>%
  group_by(CohortPeriod) %>%
  summarise(avg_retention = mean(retention_rate, na.rm = TRUE), .groups = "drop")

p <- ggplot(avg_retention, aes(x = CohortPeriod, y = avg_retention)) +
  geom_line(color = "steelblue", linewidth = 1) +
  geom_point(color = "steelblue", size = 3) +
  scale_y_continuous(labels = scales::percent) +
  labs(title = "Average Retention Curve Across Cohorts",
       x = "Months Since First Purchase", y = "Retention Rate") +
  theme_minimal()
save_plot(p, "20_avg_retention_curve.png")

### 6. CUSTOMER BASE PROJECTIONS (BG/NBD MODEL) #######################################
cat("\n=== SECTION 6: BG/NBD MODEL ===\n")

## 6.1 Prepare data for BG/NBDobservation_end <- max(orders$order_date_local, na.rm = TRUE)

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
  )

bgf_data <- bgf_data %>% filter(T.cal > 0)
cat("BG/NBD data prepared for", nrow(bgf_data), "customers\n")

## 6.2 Estimate BG/NBD modelbgf_input <- as.data.frame(bgf_data[, c("x", "t.x", "T.cal")])
params <- BTYD::bgnbd.EstimateParameters(bgf_input)
names(params) <- c("r", "alpha", "a", "b")
cat("BG/NBD Parameters:\n")
print(params)

bgf_data$predicted_purchases <- BTYD::bgnbd.ConditionalExpectedTransactions(
  params = params, T.star = 52,
  x = bgf_data$x, t.x = bgf_data$t.x, T.cal = bgf_data$T.cal
)

bgf_data$p_alive <- BTYD::bgnbd.PAlive(
  params = params,
  x = bgf_data$x, t.x = bgf_data$t.x, T.cal = bgf_data$T.cal
)

cat("\nP(Alive) summary:\n")
summary(bgf_data$p_alive)

p <- ggplot(bgf_data, aes(x = p_alive)) +
  geom_histogram(bins = 50, fill = "lightblue", color = "black") +
  labs(title = "Distribution of P(Alive)", x = "Probability of Being Active", y = "Count") +
  theme_minimal()
save_plot(p, "21_p_alive_distribution.png")

## 6.3 Expected future transactions by segment ----------------------------------------
bgf_data <- bgf_data %>%
  left_join(customer_rfm %>% select(CustomerID, Segment), by = "CustomerID")

segment_projections <- bgf_data %>%
  group_by(Segment) %>%
  summarise(
    n_customers                = n(),
    avg_p_alive                = round(mean(p_alive, na.rm = TRUE), 3),
    avg_predicted_purchases    = round(mean(predicted_purchases, na.rm = TRUE), 2),
    total_predicted_purchases  = round(sum(predicted_purchases, na.rm = TRUE), 0),
    .groups = "drop"
  ) %>%
  arrange(desc(total_predicted_purchases))

print(segment_projections)
save_csv(segment_projections, "06_bgnbd_segment_projections.csv")

p <- ggplot(segment_projections, aes(x = reorder(Segment, total_predicted_purchases),
                                      y = total_predicted_purchases, fill = Segment)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(title = "Predicted Orders Next 12 Months by Segment",
       x = "Segment", y = "Total Predicted Orders") +
  theme_minimal() +
  theme(legend.position = "none")
save_plot(p, "22_predicted_orders_by_segment.png")

## 6.4 CLV projectionavg_order_value <- mean(orders$PriceIncVat)
bgf_data$projected_CLV <- bgf_data$predicted_purchases * avg_order_value

p <- ggplot(bgf_data, aes(x = projected_CLV)) +
  geom_histogram(bins = 100, fill = "lightgreen", color = "black") +
  xlim(0, quantile(bgf_data$projected_CLV, 0.95, na.rm = TRUE)) +
  labs(title = "Projected CLV (Next 12 Months)", x = "Projected CLV (EUR)", y = "Count") +
  theme_minimal()
save_plot(p, "23_clv_distribution.png")

# CLV by segment bar chart
clv_by_segment <- bgf_data %>%
  group_by(Segment) %>%
  summarise(
    n_customers = n(),
    avg_CLV     = round(mean(projected_CLV, na.rm = TRUE), 2),
    median_CLV  = round(median(projected_CLV, na.rm = TRUE), 2),
    total_CLV   = round(sum(projected_CLV, na.rm = TRUE), 0),
    .groups = "drop"
  ) %>%
  arrange(desc(total_CLV))

save_csv(clv_by_segment, "07_clv_by_segment.csv")

p <- ggplot(clv_by_segment, aes(x = reorder(Segment, avg_CLV), y = avg_CLV, fill = Segment)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(title = "Average Projected CLV by Segment",
       x = "Segment", y = "Average CLV (EUR)") +
  theme_minimal() +
  theme(legend.position = "none")
save_plot(p, "24_avg_clv_by_segment.png")

cat("Total projected revenue (next year):",
    round(sum(bgf_data$projected_CLV, na.rm = TRUE), 0), "EUR\n")

### 7. CUSTOMER BASE HEALTH METRICS ####################################################
cat("\n=== SECTION 7: CUSTOMER HEALTH ===\n")

## 7.1 New vs returning customers over time -------------------------------------------
monthly_customer_type <- orders %>%
  group_by(order_year_month, is_first_order) %>%
  summarise(n_customers = n_distinct(CustomerID), .groups = "drop") %>%
  mutate(CustomerType = ifelse(is_first_order, "New", "Returning"))

p <- ggplot(monthly_customer_type, aes(x = order_year_month, y = n_customers, fill = CustomerType)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(title = "New vs Returning Customers Over Time", x = "Month", y = "Number of Customers") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
save_plot(p, "25_new_vs_returning.png")

## 7.2 Repeat purchase rate over time -------------------------------------------------
repeat_customer_ids <- customer_rfm$CustomerID[customer_rfm$Frequency > 1]

monthly_repeat <- orders %>%
  group_by(order_year_month) %>%
  summarise(
    total_customers  = n_distinct(CustomerID),
    repeat_customers = n_distinct(CustomerID[CustomerID %in% repeat_customer_ids]),
    .groups = "drop"
  ) %>%
  mutate(repeat_rate = repeat_customers / total_customers)

p <- ggplot(monthly_repeat, aes(x = order_year_month, y = repeat_rate, group = 1)) +
  geom_line(color = "steelblue", linewidth = 1) +
  geom_point(color = "steelblue") +
  scale_y_continuous(labels = scales::percent) +
  labs(title = "Repeat Customer Rate Over Time", x = "Month", y = "% Repeat Buyers") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
save_plot(p, "26_repeat_rate_over_time.png")

## 7.3 Inter-purchase time analysis ---------------------------------------------------
ipt <- orders %>% filter(!is.na(days_since_prev_order))

cat("Average inter-purchase time:", round(mean(ipt$days_since_prev_order), 1), "days\n")
cat("Median inter-purchase time:", round(median(ipt$days_since_prev_order), 1), "days\n")

p <- ggplot(ipt, aes(x = days_since_prev_order)) +
  geom_histogram(bins = 100, fill = "coral", color = "black") +
  xlim(0, quantile(ipt$days_since_prev_order, 0.95)) +
  labs(title = "Distribution of Inter-Purchase Time", x = "Days Between Purchases", y = "Count") +
  theme_minimal()
save_plot(p, "27_inter_purchase_time.png")

### 8. GEOGRAPHIC ANALYSIS #############################################################
city_summary <- orders %>%
  filter(!is.na(City) & as.character(City) != "NA" & as.character(City) != "") %>%
  group_by(City) %>%
  summarise(
    n_orders        = n(),
    total_revenue   = sum(PriceIncVat),
    n_customers     = n_distinct(CustomerID),
    avg_order_value = mean(PriceIncVat),
    rev_per_customer = round(sum(PriceIncVat) / n_distinct(CustomerID), 2),
    .groups = "drop"
  ) %>%
  arrange(desc(total_revenue))

save_csv(city_summary, "08_geographic_summary.csv")

p <- ggplot(head(city_summary, 20), aes(x = reorder(City, total_revenue), y = total_revenue)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +
  labs(title = "Top 20 Cities by Revenue", x = "City", y = "Total Revenue (EUR)") +
  theme_minimal()
save_plot(p, "28_top_cities_revenue.png")

p <- ggplot(head(city_summary, 20), aes(x = reorder(City, rev_per_customer), y = rev_per_customer)) +
  geom_bar(stat = "identity", fill = "darkgreen") +
  coord_flip() +
  labs(title = "Top 20 Cities by Revenue per Customer", x = "City", y = "Revenue per Customer (EUR)") +
  theme_minimal()
save_plot(p, "29_top_cities_rev_per_customer.png")

### 9. SUMMARY TABLE ###################################################################
cat("\n========== KEY METRICS SUMMARY ==========\n")
cat("Total orders:", nrow(orders), "\n")
cat("Total customers:", nrow(customer_rfm), "\n")
cat("Total revenue:", round(sum(orders$PriceIncVat), 0), "EUR\n")
cat("Average order value:", round(mean(orders$PriceIncVat), 2), "EUR\n")
cat("Average orders per customer:", round(mean(customer_rfm$Frequency), 2), "\n")
cat("% one-time buyers:", round(100 * mean(customer_rfm$Frequency == 1), 1), "%\n")
cat("Top 20% customers =", round(100 * pct_revenue_top20, 1), "% of revenue\n")
cat("Average inter-purchase time:", round(mean(ipt$days_since_prev_order), 1), "days\n")
cat("Avg P(alive):", round(mean(bgf_data$p_alive, na.rm = TRUE), 3), "\n")
cat("Projected next-year revenue:", round(sum(bgf_data$projected_CLV, na.rm = TRUE), 0), "EUR\n")

# Save executive summary as CSV
executive_summary <- data.frame(
  Metric = c(
    "Total Customers",
    "Total Orders",
    "Total Revenue",
    "Avg Order Value",
    "Avg Orders per Customer",
    "One-Time Buyers",
    "Top 20% Revenue Share",
    "Avg Inter-Purchase Time",
    "Avg P(Alive)",
    "Projected Revenue (12mo)"
  ),
  Value = c(
    format(nrow(customer_rfm), big.mark = ","),
    format(nrow(orders), big.mark = ","),
    paste0("EUR ", format(round(sum(orders$PriceIncVat), 0), big.mark = ",")),
    paste0("EUR ", round(mean(orders$PriceIncVat), 2)),
    round(mean(customer_rfm$Frequency), 2),
    paste0(round(100 * mean(customer_rfm$Frequency == 1), 1), "%"),
    paste0(round(100 * pct_revenue_top20, 1), "%"),
    paste0(round(mean(ipt$days_since_prev_order), 1), " days"),
    round(mean(bgf_data$p_alive, na.rm = TRUE), 3),
    paste0("EUR ", format(round(sum(bgf_data$projected_CLV, na.rm = TRUE), 0), big.mark = ","))
  )
)
save_csv(executive_summary, "09_executive_summary.csv")

