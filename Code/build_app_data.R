
# Install packages if needed
needed <- c("dplyr", "tidyr")
for (pkg in needed) {
  if (!require(pkg, character.only = TRUE, quietly = TRUE)) {
    install.packages(pkg, repos = "https://cloud.r-project.org")
    library(pkg, character.only = TRUE)
  }
}

library(dplyr)
library(tidyr)


find_project_root <- function() {
  candidates <- c(".", "..", "../..", "../../..")
  
  for (d in candidates) {
    has_outputs <- dir.exists(file.path(d, "outputs"))
    has_script  <- file.exists(file.path(d, "build_app_data.R")) || file.exists(file.path(d, "app.R"))
    
    if (has_outputs && has_script) {
      return(normalizePath(d, winslash = "/", mustWork = TRUE))
    }
  }
  
  stop(
    "Could not find the project folder. Please open the folder containing app.R, build_app_data.R, and outputs/ in RStudio, then run again."
  )
}

root_dir <- find_project_root()
outputs_dir <- file.path(root_dir, "outputs")

cat("Using project folder:\n", root_dir, "\n\n")

require_file <- function(path) {
  if (!file.exists(path)) {
    stop("Required file not found: ", path)
  }
}


files_to_read <- c(
  "01_orders_per_month.csv",
  "02_channel_summary.csv",
  "04_segment_summary.csv",
  "05_retention_matrix.csv",
  "06_bgnbd_segment_projections.csv",
  "07_clv_by_segment.csv",
  "09_executive_summary.csv",
  "05_revenue_at_risk.csv",
  "06_channel_migration.csv",
  "07_clv_by_acquisition_channel.csv",
  "08_power_analysis_summary.csv"
)

invisible(lapply(file.path(outputs_dir, files_to_read), require_file))

orders_per_month <- read.csv(file.path(outputs_dir, "01_orders_per_month.csv"), stringsAsFactors = FALSE, check.names = FALSE)
channel_summary <- read.csv(file.path(outputs_dir, "02_channel_summary.csv"), stringsAsFactors = FALSE, check.names = FALSE)
segment_summary <- read.csv(file.path(outputs_dir, "04_segment_summary.csv"), stringsAsFactors = FALSE, check.names = FALSE)
retention_matrix <- read.csv(file.path(outputs_dir, "05_retention_matrix.csv"), stringsAsFactors = FALSE, check.names = FALSE)
bgnbd_segment_projections <- read.csv(file.path(outputs_dir, "06_bgnbd_segment_projections.csv"), stringsAsFactors = FALSE, check.names = FALSE)
clv_by_segment <- read.csv(file.path(outputs_dir, "07_clv_by_segment.csv"), stringsAsFactors = FALSE, check.names = FALSE)
executive_summary <- read.csv(file.path(outputs_dir, "09_executive_summary.csv"), stringsAsFactors = FALSE, check.names = FALSE)

revenue_at_risk <- read.csv(file.path(outputs_dir, "05_revenue_at_risk.csv"), stringsAsFactors = FALSE, check.names = FALSE)
channel_migration <- read.csv(file.path(outputs_dir, "06_channel_migration.csv"), stringsAsFactors = FALSE, check.names = FALSE)
clv_by_acquisition_channel <- read.csv(file.path(outputs_dir, "07_clv_by_acquisition_channel.csv"), stringsAsFactors = FALSE, check.names = FALSE)
power_analysis_summary <- read.csv(file.path(outputs_dir, "08_power_analysis_summary.csv"), stringsAsFactors = FALSE, check.names = FALSE)


# Orders over time
orders_per_month <- orders_per_month %>%
  mutate(
    month_date = as.Date(paste0(order_year_month, "-01"))
  )

# Segment summary: add revenue share
segment_summary <- segment_summary %>%
  mutate(
    revenue_share_pct = 100 * total_revenue / sum(total_revenue)
  )

# Retention matrix -> long format for heatmap and average curve
retention_long <- retention_matrix %>%
  pivot_longer(
    cols = -CohortMonth,
    names_to = "CohortPeriod",
    values_to = "retention_rate"
  ) %>%
  mutate(
    CohortPeriod = as.integer(CohortPeriod)
  )

avg_retention <- retention_long %>%
  filter(!is.na(retention_rate)) %>%
  group_by(CohortPeriod) %>%
  summarise(
    avg_retention = mean(retention_rate),
    .groups = "drop"
  )

# Keep segment order consistent with your main segment summary
segment_order <- segment_summary$Segment

segment_summary$Segment <- factor(segment_summary$Segment, levels = segment_order)
bgnbd_segment_projections$Segment <- factor(bgnbd_segment_projections$Segment, levels = segment_order)
clv_by_segment$Segment <- factor(clv_by_segment$Segment, levels = segment_order)
revenue_at_risk$Segment <- factor(revenue_at_risk$Segment, levels = segment_order)


saveRDS(orders_per_month, file.path(outputs_dir, "app_orders_per_month.rds"))
saveRDS(channel_summary, file.path(outputs_dir, "app_channel_summary.rds"))
saveRDS(segment_summary, file.path(outputs_dir, "app_segment_summary.rds"))
saveRDS(retention_matrix, file.path(outputs_dir, "app_retention_matrix.rds"))
saveRDS(retention_long, file.path(outputs_dir, "app_retention_long.rds"))
saveRDS(avg_retention, file.path(outputs_dir, "app_avg_retention.rds"))
saveRDS(bgnbd_segment_projections, file.path(outputs_dir, "app_bgnbd_segment_projections.rds"))
saveRDS(clv_by_segment, file.path(outputs_dir, "app_clv_by_segment.rds"))
saveRDS(executive_summary, file.path(outputs_dir, "app_executive_summary.rds"))
saveRDS(revenue_at_risk, file.path(outputs_dir, "app_revenue_at_risk.rds"))
saveRDS(channel_migration, file.path(outputs_dir, "app_channel_migration.rds"))
saveRDS(clv_by_acquisition_channel, file.path(outputs_dir, "app_clv_by_acquisition_channel.rds"))
saveRDS(power_analysis_summary, file.path(outputs_dir, "app_power_analysis_summary.rds"))

cat("\nDone. App-ready files created:\n")
print(list.files(outputs_dir, pattern = "^app_"))