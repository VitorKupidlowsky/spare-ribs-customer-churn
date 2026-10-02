library(dplyr)
library(lubridate)

######################
# 1) UPLOAD THE FILE #
######################
orders <- read.csv(
  file = "group_assignment_2026.csv",
  stringsAsFactors = FALSE,
  colClasses = c(
    "integer",   # OrderID
    "character", # DeliveryTimestamp
    "numeric",   # PriceIncVat
    "numeric",   # PriceExcVat
    "character", # ZipCode
    "character", # City
    "character", # CustomerID
    "character", # StoreName
    "character"  # Channel
    )
  )

dir.create("outputs", showWarnings = FALSE)

#####################
# 2) BASIC CLEANING #
#####################
orders$CustomerID <- trimws(orders$CustomerID)
orders$StoreName  <- trimws(orders$StoreName)
orders$Channel    <- trimws(orders$Channel)
orders$City       <- toupper(trimws(orders$City))
orders$ZipCode    <- toupper(gsub("\\s+", "", trimws(orders$ZipCode)))

orders$City[orders$City == ""] <- NA
orders$ZipCode[orders$ZipCode %in% c("", "X")] <- NA

orders$valid_zip <- grepl("^[0-9]{4}[A-Z]{2}$", orders$ZipCode)

orders$vat_amount <- orders$PriceIncVat - orders$PriceExcVat
orders$zero_value_order <- orders$PriceIncVat == 0 | orders$PriceExcVat == 0
orders$negative_value_order <- orders$PriceIncVat < 0 | orders$PriceExcVat < 0

############################
# 3) DATE / TIME VARIABLES #
############################
orders$order_datetime_utc <- ymd_hms(orders$DeliveryTimestamp, tz = "UTC")
orders$order_datetime_local <- with_tz(orders$order_datetime_utc, tzone = "Europe/Amsterdam")
orders$order_date_local <- as.Date(orders$order_datetime_local)

orders$order_year <- year(orders$order_datetime_local)
orders$order_month <- month(orders$order_datetime_local)
orders$order_year_month <- format(orders$order_date_local, "%Y-%m")
orders$order_hour <- hour(orders$order_datetime_local)
orders$order_weekday_num <- wday(orders$order_datetime_local, week_start = 1)
orders$is_weekend <- orders$order_weekday_num >= 6

#######################################
# 4) REMOVE EXACT DUPLICATES AND SORT #
#######################################
orders <- orders %>% distinct()
orders <- orders %>% arrange(CustomerID, order_datetime_local, OrderID)

##################################
# 5) ORDER SEQUENCE PER CUSTOMER #
##################################
orders <- orders %>%
  group_by(CustomerID) %>%
  mutate(
    order_sequence = row_number(),
    is_first_order = order_sequence == 1,
    is_repeat_order = order_sequence > 1,
    days_since_prev_order = as.numeric(
      difftime(order_datetime_local, lag(order_datetime_local), units = "days")
    )
  ) %>%
  ungroup()

analysis_date <- max(orders$order_date_local, na.rm = TRUE)

#################################
# 6) CUSTOMER x CHANNEL SUMMARY #
#################################
customer_channel_summary <- orders %>%
  group_by(CustomerID, Channel) %>%
  summarise(
    n_orders_channel = n(),
    revenue_channel = sum(PriceIncVat, na.rm = TRUE),
    first_order_channel = min(order_date_local, na.rm = TRUE),
    last_order_channel = max(order_date_local, na.rm = TRUE),
    .groups = "drop")

primary_channel <- customer_channel_summary %>%
  arrange(CustomerID, desc(n_orders_channel), desc(revenue_channel), Channel) %>%
  group_by(CustomerID) %>%
  slice(1) %>%
  ungroup() %>%
  select(CustomerID, primary_channel = Channel)

###############################
# 7) CUSTOMER x STORE SUMMARY #
###############################
customer_store_summary <- orders %>%
  group_by(CustomerID, StoreName) %>%
  summarise(
    n_orders_store = n(),
    revenue_store = sum(PriceIncVat, na.rm = TRUE),
    first_order_store = min(order_date_local, na.rm = TRUE),
    last_order_store = max(order_date_local, na.rm = TRUE),
    .groups = "drop"
  )

primary_store <- customer_store_summary %>%
  arrange(CustomerID, desc(n_orders_store), desc(revenue_store), StoreName) %>%
  group_by(CustomerID) %>%
  slice(1) %>%
  ungroup() %>%
  select(CustomerID, primary_store = StoreName)

###########################
# 8) CUSTOMER-LEVEL TABLE #
###########################
customer_level <- orders %>%
  group_by(CustomerID) %>%
  summarise(
    n_orders = n(),
    total_revenue_inc_vat = sum(PriceIncVat, na.rm = TRUE),
    total_revenue_exc_vat = sum(PriceExcVat, na.rm = TRUE),
    avg_order_value_inc_vat = mean(PriceIncVat, na.rm = TRUE),
    avg_order_value_exc_vat = mean(PriceExcVat, na.rm = TRUE),
    first_order_date = min(order_date_local, na.rm = TRUE),
    last_order_date = max(order_date_local, na.rm = TRUE),
    customer_age_days = as.numeric(analysis_date - first_order_date),
    recency_days = as.numeric(analysis_date - last_order_date),
    active_span_days = as.numeric(last_order_date - first_order_date),
    avg_days_between_orders = if (all(is.na(days_since_prev_order))) NA_real_ else mean(days_since_prev_order, na.rm = TRUE),
    median_days_between_orders = if (all(is.na(days_since_prev_order))) NA_real_ else median(days_since_prev_order, na.rm = TRUE),
    n_distinct_stores = n_distinct(StoreName, na.rm = TRUE),
    n_distinct_channels = n_distinct(Channel, na.rm = TRUE),
    first_store = first(StoreName),
    last_store = last(StoreName),
    first_channel = first(Channel),
    last_channel = last(Channel),
    first_order_value_inc_vat = first(PriceIncVat),
    last_order_value_inc_vat = last(PriceIncVat),
    is_repeat_customer = n() > 1,
    .groups = "drop"
  ) %>%
  left_join(primary_channel, by = "CustomerID") %>%
  left_join(primary_store, by = "CustomerID")

######################
# 9) SIMPLE QC TABLE #
######################
qc_summary <- data.frame(
  metric = c(
    "n_rows",
    "n_unique_orders",
    "n_unique_customers",
    "duplicate_order_ids",
    "missing_city",
    "missing_zip",
    "valid_zip",
    "zero_value_orders",
    "negative_value_orders",
    "analysis_date"
  ),
  value = c(
    nrow(orders),
    length(unique(orders$OrderID)),
    length(unique(orders$CustomerID)),
    sum(duplicated(orders$OrderID)),
    sum(is.na(orders$City)),
    sum(is.na(orders$ZipCode)),
    sum(orders$valid_zip, na.rm = TRUE),
    sum(orders$zero_value_order, na.rm = TRUE),
    sum(orders$negative_value_order, na.rm = TRUE),
    as.character(analysis_date)))

##########################
# 10) METRIC DEFINITIONS #
##########################
metric_definitions <- data.frame(
  variable = c(
    "n_orders",
    "total_revenue_inc_vat",
    "total_revenue_exc_vat",
    "avg_order_value_inc_vat",
    "first_order_date",
    "last_order_date",
    "customer_age_days",
    "recency_days",
    "active_span_days",
    "avg_days_between_orders",
    "n_distinct_stores",
    "n_distinct_channels",
    "primary_store",
    "primary_channel",
    "is_repeat_customer"
  ),
  definition = c(
    "Number of observed orders for the customer",
    "Total revenue including VAT",
    "Total revenue excluding VAT",
    "Average order value including VAT",
    "Date of first observed order",
    "Date of most recent observed order",
    "Days between analysis date and first order",
    "Days between analysis date and last order",
    "Days between first and last order",
    "Average days between consecutive orders",
    "Number of different stores used",
    "Number of different channels used",
    "Store with most orders for that customer",
    "Channel with most orders for that customer",
    "TRUE if customer has more than 1 order"
  )
)

####################
# 11) SAVE OUTPUTS #
####################
write.csv(orders, "outputs/orders_clean.csv", row.names = FALSE)
write.csv(customer_level, "outputs/customer_level.csv", row.names = FALSE)
write.csv(customer_channel_summary, "outputs/customer_channel_summary.csv", row.names = FALSE)
write.csv(customer_store_summary, "outputs/customer_store_summary.csv", row.names = FALSE)
write.csv(qc_summary, "outputs/qc_summary.csv", row.names = FALSE)
write.csv(metric_definitions, "outputs/metric_definitions.csv", row.names = FALSE)

saveRDS(orders, "outputs/orders_clean.rds")
saveRDS(customer_level, "outputs/customer_level.rds")

###################
# 12) QUICK CHECK #
###################
head(orders)
head(customer_level)
qc_summary