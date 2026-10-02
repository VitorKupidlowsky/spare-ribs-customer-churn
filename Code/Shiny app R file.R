
needed <- c("shiny", "ggplot2", "dplyr", "scales", "DT")
for (pkg in needed) {
  if (!require(pkg, character.only = TRUE, quietly = TRUE)) {
    install.packages(pkg, repos = "https://cloud.r-project.org")
    library(pkg, character.only = TRUE)
  }
}

library(shiny)
library(ggplot2)
library(dplyr)
library(scales)
library(DT)


find_project_root <- function() {
  candidates <- c(".", "..", "../..", "../../..")
  
  for (d in candidates) {
    has_outputs <- dir.exists(file.path(d, "outputs"))
    has_app     <- file.exists(file.path(d, "app.R"))
    
    if (has_outputs && has_app) {
      return(normalizePath(d, winslash = "/", mustWork = TRUE))
    }
  }
  
  stop(
    "Could not find the project folder. Please open the folder containing app.R and outputs/ in RStudio, then run again."
  )
}

root_dir <- find_project_root()
outputs_dir <- file.path(root_dir, "outputs")

cat("Using project folder:\n", root_dir, "\n\n")


required_app_files <- c(
  "app_orders_per_month.rds",
  "app_channel_summary.rds",
  "app_segment_summary.rds",
  "app_retention_long.rds",
  "app_avg_retention.rds",
  "app_bgnbd_segment_projections.rds",
  "app_clv_by_segment.rds",
  "app_executive_summary.rds",
  "app_revenue_at_risk.rds",
  "app_channel_migration.rds",
  "app_clv_by_acquisition_channel.rds",
  "app_power_analysis_summary.rds"
)

missing_app_files <- required_app_files[!file.exists(file.path(outputs_dir, required_app_files))]

if (length(missing_app_files) > 0) {
  cat("Missing app-ready files. Running build_app_data.R ...\n")
  source(file.path(root_dir, "build_app_data.R"), local = TRUE)
}


orders_per_month <- readRDS(file.path(outputs_dir, "app_orders_per_month.rds"))
channel_summary <- readRDS(file.path(outputs_dir, "app_channel_summary.rds"))
segment_summary <- readRDS(file.path(outputs_dir, "app_segment_summary.rds"))
retention_long <- readRDS(file.path(outputs_dir, "app_retention_long.rds"))
avg_retention <- readRDS(file.path(outputs_dir, "app_avg_retention.rds"))
bgnbd_segment_projections <- readRDS(file.path(outputs_dir, "app_bgnbd_segment_projections.rds"))
clv_by_segment <- readRDS(file.path(outputs_dir, "app_clv_by_segment.rds"))
executive_summary <- readRDS(file.path(outputs_dir, "app_executive_summary.rds"))
revenue_at_risk <- readRDS(file.path(outputs_dir, "app_revenue_at_risk.rds"))
channel_migration <- readRDS(file.path(outputs_dir, "app_channel_migration.rds"))
clv_by_acquisition_channel <- readRDS(file.path(outputs_dir, "app_clv_by_acquisition_channel.rds"))
power_analysis_summary <- readRDS(file.path(outputs_dir, "app_power_analysis_summary.rds"))


segment_counts <- segment_summary %>%
  select(Segment, segment_customers = n_customers)

bgnbd_display <- bgnbd_segment_projections %>%
  rename(model_n_customers = n_customers) %>%
  left_join(segment_counts, by = "Segment") %>%
  select(
    Segment,
    segment_customers,
    model_n_customers,
    avg_p_alive,
    avg_predicted_purchases,
    total_predicted_purchases
  )

clv_display <- clv_by_segment %>%
  rename(model_n_customers = n_customers) %>%
  left_join(segment_counts, by = "Segment") %>%
  select(
    Segment,
    segment_customers,
    model_n_customers,
    avg_CLV,
    median_CLV,
    total_CLV
  )

risk_display <- revenue_at_risk %>%
  rename(risk_file_n_customers = n_customers) %>%
  left_join(segment_counts, by = "Segment") %>%
  select(
    Segment,
    segment_customers,
    risk_file_n_customers,
    total_revenue,
    revenue_at_risk,
    pct_at_risk,
    avg_churn_prob
  )


get_metric <- function(metric_name) {
  executive_summary$Value[executive_summary$Metric == metric_name][1]
}

fmt_eur <- function(x) {
  paste0("€", format(round(x, 2), nsmall = 2, big.mark = ",", scientific = FALSE))
}


ui <- navbarPage(
  "Spare Rib Express - Customer Analytics",
  
  tabPanel(
    "Overview",
    fluidRow(
      column(3, wellPanel(h4("Total customers"), textOutput("kpi_customers"))),
      column(3, wellPanel(h4("Total orders"), textOutput("kpi_orders"))),
      column(3, wellPanel(h4("Total revenue"), textOutput("kpi_revenue"))),
      column(3, wellPanel(h4("One-time buyers"), textOutput("kpi_one_time")))
    ),
    fluidRow(
      column(6, plotOutput("monthly_orders_plot", height = "350px")),
      column(6, plotOutput("channel_orders_plot", height = "350px"))
    ),
    fluidRow(
      column(12, DTOutput("executive_summary_table"))
    )
  ),
  
  tabPanel(
    "Segments & Retention",
    fluidRow(
      column(6, plotOutput("segment_size_plot", height = "350px")),
      column(6, plotOutput("segment_revenue_share_plot", height = "350px"))
    ),
    fluidRow(
      column(6, plotOutput("retention_curve_plot", height = "350px")),
      column(6, plotOutput("retention_heatmap_plot", height = "350px"))
    ),
    fluidRow(
      column(12, DTOutput("segment_table"))
    )
  ),
  
  tabPanel(
    "CLV & Risk",
    fluidRow(
      column(
        12,
        p("Segment sizes are taken from the main segment summary. Model tables may come from slightly different eligible samples or output versions.")
      )
    ),
    fluidRow(
      column(6, plotOutput("clv_plot", height = "350px")),
      column(6, plotOutput("risk_plot", height = "350px"))
    ),
    fluidRow(
      column(6, DTOutput("bgnbd_table")),
      column(6, DTOutput("clv_table"))
    ),
    fluidRow(
      column(12, DTOutput("risk_table"))
    )
  ),
  
  tabPanel(
    "Channels",
    fluidRow(
      column(6, plotOutput("acq_channel_plot", height = "350px")),
      column(6, plotOutput("migration_heatmap_plot", height = "350px"))
    ),
    fluidRow(
      column(6, DTOutput("channel_table")),
      column(6, DTOutput("migration_table"))
    )
  ),
  
  tabPanel(
    "Experiments",
    fluidRow(
      column(
        12,
        h4("Power analysis summary"),
        p("From the saved experiment-design output."),
        DTOutput("power_table")
      )
    )
  )
)


server <- function(input, output, session) {
  
  output$kpi_customers <- renderText({
    get_metric("Total Customers")
  })
  
  output$kpi_orders <- renderText({
    get_metric("Total Orders")
  })
  
  output$kpi_revenue <- renderText({
    get_metric("Total Revenue")
  })
  
  output$kpi_one_time <- renderText({
    get_metric("One-Time Buyers")
  })
  
  output$monthly_orders_plot <- renderPlot({
    ggplot(orders_per_month, aes(x = month_date, y = n_orders)) +
      geom_line(color = "#2C7FB8", linewidth = 1) +
      geom_point(color = "#2C7FB8", size = 2) +
      scale_y_continuous(labels = comma) +
      labs(
        title = "Orders Over Time",
        x = NULL,
        y = "Number of orders"
      ) +
      theme_minimal(base_size = 13)
  })
  
  output$channel_orders_plot <- renderPlot({
    ggplot(channel_summary, aes(x = reorder(Channel, n_orders), y = n_orders)) +
      geom_col(fill = "#4C78A8") +
      coord_flip() +
      scale_y_continuous(labels = comma) +
      labs(
        title = "Orders by Channel",
        x = NULL,
        y = "Number of orders"
      ) +
      theme_minimal(base_size = 13)
  })
  
  output$executive_summary_table <- renderDT({
    datatable(
      executive_summary,
      rownames = FALSE,
      options = list(dom = "t", paging = FALSE)
    )
  })
  
  output$segment_size_plot <- renderPlot({
    ggplot(segment_summary, aes(x = reorder(Segment, pct_customers), y = pct_customers)) +
      geom_col(fill = "#72B7B2") +
      coord_flip() +
      scale_y_continuous(labels = function(x) paste0(x, "%")) +
      labs(
        title = "Segment Size",
        x = NULL,
        y = "% of customers"
      ) +
      theme_minimal(base_size = 13)
  })
  
  output$segment_revenue_share_plot <- renderPlot({
    ggplot(segment_summary, aes(x = reorder(Segment, revenue_share_pct), y = revenue_share_pct)) +
      geom_col(fill = "#59A14F") +
      coord_flip() +
      scale_y_continuous(labels = function(x) paste0(round(x, 1), "%")) +
      labs(
        title = "Revenue Share by Segment",
        x = NULL,
        y = "% of total revenue"
      ) +
      theme_minimal(base_size = 13)
  })
  
  output$retention_curve_plot <- renderPlot({
    ggplot(avg_retention, aes(x = CohortPeriod, y = avg_retention)) +
      geom_line(color = "#E15759", linewidth = 1) +
      geom_point(color = "#E15759", size = 2) +
      scale_y_continuous(labels = percent) +
      scale_x_continuous(breaks = 0:12) +
      labs(
        title = "Average Retention Curve",
        x = "Months since first purchase",
        y = "Retention"
      ) +
      theme_minimal(base_size = 13)
  })
  
  output$retention_heatmap_plot <- renderPlot({
    ggplot(retention_long, aes(x = CohortPeriod, y = CohortMonth, fill = retention_rate)) +
      geom_tile(color = "white") +
      scale_fill_gradient(low = "white", high = "#2C7FB8", labels = percent) +
      labs(
        title = "Cohort Retention Heatmap",
        x = "Months since first purchase",
        y = "Cohort",
        fill = "Retention"
      ) +
      theme_minimal(base_size = 12)
  })
  
  output$segment_table <- renderDT({
    df <- segment_summary %>%
      mutate(
        pct_customers = paste0(pct_customers, "%"),
        avg_monetary = fmt_eur(avg_monetary),
        total_revenue = fmt_eur(total_revenue),
        revenue_share_pct = paste0(round(revenue_share_pct, 1), "%")
      ) %>%
      rename(
        customers = n_customers,
        avg_spend = avg_monetary,
        revenue = total_revenue,
        revenue_share = revenue_share_pct
      )
    
    datatable(
      df,
      rownames = FALSE,
      options = list(pageLength = 10, scrollX = TRUE)
    )
  })
  
  output$clv_plot <- renderPlot({
    ggplot(clv_by_segment, aes(x = reorder(Segment, avg_CLV), y = avg_CLV)) +
      geom_col(fill = "#F28E2B") +
      coord_flip() +
      labs(
        title = "Average CLV by Segment",
        x = NULL,
        y = "Average CLV (€)"
      ) +
      theme_minimal(base_size = 13)
  })
  
  output$risk_plot <- renderPlot({
    ggplot(revenue_at_risk, aes(x = reorder(Segment, revenue_at_risk), y = revenue_at_risk)) +
      geom_col(fill = "#E15759") +
      coord_flip() +
      scale_y_continuous(labels = comma) +
      labs(
        title = "Revenue at Risk by Segment",
        x = NULL,
        y = "Revenue at risk (€)"
      ) +
      theme_minimal(base_size = 13)
  })
  
  output$bgnbd_table <- renderDT({
    df <- bgnbd_display %>%
      mutate(
        avg_p_alive = round(avg_p_alive, 3),
        avg_predicted_purchases = round(avg_predicted_purchases, 2),
        total_predicted_purchases = format(total_predicted_purchases, big.mark = ",", scientific = FALSE)
      )
    
    datatable(
      df,
      rownames = FALSE,
      options = list(pageLength = 10, scrollX = TRUE)
    )
  })
  
  output$clv_table <- renderDT({
    df <- clv_display %>%
      mutate(
        avg_CLV = fmt_eur(avg_CLV),
        median_CLV = fmt_eur(median_CLV),
        total_CLV = fmt_eur(total_CLV)
      )
    
    datatable(
      df,
      rownames = FALSE,
      options = list(pageLength = 10, scrollX = TRUE)
    )
  })
  
  output$risk_table <- renderDT({
    df <- risk_display %>%
      mutate(
        total_revenue = fmt_eur(total_revenue),
        revenue_at_risk = fmt_eur(revenue_at_risk),
        pct_at_risk = paste0(pct_at_risk, "%"),
        avg_churn_prob = round(avg_churn_prob, 3)
      )
    
    datatable(
      df,
      rownames = FALSE,
      options = list(pageLength = 10, scrollX = TRUE)
    )
  })
  
  output$acq_channel_plot <- renderPlot({
    ggplot(clv_by_acquisition_channel, aes(x = reorder(FirstChannel, avg_monetary), y = avg_monetary)) +
      geom_col(fill = "#76B7B2") +
      coord_flip() +
      labs(
        title = "Average Customer Spend by Acquisition Channel",
        x = NULL,
        y = "Average total spend (€)"
      ) +
      theme_minimal(base_size = 13)
  })
  
  output$migration_heatmap_plot <- renderPlot({
    ggplot(channel_migration, aes(x = LastChannel, y = FirstChannel, fill = n)) +
      geom_tile(color = "white") +
      geom_text(aes(label = comma(n)), size = 3) +
      scale_fill_gradient(low = "white", high = "#4C78A8") +
      labs(
        title = "Channel Migration: First -> Last Channel",
        x = "Last channel",
        y = "First channel",
        fill = "Customers"
      ) +
      theme_minimal(base_size = 12) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  output$channel_table <- renderDT({
    df <- channel_summary %>%
      mutate(
        total_revenue = fmt_eur(total_revenue),
        avg_order_value = fmt_eur(avg_order_value)
      )
    
    datatable(
      df,
      rownames = FALSE,
      options = list(pageLength = 10, scrollX = TRUE)
    )
  })
  
  output$migration_table <- renderDT({
    datatable(
      channel_migration,
      rownames = FALSE,
      options = list(pageLength = 15, scrollX = TRUE)
    )
  })
  
  output$power_table <- renderDT({
    datatable(
      power_analysis_summary,
      rownames = FALSE,
      options = list(dom = "t", paging = FALSE, scrollX = TRUE)
    )
  })
}

shinyApp(ui = ui, server = server)