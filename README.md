# spare-ribs-customer-churn

[![R 4.3+](https://img.shields.io/badge/R-4.3+-276DC3.svg?logo=r&logoColor=white)](https://www.r-project.org/)
[![Shiny](https://img.shields.io/badge/Shiny-Interactive%20Dashboard-blue?logo=rstudio&logoColor=white)](https://shiny.posit.co/)
[![BTYD Modeling](https://img.shields.io/badge/Model-BG%2FNBD%20%26%20Gamma--Gamma-success.svg)](https://cran.r-project.org/web/packages/BTYD/index.html)
[![Machine Learning](https://img.shields.io/badge/ML-Logistic%20Regression%20%7C%20K--Means-orange.svg)](https://github.com/VitorKupidlowsky/spare-ribs-customer-churn)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)

**spare-ribs-customer-churn** is an enterprise-grade customer analytics and predictive retention framework developed for **Spare Rib Express (SRE)**—the leading Dutch gourmet food delivery franchise operating across 82 franchise locations.

This repository hosts the computational codebase and strategic customer analysis for the project:
> **"From Ribs to Relationships: A Customer Analytics Strategy for Sustainable Growth"**  
> *Course:* Customer Analytics (BM05BAM) — M.Sc. Business Analytics & Management (BAM)  
> *Institution:* Rotterdam School of Management, Erasmus University (March 2026)  
> *Authors (Group 11):* Vitor Medeiros Kupidlowsky Fernandes, Lisa Jürgens, Shinyoung Kim, Ole Kruse, Valentine Lusson, Stephanie Price  
> *Industry Partners:* Spare Rib Express & Rockfeather

---

## Table of Contents

1. [Project Overview & Core Insights](#project-overview--core-insights)
2. [Data Sources & Hygiene Pipeline](#data-sources--hygiene-pipeline)
3. [Customer Segmentation (6-Segment RFM Matrix)](#customer-segmentation-6-segment-rfm-matrix)
4. [Predictive Modeling (BG/NBD & Gamma-Gamma CLV)](#predictive-modeling-bgnbd--gamma-gamma-clv)
5. [Churn Prediction & Revenue at Risk](#churn-prediction--revenue-at-risk)
6. [Channel Economics & Migration Dynamics](#channel-economics--migration-dynamics)
7. [Retention Strategy & A/B Testing Roadmap](#retention-strategy--ab-testing-roadmap)
8. [Interactive Shiny Dashboard](#interactive-shiny-dashboard)
9. [Repository Structure](#repository-structure)
10. [Installation & Setup](#installation--setup)
11. [Pipeline Execution](#pipeline-execution)
12. [Academic Citation](#academic-citation)
13. [License & Contact](#license--contact)

---

## Project Overview & Core Insights

Between September 2023 and August 2024, Spare Rib Express scaled monthly order volume by **23×** (expanding from ~4,500 to a peak of ~105,000 monthly orders). However, post-peak volume stabilized around 75,000–85,000 monthly orders, exposing severe structural retention challenges:

1. **The One-Time Buyer Cliff:** **62.6%** of all acquired customers order exactly once and never return. Only **37.4%** reach a second order, and only **~17%** reorder within the initial 30 days.
2. **Cohort Retention Decay:** Rapid acquisition diluted customer quality; late cohorts (post-July 2024) retain at **8.8%** at Month 1 and **5.0%** at Month 12, representing a **44% to 52% decline** in cohort retention quality compared to early cohorts (15.7% at Month 1, 9.0% at Month 12).
3. **Extreme Value Concentration (Pareto & Gini):** The top **20%** of customers generate **55.7%** of cumulative revenue (**Gini coefficient = 0.487**). **Champions** (22% of customer base) account for **48%** of historical revenue and **67%** of projected future order volume (88% of projected repeat CLV).
4. **The €46.8M Revenue at Risk:** **54.4%** of total historical gross revenue is at risk of churning. **87%** of customers in the *At Risk* segment will churn without targeted reactivation. Protecting just 10% of at-risk revenue preserves **€4.0M–€5.0M**.
5. **Aggregator Commission Trap vs. Owned Channels:** While third-party aggregators (*TakeAway.com*) capture **67.7%** of total orders, proprietary channels (*Direct Website* and *ConsumerApp*) yield **+8.7% higher Average Order Value (€47.06 vs. €43.30)**, significantly higher lifetime value (€107+ vs. €84), and nearly double the repeat rate (**45.9% vs. 25.5%** on UberEats), all while bypassing 20–30% platform commissions.

---

## Data Sources & Hygiene Pipeline

The workflow ingests `group_assignment_2026.csv` (1.94M transaction logs spanning 82 franchise locations across the Netherlands) via `scripts/01_data_cleaning.R`:

- **Data Hygiene:** Standardized alphanumeric customer identifiers, postal codes, store franchise naming conventions, and transaction channel metadata. Removed exact duplicate records and filtered corrupted transactions.
- **Temporal Alignment:** Parsed non-standard timestamps into local Western European Time (`Europe/Amsterdam`), deriving cyclical features (order hour, day of week, seasonal month). Peak order volume occurs on weekends (Fri–Sun: 57.6% of volume) and between 17:00 and 20:00 (90.6% of daily orders).
- **Sequence Engineering:** Sorted transactions chronologically per customer to compute:
  - First-order flags ($IsFirstOrder \in \{0, 1\}$).
  - Inter-purchase intervals ($\Delta t$ days since prior purchase; median: 43 days, mean: 80.8 days).
  - Cumulative historical frequency ($x$) and total monetary spend ($M$).
- **Customer-Level Aggregation:** Constructed unified customer profiles capturing recency ($t_x$), customer age / tenure ($T$), channel variety ($\text{NumChannels}$), multi-store exploration ($\text{NumStores}$), and preferred acquisition channel.

Cleaned artifacts are serialized as `.rds` binaries for zero-overhead vector loading across downstream analytics modules.

---

## Customer Segmentation (6-Segment RFM Matrix)

Using recency, frequency, and monetary metrics, customers were classified into six strategically actionable segments (validated via unsupervised **K-Means clustering**):

| Segment | Customer Count | % Cust | % Hist. Rev | Avg. Spend | Churn Risk | Strategic Priority |
| :--- | :---: | :---: | :---: | :---: | :---: | :---: |
| **Champions** | **194,563** | 22.2% | **48.0%** | **€214.20** | 34% | **Urgent High**: VIP tiers, zero-click reordering, retention incentives |
| **Loyal Customers** | 157,500 | 18.0% | 15.0% | €94.10 | 60% | **Medium**: Habit reinforcement, upsell cross-category sides |
| **New Customers** | 105,000 | 12.0% | 8.0% | €50.40 | 39% | **High**: Immediate second-order onboarding within 43 days |
| **Potential Loyalists** | 70,000 | 8.0% | 5.0% | €49.20 | 83% | **Medium**: Multi-store nudges, personalized menu suggestions |
| **At High Risk** | 175,000 | 20.0% | 13.0% | €75.30 | **87%** | **Critical**: High-margin reactivation vouchers, win-back sequence |
| **Lost** | 172,937 | 19.8% | 11.0% | €46.10 | **98%** | **Low/Automated**: Low-cost programmatic email clearance |

> **Key Takeaway:** Every €1 invested in protecting a Champion produces **12× higher return** than re-acquiring or salvaging a lost customer (€87.88 vs. €7.40 avg. projected CLV).

### K-Means Cluster Validation ($k = 5$, Silhouette = 0.383)
Unsupervised clustering confirmed the underlying behavioral structure:
- **Cluster 2 (VIP Power Buyers, 8.0%):** Avg. 9.9 orders, €421 spend, 130 days recency.
- **Cluster 1 (Active Regulars, 15.8%):** Avg. 3.1 orders, €143 spend, 131 days recency.
- **Cluster 5 (Lapsing Repeaters, 14.6%):** Avg. 2.1 orders, €104 spend, 447 days recency.
- **Cluster 3 (Recent One-Timers, 29.1%):** Avg. 1.1 orders, €48 spend, 172 days recency.
- **Cluster 4 (Gone & Forgotten, 32.5%):** Avg. 1.0 order, €40 spend, 543 days recency.

---

## Predictive Modeling (BG/NBD & Gamma-Gamma CLV)

Implemented using the `BTYD` package to project expected transactional frequency and financial value over a **52-week forward horizon** ($327,183$ repeat customers modeled):

### 1. BG/NBD Latent Transaction Model
Accounts for transaction rates while active following a Poisson process ($\lambda$) and unobserved churn probability following a Pareto/Geometric process ($\mu$):

$$\mathbb{E}[X(t) \mid x, t_x, T, r, \alpha, a, b]$$

Computes the individual likelihood of remaining active ($P(\text{Alive})$) as a function of recency ($t_x$), frequency ($x$), and observation window ($T$). Across the customer base:
- **Average $P(\text{Alive})$:** 84.2%
- **$P(\text{Alive}) > 0.9$:** 562,190 customers (64.2%)
- **$P(\text{Alive}) < 0.5$ (likely inactive):** 113,655 customers (13.0%)
- **Total predicted orders (next 12 months):** 568,704 orders

### 2. Gamma-Gamma Spend Model
Assumes individual customer transaction values vary randomly around an individual average monetary value $\mathbb{E}[M]$, distributed according to a Gamma-Gamma conjugate structure independent of transaction frequency (correlation between frequency and monetary spend $r = -0.072$, confirming independence):

$$\text{CLV}_i = \mathbb{E}[X_i(\tau)] \times \mathbb{E}[M_i]$$

### 3. Forward 12-Month CLV Projections by Segment

| Segment | Avg. $P(\text{Alive})$ | Avg. 12M CLV | Median CLV | Total Projected CLV | Share of Projected CLV |
| :--- | :---: | :---: | :---: | :---: | :---: |
| **Champions** | **0.740** | **€87.88** | **€60.13** | **€16.5M** | **88.2%** |
| **Loyal Customers** | 0.442 | €22.08 | €19.76 | €1.5M | 8.0% |
| **At Risk** | 0.279 | €9.21 | €8.32 | €0.7M | 3.8% |
| **Total Repeat Base** | — | — | — | **€18.7M** | **100.0%** |

*Top individual customer projected 1-year CLV:* **€5,373** (25 historical repeat purchases, €503 average order value).

---

## Churn Prediction & Revenue at Risk

Rather than imposing arbitrary static churn thresholds (e.g., 30 or 60 days), a data-driven threshold was derived from the **75th percentile of observed inter-purchase intervals** ($\approx 160$ days). Under this threshold, the empirical churn rate is **70.7%**.

A binomial **Logistic Regression Model** (`glm(..., family = binomial)`) was trained across customer behavioral metrics, achieving an overall classification accuracy of **89.2%** and an **F1-score of 0.925**:

$$\text{logit}(P(\text{Churn})) = \beta_0 + \beta_1 \text{Freq} + \beta_2 \text{AOV} + \beta_3 \text{Tenure} + \beta_4 \text{NumStores} + \beta_5 \text{NumChannels} + \beta_6 \Delta t_{\text{avg}}$$

### Empirical Feature Impact (Odds Ratios)

```text
Multi-Store Ordering (2+ Stores)  ───► [OR: 0.470]  53% Churn Odds Reduction (p < 0.001)
Multi-Channel Usage (App + Web)   ───► [OR: 0.528]  47% Churn Odds Reduction (p < 0.001)
Order Frequency                   ───► [OR: 0.589]  41% Churn Odds Reduction per order (p < 0.001)
Average Order Value (AOV)         ───► [OR: 0.997]  Marginal Protective Effect (p < 0.001)
Inter-Purchase Interval Gap       ───► [OR: 1.002]  0.2% Churn Odds Increase per Inactive Day (p < 0.001)
Customer Tenure (Calendar Age)    ───► [OR: 1.011]  1.1% Churn Odds Increase per Month (Tenure Paradox, p < 0.001)
```

> **The Behavioral Insight:** **The Tenure Paradox** reveals that tenure alone does not indicate loyalty—an old customer with widening purchase intervals has very high churn probability. Conversely, **engagement breadth (ordering across multiple stores and channels) is the single strongest protective factor against churn.**

### Revenue at Risk Quantification
Multiplying each customer’s historical spend by their model-predicted churn probability:

$$\text{Revenue at Risk} = \sum_{i=1}^N \text{Spend}_i \times \hat{P}(\text{Churn}_i) = \mathbf{€46.8M} \quad (54.4\% \text{ of Gross Revenue})$$

| Customer Segment | Historical Revenue | Model-Predicted Revenue at Risk | % of Segment Revenue at Risk |
| :--- | :---: | :---: | :---: |
| **Champions** | €41.7M | €14.0M | 33.6% |
| **At Risk** | €13.0M | €11.3M | **87.0%** |
| **Loyal Customers** | €14.7M | €8.8M | 59.5% |
| **Lost** | €8.1M | €7.9M | **97.7%** |
| **Potential Loyalists** | €3.5M | €2.9M | 83.0% |
| **New Customers** | €5.1M | €2.0M | 39.0% |
| **Total** | **€86.0M** | **€46.8M** | **54.4%** |

---

## Channel Economics & Migration Dynamics

A critical strategic finding is the stark contrast between third-party volume aggregators and high-retention direct channels:

### 1. Channel Order Volume vs. Customer Quality

| Acquisition Channel | % of Orders | Avg. Order Value (AOV) | 30-Day Repeat Rate | Long-Term Repeat Rate | Customer Lifetime Spend | Platform Commission |
| :--- | :---: | :---: | :---: | :---: | :---: | :---: |
| **TakeAway.com** | **67.7%** | €43.30 | ~16% | 37.4% | €84.10 | 20% – 30% |
| **Direct Website** | **28.0%** | **€47.06** (+8.7%) | ~21% | 42.8% | **€107.40** | **0% (Owned)** |
| **Consumer App** | **1.3%** | **€45.08** | ~24% | **45.9%** | **€108.20** | **0% (Owned)** |
| **UberEats** | 1.4% | €39.36 | ~11% | 25.5% | €61.30 | 25% – 30% |
| **Intake Module / Phone** | 1.5% | €39.68 | ~18% | 34.1% | €74.50 | 0% |
| **S4D Cash Register** | <0.1% | €36.50 | — | — | — | In-store POS |

> **Key Economic Takeaway:** While TakeAway provides initial reach, proprietary channels (*Website* and *ConsumerApp*) attract customers who spend **+€3.76 higher AOV**, exhibit nearly **double the repeat purchase rate** compared to UberEats (45.9% vs. 25.5%), and generate zero commission leakage.

### 2. First-to-Last Channel Migration Matrix

Tracking customer channel touchpoints across 327,183 repeat customers demonstrates high platform stickiness:

| First Touchpoint | Repeat Cohort ($n$) | Retained on Channel (%) | Migrated to Other Channels (%) | Primary Destination |
| :--- | :---: | :---: | :---: | :--- |
| **TakeAway** | 227,697 | **90.3%** | 9.7% | Direct Website (8.1%) |
| **Direct Website** | 88,351 | **81.2%** | 18.8% | TakeAway (16.2%) |
| **Intake Module** | 3,602 | **65.9%** | 34.1% | Direct Website (22.4%) |
| **Consumer App** | 3,018 | **65.3%** | 34.7% | Direct Website (24.1%) |
| **UberEats** | 4,516 | **62.6%** | 37.4% | TakeAway (28.3%) |

### 3. Channel Strategy Implications
- **Aggregator Lock-in:** Customers acquired through TakeAway overwhelmingly stay on TakeAway (90.3% stickiness), locking the franchise into perpetual 20–30% commission payouts.
- **The Owned-Channel Flywheel:** Direct channels generate superior customer unit economics. The strategic mandate is to utilize third-party aggregators purely as top-of-funnel customer acquisition vehicles while actively steering subsequent repeat orders to the owned Website and App.

---

## Retention Strategy & A/B Testing Roadmap

To address customer churn scientifically, three randomized control trials (RCTs) are designed with rigorous statistical power calculations ($\alpha = 0.05, 1-\beta = 0.80$):

| Experiment | Target Population | Core Intervention | Primary Metric & Baseline | Required $n$ / Group | Feasibility Status |
| :--- | :--- | :--- | :--- | :---: | :--- |
| **1. The "Second-Bite" Onboarding Sequence** | New Customers ($N \approx 105\text{K}$) | Personalized automated SMS/in-app nudges at Day 14 and Day 28 post-first-order | 60-day repeat rate (Baseline: 37.4% $\rightarrow$ Target: 45.0%) | **1,223** | Highly Feasible |
| **2. Aggregator-to-Owned App Migration** | TakeAway / UberEats Orders | In-box physical voucher insert offering 15% off first two proprietary App orders | App order share & Net Contribution Margin | **1,189** | Highly Feasible (~1 week of orders) |
| **3. At-Risk Reactivation with Uplift Modeling** | At High Risk ($N \approx 175\text{K}$) | Tiered margin reactivation vouchers (free dessert/side vs. €5 credit) | 30-day reactivation rate (Baseline: 5% $\rightarrow$ Target: 10%) | **334** | Very High Feasibility (173K+ available) |

> **Uplift Modeling Recommendation:** In line with the Artea experimentation framework, Spare Rib Express should deploy 4-quadrant Uplift Modeling (*Persuadables*, *Sure Things*, *Lost Causes*, *Sleeping Dogs*) to avoid sending discounts to customers who would purchase anyway or alienating dormant customers.

---

## Interactive Shiny Dashboard

The repository includes a production-ready Shiny dashboard (`shiny_app/app.R`) enabling regional managers, franchise owners, and C-level stakeholders to explore the data dynamically:

- **Executive KPI Cockpit:** Filter order trends, gross margin, and active customer counts by store (82 locations) and province.
- **RFM Interactive Segmentation Grid:** Dynamic bubble charts exploring customer migration between cohorts.
- **CLV & Churn Simulator:** Parameterized sliders to forecast store-level revenue gains from hypothetical 5%, 10%, or 15% reductions in churn probability.
- **Store-Level League Tables:** Benchmark all 82 franchise locations across AOV, repeat purchase rates, and customer quality indices.

```bash
# Launch dashboard locally from R / RStudio
Rscript -e "shiny::runApp('shiny_app', port = 7860)"
```

---

## Repository Structure

```text
spare-ribs-customer-churn/
├── data/
│   ├── raw/                         # Raw transactional data (group_assignment_2026.csv - git-ignored)
│   ├── processed/                   # Cleaned order- and customer-level tables (.rds and .csv)
│   └── samples/                     # Synthetic / lightweight sample data for immediate reproduction
│       └── sample_orders_5k.csv
├── scripts/
│   ├── 01_data_cleaning.R           # Standardization, timestamp parsing, sequence feature engineering
│   ├── 02_exploratory_analytics.R   # Descriptive distributions, channel breakdowns, cohort analysis
│   ├── 03_rfm_segmentation.R        # Quintile scoring, 6-segment matrix, K-Means cluster validation
│   ├── 04_predictive_clv_btyd.R     # BG/NBD transaction & Gamma-Gamma spend modeling (BTYD)
│   ├── 05_churn_logistic_reg.R      # Dynamic churn labeling, binomial GLM, Odds Ratios, Revenue at Risk
│   └── 06_ab_test_power_calc.R      # Sample size & statistical power calculations for field experiments
├── shiny_app/
│   ├── app.R                        # Full interactive Shiny web application
│   ├── ui.R                         # UI layout: RFM explorer, CLV simulator, store benchmarking
│   ├── server.R                     # Reactive data filtering, model inference, and ggplot2 visualizers
│   └── www/                         # Custom styling, logos, and report assets
├── outputs/
│   ├── figures/                     # High-resolution generated charts (PNG, 300 DPI)
│   │   ├── 01_orders_by_channel.png
│   │   ├── 02_new_vs_returning_cohorts.png
│   │   ├── 03_purchase_frequency_distribution.png
│   │   ├── 04_rfm_segment_value_map.png
│   │   ├── 05_lorenz_curve_concentration.png
│   │   ├── 06_bgnbd_gamma_clv_distribution.png
│   │   ├── 07_churn_odds_ratios_importance.png
│   │   ├── 08_revenue_at_risk_by_segment.png
│   │   └── 09_channel_migration_heatmap.png
│   └── tables/                      # Segment aggregates, store performance summaries, model metrics
├── .gitignore
├── .Rprofile
├── LICENSE
├── README.md
└── requirements_R.txt               # R package dependency manifest
```

---

## Installation & Setup

### 1. Prerequisites
- **R (version 4.3.0 or higher)**
- **RStudio** (recommended)

### 2. Clone Repository
```bash
git clone [https://github.com/VitorKupidlowsky/spare-ribs-customer-churn.git](https://github.com/VitorKupidlowsky/spare-ribs-customer-churn.git)
cd spare-ribs-customer-churn
```

### 3. Install Required R Packages
```R
install.packages(c(
  "tidyverse",   # ggplot2, dplyr, readr, purrr, tidyr
  "lubridate",   # Datetime handling
  "BTYD",        # Buy 'Til You Die modeling (BG/NBD, Gamma-Gamma)
  "scales",      # Formatting scales & currencies
  "gridExtra",   # Multi-panel visualization layouts
  "cluster",     # K-Means segmentation
  "shiny",       # Interactive web application
  "shinydashboard",
  "DT"           # Interactive tabular views
))
```

### 4. Dataset Access & Large File Handling
Due to GitHub's **100 MB hard file-size limit**, the raw transaction dataset (`group_assignment_2026.csv`, ~150 MB uncompressed, >1.5M records) is not tracked in Git:
- **Option A (GitHub Releases):** Download `group_assignment_2026.csv.zip` from repository Releases and unzip into `data/raw/`.
- **Option B (Automated Download):** Run `Rscript scripts/download_dataset.R` to download from the project mirror.
- **Option C (Quickstart Sample):** The repository includes a verified 5,000-order sample in `data/samples/sample_orders_5k.csv`. All scripts automatically fall back to the sample dataset if the full raw file is absent.

---

## Pipeline Execution

Execute the analytics pipeline in sequential order:

```bash
# Step 1: Clean raw transactions and generate feature-engineered datasets
Rscript scripts/01_data_cleaning.R

# Step 2: Generate descriptive distributions and cohort visualizations
Rscript scripts/02_exploratory_analytics.R

# Step 3: Run RFM segmentation and K-Means validation
Rscript scripts/03_rfm_segmentation.R

# Step 4: Fit stochastic BG/NBD and Gamma-Gamma CLV models
Rscript scripts/04_predictive_clv_btyd.R

# Step 5: Train logistic regression churn classifier and calculate Revenue at Risk
Rscript scripts/05_churn_logistic_reg.R

# Step 6: Run power calculations for proposed A/B testing interventions
Rscript scripts/06_ab_test_power_calc.R

# Step 7: Launch the interactive Shiny dashboard
Rscript -e "shiny::runApp('shiny_app', port = 7860)"
```

---

## Academic Citation

If you utilize this workflow, methodology, or modeling architecture in your research, please cite:

```bibtex
@misc{kupidlowsky2026spareribs,
  author       = {Kupidlowsky, Vitor and J{\"u}rgens, Lisa and Kim, Shinyoung and Kruse, Ole and Lusson, Valentine and Price, Stephanie},
  title        = {Customer Analytics Case Report: Predictive Retention, CLV and Churn Modeling for Spare Rib Express},
  year         = {2026},
  month        = {March},
  institution  = {Rotterdam School of Management, Erasmus University},
  howpublished = {GitHub Repository},
  url          = {[https://github.com/VitorKupidlowsky/spare-ribs-customer-churn](https://github.com/VitorKupidlowsky/spare-ribs-customer-churn)}
}
```

---

## License & Contact

- **Author:** Vitor Medeiros Kupidlowsky Fernandes
- **GitHub:** [@VitorKupidlowsky](https://github.com/VitorKupidlowsky)
- **Institution:** Rotterdam School of Management, Erasmus University
- **License:** Released under the [MIT License](LICENSE).
