############################################################
## 0. Libraries
############################################################

library(ggplot2)
library(patchwork)
library(dplyr)
library(pwr)
library(boot)

############################################################
## 1. Data import and preparation
############################################################

df <- read.csv("~/Downloads/ab_testing.csv")  # adjust path if needed

df$Group      <- as.factor(df$Group)
df$Conversion <- as.factor(df$Conversion)
df$Location   <- as.factor(df$Location)

############################################################
## 2. Exploratory visualisations and summaries
############################################################

### 2.1 Page Views distribution
p1 <- ggplot(df, aes(x = Page.Views)) +
  geom_histogram(bins = 30, fill = "lightgreen", color = "black", alpha = 0.7) +
  facet_wrap(~ Group, ncol = 1) +
  labs(
    title = "Page Views Distribution by Group",
    x     = "Page Views",
    y     = "Count"
  )

### 2.2 Time Spent distribution
p2 <- ggplot(df, aes(x = Time.Spent)) +
  geom_histogram(bins = 30, fill = "lightblue", color = "black", alpha = 0.7) +
  facet_wrap(~ Group, ncol = 1) +
  labs(
    title = "Time Spent Distribution by Group",
    x     = "Time Spent (seconds)",
    y     = "Count"
  )

### 2.3 Conversion distribution by group
conv_counts <- as.data.frame(table(df$Group, df$Conversion))
colnames(conv_counts) <- c("Group", "Conversion", "Count")

p3 <- ggplot(conv_counts, aes(x = "", y = Count, fill = Conversion)) +
  geom_bar(stat = "identity", width = 1, color = "black") +
  coord_polar("y", start = 0) +
  facet_wrap(~ Group) +
  labs(title = "Conversion Distribution by Group") +
  scale_fill_manual(values = c("No" = "grey", "Yes" = "cyan"))

### 2.4 Location distribution by group
p4 <- ggplot(df, aes(x = Location)) +
  geom_bar(fill = "coral", color = "black") +
  facet_wrap(~ Group, ncol = 1) +
  coord_flip() +
  labs(
    title = "User Location Distribution by Group",
    x     = "Location",
    y     = "Count"
  )

### 2.5 Combined EDA plot
(p1 | p2) / (p3 | p4)

### 2.6 Numerical summaries
## Five-number summary for Page Views and Time Spent
page_summary <- aggregate(Page.Views ~ Group, df, summary)
time_summary <- aggregate(Time.Spent ~ Group, df, summary)

print(page_summary)
print(time_summary)

## Average Time Spent per group
avg_time <- aggregate(Time.Spent ~ Group, df, mean)
colnames(avg_time)[2] <- "Average_Time_Spent"
print(avg_time)

## Proportion of Conversion per group (Yes = 1, No = 0)
df$Conversion_Binary <- ifelse(df$Conversion == "Yes", 1, 0)

prop_conversion <- aggregate(Conversion_Binary ~ Group, df, mean)
colnames(prop_conversion)[2] <- "Proportion_Conversion"
print(prop_conversion)

############################################################
## 3. Hypothesis tests (classical)
############################################################

### 3.1 Page.Views – chi-square using categorical bins
df$Page.Views.Category <- cut(
  df$Page.Views,
  breaks        = quantile(df$Page.Views, probs = seq(0, 1, 0.25), na.rm = TRUE),
  include.lowest = TRUE,
  labels        = c("Low", "Medium", "High", "Very High")
)

table_views <- table(df$Group, df$Page.Views.Category)
print(table_views)

chi_pageviews <- chisq.test(table_views)
print(chi_pageviews)

### 3.2 Conversion – chi-square
table_conv     <- table(df$Group, df$Conversion)
print(table_conv)
chi_conversion <- chisq.test(table_conv)
print(chi_conversion)

### 3.3 Time.Spent – two-sample Welch t-test
t_time <- t.test(Time.Spent ~ Group, data = df, var.equal = FALSE)
print(t_time)

### 3.4 Conversion – two-proportion test (A vs B)
success_A <- table_conv["A", "Yes"]
success_B <- table_conv["B", "Yes"]

n_A <- sum(table_conv["A", ])
n_B <- sum(table_conv["B", ])

prop_test <- prop.test(
  x       = c(success_A, success_B),
  n       = c(n_A, n_B),
  correct = TRUE
)
print(prop_test)

### 3.5 Wald tests via regression models

## Logistic GLM (Conversion)
m_conv <- glm(Conversion_Binary ~ Group, data = df, family = binomial())
summary(m_conv)

## Linear Model (Time Spent)
m_time <- lm(Time.Spent ~ Group, data = df)
summary(m_time)

## Quasi-Poisson GLM (Page Views)
m_views_quasi <- glm(Page.Views ~ Group, data = df, family = quasipoisson())
summary(m_views_quasi)

## Poisson GLM (Page Views)
m_views_pois <- glm(Page.Views ~ Group, data = df, family = poisson())
summary(m_views_pois)

############################################################
## 4. Assumption checks
############################################################

### 4.1 Time.Spent – normality
## Shapiro–Wilk per group
shapiro_A_time <- shapiro.test(df$Time.Spent[df$Group == "A"])
shapiro_B_time <- shapiro.test(df$Time.Spent[df$Group == "B"])

print(shapiro_A_time)
print(shapiro_B_time)

## QQ plots for raw and log-transformed Time.Spent
par(mfrow = c(2, 2))

qqnorm(
  df$Time.Spent[df$Group == "A"],
  main = "QQ Plot - Time.Spent (Group A)",
  pch  = 16,
  col  = "darkblue"
)
qqline(df$Time.Spent[df$Group == "A"], col = "red", lwd = 2)

qqnorm(
  df$Time.Spent[df$Group == "B"],
  main = "QQ Plot - Time.Spent (Group B)",
  pch  = 16,
  col  = "darkblue"
)
qqline(df$Time.Spent[df$Group == "B"], col = "red", lwd = 2)

df$log_Time.Spent <- log(df$Time.Spent + 1)

qqnorm(
  df$log_Time.Spent[df$Group == "A"],
  main = "QQ Plot - log(Time.Spent) (Group A)",
  pch  = 16,
  col  = "darkgreen"
)
qqline(df$log_Time.Spent[df$Group == "A"], col = "red", lwd = 2)

qqnorm(
  df$log_Time.Spent[df$Group == "B"],
  main = "QQ Plot - log(Time.Spent) (Group B)",
  pch  = 16,
  col  = "darkgreen"
)
qqline(df$log_Time.Spent[df$Group == "B"], col = "red", lwd = 2)

par(mfrow = c(1, 1))

### 4.2 Conversion – cell counts and events
chi_conversion <- chisq.test(table_conv, correct = TRUE)

chi_conversion$expected
min(chi_conversion$expected)

tapply(df$Conversion_Binary, df$Group, mean)

### 4.3 Page.Views – overdispersion check
overdisp_factor <- sum(residuals(m_views_quasi, type = "pearson")^2) /
  df.residual(m_views_quasi)
overdisp_factor

############################################################
## 5. Analytic power analysis
############################################################

### 5.1 Effect sizes
## Time.Spent – Cohen's d (two-sample t-test)
mean_A <- mean(df$Time.Spent[df$Group == "A"])
mean_B <- mean(df$Time.Spent[df$Group == "B"])

sd_pooled <- sqrt(
  ((n_A - 1) * var(df$Time.Spent[df$Group == "A"]) +
     (n_B - 1) * var(df$Time.Spent[df$Group == "B"])) /
    (n_A + n_B - 2)
)

d <- abs(mean_A - mean_B) / sd_pooled

## Time.Spent (linear model) – f² from Wald t
lm_time_summary <- summary(m_time)   
t_time_wald          <- lm_time_summary$coefficients["GroupB", "t value"]

f2_time <- (t_time_wald^2) / (t_time_wald^2 + nrow(df))

## Conversion – Cohen's h
eps <- 1e-12
p1  <- success_A / n_A
p1  <- pmin(pmax(p1, eps), 1 - eps)

p2  <- success_B / n_B
p2  <- pmin(pmax(p2, eps), 1 - eps)

h <- abs(ES.h(p1, p2))

## Conversion (logistic) – f² approximation from z
model_summary <- summary(m_conv)
z_value       <- model_summary$coefficients[2, 3]
f2_logit      <- (z_value)^2 / ((z_value)^2 + nrow(df))

## Page.Views (quartiles) – Cohen's w
w <- sqrt(
  sum((chi_pageviews$observed - chi_pageviews$expected)^2 / chi_pageviews$expected) / 
    sum(chi_pageviews$expected)
)

## 5.1.5 Page.Views – quasi-Poisson GLM – f² from Wald t
quasi_summary <- summary(m_views_quasi)
t_quasi       <- quasi_summary$coefficients["GroupB", "t value"]

f2_quasi <- (t_quasi^2) / (t_quasi^2 + nrow(df))

## 5.1.6 Page.Views – Poisson GLM – f² from Wald z
pois_summary <- summary(m_views_pois)
z_pois       <- pois_summary$coefficients["GroupB", "z value"]

f2_pois <- (z_pois^2) / (z_pois^2 + nrow(df))

cat("\nObserved effect sizes:\n")
cat("Time Spent – d (t-test):", d, "\n")
cat("Time Spent – f² (linear model):", f2_time, "\n")
cat("Conversion (proportion) – h:", h, "\n")
cat("Page Views (chi-square) – w:", w, "\n")
cat("Conversion (logistic) – f²:", f2_logit, "\n")
cat("Page Views (quasi-Poisson) – f²:", f2_quasi, "\n")
cat("Page Views (Poisson) – f²:", f2_pois, "\n\n")

### 5.2 Analytic power 
## 5.2.1 Time Spent – two-sample t-test (d)
power_t <- pwr.t2n.test(
  n1        = n_A,
  n2        = n_B,
  d         = d,
  sig.level = 0.05
)
print(power_t)

## 5.2.2 Conversion – two-proportion test (h)
power_prop <- pwr.2p2n.test(
  h         = h,
  n1        = n_A,
  n2        = n_B,
  sig.level = 0.05
)
print(power_prop)

## 5.2.3 Page.Views – chi-square
df_chi <- (nrow(table_views) - 1) * (ncol(table_views) - 1)

power_chi_pageviews <- pwr.chisq.test(
  w         = w,
  N         = sum(table_views),
  df        = df_chi,
  sig.level = 0.05
)
print(power_chi_pageviews)

## 5.2.4 Conversion – chi-square on 2x2 table
w_conv <- sqrt(
  sum((chi_conversion$observed - chi_conversion$expected)^2 / chi_conversion$expected) /
    sum(chi_conversion$expected)
)

df_conv <- (nrow(table_conv) - 1) * (ncol(table_conv) - 1)

power_chi_conversion <- pwr.chisq.test(
  w         = w_conv,
  N         = sum(table_conv),
  df        = df_conv,
  sig.level = 0.05
)
print(power_chi_conversion)

## 5.2.5 Conversion – logistic regression (f²)
power_logit <- pwr.f2.test(
  u         = 1,
  v         = nrow(df) - 2,
  f2        = f2_logit,
  sig.level = 0.05
)
print(power_logit)

## 5.2.6 Time Spent – linear regression with Wald f²
power_lm_time <- pwr.f2.test(
  u         = 1,                 
  v         = nrow(df) - 2,      
  f2        = f2_time,
  sig.level = 0.05
)
print(power_lm_time)

## 5.2.7 Page.Views – quasi-Poisson GLM (Wald t → f²)
power_quasi_views <- pwr.f2.test(
  u         = 1,
  v         = nrow(df) - 2,
  f2        = f2_quasi,
  sig.level = 0.05
)
print(power_quasi_views)

## 5.2.8 Page.Views – Poisson GLM (Wald z → f²)
power_pois_views <- pwr.f2.test(
  u         = 1,
  v         = nrow(df) - 2,
  f2        = f2_pois,
  sig.level = 0.05
)
print(power_pois_views)

### 5.3 Observed effect sizes for power plots
obs_d_time      <- d          
obs_f2_time     <- f2_time    
obs_h_conv      <- h
obs_w_views     <- w
obs_f2_logit    <- f2_logit
obs_f2_quasi    <- f2_quasi
obs_f2_pois     <- f2_pois

### 5.4 Effect size sequences for power curves
d_seq  <- seq(0, 0.25, length.out = 200)
h_seq  <- seq(0, 0.40, length.out = 200)
w_seq  <- seq(0, 0.15, length.out = 200)
f2_seq <- seq(0, 0.02, length.out = 200)

### 5.5 Power curves (per test)

## Time Spent – two-sample t (d)
df_t_curve <- data.frame(
  effect = d_seq,
  power  = sapply(
    d_seq,
    function(x) pwr.t2n.test(n1 = n_A, n2 = n_B, d = x, sig.level = 0.05)$power
  ),
  test   = "Two-sample t (d)",
  metric = "Time Spent (2 Sample Mean)"
)

## Time Spent – linear regression (f²)
df_lm_time_curve <- data.frame(
  effect = f2_seq,
  power  = sapply(
    f2_seq,
    function(x) pwr.f2.test(u = 1, v = nrow(df) - 2, f2 = x, sig.level = 0.05)$power
  ),
  test   = "LM/Wald (f²)",
  metric = "Time Spent (Linear regression)"
)

## Conversion – two-proportion (h)
df_prop_curve <- data.frame(
  effect = h_seq,
  power  = sapply(
    h_seq,
    function(x) pwr.2p2n.test(h = x, n1 = n_A, n2 = n_B, sig.level = 0.05)$power
  ),
  test   = "Two-prop (h)",
  metric = "Conversion (Proportion)"
)

## Conversion – logistic regression (f²)
df_f2_curve <- data.frame(
  effect = f2_seq,
  power  = sapply(
    f2_seq,
    function(x) pwr.f2.test(u = 1, v = nrow(df) - 2, f2 = x, sig.level = 0.05)$power
  ),
  test   = "F-test (f2)",
  metric = "Conversion (Logistic regression)"
)

## Page Views – chi-square (w)
df_chi_curve <- data.frame(
  effect = w_seq,
  power  = sapply(
    w_seq,
    function(x) pwr.chisq.test(
      w         = x,
      N         = (n_A + n_B),
      df        = (nrow(table_views) - 1) * (ncol(table_views) - 1),
      sig.level = 0.05
    )$power
  ),
  test   = "Chi-square (w)",
  metric = "Page Views (Chi^2)"
)

## Page Views – quasi-Poisson GLM (f²)
df_quasi_curve <- data.frame(
  effect = f2_seq,
  power  = sapply(
    f2_seq,
    function(x) pwr.f2.test(u = 1, v = nrow(df) - 2, f2 = x, sig.level = 0.05)$power
  ),
  test   = "Wald (f2)",
  metric = "Page Views (Quasi-Poisson GLM)"
)

## Page Views – Poisson GLM (f²)
df_pois_curve <- data.frame(
  effect = f2_seq,
  power  = sapply(
    f2_seq,
    function(x) pwr.f2.test(u = 1, v = nrow(df) - 2, f2 = x, sig.level = 0.05)$power
  ),
  test   = "Wald (f2)",
  metric = "Page Views (Poisson GLM)"
)

### 5.6 Combine power curves and add observed markers

plot_df <- bind_rows(
  df_t_curve,
  df_lm_time_curve,
  df_prop_curve,
  df_f2_curve,
  df_chi_curve,
  df_quasi_curve,
  df_pois_curve
)

metric_levels <- c(
  "Time Spent (2 Sample Mean)",
  "Time Spent (Linear regression)",
  "Conversion (Proportion)",
  "Conversion (Logistic regression)",
  "Page Views (Chi^2)",
  "Page Views (Quasi-Poisson GLM)",
  "Page Views (Poisson GLM)"
)

plot_df$metric <- factor(plot_df$metric, levels = metric_levels)

obs_markers <- data.frame(
  metric     = c(
    "Time Spent (2 Sample Mean)",
    "Time Spent (Linear regression)",
    "Conversion (Proportion)",
    "Conversion (Logistic regression)",
    "Page Views (Chi^2)",
    "Page Views (Quasi-Poisson GLM)",
    "Page Views (Poisson GLM)"
  ),
  obs_effect = c(
    obs_d_time,      
    obs_f2_time,     
    obs_h_conv,
    obs_f2_logit,
    obs_w_views,
    obs_f2_quasi,
    obs_f2_pois
  ),
  y          = 0.95,
  stringsAsFactors = FALSE
)

obs_markers$metric <- factor(obs_markers$metric, levels = metric_levels)

### 5.7 Power vs effect size plots 

## Helper function
make_power_plot <- function(plot_df_sub, obs_sub, panel_title) {
  ggplot(plot_df_sub, aes(x = effect, y = power)) +
    geom_line() +
    facet_wrap(~ metric, scales = "free_x", ncol = 2) +  
    geom_vline(
      data     = obs_sub,
      aes(xintercept = obs_effect),
      color    = "red",
      linetype = "dotted"
    ) +
    geom_label(
      data  = obs_sub,
      aes(
        x     = obs_effect,
        y     = y,
        label = paste0("obs = ", round(obs_effect, 5))
      ),
      color = "red",
      size  = 3,
      vjust = 1
    ) +
    labs(
      title = panel_title,
      x     = "Effect size (metric-specific)",
      y     = "Power"
    ) +
    theme_minimal()
}

## 5.7.1 Time Spent 
metrics_time <- c(
  "Time Spent (2 Sample Mean)",
  "Time Spent (Linear regression)"
)

plot_df_time  <- dplyr::filter(plot_df,  metric %in% metrics_time)
obs_time      <- dplyr::filter(obs_markers, metric %in% metrics_time)

p_power_time <- make_power_plot(
  plot_df_sub = plot_df_time,
  obs_sub     = obs_time,
  panel_title = "Power vs Effect Size – Time Spent"
)
print(p_power_time)

## 5.7.2 Conversion (2 facets in one panel)
metrics_conv <- c(
  "Conversion (Proportion)",
  "Conversion (Logistic regression)"
)

plot_df_conv <- dplyr::filter(plot_df,  metric %in% metrics_conv)
obs_conv     <- dplyr::filter(obs_markers, metric %in% metrics_conv)

p_power_conv <- make_power_plot(
  plot_df_sub = plot_df_conv,
  obs_sub     = obs_conv,
  panel_title = "Power vs Effect Size – Conversion"
)
print(p_power_conv)

## 5.7.3 Page Views (3 facets, arranged in up to 2 per row)
metrics_views <- c(
  "Page Views (Chi^2)",
  "Page Views (Quasi-Poisson GLM)"
  #  "Page Views (Poisson GLM)"
)

plot_df_views <- dplyr::filter(plot_df,  metric %in% metrics_views)
obs_views     <- dplyr::filter(obs_markers, metric %in% metrics_views)

p_power_views <- make_power_plot(
  plot_df_sub = plot_df_views,
  obs_sub     = obs_views,
  panel_title = "Power vs Effect Size – Page Views"
)
print(p_power_views)

############################################################
## 6. Bootstrap resampling (within-group)
############################################################

set.seed(123)
R_boot <- 10000

### 6.1 Helper to print bootstrap summary
print_boot_summary_abs <- function(name, obs_abs, boot_vec) {
  se    <- sd(boot_vec, na.rm = TRUE)
  ci    <- quantile(boot_vec, probs = c(0.025, 0.975), na.rm = TRUE)
  p_val <- mean(boot_vec >= abs(obs_abs), na.rm = TRUE)
  
  cat("-----", name, "-----\n")
  cat("Observed (abs):", signif(abs(obs_abs), 6), "\n")
  cat("Bootstrap SE:", signif(se, 6), "\n")
  cat(
    "Bootstrap 95% CI (percentile):",
    signif(ci[1], 6), "-",
    signif(ci[2], 6), "\n"
  )
  cat("Bootstrap p-value:", signif(p_val, 6), "\n\n")
  
  invisible(list(se = se, ci = ci, p = p_val))
}

### 6.2 Nonparametric bootstrap statistics (absolute differences)
boot_time_stat <- function(data, indices) {
  d  <- data[indices, , drop = FALSE]
  mA <- mean(d$Time.Spent[d$Group == "A"], na.rm = TRUE)
  mB <- mean(d$Time.Spent[d$Group == "B"], na.rm = TRUE)
  abs(mA - mB)
}

boot_views_stat <- function(data, indices) {
  d  <- data[indices, , drop = FALSE]
  mA <- mean(d$Page.Views[d$Group == "A"], na.rm = TRUE)
  mB <- mean(d$Page.Views[d$Group == "B"], na.rm = TRUE)
  abs(mA - mB)
}

boot_conv_stat <- function(data, indices) {
  d  <- data[indices, , drop = FALSE]
  pA <- mean(d$Conversion_Binary[d$Group == "A"], na.rm = TRUE)
  pB <- mean(d$Conversion_Binary[d$Group == "B"], na.rm = TRUE)
  abs(pA - pB)
}

### 6.3 Prepare data frames for nonparametric bootstrap
df_time  <- df[, c("Group", "Time.Spent")]
df_views <- df[, c("Group", "Page.Views")]
df_conv  <- df[, c("Group", "Conversion_Binary")]

## Ensure Group is a factor with the intended levels, just in case
df_time$Group  <- factor(df_time$Group)
df_views$Group <- factor(df_views$Group)
df_conv$Group  <- factor(df_conv$Group)

### 6.4 Run nonparametric bootstrap (within-group / stratified)
boot_time <- boot(
  data      = df_time,
  statistic = boot_time_stat,
  R         = R_boot,
  strata    = df_time$Group
)

boot_views <- boot(
  data      = df_views,
  statistic = boot_views_stat,
  R         = R_boot,
  strata    = df_views$Group
)

boot_conv <- boot(
  data      = df_conv,
  statistic = boot_conv_stat,
  R         = R_boot,
  strata    = df_conv$Group
)

### 6.5 Observed absolute statistics
obs_time_abs <- abs(
  mean(df$Time.Spent[df$Group == "A"], na.rm = TRUE) -
    mean(df$Time.Spent[df$Group == "B"], na.rm = TRUE)
)

obs_views_abs <- abs(
  mean(df$Page.Views[df$Group == "A"], na.rm = TRUE) -
    mean(df$Page.Views[df$Group == "B"], na.rm = TRUE)
)

obs_conv_abs <- abs(
  mean(df$Conversion_Binary[df$Group == "A"], na.rm = TRUE) -
    mean(df$Conversion_Binary[df$Group == "B"], na.rm = TRUE)
)

### 6.6 Print nonparametric bootstrap summaries
print_boot_summary_abs(
  "Time.Spent (abs mean difference)",
  obs_time_abs,
  boot_time$t
)

print_boot_summary_abs(
  "Page.Views (abs mean difference)",
  obs_views_abs,
  boot_views$t
)

print_boot_summary_abs(
  "Conversion (abs proportion difference)",
  obs_conv_abs,
  boot_conv$t
)

cat("Confidence intervals via boot.ci (percentile and BCa):\n")
cat("Time.Spent:\n")
print(boot.ci(boot_time,  type = c("perc", "bca")))
cat("\nPage.Views:\n")
print(boot.ci(boot_views, type = c("perc", "bca")))
cat("\nConversion:\n")
print(boot.ci(boot_conv,  type = c("perc", "bca")))

### 6.7 Nonparametric bootstrap histograms
par(mfrow = c(1, 3))

hist(
  boot_time$t,
  breaks = 60,
  main   = "Bootstrap: |Time mean A - B|",
  xlab   = "Abs mean diff (sec)",
  col    = "lightblue"
)
abline(v = obs_time_abs, col = "red", lwd = 2)

hist(
  boot_views$t,
  breaks = 60,
  main   = "Bootstrap: |PageViews mean A - B|",
  xlab   = "Abs mean diff",
  col    = "lightgreen"
)
abline(v = obs_views_abs, col = "red", lwd = 2)

hist(
  boot_conv$t,
  breaks = 60,
  main   = "Bootstrap: |Conv prop A - B|",
  xlab   = "Abs prop diff",
  col    = "pink"
)
abline(v = obs_conv_abs, col = "red", lwd = 2)

par(mfrow = c(1, 1))

### 6.8 Model-based bootstrap (absolute coefficients, within-group)
## Here we resample A from A and B from B separately, then refit the models.

coef_abs_time  <- numeric(R_boot)
coef_abs_views <- numeric(R_boot)
coef_abs_conv  <- numeric(R_boot)

## Make sure Group has levels c("A","B") and define coef_name
df$Group <- factor(df$Group, levels = c("A", "B"))
lvl       <- levels(df$Group)
coef_name <- paste0("Group", lvl[2])  # "GroupB"

## Indices for each group (reuse n_A and n_B defined earlier)
idx_A <- which(df$Group == "A")
idx_B <- which(df$Group == "B")

for (i in seq_len(R_boot)) {
  ## Within-group resampling
  samp_A   <- sample(idx_A, size = n_A, replace = TRUE)
  samp_B   <- sample(idx_B, size = n_B, replace = TRUE)
  samp_idx <- c(samp_A, samp_B)
  d_samp   <- df[samp_idx, , drop = FALSE]
  
  ## Time Spent – OLS
  mtime <- lm(Time.Spent ~ Group, data = d_samp)
  coef_abs_time[i] <- abs(coef(mtime)[coef_name])
  
  ## Page Views – quasi-Poisson GLM
  mviews <- glm(Page.Views ~ Group, data = d_samp, family = quasipoisson())
  coef_abs_views[i] <- abs(coef(mviews)[coef_name])
  
  ## Conversion – logistic regression
  mconv <- glm(Conversion_Binary ~ Group, data = d_samp, family = binomial())
  coef_abs_conv[i] <- abs(coef(mconv)[coef_name])
}

coef_abs_time_clean  <- coef_abs_time[!is.na(coef_abs_time)]
coef_abs_views_clean <- coef_abs_views[!is.na(coef_abs_views)]
coef_abs_conv_clean  <- coef_abs_conv[!is.na(coef_abs_conv)]

## Observed absolute coefficients from your fitted models m_time, m_views, m_conv
obs_coef_time_abs  <- abs(coef(m_time)[coef_name])
obs_coef_views_abs <- abs(coef(m_views_quasi)[coef_name])
obs_coef_conv_abs  <- abs(coef(m_conv)[coef_name])

print_boot_summary_abs(
  paste0("Model abs coef - Time (", coef_name, ")"),
  obs_coef_time_abs,
  coef_abs_time_clean
)

print_boot_summary_abs(
   paste0("Model abs coef - Views (", coef_name, ")"),
   obs_coef_views_abs,
   coef_abs_views_clean
)

print_boot_summary_abs(
  paste0("Model abs coef - Conv (", coef_name, ")"),
  obs_coef_conv_abs,
  coef_abs_conv_clean
)

### 6.9 Model-based bootstrap histograms
par(mfrow = c(1, 3))

hist(
  coef_abs_time_clean,
  breaks = 60,
  main   = paste0("Boot |coef| - Time (", coef_name, ")"),
  xlab   = "|coef|",
  col    = "lightblue"
)
abline(v = obs_coef_time_abs, col = "red", lwd = 2)

hist(
  coef_abs_views_clean,
  breaks = 60,
  main   = paste0("Boot |coef| - Views (", coef_name, ")"),
  xlab   = "|coef|",
  col    = "lightgreen"
)
abline(v = obs_coef_views_abs, col = "red", lwd = 2)

hist(
  coef_abs_conv_clean,
  breaks = 60,
  main   = paste0("Boot |coef| - Conv (", coef_name, ")"),
  xlab   = "|coef|",
  col    = "pink"
)
abline(v = obs_coef_conv_abs, col = "red", lwd = 2)

par(mfrow = c(1, 1))
