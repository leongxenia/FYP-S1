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
## 3. Assumption checks
############################################################

### 3.1 Time.Spent – normality
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

### 3.2 Conversion – cell counts and events
table_conv     <- table(df$Group, df$Conversion)
chi_conversion <- chisq.test(table_conv, correct = TRUE)

chi_conversion$expected
min(chi_conversion$expected)

events    <- sum(df$Conversion_Binary == 1)
nonevents <- sum(df$Conversion_Binary == 0)

tapply(df$Conversion_Binary, df$Group, mean)

### 3.3 Page.Views – overdispersion (quasi-Poisson)
m_views_quasi <- glm(Page.Views ~ Group, data = df, family = quasipoisson())
overdisp_factor <- sum(residuals(m_views_quasi, type = "pearson")^2) /
  df.residual(m_views_quasi)
overdisp_factor

############################################################
## 4. Hypothesis tests (classical)
############################################################

### 4.1 Page.Views – chi-square using categorical bins
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

### 4.2 Conversion – chi-square
print(table_conv)
chi_conversion <- chisq.test(table_conv)
print(chi_conversion)

### 4.3 Time.Spent – two-sample Welch t-test
t_time <- t.test(Time.Spent ~ Group, data = df, var.equal = FALSE)
print(t_time)

### 4.4 Conversion – two-proportion test (A vs B)
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

### 4.5 Wald tests via regression models
## Conversion – logistic regression (Wald z-test on Group)
m_conv <- glm(Conversion_Binary ~ Group, data = df, family = binomial())
summary(m_conv)

## Time.Spent – linear model (Wald t-test on Group)
m_time <- lm(Time.Spent ~ Group, data = df)
summary(m_time)

## Page.Views – quasi-Poisson regression (Wald-type test on Group)
m_views <- glm(Page.Views ~ Group, data = df, family = quasipoisson())
summary(m_views)

############################################################
## 5. Analytic power analysis
############################################################

### 5.1 Effect sizes
## Time.Spent – Cohen's d
mean_A <- mean(df$Time.Spent[df$Group == "A"])
mean_B <- mean(df$Time.Spent[df$Group == "B"])

sd_pooled <- sqrt(
  ((n_A - 1) * var(df$Time.Spent[df$Group == "A"]) +
     (n_B - 1) * var(df$Time.Spent[df$Group == "B"])) /
    (n_A + n_B - 2)
)

d <- abs(mean_A - mean_B) / sd_pooled

## Conversion – Cohen's h
eps <- 1e-12
p1  <- success_A / n_A
p1  <- pmin(pmax(p1, eps), 1 - eps)

p2  <- success_B / n_B
p2  <- pmin(pmax(p2, eps), 1 - eps)

h <- abs(ES.h(p1, p2))

## Page.Views (quartiles) – Cohen's w
w <- sqrt(
  sum((chi_pageviews$observed - chi_pageviews$expected)^2 / chi_pageviews$expected) /
    sum(chi_pageviews$expected)
)

## Conversion (logistic) – f² approximation from z
model_summary <- summary(m_conv)
z_value       <- model_summary$coefficients[2, 3]
effect_size   <- (z_value)^2 / ((z_value)^2 + nrow(df))

## Page.Views (log-transform) – Cohen's d for log(Page.Views + 1)
log_mean_A <- mean(log(df$Page.Views[df$Group == "A"] + 1))
log_mean_B <- mean(log(df$Page.Views[df$Group == "B"] + 1))

sd_pooled_log <- sqrt(
  ((n_A - 1) * var(log(df$Page.Views[df$Group == "A"] + 1)) +
     (n_B - 1) * var(log(df$Page.Views[df$Group == "B"] + 1))) /
    (n_A + n_B - 2)
)

d_views <- abs(log_mean_A - log_mean_B) / sd_pooled_log

### 5.2 Analytic power (printed)
power_t <- pwr.t2n.test(
  n1        = n_A,
  n2        = n_B,
  d         = d,
  sig.level = 0.05
)
print(power_t)

power_prop <- pwr.2p2n.test(
  h         = h,
  n1        = n_A,
  n2        = n_B,
  sig.level = 0.05
)
print(power_prop)

df_chi <- (nrow(table_views) - 1) * (ncol(table_views) - 1)

power_chi_pageviews <- pwr.chisq.test(
  w         = w,
  N         = sum(table_views),
  df        = df_chi,
  sig.level = 0.05
)
print(power_chi_pageviews)

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

power_logit <- pwr.f2.test(
  u         = 1,
  v         = nrow(df) - 2,
  f2        = effect_size,
  sig.level = 0.05
)
print(power_logit)

power_wald_time <- pwr.t2n.test(
  n1        = n_A,
  n2        = n_B,
  d         = d,
  sig.level = 0.05
)
print(power_wald_time)

power_wald_views <- pwr.t2n.test(
  n1        = n_A,
  n2        = n_B,
  d         = d_views,
  sig.level = 0.05
)
print(power_wald_views)

### 5.3 Observed effect sizes for power plots
obs_d_time      <- d
obs_h_conv      <- h
obs_w_views     <- w
obs_f2_logit    <- effect_size
obs_d_views_log <- d_views

### 5.4 Effect size sequences for power curves
d_seq       <- seq(0, 0.50, length.out = 200)
h_seq       <- seq(0, 0.60, length.out = 200)
w_seq       <- seq(0, 0.30, length.out = 200)
f2_seq      <- seq(0, 0.08, length.out = 200)
d_views_seq <- seq(0, 0.05, length.out = 200)

### 5.5 Power curves (per test)
df_t_curve <- data.frame(
  effect = d_seq,
  power  = sapply(
    d_seq,
    function(x) pwr.t2n.test(n1 = n_A, n2 = n_B, d = x, sig.level = 0.05)$power
  ),
  test   = "Two-sample t (d)",
  metric = "Time Spent (2 Sample Mean)"
)

df_lm_time_curve <- data.frame(
  effect = d_seq,
  power  = sapply(
    d_seq,
    function(x) pwr.t2n.test(n1 = n_A, n2 = n_B, d = x, sig.level = 0.05)$power
  ),
  test   = "LM/Wald (d)",
  metric = "Time Spent (Linear regression)"
)

df_prop_curve <- data.frame(
  effect = h_seq,
  power  = sapply(
    h_seq,
    function(x) pwr.2p2n.test(h = x, n1 = n_A, n2 = n_B, sig.level = 0.05)$power
  ),
  test   = "Two-prop (h)",
  metric = "Conversion (Proportion)"
)

df_f2_curve <- data.frame(
  effect = f2_seq,
  power  = sapply(
    f2_seq,
    function(x) pwr.f2.test(u = 1, v = nrow(df) - 2, f2 = x, sig.level = 0.05)$power
  ),
  test   = "F-test (f2)",
  metric = "Conversion (Logistic regression)"
)

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

df_views_d_curve <- data.frame(
  effect = d_views_seq,
  power  = sapply(
    d_views_seq,
    function(x) pwr.t2n.test(n1 = n_A, n2 = n_B, d = x, sig.level = 0.05)$power
  ),
  test   = "t on log(Page.Views+1)",
  metric = "Page Views (Quasi-Poisson)"
)

### 5.6 Combine power curves and add observed markers
plot_df <- dplyr::bind_rows(
  df_t_curve,
  df_lm_time_curve,
  df_prop_curve,
  df_f2_curve,
  df_chi_curve,
  df_views_d_curve
)

metric_levels <- c(
  "Time Spent (2 Sample Mean)",
  "Time Spent (Linear regression)",
  "Conversion (Proportion)",
  "Conversion (Logistic regression)",
  "Page Views (Chi^2)",
  "Page Views (Quasi-Poisson)"
)

plot_df$metric <- factor(plot_df$metric, levels = metric_levels)

obs_markers <- data.frame(
  metric     = c(
    "Time Spent (2 Sample Mean)",
    "Time Spent (Linear regression)",
    "Conversion (Proportion)",
    "Conversion (Logistic regression)",
    "Page Views (Chi^2)",
    "Page Views (Quasi-Poisson)"
  ),
  obs_effect = c(
    obs_d_time,
    obs_d_time,
    obs_h_conv,
    obs_f2_logit,
    obs_w_views,
    obs_d_views_log
  ),
  y          = 0.95,
  stringsAsFactors = FALSE
)

obs_markers$metric <- factor(obs_markers$metric, levels = metric_levels)

### 5.7 Power vs effect size plot
p_power <- ggplot(plot_df, aes(x = effect, y = power)) +
  geom_line() +
  facet_wrap(~ metric, scales = "free_x", ncol = 2) +
  geom_vline(
    data     = obs_markers,
    aes(xintercept = obs_effect),
    color    = "red",
    linetype = "dotted"
  ) +
  geom_label(
    data  = obs_markers,
    aes(
      x     = obs_effect,
      y     = y,
      label = paste0("obs = ", round(obs_effect, 4))
    ),
    color = "red",
    size  = 3,
    vjust = 1
  ) +
  labs(
    title = "Power vs Effect Size (alpha = 0.05)",
    x     = "Effect size (metric-specific)",
    y     = "Power"
  ) +
  theme_minimal()

print(p_power)

############################################################
## 5A. Poisson model for Page.Views (Wald test + power)
############################################################

### 5A.1 Poisson GLM for Page.Views ~ Group

m_views_pois <- glm(Page.Views ~ Group, data = df, family = poisson())
summary(m_views_pois)

### 5A.2 Effect size for Poisson Wald test (f²-style from z)

pois_summary <- summary(m_views_pois)

## assumes Group has levels c("A","B"), so coefficient is "GroupB"
z_pois <- pois_summary$coefficients["GroupB", "z value"]

## f² approximation using the Wald z-stat (same idea as logistic)
f2_pois <- (z_pois^2) / (z_pois^2 + nrow(df))

cat("Poisson GLM – Group effect z:", z_pois, "\n")
cat("Poisson GLM – f² approximation:", f2_pois, "\n\n")

### 5A.3 Analytic power at observed Poisson effect size

power_pois_wald <- pwr.f2.test(
  u         = 1,                 ## 1 df for Group
  v         = nrow(df) - 2,      ## approx residual df (intercept + Group)
  f2        = f2_pois,
  sig.level = 0.05
)
print(power_pois_wald)

### 5A.4 Power vs effect size curve for Poisson Wald test

## choose a sensible range for f²; you can tweak upper limit if needed
f2_pois_seq <- seq(0, max(0.08, f2_pois * 1.5), length.out = 200)

df_pois_curve <- data.frame(
  effect = f2_pois_seq,
  power  = sapply(
    f2_pois_seq,
    function(x) pwr.f2.test(u = 1, v = nrow(df) - 2, f2 = x, sig.level = 0.05)$power
  )
)

## data frame for marking observed effect size on the plot
obs_marker_pois <- data.frame(
  effect = f2_pois,
  power  = pwr.f2.test(u = 1, v = nrow(df) - 2, f2 = f2_pois, sig.level = 0.05)$power
)

### 5A.5 Plot: Power vs f² (Poisson Wald test for Page.Views)

p_power_pois <- ggplot(df_pois_curve, aes(x = effect, y = power)) +
  geom_line() +
  geom_vline(
    xintercept = f2_pois,
    color      = "red",
    linetype   = "dotted"
  ) +
  geom_label(
    data  = obs_marker_pois,
    aes(
      x     = effect,
      y     = power,
      label = paste0("obs f² = ", round(effect, 4))
    ),
    color = "red",
    size  = 3,
    vjust = -0.5
  ) +
  scale_x_continuous(
    breaks = seq(0, max(f2_pois_seq), by = 0.01),
    limits = c(0, max(f2_pois_seq))
  ) +
  labs(
    title = "Power vs Effect Size (Poisson Wald test for Page.Views)",
    x     = "Effect size f² (Poisson GLM, Group effect)",
    y     = "Power"
  ) +
  ylim(0, 1) +
  theme_minimal()

print(p_power_pois)
