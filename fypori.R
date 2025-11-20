library(ggplot2)
library(patchwork)

################################################# VISUALIZATIONS ###########################################################
df <- read.csv("~/Downloads/ab_testing.csv")  # adjust path if needed
df$Group <- as.factor(df$Group)
df$Conversion <- as.factor(df$Conversion)
df$Location <- as.factor(df$Location)

# 1. Page Views Distribution
p1 <- ggplot(df, aes(x = Page.Views)) +
  geom_histogram(bins = 30, fill = "lightgreen", color = "black", alpha = 0.7) +
  facet_wrap(~Group, ncol = 1) +
  labs(title = "Page Views Distribution by Group", x = "Page Views", y = "Count")

# 2. Time Spent Distribution 
p2 <- ggplot(df, aes(x = Time.Spent)) +
  geom_histogram(bins = 30, fill = "lightblue", color = "black", alpha = 0.7) +
  facet_wrap(~Group, ncol = 1) +
  labs(title = "Time Spent Distribution by Group", x = "Time Spent (seconds)", y = "Count")

# 3. Conversion Rate 
conv_counts <- as.data.frame(table(df$Group, df$Conversion))
colnames(conv_counts) <- c("Group", "Conversion", "Count")

p3 <- ggplot(conv_counts, aes(x = "", y = Count, fill = Conversion)) +
  geom_bar(stat = "identity", width = 1, color = "black") +   
  coord_polar("y", start = 0) +
  facet_wrap(~Group) +
  labs(title = "Conversion Distribution by Group") +
  scale_fill_manual(values = c("No" = "grey", "Yes" = "cyan"))

# 4. Location Distribution (bar chart, separate by group)
p4 <- ggplot(df, aes(x = Location)) +
  geom_bar(fill = "coral", color = "black") +
  facet_wrap(~Group, ncol = 1) +
  coord_flip() +
  labs(title = "User Location Distribution by Group", x = "Location", y = "Count")

(p1 | p2) /
  (p3 | p4)

# Five-number summary for Page Views and Time Spent 
page_summary <- aggregate(Page.Views ~ Group, df, summary)
time_summary <- aggregate(Time.Spent ~ Group, df, summary)

print(page_summary)
print(time_summary)

# Average Time Spent per Group
avg_time <- aggregate(Time.Spent ~ Group, df, mean)
colnames(avg_time)[2] <- "Average_Time_Spent"
print(avg_time)

# Proportion of Conversion per Group
# Yes = 1, No = 0 
df$Conversion_Binary <- ifelse(df$Conversion == "Yes", 1, 0)
prop_conversion <- aggregate(Conversion_Binary ~ Group, df, mean)
colnames(prop_conversion)[2] <- "Proportion_Conversion"
print(prop_conversion)

################################################# ASSUMPTION CHECKS ###########################################################
################ TIME.SPENT – normality ################

# Shapiro–Wilk
shapiro_A_time <- shapiro.test(df$Time.Spent[df$Group == "A"])
shapiro_B_time <- shapiro.test(df$Time.Spent[df$Group == "B"])

print(shapiro_A_time)
print(shapiro_B_time)

# QQ plots raw + log
par(mfrow = c(2, 2))
qqnorm(df$Time.Spent[df$Group == "A"],
       main = "QQ Plot - Time.Spent (Group A)",
       pch = 16, col = "darkblue")
qqline(df$Time.Spent[df$Group == "A"], col = "red", lwd = 2)

qqnorm(df$Time.Spent[df$Group == "B"],
       main = "QQ Plot - Time.Spent (Group B)",
       pch = 16, col = "darkblue")
qqline(df$Time.Spent[df$Group == "B"], col = "red", lwd = 2)

# Check log transform 
df$log_Time.Spent <- log(df$Time.Spent + 1)
qqnorm(df$log_Time.Spent[df$Group == "A"],
       main = "QQ Plot - log(Time.Spent) (Group A)",
       pch = 16, col = "darkgreen")
qqline(df$log_Time.Spent[df$Group == "A"], col = "red", lwd = 2)

qqnorm(df$log_Time.Spent[df$Group == "B"],
       main = "QQ Plot - log(Time.Spent) (Group B)",
       pch = 16, col = "darkgreen")
qqline(df$log_Time.Spent[df$Group == "B"], col = "red", lwd = 2)

par(mfrow = c(1, 1))

################ CONVERSION – cell counts & events ################

table_conv <- table(df$Group, df$Conversion)
chi_conversion <- chisq.test(table_conv, correct = TRUE)
chi_conversion$expected
min(chi_conversion$expected)

# event counts for logistic regression
events    <- sum(df$Conversion_Binary == 1)
nonevents <- sum(df$Conversion_Binary == 0)
tapply(df$Conversion_Binary, df$Group, mean)  # conversion rate by group

################ PAGE.VIEWS – overdispersion ################

m_views_quasi <- glm(Page.Views ~ Group, data = df, family = quasipoisson())
overdisp_factor <- sum(residuals(m_views_quasi, type = "pearson")^2) / df.residual(m_views_quasi)
overdisp_factor


#################################################### TESTS #############################################################
# Check normality of numeric variables by group 
# Shapiro-Wilk test (parametric) 

# Time.Spent
shapiro_A_time <- shapiro.test(df$Time.Spent[df$Group == "A"])
shapiro_B_time <- shapiro.test(df$Time.Spent[df$Group == "B"])

print(shapiro_A_time)
print(shapiro_B_time)


# Page Views categorical bins for chi-square (quartiles)
df$Page.Views.Category <- cut(
  df$Page.Views,
  breaks = quantile(df$Page.Views, probs = seq(0, 1, 0.25), na.rm = TRUE),
  include.lowest = TRUE,
  labels = c("Low", "Medium", "High", "Very High")
)

## Chi-squared tests
# Page Views 
table_views <- table(df$Group, df$Page.Views.Category)
print(table_views)
chi_pageviews <- chisq.test(table_views)
print(chi_pageviews)

# Conversion 
table_conv <- table(df$Group, df$Conversion)
print(table_conv)
chi_conversion <- chisq.test(table_conv)
print(chi_conversion)

## Two-sample mean test (Welch t-test) 
# Time Spent 
t_time <- t.test(Time.Spent ~ Group, data = df, var.equal = FALSE)
print(t_time)

## Binomial / Proportion tests
# Two-proportion test for conversion (A vs B)
success_A <- table_conv["A", "Yes"]; n_A <- sum(table_conv["A", ])
success_B <- table_conv["B", "Yes"]; n_B <- sum(table_conv["B", ])
prop_test <- prop.test(x = c(success_A, success_B), n = c(n_A, n_B), correct = TRUE)
print(prop_test)

## Wald tests via regression models 
# fit one model per outcome with Group as predictor, then inspect the Group coefficient.

# Conversion → Logistic regression (Wald z-test on Group)
m_conv <- glm(Conversion_Binary ~ Group, data = df, family = binomial())
summary(m_conv)  # z value & p-value for Group

# Time Spent (continuous) → Linear model (Wald t-test on Group)
m_time <- lm(Time.Spent ~ Group, data = df)
summary(m_time)  # t value & p-value for Group

# Page Views (counts) → Count model (use quasi-Poisson to handle overdispersion)
m_views <- glm(Page.Views ~ Group, data = df, family = quasipoisson())
summary(m_views)  # t value (quasi) & p-value for Group

# overdispersion factor (~1 for Poisson; >>1 suggests overdispersion)
overdisp_factor <- sum(residuals(m_views, type = "pearson")^2) / df.residual(m_views)
print(overdisp_factor)

######################################################## POWER ANALYSIS ########################################################
library(dplyr)
library(pwr)

# Sizes
n_A <- sum(df$Group == "A")
n_B <- sum(df$Group == "B")

# Time.Spent: Cohen's d (used by Welch t and LM/Wald)
mean_A <- mean(df$Time.Spent[df$Group == "A"])
mean_B <- mean(df$Time.Spent[df$Group == "B"])
sd_pooled <- sqrt(((n_A - 1)*var(df$Time.Spent[df$Group == "A"]) + 
                     (n_B - 1)*var(df$Time.Spent[df$Group == "B"])) / (n_A + n_B - 2))
d <- abs(mean_A - mean_B) / sd_pooled

# Conversion: Cohen's h (magnitude)
eps <- 1e-12
p1 <- success_A / n_A; p1 <- pmin(pmax(p1, eps), 1 - eps)
p2 <- success_B / n_B; p2 <- pmin(pmax(p2, eps), 1 - eps)
h  <- abs(ES.h(p1, p2))

# Page.Views quartiles: Cohen's w
w <- sqrt(sum((chi_pageviews$observed - chi_pageviews$expected)^2 / chi_pageviews$expected) /
            sum(chi_pageviews$expected))

# Logistic regression (Conversion): f^2 approximation from z
model_summary <- summary(m_conv)
z_value      <- model_summary$coefficients[2, 3]
effect_size  <- (z_value)^2 / ((z_value)^2 + nrow(df))   # f^2 approx

# Page.Views (log-transform) for t-approx
log_mean_A <- mean(log(df$Page.Views[df$Group == "A"] + 1))
log_mean_B <- mean(log(df$Page.Views[df$Group == "B"] + 1))
sd_pooled_log <- sqrt(((n_A - 1)*var(log(df$Page.Views[df$Group == "A"] + 1)) +
                         (n_B - 1)*var(log(df$Page.Views[df$Group == "B"] + 1))) / (n_A + n_B - 2))
d_views <- abs(log_mean_A - log_mean_B) / sd_pooled_log

# Analytic power (print)
power_t             <- pwr.t2n.test(n1 = n_A, n2 = n_B, d = d,       sig.level = 0.05); print(power_t)
power_prop          <- pwr.2p2n.test(h = h, n1 = n_A, n2 = n_B,      sig.level = 0.05); print(power_prop)
df_chi              <- (nrow(table_views) - 1) * (ncol(table_views) - 1)
power_chi_pageviews <- pwr.chisq.test(w = w, N = sum(table_views), df = df_chi, sig.level = 0.05); print(power_chi_pageviews)
w_conv              <- sqrt(sum((chi_conversion$observed - chi_conversion$expected)^2 / chi_conversion$expected) /
                              sum(chi_conversion$expected))
df_conv             <- (nrow(table_conv) - 1) * (ncol(table_conv) - 1)
power_chi_conversion<- pwr.chisq.test(w = w_conv, N = sum(table_conv), df = df_conv, sig.level = 0.05); print(power_chi_conversion)
power_logit         <- pwr.f2.test(u = 1, v = nrow(df) - 2, f2 = effect_size, sig.level = 0.05); print(power_logit)
power_wald_time     <- pwr.t2n.test(n1 = n_A, n2 = n_B, d = d,       sig.level = 0.05); print(power_wald_time)
power_wald_views    <- pwr.t2n.test(n1 = n_A, n2 = n_B, d = d_views, sig.level = 0.05); print(power_wald_views)

# Observed effect sizes for markers
obs_d_time      <- d
obs_h_conv      <- h
obs_w_views     <- w
obs_f2_logit    <- effect_size
obs_d_views_log <- d_views

# Effect size sequences for power curves
d_seq       <- seq(0, 0.30, length.out = 200)   # Time Spent d
h_seq       <- seq(0, 0.40, length.out = 200)   # Conversion h
w_seq       <- seq(0, 0.15, length.out = 200)   # Page Views w
f2_seq      <- seq(0, 0.04, length.out = 200)   # Conversion f2
d_views_seq <- seq(0, 0.03, length.out = 200)   # Page Views quasiPois

# Build curves
df_t_curve <- data.frame(
  effect = d_seq,
  power  = sapply(d_seq, function(x) pwr.t2n.test(n1 = n_A, n2 = n_B, d = x, sig.level = 0.05)$power),
  test   = "Two-sample t (d)",
  metric = "Time Spent (2 Sample Mean)"
)

df_lm_time_curve <- data.frame(
  effect = d_seq,
  power  = sapply(d_seq, function(x) pwr.t2n.test(n1 = n_A, n2 = n_B, d = x, sig.level = 0.05)$power),
  test   = "LM/Wald (d)",
  metric = "Time Spent (Linear regression)"
)

df_prop_curve <- data.frame(
  effect = h_seq,
  power  = sapply(h_seq, function(x) pwr.2p2n.test(h = x, n1 = n_A, n2 = n_B, sig.level = 0.05)$power),
  test   = "Two-prop (h)",
  metric = "Conversion (Proportion)"
)

df_f2_curve <- data.frame(
  effect = f2_seq,
  power  = sapply(f2_seq, function(x) pwr.f2.test(u = 1, v = nrow(df)-2, f2 = x, sig.level = 0.05)$power),
  test   = "F-test (f2)",
  metric = "Conversion (Logistic regression)"
)

df_chi_curve <- data.frame(
  effect = w_seq,
  power  = sapply(w_seq, function(x) pwr.chisq.test(w = x, N = (n_A + n_B),
                                                    df = (nrow(table_views)-1)*(ncol(table_views)-1),
                                                    sig.level = 0.05)$power),
  test   = "Chi-square (w)",
  metric = "Page Views (Chi^2)"
)

df_views_d_curve <- data.frame(
  effect = d_views_seq,
  power  = sapply(d_views_seq, function(x) pwr.t2n.test(n1 = n_A, n2 = n_B, d = x, sig.level = 0.05)$power),
  test   = "t on log(Page.Views+1)",
  metric = "Page Views (Quasi-Poisson)"
)

# Combine all curves
plot_df <- dplyr::bind_rows(
  df_t_curve,
  df_lm_time_curve,
  df_prop_curve,
  df_f2_curve,
  df_chi_curve,
  df_views_d_curve
)

# Facet order (top-left → top-right → next rows)
metric_levels <- c(
  "Time Spent (2 Sample Mean)",    
  "Time Spent (Linear regression)", 
  "Conversion (Proportion)",
  "Conversion (Logistic regression)",
  "Page Views (Chi^2)",
  "Page Views (Quasi-Poisson)"
)
plot_df$metric <- factor(plot_df$metric, levels = metric_levels)

# Observed markers at top (y=0.95)
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
    obs_d_time,      # Welch t
    obs_d_time,      # LM/Wald
    obs_h_conv,      # prop
    obs_f2_logit,    # logistic
    obs_w_views,     # chi^2
    obs_d_views_log  # quasi-poisson (log t approx)
  ),
  y = 0.95,
  stringsAsFactors = FALSE
)
obs_markers$metric <- factor(obs_markers$metric, levels = metric_levels)

# Plot: Power vs Effect Size
p_power <- ggplot(plot_df, aes(x = effect, y = power)) +
  geom_line() +
  facet_wrap(~metric, scales = "free_x", ncol = 2) +
  geom_vline(data = obs_markers, aes(xintercept = obs_effect),
             color = "red", linetype = "dotted") +
  geom_label(data = obs_markers,
             aes(x = obs_effect, y = y,
                 label = paste0("obs = ", round(obs_effect, 4))),
             color = "red", size = 3, vjust = 1) +
  labs(title = "Power vs Effect Size (alpha = 0.05)",
       x = "Effect size (metric-specific)", y = "Power") +
  theme_minimal()

print(p_power)

####################################################### BOOTSTRAP RESAMPLING ####################################################################
set.seed(123)
R_boot <- 10000 

library(boot)

# helper to print summary 
print_boot_summary_abs <- function(name, obs_abs, boot_vec) {
  se <- sd(boot_vec, na.rm = TRUE)
  ci <- quantile(boot_vec, probs = c(0.025, 0.975), na.rm = TRUE)
  p_val <- mean(boot_vec >= abs(obs_abs), na.rm = TRUE)
  cat("-----", name, "-----\n")
  cat("Observed (abs):", signif(abs(obs_abs), 6), "\n")
  cat("Bootstrap SE:", signif(se, 6), "\n")
  cat("Bootstrap 95% CI (percentile):", signif(ci[1], 6), "-", signif(ci[2], 6), "\n")
  cat("Bootstrap p-value:", signif(p_val, 6), "\n\n")
  invisible(list(se = se, ci = ci, p = p_val))
}

# signed boot statistic functions (absolute) 
boot_time_stat <- function(data, indices) {
  d <- data[indices, , drop = FALSE]
  mA <- mean(d$Time.Spent[d$Group == "A"], na.rm = TRUE)
  mB <- mean(d$Time.Spent[d$Group == "B"], na.rm = TRUE)
  abs(mA - mB)
}

boot_views_stat <- function(data, indices) {
  d <- data[indices, , drop = FALSE]
  mA <- mean(d$Page.Views[d$Group == "A"], na.rm = TRUE)
  mB <- mean(d$Page.Views[d$Group == "B"], na.rm = TRUE)
  abs(mA - mB)
}

boot_conv_stat <- function(data, indices) {
  d <- data[indices, , drop = FALSE]
  pA <- mean(d$Conversion_Binary[d$Group == "A"], na.rm = TRUE)
  pB <- mean(d$Conversion_Binary[d$Group == "B"], na.rm = TRUE)
  abs(pA - pB)
}

# Prepare data frames
df_time <- df[, c("Group", "Time.Spent")]
df_views <- df[, c("Group", "Page.Views")]
df_conv <- df[, c("Group", "Conversion_Binary")]

boot_time <- boot(data = df_time, statistic = boot_time_stat, R = R_boot)
boot_views <- boot(data = df_views, statistic = boot_views_stat, R = R_boot)
boot_conv <- boot(data = df_conv, statistic = boot_conv_stat, R = R_boot)

# Observed absolute statistics
obs_time_abs <- abs(mean(df$Time.Spent[df$Group == "A"], na.rm = TRUE) - mean(df$Time.Spent[df$Group == "B"], na.rm = TRUE))
obs_views_abs <- abs(mean(df$Page.Views[df$Group == "A"], na.rm = TRUE) - mean(df$Page.Views[df$Group == "B"], na.rm = TRUE))
obs_conv_abs <- abs(mean(df$Conversion_Binary[df$Group == "A"], na.rm = TRUE) - mean(df$Conversion_Binary[df$Group == "B"], na.rm = TRUE))

print_boot_summary_abs("Time.Spent (abs mean difference)", obs_time_abs, boot_time$t)
print_boot_summary_abs("Page.Views (abs mean difference)", obs_views_abs, boot_views$t)
print_boot_summary_abs("Conversion (abs proportion difference)", obs_conv_abs, boot_conv$t)

cat("Confidence intervals via boot.ci (percentile and BCa):\n")
cat("Time.Spent:\n"); print(boot.ci(boot_time, type = c("perc","bca")))
cat("\nPage.Views:\n"); print(boot.ci(boot_views, type = c("perc","bca")))
cat("\nConversion:\n"); print(boot.ci(boot_conv, type = c("perc","bca")))

# plot histograms
par(mfrow = c(1, 3))
hist(boot_time$t, breaks = 60, main = "Bootstrap: |Time mean A - B|", xlab = "Abs mean diff (sec)", col = "lightblue")
abline(v = obs_time_abs, col = "red", lwd = 2)
hist(boot_views$t, breaks = 60, main = "Bootstrap: |PageViews mean A - B|", xlab = "Abs mean diff", col = "lightgreen")
abline(v = obs_views_abs, col = "red", lwd = 2)
hist(boot_conv$t, breaks = 60, main = "Bootstrap: |Conv prop A - B|", xlab = "Abs prop diff", col = "pink")
abline(v = obs_conv_abs, col = "red", lwd = 2)
par(mfrow = c(1, 1))

# Model-based bootstrap (absolute coefficients) 
coef_abs_time <- numeric(R_boot)
coef_abs_views <- numeric(R_boot)
coef_abs_conv <- numeric(R_boot)

lvl <- levels(df$Group)
coef_name <- paste0("Group", lvl[2])

pb <- txtProgressBar(min = 0, max = R_boot, style = 3)
for (i in seq_len(R_boot)) {
  samp_idx <- sample(seq_len(nrow(df)), size = nrow(df), replace = TRUE)
  d_samp <- df[samp_idx, ]
  
  # Time: linear model
  mtime <- tryCatch(lm(Time.Spent ~ Group, data = d_samp), error = function(e) NULL)
  coef_abs_time[i] <- if (!is.null(mtime)) abs(coef(mtime)[coef_name]) else NA
  
  # Views: quasi-poisson
  mviews <- tryCatch(glm(Page.Views ~ Group, data = d_samp, family = quasipoisson()), error = function(e) NULL)
  coef_abs_views[i] <- if (!is.null(mviews)) abs(coef(mviews)[coef_name]) else NA
  
  # Conversion: logistic
  mconv <- tryCatch(glm(Conversion_Binary ~ Group, data = d_samp, family = binomial()), error = function(e) NULL)
  coef_abs_conv[i] <- if (!is.null(mconv)) abs(coef(mconv)[coef_name]) else NA
  
  if (i %% 50 == 0) setTxtProgressBar(pb, i)
}
close(pb)

coef_abs_time_clean <- coef_abs_time[!is.na(coef_abs_time)]
coef_abs_views_clean <- coef_abs_views[!is.na(coef_abs_views)]
coef_abs_conv_clean <- coef_abs_conv[!is.na(coef_abs_conv)]

obs_coef_time_abs <- abs(coef(m_time)[coef_name])
obs_coef_views_abs <- abs(coef(m_views)[coef_name])
obs_coef_conv_abs <- abs(coef(m_conv)[coef_name])

print_boot_summary_abs(paste0("Model abs coef - Time (", coef_name, ")"), obs_coef_time_abs, coef_abs_time_clean)
print_boot_summary_abs(paste0("Model abs coef - Views (", coef_name, ")"), obs_coef_views_abs, coef_abs_views_clean)
print_boot_summary_abs(paste0("Model abs coef - Conv (", coef_name, ")"), obs_coef_conv_abs, coef_abs_conv_clean)

if (!is.na(obs_coef_conv_abs) && length(coef_abs_conv_clean) > 0) {
  obs_or_mag <- exp(obs_coef_conv_abs)
  or_boot_mag <- exp(coef_abs_conv_clean)
  cat("Observed OR magnitude (exp(abs(log-odds))):", signif(obs_or_mag, 6), "\n")
  cat("Bootstrap OR magnitude 95% CI (percentile):", signif(quantile(or_boot_mag, c(0.025, 0.975)), 6), "\n\n")
}

# plots for model-based bootstrap magnitudes
par(mfrow = c(1, 3))
hist(coef_abs_time_clean, breaks = 60, main = paste0("Boot |coef| - Time (", coef_name, ")"), xlab = "|coef|", col = "lightgray")
abline(v = obs_coef_time_abs, col = "red", lwd = 2)
hist(coef_abs_views_clean, breaks = 60, main = paste0("Boot |coef| - Views (", coef_name, ")"), xlab = "|coef|", col = "lightgray")
abline(v = obs_coef_views_abs, col = "red", lwd = 2)
hist(coef_abs_conv_clean, breaks = 60, main = paste0("Boot |coef| - Conv (", coef_name, ")"), xlab = "|coef|", col = "lightgray")
abline(v = obs_coef_conv_abs, col = "red", lwd = 2)
par(mfrow = c(1, 1))


## 7) Bootstrap-based Power Estimation (empirical permutation-within-bootstrap)
set.seed(123)

# Parameters (tune these)
R_outer <- 1000   # number of bootstrap datasets (outer). Increase to 2000-5000 for more precision.
R_perm  <- 1000   # number of label-permutations per bootstrap sample for the null distribution.
alpha   <- 0.05

# Utility: signed statistics (use signed stats and compare abs() when computing two-sided p-value)
stat_mean_diff_signed <- function(d, value_col) {
  mA <- mean(d[[value_col]][d$Group == "A"], na.rm = TRUE)
  mB <- mean(d[[value_col]][d$Group == "B"], na.rm = TRUE)
  return(mB - mA)
}
stat_prop_diff_signed <- function(d, value_col) {
  pA <- mean(d[[value_col]][d$Group == "A"], na.rm = TRUE)
  pB <- mean(d[[value_col]][d$Group == "B"], na.rm = TRUE)
  return(pB - pA)
}
stat_glm_coef_signed <- function(d, formula, family) {
  m <- tryCatch(glm(formula = formula, data = d, family = family), error = function(e) NULL)
  if (is.null(m)) return(NA_real_)
  # Determine group coefficient name; assumes Group factor with levels A,B
  nm <- names(coef(m))
  grp_co <- nm[grep("GroupB$", nm)]
  if (length(grp_co) == 0) {
    grp_co <- nm[grep("^Group", nm)]
  }
  if (length(grp_co) == 0) return(NA_real_)
  coef(m)[grp_co[1]]
}

# Generic permutation-within-bootstrap function
bootstrap_power_perm <- function(data,
                                 stat_fun,       # function(d) -> signed statistic
                                 R_outer = 1000,
                                 R_perm = 1000,
                                 alpha = 0.05,
                                 verbose = TRUE) {
  n <- nrow(data)
  rejects <- logical(R_outer)
  pb <- if (verbose) txtProgressBar(min = 0, max = R_outer, style = 3) else NULL
  c
  for (i in seq_len(R_outer)) {
    # 1) bootstrap sample (replicate dataset under H1)
    samp_idx <- sample.int(n, n, replace = TRUE)
    d_samp <- data[samp_idx, , drop = FALSE]
    
    # 2) observed statistic in that bootstrap dataset
    obs_stat <- stat_fun(d_samp)
    
    # 3) null distribution via permuting Group labels inside this bootstrap sample
    perm_stats <- numeric(R_perm)
    for (j in seq_len(R_perm)) {
      d_perm <- d_samp
      d_perm$Group <- sample(d_perm$Group)  # permute labels (break A/B relationship)
      perm_stats[j] <- stat_fun(d_perm)
    }
    
    # 4) two-sided p-value: proportion of perm stats as or more extreme than observed
    if (is.na(obs_stat) || all(is.na(perm_stats))) {
      pval <- NA_real_
      reject <- FALSE
    } else {
      pval <- mean(abs(perm_stats) >= abs(obs_stat), na.rm = TRUE)
      reject <- !is.na(pval) && (pval < alpha)
    }
    
    rejects[i] <- reject
    if (verbose && (i %% 10 == 0)) setTxtProgressBar(pb, i)
  }
  if (verbose) close(pb)
  power_est <- mean(rejects, na.rm = TRUE)
  # binomial CI for power estimate
  se <- sqrt(power_est * (1 - power_est) / sum(!is.na(rejects)))
  ci_lower <- qbeta(0.025, 1 + sum(rejects, na.rm = TRUE), 1 + sum(!rejects, na.rm = TRUE))
  ci_upper <- qbeta(0.975, 1 + sum(rejects, na.rm = TRUE), 1 + sum(!rejects, na.rm = TRUE))
  list(power = power_est, n = sum(!is.na(rejects)), conf.int = c(ci_lower, ci_upper))
}

# Apply to tests
df$Group <- factor(df$Group, levels = c("A", "B"))

# 1) Time.Spent mean difference 
res_time <- bootstrap_power_perm(data = df,
                                 stat_fun = function(d) stat_mean_diff_signed(d, "Time.Spent"),
                                 R_outer = R_outer, R_perm = R_perm, alpha = alpha, verbose = TRUE)

# 2) Page.Views mean difference
res_views <- bootstrap_power_perm(data = df,
                                  stat_fun = function(d) stat_mean_diff_signed(d, "Page.Views"),
                                  R_outer = R_outer, R_perm = R_perm, alpha = alpha, verbose = TRUE)

# 3) Conversion proportion difference
res_conv <- bootstrap_power_perm(data = df,
                                 stat_fun = function(d) stat_prop_diff_signed(d, "Conversion_Binary"),
                                 R_outer = R_outer, R_perm = R_perm, alpha = alpha, verbose = TRUE)

# 4) LM for Time.Spent
res_lm_time <- bootstrap_power_perm(data = df,
                                    stat_fun = function(d) stat_glm_coef_signed(d, as.formula("Time.Spent ~ Group"), gaussian()),
                                    R_outer = R_outer, R_perm = R_perm, alpha = alpha, verbose = TRUE)

# 5) Quasi-Poisson for Page.Views
res_qp_views <- bootstrap_power_perm(data = df,
                                     stat_fun = function(d) stat_glm_coef_signed(d, as.formula("Page.Views ~ Group"), quasipoisson()),
                                     R_outer = R_outer, R_perm = R_perm, alpha = alpha, verbose = TRUE)

# 6) Logistic for Conversion
res_logit_conv <- bootstrap_power_perm(data = df,
                                       stat_fun = function(d) stat_glm_coef_signed(d, as.formula("Conversion_Binary ~ Group"), binomial()),
                                       R_outer = R_outer, R_perm = R_perm, alpha = alpha, verbose = TRUE)

# Print summary
print_power <- function(name, res) {
  cat("----", name, "----\n")
  cat("Power estimate:", signif(res$power, 4), " (n reps:", res$n, ")\n")
  cat("95% CI (beta-based):", signif(res$conf.int[1], 4), "-", signif(res$conf.int[2], 4), "\n\n")
}

print_power("Time.Spent mean diff", res_time)
print_power("Page.Views mean diff", res_views)
print_power("Conversion proportion diff", res_conv)
print_power("LM coef Time.Spent", res_lm_time)
print_power("Quasi-Poisson coef Page.Views", res_qp_views)
print_power("Logistic coef Conversion", res_logit_conv)

