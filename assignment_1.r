library(gt)
library(webshot2)
library(dagitty)
library(ggdag)
library(ggplot2)
library(officer)
library(flextable)
library(sandwich)
library(lmtest)
library(car)

doc <- read_docx()

cur_dir <- getwd()
setwd(cur_dir)
seed <- 20260000 + 2033

cat("\n====================\nPART A.1\n====================\n")

round3 <- function(x) round(x, 3)

url <- paste0(
  "https://raw.githubusercontent.com/",
  "cshsiehecon/SPE7002/main/data/MROZ.csv"
)



df <- read.csv(url)
# only consider those in the labor force and with wage
earn <- subset(df, inlf == 1 & !is.na(lwage))

## Short
short <- lm(lwage ~ educ, data = earn)
sum_est <- summary(short)
coef_tab <- coef(sum_est)
se <- sqrt(diag(vcov(short)))

print("start of short regression")
cat("\nR-squared short:\n")
print(round3(sum_est$r.squared))

cat("\nAdjusted R-squared short:\n")
print(round3(sum_est$adj.r.squared))

cat("\nSample size short:\n")
print(round3(length(residuals(short))))

df_short <- data.frame(
  Variable = rownames(coef_tab),
  Coefficient = round3(coef_tab[, "Estimate"]),
  Std_Error = round3(se),
  P_Value = round3(coef_tab[, "Pr(>|t|)"])
)

print(df_short)

print("end of short regression")

## Mincer
mincer <- lm(lwage ~ educ + exper + expersq, data = earn)

sum_minc <- summary(mincer)
coef_minc <- coef(sum_minc)
se <- sqrt(diag(vcov(mincer)))

print("start of mincer regression")
cat("\nR-squared:\n")
print(round3(sum_minc$r.squared))

cat("\nAdjusted R-squared:\n")
print(round3(sum_minc$adj.r.squared))

cat("\nSample size:\n")
print(round3(length(residuals(mincer))))

df_mincer <- data.frame(
  Variable = rownames(coef_minc),
  Coefficient = round3(coef_minc[, "Estimate"]),
  Std_Error = round3(se),
  P_Value = round3(coef_minc[, "Pr(>|t|)"])
)

print(df_mincer)

print("end of mincer regression")

reg_table <- gt(df_mincer) %>%
  tab_header(
    title = "Regression Results",
    subtitle = "OLS coefficients, standard errors, and p-values"
  ) %>%
  fmt_number(
    columns = c(Coefficient, Std_Error, P_Value),
    decimals = 3
  )

cat("\n====================\nPART A.3\n====================\n")

educ_estimate <- coef_minc["educ","Estimate"]
usual_coef <- 100 * educ_estimate
exact_percentage_change <- 100 * (exp(educ_estimate) - 1)

print(doc, target = "OLS_normal_equation.docx")
print(paste("Usual coefficient: ", round3(usual_coef), "%, Exact percentage change: ", round3(exact_percentage_change), "%"))


cat("\n====================\nPART A.4\n====================\n")

# prove Xe = 0
X <- model.matrix(mincer)
e <- residuals(mincer)

res <- t(X) %*% e

res_table <- data.frame(
  Variable = rownames(res),
  XTe = as.numeric(res[, 1])
)
res_table$XTe <- format(
  res_table$XTe,
  scientific = TRUE,
  digits = 3
)

doc <- body_add_par(doc, "OLS Normal Equation Check: X'e = 0")
doc <- body_add_par(
  doc,
  paste(capture.output(print(res)), collapse = "\n")
)
ft <- flextable(res_table)
ft <- autofit(ft)

doc <- body_add_flextable(doc, ft)

print(round3(res))

# Frisch–Waugh–Lovell
lwage_controls <- lm(lwage ~ exper + expersq, data = earn)
lwage_resid <- residuals(lwage_controls)

educ_controls <- lm(educ ~ exper + expersq, data = earn)
educ_resid <- residuals(educ_controls)

res <- lm(lwage_resid ~ educ_resid)

sum_res <- summary(res)
coef_res <- coef(sum_res)

df_fwl <- data.frame(
  Variable = rownames(coef_res),
  Coefficient = coef_res[, "Estimate"],
  Std_Error = coef_res[, "Std. Error"],
  T_Value = coef_res[, "t value"],
  P_Value = coef_res[, "Pr(>|t|)"]
)
df_fwl[, -1] <- lapply(
  df_fwl[, -1],
  function(x) formatC(x, format = "f", digits = 3)
)
doc <- body_add_par(doc, "Frisch–Waugh–Lovell")
ft <- flextable(df_fwl)
ft <- autofit(ft)

doc <- body_add_flextable(doc, ft)



print(res)

earn$educ_months <- 12 * earn$educ

# Number 5 
cat("\n====================\nPART A.5\n====================\n")

# Mincer redo
mincer_redo <- lm(lwage ~ educ_months + exper + expersq, data = earn)

sum_min_redo <- summary(mincer_redo)
coef_minc_redo <- coef(sum_min_redo)
se_minc_redo <- sqrt(diag(vcov(mincer_redo)))

df_mincer_redo <- data.frame(
  Variable = rownames(coef_minc_redo),
  Coefficient = round3(coef_minc_redo[, "Estimate"]),
  Std_Error = round3(se_minc_redo),
  P_Value = round3(coef_minc_redo[, "Pr(>|t|)"])
)
df_mincer_redo[, -1] <- lapply(
  df_mincer_redo[, -1],
  function(x) formatC(x, format = "f", digits = 3)
)
doc <- body_add_par(doc, "Mincer times 12")
ft <- flextable(df_mincer_redo)
ft <- autofit(ft)

doc <- body_add_flextable(doc, ft)


comparison <- data.frame(
  Statistic = c(
    "Education coefficient",
    "Standard error",
    "t-statistic",
    "p-value",
    "R-squared"
  ),

  Before = c(
    coef_minc["educ", "Estimate"],
    coef_minc["educ", "Std. Error"],
    coef_minc["educ", "t value"],
    coef_minc["educ", "Pr(>|t|)"],
    sum_minc$r.squared
  ),

  After = c(
    coef_minc_redo["educ_months", "Estimate"],
    coef_minc_redo["educ_months", "Std. Error"],
    coef_minc_redo["educ_months", "t value"],
    coef_minc_redo["educ_months", "Pr(>|t|)"],
    sum_min_redo$r.squared
  )
)
comparison[, c("Before", "After")] <- lapply(
  comparison[, c("Before", "After")],
  function(x) format(x, scientific = FALSE, digits = 3)
)

fitted_comparison <- data.frame(
  Before = fitted(mincer),
  After = fitted(mincer_redo)
)

head(fitted_comparison)

doc <- body_add_par(doc, "Mincer Regression Rescaling Comparison")

ft1 <- flextable(comparison)
ft1 <- autofit(ft1)

doc <- body_add_flextable(doc, ft1)

doc <- body_add_par(doc, "Fitted Values Comparison")

ft2 <- flextable(head(fitted_comparison))
ft2 <- autofit(ft2)

doc <- body_add_flextable(doc, ft2)


cat("\n====================\nPART A.6\n====================\n")

# Number 6
# a
dag <- dagitty("
dag {
  ability -> educ
  ability -> lwage
  educ -> lwage
}
")

p <- ggdag(dag) + theme_dag()
ggsave(
  filename = "causal_diagram.png",
  plot = p,
  width = 6,
  height = 4,
  dpi = 300
)



## part B
make_sample <- function(s, n = 250) {
  set.seed(s)
  A <- rnorm(n)
  X <- 0.80 * A + rnorm(n)
  u_valid <- rnorm(n)
  u_hetero <- exp(0.25 * X) * rnorm(n)
  v <- rnorm(n)
  Y_valid <- 1 + 0.50 * X + u_valid
  Y_hetero <- 1 + 0.50 * X + u_hetero
  Y_endog <- 1 + 0.50 * X + 0.80 * A + v
  data.frame(Y_valid, Y_hetero, Y_endog, X, A)
}


# True population coefficient
beta_true <- 0.50

R <- 500

results <- data.frame(
  replication = integer(),
  model = character(),
  beta_hat = numeric(),
  se_conventional = numeric(),
  se_HC1 = numeric()
)

for (r in 1:R) {

  # Generate one sample
  dat <- make_sample(seed + r)
  

  # Run regression
  valid <- lm(Y_valid ~ X, data = dat)
  hetero <- lm(Y_hetero ~ X, data = dat)
  omitted <- lm(Y_endog ~ X, data = dat)
  controlled <- lm(Y_endog ~ X + A, data = dat)

  # Put models in a list
  models <- list(
    Valid = valid,
    Heteroskedastic = hetero,
    Omitted = omitted,
    Controlled = controlled
  )

  for (model_name in names(models)) {

    model <- models[[model_name]]

    # Estimated coefficient on X
    beta_hat <- coef(model)["X"]

    # Conventional standard error
    se_conv <- summary(model)$coefficients["X", "Std. Error"]

    # HC1 robust standard error
    se_hc1 <- sqrt(diag(vcovHC(model, type = "HC1")))["X"]


    # Save results
    results <- rbind(
      results,
      data.frame(
        replication = r,
        model = model_name,
        beta_hat = beta_hat,
        se_conventional = se_conv,
        se_HC1 = se_hc1
      )
    )
  }
}

# construct 95% confidence interval

results$lower_conv <- results$beta_hat - 1.96 * results$se_conventional
results$upper_conv <- results$beta_hat + 1.96 * results$se_conventional
results$lower_HC1 <- results$beta_hat - 1.96 * results$se_HC1
results$upper_HC1 <- results$beta_hat + 1.96 * results$se_HC1

# Check CI coverage
results$cover_conv <-
  results$lower_conv <= beta_true &
  results$upper_conv >= beta_true


results$cover_HC1 <-
  results$lower_HC1 <= beta_true &
  results$upper_HC1 >= beta_true

# Monte Carlo Summary Table

cat("\n====================\nPART B.1\n====================\n")

models_names <- unique(results$model)

mc_table <- data.frame()

for (m in models_names) {

  tmp <- results[results$model == m, ]

  row <- data.frame(
    Model = m,

    Mean_Estimate =
      mean(tmp$beta_hat),

    Empirical_Bias =
      mean(tmp$beta_hat) - beta_true,

    Monte_Carlo_SD =
      sd(tmp$beta_hat),

    Avg_Conventional_SE =
      mean(tmp$se_conventional),

    Avg_HC1_SE =
      mean(tmp$se_HC1),

    Conventional_Coverage =
      mean(tmp$cover_conv),

    HC1_Coverage =
      mean(tmp$cover_HC1)
  )

  mc_table <- rbind(mc_table, row)
}

# Round to three decimals
mc_table[, -1] <- round(mc_table[, -1], 3)

print(mc_table)

doc <- body_add_par(doc, "Monte Carlo Simulation Results")

ft_mc <- flextable(mc_table)
ft_mc <- autofit(ft_mc)

doc <- body_add_flextable(doc, ft_mc)


plot_data <- results[results$model %in% c("Valid", "Omitted"),]

# plot sampliny distribution
p <- ggplot(
  plot_data,
  aes(
    x = beta_hat,
    linetype = model
  )
) +
  geom_density(
    linewidth = 1
  ) +
  geom_vline(
    xintercept = 0.50,
    linetype = "dashed",
    linewidth = 1
  ) +
  annotate(
    "text",
    x = 0.50,
    y = Inf,
    label = "True beta = 0.50",
    vjust = 2,
    hjust = -0.1
  ) +
  labs(
    title = "Sampling Distributions: Valid vs Omitted Model",
    x = "Estimated coefficient on X",
    y = "Density",
    linetype = "Model"
  ) +
  theme_minimal()

print(p)

ggsave(
  "part_B_sampling_distribution.png",
  plot = p,
  width = 7,
  height = 5,
  dpi = 300
)


cat("\n====================\nPART B.4\n====================\n")

# True coefficients from the DGP
beta_X <- 0.50
beta_A <- 0.80

# From:
# X = 0.80 * A + error
#
# Var(A) = 1
# Var(error) = 1
# A is independent of error

cov_XA <- 0.80 * 1

var_X <- (0.80^2) * 1 + 1

# Theoretical probability limit
plim_beta_X <- beta_X + beta_A * (cov_XA / var_X)

cat("Cov(X, A):", cov_XA, "\n")
cat("Var(X):", var_X, "\n")
cat("Theoretical plim of beta_X:", plim_beta_X, "\n")

# Simulated mean estimate from Omitted model
omitted_mean <- mean(
  results$beta_hat[results$model == "Omitted"]
)

cat("Simulated mean estimate:", omitted_mean, "\n")
cat("Theoretical probability limit:", plim_beta_X, "\n")
cat("Difference:", omitted_mean - plim_beta_X, "\n")

comparison_b4 <- data.frame(
  Measure = c(
    "Theoretical probability limit",
    "Simulated mean estimate",
    "Difference"
  ),
  Value = c(
    plim_beta_X,
    omitted_mean,
    omitted_mean - plim_beta_X
  )
)

comparison_b4$Value <- formatC(
  comparison_b4$Value,
  format = "f",
  digits = 3
)

print(comparison_b4)

ft_b4 <- flextable(comparison_b4)
ft_b4 <- autofit(ft_b4)

doc <- body_add_par(
  doc,
  "B.4 Theoretical Probability Limit"
)

doc <- body_add_flextable(
  doc,
  ft_b4
)

cat("\n====================\nPART B.6\n====================\n")


R_large <- 300

results_large <- data.frame(
  replication = integer(),
  model = character(),
  beta_hat = numeric()
)

for (r in 1:R_large) {

  # Generate sample with n = 1000
  dat <- make_sample(seed + r, n = 1000)

  # Run the two regressions
  valid <- lm(Y_valid ~ X, data = dat)

  omitted <- lm(Y_endog ~ X, data = dat)

  # Save estimated beta_X
  results_large <- rbind(
    results_large,

    data.frame(
      replication = r,
      model = "Valid",
      beta_hat = coef(valid)["X"]
    ),

    data.frame(
      replication = r,
      model = "Omitted",
      beta_hat = coef(omitted)["X"]
    )
  )
}

# True beta
beta_true <- 0.50
models_large <- c("Valid", "Omitted")
summary_large <- data.frame()

for (m in models_large) {
  tmp <- results_large[results_large$model == m,]
  row <- data.frame(
    Model = m,
    Mean_Estimate = mean(tmp$beta_hat),
    Empirical_Bias = mean(tmp$beta_hat) - beta_true,
    Monte_Carlo_SD = sd(tmp$beta_hat)
  )

  summary_large <- rbind(
    summary_large,
    row
  )
}

summary_large[, -1] <- round(summary_large[, -1], 3)

print(summary_large)

summary_small <- data.frame()

for (m in c("Valid", "Omitted")) {

  tmp <- results[results$model == m,]

  row <- data.frame(
    Model = m,
    Mean_Estimate = mean(tmp$beta_hat),
    Empirical_Bias = mean(tmp$beta_hat) - beta_true,
    Monte_Carlo_SD = sd(tmp$beta_hat)
  )

  summary_small <- rbind(
    summary_small,
    row
  )
}

comparison_b6 <- rbind(
  data.frame(
    Sample_Size = 250,
    summary_small
  ),
  data.frame(
    Sample_Size = 1000,
    summary_large
  )
)

comparison_b6[, 3:5] <- round(comparison_b6[, 3:5], 3)

print(comparison_b6)

doc <- body_add_par(
  doc,
  "B.6 Effect of Increasing Sample Size"
)

ft_b6 <- flextable(comparison_b6)
ft_b6 <- autofit(ft_b6)

doc <- body_add_flextable(
  doc,
  ft_b6
)

## part C prep
set.seed(seed + 9000)
T <- 360
Sigma <- matrix(c(
  0.0016, 0.0003, -0.0002,
  0.0003, 0.0009, 0.00015,
  -0.0002, 0.00015, 0.0009
), nrow = 3, byrow = TRUE)
Z <- matrix(rnorm(T * 3), nrow = T) %*% chol(Sigma)
MKT <- 0.005 + Z[, 1]
SMB <- 0.002 + Z[, 2]
HML <- 0.003 + Z[, 3]
innov <- (0.018 + 0.25 * abs(MKT)) * rnorm(T)
eps <- as.numeric(stats::filter(innov,
  filter = 0.45,
  method = "recursive"
))
asset_excess <- 0.001 + 1.15 * MKT + 0.45 * SMB -
  0.30 * HML + eps
dat_F <- data.frame(asset_excess, MKT, SMB, HML)


cat("\n====================\nPART C.1\n====================\n")

# CAPM
capm <- lm(
  asset_excess ~ MKT,
  data = dat_F
)

sum_capm <- summary(capm)


# FAMA-FRENCH 3 FACTOR

ff3 <- lm(
  asset_excess ~ MKT + SMB + HML,
  data = dat_F
)

sum_ff3 <- summary(ff3)

coef_capm <- coef(sum_capm)
coef_ff3 <- coef(sum_ff3)

capm_table <- data.frame(
  Model = "CAPM",
  Variable = rownames(coef_capm),
  Coefficient = coef_capm[, "Estimate"],
  Std_Error = coef_capm[, "Std. Error"]
)

ff3_table <- data.frame(
  Model = "Fama-French 3 Factor",
  Variable = rownames(coef_ff3),
  Coefficient = coef_ff3[, "Estimate"],
  Std_Error = coef_ff3[, "Std. Error"]
)

capm_table$Adjusted_R2 <- sum_capm$adj.r.squared
ff3_table$Adjusted_R2 <- sum_ff3$adj.r.squared

c1_table <- rbind(
  capm_table,
  ff3_table
)

c1_table[, c(
  "Coefficient",
  "Std_Error",
  "Adjusted_R2"
)] <- lapply(
  c1_table[, c(
    "Coefficient",
    "Std_Error",
    "Adjusted_R2"
  )],
  function(x) round(x, 3)
)

print(c1_table)

ft_c1 <- flextable(c1_table)
ft_c1 <- autofit(ft_c1)

doc <- body_add_par(
  doc,
  "C.1 CAPM and Fama-French Three-Factor Regression"
)

doc <- body_add_flextable(
  doc,
  ft_c1
)

cat("\n====================\nPART C.5\n====================\n")

# Function to get alpha results from one model
get_alpha_results <- function(model, model_name) {

  # Alpha estimate = intercept
  alpha <- coef(model)["(Intercept)"]

  # Conventional
  conventional <- summary(model)$coefficients["(Intercept)", ]

  se_conv <- conventional["Std. Error"]
  p_conv <- conventional["Pr(>|t|)"]

  # HC1
  hc1_test <- coeftest(
    model,
    vcov. = vcovHC(model, type = "HC1")
  )

  se_hc1 <- hc1_test["(Intercept)", "Std. Error"]
  p_hc1 <- hc1_test["(Intercept)", "Pr(>|t|)"]

  # Newey-West
  # lag = 4
  nw_test <- coeftest(
    model,
    vcov. = NeweyWest(
      model,
      lag = 4,
      prewhite = FALSE,
      adjust = TRUE
    )
  )

  se_nw <- nw_test["(Intercept)", "Std. Error"]
  p_nw <- nw_test["(Intercept)", "Pr(>|t|)"]

  # Table
  data.frame(
    Model = model_name,
    Alpha = alpha,

    Conventional_SE = se_conv,
    Conventional_P = p_conv,

    HC1_SE = se_hc1,
    HC1_P = p_hc1,

    NeweyWest_SE = se_nw,
    NeweyWest_P = p_nw
  )
}

capm_alpha <- get_alpha_results(
  capm,
  "CAPM"
)

ff3_alpha <- get_alpha_results(
  ff3,
  "Fama-French 3 Factor"
)

c4_table <- rbind(
  capm_alpha,
  ff3_alpha
)

print(c4_table)

c4_table[, -1] <- lapply(
  c4_table[, -1],
  function(x) formatC(
    x,
    format = "f",
    digits = 3
  )
)

print(c4_table)

ft_c4 <- flextable(c4_table)
ft_c4 <- autofit(ft_c4)

doc <- body_add_par(
  doc,
  "C.4 Alpha Standard Errors and P-values"
)

doc <- body_add_flextable(
  doc,
  ft_c4
)

# Newey-West covariance matrix for FF3 model
nw_vcov <- NeweyWest(
  ff3,
  lag = 4,
  prewhite = FALSE,
  adjust = TRUE
)
cat("\n====================\nPART C.6\n====================\n")

# Joint Wald test:
# H0: beta_SMB = 0 and beta_HML = 0
wald_c5 <- linearHypothesis(
  ff3,
  c("SMB = 0", "HML = 0"),
  vcov. = nw_vcov,
  test = "Chisq"
)

print(wald_c5)

# Extract Wald test result
wald_table <- data.frame(
  Statistic = "Wald Chi-square",
  Value = wald_c5$Chisq[2],
  df = wald_c5$Df[2],
  P_Value = wald_c5$`Pr(>Chisq)`[2]
)

# # Format
# wald_table$Value <- formatC(
#   wald_table$Value,
#   format = "f",
#   digits = 3
# )

# wald_table$P_Value <- formatC(
#   wald_table$P_Value,
#   format = "f",
#   digits = 3
# )

print(wald_table)

ft_c5 <- flextable(wald_table)
ft_c5 <- autofit(ft_c5)

doc <- body_add_par(
  doc,
  "C.5 Joint Wald Test: H0: SMB = HML = 0"
)

doc <- body_add_flextable(
  doc,
  ft_c5
)

# print 
print(doc, target = "assignment1.docx")


