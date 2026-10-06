library(gt)
library(webshot2)

cat("\n====================\nNUMBER 1\n====================\n")

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

sum_est <- summary(mincer)
coef_tab <- coef(sum_est)
se <- sqrt(diag(vcov(mincer)))

print("start of mincer regression")
cat("\nR-squared:\n")
print(round3(sum_est$r.squared))

cat("\nAdjusted R-squared:\n")
print(round3(sum_est$adj.r.squared))

cat("\nSample size:\n")
print(round3(length(residuals(mincer))))

df_mincer <- data.frame(
  Variable = rownames(coef_tab),
  Coefficient = round3(coef_tab[, "Estimate"]),
  Std_Error = round3(se),
  P_Value = round3(coef_tab[, "Pr(>|t|)"])
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

# usual coefficient



