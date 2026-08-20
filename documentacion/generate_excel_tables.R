library(writexl)

if (!dir.exists("documentation")) {
  dir.create("documentation", recursive = TRUE)
}

cuotas <- data.frame(
  Variable = c(
    "Sex", "Sex",
    "Age", "Age", "Age", "Age",
    "Education", "Education", "Education", "Education"
  ),
  Category = c(
    "Male", "Female",
    "18–29", "30–44", "45–59", "60 or older",
    "Basic or less", "Secondary", "Higher technical", "University or more"
  ),
  N = as.integer(c(
    6780956, 7416995,
    3074306, 4128793, 3396454, 3598398,
    4079158, 5874173, 1402491, 2842129
  )),
  Percent = round(c(
    47.76, 52.24,
    21.65, 29.08, 23.92, 25.34,
    28.73, 41.37, 9.88, 20.02
  ), 2),
  stringsAsFactors = FALSE
)

diseno_olas <- data.frame(
  Wave = c("Wave 1", "Wave 2", "Wave 3"),
  `Effective cases` = as.integer(c(4500, 2473, 1500)),
  `Retention from previous wave (%)` = c(NA_real_, 55.0, 60.7),
  `Cumulative retention from Wave 1 (%)` = c(100.0, 55.0, 33.3),
  `Quota control` = c(
    "Wave-1 quotas (Census 2024)",
    "Natural attrition",
    "Natural attrition"
  ),
  check.names = FALSE,
  stringsAsFactors = FALSE
)

write_xlsx(
  list(cuotas = cuotas),
  path = file.path("documentation", "cuotas.xlsx"),
  col_names = TRUE
)

write_xlsx(
  list(diseno_olas = diseno_olas),
  path = file.path("documentation", "diseno_olas.xlsx"),
  col_names = TRUE
)

cat("\n--- cuotas.xlsx ---\n")
print(head(cuotas, 10))

cat("\n--- diseno_olas.xlsx ---\n")
print(head(diseno_olas, 3))
