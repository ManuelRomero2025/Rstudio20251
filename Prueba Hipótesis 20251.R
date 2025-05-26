library(BSDA)

# "greater", "less" or "two.sided"

diabetes=read_csv("diabetes_risk_dataset.csv")
diabetes=as.data.frame(unclass(diabetes),
                       stringsAsFactors = TRUE)

boxplot.stats(diabetes$age)

z.test(
  x=diabetes$age,
  alternative = "two.sided",
  mu = 49,
  sigma.x = sd(diabetes$age),
  conf.level = 0.95
)
