library(tidyverse)
library(readr)
library(estimatr)
library(modelsummary)
library(kableExtra)
library(fixest)

data610 <- read_csv("yamaguchi.csv")
glimpse(data610)

# データの絞り込み
data610 <- data610 |> 
  filter(year > 1999) |> 
  filter(hh.type == "all")


# 6-10-1
# 各モデルの推計を、lm_robust,feolsの2つの関数で実施
# (1)
model1 <- lm_robust(emp.rate ~ cap.rate, clusters = pref, se_type = "stata",
                    data = data610)
summary(model1)
model1f <- fixest::feols(emp.rate ~ cap.rate, cluster = ~pref, data = data610)
summary(model1f)

# (2)
model2 <- lm_robust(emp.rate ~ cap.rate, clusters = pref, 
                    fixed_effects = pref, se_type = "stata", data = data610)
summary(model2)
model2f <- fixest::feols(emp.rate ~ cap.rate | pref, cluster = ~pref,
                         data = data610)
summary(model2f)

# (3)
model3 <- lm_robust(emp.rate ~ cap.rate, clusters = pref,
                    fixed_effects = year, se_type = "stata", data = data610)
summary(model3)
model3f <- fixest::feols(emp.rate ~ cap.rate | year, cluster = ~pref,
                         data = data610)
summary(model3f)

# (4)
model4 <- lm_robust(emp.rate ~ cap.rate, clusters = pref,
                    fixed_effects = pref + year,
                    se_type = "stata", data = data610)
summary(model4)
model4f <- fixest::feols(emp.rate ~ cap.rate | pref + year, 
                         cluster = ~pref, data = data610)
summary(model4f)

# (5)
model5 <- lm_robust(emp.rate ~ cap.rate
                    + age + age.hus + emp.rate.hus + urate,
                    clusters = pref, fixed_effects = year,
                    se_type = "stata", data = data610)
summary(model5)
model5f <- fixest::feols(emp.rate ~ cap.rate
                         + age + age.hus + emp.rate.hus + urate | year,
                         cluster = ~pref, data = data610)
summary(model5f)

# (6)
model6 <- lm_robust(emp.rate ~ cap.rate
                    + age + age.hus + emp.rate.hus + urate,
                    clusters = pref, fixed_effects = pref + year,
                    se_type = "stata", data = data610)
summary(model6)
model6f <- fixest::feols(emp.rate ~ cap.rate
                         + age + age.hus + emp.rate.hus + urate
                         | pref + year,
                         cluster = ~pref, data = data610)
summary(model6f)


# 表形式で出力(modelsummary::msumaryを使用)
models_6_10 <- list("(1)" = model1,
                    "(2)" = model2,
                    "(3)" = model3,
                    "(4)" = model4,
                    "(5)" = model5,
                    "(6)" = model6)

cm <- c("cap.rate" = "保育所定員率",
        "age" = "母親平均年齢",
        "age.hus" = "父親平均年齢",
        "emp.rate.hus" = "父親就業率",
        "urate" = "失業率")

gm <- tribble(
  ~raw, ~clean, ~fmt,
  "adj.r.squared", "$\\bar{R}^2$", 2,
  "nobs", "サンプルサイズ", 0
)

modelsummary::msummary(
  models_6_10,
  coef_map = cm,
  gof_map = gm,
  stars = c("*" = 0.05, "**" = 0.01, "***" = 0.001),
  estimate = "{estimate}{stars}",
  output = "kableExtra",
  notes = "* p &lt; 0.05, ** p &lt; 0.01, *** p &lt; 0.001"
  ) |> 
  kableExtra::row_spec(0, extra_css = "border-bottom: 1.5px solid", ) |> 
  kableExtra::row_spec(12, extra_css = "border-bottom: 1.5px solid", )


# 表形式で出力(fixest::esttableを使用)
models_6_10f <- list("(1)" = model1f,
                     "(2)" = model2f,
                     "(3)" = model3f,
                     "(4)" = model4f,
                     "(5)" = model5f,
                     "(6)" = model6f)

keep = "%cap.rate|age|age.hus|emp.rate.hus|urate"

dict = c("cap.rate" = "保育所定員率",
         "age" = "母親平均年齢",
         "age.hus" = "父親平均年齢",
         "emp.rate.hus" = "父親就業率",
         "urate" = "失業率",
         "pref" = "都道府県効果",
         "year" = "年効果")

table610 <- fixest::esttable(
  models_6_10f,
  keep = keep,
  dict = dict,
  depvar = FALSE,
  digits = 3,
  digits.stats = 3,
  fitstat = ~ar2 + n,
  se.row = TRUE
)

colnames(table610)[1] <- " "
gt::gt(table610[-c(9, 10), ])



# 6-10-2
# lm_robustによる推定
model6_2cr <- lm_robust(
  emp.rate ~ cap.rate
  + age + age.hus + emp.rate.hus + urate + pref,
  clusters = pref, fixed_effects = year,
  se_type = "stata", data = data610)

model6_2r <- lm_robust(
  emp.rate ~ cap.rate
  + age + age.hus + emp.rate.hus + urate + pref,
  fixed_effects = year,
  se_type = "stata", data = data610)

summary(model6_2cr)
summary(model6_2r)


#feolsによる推定
model6f_2cr <- fixest::feols(
  emp.rate ~ cap.rate
  + age + age.hus + emp.rate.hus + urate + pref
  |year,
  cluster = ~pref, data = data610)

model6f_2r <- fixest::feols(
  emp.rate ~ cap.rate
  + age + age.hus + emp.rate.hus + urate + pref
  |year,
  vcov = "HC1", data = data610)

summary(model6f_2cr)
summary(model6f_2r)



# 6-10-3
fixest::feols(emp.rate ~ cap.rate | pref,
              cluster = ~pref, data = data610) |> 
  r2()


# 6-10-4
fixest::feols(emp.rate ~ cap.rate | pref + year,
              cluster = ~pref, data = data610) |> 
  r2()



