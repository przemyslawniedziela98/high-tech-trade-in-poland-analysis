---
title: "High-tech trade analysis in Poland"
output:
  html_document:
    df_print: paged
  pdf_document: default
  github_document:
    html_preview: false
warnings: no
---
<b>Author:</b> Przemyslaw Niedziela (przemyslaw.niedziela98@gmail.com) <br> 
<b>Date:</b> August 2024 <br>

Everything explained in <br> [the analysis documentation](https://docs.google.com/document/d/1vRxHktMtdoP6s83Nn1n3_Xa-nBSd2VSwi1NKaAbl4TQ/edit) <br> 


```{r}
library(dplyr)
library(tidyr)
library(tseries)
library(urca)
library(data.table)
library(zoo)
library(ggplot2)
library(nortest)
library(lmtest)
library(car)

```

Reading datasets
```{r}
y1_raw <- read.csv("y1.csv")
x1_raw <- read.csv("x1.csv")
x2_raw <- read.csv("x2.csv")
x3_raw <- read.csv("x3.csv")
x4_raw <- read.csv("x4.csv")
x5_raw <- read.csv("x5.csv")
x6_raw <- read.csv("x6.csv")
x7_raw <- read.csv("x7.csv")
x8_raw <- read.csv("x8.csv")
x9_raw <- read.csv("x9.csv")
x10_raw <- read.csv("x10.csv")
```

Combining datasets 
```{r}
process_dataframe <- function(raw_df, variable_name, multiplier = 1) {
  raw_df %>%
    filter(geo == "PL") %>%
    select(TIME_PERIOD, OBS_VALUE) %>%
    pivot_wider(names_from = TIME_PERIOD, values_from = OBS_VALUE) %>%
    mutate(variable_name = variable_name) %>%
    select(variable_name, everything()) %>%
    mutate(across(where(is.numeric), ~ . * multiplier))
}

y1 <- y1_raw %>%
  rename(variable_name = freq.stk_flow.unit.partner.geo.TIME_PERIOD) %>%
  filter(variable_name %in% c("A,EXP,MIO_EUR,INT_EU27_2020,PL")) %>%
  mutate(variable_name = ifelse(variable_name %in% c("A,EXP,MIO_EUR,INT_EU27_2020,PL"), "y1", variable_name)) %>%
  rename_with(~ gsub("X", "", .), everything()) %>%
  mutate(across(where(is.numeric), ~ . * 1e6))

x1 <- process_dataframe(x1_raw, "x1")
x2 <- process_dataframe(x2_raw, "x2", 1000)
x3 <- process_dataframe(x3_raw, "x3")
x4 <- process_dataframe(x4_raw, "x4")
x5 <- process_dataframe(x5_raw, "x5")
x6 <- process_dataframe(x6_raw, "x6")
x7 <- process_dataframe(x7_raw, "x7")
x8 <- process_dataframe(x8_raw, "x8")
x9 <- process_dataframe(x9_raw, "x9")
x10 <- process_dataframe(x10_raw, "x10", 1e6)

raw_df = bind_rows(list(x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, y1))
```

## Filtering and handling missing values
Filtering values
```{r}
filter_years <- function(df, start_year, end_year) {
  year_columns <- names(df)[sapply(names(df), function(x) grepl("^\\d{4}$", x) && as.numeric(x) >= start_year && as.numeric(x) <= end_year)]
  
  df %>%
    select(variable_name, all_of(year_columns))
}

filtered_df <- filter_years(raw_df, 2013, 2022)
```

Missing values
```{r}
fill_missing_values <- function(df) {
  df %>%
    mutate(across(, ~ {
      col <- .
      if (any(is.na(col))) {
        if (is.na(col[1]) && is.na(col[2])) {
          if (length(col) > 2) {
            col[1] <- 3 * col[3] - 2 * col[4] 
            col[2] <- 2 * col[3] - col[4]  
          }
        } else if (is.na(col[1])) {
          if (length(col) > 2) {
            col[1] <- 2 * col[2] - col[3]
          }
        }
        col <- na.fill(col, "extend")
        col <- na.fill(col, "mean")
      }
      
      return(col)
    }))
}

filtered_df_raw = filtered_df %>%
    select(-variable_name) 
processed_df <- transpose(filtered_df_raw)
colnames(processed_df) <- filtered_df$variable_name
rownames(processed_df) <- colnames(filtered_df_raw)
processed_df <- processed_df %>% 
  mutate(year = colnames(filtered_df_raw)) 
processed_df <- processed_df[, c(ncol(processed_df), 1:(ncol(processed_df) - 1))]
processed_df = fill_missing_values(processed_df)

write.csv(processed_df, "processed_df.csv", row.names = FALSE)
```

Summary 
```{r}
summary(processed_df)
```
Stationarity tests
```{r}
for (col in names(processed_df)) {
  if (col != "year") {
    cat("Tests for ", col)
    col_data <- ts(processed_df[[col]])  
    adf_result <- adf.test(col_data)
    adf_gls_result <- ur.df(col_data, type = "drift", lags = 1)
    kpss_result <- kpss.test(col_data)
    
    print(adf_result)
    print(summary(adf_gls_result))
    print(kpss_result)
  }
}
```

Removal of non-stationarity with diff transformation
```{r}
processed_df_diff <- processed_df[-1, ]
for (col in names(processed_df)) {
  if (!(col %in% c("year"))) {
   processed_df_diff[[col]] <- diff(processed_df[[col]])
  }
}

plot(processed_df$year, processed_df$y1, type = "l", main = "y1", xlab = "lata", ylab = "y1")
plot(processed_df_diff$year, processed_df_diff$y1, type = "l", main = "y1 with diff transformation", xlab = "Czas", ylab = "y1_diff")
```


```{r}
for (col in names(processed_df_diff)) {
  if (col != "year") {
    cat("Tests for ", col)
    col_data <- ts(processed_df_diff[[col]])  
    adf_result <- adf.test(col_data)
    adf_gls_result <- ur.df(col_data, type = "drift", lags = 1)
    kpss_result <- kpss.test(col_data)
    
    print(adf_result)
    print(summary(adf_gls_result))
    print(kpss_result)
  }
}

write.csv(processed_df_diff, "diff_df.csv", row.names = FALSE)
```

Relational plots 
```{r}
for (var in paste0("x", 1:10)) {
  plot(processed_df_diff[[var]], processed_df_diff$y1, 
       main = paste("Relation of ", var, "and y1 (with regression)"),
       xlab = var, ylab = "y1", pch = 19, col = "red")
  
  model <- lm(processed_df_diff$y1 ~ processed_df_diff[[var]])
  abline(model, col = "blue")
  
  eq <- paste("Y =", round(coef(model)[1], 2), "+", round(coef(model)[2], 2), "X")
  legend("topleft", legend = eq, col = "blue", lty = 1, cex = 0.8)
}
```

Initial model
```{r}
model <- lm(y1 ~ x1 + x2 + x4 + x5 + x6 + x7 + x9 + x10, data = processed_df)

summary(model)
aic_value <- AIC(model)
bic_value <- BIC(model)
dw_test <- dwtest(model)

cat("AIC: ", aic_value, "\n")
cat("BIC: ", bic_value, "\n")
print(dw_test)
```

Hellwig method for variables selection 
```{r}
xlist <- list(
  x1 = processed_df$x1,
  x2 = processed_df$x2,
  x4 = processed_df$x4,
  x5 = processed_df$x5,
  x6 = processed_df$x6,
  x7 = processed_df$x7,
  x9 = processed_df$x9,
  x10 = processed_df$x10
)

Y = processed_df$y1

hellwig <- function(Y, xlist) {
  H <- 0
  for (i in 1:length(xlist)) {
    h <- (cor(Y, xlist[[i]]))^2
    s <- 0
    for (j in 1:length(xlist)) {
      s <- s + abs(cor(xlist[[i]], xlist[[j]]))
    }
    h <- h / s
    H <- H + h
  }
  return(H)
}

binary_addition <- function(n, position) {
  to_add <- 0
  for (i in 1:n) {
    if (to_add == 0) {
      if (position[i, 1] == 0) {
        position[i, 1] <- 1
        to_add <- 1
      } else {
        position[i, 1] <- 0
      }
    }
  }
  return(position)
}

n <- length(xlist)
position <- matrix(0, n, 1)
combinations <- 2^n - 1
best_list <- xlist
H_max <- hellwig(Y, xlist)

for (i in 1:combinations) {
  position <- binary_addition(n, position)
  current_list <- xlist
  for (j in 1:n) {
    if (position[j, 1] == 1) {
      current_list <- current_list[-j]
    }
  }
  H_current <- hellwig(Y, current_list)
  if (H_current > H_max) {
    H_max <- H_current
    best_list <- current_list
  }
}

print(H_max)
print(best_list)
```
Model with x3, x6, x10
```{r}
model_selected <- lm(y1 ~ x3 + x6 + x10, data = processed_df)

summary(model_selected)
aic_value <- AIC(model_selected)
bic_value <- BIC(model_selected)
dw_test <- dwtest(model_selected)

cat("AIC: ", aic_value, "\n")
cat("BIC: ", bic_value, "\n")
print(dw_test)
```

Residuals analysis
```{r}
residuals <- resid(model_selected)
ggplot(data.frame(residuals), aes(x = residuals)) +
  geom_histogram(aes(y = ..density..), bins = 5, fill = "gray", color = "black") +
  stat_function(fun = dnorm, args = list(mean = mean(residuals), sd = sd(residuals)), 
                color = "black", size = 1) +
  labs(title = "Residuals test", 
       x = "residuals", 
       y = "density") +
  theme_minimal()

breaks <- quantile(residuals, probs = seq(0, 1, by = 0.2))
cut_residuals <- cut(residuals, breaks = breaks, include.lowest = TRUE)

residual_table <- table(cut_residuals)
print(residual_table)
```

Autocorrelation analysis
```{r}
bg_test <- bgtest(model_selected, order = 4)
print(bg_test)

lb_test <- Box.test(residuals, lag = 4, type = "Ljung-Box")
print(lb_test)
```
Heteroskedasticity analysis
```{r}
processed_df$sq_x3 <- processed_df$x3^2
processed_df$sq_x6 <- processed_df$x6^2
processed_df$sq_x10 <- processed_df$x10^2

residuals_squared <- residuals(model_selected)^2
white_model <- lm(residuals_squared ~ x3 + x6 + x10 + sq_x3 + sq_x6 + sq_x10, data = processed_df)

summary(white_model)

TR2 <- nrow(processed_df) * summary(white_model)$r.squared
p_value_white <- 1 - pchisq(TR2, df = length(coefficients(white_model)) - 1)
cat("Test White'a: TR^2 =", TR2, "p-value =", p_value_white, "\n")


bp_test <- bptest(model_selected)
print(bp_test)

```

Collinearity test
```{r}
print(vif(model_selected))
```


Removing collinearity by removing x6
```{r}
model_selected_v1 <- lm(y1 ~ x3 + x10, data = processed_df)

summary(model_selected_v1)
aic_value <- AIC(model_selected_v1)
bic_value <- BIC(model_selected_v1)
dw_test <- dwtest(model_selected_v1)

cat("AIC: ", aic_value, "\n")
cat("BIC: ", bic_value, "\n")
print(dw_test)

breaks <- quantile(residuals, probs = seq(0, 1, by = 0.2))
cut_residuals <- cut(residuals, breaks = breaks, include.lowest = TRUE)

residual_table <- table(cut_residuals)
print(residual_table)

bg_test <- bgtest(model_selected_v1, order = 4)
print(bg_test)

lb_test <- Box.test(residuals, lag = 4, type = "Ljung-Box")
print(lb_test)

bp_test <- bptest(model_selected_v1)
print(bp_test)

print(vif(model_selected_v1))
```

RESET test 
```{r}
reset_test_all <- resettest(model_selected_v1, power = 2:3, type = "fitted")
print(reset_test_all)

reset_test_square <- resettest(model_selected_v1, power = 2, type = "fitted")
print(reset_test_square)

reset_test_cube <- resettest(model_selected_v1, power = 3, type = "fitted")
print(reset_test_cube)
```


