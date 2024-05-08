library(dplyr)
library(reshape2)
library(AER)
library(plm)
library(lmtest)
library(tseries)
library(sandwich)

combined_data <- read.csv("up_combined_data.csv")
combined_data <- na.omit(combined_data) # remove rows with NA values
# OLS model for the combined data
model <- lm(AvIncome ~ InfRate + School + EducExpen + Unempl  + log(GDPoecd), data = combined_data)
head(combined_data)
summary(model)

# Test reset
reset_result <- resettest(model, power = 2, type = "regressor")
print(reset_result)

# nowy wektor który będzie wektorem składnika losowego, reszt z modelu
residuals <- residuals(model)

# wykres reszt
png("myplot.png", width = 800, height = 600) # set width and height
plot(residuals)
dev.off()

log_GDPoecd <- log(combined_data$GDPoecd)
residuals_numeric <- as.numeric(residuals)
cov_log_GDPoecd_residuals <- cov(log_GDPoecd, residuals_numeric)

cov(log(combined_data$GDPoecd), residuals)
cov(combined_data$InfRate, residuals)
cov(combined_data$School, residuals)
cov(combined_data$EducExpen, residuals)
cov(combined_data$Unempl , residuals)
cov(combined_data$AvIncome, residuals)

# Badanie heteroskedastyczności
# Test Breuscha-Pagana
bptest(model)

# Test Goldfelda-Quandta
gqtest(model)

# Wykres reszt modelu
plot(fitted_values, residuals)

# Normalność składnika losowego
# Test Jarque-Bera
jarque.bera.test(residuals)

# Test na współliniowość
# Test VIF
vif(model)

print(residuals)

# Wartość oczekiwana składnika losowego
expected_value <- mean(residuals)
print(expected_value)

covariance_matrix <- cov(residuals, residuals)

# Macierz kowariancji
print(covariance_matrix)



mean_residuals <- mean(residuals)
n <- length(residuals)

n_numeric <- as.numeric(n) # convert n to a numeric value
cov_manual <- sum((residuals - mean_residuals) %*% (residuals - mean_residuals)) / (n - 1)
print(cov_manual)

# test White'a
white_test_result <- white.test(y = model$residuals, x = model$model)
print(white_test_result)

combined_data$ehat2 <- residuals^2
white.model= lm(ehat2 ~ InfRate + School + EducExpen + Unempl  + log(GDPoecd), data = combined_data)
summary(white.model)


# Test na endogeniczność
first_stage <- lm(log(GDPoecd) ~ EducExpen + School + Unempl + InfRate + life_exp, data = combined_data)

combined_data$LogGDP_hat <- predict(first_stage, combined_data)
second_stage <- lm(AvIncome ~ EducExpen + School + LogGDP_hat + Unempl + log(GDPoecd), data = combined_data)
ols_model <- lm(AvIncome ~ InfRate + School + EducExpen + Unempl + log(GDPoecd), data = combined_data)
hausman_test <- coeftest(second_stage, vcov. = vcovHC(second_stage)) - coeftest(ols_model, vcov. = vcovHC(ols_model))

summary(ols_model)
summary(second_stage)

hausman_test <- coeftest(second_stage, vcov. = vcovHC(second_stage, type = "HC3")) - 
                coeftest(ols_model, vcov. = vcovHC(ols_model, type = "HC3"))

print(hausman_test)

first_stage <- lm(log(GDPoecd) ~ EducExpen + School + Unempl + InfRate + life_exp, data = combined_data)

first_stage_fstat <- summary(first_stage)$fstatistic
print(first_stage_fstat)
# <10 - słaby instrument, nie ma konieczności korzystania z testu Hausmana ze względu na zbadaną kowariancję

