df = data.frame(
  
  Fertilizer = c(100,200,300,400,500,600,700),
  Rainfall = c(10, 20, 10, 30, 20, 20, 30),
  yield = c(40,50,50,70,65,65,80)
)


print(df)
fit_model = lm(yield ~ Fertilizer + Rainfall, data = df)

print(summary(fit_model)) #summary for variable detail
cat("\n\n")
print(coefficients(fit_model)) #coefficent for b0 and b1,b2 values 
cat("\nY_hat\n")
print(fitted(fit_model)) #fitted for calculating Y hat values
cat("\n\n")
print(anova(fit_model)) #anova for complete anova table tells us how much depend a variable
cat("\n\n")
R2 = round(summary(fit_model)$r.squared, 3) #$ sign for fetching the values from summary (r2 values)
sigma2 = round(summary(fit_model)$sigma^2, 3)
print(paste("R^2 =", R2,
            collapse = " ")) #for space in center
print(paste("MSE =", sigma2,
            collapse = " "))

# Variance of coefficients
print(vcov(fit_model))

cat("\nConfidence Interval")
print(confint(fit_model)) #confitence interval +- values

scatter_plot = scatterplot3d(df, 
                     main= "Linear Regression Line",
                     xlab = "Fertilizer",
                     ylab = "Rainfall",
                     zlab = "Yield", color = "red"
)
scatter_plot$plane3d(fit_model)