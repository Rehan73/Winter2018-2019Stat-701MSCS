df = data.frame(
  
  x1 = c(0, 0, 10, 10, 20, 20),
  x2 = c(0, 0, 100, 100, 400, 400),
  y = c(5, 7, 15, 17, 9, 11)
)


print(df)
fit_model = lm(y ~ x1 + x2, data = df)

print(summary(fit_model)) #summary for variable detail
cat("\n\n")
print(coefficients(fit_model)) #coefficent for b0 and b1,b2 values 
cat("\nY_hat\n")
print(fitted(fit_model)) #fitted for calculating Y hat values
cat("\n\n")
print(anova(fit_model)) #anova for complete anova table tells us how much depend a variable
cat("\n\n")
R2 = round(summary(fit_model)$r.squared, 3) #$ sign for fetching the values from summary (r2 values)
mse = round(summary(fit_model)$sigma^2, 3)
print(paste("R^2 =", R2,
            collapse = " ")) #for space in center
print(paste("MSE =", mse,
            collapse = " "))

# Variance of coefficients
print(vcov(fit_model))

cat("\nConfidence Interval")
print(confint(fit_model)) #confitence interval +- values

scatter_plot = scatterplot3d(df, 
                     xlab = "x1",
                     ylab = "x2",
                     zlab = "y", color = "red"
)
scatter_plot$plane3d(fit_model)