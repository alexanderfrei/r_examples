
x <- sample(1:10, 100, replace=T)
y <- x * runif(100, 1, 2)

df <- data.frame(x,y)
slope <- lm(formula = y ~ x, data = df)$coefficients[2]

nrow <- dim(df)[1]

b_slope <- vector(length = 1000)
for (i in 1:1000){
  x_boot <- sample(df$x, nrow, replace = T)
  y_boot <- sample(df$y, nrow, replace = T)
  b_slope[i] <- lm(y_boot ~ x_boot)$coefficients[2]
}

quant <- quantile(b_slope, c(0.025,0.975))
low <- quant[1]
high <- quant[2]

c(2*slope - high, 2*slope - low)


