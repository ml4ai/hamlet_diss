source("format.R")

### Generate data
set.seed(5)
n <- 24
noise.sigma <- 1
coef.sigma <- c(1, 1, 6)
xmin <- -5; xmax <- 5
x <- sort(runif(n, xmin, xmax))
b <- rnorm(3, 0, coef.sigma)
## X <- cbind(1, x, x^2, x^3, x^4, x^5, x^6, x^7, x^8, x^9, x^10, x^11)
X <- poly(x, degree = round(n/2) - 1)
y.fit <- cbind(1,X[,1:2]) %*% b
noise <- rnorm(n, 0, noise.sigma)
y <- y.fit + noise
i <- sort(sample.int(n, round(n/2)))

models <- list()
errors <- numeric(0)
test.errors <- numeric(0)

for(k in 1:(round(n/2) - 1))
{
    models[[k]] <- lm(y[i] ~ X[i,1:k])
    errors[[k]] <- sum(residuals(models[[k]])^2) / n
    test.pred <- cbind(1, X[-i, 1:k]) %*% coef(models[[k]])
    test.errors[[k]] <- sum((y[-i] - test.pred)^2) / n
}

xmesh <- seq(xmin, xmax, 0.001)
Xmesh <- predict(X, xmesh)

plot_train <-
    function(
        plot.points = TRUE,
        degree = 1,
        color = "bg",
        with.test = FALSE
        )
{
    set_pars()
    par(
        ## mfrow = c(1,2),
        mgp = c(2, 1, 0)
        )
    if(plot.points)
    {
        plot(x[i], y[i], xlim = c(xmin, xmax), ylim = c(-20, 20), xlab = "", ylab = "",
             xaxt = "n", yaxt = "n", pch = 16)
        lines(
            xmesh,
            cbind(1, Xmesh[,1:degree]) %*% coef(models[[degree]]),
            col = solar[color]
            )
    }
    if(plot.points & with.test)
    {
        points(x[-i], y[-i], col = solar["violet"])
    }
    m <- ifelse(with.test, 3, 1)
    if(!plot.points)
    {
        plot(
            1:degree, errors[1:degree], type = "b",
            pch = 15, col = solar["red"],
            xlab = "Polynomial Degree",
            ylab = "Sum of Squared Errors",
            xlim = c(0, length(models)),
            ylim = c(0, m * max(errors)),
            xaxt = "n",
            yaxt = "n")
    }
    if(with.test & !plot.points)
    {
        lines(
            1:degree, test.errors[1:degree], type = "b",
            pch = 0, col = solar["violet"]
            )
        legend("topleft", col = solar[c("red","violet")],
               pch = c(15, 0), lty = 1,
               legend = c("In-Sample", "Out-Of-Sample")
               )
    }
    if(!plot.points)
    {
        axis(side = 1, at = 1:11, labels = 1:11)
        axis(side = 2,
             at = seq(min(errors), m * max(errors), length = 5*m + 1),
             labels = seq(0,m, length = 5*m + 1)
             )
    }
}


### Plot linear fit
pdf("../fig/train_linear.pdf", width = 5, height = 5)
plot_train(degree = 1, color = "blue")
dev.off()

### Plot quadratic fit
pdf("../fig/train_quadratic.pdf", width = 5, height = 5)
plot_train(degree = 2, color = "green")
dev.off()

### Plot over fit
pdf("../fig/train_overfit.pdf", width = 5, height = 5)
plot_train(degree = length(models), color = "orange")
dev.off()

### Plot linear fit with test
pdf("../fig/test_linear.pdf", width = 5, height = 5)
plot_train(degree = 1, color = "blue", with.test = TRUE)
dev.off()

### Plot quadratic fit with test
pdf("../fig/test_quadratic.pdf", width = 5, height = 5)
plot_train(degree = 2, color = "green", with.test = TRUE)
dev.off()

### Plot over fit with test
pdf("../fig/test_overfit.pdf", width = 5, height = 5)
plot_train(degree = length(models), color = "orange", with.test = TRUE)
dev.off()

pdf("../fig/poly_errors.pdf", width = 5, height = 5)
plot_train(degree = length(models), color = "orange", with.test = TRUE, plot.points = FALSE)
dev.off()


