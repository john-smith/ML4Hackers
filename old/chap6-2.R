library(ggplot2)

set.seed(1)

x <- seq(0, 1, 0.01)
y <- sin(2 * pi * x) + rnorm(length(x), 0, 0.1)

n <- length(x)
indices <- sort(sample(1:n, round(0.5 * n)))

training.x <- x[indices]
training.y <- y[indices]

test.x <- x[-indices]
test.y <- y[-indices]

training.df <- data.frame(X = training.x, Y = training.y)
test.df <- data.frame(X = test.x, Y = test.y)

rmse <- function(y, h) {
    return(sqrt(mean((y - h) ^ 2)))
}

performance <- data.frame()
for(d in 1:12) {
    poly.fit <- lm(Y ~ poly(X, degree = d), data = training.df)
    performance <- rbind(performance, data.frame(
        Degree = d, Data = "Training", RMSE = rmse(training.y, predict(poly.fit))))
    
    performance <- rbind(performance, data.frame(
        Degree = d, Data = "Test", RMSE = rmse(test.y, predict(poly.fit, newdata = test.df))))
    
}

ggplot(performance, aes(x = Degree, y = RMSE, linetype = Data)) + geom_point() + geom_line()

lm.fit <- lm(y ~ x)
#lm.fit <- lm(y ~ poly(x, digree = 3))
model.complexity <- sum(coef(lm.fit) ^ 2)


l2.model.complexity <- sum(coef(lm.fit) ^ 2)
l1.model.complexity <- sum(abs(coef(lm.fit)))

set.seed(1)

x <- seq(0, 1, 0.01)
y <- sin(2 * pi * x) + rnorm(length(x), 0, 0.1)

x <- matrix(x)

library('glmnet')
glmnet(x, y)
