library(ggplot2)
set.seed(1)

x <- seq(-10, 10, 0.01)
y <- 1 - x ^ 2 + rnorm(length(x), 0, 5)

ggplot(data.frame(X = x, Y = y), aes(x = X, y = Y)) + geom_point() + geom_smooth(se = F)

x.squared <- x ^ 2
ggplot(data.frame(X = x.squared, Y = y), aes(x = X, y = Y)) + geom_point() + geom_smooth(method = 'lm', se = F)

summary(lm(y ~ x))$r.squared
summary(lm(y ~ x.squared))$r.squared

x <- seq(0, 1, 0.01)
y <- sin(2 * pi * x) + rnorm(length(x), 0, 0.1)

df <- data.frame(X = x, Y = y)
ggplot(df, aes(x = X, y = Y)) + geom_point() 

summary(lm(Y ~ X, data = df))
ggplot(df, aes(x = X, y = Y)) + geom_point() + geom_smooth(method = 'lm', se = F)

df <- transform(df, X2 = X ^ 2, X3 = X ^ 3)
summary(lm(Y ~ X + X2 + X3, data = >df))

df <- transform(df, X4 = X ^ 4, X5 = X ^ 5, X6 = X ^ 6, X7 = X ^ 7, X8 = X ^ 8, X9 = X ^ 9,
                X10 = X ^ 10, X11 = X ^ 11, X12 = X ^ 12, X13 = X ^ 13, X14 = X ^ 14, X15 = X ^ 15)
summary(lm(Y ~ X + X2 + X3 + X4 + X5 + X6 + X7 + X8 + X9 + X10 + X11 + X12 + X13 + X14, data = df))
summary(lm(Y ~ X + X2 + X3 + X4 + X5 + X6 + X7 + X8 + X9 + X10 + X11 + X12 + X13 + X14 + X15, data = df))

summary(lm(Y ~ poly(X, degree = 14), data = df))

poly.fit <- lm(Y ~ poly(X, dgree = 1), data = df)
df <- transform(df, PredictedY = predict(poly.fit))
ggplot(df, aes(x = X, y = PredictedY)) + geom_point() + geom_line()

poly.fit <- lm(Y ~ poly(X, dgree = 3), data = df)
df <- transform(df, PredictedY = predict(poly.fit))
ggplot(df, aes(x = X, y = PredictedY)) + geom_point() + geom_line()

poly.fit <- lm(Y ~ poly(X, dgree = 5), data = df)
df <- transform(df, PredictedY = predict(poly.fit))
ggplot(df, aes(x = X, y = PredictedY)) + geom_point() + geom_line()

poly.fit <- lm(Y ~ poly(X, dgree = 25), data = df)
df <- transform(df, PredictedY = predict(poly.fit))
ggplot(df, aes(x = X, y = PredictedY)) + geom_point() + geom_line()

set.seed(1)

x <- seq(0, 1, by = 0.01)
y <- sin(2 * pi * x) + rnorm(length(x), 0, 0.1)

n <- length(x)
indices <- sort(sample(1:n, round(0.5 * n)))

training.x <- x[indices]
training.y <- y[indices]

test.x <- x[-indices]
test.y <- y[-indices]

training.df <- data.frame(X = training.x, Y = training.y)
test.df <- data.frame(X = test.x, Y = test.y)

rmse <- function(y, h)
{
  return(sqrt(mean((y - h) ^ 2)))
}

performance <- data.frame()
for (d in 1:12)
{
  poly.fit <- lm(Y ~ poly(X, degree = d), data = training.df)

  performance <- rbind(performance,
                       data.frame(Degree = d,
                                  Data = 'Training',
                                  RMSE = rmse(training.y, predict(poly.fit))))

  performance <- rbind(performance,
                       data.frame(Degree = d,
                                  Data = 'Test',
                                  RMSE = rmse(test.y, predict(poly.fit,
                                                              newdata = test.df))))
}

ggplot(performance, aes(x = Degree, y = RMSE, linetype = Data)) +
  geom_point() +
  geom_line()

lm.fit <- lm(y ~x)
model.complexity <- sum(coef(lm.fit) ^ 2)

l2.model.complexity <- sum(coef(lm.fit) ^ 2)
l1.model.complexity <- sum(abs(coef(lm.fit)))

set.seed(1)

x <- seq(0, 1, by = 0.01)
y <- sin(2 * pi * x) + rnorm(length(x), 0, 0.1)

x <- matrix(x)

library('glmnet')
glmnet(x, y)
#glmnet(poly(x, degree = 10), y)

set.seed(1)

x <- seq(0, 1, by = 0.01)
y <- sin(2 * pi * x) + rnorm(length(x), 0, 0.1)

n <- length(x)
indices <- sort(sample(1:n, round(0.5 * n)))

training.x <- x[indices]
training.y <- y[indices]

test.x <- x[-indices]
test.y <- y[-indices]

df <- data.frame(X = x,Y = y)
training.df <- data.frame(X = training.x, Y = training.y)
test.df <- data.frame(X = test.x, Y = test.y)

glmnet.fit <- glmnet(poly(training.df$X, degree = 10), training.df$Y)
lambdas <- glmnet.fit$lambda

performance <- data.frame()

for(lambda in lambdas) {
    performance <- rbind(
        performance,
        data.frame(
            Lambda = lambda,
            RMSE = rmse(
                test.y, predict(
                    glmnet.fit, poly(test.df$X, degree = 10), s = lambda))))
}

ggplot(performance, aes(x = Lambda, y = RMSE)) + geom_point() + geom_line()
which(performance$RMSE == min(performance$RMSE))

best.lambda <- with(performance, Lambda[which(RMSE == min(RMSE))])
glmnet.fit <- glmnet(poly(df$X, degree = 10), df$Y)

coef(glmnet.fit, s = best.lambda)

ranks <- read.csv('data/oreilly.csv', stringsAsFactors = F)

library('tm')
documents <- data.frame(Text = ranks$Long.Desc)
row.names(documents) <- 1:nrow(documents)
corpus <- Corpus(DataframeSource(documents))
corpus <- tm_map(corpus, tolower)
corpus <- tm_map(corpus, stripWhitespace)
corpus <- tm_map(corpus, removeWords, stopwords('english'))

dtm <- DocumentTermMatrix(corpus)

x <- as.matrix(dtm)
y <- rev(1:100)


set.seed(1)
library('glmnet')

indices <- sample(1:100, 80)
training.x <- x[indices, ]
training.y <- y[indices]

test.x <- x[-indices, ]
test.y <- y[-indices]

glm.fit <- glmnet(training.x, training.y)
predicted.y <- predict(glm.fit, test.x, s = 0.1)
rmse <- sqrt(mean((predicted.y - test.y) ^ 2))


performance <- data.frame()

for(lambda in c(0.1, 0.25, 0.5, 1, 2, 5)) {
    for(i in 1:50) {
        indices <- sample(1:100, 80)
        training.x <- x[indices, ]
        training.y <- y[indices]

        test.x <- x[-indices, ]
        test.y <- y[-indices]

        glm.fit <- glmnet(training.x, training.y)
        predicted.y <- predict(glm.fit, test.x, s = lambda)
        rmse <- sqrt(mean((predicted.y - test.y) ^ 2))

        performance <- rbind(
            performance,
            data.frame(Lambda = lambda, Iteration = i, RMSE = rmse))
    }
}

head(performance)

#install.packages("Hmisc") 

ggplot(performance, aes(x = Lambda, y = RMSE)) +
    stat_summary(fun.data = 'mean_cl_boot', geom = 'errorbar') +
    stat_summary(fun.data = 'mean_cl_boot', geom = 'point')

y <- rep(c(1,0), each = 50)

regularized.fit <- glmnet(x, y, family = 'binomial')
regularized.fit 

regularized.fit <- glmnet(x, y)
regularized.fit <- glmnet(x, y, family = 'gaussian')
regularized.fit <- glmnet(x, y, family = 'binomial')

predict(regularized.fit, newx = x, s = 0.001)
ifelse(predict(regularized.fit, newx = x, s = 0.001) > 0, 1, 0)

library('boot')

inv.logit(predict(regularized.fit, newx = x, s = 0.001))


set.seed(1)

performance <- data.frame()

for(i in 1:250) {
    indices <- sample(1:100, 80)

    training.x <- x[indices, ]
    training.y <- y[indices]

    test.x <- x[-indices, ]
    test.y <- y[-indices]

    for(lambda in c(0.0001, 0.001, 0.0025, 0.005, 0.01, 0.025, 0.5, 0.1)) {
        glm.fit <- glmnet(training.x, training.y, family = 'binomial')
        predicted.y <- ifelse(predict(glm.fit, test.x, s = lambda) > 0, 1, 0)
        error.rate <- mean(predicted.y != test.y)

        performance <- rbind(performance, data.frame(Lambda = lambda, Iteration = i, ErrorRate = error.rate))
    }
}

ggplot(performance, aes(x = Lambda, y = ErrorRate)) +
    stat_summary(fun.data = 'mean_cl_boot', geom = 'errorbar') +
    stat_summary(fun.data = 'mean_cl_boot', geom = 'point') +
    scale_x_log10()
