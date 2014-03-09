library(ggplot2)

df <- read.csv('data/df.csv')

ggplot(df, aes(x = X, y = Y, color = Label)) + geom_point()


logit.fit <- glm(Label ~ X + Y, family = binomial(link = 'logit'), data = df)
logit.predictions <- ifelse(predict(logit.fit) > 0, 1, 0)

mean(logit.predictions == df$Label)
mean(df$Label == 0)

ggplot(df, aes(x = X, y = Y, color = Label)) + geom_point() + geom_smooth()


library('e1071')

svm.fit <- svm(Label ~ X + Y, data = df)
svm.predictions <- ifelse(predict(svm.fit) > 0, 1, 0)
mean(svm.predictions == df$Label)


library("reshape")
df <- cbind(df, data.frame(Logit = ifelse(predict(logit.fit) > 0, 1 ,0)),
            SVM = ifelse(predict(svm.fit) > 0, 1, 0))

predictions <- melt(df, id.vars = c('X', 'Y'))

ggplot(predictions, aes(x = X, y = Y, color = factor(value))) + geom_point() + facet_grid(variable ~ .)

df <- df[, c("X", "Y", "Label")]

linear.svm.fit <- svm(Label ~ X + Y, data = df, kernel = 'linear')
mean(df$Label == ifelse(predict(linear.svm.fit) > 0, 1, 0))

polynomial.svm.fit <- svm(Label ~ X + Y, data = df, kernel = 'polynomial')
mean(df$Label == ifelse(predict(polynomial.svm.fit) > 0, 1, 0))

radial.svm.fit <- svm(Label ~ X + Y, data = df, kernel = 'radial')
mean(df$Label == ifelse(predict(radial.svm.fit) > 0, 1, 0))

sigmoid.svm.fit <- svm(Label ~ X + Y, data = df, kernel = 'sigmoid')
mean(df$Label == ifelse(predict(sigmoid.svm.fit) > 0, 1, 0))

df <- cbind(df,
            data.frame(LinearSVM = ifelse(predict(linear.svm.fit) > 0, 1, 0),
                       PolynomialSVM = ifelse(predict(polynomial.svm.fit) > 0, 1, 0),
                       RadialSVM = ifelse(predict(radial.svm.fit) > 0, 1, 0),
                       SigmoidSVM = ifelse(predict(sigmoid.svm.fit) > 0, 1, 0)))

predictions <- melt(df, id.vars = c('X', 'Y'))

ggplot(predictions, aes(x = X, y = Y, color = factor(value))) + geom_point() + facet_grid(variable ~ .)


polynomial.degree3.svm.fit <- svm(Label ~ X + Y, data = df, kernel = 'polynomial', degree = 3)
mean(df$Label != ifelse(predict(polynomial.degree3.svm.fit) > 0, 1, 0))

polynomial.degree5.svm.fit <- svm(Label ~ X + Y, data = df, kernel = 'polynomial', degree = 5)
mean(df$Label != ifelse(predict(polynomial.degree5.svm.fit) > 0, 1, 0))

polynomial.degree10.svm.fit <- svm(Label ~ X + Y, data = df, kernel = 'polynomial', degree = 10)
mean(df$Label != ifelse(predict(polynomial.degree10.svm.fit) > 0, 1, 0))

polynomial.degree12.svm.fit <- svm(Label ~ X + Y, data = df, kernel = 'polynomial', degree = 12)
mean(df$Label != ifelse(predict(polynomial.degree12.svm.fit) > 0, 1, 0))

df <- df[, c("X", "Y", "Label")]

df <- cbind(df,
            data.frame(Degree3SVM = ifelse(predict(polynomial.degree3.svm.fit) > 0, 1, 0),
                       Degree5SVM = ifelse(predict(polynomial.degree5.svm.fit) > 0, 1, 0),
                       Degree10SVM = ifelse(predict(polynomial.degree10.svm.fit) > 0, 1, 0),
                       Degree12SVM = ifelse(predict(polynomial.degree12.svm.fit) > 0, 1, 0)))

predictions <- melt(df, id.vars = c('X', 'Y'))

ggplot(predictions, aes(x = X, y = Y, color = factor(value))) + geom_point() + facet_grid(variable ~ .)


radial.cost1.svm.fit <- svm(Label ~ X + Y, data = df, kernel = 'radial', cost = 1)
mean(df$Label == ifelse(predict(radial.cost1.svm.fit) > 0, 1, 0))

radial.cost2.svm.fit <- svm(Label ~ X + Y, data = df, kernel = 'radial', cost = 2)
mean(df$Label == ifelse(predict(radial.cost2.svm.fit) > 0, 1, 0))

radial.cost3.svm.fit <- svm(Label ~ X + Y, data = df, kernel = 'radial', cost = 3)
mean(df$Label == ifelse(predict(radial.cost3.svm.fit) > 0, 1, 0))

radial.cost4.svm.fit <- svm(Label ~ X + Y, data = df, kernel = 'radial', cost = 4)
mean(df$Label == ifelse(predict(radial.cost4.svm.fit) > 0, 1, 0))

df <- df[, c("X", "Y", "Label")]

df <- cbind(df,
            data.frame(Cost1SVM = ifelse(predict(radial.cost1.svm.fit) > 0, 1, 0),
                       Cost2SVM = ifelse(predict(radial.cost2.svm.fit) > 0, 1, 0),
                       Cost3SVM = ifelse(predict(radial.cost3.svm.fit) > 0, 1, 0),
                       Cost4SVM = ifelse(predict(radial.cost4.svm.fit) > 0, 1, 0)))

predictions <- melt(df, id.vars = c('X', 'Y'))

ggplot(predictions, aes(x = X, y = Y, color = factor(value))) + geom_point() + facet_grid(variable ~ .)


sigmoid.gamma1.svm.fit <- svm(Label ~ X + Y, data = df, kernel = 'sigmoid', gamma = 1)
mean(df$Label == ifelse(predict(sigmoid.gamma1.svm.fit) > 0, 1, 0))

sigmoid.gamma2.svm.fit <- svm(Label ~ X + Y, data = df, kernel = 'sigmoid', gamma = 2)
mean(df$Label == ifelse(predict(sigmoid.gamma2.svm.fit) > 0, 1, 0))

sigmoid.gamma3.svm.fit <- svm(Label ~ X + Y, data = df, kernel = 'sigmoid', gamma = 3)
mean(df$Label == ifelse(predict(sigmoid.gamma3.svm.fit) > 0, 1, 0))

sigmoid.gamma4.svm.fit <- svm(Label ~ X + Y, data = df, kernel = 'sigmoid', gamma = 4)
mean(df$Label == ifelse(predict(sigmoid.gamma4.svm.fit) > 0, 1, 0))

df <- df[, c("X", "Y", "Label")]

df <- cbind(df,
            data.frame(Gamma1SVM = ifelse(predict(sigmoid.gamma1.svm.fit) > 0, 1, 0),
                       Gamma2SVM = ifelse(predict(sigmoid.gamma2.svm.fit) > 0, 1, 0),
                       Gamma3SVM = ifelse(predict(sigmoid.gamma3.svm.fit) > 0, 1, 0),
                       Gamma4SVM = ifelse(predict(sigmoid.gamma4.svm.fit) > 0, 1, 0)))
                       
predictions <- melt(df, id.vars = c('X', 'Y'))

ggplot(predictions, aes(x = X, y = Y, color = factor(value))) + geom_point() + facet_grid(variable ~ .)


load('data/dtm.RData')
set.seed(1)

training.indices <- sort(sample(1:nrow(dtm), round(0.5 * nrow(dtm))))
test.indices <- which(!1:nrow(dtm) %in% training.indices)
train.x <- dtm[training.indices, 3:ncol(dtm)]
train.y <- dtm[training.indices, 1]
test.x <- dtm[test.indices, 3:ncol(dtm)]
test.y <- dtm[test.indices, 1]

rm(dtm)

library('glmnet')
regularized.logit.fit <- glmnet(train.x, train.y, family = c('binomial'))

lambdas <- regularized.logit.fit$lambda
performance <- data.frame()

for(lambda in lambdas) {
    predictions <- predict(regularized.logit.fit, test.x, s = lambda)
    predictions <- as.numeric(predictions > 0)
    mse <- mean(predictions != test.y)
    
    performance <- rbind(performance, data.frame(Lambda = lambda, MSE = mse))
}

ggplot(performance, aes(x = Lambda, y = MSE)) + geom_point() + scale_x_log10()

best.lambda <- max(performance$Lambda[which(performance$MSE == min(performance$MSE))])

mse <- with(subset(performance, Lambda == best.lambda), MSE)
mse

linear.svm.fit <- svm(train.x, train.y, lernel = 'linear')

predictions <- predict(linear.svm.fit, test.x)
predictions <- as.numeric(predictions > 0)
mse <- mean(predictions != test.y)
mse

radial.svm.fit <- svm(train.x, train.y, kernel = 'radial')

predictions <- predict(radial.svm.fit, test.x)
predictions <- as.numeric(predictions > 0)
mse <- mean(predictions != test.y)
mse

library('class')

knn.fit <- knn(train.x, test.x, train.y, k = 50)
predictions <- as.numeric(as.character(knn.fit))
mse <- mean(predictions != test.y)
mse
