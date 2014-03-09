prices <- read.csv('data/stock_prices.csv', stringsAsFactors = F)
prices[1, ]

library('lubridate')
prices <- transform(prices, Date = ymd(Date))

library('reshape')
date.stock.matrix <- cast(prices, Date ~ Stock, value = 'Close')

prices <- subset(prices, Date != ymd('2002-02-01'))
prices <- subset(prices, Stock != 'DDR')

date.stock.matrix <- cast(prices, Date ~ Stock, value = 'Close')

cor.matrix <- cor(date.stock.matrix[, 2:ncol(date.stock.matrix)])
correlations <- as.numeric(cor.matrix)

library('ggplot2')

ggplot(data.frame(Correlations = correlations), aes(x = Correlations, fill = 1)) +
    geom_density() +
    opts(legend.position = 'none')

pca <- princomp(date.stock.matrix[, 2:ncol(date.stock.matrix)])
pca

principal.component <- pca$loadings[, 1]

loadings <- as.numeric(principal.component)

ggplot(data.frame(Loading = loadings), aes(x = Loading, fill = 1)) +
    geom_density() +
    opts(legend.position = 'none')


market.index <- predict(pca)[, 1]

dji.prices <- read.csv('data/DJI.csv', stringsAsFactors = F);
dji.prices <- transform(dji.prices, Date = ymd(Date))

dji.prices <- subset(dji.prices, Date > ymd('2001-12-31'))
dji.prices <- subset(dji.prices, Date != ymd('2002-02-01'))

dji <- rev(dji.prices$Close)
dates <- rev(dji.prices$Date)

comparson <- data.frame(Date = dates, MarketIndex = market.index, DJI = dji)

ggplot(comparson, aes(x = MarketIndex, y = DJI)) +
    geom_point() +
    geom_smooth(method = 'lm', se = F)

comparson <- transform(comparson, MarketIndex = -1 * MarketIndex)

ggplot(comparson, aes(x = MarketIndex, y = DJI)) +
    geom_point() +
    geom_smooth(method = 'lm', se = F)

alt.comparsion <- melt(comparson, id.vars = 'Date')
names(alt.comparsion) <- c('Date', 'Index', 'Price')

ggplot(alt.comparsion, aes(x = Date, y = Price, group = Index, color = Index)) +
    geom_point() +
    geom_line()

comparison <- transform(comparson, MarketIndex = scale(MarketIndex))
comparison <- transform(comparson, DJI = scale(DJI))
alt.comparison <- melt(comparison, id.vars = 'Date')
names(alt.comparison) <- c('Date', 'Index', 'Price')

p <- ggplot(alt.comparison, aes(x = Date, y = Price, group = Index, color = Index)) +
    geom_point() +
    geom_line()

print(p)
