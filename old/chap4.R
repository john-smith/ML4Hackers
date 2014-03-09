library(tm)
library(ggplot2)
library(plyr)

data.path <- "../03-Classification/data/"
easyham.path <- paste(data.path, "easy_ham/", sep="")

msg.full <- function(path) {
    con <- file(path, open="rt", encoding="latin1")
    msg <- readLines(con)
    close(con)
    return(msg)
}

get.from <- function(msg.vec) {
    from <- msg.vec[grepl("From: ", msg.vec)]
    from <- strsplit(from, '[":<> ]')[[1]]
    from <- from[which(from != "" & from != " ")]
    return(from[grepl("@", from)][1])
}

get.msg <- function(msg.vec) {
    msg <- msg.vec[seq(which(msg.vec == "")[1] + 1, length(msg.vec), 1)]
    return(paste(msg, collapse="\n"))
}

get.subject <- function(msg.vec) {
    subj <- msg.vec[grepl("Subject: ", msg.vec)]
    if(length(subj) > 0) {
        return(strsplit(subj, "Subject: ")[[1]][2])
    } else {
        return("")
    }
}

get.date <- function(msg.vec) {
    date.grep <- grepl("^Date: ", msg.vec)
    date.grepl <- which(date.grep == T)
    date <- msg.vec[date.grepl[1]]
    date <- strsplit(date, "\\+|\\-|: ")[[1]][2]
    date <- gsub("^\\s+|\\s+$", "", date)
    return(strtrim(date, 25))
}

parse.email <- function(path) {
    full.msg <- msg.full(path)
    date <- get.date(full.msg)
    from <- get.from(full.msg)
    subj <- get.subject(full.msg)
    msg <- get.msg(full.msg)
    return(c(date, from, subj, msg, path))
}


easyham.docs <- dir(easyham.path)
easyham.docs <- easyham.docs[which(easyham.docs != "cmds")]
easyham.parse <- lapply(easyham.docs,
                        function(p) parse.email(paste(easyham.path, p, sep="")))
easyham.matrix <- do.call(rbind, easyham.parse)
allparse.df <- data.frame(easyham.matrix,stringsAsFactors=F)
names(allparse.df) <- c("Date", "From.EMail", "Subject", "Message", "Path")

date.converter <- function(dates, pattern1, pattern2) {
    #月、曜日を英語で扱えるようにロケールを設定
    lct <- Sys.getlocale("LC_TIME")
    Sys.setlocale("LC_TIME", "C")
    
    pattern1.convert <- strptime(dates, pattern1)
    pattern2.convert <- strptime(dates, pattern2)
    pattern1.convert[is.na(pattern1.convert)] <- pattern2.convert[is.na(pattern1.convert)]

    Sys.setlocale("LC_TIME", lct)
    return(pattern1.convert)
}

pattern1 <- "%a, %d %b %Y %H:%M:%S"
pattern2 <- "%d %b %Y %H:%M:%S"

allparse.df$Date <- date.converter(allparse.df$Date, pattern1, pattern2)

allparse.df$Subject <- tolower(allparse.df$Subject)
allparse.df$From.EMail <- tolower(allparse.df$From.EMail)

priority.df <- allparse.df[with(allparse.df, order(Date)), ]
priority.df$Date <- as.POSIXct(priority.df$Date)
priority.train <- priority.df[1:(round(nrow(priority.df) / 2)), ]

from.weight <- ddply(priority.train, .(From.EMail), summarise, Freq=length(Subject))
from.weight <- transform(from.weight, Weight=log(Freq + 1))

find.threads <- function(email.df) {
    response.threads <- strsplit(email.df$Subject, "re: ")
    is.thread <- sapply(response.threads, function(subj) ifelse(subj[1] == '', T, F))
    threads <- response.threads[is.thread]
    senders <- email.df$From.EMail[is.thread]
    threads <- sapply(threads, function(t) paste(t[2:length(t)], collapse="re: "))
    return(cbind(senders, threads))
}

threads.matrix <- find.threads(priority.train)

email.thread <- function(thread.matrix) {
    senders <- thread.matrix[, 1]
    senders.freq <- table(senders)
    senders.matrix <- cbind(names(senders.freq), senders.freq, log1p(senders.freq))
    senders.df <- data.frame(senders.matrix, stringsAsFactors=F)
    row.names(senders.df) <- 1:nrow(senders.df)
    names(senders.df) <- c("From.Email", "Freq", "Weight")
    senders.df$Freq <- as.numeric(senders.df$Freq)
    senders.df$Weight <- as.numeric(senders.df$Weight)
    return(senders.df)
}

senders.df <- email.thread(threads.matrix)

thread.counts <- function(thread, email.df) {
    thread.times <- email.df$Date[which(email.df$Subject == thread | email.df$Subject == paste("re:", thread))]
    freq <- length(thread.times)
    min.time <- min(thread.times)
    max.time <- max(thread.times)
    time.span <- as.numeric(difftime(max.time, min.time, units='secs'))
    if(freq < 2) {
        return(c(NA, NA, NA))
    } else {
        trans.weight <- freq / time.span
        log.trans.weight <- 10 + log(trans.weight, base = 10)
        return(c(freq, time.span, log.trans.weight))
    }
}

get.threads <- function(threads.matrix, email.df) {
    threads <- unique(threads.matrix[, 2])
    thread.counts <- lapply(threads, function(t) thread.counts(t, email.df))
    thread.matrix <- do.call(rbind, thread.counts)
    return(cbind(threads, thread.matrix))
}

thread.weights <- get.threads(threads.matrix, priority.train)
thread.weights <- data.frame(thread.weights, stringsAsFactors=F)
names(thread.weights) <- c("Thread", "Freq", "Response", "Weight")
thread.weights$Freq <- as.numeric(thread.weights$Freq)
thread.weights$Response <- as.numeric(thread.weights$Response)
thread.weights$Weight <- as.numeric(thread.weights$Weight)
thread.weights <- subset(thread.weights, is.na(thread.weights$Freq) == F)

term.counts <- function(term.vec, control) {
    vec.corpus <- Corpus(VectorSource(term.vec))
    vec.tdm <- TermDocumentMatrix(vec.corpus, control=control)
    return(rowSums(as.matrix(vec.tdm)))
}

thread.terms <- term.counts(thread.weights$Thread, control=list(stopwords=stopwords()))
thread.terms <- names(thread.terms)

term.weights <- sapply(thread.terms, function(t) mean(thread.weights$Weight[grepl(t, thread.weights$Thread, fixed=T)]))
term.weights <- data.frame(list(Term=names(term.weights), Weight=term.weights), stringsAsFactors=F, row.names=1:length(term.weights))

msg.terms <- term.counts(priority.train$Message, control=list(stopwords=stopwords(), removePunctuation=T, removeNumbers=T))
msg.weight <- data.frame(list(Term=names(msg.terms), Weight=log(msg.terms, base=10)), stringsAsFactors=F, row.names=1:length(msg.terms))
msg.weights <- subset(msg.weight, Weight > 0)


get.weights <- function(search.term, weight.df, term=T) {
    if(length(search.term) > 0) {
        if(term) {
            term.match <- match(names(search.term), weight.df$Term)
        } else {
            term.match <- match(search.term, weight.df$Thread)
        }
        print(term)
        match.weights <- weight.df$Weight[which(!is.na(term.match))]
        if(length(match.weights) < 1) {
            return(1)
        } else {
            return(mean(match.weights))
        }
    } else {
        return(1)
    }
}

rank.message <- function(path) {
    msg <- parse.email(path)

    from <- ifelse(length(which(from.weight$From.EMail == msg[2])) > 0,
                   from.weight$Weight[which(from.weight$From.EMail == msg[2])], 1)

    thread.from <- ifelse(length(which(senders.df$From.EMail == msg[2])) > 0,
                     sender.df$Weight[which(senders.df$From.EMail == msg[2])], 1)

    subj <- strsplit(tolower(msg[3]), "re: ")
    is.thread <- ifelse(subj[[1]][1] == "", T, F)
    if(is.thread) {
        activity <- get.weights(subj[[1]][2], thread.weights, term=F)
    } else {
        activity <- 1
    }

    thread.terms <- term.counts(msg[3], control=list(stopwords=stopwords()))
    thread.terms.weights <- get.weights(thread.terms, term.weights)

    msg.terms <- term.counts(msg[4], control=list(stopwords=stopwords(), removePunctuation=T, removeNumbers=T))
    msg.weights <- get.weights(msg.terms, msg.weights)

    rank <- prod(from, thread.from, activity, thread.terms.weights, msg.weights)

    return(c(msg[1], msg[2], msg[3], rank))
}

train.paths <- priority.df$Path[1:(round(nrow(priority.df) / 2))]
test.paths <- priority.df$Path[((round(nrow(priority.df) / 2)) + 1):nrow(priority.df)]

train.ranks <- lapply(train.paths, rank.message)
train.ranks.matrix <- do.call(rbind, train.ranks)
train.ranks.matrix <- cbind(train.paths, train.ranks.matrix, "TRAINING")
train.ranks.df <- data.frame(train.ranks.matrix, stringsAsFactors=F)
names(train.ranks.df) <- c("Message", "Date", "From", "Subj", "Rank", "Type")
train.ranks.df$Rank <- as.numeric(train.ranks.df$Rank)

priority.threshold <- median(train.ranks.df$Rank)

train.ranks.df$Priority <- ifelse(train.ranks.df$Rank >= priority.threshold, 1, 0)
