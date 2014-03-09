library(ggplot2)
ufo <- read.delim("data/ufo/ufo_awesome.tsv", sep="\t", stringsAsFactors=F, header=F, na.strings="")

names(ufo) <- c("DateOccurred", "DateReported", "Location", "ShortDescription", "Duration", "LongDescription")

good.rows <- ifelse(nchar(ufo$DateOccurred) != 8 | nchar(ufo$DateReported) != 8, F, T)
ufo <- ufo[good.rows, ]

ufo$DateOccurred<-as.Date(ufo$DateOccurred, format="%Y%m%d")
ufo$DateReported<-as.Date(ufo$DateReported, format="%Y%m%d")

get.location <- function(l) {
    split.location <- tryCatch(strsplit(l, ",")[[1]], error= function(e) return(c(NA, NA)))
    clean.location <- gsub("^ ", "", split.location)
    if(length(clean.location) > 2) {
        return(c(NA, NA))
    } else {
        return(clean.location)
    }
}

city.state <- lapply(ufo$Location, get.location)
location.matrix <- do.call(rbind, city.state)
ufo <- transform(ufo, USCity=location.matrix[, 1], USState=tolower(location.matrix[, 2]), stringsAsFactors=F)

us.states <- c("ak", "al", "ar", "az", "ca", "co", "ct", "de", "fl", "ga", "hi", "ia", "id",
               "il", "in", "ks", "ky", "la", "ma", "md", "me", "mi", "mn", "mo", "ms", "mt",
               "nc", "nd", "ne", "nh", "nj", "nm", "nv", "ny", "oh", "ok", "or", "pa", "ri",
               "sc", "sd", "tn", "tx", "ut", "va", "vt", "wa", "wi", "wv", "wy")

ufo$USState <- us.states[match(ufo$USState, us.states)]
ufo$USCity[is.na(ufo$USState)] <- NA

ufo.us <- subset(ufo, !is.na(USState))

quick.hist <- ggplot(ufo.us, aes(x=DateOccurred))+geom_histogram()+scale_x_date(breaks="50 years")
ggsave(plot=quick.hist, filename="quick_hist.png", height=6, width=12)

ufo.us <- subset(ufo.us, DateOccurred >= as.Date("1990-01-01"))
quick.hist2 <- ggplot(ufo.us, aes(x=DateOccurred))+geom_histogram()+scale_x_date(breaks="5 years")
ggsave(plot=quick.hist, filename="quick_hist2.png", height=6, width=12)

ufo.us$YearMonth <- strftime(ufo.us$DateOccurred, format="%Y-%m")

library(plyr)
sightings.counts <- ddply(ufo.us, .(USState, YearMonth), nrow)

date.range <- seq.Date(from=as.Date(min(ufo.us$DateOccurred)), to=as.Date(max(ufo.us$DateOccurred)), by="month")
date.strings <- strftime(date.range, "%Y-%m")

states.dates <- lapply(us.states, function(s) cbind(s, date.strings))
states.dates <- data.frame(do.call(rbind, states.dates), stringsAsFactors=F)

all.sightings <- merge(states.dates, sightings.counts, by.x=c("s", "date.strings"), by.y=c("USState", "YearMonth"), all=T)
names(all.sightings) <- c("State", "YearMonth", "Sightings")
all.sightings$Sightings[is.na(all.sightings$Sightings)] <- 0
all.sightings$YearMonth <- as.Date(rep(date.range, length(us.states)))
all.sightings$State <- as.factor(toupper(all.sightings$State))

library(scales)
state.plot <- ggplot(all.sightings, aes(x=YearMonth, y=Sightings))+
    geom_line(aes(color="darkblue"))+
    facet_wrap(~State, nrow=10, ncol=5)+
    theme_bw()+
    scale_color_manual(values=c("darkblue"="darkblue"), guide="none")+
    scale_x_date(breaks="5 years", labels=date_format("%Y"))+
    xlab("Time")+
    ylab("Number of Sightings")+
    ggtitle("Number of UFO sightings by Month-Year and U.S. State(1990-2010)")
ggsave(plot=state.plot, filename="ufo_sightings.pdf", width=14, height=8.5)

print(state.plot)
