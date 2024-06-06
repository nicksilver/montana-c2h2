library(seas)

seas.width <- "DJF"
var.season <- "DJF"
var.nm <- 'TMIN'
start.year <- 1950
data <- read.csv("./2243395.csv")

plottrend <- function(data, seas.width, var.season, var.nm, start.year){
    data.clean <- data[c('DATE', 'TMAX', 'TMIN')]
    data.clean$DATE <- as.Date(data.clean$DATE, format="%Y-%m-%d")
    data.clean$year <- format(data.clean$DATE, "%Y")
    data.clean <- data.clean[data.clean$year >= start.year,]

    data.clean$seas <- mkseas(x=data.clean$DATE, width=seas.width)
    metv.seasons <- aggregate(formula(paste(var.nm, "~ seas + year")), data = data.clean, mean)
    metv.season <- metv.seasons[metv.seasons$seas==var.season,]
    metv.season$year <- as.Date(metv.season$year, format="%Y")
    metv.season <- metv.season[,c("year", var.nm)]

    mod <- lm(paste(var.nm, ' ~ year'), data = metv.season)
    plot(metv.season[,'year'], metv.season[,var.nm])
    abline(mod, col='red')
    return(mod)
}


mod <- plottrend(data, seas.width, var.season, var.nm, start.year)
summary(mod)
