library(ggplot2)
library(seas)
library(rnoaa)
library(dplyr)
library(ggpmisc)

# Define constants
start.yr <- 1990
end.yr <- 2020
var.nm <- "TMIN"  # met variable for analysis (see 'rnoaa' package for more information)
seas.width <- "DJF"  # width for season (see 'seas' package for more information) 
var.season <- "JJA"  # season for analysis

# Find stations that match criteria
(stations <- ghcnd_stations()) # UNCOMMENT TO GET STATION LIST
stns.f <- stations %>% 
  filter(state == "MT") %>% 
  filter(first_year < start.yr) %>%
  filter(last_year == end.yr) %>%
  filter(element == var.nm) %>%
  filter(wmo_id != "")  # can comment this out if you want more stations

# Get station ids and names that match criteria
stns.id <- stns.f[1]
stns.nm <- stns.f[6]

# Define start/end dates and create list to store data frames from for loop
start.date <- paste(as.character(start.yr), "-01-01", sep="")
end.date <- paste(as.character(end.yr), "-12-31", sep="")

# Loop through met stations that meet criteria
metv <- list()
stats <- list()
for(i in 1:nrow(stns.id)){
  stn.id <- stns.id[i,]$id
  stn.nm <- stns.nm[i,]$name
  
  # Shorten names 
  if (stn.nm == "BOZEMAN MONTANA STATE UNIV"){
    stn.nm <- "BOZEMAN MSU"
  } else if (stn.nm == "DILLON U OF MONTANA WESTERN"){
    stn.nm <- "DILLON UM W"
  } else if (stn.nm == "NORRIS MADISON PWR HOUSE"){
    stn.nm <- "NORRIS"
  } else if (stn.nm == "BILLINGS LOGAN INTL AP"){
    stn.nm <- "BILLINGS AP"
  } else if (stn.nm == "MILES CITY F WILEY FLD"){
    stn.nm <- "MILES CITY"
  } else if (stn.nm == "BUTTE BERT MOONEY AP"){
    stn.nm <- "BUTTE AP"
  } else if (stn.nm == "GREAT FALLS INTL AP"){
    stn.nm <- "GREAT FALLS AP"
  } else if (stn.nm == "KALISPELL GLACIER AP"){
    stn.nm <- "KALISPELL AP"
  }
  
  # Calculate seasonal data
  data <- ghcnd_search(stn.id, date_min = start.date, date_max = end.date, var = var.nm)
  data <- as.data.frame(data)
  colnames(data) <- c("id", var.nm, "date", "mflag", "qflag", "sflag")
  data$date <- as.Date(data$date, format="%Y-%m-%d")
  data$year <- format(data$date, "%Y")
  data$seas <- mkseas(x=data$date, width=seas.width)
  metv.seasons <- aggregate(formula(paste(var.nm, "~ seas + year")), data = data, mean)
  metv.season <- metv.seasons[metv.seasons$seas==var.season,]
  metv.season$year <- as.Date(metv.season$year, format="%Y")
  metv.season <- metv.season[,c("year", var.nm)]
  metv.season$id <- rep(stn.id, nrow(metv.season))
  metv.season$name <- rep(stn.nm, nrow(metv.season))
  
  # Convert units (only have TMIN and TMAX)
  if (var.nm == "TMIN" | var.nm == "TMAX"){
    metv.season[var.nm] <- (9/5)*(metv.season[var.nm]/10) + 32
  } else {
    "Please check units for this variable and add to code!"
  }
  
  # Add dataframe to list
  metv[[i]] <- metv.season
  
  # Calculate slope and p-value
  mod.summ <- summary(lm(paste(var.nm, "~year", sep=""), data=metv.season))
  pval <- mod.summ$coefficients[8]
  m <- mod.summ$coefficients[2]
  stats.season <- data.frame(id=stn.id, name=stn.nm, pval=pval, slope=m)
  stats.season$id <- as.character(stats.season$id)
  stats.season$name <- as.character(stats.season$name)
  stats[[i]] <- stats.season
}

# Append list of dataframes into one dataframe
metv.all <- bind_rows(metv)
stats.all <- bind_rows(stats)

# Set-up plot based on met variable and season
if(var.nm=="TMIN" & var.season=="JJA"){
  y.title <- "Avg. Daily Summer Tmin (°F)"
} else if (var.nm=="TMIN" & var.season=="DJF"){
  y.title <- "Avg. Daily Winter Tmin (°F)"
}
miny <- min(metv.all$TMIN) - 5
maxy <- max(metv.all$TMIN) + 5
ylim.vals <- c(miny, maxy)

# Facet plot
ggplot(metv.all, aes_string(x="year", y=var.nm)) + 
  geom_line() +
  geom_smooth(method='lm', formula=y~x) +
  stat_poly_eq(formula = y~x,
               aes(label = paste(..eq.label..)),
               parse = T, size=3, label.x='center', label.y='top') +
  stat_fit_glance(method = 'lm',
                  method.args = list(formula = y~x),
                  geom = 'text',
                  aes(label = paste("p = ", signif(..p.value.., digits = 2), sep = "")),
                  size = 3, label.x="center", label.y=miny+3) +
  scale_x_date(date_labels = "%Y", date_breaks = "10 year") + 
  labs(x="Year", y=y.title) + 
  ylim(ylim.vals) +
  facet_wrap(~name, ncol=8)


##################################################################3
# Plot single station
## ggplot(metv[[11]], aes_string(x="year", y=var.nm)) +
##   geom_line() +
##   geom_smooth(method='lm', formula=y~x) +
##   scale_x_date(date_labels = "%Y", date_breaks = "10 year") +
##   labs(x="Year", y=y.title)
