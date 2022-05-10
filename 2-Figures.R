library(tidyverse)
library(sf)
library(ggmap)

setwd("~/Projects/climate-swcl-prism-download/")

dat = read_csv("data/PRISM_monthly_1895-2020.csv")

wv_shp = read_sf("data/shapefiles/willamette_valley.shp")

# Get map of WV in Oregon
gkey <- read_file("Google_api_key.txt")
register_google(key = gkey)
wv_map = get_googlemap(c(-123.30412457626555, 44.673128831510105), zoom = 8) 

# Filter out data
dat = filter(dat, var == "tmax")
dat$char = nchar(dat$date)
dat = filter(dat, char == 6)

# Get month and year
dat$month = substring(dat$date, 5, 6)
dat$year = substring(dat$date, 1, 4)

# Look
head(dat)

# Aggregate
pdat = dat %>% 
  group_by(month) %>% 
  mutate(avg_value = mean(value)) %>% 
  group_by(year, month) %>% 
  summarise(value = mean(value),
            avg_value = mean(avg_value)) %>% 
  ungroup() %>% 
  mutate(anom_value = value - avg_value)


# --------------------------------------------------------------------------
# Tile plot
ggplot(pdat, aes(month, year, fill=value)) + 
  geom_tile() + 
  theme_minimal() +
  labs(x="Month", y=NULL, fill="Max \n Temp") +
  scale_y_discrete(breaks = seq(1885, 2020, 5)) +
  scale_fill_viridis_c() +  
  guides(fill = guide_colorbar(title.position = "top", 
                               direction = "vertical",
                               frame.colour = "black",
                               barwidth = .5,
                               barheight = 17)) +
  NULL

# --------------------------------------------------------------------------
# Create difference map in WV
cdat = dat %>% mutate(month = as.integer(month),
                       year = as.integer(year))

cdat$lat_lon = paste0(cdat$latitude, "_", cdat$longitude)

cdat2 = filter(cdat, var == "tmax" & month >= 4 & month <= 8 & year <= 1920)

cdat2$lat_lon = paste0(cdat2$latitude, "_", cdat2$longitude)

cdat2 = cdat2 %>% group_by(lat_lon) %>% summarise(avg_value = mean(value))

cdat3 = left_join(cdat, cdat2, by='lat_lon')

cdat4 = cdat3 %>% group_by(lat_lon) %>% mutate(diff = value - avg_value)

cdat5 = cdat4 %>% filter(year >= 2010) %>% group_by(lat_lon) %>% summarise(diff = mean(diff)) %>% separate(lat_lon, c("lat", "lon"), "_")

cdat5$lon = as.numeric(cdat5$lon)
cdat5$lat = as.numeric(cdat5$lat)

ggmap(wv_map) + 
  geom_tile(data=cdat5, aes(lon, lat, fill=abs(diff))) +
  geom_sf(data=wv_shp, inherit.aes = FALSE, fill=NA, color="black") + 
  theme_minimal() +
  # geom_tile() + 
  labs(x=NULL, y=NULL, fill='Apr-Aug \n 1920 Delta \n Tmax') +
  scale_fill_viridis_c() + 
  guides(fill = guide_colorbar(title.position = "top", 
                               direction = "vertical",
                               frame.colour = "black",
                               barwidth = 1,
                               barheight = 17)) +
  NULL





# --------------------------------------------------------------------------
# Daily data
dat2 = read_csv("data/PRISM_daily_1981-2020.csv")

# Subset
dat2 = filter(dat2, var == "tmax")
dat2$month = substring(dat2$date, 5, 6)
dat2$year = substring(dat2$date, 1, 4)
dat2$day = substring(dat2$date, 7, 8)

# Aggregate
pdat2 = dat2 %>% group_by(year, month, day) %>% summarise(value = mean(value))

# Get day of year
pdat2$date = paste0(pdat2$year, "-", pdat2$month, "-", pdat2$day)
pdat2$day_of_year = yday(pdat2$date)


ggplot(pdat2, aes(day_of_year, year, fill=value)) + 
  geom_tile() + 
  theme_minimal() +
  labs(x="Day of year", y=NULL, fill="Max \n Temp") +
  scale_y_discrete(breaks = seq(1981, 2020, 1)) +
  scale_x_continuous(breaks = seq(0, 365, 20)) +
  scale_fill_viridis_c() +  
  guides(fill = guide_colorbar(title.position = "top", 
                               direction = "vertical",
                               frame.colour = "black",
                               barwidth = .5,
                               barheight = 17)) +
  NULL




