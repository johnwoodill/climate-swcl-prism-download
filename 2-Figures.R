library(tidyverse)

setwd("~/Projects/climate-swcl-prism-download/")

dat = read_csv("data/PRISM_monthly_1895-2020.csv")

dat = filter(dat, var == "tmax")
dat$char = nchar(dat$date)
dat = filter(dat, char == 6)

dat$month = substring(dat$date, 5, 6)
dat$year = substring(dat$date, 1, 4)

head(dat)

pdat = dat %>% 
  group_by(month) %>% 
  mutate(avg_value = mean(value)) %>% 
  group_by(year, month) %>% 
  summarise(value = mean(value),
            avg_value = mean(avg_value)) %>% 
  ungroup() %>% 
  mutate(anom_value = value - avg_value)

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



dat2 = read_csv("data/PRISM_daily_1981-2020.csv")

dat2 = filter(dat2, var == "tmax")
dat2$month = substring(dat2$date, 5, 6)
dat2$year = substring(dat2$date, 1, 4)
dat2$day = substring(dat2$date, 7, 8)

pdat2 = dat2 %>% group_by(year, month, day) %>% summarise(value = mean(value))

pdat2$date = paste0(pdat2$year, "-", pdat2$month, "-", pdat2$day)

ggplot(pdat2, aes(as.Date(date), value)) + geom_line()

pdat3 = pdat2 %>% mutate(diff = mean(value)) %>% group_by(year, month) %>% summarise(value = mean(value))

ggplot(pdat3, aes(month, year, fill=value)) + geom_tile()+ scale_fill_viridis_c(option = "D")

