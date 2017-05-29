library(tidyverse)
library(ggthemes)

frame15 <- read_csv("chrisi15.csv")
frame16 <- read.csv("chrisi16.csv", header = TRUE, stringsAsFactors = FALSE)
frame17 <- read_csv("iannetta17.csv")

names <- read.csv("names.csv", header = TRUE, stringsAsFactors = FALSE)

frame15 <- frame15 %>% mutate_at(.cols = vars(px,pz, start_speed, sz_top, sz_bot, hit_speed, spin_rate, zone),
                                 .funs = funs(as.numeric))

frame16 <- frame16 %>% mutate_at(.cols = vars(px,pz, start_speed, sz_top, sz_bot, hit_speed, spin_rate, zone),
                      .funs = funs(as.numeric))

frame17 <- frame17 %>% mutate_at(.cols = vars(plate_x,plate_z, release_speed, sz_top, sz_bot, launch_speed, release_spin_rate, zone),
                                 .funs = funs(as.numeric))

frame15$game_date <- as.Date(frame15$game_date)
frame16$game_date <- as.Date(frame16$game_date)

makeZone <- function(df) {
  #strikeZone by Jim Albert
  topKzone = median(df$sz_top, na.rm = T)
  botKzone = median(df$sz_bot, na.rm = T)
  inKzone = -.80
  outKzone = 0.80
  kZone = data.frame(
    x = c(inKzone, inKzone, outKzone, outKzone, inKzone)
    , y = c(botKzone, topKzone, topKzone, botKzone, botKzone)
  )
  return(kZone)
}

str(frame16)
pit <- frame16 %>% select(px, pz, p_throws, description) %>% group_by(p_throws) %>% summarise(pitches = n())
pitStrike <- frame16 %>% select(px, pz, p_throws, description) %>% filter(description == "Called Strike") %>% group_by(p_throws) %>% summarise(k = n())

pit
pitStrike
paste(round(pitStrike$k / pit$pitches, 3) * 100, "%", sep = "")

fr <- frame16 %>% select(pitch_type, px, pz, zone, p_throws, stand, description) %>% filter(description == "Called Strike", zone >= 11 & zone != "null") 
fr %>% summarise(cs = n()) * 0.14

ggplot(fr) + 
  geom_point(aes(px, pz), col = "dodgerblue", alpha = 0.7) + 
  facet_grid(~pitch_type) +
  geom_point(data = frame16 %>% filter(pitch_type != "null", pitch_type != "IN"), aes(px, pz, col = pitch_type), alpha = 0.1) +
  facet_grid(p_throws~pitch_type)+
  geom_path(data = makeZone(frame16), aes(x,y), alpha = 0.7) +
  theme_tufte() + 
  scale_x_continuous(limits = c(-1.8, 1.8)) +
  scale_y_continuous(limits = c(0, 4)) + 
  labs(title = "2016")

mPx <- mean(0 - fr$px)
mPz <- mean(mean(fr$pz)-fr$pz) + mean(fr$pz)
loc <- data.frame(mPx, mPz)
dist <- sqrt(((loc$mPx - fr$px)^2) + ((loc$mPz - fr$pz)^2))
(loc$mPx - fr$px)
fr$px
loc$dist <- mean(dist)
loc

ggplot(paxAll, aes(px, pz, col=pitch_type)) + 
paxAll <- frame16 %>% select(px, pz, zone, p_throws, stand, description, pitcher, pitch_type) %>% filter(zone != "null", p_throws == "L", pitcher == 572020) 
paxAll %>% summarise(n())
paxAll$pitch_type <- as.factor(paxAll$pitch_type)
paxAll

fr3 <- frame16 %>% select(px, pz, zone, p_throws, stand, description, pitcher, pitch_type) %>% filter(description == "Called Strike", zone >= 11 & zone != "null", p_throws == "L", pitcher == 572020) %>% group_by(pitcher)
  geom_point() + 
  scale_x_continuous(limits = c(-1.8,1.8)) + 
  scale_y_continuous(limits = c(0,3.8))

 ggplot(paxAll, aes(zone, fill = pitch_type)) + 
  geom_histogram(bins = 28) + 
  theme_tufte() +
  scale_x_continuous(breaks = c(1:14))
