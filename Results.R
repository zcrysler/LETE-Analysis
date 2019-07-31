library(ggmap)
library(tidyr)
library(gridExtra)
library(RColorBrewer)
register_google(key = "ADD HERE")


## Map of 2017 station locations
# Google Map
gmap <- get_map(location = c(lon = -93, lat = 29), maptype = "satellite", source = "google", zoom = 6)
p <- ggmap(gmap)
p + geom_point(data = filter(df.recvDepsCombined, active == TRUE),
               aes(longitude, latitude), pch = 21, colour = "black", fill = "red") +
  geom_point(data = filter(df.recvDepsCombined, active == FALSE),
             aes(longitude, latitude), pch = 21, colour = "black", fill = "blue") +
  geom_point(data = inc, aes(recvLon, recvLat), pch = 21, colour = "black", fill = "yellow")

## outline map
#triangles = 2017 AND 2018 receivers, circles = 2017 receivers ONLY
# coloured = positive incubation detections, white with red outline = no incubation detections
# dashed line = Bolivar_Flats 2018 antenna bearings
#bearings <- select(lete, recvDeployName, )
na.map <- map_data(map = "world2")
na.map <- na.map %>% filter(region %in% c("Canada",
                                          "USA")) %>% mutate(long = long - 360)
arr.sc <- 0.24  # determines length of the vectors for antenna bearings and vanishing bearing lines (0.3 = 30 km)
bigMap <- ggplot(na.map, aes(long, lat)) +
  geom_polygon(data = na.map, aes(long, lat, group = group), colour = "grey") +
  geom_polygon(aes(group = group), colour = "grey", fill = "grey98") +
  coord_map(projection = "mercator", xlim = c(-100, -88), ylim = c(21, 31)) +
  xlab("") + ylab("") + theme_bw() +
  geom_segment(data = filter(df.antDeps, !(recvDeployID %in% c(5149, 4304, 4615, 4616))), # remove Quintana and Bolivar deployments outside detection range
               aes(x = longitude, xend = longitude +
                     (sin(rad(antBearing)) *arr.sc), y = latitude, yend = latitude +
                     (cos(rad(antBearing)) *arr.sc)), colour = "blue") +
  geom_segment(data = filter(df.antDeps, recvDeployID == 4616), # Create dashed lines for Bolicvar 2018 antenna bearings
               aes(x = longitude, xend = longitude +
                     (sin(rad(antBearing)) *arr.sc), y = latitude, yend = latitude +
                     (cos(rad(antBearing)) *arr.sc)), colour = "blue", linetype = "dashed") +
  geom_point(data = filter(df.recvDepsCombined, is.na(incubate)),
             aes(longitude, latitude, shape = active), fill = "white", colour = "red", size = 3) +
  scale_shape_manual(values = c(21, 24)) +
  geom_point(data = filter(df.recvDepsCombined, incubate == TRUE),
             aes(longitude, latitude, shape = active, fill = recvDeployName), colour = "black", size = 3) +
  scale_fill_manual(values = cols) +
  #geom_point(data = df.recvDepsCombined,
  #           aes(longitude, latitude, shape = active, fill = incubate), colour = "black", size = 4) +
  #scale_shape_manual(values = c(21, 24)) +
  geom_text(data = filter(df.recvDepsCombined, recvDeployName %in% c("Quintana", "Bolivar_Flats")),
            aes(longitude, latitude, label=recvDeployName, hjust = -0.2, vjust = 1), size = 3) +
  #  geom_text(data = filter(df.recvDepsCombined, recvDeployName %in% c("Wisner")),
  #            aes(longitude, latitude, label=recvDeployName, hjust = 0, vjust = 2)) +  
  #  geom_text(data = filter(df.recvDepsCombined, recvDeployName %in% c("East Grand Terre")),
  #            aes(longitude, latitude, label=recvDeployName, hjust = -0.1, vjust = 0.1)) +
  geom_text(data = filter(df.recvDepsCombined, recvDeployName %in% c("Scenic Galveston")),
            aes(longitude, latitude, label=recvDeployName, hjust = 1.1, vjust = -0.5), size = 3) +
  geom_text(data = filter(df.recvDepsCombined, recvDeployName %in% c("San Bernard NWR (Big Pond Unit)")),
            aes(longitude, latitude, label="San Bernard NWR (BPU)", hjust = 1.1, vjust = 0.5), size = 3) +  
  geom_text(data = filter(df.recvDepsCombined, recvDeployName %in% c("San Bernard National Wildlife Refuge Headquarters")),
            aes(longitude, latitude, label="San Bernard NWR HQ", hjust = 0.9, vjust = 1.7), size = 3) +
  #  geom_text(data = filter(df.recvDepsCombined, recvDeployName %in% c("Grand_Isle")),
  #            aes(longitude, latitude, label=recvDeployName, hjust = 1.2, vjust = 0.5)) +
  #  geom_text(data = filter(df.recvDepsCombined, recvDeployName %in% c("Exxon Fields")),
  #            aes(longitude, latitude, label=recvDeployName, hjust = 1, vjust = -1)) +
  annotate(geom = "rect", xmin=-90.5, xmax=-88.9, ymin=28.6, ymax=29.9, color="red", fill = NA) +
  theme(legend.position = "none",
        plot.background = element_rect(fill = "transparent", color = NA)) +
  scale_bar(lon = -99, lat = 21.8, 
            distance_lon = 50, distance_lat = 10, distance_legend = 25, 
            dist_unit = "mi", orientation = FALSE)

## closeup map of breeding area
arr.sc <- 0.24  # determines length of the vectors for antenna bearings and vanishing bearing lines (0.3 = 30 km)
insetMap <- ggplot(na.map, aes(long, lat)) +
  geom_polygon(data = na.map, aes(long, lat, group = group), colour = "grey", fill = "blue") +
  geom_polygon(aes(group = group), colour = "grey", fill = "grey98") +
  coord_map(projection = "mercator", xlim = c(-90.5, -88.9), ylim = c(28.6, 29.9)) +
  xlab("") + ylab("") + theme_bw() +
  geom_segment(data = filter(df.antDeps, !(recvDeployID %in% c(5149, 4304, 4615, 4616))), # remove Quintana and Bolivar deployments outside detection range
               aes(x = longitude, xend = longitude +
                     (sin(rad(antBearing)) *arr.sc), y = latitude, yend = latitude +
                     (cos(rad(antBearing)) *arr.sc)), colour = "blue") +
  geom_segment(data = filter(df.antDeps, recvDeployID == 4616), # Create dashed lines for Bolicvar 2018 antenna bearings
               aes(x = longitude, xend = longitude +
                     (sin(rad(antBearing)) *arr.sc), y = latitude, yend = latitude +
                     (cos(rad(antBearing)) *arr.sc)), colour = "blue", linetype = "dashed") +
  geom_point(data = filter(df.recvDepsCombined, incubate == TRUE),
             aes(longitude, latitude, shape = active, fill = recvDeployName), colour = "black", size = 4) +
  scale_shape_manual(values = c(21, 24)) +
  geom_point(data = filter(df.recvDepsCombined, is.na(incubate)),
             aes(longitude, latitude, shape = active), fill = "white", colour = "red", size = 4) +
  scale_fill_manual(values = cols) +
  #  geom_point(data = df.recvDepsCombined,
  #             aes(longitude, latitude, shape = active, fill = incubate), colour = "black", size = 4) +
  #  scale_shape_manual(values = c(21, 24)) +
  geom_text(data = filter(df.recvDepsCombined, recvDeployName %in% c("Wisner")),
            aes(longitude, latitude, label=recvDeployName, hjust = 1.3, vjust = 0.9), size = 3.5) +
  geom_text(data = filter(df.recvDepsCombined, recvDeployName %in% c("East Grand Terre")),
            aes(longitude, latitude, label=recvDeployName, hjust = -0.1, vjust = 1), size = 3.5) +
  #  geom_text(data = filter(df.recvDepsCombined, recvDeployName %in% c("Scenic Galveston", "San Bernard NWR (Big Pond Unit)")),
  #            aes(longitude, latitude, label=recvDeployName, hjust = 1.1, vjust = 0.5)) +
  #  geom_text(data = filter(df.recvDepsCombined, recvDeployName %in% c("San Bernard National Wildlife Refuge Headquarters")),
  #            aes(longitude, latitude, label="San Bernard NWR Headquarters", hjust = 1, vjust = 0.5)) +
  geom_text(data = filter(df.recvDepsCombined, recvDeployName %in% c("Grand_Isle")),
            aes(longitude, latitude, label=recvDeployName, hjust = 1.2, vjust = 0.8), size = 3.5) +
  geom_text(data = filter(df.recvDepsCombined, recvDeployName %in% c("Exxon Fields")),
            aes(longitude, latitude, label=recvDeployName, hjust = 1, vjust = -1), size = 3.5) +
  theme(legend.position = "none",
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        plot.background = element_rect(fill = "transparent", color = NA)) + 
  scale_bar(lon = -89.9, lat = 28.7, 
            distance_lon = 25, distance_lat = 3, distance_legend = 6, 
            dist_unit = "mi", orientation = FALSE)

# Create complete station map with inset
vp_inset <- grid::viewport(width = 0.5, x = 0.6, y = 0.4)
print(bigMap)
print(insetMap, vp = vp_inset)


## Map of 2017 detections
gmap <- get_map(location = c(lon = -93, lat = 29), maptype = "satellite", source = "google", zoom = 7)
p <- ggmap(gmap)
p + geom_point(data = filter(df.recvDeps17, active17 == TRUE), aes(longitude, latitude), pch = 21, colour = "black", fill = "red") +
  geom_point(data = filter(lete.path, year == "2017"), aes(recvLon, recvLat), pch = 21, colour = "black", fill = "yellow") +
  geom_path(data = filter(lete.path, year == "2017"), aes(recvLon, recvLat, group = motusTagID, col = as.factor(motusTagID)))
# outline map of 2017 incubation detections
ggplot(na.map, aes(long, lat)) +
  geom_polygon(data = na.map, aes(long, lat, group = group), colour = "grey", fill = "blue") +
  geom_polygon(aes(group = group), colour = "grey", fill = "grey98") +
  coord_map(projection = "mercator", xlim = c(-98, -88), ylim = c(27, 31)) +
  geom_point(data = df.recvDepsCombined,
             aes(longitude, latitude, fill = incubate), shape = 21, colour = "black", size = 4) +
  geom_path(data = filter(lete.path, year == "2017"),
            aes(recvLon, recvLat, group = motusTagID, col = as.factor(motusTagID)),
            position = position_jitter(w=0, h = 0.05)) +
  labs(x = NULL, y = NULL, col = "Motus Tag ID") + theme_bw() + 
  guides(fill = FALSE,
         colour = guide_legend(override.aes = list(size = 2)))

## Map of 2017 detections removing 23270
gmap <- get_map(location = c(lon = -90, lat = 29.2), maptype = "satellite", source = "google", zoom = 11)
p <- ggmap(gmap)
p + geom_point(data = filter(df.recvDeps17, active17 == TRUE), aes(longitude, latitude), pch = 21, colour = "black", fill = "red") +
  geom_point(data = filter(lete.path, year == "2017"), aes(recvLon, recvLat), pch = 21, colour = "black", fill = "yellow") +
  geom_path(data = filter(lete.path, year == "2017", motusTagID != 23270), aes(recvLon, recvLat, group = motusTagID, col = as.factor(motusTagID)), position=position_jitter(w=0.01, h=0.01))

## Map of 2018 detections
ggplot(na.map, aes(long, lat)) +
  geom_polygon(data = na.map, aes(long, lat, group = group), colour = "grey", fill = "blue") +
  geom_polygon(aes(group = group), colour = "grey", fill = "grey98") +
  coord_map(projection = "mercator", xlim = c(-98, -88), ylim = c(27, 31)) +
  geom_point(data = df.recvDepsCombined,
             aes(longitude, latitude, fill = incubate), shape = 21, colour = "black", size = 4) +
  geom_path(data = filter(lete.path, year == "2018"),
            aes(recvLon, recvLat, group = motusTagID, col = as.factor(motusTagID)),
            position = position_jitter(w=0, h = 0.05)) +
  labs(x = NULL, y = NULL, col = "Motus Tag ID") + theme_bw() +
  guides(fill = FALSE,
         colour = guide_legend(override.aes = list(size = 2)))
## Map of 2018 detections removing 28520
gmap <- get_map(location = c(lon = -89.9, lat = 29.3), maptype = "satellite", source = "google", zoom = 11)
p <- ggmap(gmap)
p + geom_point(data = filter(df.recvDeps17, active17 == TRUE), aes(longitude, latitude), pch = 21, colour = "black", fill = "red") +
  geom_point(data = filter(lete.path, year == "2018"), aes(recvLon, recvLat), pch = 21, colour = "black", fill = "yellow") +
  geom_path(data = filter(lete.path, year == "2018", motusTagID != 28520), aes(recvLon, recvLat, group = motusTagID, col = as.factor(motusTagID)), position=position_jitter(w=0.01, h=0.01))

## Create colour blind friendly scales for stations
# Get all station names
unique(lete.path$recvDeployName)
cols <- c("Exxon Fields" = "#0072B2", "Grand_Isle" = "#E69F00", "Wisner" = "#D55E00",
          "East Grand Terre" = "#009E73", "Quintana" = "#000000",
          "San Bernard National Wildlife Refuge Headquarters" = "#CC79A7",
          "San Bernard NWR (Big Pond Unit)" = "#F0E442", "Scenic Galveston" = "#999999",
          "Bolivar_Flats" = "#56B4E9")

## lon 2017 detections
ggplot(filter(lete.path, year == "2017"),
       aes(ts.h, recvLon, col = as.factor(motusTagID), group = as.factor(motusTagID))) +
  geom_point() + geom_path() + th
## removing 23270 to see more clearly
ggplot(filter(lete.path, year == "2017", motusTagID != "23270"),
       aes(ts.h, recvLon, col = as.factor(motusTagID), group = as.factor(motusTagID))) +
  geom_point() + geom_path() + th
# All 2017 tags by longitude
ggplot(filter(lete.path, year == "2017"),
       aes(ts.h, recvLon, group = as.factor(motusTagID))) +
  geom_point(aes(colour = as.factor(recvDeployName)), size = 1) +
  geom_path(col = "grey45") +
  th + labs(y = "what") + facet_wrap(~motusTagID, scales = "free", ncol = 4) +
  theme(legend.direction = "horizontal",
        legend.position = c(0.61, 0.07),
        legend.title = element_blank(),
        axis.title = element_blank()) +
  scale_color_manual(values = cols) +
  theme(text = element_text(size = 8),
        legend.text=element_text(size=10),
        strip.text.x = element_text(size = 10)) +
  guides(colour = guide_legend(override.aes = list(size = 3), ncol = 2))
ggsave("./images/2017LongitudePlot.png")

  #scale_color_viridis(discrete = TRUE)

# All 2018 tags by longitude
ggplot(filter(lete.path, year == "2018"),
       aes(ts.h, recvLon, group = as.factor(motusTagID))) +
  geom_point(aes(colour = as.factor(recvDeployName)), size = 1) +
  geom_path(col = "grey45") +
  th + labs(y = "what") + facet_wrap(~motusTagID, scales = "free", ncol = 4) +
  theme(legend.direction = "horizontal",
        legend.position = "bottom",
        legend.title = element_blank(),
        axis.title = element_blank()) +
  scale_color_manual(values = cols) +
  theme(text = element_text(size = 8),
        legend.text=element_text(size=10),
        strip.text.x = element_text(size = 10)) +
  guides(colour = guide_legend(override.aes = list(size = 3)))

#  scale_color_viridis(discrete = TRUE)

ggplot(filter(lete.path, year == "2018"),
       aes(ts.h, recvDeployName, col = as.factor(motusTagID), group = as.factor(motusTagID))) +
  geom_point() + geom_path() + th + facet_wrap(~motusTagID, scales = "free_y")

# what's happening at wisner - why no 2018 detections?
ggplot(filter(lete, recvDeployName == "Wisner"), aes(ts, sig)) + geom_point()

## time partitioning
# all tags by signal strength
ggplot(filter(inc, year == "2017"), aes(ts, sig, col = recvDeployName)) + geom_point() + facet_wrap(~motusTagID, scales = "free")
ggplot(filter(inc, year == "2018"), aes(ts, sig, col = recvDeployName)) + geom_point() + facet_wrap(~motusTagID, scales = "free")
# it's not consistent that birds are detected while on the nest, eg. 23218 gets incubation detections, 23251 does not

# looking at individual birds
p1 <- ggplot(filter(inc, motusTagID == 23219), aes(ts, sig, col = recvDeployName)) + geom_point(size = 1) +
  facet_grid(recvDeployName~.) +
  geom_vline(xintercept = inc$sunrise,col = "orange") +
  geom_vline(xintercept = inc$sunset, col = "blue") + th +
  theme(legend.position = "none") + labs (y = "Signal Strength", x = NULL, title ="2017: motusTagID 23219")
p2 <- ggplot(filter(inc, motusTagID == 23218, ts > as.POSIXct("2017-06-15")), aes(ts, sig, col = recvDeployName)) + geom_point(size = 1) +
  facet_grid(recvDeployName~.) +
  geom_vline(xintercept = inc$sunrise,col = "orange") +
  geom_vline(xintercept = inc$sunset, col = "blue") + th +
  theme(legend.position = "none") + labs (y = "Signal Strength", x = NULL, title ="2017: motusTagID 23218")
p3 <- ggplot(filter(inc, motusTagID == 28593), aes(ts, sig, col = recvDeployName)) + geom_point(size = 1) +
  facet_grid(recvDeployName~.) +
  geom_vline(xintercept = inc$sunrise,col = "orange") +
  geom_vline(xintercept = inc$sunset, col = "blue") + th +
theme(legend.position = "none") + labs (y = "Signal Strength", x = NULL, title ="2018: motusTagID 28593")
p4 <- ggplot(filter(inc, motusTagID == 28522), aes(ts, sig, col = recvDeployName)) + geom_point(size = 1) +
  facet_grid(recvDeployName~.) +
  geom_vline(xintercept = inc$sunrise,col = "orange") +
  geom_vline(xintercept = inc$sunset, col = "blue") + th +
  theme(legend.position = "none") + labs (y = "Signal Strength", x = NULL, title ="2018: motusTagID 28522")
grid.arrange(p1, p2, p3, p4, nrow = 2)
# can't really tell for sure when incubating, but during the night consistent detections below 80 sig

# strength suggests incubation at Exxon Fields - so maybe remove those so we can include other
# Exxon detections?
foraging <- filter(inc, !(recvDeployName == "Exxon Fields" & sig <70))

# how often are birds detected simultaneously at 2 or more stations?
simSites <- simSiteDet(foraging)
# in 2017 the vast majority are simultaneous detections between Exxon Fields and Grand_Isle,
# with 2 instance between East Grand Terre and Grand_isle,
# 1 instance between East Grand Terre and Exxon Fields,
# 1 instance of a 3-way detection at East Grand Terre, Exxon Fields, and Grand_Isle
# % detections at Grand_Isle that were simultaneously at Exxon Fields
(length(filter(simSites, year == 2017, recvDeployName == "Grand_Isle")$motusTagID)) / (length(filter(inc, year == 2017, recvDeployName == "Grand_Isle")$motusTagID)) * 100
# in 2018 there were only 2 instances, both between East Grand Terre and Exxon Fields,
(length(filter(simSites, year == 2018, recvDeployName == "Grand_Isle")$motusTagID)) / (length(filter(inc, year == 2018, recvDeployName == "Grand_Isle")$motusTagID)) * 100



## Time partitioning between foraging activities and nest site attendance during incubation
# lets look at what birds are detected while on the nest:
unique(inc$motusTagID)
#28523, 28607, 28526, 28605, 28608, 28593, 
#28604, 28525, 28615, 28609, 28520, 28521, 
#28527, 23270, 23261, 23262, 23257, 23259, 
#23258, 23251, 23249, 23248, 23246, 23252, 
#23244, 23245, 23226, 23219, 23225, 23223, 
#23217, 23218, 23216, 23210, 23214, 23212, 
#23227, 23209, 28595, 28597, 28598, 28599,
#28600, 28522, 28524, 28602, 28603, 28606, 
#28592, 28611, 28528, 28613, 28529
ggplot(filter(inc, motusTagID %in% c(28592, 28611, 28528, 28613, 28529), recvDeployName == "Exxon Fields"), aes(ts, sig)) + 
  geom_point() + facet_wrap(~motusTagID, scales = "free")
# potential for incubation detections: 28523, 28526, 28608, 28525, 28520, 23262, 
# 23257, 23248, 23246, 23219, 23218, 28595, 28600
ggplot(filter(inc, motusTagID %in% c(28523, 28526, 28608, 28525, 28520, 23262,23257, 23248, 
                                     23246, 23219, 23218, 28595, 28600), recvDeployName == "Exxon Fields"), aes(ts, sig)) + 
  geom_point() + facet_wrap(~motusTagID, scales = "free") + th + labs(x = NULL, y = "Signal Strength")
#lets remove 23257, not consistently detected at night
# tag 28520, first egg hatched between may 21 and may 24, 
# pattern changes on the 21st so lets remove detections after the 21st for this bird
# tag 28525 started hatching may 24, so lets remove detections after this
# tag 28526 failed between may 27-28 so remove detections after 27th
# tag 28595 failed between may 18-21st so remove detections after 18th
nestSite <- inc %>% filter(motusTagID %in% c(28523, 28526, 28608, 28525, 28520, 23262, 23248, 
                                    23246, 23219, 23218, 28595, 28600),
              recvDeployName == "Exxon Fields",
              !(motusTagID == 28520 & ts > as.POSIXct("2018-05-21")),
              !(motusTagID == 28525 & ts > as.POSIXct("2018-05-24")),
              !(motusTagID == 28526 & ts > as.POSIXct("2018-05-27")),
              !(motusTagID == 28595 & ts > as.POSIXct("2018-05-18"))) %>% 
  select(-sunrise, -sunset)
# convert times to CDT since these are lotek receivers that were recording in local time instead of UTC
nestSite$ts <- (nestSite$ts - 5*60*60)
nestSite$ts <- force_tz(nestSite$ts, tzone = "America/Chicago")
nestSite$date <- as.Date(nestSite$ts, tz = "America/Chicago")


ggplot(filter(nestSite, year == 2017), aes(ts, sig)) + 
  geom_point() + facet_wrap(~motusTagID, scales = "free_x") + th
ggplot(filter(nestSite, year == 2018), aes(ts, sig)) + 
  geom_point() + facet_wrap(~motusTagID, scales = "free_x") + th

# so with this data, group detections into "visits" to look at mean signal strength
incPeriods <- nestSite %>% group_by(motusTagID, mfgID, recvDeployName, runID, year) %>%
  summarize(mints = min(ts),
            maxts = max(ts),
            numHits = length(motusTagID))
incPeriods <- as.data.frame(incPeriods)
incPeriods <- incPeriods[with(incPeriods, order(motusTagID, mints)),]
incPeriods <- incPeriods %>%
  mutate(diffsec = as.numeric(mints) - lag(as.numeric(maxts)),
         diffmin = as.numeric(diffsec/60)) %>% as.data.frame
incPeriods <- incPeriods[with(incPeriods, order(motusTagID, mints)),] ## first make sure it's ordered correctly
incPeriods$diffmin[is.na(incPeriods$diffmin)] <- 0 ## set NA in time differences to 0
incPeriods <- incPeriods %>% group_by(motusTagID) %>% mutate(visitID = c(cumsum(diffmin >10)), # lump together if time dif is less than 10 mins
                                                             tagVisitID = paste(motusTagID, visitID, sep = ".")) %>% data.frame() ## now get one value to number each variable
## now get the total time (in minutes), and mean signal strength for each "visit"
nestSite <- left_join(nestSite, select(incPeriods, motusTagID, runID, visitID, tagVisitID), by = c("motusTagID", "runID")) 
incPeriods <- nestSite %>% group_by(motusTagID, mfgID, recvDeployName, visitID, tagVisitID, year) %>%
  summarize(mints = min(ts),
            maxts = max(ts),
            numHits = length(motusTagID),
            meanSig = mean(sig),
            minSig = min(sig),
            maxSig = max(sig)) %>% as.data.frame()
incPeriods <- incPeriods %>% mutate(visitLength = as.numeric(difftime(maxts, mints), units = "mins"),
                                    visitID = as.numeric(visitID),
                                    date = as.Date(mints))
# add dawn/dusk times to incPeriods based on first detection for the run
days <- unique(incPeriods$date)
dusk <- getSunlightTimes(date = days, keep = c("dawn", "dusk"),
                         lat = 29.20644, lon = -89.81102, tz = "America/Chicago")
incPeriods <- left_join(incPeriods, dusk, by = "date")
incPeriods$period <- ifelse(incPeriods$mints > incPeriods$dawn & incPeriods$mints < incPeriods$dusk, "day", "night")
# get sunrise etc. info for nestSite dataframe
days <- unique(nestSite$date)
dusk <- getSunlightTimes(date = days, keep = c("sunrise", "sunset", "dawn", "dusk"),
                         lat = 29.20644, lon = -89.81102, tz = "America/Chicago")
nestSite <- left_join(nestSite, dusk, by = "date")
nestSite$period <- ifelse(nestSite$ts > nestSite$dawn & nestSite$ts < nestSite$dusk, "day", "night")


ggplot(filter(nestSite, motusTagID == 28523), aes(ts, sig, col = period)) + 
  geom_point() +
  geom_vline(xintercept = nestSite$dawn, col = "orange") +
  geom_vline(xintercept = nestSite$dusk, col = "blue") +
  facet_wrap(~motusTagID, scales = "free_x") + th

## get summaries by day
# longer average length of visit period during night than day in both years (more obvious in 2017)
# mean signal strength was lower during the night as the birds were on the nest
#maybe look at variation in signal strength during day vs night
incPeriods %>% group_by(year, period) %>% summarize(meanNumHits = mean(numHits),
                                                   meanLength = mean(visitLength),
                                                   meanSig = mean(meanSig)) %>% as.data.frame()

tmp <- aov(data = incPeriods, visitLength ~ period) # not significant, but incredibly small sample size
summary(tmp) # significant difference 6.6e-11 ***
tmp <- aov(data = incPeriods, meanSig ~ period) # not significant, but incredibly small sample size
summary(tmp) # significant difference 6.6e-11 ***

# on average they spent 198.23 and 144.36 minutes at Exxon in 2017 and 2018 respectively during the day
# on average they spent 251.15 and 133.21 minutes at Exxon in 2017 and 2018 respectively during the night
# but not comparable because day was much longer than the night
tmp <- incPeriods %>% group_by(year, period, date, motusTagID) %>% 
  summarize(totNumHits = sum(numHits),
            totLength = sum(visitLength),
            meanSig = mean(meanSig)) %>% as.data.frame()
tmp %>% group_by(year, period) %>% summarize(meanNumHits = mean(totNumHits),
                                             meanLength = mean(totLength),
                                             meanSig = mean(meanSig)) %>% as.data.frame()

# detections during the day
tmp <- incPeriods %>% filter(period == "day") %>% group_by(year, motusTagID)

tmp1 <- ggplot(filter(nestSite, motusTagID == 23218), aes(ts, sig, col = period)) + 
  geom_point(size = 1) +
  geom_vline(xintercept = nestSite$dawn, col = "orange") +
  geom_vline(xintercept = nestSite$dusk, col = "blue") +
  facet_wrap(~motusTagID) + th + labs(x = NULL, y = " ") +
  theme(legend.position = "none")
tmp2 <- ggplot(filter(nestSite, year == 2017, motusTagID != 23218), aes(ts, sig, col = period)) + 
  geom_point(size = 1) +
  geom_vline(xintercept = nestSite$dawn, col = "orange") +
  geom_vline(xintercept = nestSite$dusk, col = "blue") +
  facet_wrap(motusTagID~., scales = "free_x", ncol = 2) + th + labs(x = NULL, y = "Signal Strength") +
  scale_color_discrete(labels = c("Day", "Night"), name = NULL) +
  theme(legend.position = "bottom", legend.direction = "horizontal")

grid.arrange(tmp1, tmp2, nrow = 2, heights = c(1, 3))

ggplot(filter(nestSite, year == 2018), aes(ts, sig, col = period)) + 
  geom_point(size = 1) +
  geom_vline(xintercept = nestSite$dawn, col = "orange") +
  geom_vline(xintercept = nestSite$dusk, col = "blue") +
  facet_wrap(~motusTagID, scales = "free_x", ncol = 2) + th + labs(x = NULL, y = "Signal Strength") +
  scale_color_discrete(labels = c("Day", "Night"), name = "Period")

# get visit lengths for daylight detections with sig strength under 70
dayInc <- inc %>% filter(motusTagID %in% c(28523, 28526, 28608, 28525, 28520, 23262, 23248, 
                                             23246, 23219, 23218, 28595, 28600),
                           recvDeployName == "Exxon Fields",
                           !(motusTagID == 28520 & ts > as.POSIXct("2018-05-21")),
                           !(motusTagID == 28525 & ts > as.POSIXct("2018-05-24")),
                           !(motusTagID == 28526 & ts > as.POSIXct("2018-05-27")),
                           !(motusTagID == 28595 & ts > as.POSIXct("2018-05-18")),
                           sig < 70) %>% 
  select(-sunrise, -sunset)
# convert times to CDT since these are lotek receivers that were recording in local time instead of UTC
dayInc$ts <- (dayInc$ts - 5*60*60)
dayInc$ts <- force_tz(dayInc$ts, tzone = "America/Chicago")
dayInc$date <- as.Date(dayInc$ts, tz = "America/Chicago")

# so with this data, group detections into "visits" to look at mean signal strength
dayPeriods <- dayInc %>% group_by(motusTagID, mfgID, recvDeployName, runID, year) %>%
  summarize(mints = min(ts),
            maxts = max(ts),
            numHits = length(motusTagID))
dayPeriods <- as.data.frame(dayPeriods)
dayPeriods <- dayPeriods[with(dayPeriods, order(motusTagID, mints)),]
dayPeriods <- dayPeriods %>%
  mutate(diffsec = as.numeric(mints) - lag(as.numeric(maxts)),
         diffmin = as.numeric(diffsec/60)) %>% as.data.frame
dayPeriods <- dayPeriods[with(dayPeriods, order(motusTagID, mints)),] ## first make sure it's ordered correctly
dayPeriods$diffmin[is.na(dayPeriods$diffmin)] <- 0 ## set NA in time differences to 0
dayPeriods <- dayPeriods %>% group_by(motusTagID) %>% mutate(visitID = c(cumsum(diffmin >10)), # lump together if time dif is less than 10 mins
                                                             tagVisitID = paste(motusTagID, visitID, sep = ".")) %>% data.frame() ## now get one value to number each variable
## now get the total time (in minutes), and mean signal strength for each "visit"
dayInc <- left_join(dayInc, select(dayPeriods, motusTagID, runID, visitID, tagVisitID), by = c("motusTagID", "runID")) 
dayPeriods <- dayInc %>% group_by(motusTagID, mfgID, recvDeployName, visitID, tagVisitID, year) %>%
  summarize(mints = min(ts),
            maxts = max(ts),
            numHits = length(motusTagID),
            meanSig = mean(sig),
            minSig = min(sig),
            maxSig = max(sig)) %>% as.data.frame()
dayPeriods <- dayPeriods %>% mutate(visitLength = as.numeric(difftime(maxts, mints), units = "mins"),
                                    visitID = as.numeric(visitID),
                                    date = as.Date(mints))
# add dawn/dusk times to dayPeriods based on first detection for the run
days <- unique(dayPeriods$date)
dusk <- getSunlightTimes(date = days, keep = c("dawn", "dusk"),
                         lat = 29.20644, lon = -89.81102, tz = "America/Chicago")
dayPeriods <- left_join(dayPeriods, dusk, by = "date")
dayPeriods$period <- ifelse(dayPeriods$mints > dayPeriods$dawn & dayPeriods$mints < dayPeriods$dusk, "day", "night")
# get sunrise etc. info for dayInc dataframe
days <- unique(dayInc$date)
dusk <- getSunlightTimes(date = days, keep = c("sunrise", "sunset", "dawn", "dusk"),
                         lat = 29.20644, lon = -89.81102, tz = "America/Chicago")
dayInc <- left_join(dayInc, dusk, by = "date")
dayInc$period <- ifelse(dayInc$ts > dayInc$dawn & dayInc$ts < dayInc$dusk, "day", "night")

#average length of visit
filter(dayPeriods) %>% group_by(year, period) %>% summarize(meanNumHits = mean(numHits),
                                                            meanLength = mean(visitLength)) %>% as.data.frame()

# average number of daily visits
tmp <- dayPeriods %>% group_by(year, period, date, motusTagID) %>% 
  summarize(totNumHits = sum(numHits),
            totLength = sum(visitLength),
            totVisits = length(motusTagID)) %>% as.data.frame()
tmp %>% group_by(year, period) %>% summarize(meanNumHits = mean(totNumHits),
                                             meanLength = mean(totLength),
                                             meanVisits = mean(totVisits)) %>% as.data.frame()







## 3) Identification of heavily frequented foraging areas during incubation

# distance of stations from Exxon Fields
p <- filter(dist, recvDeployName1 == "Exxon Fields")

# 2017: most detections at Exxon Fields, followed by Grand_Isle. 18 to Wisner (3 birds), 6 to East Grand Terre (4 birds),
# 2 to San Bernard NWR Big Pond (1 bird),
# 3 to Galveston (1 bird), 1 visit to Quintana, 1 to San Bernard Headquarters
table(filter(visits, year == "2017")$recvDeployName)
# 2018: most at Exxon Fields, 332 Grand_Isle, 35 East Grand Terre (11 birds), 1 Bolivar_Flats
table(filter(visits, year == "2018")$recvDeployName)
table(filter(visits)$recvDeployName)
filter(visits, !(recvDeployName %in% c("Exxon Fields", "Grand_Isle"))) %>% group_by(motusTagID, mfgID, year, recvDeployName) %>% summarize(nVisits = length(recvDeployName)) %>% as.data.frame()

# both years detected at Grand_Isle most frequently - check about state of stations in 2018, were they running properly??
# not always - Wisner was essentially non-functional in 2018
ggplot(filter(visits), aes(recvDeployName)) +
  geom_bar(stat = "count") + geom_text(stat= "count", aes(label = ..count..), vjust = -1, size = 3) +
  facet_grid(year~.) + th + ylim(0,2000) +
  labs(y = "Number of site visits", x = NULL,
       title = "Detection Frequencies of tags at each station by year, \n ordered by distance from Exxon Fields")

#Longest visits at Grand_Isle both years BUT SHOULD PLAY WITH WHAT'S CONSIDERED A VISIT
ggplot(filter(visits, recvDeployName != "Exxon Fields"), aes(recvDeployName, visitLength)) +
         geom_boxplot() + facet_grid(year~.)

visits.tag <- visits %>% group_by(year, recvDeployName, motusTagID, mfgID) %>%
  summarize(meanLength = mean(visitLength),
            totVisits = length(visitLength))
p <- ggplot(visits, aes(recvDeployName, visitLength)) +
  geom_boxplot() + facet_grid(year~.)
ylim1 <- boxplot.stats(visits$visitLength)$stats[c(1,5)]
p + coord_cartesian(ylim = ylim1*1.05) + th +
  labs(title = "Length of visits (mins) to each station during foraging for 2017 and 2018, \n ordered by distance from Exxon Fields",
       y = "Detection time (mins)",
       x = NULL)
tmp <- aov(data = filter(visits, recvDeployName %in% c("East Grand Terre", "Exxon Fields", "Grand_Isle", "Wisner")), visitLength ~ recvDeployName)
summary(tmp) # there is a significant difference, among which?
TukeyHSD(tmp, conf.level=0.95) # sig difference between Grand_Isle and Exxon Fields
plot(TukeyHSD(tmp, conf.level = 0.95), las=1, col = "red")

visits.recv <- filter(visits.tag) %>% group_by(year, recvDeployName) %>%
  summarize(meanLength = mean(meanLength),
            meanVisits = mean(totVisits),
            nTags = length(unique(motusTagID)))

# So lets look at the birds individually that were picked up more than twice at a station - were these regular visits?
# ignore Exxon Fields and Grand_Isle
#23216 - no
ggplot(filter(inc, motusTagID == 23216), aes(ts, sig, col = recvDeployName)) + geom_point() +
  facet_grid(recvDeployName~.) +
  geom_vline(xintercept = inc$sunrise,col = "orange") +
  geom_vline(xintercept = inc$sunset, col = "blue")
# 23261 - maybe semi regularly
ggplot(filter(inc, motusTagID == 23261), aes(ts, sig, col = recvDeployName)) + geom_point() +
  facet_grid(recvDeployName~.) +
  geom_vline(xintercept = inc$sunrise,col = "orange") +
  geom_vline(xintercept = inc$sunset, col = "blue")
# 23270 - not to Galveston, those were likely after a failed nest sometime between May 18-22
ggplot(filter(inc, motusTagID == 23270), aes(ts, sig, col = recvDeployName)) + geom_point() +
  facet_grid(recvDeployName~.) +
  geom_vline(xintercept = inc$sunrise,col = "orange") +
  geom_vline(xintercept = inc$sunset, col = "blue")
# 28522 - yes to east grand terre
ggplot(filter(foraging, motusTagID == 28522), aes(ts, sig, col = recvDeployName)) + geom_point() +
  facet_grid(recvDeployName~.) +
  geom_vline(xintercept = inc$sunrise,col = "orange") +
  geom_vline(xintercept = inc$sunset, col = "blue")
# 28523 - not really
ggplot(filter(foraging, motusTagID == 28523), aes(ts, sig, col = recvDeployName)) + geom_point() +
  facet_grid(recvDeployName~.) +
  geom_vline(xintercept = inc$sunrise,col = "orange") +
  geom_vline(xintercept = inc$sunset, col = "blue")
# 28529 - maybe to east grand terre
ggplot(filter(foraging, motusTagID == 28529), aes(ts, sig, col = recvDeployName)) + geom_point() +
  facet_grid(recvDeployName~.) +
  geom_vline(xintercept = inc$sunrise,col = "orange") +
  geom_vline(xintercept = inc$sunset, col = "blue")
# 28602 - maybe to east grand terre
ggplot(filter(foraging, motusTagID == 28602), aes(ts, sig, col = recvDeployName)) + geom_point() +
  facet_grid(recvDeployName~.) +
  geom_vline(xintercept = inc$sunrise,col = "orange") +
  geom_vline(xintercept = inc$sunset, col = "blue")
# 28605 - yes to east grand terre BUT only after hatch
ggplot(filter(foraging, motusTagID == 28605), aes(ts, sig, col = recvDeployName)) + geom_point() +
  facet_grid(recvDeployName~.) +
  geom_vline(xintercept = inc$sunrise,col = "orange") +
  geom_vline(xintercept = inc$sunset, col = "blue")
# 28611 - no to east grand terre
ggplot(filter(foraging, motusTagID == 28611), aes(ts, sig, col = recvDeployName)) + geom_point() +
  facet_grid(recvDeployName~.) +
  geom_vline(xintercept = inc$sunrise,col = "orange") +
  geom_vline(xintercept = inc$sunset, col = "blue")
# 28615 - yes to east grand terre but likely only after failed nest
ggplot(filter(foraging, motusTagID == 28615), aes(ts, sig, col = recvDeployName)) + geom_point() +
  facet_grid(recvDeployName~.) +
  geom_vline(xintercept = inc$sunrise,col = "orange") +
  geom_vline(xintercept = inc$sunset, col = "blue")
# so most birds foraged either out of range of detection, or around nesting sites.

# So lets look at antenna bearings of all detections to see where most often detected
# get summary of foraging antenna bearings
#for.dir <- foraging %>% mutate(recvLat = round(recvLat, digits = 3))
for.dir <- foraging %>% group_by(year, recvDeployName, antBearing, recvLat, recvLon, recvDeployID) %>%
  summarize(nDet = length(antBearing)) %>% as.data.frame()
for.dir$siteYear <- paste(for.dir$recvDeployName, for.dir$year, sep = "")
# make a theme for the circular plots
thCirc <- theme_bw() + theme(axis.title.y = element_blank(),
                             legend.position = "none",
                             plot.title = element_text(hjust = 0.5),
                             axis.ticks.y = element_blank())
# make scales for all plots
sf <- scale_fill_gradient(low = "#3399FF", high = "#000033", limits = c(1, 7200))
cp <- coord_polar(theta = "x", start = 0)
sx <- scale_x_continuous("",limits = c(0,360), breaks = seq(0, 360-1, 45))
gb <- geom_bar(stat = "identity", width = 15)
# make a colour palette so it's uniform across plots
#myPalette <- colorRampPalette(rev(brewer.pal(11, "Spectral")))
#myPalette <- colorRampPalette(brewer.pal(9, "Blues"))(50)
#sc <- scale_fill_gradientn(colours = myPalette, limits = c(1, 7200))
#sc <- scale_fill_gradient(low = "#3399FF", high = "#000033", limits = c(1, 7200))
# make plot to get full legend across all plots
legendPlot <- ggplot(for.dir, aes(antBearing, nDet, fill = nDet)) +
  sf + cp + sx + gb + labs(fill = "Number of Detections") +
  theme(legend.direction = "horizontal",
        legend.position = "bottom")
# get common legend across all plots
g_legend<-function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)}
mylegend<-g_legend(legendPlot)

# 2017
# East Grand Terre
grandTerre17 <- ggplot(filter(for.dir, year == 2017, recvDeployName == "East Grand Terre"), aes(antBearing, nDet, fill = nDet)) +
  geom_vline(xintercept = filter(df.antDeps, recvDeployName == "East Grand Terre")$antBearing,col = "orange") +
  labs(title ="East Grand Terre") + sf + cp + sx + gb + thCirc
# Exxon Fields
exxon17 <- ggplot(filter(for.dir, year == 2017, recvDeployName == "Exxon Fields"), aes(antBearing, nDet, fill = nDet)) +
  geom_vline(xintercept = filter(df.antDeps, recvDeployName == "Exxon Fields")$antBearing,col = "orange") +
  labs(title ="Exxon Fields") + sf + cp + sx + gb + thCirc
# Grand_Isle
grandIsle17 <- ggplot(filter(for.dir, year == 2017, recvDeployName == "Grand_Isle"), aes(antBearing, nDet, fill = nDet)) +
  geom_vline(xintercept = filter(df.antDeps, recvDeployName == "Grand_Isle")$antBearing,col = "orange") +
  labs(title ="Grand Isle") + sf + cp + sx + gb + thCirc
# Quintana
quintana17 <- ggplot(filter(for.dir, year == 2017, recvDeployName == "Quintana"), aes(antBearing, nDet, fill = nDet)) +
  geom_vline(xintercept = filter(df.antDeps, recvDeployID == 2985)$antBearing,col = "orange") +
  labs(title ="Quintana") + sf + cp + sx + gb + thCirc
# San Bernard National Wildlife Refuge Headquarters
headquarters17 <- ggplot(filter(for.dir, year == 2017, recvDeployName == "San Bernard National Wildlife Refuge Headquarters"), aes(antBearing, nDet, fill = nDet)) +
  geom_vline(xintercept = filter(df.antDeps, recvDeployName == "San Bernard National Wildlife Refuge Headquarters")$antBearing,col = "orange") +
  labs(title ="San Bernard NWR HQ") + sf + cp + sx + gb + thCirc
# San Bernard NWR (Big Pond Unit)
bigPond17 <- ggplot(filter(for.dir, year == 2017, recvDeployName == "San Bernard NWR (Big Pond Unit)"), aes(antBearing, nDet, fill = nDet)) +
  geom_vline(xintercept = filter(df.antDeps, recvDeployName == "San Bernard NWR (Big Pond Unit)")$antBearing,col = "orange") +
  labs(title ="San Bernard NWR (Big Pond Unit)") + sf + cp + sx + gb + thCirc
# Scenic Galveston
galveston17 <- ggplot(filter(for.dir, year == 2017, recvDeployName == "Scenic Galveston"), aes(antBearing, nDet, fill = nDet)) +
  geom_vline(xintercept = filter(df.antDeps, recvDeployName == "Scenic Galveston")$antBearing,col = "orange") +
  labs(title ="Galveston") + sf + cp + sx + gb + thCirc
# Wisner
wisner17 <- ggplot(filter(for.dir, year == 2017, recvDeployName == "Wisner"), aes(antBearing, nDet, fill = nDet)) +
  geom_vline(xintercept = filter(df.antDeps, recvDeployName == "Wisner")$antBearing,col = "orange") +
  labs(title ="Wisner") + sf + cp + sx + gb + thCirc
# all of 2017
grid.arrange(arrangeGrob(exxon17, grandIsle17, wisner17, grandTerre17,
             galveston17, bigPond17, quintana17, headquarters17,
             nrow = 4,
             layout_matrix = rbind(c(1, 1, 3, 4),
                                   c(1, 1, 3, 4),
                                   c(2, 2, 5, 6),
                                   c(2, 2, 7, 8))),
             mylegend, nrow = 2, heights = c(20,1),
             top = "2017: Number of detections by antenna bearing")

# 2018
# Bolivar_Flats
bolivar18 <- ggplot(filter(for.dir, year == 2018, recvDeployName == "Bolivar_Flats"), aes(antBearing, nDet, fill = nDet)) +
  geom_vline(xintercept = filter(df.antDeps, recvDeployID == 4615)$antBearing,col = "orange") +
  labs(title ="Bolivar Flats") + sf + cp + sx + gb + thCirc
# East Grand Terre
grandTerre18 <- ggplot(filter(for.dir, year == 2018, recvDeployName == "East Grand Terre"), aes(antBearing, nDet, fill = nDet)) +
  geom_vline(xintercept = filter(df.antDeps, recvDeployName == "East Grand Terre")$antBearing,col = "orange") +
  labs(title ="East Grand Terre") + sf + cp + sx + gb + thCirc
# Exxon Fields
exxon18 <- ggplot(filter(for.dir, year == 2018, recvDeployName == "Exxon Fields"), aes(antBearing, nDet, fill = nDet)) +
  geom_vline(xintercept = filter(df.antDeps, recvDeployName == "Exxon Fields")$antBearing,col = "orange") +
  labs(title ="Exxon Fields") + sf + cp + sx + gb + thCirc
# Grand_Isle
grandIsle18 <- ggplot(filter(for.dir, year == 2018, recvDeployName == "Grand_Isle"), aes(antBearing, nDet, fill = nDet)) +
  geom_vline(xintercept = filter(df.antDeps, recvDeployName == "Grand_Isle")$antBearing,col = "orange") +
  labs(title ="Grand Isle") + sf + cp + sx + gb + thCirc
# all of 2018
grid.arrange(arrangeGrob(exxon18, grandIsle18, grandTerre18, bolivar18,
             nrow = 2,
             layout_matrix = rbind(c(1, 2),
                                   c(3, 4))),
             mylegend, nrow = 2, heights = c(20,1),
             top = "2018: Number of detections by antenna bearing")


# directions for mapping
thBearingMap <- theme(axis.ticks.y = element_blank(),
                      axis.text.y = element_blank(),
                      axis.text.x = element_blank(),
                      panel.background = element_rect(fill = "transparent", colour = NA),
                      plot.background = element_rect(fill= "transparent", colour = NA),
                      legend.position = "none",
                      panel.grid.major = element_line(colour = "grey"),
                      panel.grid.minor = element_line(colour = NA),
                      axis.line = element_line(colour = NA))

sites <- unique(for.dir$siteYear)
# create list with all sites and year
out.plot <- list()
for(i in sites) {
  out.plot[[i]] <- sites[i]
}
# create direction plots for all sites
for(i in 1:length(sites)) {
  data <- as.data.frame(filter(for.dir, siteYear %in% sites[i]))
  out.plot[[i]] <- ggplot(data, aes_string(x = "antBearing", y = "nDet", fill = "nDet")) +
    geom_bar(stat = "identity", width = 5) + scale_x_continuous("",limits = c(0,360), breaks = seq(0, 360-1, 45)) +
    coord_polar(theta = "x", start = 0) + ylab("") + thBearingMap + labs(title = "siteYear")
}
# to see the plots:
print(out.plot[["Exxon Fields2017"]])

# Create limits for each station
for.dir <- for.dir %>%
  mutate(xmin = (recvLon - 1),
         xmax = (recvLon + 1),
         ymin = (recvLat - 1),
         ymax = (recvLat + 1))
EGrandTerre17 <- out.plot[["East Grand Terre2017"]]
Exxon17 <- out.plot[["Exxon Fields2017"]]
Grand_Isle2017 <- out.plot[["Grand_Isle2017"]]
Quintana2017 <- out.plot[["Quintana2017"]]
SanBernardHQ17 <- out.plot[["San Bernard National Wildlife Refuge Headquarters2017"]]
SanBernardBigPond17 <- out.plot[["San Bernard NWR (Big Pond Unit)2017"]]
Galveston17 <- out.plot[["Scenic Galveston2017"]]
Wisner2017 <- out.plot[["Wisner2017"]]
EGrandTerre18 <- out.plot[["East Grand Terre2018"]]
Bolivar18 <- out.plot[["Bolivar_Flats2018"]]
Exxon18 <- out.plot[["Exxon Fields2018"]]
GrandIsle18 <- out.plot[["Grand_Isle2018"]]

ggplot(na.map, aes(long, lat)) +
  geom_polygon(data = na.map, aes(long, lat, group = group), colour = "grey", fill = "blue") +
  geom_polygon(aes(group = group), colour = "grey", fill = "grey98") +
  coord_map(projection = "mercator", xlim = c(-98, -88), ylim = c(25, 32)) +
  geom_point(data = filter(df.recvDeps17, active17 == TRUE), aes(longitude, latitude), pch = 21, colour = "black", fill = "red") +
  geom_point(data = inc, aes(recvLon, recvLat), pch = 21, colour = "black", fill = "yellow") +
  xlab("") + ylab("") + theme_bw() +
  inset(ggplotGrob(EGrandTerre17),
        xmin = unique(filter(for.dir, siteYear == "East Grand Terre2017")$xmin),
        xmax = unique(filter(for.dir, siteYear == "East Grand Terre2017")$xmax),
        ymin = unique(filter(for.dir, siteYear == "East Grand Terre2017")$ymin),
        ymax = unique(filter(for.dir, siteYear == "East Grand Terre2017")$ymax)) +
  inset(ggplotGrob(Exxon17),
        xmin = unique(filter(for.dir, siteYear == "Exxon Fields2017")$xmin),
        xmax = unique(filter(for.dir, siteYear == "Exxon Fields2017")$xmax),
        ymin = unique(filter(for.dir, siteYear == "Exxon Fields2017")$ymin),
        ymax = unique(filter(for.dir, siteYear == "Exxon Fields2017")$ymax)) +
  inset(ggplotGrob(Grand_Isle2017),
        xmin = unique(filter(for.dir, siteYear == "Grand_Isle2017")$xmin),
        xmax = unique(filter(for.dir, siteYear == "Grand_Isle2017")$xmax),
        ymin = unique(filter(for.dir, siteYear == "Grand_Isle2017")$ymin),
        ymax = unique(filter(for.dir, siteYear == "Grand_Isle2017")$ymax)) +
  inset(ggplotGrob(Quintana2017),
        xmin = unique(filter(for.dir, siteYear == "Quintana2017")$xmin),
        xmax = unique(filter(for.dir, siteYear == "Quintana2017")$xmax),
        ymin = unique(filter(for.dir, siteYear == "Quintana2017")$ymin),
        ymax = unique(filter(for.dir, siteYear == "Quintana2017")$ymax)) +
  inset(ggplotGrob(SanBernardHQ17),
        xmin = unique(filter(for.dir, siteYear == "San Bernard National Wildlife Refuge Headquarters2017")$xmin),
        xmax = unique(filter(for.dir, siteYear == "San Bernard National Wildlife Refuge Headquarters2017")$xmax),
        ymin = unique(filter(for.dir, siteYear == "San Bernard National Wildlife Refuge Headquarters2017")$ymin),
        ymax = unique(filter(for.dir, siteYear == "San Bernard National Wildlife Refuge Headquarters2017")$ymax)) +
  inset(ggplotGrob(SanBernardBigPond17),
        xmin = unique(filter(for.dir, siteYear == "San Bernard NWR (Big Pond Unit)2017")$xmin),
        xmax = unique(filter(for.dir, siteYear == "San Bernard NWR (Big Pond Unit)2017")$xmax),
        ymin = unique(filter(for.dir, siteYear == "San Bernard NWR (Big Pond Unit)2017")$ymin),
        ymax = unique(filter(for.dir, siteYear == "San Bernard NWR (Big Pond Unit)2017")$ymax)) +
  inset(ggplotGrob(Galveston17),
        xmin = unique(filter(for.dir, siteYear == "Scenic Galveston2017")$xmin),
        xmax = unique(filter(for.dir, siteYear == "Scenic Galveston2017")$xmax),
        ymin = unique(filter(for.dir, siteYear == "Scenic Galveston2017")$ymin),
        ymax = unique(filter(for.dir, siteYear == "Scenic Galveston2017")$ymax)) +
  inset(ggplotGrob(Wisner2017),
        xmin = unique(filter(for.dir, siteYear == "Wisner2017")$xmin),
        xmax = unique(filter(for.dir, siteYear == "Wisner2017")$xmax),
        ymin = unique(filter(for.dir, siteYear == "Wisner2017")$ymin),
        ymax = unique(filter(for.dir, siteYear == "Wisner2017")$ymax))









# look at antenna directions for these areas
ggplot(foraging, aes(antBearing)) +
  geom_bar(stat = "count") + geom_text(stat= "count", aes(label = ..count..), vjust = -1) +
  facet_grid(recvDeployName~year)


#directions
ggplot(filter(for.dir, year == 2017), aes(antBearing, nDet, fill = nDet)) +
  geom_bar(stat = "identity", width = 5) + scale_x_continuous("",limits = c(0,360), breaks = seq(0, 360-1, 45)) +
  coord_polar(theta = "x", start = 0) +
  theme(axis.ticks.y = element_blank(),
        axis.text.y = element_blank()) +
  ylab("") + guides(fill=guide_legend(title = "number of detections"))




## LONGER WAY TO MAKE INDIVIDUAL MAPPING WINDROSES
# 2017
# East Grand Terre
grandTerre17 <- ggplot(filter(for.dir, year == 2017, recvDeployName == "East Grand Terre"), aes(antBearing, nDet, fill = "red")) +
  geom_bar(stat = "identity", width = 5) + scale_x_continuous("",limits = c(0,360), breaks = seq(0, 360-1, 45)) +
  coord_polar(theta = "x", start = 0) + ylab("") + thBearingMap
ggsave(tmp, filename = "EGrandTerre.png", bg = "transparent", device = NULL)
# Exxon Fields
exxon17 <- ggplot(filter(for.dir, year == 2017, recvDeployName == "Exxon Fields"), aes(antBearing, nDet, fill = nDet)) +
  geom_bar(stat = "identity", width = 5) + scale_x_continuous("",limits = c(0,360), breaks = seq(0, 360-1, 45)) +
  coord_polar(theta = "x", start = 0)  + ylab("") + thBearingMap
# Grand_Isle
grandIsle17 <- ggplot(filter(for.dir, year == 2017, recvDeployName == "Grand_Isle"), aes(antBearing, nDet, fill = nDet)) +
  geom_bar(stat = "identity", width = 5) + scale_x_continuous("",limits = c(0,360), breaks = seq(0, 360-1, 45)) +
  coord_polar(theta = "x", start = 0) + ylab("") + thBearingMap
# Quintana
quintana17 <- ggplot(filter(for.dir, year == 2017, recvDeployName == "Quintana"), aes(antBearing, nDet, fill = nDet)) +
  geom_bar(stat = "identity", width = 5) + scale_x_continuous("",limits = c(0,360), breaks = seq(0, 360-1, 45)) +
  coord_polar(theta = "x", start = 0)  + ylab("") + thBearingMap
# San Bernard National Wildlife Refuge Headquarters
headquarters17 <- ggplot(filter(for.dir, year == 2017, recvDeployName == "San Bernard National Wildlife Refuge Headquarters"), aes(antBearing, nDet, fill = nDet)) +
  geom_bar(stat = "identity", width = 5) + scale_x_continuous("",limits = c(0,360), breaks = seq(0, 360-1, 45)) +
  coord_polar(theta = "x", start = 0)  + ylab("") + thBearingMap
# San Bernard NWR (Big Pond Unit)
bigPond17 <- ggplot(filter(for.dir, year == 2017, recvDeployName == "San Bernard NWR (BPU)"), aes(antBearing, nDet, fill = nDet)) +
  geom_bar(stat = "identity", width = 5) + scale_x_continuous("",limits = c(0,360), breaks = seq(0, 360-1, 45)) +
  coord_polar(theta = "x", start = 0)  + ylab("") + thBearingMap
# Scenic Galveston
galveston17 <- ggplot(filter(for.dir, year == 2017, recvDeployName == "Scenic Galveston"), aes(antBearing, nDet, fill = nDet)) +
  geom_bar(stat = "identity", width = 5) + scale_x_continuous("",limits = c(0,360), breaks = seq(0, 360-1, 45)) +
  coord_polar(theta = "x", start = 0) + ylab("") + thBearingMap
# Wisner
wisner17 <- ggplot(filter(for.dir, year == 2017, recvDeployName == "Wisner"), aes(antBearing, nDet, fill = nDet)) +
  geom_bar(stat = "identity", width = 5) + scale_x_continuous("",limits = c(0,360), breaks = seq(0, 360-1, 45)) +
  coord_polar(theta = "x", start = 0) + ylab("") + thBearingMap
# 2018
# Bolivar_Flats
bolivar18 <- ggplot(filter(for.dir, year == 2018, recvDeployName == "Bolivar_Flats"), aes(antBearing, nDet, fill = nDet)) +
  geom_bar(stat = "identity", width = 5) + scale_x_continuous("",limits = c(0,360), breaks = seq(0, 360-1, 45)) +
  coord_polar(theta = "x", start = 0) + ylab("") + thBearingMap
# East Grand Terre
grandTerre18 <- ggplot(filter(for.dir, year == 2018, recvDeployName == "East Grand Terre"), aes(antBearing, nDet, fill = nDet)) +
  geom_bar(stat = "identity", width = 5) + scale_x_continuous("",limits = c(0,360), breaks = seq(0, 360-1, 45)) +
  coord_polar(theta = "x", start = 0) + ylab("") + thBearingMap
# Exxon Fields
exxon18 <- ggplot(filter(for.dir, year == 2018, recvDeployName == "Exxon Fields"), aes(antBearing, nDet, fill = nDet)) +
  geom_bar(stat = "identity", width = 5) + scale_x_continuous("",limits = c(0,360), breaks = seq(0, 360-1, 45)) +
  coord_polar(theta = "x", start = 0) + ylab("") + thBearingMap
# Grand_Isle
grandIsle18 <- ggplot(filter(for.dir, year == 2018, recvDeployName == "Grand_Isle"), aes(antBearing, nDet, fill = nDet)) +
  geom_bar(stat = "identity", width = 5) + scale_x_continuous("",limits = c(0,360), breaks = seq(0, 360-1, 45)) +
  coord_polar(theta = "x", start = 0) + ylab("") + thBearingMap



gmap <- get_map(location = c(lon = -93, lat = 29), maptype = "satellite", source = "google", zoom = 6)
p <- ggmap(gmap)
p + geom_point(data = filter(df.recvDeps17, active17 == TRUE), aes(longitude, latitude), pch = 21, colour = "black", fill = "red") +
  geom_point(data = inc, aes(recvLon, recvLat), pch = 21, colour = "black", fill = "yellow") +
  inset(ggplotGrob(exxon18),
        xmin = filter(tmp, siteYear == "East Grand Terre2017")$xmin,
        xmax = filter(tmp, siteYear == "East Grand Terre2017")$xmax,
        ymin = filter(tmp, siteYear == "East Grand Terre2017")$ymin,
        ymax = filter(tmp, siteYear == "East Grand Terre2017")$ymax)


for(i in sites) {
  #df <- filter(for.dir, siteYear == i)
  out.plot[[i]] <- ggplot(filter(for.dir, siteYear %in% sites[i]), aes_string(x = "antBearing", y = "nDet", fill = "nDet")) +
    geom_bar(stat = "identity", width = 5) + scale_x_continuous("",limits = c(0,360), breaks = seq(0, 360-1, 45)) +
    coord_polar(theta = "x", start = 0) + ylab("") + thBearingMap
  #ggsave(out.plot[i], filename = paste("bearings_", sites[i], ".png", sep = ""))
  #print(out.plot)
  #plot_list[[i]] = p
}

ggplot(filter(for.dir, year == 2017), aes(antBearing, nDet, fill = "white")) +
  geom_bar(stat = "identity", width = 10) + scale_x_continuous("",limits = c(0,360), breaks = seq(0, 360-1, 45)) +
  coord_polar(theta = "x", start = 0) +
 +
  ylab("") + guides(fill=guide_legend(title = "number of detections"))

ggplot(filter(for.dir, year == 2018, recvDeployName == "Grand_Isle"), aes(antBearing, nDet, fill = nDet)) +
  geom_bar(stat = "identity", width = 5) + scale_x_continuous("",limits = c(0,360), breaks = seq(0, 360-1, 45)) +
  coord_polar(theta = "x", start = 0) +
  ylab("") + guides(fill=guide_legend(title = "number of detections")) + labs(title ="Grand Isle") + thBearingMap


ggsave(directionplot, file = "directionplot.png", bg = "transparent", device = NULL)

ggsave(p, file = "map.png", device = NULL)
# https://rpubs.com/Kaden_Loring/WindRoseMap_Transposition
mappng <- readImage("~/directionplot.png")
pplot <- readImage("~/map.png")
tmp <- readImage("/EGrandTerre.png")
bar_annotation_list <- annotation_custom(circplot, xmin = -90 - 5e3,
                                         xmax = -90 + 5e3,
                                         ymin = 29.2 - 5e3,
                                         ymax = 29.2 + 5e3)

(p + directionplot)

gmap <- get_map(location = c(lon = -93, lat = 29), maptype = "satellite", source = "google", zoom = 6)
p <- ggmap(gmap)
p + geom_point(data = filter(df.recvDeps17, active17 == TRUE), aes(longitude, latitude), pch = 21, colour = "black", fill = "red") +
  geom_point(data = inc, aes(recvLon, recvLat), pch = 21, colour = "black", fill = "yellow") +
  inset(ggplotGrob(directionplot), xmin = -90.0, xmax = -90.1, ymin = 29.1, ymax = 29.2)


  tmp <- for.dir %>%
    mutate(xmin = (recvLon - 0.1),
              xmax = (recvLon + 0.1),
              ymin = (recvLat - 0.1),
              ymax = (recvLat + 0.1))
  tmp$siteYear <- paste(tmp$recvDeployName, tmp$year, sep = "")
tmp <- select(tmp, siteYear, xmin, xmax, ymin, ymax)
tmp <- unique(tmp)

circ <- circular(for.dir$antBearing, template = "geographics", type = "directions", units = "degrees", rotation = "clock")
rose.diag(circ, bins = 18, col = "gray")

## 2) Mean distances travelled from nest site to foraging destinations during incubation
summary(visits$distFromExxon)
# mean of 1670m, max of 569092m
summary(filter(visits, year == 2017)$distFromExxon) # mean 2528m, max 569092
summary(filter(visits, year == 2018)$distFromExxon) # mean 866.4m, max 464126.4m
table(visits$year, visits$recvDeployName)
# but in 2017 on 7 visits away from breeding area (Exxon, Grand Isle, E Grand Terre, Wisner), in 2018 only 1 visit away
# So really 4,366/4,374 = 99.8% were within 23072m from Exxon (furthest being Wisner)



