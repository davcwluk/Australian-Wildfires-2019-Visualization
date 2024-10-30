library(maps)
library(mapdata) # "a cleaned-up version of the CIA World Data Bank II data"

# print a A4 size PDF
pdf("output.pdf", width = 8.27, height = 11.69, bg = "snow", family = "ArialMT")

setwd("~/Desktop/Study/IM942 Visualisation Foundations/Coursework 2/")
fia <- read.csv("fire_nrt_M6_96062.csv", sep = ",")
fia$acq_date <- as.Date(fia$acq_date, format = "%d/%m/%Y")

# add a column in dataset which scan times track as the area
fia$scan_track <- fia$scan * fia$track

# layout matrix for the infographic 
layout_matrix <- matrix(c(1,1,1,1,
                          2,3,3,4,
                          2,3,3,4,
                          5,6,7,8,
                          9,10,11,12,
                          13,13,14,14,
                          13,13,14,14,
                          15,15,15,15), byrow = TRUE, nrow = 8)
layout(layout_matrix)

# main title
par(mar = c(0, 0, 0, 0), oma = c(0, 0, 0, 0))
plot.new()
text(x = 0.5, y = 0.8, "IM942 Visualisation Foundations - Graphical Report", cex = 2.5)
text(x = 0.5, y = 0.6, "Australian wildfires 2019", cex = 2.5)
text(x = 0.5, y = 0.25, "Total fires area", cex = 1.5)
text(x = 0.5, y = 0.15, "from 05/12/2019 to 05/01/2020", cex = 1)

# left text
par(mar = c(0, 0, 0, 0))
plot.new()
text(x = 0.12, y = 0.7, 
"This visualization report of the 2019
Australian wildfires presents the incidents
with different views. Under the main map
with details of all fire areas, a sequence
of small maps shows the progression of
fires over the time. Moreover, a heatmap with
day and night fire brightness
and a line graph that estimates the change
of fire size over time are shown.",
cex = 0.82, adj = 0)

# main map
map(
  database = "world",
  regions = "Australia",
  fill = TRUE,
  col = "darkolivegreen1",
  mar = c(0, 0, 0, 0),
  bg = "lightyellow"
)
brightness_color <- colorRamp(c("lightyellow", "red"))
point_color_scale <-
  (fia$confidence - min(fia$confidence)) / (max(fia$confidence) - min(fia$confidence))
point_color <-
  rgb(brightness_color(point_color_scale), maxColorValue = 255)
points(
  fia$longitude,
  fia$latitude,
  pch = 19,
  cex = 0.5,
  col = point_color
)

# colour indicator
color_ramp_matrix <- matrix(1:100, nrow = 1)
color_palette <- colorRampPalette(c("lightyellow", "red"))(100)
y = seq(0, 1, len = 100)
par(mar = c(7, 5, 0, 10))
image(1, y, color_ramp_matrix, col = color_palette, axes = FALSE, xlab = "", ylab = "")
axis(side = 4, at = seq(0, 1, by = 0.1), labels = seq(0, 1, by = 0.1), las = 2)
mtext(side = 2, text = "Confidence level", line = 1, las = 2, cex = 0.5)

# 8 small multiples
# find the min date and max, and divide the period into 8 pieces
fia_min_date <- min(fia$acq_date)
fia_max_date <- max(fia$acq_date)
fia_seq <- seq(from = fia_min_date, by = (fia_max_date - fia_min_date)/8, length.out = 9)
fia_part1 <- subset(fia, acq_date >= fia_seq[1] & acq_date <= fia_seq[2])
fia_part2 <- subset(fia, acq_date >= fia_seq[1] & acq_date <= fia_seq[3])
fia_part3 <- subset(fia, acq_date >= fia_seq[1] & acq_date <= fia_seq[4])
fia_part4 <- subset(fia, acq_date >= fia_seq[1] & acq_date <= fia_seq[5])
fia_part5 <- subset(fia, acq_date >= fia_seq[1] & acq_date <= fia_seq[6])
fia_part6 <- subset(fia, acq_date >= fia_seq[1] & acq_date <= fia_seq[7])
fia_part7 <- subset(fia, acq_date >= fia_seq[1] & acq_date <= fia_seq[8])
fia_part8 <- subset(fia, acq_date >= fia_seq[1] & acq_date <= fia_seq[9])

# colour progression
color_palette_8maps <- colorRampPalette(c("orange", "red"))
colors_8maps <- color_palette_8maps(8)

# plot the map
par(mar = c(0, 0, 0, 0))
map(database = "world", regions = "Australia", fill = TRUE, col = "darkolivegreen1")
title(main = "08/12/2019", font.main = 1, line=1)
points(fia_part1$longitude, fia_part1$latitude, pch = 19, cex = 0.05, col = colors_8maps[1])
par(mar = c(0, 0, 0, 0))
map(database = "world", regions = "Australia", fill = TRUE, col = "darkolivegreen1")
title(main = "12/12/2019", font.main = 1, line=1)
points(fia_part2$longitude, fia_part2$latitude, pch = 19, cex = 0.05, col = colors_8maps[2])
par(mar = c(0, 0, 0, 0))
map(database = "world", regions = "Australia", fill = TRUE, col = "darkolivegreen1")
title(main = "16/12/2019", font.main = 1, line=1)
points(fia_part3$longitude, fia_part3$latitude, pch = 19, cex = 0.05, col = colors_8maps[3])
par(mar = c(0, 0, 0, 0))
map(database = "world", regions = "Australia", fill = TRUE, col = "darkolivegreen1")
title(main = "20/12/2019", font.main = 1, line=1)
points(fia_part4$longitude, fia_part4$latitude, pch = 19, cex = 0.05, col = colors_8maps[4])
par(mar = c(0, 0, 0, 0))
map(database = "world", regions = "Australia", fill = TRUE, col = "darkolivegreen1")
title(main = "24/12/2019", font.main = 1, line=1)
points(fia_part5$longitude, fia_part5$latitude, pch = 19, cex = 0.05, col = colors_8maps[5])
par(mar = c(0, 0, 0, 0))
map(database = "world", regions = "Australia", fill = TRUE, col = "darkolivegreen1")
title(main = "28/12/2019", font.main = 1, line=1)
points(fia_part6$longitude, fia_part6$latitude, pch = 19, cex = 0.05, col = colors_8maps[6])
par(mar = c(0, 0, 0, 0))
map(database = "world", regions = "Australia", fill = TRUE, col = "darkolivegreen1")
title(main = "01/01/2020", font.main = 1, line=1)
points(fia_part7$longitude, fia_part7$latitude, pch = 19, cex = 0.05, col = colors_8maps[7])
par(mar = c(0, 0, 0, 0))
map(database = "world", regions = "Australia", fill = TRUE, col = "darkolivegreen1")
title(main = "05/01/2020", font.main = 1, line=1)
points(fia_part8$longitude, fia_part8$latitude, pch = 19, cex = 0.05, col = colors_8maps[8])

# heatmap of daytime and nighttime fire
# divide the data into day and night
fia_day <- fia[fia$daynight == 'D', ]
fia_night <- fia[fia$daynight == 'N', ]
fia_day_by_date <- split(fia_day, fia_day$acq_date)
fia_night_by_date <- split(fia_night, fia_night$acq_date)
fia_day_brightness_sum_by_date <- sapply(fia_day_by_date, function(df) sum(df$brightness))
fia_night_brightness_sum_by_date <- sapply(fia_night_by_date, function(df) sum(df$brightness))

# create matrix for heatmap operations
fia_day_brightness_matrix <- matrix(fia_day_brightness_sum_by_date, ncol = 1)
fia_night_brightness_matrix <- matrix(fia_night_brightness_sum_by_date, ncol = 1)
rownames(fia_day_brightness_matrix) <- names(fia_day_by_date)
rownames(fia_night_brightness_matrix) <- names(fia_night_by_date)
fia_brightness_matrix <- t(cbind(fia_day_brightness_matrix, fia_night_brightness_matrix))
rownames(fia_brightness_matrix) <- c("Total brightness temperature of daytime fire", "Total brightness temperature of nighttime fire")

# plot the heatmap
zlim <- range(fia_brightness_matrix, na.rm = TRUE)
breaks <-
  seq(zlim[1], zlim[2], length.out = length(colorRampPalette(c(
    "lightyellow", "red"
  ))(255)) + 1)
par(mar = c(5, 5, 5, 5))
image(1:ncol(fia_brightness_matrix), 1:nrow(fia_brightness_matrix), t(fia_brightness_matrix),
      col = colorRampPalette(c("lightyellow", "red"))(255), 
      breaks = breaks, 
      yaxt = "n",
      xaxt = "n",
      ylab = "",
      xlab = "",
      main = "Heatmap of daytime and nighttime fire")
heatmap_axis <- 1:length(colnames(fia_brightness_matrix))
axis.Date(
  1,
  at = heatmap_axis,
  labels = colnames(fia_brightness_matrix),
  las = 2,
  cex.axis = 0.7
)
text(16, 2, "Nighttime fire total brightness", cex = 1.5)
text(16, 1, "Daytime fire total brightness", cex = 1.5)

# total fire area per day calculated by scan and trac
# divide the data by dates
fia_by_date <- split(fia, fia$acq_date)
fia_area_sum_by_date <- sapply(fia_by_date, function(df) sum(df$scan_track))
# create a dataframe with date and scan*track as the fire area
dates <- as.Date(names(fia_area_sum_by_date), format = "%Y-%m-%d")
values <- as.numeric(fia_area_sum_by_date)
fia_area_sum_by_date_df <- data.frame(Date = dates, Value = values)
fia_area_sum_by_date_df <- fia_area_sum_by_date_df[order(fia_area_sum_by_date_df$Date),]
# plot the line graph
par(mar = c(5, 5, 5, 5))
plot(fia_area_sum_by_date_df, type = "l", col = "red", xaxt = "n", xlab = "", ylab = "", las = 1,
     main = "Total fire area per day calculated by scan and track")
mtext(side = 2, text = "Scan and track area*", line = 4, cex = 0.8)
axis.Date(1, at = fia_area_sum_by_date_df$Date, las = 2, cex.axis = 0.7, format = "%Y-%m-%d")

# footer part for caution, data source and citation
par(mar = c(0, 0, 0, 0))
plot.new()
text(x = 0.02, y = 0.7, "*With caution: The fire area are only estimated from MODIS 'scan' and 'track' data\nIt may be different from the actual size due to increasing pixels toward the edge.", cex = 0.8, adj = 0)
text(x = 0.95, y = 0.7, "Data Source: MODIS Collection 61 NRT Hotspot / \nActive Fire Detections MCD14DL distributed from NASA FIRMS.\nAvailable on-line https://earthdata.nasa.gov/firms. doi:10.5067/FIRMS/MODIS/MCD14DL.NRT.0061", cex = 0.8, adj = 1)
text(x = 0.95, y = 0.5, "R Core Team (2023). _R: A Language and Environment for Statistical Computing_. R\nFoundation for Statistical Computing, Vienna, Austria. <https://www.R-project.org/>.", cex = 0.8, adj = 1)

dev.off()
