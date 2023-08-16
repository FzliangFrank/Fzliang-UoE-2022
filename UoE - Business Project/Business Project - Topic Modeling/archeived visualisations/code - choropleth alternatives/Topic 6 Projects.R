
load("RData/enviroment.RData")
load("RData/stm_20.RData")
library(tidyverse)
library(sp)
library(leaflet)
library(reshape2)
library(tidytext)


meta %>%
  mutate(StartDate = as.Date(StartDate, format = "%d/%m/%Y")) %>%
  mutate(EndDate = as.Date(EndDate, format = "%d/%m/%Y")) %>%
  mutate(date = StartDate - min(StartDate)) %>%
  mutate(date = as.numeric(date)) %>%
  mutate(duration = EndDate - StartDate) %>%
  mutate(price = AwardPounds/as.numeric(duration)) -> meta

dplyr::select(meta, ProjectReference) %>%
  mutate(x = 1:length(ProjectReference)) -> index

stm_20 %>% tidy(matrix = "theta") %>%
  group_by(document) %>%
  dcast(document ~ topic, value.var = "gamma") %>%
  left_join(index, by = c("document" = "x")) %>%
  melt(id.vars = c("ProjectReference", "document")) %>%
  ungroup() %>%
  group_by(ProjectReference) %>% # collapse
  dplyr::mutate(max_proportion = max(value)) %>% 
  filter(max_proportion == value) %>%
  arrange(document) %>%
  left_join(meta) %>%
  filter(max_proportion > 0.75) %>% # filter parameter is any projects propotion that is 0.75
  dplyr::rename(topic = variable) %>%
  filter(topic == 6) %>% # this is the part filter topic 6 only
  group_by(LeadROName) %>%
  filter(LeadROName != "The Alan Turing Institute") %>%
  dplyr::summarise(x = mean(price)) %>% # this is anohter x
  dplyr::mutate(r = 50*((x - min(x))/(max(x) - min(x))) + 1) %>%
  left_join(geocode, by = c("LeadROName" = "LeadROName")) -> data

library(rgeos)
library(raster)
UK <- getData("GADM", country = "GB", level = 2)
crswgs84 <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")

data %>%
  dplyr::select(longitude, latitude) %>% #long first lat second!
  SpatialPoints(proj4string = crswgs84) -> data_sp
mtrx <- gContains(UK, data_sp, byid = TRUE)
mtrx_r <- colSums(data$r%*%mtrx)/colSums(mtrx)
mtrx_x <- colSums(data$x%*%mtrx)/colSums(mtrx)
mate <- data.frame(id = names(mtrx_r), r = as.numeric(mtrx_r), x = as.numeric(mtrx_x))
UK$c <- mate$r
UK$data <- mate$x

# spikes making 
rail <- c(1:length(data$x))
plist = c()
w = 0.06
h = 0.1
monitor <- NULL
for (i in rail) {
  la = as.numeric(data[i,5])
  lg = as.numeric(data[i,6])
  hi = sqrt(as.numeric(data[i,3]))
  x_coord <- c(lg - w, lg, lg + w, lg - w)
  y_coord <- c(la, la + h*hi, la, la)
  xym <- cbind(x_coord, y_coord)
  p = Polygon(xym)
  ps = Polygons(list(p), ID = i)
  plist = c(plist, ps)
  
  row = cbind(la, lg)
  monitor = rbind(monitor, row)
}
spike <- sp::SpatialPolygons(plist)
proj4string(spike) = CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
plot(spike)
spikes <- SpatialPolygonsDataFrame(spike, data)

# graph
palette_theme = "Blues"

mypal <- colorNumeric(palette = palette_theme, domain = UK$data, na.color = "transparent")
mypal_2 <- colorNumeric(palette = palette_theme, domain = sqrt(spikes$x), na.color = "transparent")
text_style <- list(
  "color" = "black",
  "font-family" = "Futura",
  "font-style" = "bold",
  "text-shadow" = "3px 3px rgba(0,0,0,0.25)",
  "text-outline" = "white"
)
leaflet() %>% 
  setView(lat = 55.3781, lng = -3.4360, zoom = 5) %>% 
  addProviderTiles(providers$CartoDB.PositronNoLabels) %>% 
  addPolygons(data = UK, stroke = TRUE, 
              fillColor = ~mypal(data),
              fillOpacity = 0.6, col = "black", weight = 0.4,
              highlightOptions = highlightOptions(opacity = 1,
                                                  weight = 1.5,
                                                  bringToFront = FALSE),
              label = paste(UK$NAME_2, round(UK$data)) ) %>%
  addLegend("bottomright", pal = mypal, values = UK$data, labFormat = labelFormat(prefix = "£/d "), opacity = 1) %>%
  addPolygons(data = spikes, col = "black", stroke = TRUE, fillColor = ~mypal_2(sqrt(x)), fillOpacity = 1, weight = 1, opacity = 0.75,
              highlightOptions = highlightOptions(fillOpacity = 2,
                                                  weight = 2,
                                                  bringToFront = TRUE),
              label = paste(spikes$LeadROName, "£/d", round(spikes$x))
  )



## there is just one project. Quite accurate actually 
stm_20 %>% tidy(matrix = "theta") %>%
  group_by(document) %>%
  dcast(document ~ topic, value.var = "gamma") %>%
  left_join(index, by = c("document" = "x")) %>%
  melt(id.vars = c("ProjectReference", "document")) %>%
  ungroup() %>%
  group_by(ProjectReference) %>% # collapse
  dplyr::mutate(max_proportion = max(value)) %>% 
  filter(max_proportion == value) %>%
  arrange(document) %>%
  left_join(meta) %>%
  filter(max_proportion > 0.5) %>% # filter parameter is any projects propotion that is 0.75
  dplyr::rename(topic = variable) %>% # this is the part filter topic 6 only
  filter(topic == 6) %>%
  group_by(LeadROName) %>%
  lookup("LeadROName", "surrey") %>%
  pull(ProjectReference) %>% viz_id()

## explore
stm_20 %>% tidy(matrix = "theta") %>%
  group_by(document) %>%
  dcast(document ~ topic, value.var = "gamma") %>%
  left_join(index, by = c("document" = "x")) %>%
  melt(id.vars = c("ProjectReference", "document")) %>%
  ungroup() %>%
  group_by(ProjectReference) %>% # collapse
  dplyr::mutate(max_proportion = max(value)) %>% 
  filter(max_proportion == value) %>%
  arrange(document) %>%
  left_join(meta) %>%
  filter(max_proportion > 0.75) %>% # filter parameter is any projects propotion that is 0.75
  dplyr::rename(topic = variable) %>%
  filter(topic == 6) %>% # this is the part filter topic 6 only
  group_by(LeadROName) %>%
  filter(LeadROName != "The Alan Turing Institute") %>%
  arrange(-value) %>%
  summarise(sum = sum(AwardPounds))

lookup(meta, "LeadROName", "Alan") %>%
  pull(ProjectReference) %>%
  viz_id()


