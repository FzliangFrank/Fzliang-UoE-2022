# Visualise Data on a map 

load("RData/enviroment.RData")
library(tidyverse)

AIbyTopic %>%
  group_by(Title) %>%
  mutate(n = length(ProjectReference)) %>%
  filter(n > 1) %>%
  arrange(Title, ProjectReference) -> dup
dup$x = 1:235
dup %>%
  mutate(x = x -1) %>%
  dplyr::select(x, ProjectReference, Title) -> ruler
dup %>%
  left_join(ruler, by = c("x" = "x")) -> pair
pair %>%
  filter(Left(pair$ProjectReference.x, 10)  == Left(pair$ProjectReference.y, 10) & Title.x == Title.y) %>%
  dplyr::select(Title.x, Title.y, ProjectReference.x, ProjectReference.y) -> pair_2

pair_2 %>% 
  left_join(select(AIbyTopic, PISurname, PIFirstName, ProjectReference ), by = c("ProjectReference.x" = "ProjectReference")) %>%
  rename(PISurname.x = PISurname, PIFirstName.x = PIFirstName) %>%
  left_join(select(AIbyTopic, PISurname, PIFirstName, ProjectReference ), by = c("ProjectReference.y" = "ProjectReference")) %>%
  rename(PISurname.y = PISurname, PIFirstName.y = PIFirstName) %>%
  filter(PISurname.x == PISurname.y & PIFirstName.x == PIFirstName.y) %>%
  select(ProjectReference.x, ProjectReference.y) %>%
  left_join(id, by = c("ProjectReference.x" = "ProjectReference")) %>%
  rename(from = LeadROName) %>%
  left_join(id, by = c("ProjectReference.y" = "ProjectReference")) %>%
  rename(to = LeadROName) %>%
  dplyr::select(from, to, ProjectReference.x, ProjectReference.y) %>%
  left_join(geocode, c("from" = "LeadROName")) %>%
  rename(lng_from = longitude, lat_from = latitude) %>%
  left_join(geocode, c("to" = "LeadROName")) %>%
  rename(lng_to = longitude, lat_to = latitude) -> edge

AIbyTopic %>% 
  filter(eval(parse(text = arg_f(transfered_id)))) %>%
  dplyr::select(LeadROName) %>%
  group_by(LeadROName) %>%
  summarise(n = length(LeadROName)) %>%
  mutate(x = 1:length(LeadROName)) %>%
  rename(id = x) %>%
  left_join(geocode, by = c("LeadROName" = "LeadROName")) -> node_raw




stand <- function(v) {
  (v - min(v) + 1)/(max(v) - min(v) + 1)
}

# compute netflow for each node
edge %>%
  group_by(to) %>%
  mutate(r1 = length(to)) %>%
  ungroup() %>%
  group_by(from) %>%
  mutate(r2 = length(to)) %>%
  ungroup() -> from_to
from_to %>%
  dplyr::select(from, r2) -> from_r2
from_to %>%
  dplyr::select(to, r1) -> to_r1
to_r1 %>% full_join(from_r2, by = c("to" = "from")) %>%
  mutate(r2 = replace(r2, is.na(r2), 0)) %>%
  mutate(r1 = replace(r1, is.na(r1), 0)) %>%
  mutate(n = r1 - r2) %>%
  rename(id = to) %>% 
  group_by(id) %>%
  summarise(n = mean(n))-> net_flow



token = "pk.eyJ1IjoiZmxpYW5ndWsiLCJhIjoiY2t1Z3dkYXJ4MXI3ZDMybXR1czhjMnN3YSJ9.h8KmaN_vC0whFf34x1m84Q"
Sys.setenv("MAPBOX_TOKEN" = token)


node_raw %>%
  left_join(from_r2, by = c("LeadROName" = "from")) %>% #just change id to to from
  rename(r = r2) %>%
  mutate(r = replace(r, is.na(r), 0)) -> node_outflow

node_raw %>%
  left_join(to_r1, by = c("LeadROName" = "to")) %>% #just change id to to from
  rename(r = r1) %>%
  mutate(r = replace(r, is.na(r), 0)) -> node_inflow

node_inflow %>% 
  arrange(-r)
node_outflow %>%
  arrange(-r)

token = "pk.eyJ1IjoiZmxpYW5ndWsiLCJhIjoiY2t1Z3dkYXJ4MXI3ZDMybXR1czhjMnN3YSJ9.h8KmaN_vC0whFf34x1m84Q"

Sys.setenv("MAPBOX_TOKEN" = token)

node_outflow
plot_mapbox() %>%
  add_segments(data = edge, 
               x = ~lng_from, xend = ~lng_to,
               y = ~lat_from, yend = ~lat_to, 
               alpha = 0.5, size = I(1.5), color = I("#5900b3"), hoverinfo = "none", 
               showlegend = FALSE
  ) %>%
  add_markers(data = node_outflow, x = ~longitude, y = ~latitude, 
              color = I("#5900b3"), 
              text = ~LeadROName, alpha = 0.5,
              hoverinfo = "text",size = ~sqrt(r), 
              name = "old place") %>%
  add_markers(data = node_inflow, x = ~longitude, y = ~latitude, 
              color = I("#4ddbff"), 
              text = ~LeadROName, alpha = 0.5,
              hoverinfo = "text",size = ~sqrt(r), 
              name = "new place") %>%
  layout(mapbox = list(
    style = "light",
    zoom = 4.2, 
    center = list(lat=54.3781, lon=-3.4360)
  ))
#4ddbff
#5900b3

#viz_id("EP/T001569/1")

#AIbyTopic %>%
  #lookup("LeadROName", "Alan") %>%
  #pull(ProjectReference) %>% viz_id()


