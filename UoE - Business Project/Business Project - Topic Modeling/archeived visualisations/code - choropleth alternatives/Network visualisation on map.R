load("RData/enviroment.RData")
load("RData/stm_20.RData")
library(tidyverse)
library(sp)
library(leaflet)
library(reshape2)
library(tidytext)

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
  select(ProjectReference.x, ProjectReference.y) -> zipChains

# Which Projects are not? 
pair_2 %>% 
  left_join(select(AIbyTopic, PISurname, PIFirstName, ProjectReference ), by = c("ProjectReference.x" = "ProjectReference")) %>%
  rename(PISurname.x = PISurname, PIFirstName.x = PIFirstName) %>%
  left_join(select(AIbyTopic, PISurname, PIFirstName, ProjectReference ), by = c("ProjectReference.y" = "ProjectReference")) %>%
  rename(PISurname.y = PISurname, PIFirstName.y = PIFirstName) %>%
  filter(PISurname.x != PISurname.y | PIFirstName.x != PIFirstName.y) %>%
  select(ProjectReference.x, ProjectReference.y)
# becase one of the project do not have PIname

AIbyTopic %>% 
  filter(eval(parse(text = arg_f(transfered_id)))) %>%
  dplyr::select(LeadROName) %>%
  group_by(LeadROName) %>%
  summarise(n = length(LeadROName)) %>%
  mutate(x = 1:length(LeadROName)) %>%
  rename(id = x) %>%
  rename(label = LeadROName) -> node

zipChains %>%
  left_join(id, by = c("ProjectReference.x" = "ProjectReference")) %>%
  rename(from = LeadROName) %>%
  left_join(id, by = c("ProjectReference.y" = "ProjectReference")) %>%
  rename(to = LeadROName) %>%
  dplyr::select(from, to, ProjectReference.x, ProjectReference.y)  -> link

link %>%
  rename(from_org = from, to_org = to) %>%
  left_join(node, by = c("from_org" = "label")) %>%
  rename(from = id) %>%
  left_join(node, by = c("to_org" = "label")) %>%
  rename(to = id) -> edges
node -> nodes

library(igraph)
library(visNetwork)

visNetwork(nodes, edges) %>%
  visEdges(shadow = TRUE,
           arrows =list(to = list(enabled = TRUE, scaleFactor = 2)))

