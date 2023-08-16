load("RData/enviroment.RData")
load("RData/stm_20.RData")

library(tidyverse)
library(stm)

overview_by_price.g
overview_by_award.g

library(ggplot2)
theme_set(theme(
  panel.grid.major = element_blank(),
  panel.grid.minor = element_blank(),
  panel.border = element_blank(),
  panel.background = element_rect(fill = "#f7f7f7"),
  plot.background = element_rect(fill = "white")
))
transfered_id %>% length()

arg_f -> arf

# Surnames
AIbyTopic %>%
  filter(eval(parse(text = arf(transfered_id)))) %>%
  group_by(PISurname) %>%
  summarise(PISurname)

# Make Link Table
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
  select(ProjectReference.x, ProjectReference.y) -> transfer_in_pair
save(transfer_in_pair, file = "RData/Transfer_link_table.RData")

# Which Projects are not? 
pair_2 %>% 
  left_join(select(AIbyTopic, PISurname, PIFirstName, ProjectReference ), by = c("ProjectReference.x" = "ProjectReference")) %>%
  rename(PISurname.x = PISurname, PIFirstName.x = PIFirstName) %>%
  left_join(select(AIbyTopic, PISurname, PIFirstName, ProjectReference ), by = c("ProjectReference.y" = "ProjectReference")) %>%
  rename(PISurname.y = PISurname, PIFirstName.y = PIFirstName) %>%
  filter(PISurname.x != PISurname.y | PIFirstName.x != PIFirstName.y) %>%
  select(ProjectReference.x, ProjectReference.y)


# linktable 
AIbyTopic %>% 
  filter(eval(parse(text = arg_f(transfered_id)))) %>%
  dplyr::select(LeadROName) %>%
  group_by(LeadROName) %>%
  summarise(n = length(LeadROName)) -> dict
transfer_in_pair %>%
  left_join(id, by = c("ProjectReference.x" = "ProjectReference")) %>%
  rename(from = LeadROName) %>%
  left_join(id, by = c("ProjectReference.y" = "ProjectReference")) %>%
  rename(to = LeadROName) %>%
  dplyr::select(from, to, ProjectReference.x, ProjectReference.y)  -> link

# "EP/N028112/1", "EP/N028112/2"
pair_2 %>%
  filter(!eval(parse(text = arg_f(transfer_in_pair$ProjectReference.x, "ProjectReference.x"))))
# EP/J004049/2     EP/J004049/3  

viz_id(c("EP/J004049/2" ,"EP/J004049/3"  ))


# make a datatable (prerequisit required)
estimateEffect(
  formula = 1:20 ~ price, 
  stmobj = stm_20, 
  metadata = meta_2, 
  uncertainty = "Global") %>%
  tidy() %>%
  filter(p.value < 0.01) %>%
  filter(term != "(Intercept)") %>%
  arrange(-estimate) -> df1
estimateEffect(
  formula = 1:20 ~ AwardPounds, 
  stmobj = stm_20, 
  metadata = meta_2, 
  uncertainty = "Global") %>%
  tidy() %>%
  filter(p.value < 0.01) %>%
  filter(term != "(Intercept)") %>%
  arrange(-estimate) -> df2
estimateEffect(
  formula = 1:20 ~ sqrt(price), 
  stmobj = stm_20, 
  metadata = meta_2, 
  uncertainty = "Global") %>%
  tidy() %>%
  filter(p.value < 0.01) %>%
  filter(term != "(Intercept)") %>%
  arrange(-estimate) -> df3
estimateEffect(
  formula = 1:20 ~ duration, 
  stmobj = stm_20, 
  metadata = meta_2, 
  uncertainty = "Global") %>%
  tidy() %>%
  filter(p.value < 0.01) %>%
  filter(term != "(Intercept)") %>%
  arrange(-estimate) -> df4
estimateEffect(
  formula = 1:20 ~ sqrt(as.numeric(AwardPounds)), 
  stmobj = stm_20, 
  metadata = meta_2, 
  uncertainty = "Global") %>%
  tidy() %>%
  filter(p.value < 0.01) %>%
  filter(term != "(Intercept)") %>%
  arrange(-estimate) 
rbind(df1,df2,df3, df4) %>% View()

# 
AIbyTopic$AwardPounds %>% median()


meta %>%
  mutate(StartDate = as.Date(StartDate, format = "%d/%m/%Y")) %>%
  mutate(EndDate = as.Date(EndDate, format = "%d/%m/%Y")) %>%
  mutate(date = StartDate - min(StartDate)) %>%
  mutate(date = as.numeric(date)) %>%
  mutate(duration = EndDate - StartDate) %>%
  mutate(price = AwardPounds/as.numeric(duration)) -> meta
# find top topic for transferred project
library(reshape2)
select(meta, ProjectReference) %>%
  mutate(x = 1:length(ProjectReference)) -> meta_3
stm_20 %>% tidy(matrix = "theta") %>%
  group_by(document) %>%
  dcast(document ~ topic, value.var = "gamma") %>%
  left_join(meta_3, by = c("document" = "x")) %>%
  melt(id.vars = c("ProjectReference", "document")) %>%
  group_by(ProjectReference) %>%
  mutate(top_topic = max(value)) %>% 
  filter(top_topic == value) %>%
  arrange(document) -> topicTable

# you cannot link transfered id to table because those are elimated for you do topic modeling
topicTable %>%
  left_join(meta) %>%
  group_by(variable) %>%
  summarise(n = length(top_topic)) %>%
  mutate(topic = as.factor(variable)) %>%
  ggplot(aes(x = reorder(topic, -n), y = n)) + geom_col() 

topicTable %>%
  group_by(variable) %>%
  summarise(sum = sum(value)) %>%
  arrange(-sum)

topicTable %>%
  left_join(meta) %>%
  filter(top_topic > 0.75) %>%
  rename(topic = variable) -> proxy_df

lm(data = proxy_df, AwardPounds ~ topic) %>% summary() %>%
  tidy() %>%
  filter(p.value < 0.05)

proxy_df %>%
  lookup("LeadROName", "Southampton") %>%
  group_by(topic) %>%
  summarise(number_of_topic = length(topic))

lookup(deep_cleaned, "LeadROName", "Southampton") %>%
  pull(LeadROName) %>%
  length()

topicTable %>%
  left_join(meta) %>%
  lookup("LeadROName", "Southampton") %>%
  group_by(variable) %>%
  filter(variable == 6) %>%
  select(ProjectReference, variable, top_topic, AwardPounds)
viz_id(c("EP/V00784X/1", "EP/R029563/1", "ES/V011278/1"))

# filter out topic 6 
proxy_df %>%
  filter(topic != 6) -> data
lm(data = data, AwardPounds ~ topic) %>% summary() %>%
  tidy() %>%
  filter(p.value < 0.05)

proxy_df %>%
  group_by(topic) %>%
  mutate(max = max(top_topic)) %>%
  filter(top_topic == max) %>%
  ggplot(aes(x = StartDate, y = AwardPounds, color = topic)) + geom_point()

topicTable %>%
  left_join(meta) %>%
  group_by(variable) %>%
  filter(variable == 6) %>%
  left_join(geocode, by = c("LeadROName" = "LeadROName")) %>%
  plot_mapbox() %>%
  add_markers(y = ~latitude, x = ~longitude)


# let's plot without topic 6 



