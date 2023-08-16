getwd()
R.version.string
remotes::install_github("publichealthengland/coronavirus-dashboard-api-R-sdk")
library(ukcovid19)
#install.packages("SEIRfansy")
library("SEIRfansy")

query_filters <- c(
  'areaType=nation',
  'areaName=England'
)
cases_and_deaths = list(
  date = "date",
  areaName = "areaName",
  areaCode = "areaCode",
  newCasesByPublishDate = "newCasesByPublishDate",
  cumCasesBySpecimenDateRate = "cumCasesBySpecimenDateRate",
  newDeaths28DaysByPublishDate = "newDeaths28DaysByPublishDate",
  cumDeaths28DaysByPublishDate = "cumDeaths28DaysByPublishDate"
)
data <- get_data(
  filters = query_filters, 
  structure = cases_and_deaths
)

head(data)


