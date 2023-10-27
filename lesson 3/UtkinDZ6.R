setwd("C:/R")
df <- read.csv("ДЗ3_superstore_data.csv")
cat("\nПервыЕ строки:")
head(df)
library(dplyr)
cat("\nIncome больше 30 000:")
df <- filter(df, Income > 30000)
print(df)

cat("\nТолько следующие столбцы: Id, Year_Birth, Education, Marital_Status, Income, Response:")
df <- select(df, Id, Year_Birth, Education, Marital_Status, Income, Response)
print(df)

cat("\nСоздайте новые столбцы: Age (возраст на момент 2023 года) и Rich_flag (который принимает True, если Income больше 80 000):")
df <- mutate(df, Age = 2023 - Year_Birth, Rich_flag = Income > 80000)
print(df)

cat("\nВ отдельный датасет запишите средние значения по Income по каждому типу Education — используйте group_by и summarize.")
summary_data <- df %>%
  group_by(Education) %>%
  summarize(Average_Income = mean(Income))
print(summary_data)

cat("\nПрисоедините созданный датасет к основному по полю Education — используйте join.")
df <- df %>%
  left_join(summary_data, by = "Education")
print(df)


#1
library(plotly)
scatter_plot <- plot_ly(df, x = ~Age, y = ~Income, mode = "markers", type = "scatter", marker = list(size = 8))
scatter_plot

#2
heatmap_plot <- plot_ly(df, x = ~Education, y = ~Marital_Status, z = ~Average_Income, type = "heatmap")
heatmap_plot

#3
data <- data.frame(
  label = c("Сотрудник 1", "Реклама 1", "Реклама 2", "Сотрудник 2", "Реклама 3"),
  parent = c("", "Сотрудник 1", "Сотрудник 1", "", "Сотрудник 2"),
  value = c(1, 1, 1, 1, 1)
)
tree_map <- plot_ly(
  data,
  ids = ~label,
  labels = ~label,
  parents = ~parent,
  type = "treemap"
)
tree_map


#4
library(leaflet)
latitude <- 55.8036
longitude <- 38.9600

map <- leaflet() %>%
  setView(lng = longitude, lat = latitude, zoom = 8) %>%
  addTiles() %>%
  addMarkers(lng = longitude, lat = latitude, popup = "Орехово-Зуево, Московская область, Россия")

map

#5
library(DT)
datatable(df)

#6
library(rpivotTable)
rpivotTable(df, rows = "Education", cols = "Marital_Status")

