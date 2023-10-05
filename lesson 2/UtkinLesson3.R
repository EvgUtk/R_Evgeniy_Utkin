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