library(shiny)
library(shinydashboard)
library(ggplot2)
library(dplyr)
library(tidyr)

data <- read.csv("C:/R/ДЗ2_vgsales.csv")

data$Year <- as.numeric(data$Year)
data <- na.omit(data)

# Создание Shiny
ui <- fluidPage(
  titlePanel("Аналитическое веб-приложение на Shiny"),
  # KPI со средними значениями
  fluidRow(
    box(width = 3, title = "Среднее NA_Sales", valueBoxOutput("kpi_na_sales")),
    box(width = 3, title = "Среднее EU_Sales", valueBoxOutput("kpi_eu_sales")),
    box(width = 3, title = "Среднее JP_Sales", valueBoxOutput("kpi_jp_sales")),
    box(width = 3, title = "Среднее Other_Sales", valueBoxOutput("kpi_other_sales"))
  ),
  
  # Линейная диаграмма
  fluidRow(
    plotOutput("line_plot")
  ),
  
  # Столбчатая диаграмма
  fluidRow(
    plotOutput("bar_plot")
  ),
  
  # Таблица
  fluidRow(
    dataTableOutput("data_table")
  ),
  
  # Фильтры
  sidebarPanel(
    selectInput("platform_filter", "Фильтр по платформе", choices = unique(data$Platform)),
    sliderInput("year_filter", "Фильтр по году", min(data$Year), max(data$Year), value = c(min(data$Year), max(data$Year)))
  )
)

# Сервер
server <- function(input, output) {
  # Вычисление KPI
  output$kpi_na_sales <- renderValueBox({
    valueBox(mean(data$NA_Sales), subtitle = "Среднее NA_Sales")
  })
  output$kpi_eu_sales <- renderValueBox({
    valueBox(mean(data$EU_Sales), subtitle = "Среднее EU_Sales")
  })
  output$kpi_jp_sales <- renderValueBox({
    valueBox(mean(data$JP_Sales), subtitle = "Среднее JP_Sales")
  })
  output$kpi_other_sales <- renderValueBox({
    valueBox(mean(data$Other_Sales), subtitle = "Среднее Other_Sales")
  })
  
  # Линейная диаграмма
  output$line_plot <- renderPlot({
    filtered_data <- data %>%
      filter(Platform %in% input$platform_filter,
             Year >= input$year_filter[1] & Year <= input$year_filter[2])
    
    ggplot(filtered_data, aes(x = Year)) +
      geom_line(aes(y = NA_Sales, color = "NA_Sales"), size = 1) +
      geom_line(aes(y = EU_Sales, color = "EU_Sales"), size = 1) +
      geom_line(aes(y = JP_Sales, color = "JP_Sales"), size = 1) +
      geom_line(aes(y = Other_Sales, color = "Other_Sales"), size = 1) +
      labs(title = "Динамика продаж по регионам", x = "Год", y = "Продажи") +
      scale_color_manual(values = c("NA_Sales" = "red", "EU_Sales" = "green", "JP_Sales" = "blue", "Other_Sales" = "yellow"))
  })
  
  # Столбчатая диаграмма
  output$bar_plot <- renderPlot({
    filtered_data <- data %>%
      filter(Platform %in% input$platform_filter,
             Year >= input$year_filter[1] & Year <= input$year_filter[2])
    
    genre_sales <- filtered_data %>%
      group_by(Genre) %>%
      summarize(NA_Sales = sum(NA_Sales), EU_Sales = sum(EU_Sales), JP_Sales = sum(JP_Sales), Other_Sales = sum(Other_Sales))
    
    genre_sales <- pivot_longer(genre_sales, cols = -Genre, names_to = "Region", values_to = "Sales")
    
    ggplot(genre_sales, aes(x = Genre, y = Sales, fill = Region)) +
      geom_bar(stat = "identity", position = "dodge") +
      labs(title = "Рейтинг жанров по продажам", x = "Жанр", y = "Продажи") +
      scale_fill_manual(values = c("NA_Sales" = "red", "EU_Sales" = "green", "JP_Sales" = "blue", "Other_Sales" = "yellow"))
  })
  
  # Таблица
  output$data_table <- renderDataTable({
    filtered_data <- data %>%
      filter(Platform %in% input$platform_filter,
             Year >= input$year_filter[1] & Year <= input$year_filter[2])
    filtered_data
  })
}

# Запуск Shiny-приложения
shinyApp(ui, server)
