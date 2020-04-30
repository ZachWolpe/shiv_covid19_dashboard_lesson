rm(list=ls())

library(ggplot2)
library(tidyr)
library(dplyr)


setwd("C:/Users/Shiv/Documents/CovidProj/novel-corona-virus-2019-dataset")


df.over <- read.csv('covid_19_data.csv')

df.over$ObservationDate <- as.Date(df.over$ObservationDate, "%m/%d/%Y")

df.over <- subset(df.over, select = -c(SNo, Last.Update, Province.State))

dfj <- subset(df.over, Country.Region == 'US'| Country.Region == 'Italy' | Country.Region == 'Mainland China' | Country.Region == 'Turkey' | Country.Region == 'UK', select = c(ObservationDate, Country.Region, Confirmed, Deaths, Recovered))

df.g <- group_by(dfj,ObservationDate, Country.Region)

gh <- summarise(df.g, Tconf = sum(Confirmed, na.rm = TRUE), Tdeaths = sum(Deaths, na.rm = TRUE), Trec = sum(Recovered, na.rm = TRUE))

View(gh)

test <- filter(gh, Country.Region == 'US')

num.cols <- sapply(df.confirmed, is.numeric)


cor.data <- cor(df, [,num.cols])

ggplot(gh, aes(x = ObservationDate, y = Tconf)) +
  geom_point(aes(color = Country.Region))

ggplot(data = df.confirmeds, aes(x= df.confirmeds$Date, y = df.confirmeds$Confirmed )) + geom_line()
View(test)


# Define UI
ui <- fluidPage(theme = shinytheme("lumen"),
                titlePanel("Corona Dash"),
                sidebarLayout(
                  sidebarPanel(
                    
                    # Select type of trend to plot
                    selectInput(inputId = "Country", label = strong("Country"),
                                choices = unique(gh$),
                                selected = "Travel"),
                    
                    # Select date range to be plotted
                    dateRangeInput("date", strong("Date range"), start = "2007-01-01", end = "2017-07-31",
                                   min = "2007-01-01", max = "2017-07-31"),
                    
                    # Select whether to overlay smooth trend line
                    checkboxInput(inputId = "smoother", label = strong("Overlay smooth trend line"), value = FALSE),
                    
                    # Display only if the smoother is checked
                    conditionalPanel(condition = "input.smoother == true",
                                     sliderInput(inputId = "f", label = "Smoother span:",
                                                 min = 0.01, max = 1, value = 0.67, step = 0.01,
                                                 animate = animationOptions(interval = 100)),
                                     HTML("Higher values give more smoothness.")
                    )
                  ),
                  
                  # Output: Description, lineplot, and reference
                  mainPanel(
                    plotOutput(outputId = "lineplot", height = "300px"),
                    textOutput(outputId = "desc"),
                    tags$a(href = "https://www.google.com/finance/domestic_trends", "Source: Google Domestic Trends", target = "_blank")
                  )
                )
)

# Define server function
server <- function(input, output) {
  
  # Subset data
  selected_trends <- reactive({
    req(input$date)
    validate(need(!is.na(input$date[1]) & !is.na(input$date[2]), "Error: Please provide both a start and an end date."))
    validate(need(input$date[1] < input$date[2], "Error: Start date should be earlier than end date."))
    trend_data %>%
      filter(
        type == input$type,
        date > as.POSIXct(input$date[1]) & date < as.POSIXct(input$date[2]
        ))
  })
  
  
  # Create scatterplot object the plotOutput function is expecting
  output$lineplot <- renderPlot({
    color = "#434343"
    par(mar = c(4, 4, 1, 1))
    plot(x = selected_trends()$date, y = selected_trends()$close, type = "l",
         xlab = "Date", ylab = "Trend index", col = color, fg = color, col.lab = color, col.axis = color)
    # Display only if smoother is checked
    if(input$smoother){
      smooth_curve <- lowess(x = as.numeric(selected_trends()$date), y = selected_trends()$close, f = input$f)
      lines(smooth_curve, col = "#E6553A", lwd = 3)
    }
  })
  
  # Pull in description of trend
  output$desc <- renderText({
    trend_text <- filter(trend_description, type == input$type) %>% pull(text)
    paste(trend_text, "The index is set to 1.0 on January 1, 2004 and is calculated only for US search traffic.")
  })
}

# Create Shiny object
shinyApp(ui = ui, server = server)

View(test)