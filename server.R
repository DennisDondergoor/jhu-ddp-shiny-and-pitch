library(shiny)
library(shinythemes)
library(tidyverse)
library(gapminder)
data(gapminder)

server <- function(input, output) {
    gapminder_subset <- reactive({
        req(input$country)
        gapminder %>%
            filter(country %in% input$country) %>%
            filter(year >= input$years[1] &
                       year <= input$years[2])
    })
    output$scatterplot <- renderPlot({
        ggplot(gapminder_subset(),
               aes(
                   x = year,
                   y = lifeExp,
                   color = country
               )) +
            geom_line(size = 1) +
            geom_point(size = 3, color = "black") +
            labs(x = "year", y = "life expectancy") +
            theme(
                text = element_text(face = "bold", size = 18),
                axis.text = element_text(face = "bold", size = 18),
                legend.text = element_text(face = "bold", size = 18),
                legend.title = element_blank(),
                legend.justification = c("right", "top")
            )
    })
    output$averages <- renderTable({
        gapminder_subset() %>%
            group_by(country) %>%
            summarise("average life expectancy" =
                          mean(lifeExp))
    },
    align = "l")
}
