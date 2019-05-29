library(shiny)
library(shinythemes)
library(tidyverse)
library(gapminder)

data(gapminder)

ui <- fluidPage(
    theme = shinytheme("readable"),
    titlePanel("Life Expectancies"),
    sidebarLayout(
        sidebarPanel(
            selectInput(
                inputId = "country",
                label = h5("countries"),
                choices = levels(gapminder$country),
                selectize = TRUE,
                multiple = TRUE
            ),
            sliderInput(
                inputId = "years",
                label = h5("time period"),
                min = 1952,
                max = 2007,
                value = c(1952, 2007),
                step = 5,
                sep = ""
            )
        ),
        mainPanel(
            tabsetPanel(
                id = "tabspanel",
                type = "tabs",
                tabPanel(
                    title = h5("documentation"),
                    h5("About this Shiny app"),
                    p(
                        "This is a Shiny app that can be used for comparing",
                        strong("life expectancies"),
                        "between countries."
                    ),
                    p(
                        "The app takes as input one or more selected countries",
                        "plus a given time period. From this, a plot is",
                        "rendered as well as a table, comparing average life",
                        "expectancies."
                    ),
                    p(
                        "The resulting",
                        strong("plot"),
                        "and",
                        strong("table"),
                        "are shown under the tabs."
                    ),
                    hr(),
                    h5("Instructions"),
                    p("On the left panel, select"),
                    p("* one or more countries;"),
                    p("*",
                      em("(optionally)"),
                      "change the time period."),
                    hr(),
                    h5("About the data"),
                    p(
                        "The data is taken from the",
                        strong("gapminder"),
                        "dataset (which can be found in the corresponding R",
                        "package). It contains an excerpt of the",
                        strong("Gapminder data"),
                        "on life expectancy, GDP per capita,and population of",
                        "142 countries."
                    ),
                    p(
                        "The",
                        strong("Gapminder data"),
                        "can be found at",
                        a("this website",
                          href = "https://www.gapminder.org/data/")
                    ),
                    hr(),
                    h5("Github repo"),
                    p(
                        "The source code for this app resides in",
                        a("this repository",
                          href = "https://github.com/DennisDondergoor/jhu-ddp-shiny-and-pitch")
                    )
                ),
                tabPanel(
                    title = h5("plot"),
                    br(),
                    plotOutput(outputId = "scatterplot",
                               height = 500)
                ),
                tabPanel(title = h5("table"),
                         br(),
                         tableOutput(outputId = "averages"))
            )
        )
    )
)

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

shinyApp(ui = ui, server = server)
