library(shiny)
library(tm)
library(wordcloud2)
library(shiny)
library(ggfortify)
library(ggplot2)
library(shinyjqui)
library(highcharter)
library(plotly)
library(DT)
library(xts)
library(dplyr)
library(genius)
library(parallel)


ui <- shinyUI(fluidPage(
    titlePanel("Dre's App"),
    sidebarLayout(
        sidebarPanel(
            textInput("artist", "Enter Artist Name", value = "Masego"),
            textInput("song", "Enter Song Name", value = "Tadow"),
            fileInput("image", "choose an image..."),
            sliderInput(inputId = "sizeSlider", label = "Word Size", min = 0, max = 2, value = .5, step = .05,
                        round = FALSE, format = NULL, locale = NULL, ticks = TRUE,
                        animate = FALSE, width = NULL, sep = ",", pre = NULL,
                        post = NULL, timeFormat = NULL, timezone = NULL,
                        dragRange = TRUE),
            sliderInput(inputId = "minSlider", label = "Min word Size", min = 0, max = 10, value = .1, step = .05,
                        round = FALSE, format = NULL, locale = NULL, ticks = TRUE,
                        animate = FALSE, width = NULL, sep = ",", pre = NULL,
                        post = NULL, timeFormat = NULL, timezone = NULL,
                        dragRange = TRUE),
            sliderInput(inputId = "gridSize", label = "Grid Size", min = 0, max = 10, value = .1, step = .05,
                        round = FALSE, format = NULL, locale = NULL, ticks = TRUE,
                        animate = FALSE, width = NULL, sep = ",", pre = NULL,
                        post = NULL, timeFormat = NULL, timezone = NULL,
                        dragRange = TRUE),
            sliderInput(inputId = "rotateRatioSlider", label = "Rotate Words", min = 0, max = 1, value = 0, step = .05,
                        round = FALSE, format = NULL, locale = NULL, ticks = TRUE,
                        animate = FALSE, width = NULL, sep = ",", pre = NULL,
                        post = NULL, timeFormat = NULL, timezone = NULL,
                        dragRange = TRUE),
            sliderInput(inputId = "minRotationSlider", label = "Min Rotation", min = -1, max = 1, value = 0, step = .05,
                        round = FALSE, format = NULL, locale = NULL, ticks = TRUE,
                        animate = FALSE, width = NULL, sep = ",", pre = NULL,
                        post = NULL, timeFormat = NULL, timezone = NULL,
                        dragRange = TRUE),
            sliderInput(inputId = "maxRotationSlider", label = "Max Rotation", min = -1, max = 1, value = 0, step = .05,
                        round = FALSE, format = NULL, locale = NULL, ticks = TRUE,
                        animate = FALSE, width = NULL, sep = ",", pre = NULL,
                        post = NULL, timeFormat = NULL, timezone = NULL,
                        dragRange = TRUE),
            sliderInput(inputId = "ellipticitySlider", label = "ellipticity", min = 0, max = 3, value = 1, step = .05,
                        round = FALSE, format = NULL, locale = NULL, ticks = TRUE,
                        animate = FALSE, width = NULL, sep = ",", pre = NULL,
                        post = NULL, timeFormat = NULL, timezone = NULL,
                        dragRange = TRUE),
            actionButton("submit", "Submit")),
        mainPanel("Dre's Webapp")
    ),
    mainPanel(
        wordcloud2Output("wcplot")
    )
)
)
