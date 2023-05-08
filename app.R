# 記得把 1111 生物資訊的 repository 設成私密
library(shiny)
library(ggplot2)
library(ggbiplot)
library(corrplot)
library(FactoMineR)
library(factoextra)

ui <- fluidPage(
    titlePanel("Interactive web service of PCA and CA analysis by Shinyapp"),
    sidebarLayout(
        sidebarPanel(
            helpText("choose how many input to do PCA:"),
            sliderInput(
                "num_of_points",
                label = "Number of points",
                min = 6,
                max = 150,
                value = 100
            ),
            helpText("choose what you want to see on 'PCA Result : Plot'"),
            selectInput(
                "x_var",
                label = "X Variable",
                choices = c("PC1",
                            "PC2",
                            "PC3",
                            "PC4"),
                selected = "PC1"
            ),
            selectInput(
                "y_var",
                label = "Y Variable",
                choices = c("PC1",
                            "PC2",
                            "PC3",
                            "PC4"),
                selected = "PC2"
            ),
        ),
        mainPanel(tabsetPanel(
            tabPanel("PCA",
                     plotOutput("pca_plot")),
            tabPanel("CA",
                     plotOutput("ca_plot")),
            tabPanel(
                "Raw Data",
                h3("Summery of raw data:"),
                verbatimTextOutput("raw_data_summary"),
                h3("Correlations"),
                plotOutput("raw_data_correlation")
            ),
            tabPanel("Debug",
                     verbatimTextOutput("debug"),)
        ), )
    )
)

server <- function(input, output) {
    data(iris)
    
    ir.raw <- reactive({
        iris[1:input$num_of_points, ]
    })
    
    ir.raw_class <- reactive({
        iris[1:input$num_of_points, 1:4]
    })
    
    # log transform
    ir.log_class <- reactive({
        log(iris[1:input$num_of_points, 1:4])
    })
    
    ir.species <- reactive({
        iris[1:input$num_of_points, 5]
    })
    
    ir.ca <- reactive({
        CA(ir.raw_class(), graph = FALSE)    
    })
    
    cr <- reactive({
        cor(ir.raw_class()[1:4])
    })
    
    x_var <- reactive({
        switch(
            input$x_var,
            "PC1" = 1,
            "PC2" = 2,
            "PC3" = 3,
            "PC4" = 4,
        )
    })
    
    y_var <- reactive({
        switch(
            input$y_var,
            "PC1" = 1,
            "PC2" = 2,
            "PC3" = 3,
            "PC4" = 4,
        )
    })
    
    output$pca_plot <- renderPlot({
        # apply PCA - scale. = TRUE is highly advisable, but default is FALSE.
        ir.pca <- prcomp(ir.log_class(), center = TRUE, scale. = TRUE)
        g <-
            ggbiplot(
                ir.pca,
                choices = c(x_var(), y_var()),
                obs.scale = 1,
                var.scale = 1,
                circle = TRUE,
                groups = ir.species()
            )
        g <- g + scale_color_discrete(name = "")
        g <-
            g + theme(legend.direction = "horizontal",
                      legend.position = "top")
        print(g)
    })
    output$ca_plot <- renderPlot({
        g <- fviz_ca_biplot(ir.ca(), repel = TRUE)
        print(g)
    })
    
    output$raw_data_summary <- renderPrint({
        summary(ir.raw())
    })
    output$raw_data_correlation <- renderPlot({
        corrplot(cr(), method = "color")
    })
    
    output$debug <- renderPrint({
        ir.raw_class()
    })
}

shinyApp(ui, server)
