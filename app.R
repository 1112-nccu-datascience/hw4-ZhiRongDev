# 記得把 1111 生物資訊的 repository 設成私密
library(shiny)
library(ggplot2)
library(ggbiplot)
library(corrplot)
library(FactoMineR)
library(factoextra)

ui <- fluidPage(
    titlePanel("PCA & CA analysis By 資碩工一 111753151 程至榮"),
    sidebarLayout(
        sidebarPanel(
            helpText("choose how many input to do PCA:"),
            sliderInput(
                "num_of_points",
                label = "Number of points",
                min = 6,
                max = 150,
                value = 150
            ),
            helpText("choose which component to show on 'PCA'"),
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
            tabPanel(
                "PCA",
                h3("Summary of PCA (log Transformed) "),
                verbatimTextOutput("pca_summary"),
                h3("Biplot of PCA (log Transformed)"),
                plotOutput("pca_biplot"),
            ),
            tabPanel("CA",
                     h3("Biplot of CA"),
                     plotOutput("ca_biplot")
            ),
            tabPanel(
                "Input Data",
                h3("Summery of raw data:"),
                verbatimTextOutput("raw_data_summary"),
                h3("Correlations of raw data:"),
                plotOutput("raw_data_correlation"),
                h3("Summery of log data:"),
                verbatimTextOutput("log_data_summary"),
                h3("Correlations of log data:"),
                plotOutput("log_data_correlation"),
                h3("Data Table"),
                tabsetPanel(
                    tabPanel(
                        'raw',
                        DT::dataTableOutput("raw_table")
                    ),
                    tabPanel(
                        'log',
                        DT::dataTableOutput("log_table")
                    )
                ),
                br()
            ),
            tabPanel(
                "Debug",
                h3("raw"),
                verbatimTextOutput("debug_raw"),
                h3("log"),
                verbatimTextOutput("debug_log"),
            )
        ),)
    )
)

server <- function(input, output) {
    data(iris)
    
    ir.raw <- reactive({
        iris[1:input$num_of_points,]
    })
    
    ir.ca <- reactive({
        CA(ir.raw()[1:4], graph = FALSE)
    })
    
    # log transform
    ir.log <- reactive({
        df <- iris[1:input$num_of_points,]
        df[, 1:4] <- log(iris[1:input$num_of_points, 1:4])
        df
    })
    
    # apply PCA - scale. = TRUE is highly advisable, but default is FALSE.
    ir.pca <- reactive({
        prcomp(ir.log()[, 1:4], center = TRUE, scale. = TRUE)
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
    
    output$pca_biplot <- renderPlot({
        g <-
            ggbiplot(
                ir.pca(),
                choices = c(x_var(), y_var()),
                obs.scale = 1,
                var.scale = 1,
                circle = TRUE,
                ellipse = TRUE,
                groups = ir.raw()[, 5]
            )
        g <- g + scale_color_discrete(name = "")
        g <-
            g + theme(legend.direction = "horizontal",
                      legend.position = "top")
        print(g)
    })
    
    output$pca_summary <- renderPrint({
        ir.pca()
    })
    
    output$ca_biplot <- renderPlot({
        g <- fviz_ca_biplot(ir.ca(), repel = TRUE)
        print(g)
    })
    
    output$raw_data_summary <- renderPrint({
        summary(ir.raw())
    })
    
    output$log_data_summary <- renderPrint({
        summary(ir.log())
    })
    
    output$raw_data_correlation <- renderPlot({
        cr <- cor(ir.raw()[, 1:4])
        corrplot(cr, method = "color")
    })
    
    output$log_data_correlation <- renderPlot({
        cr <- cor(ir.log()[, 1:4])
        corrplot(cr, method = "color")
    })
    
    output$debug_raw <- renderPrint({
        ir.raw()
    })
    
    output$debug_log <- renderPrint({
        ir.log()
    })
    
    output$raw_table <- DT::renderDataTable({
        DT::datatable(ir.raw(), options = list(lengthMenu = c(5, 30, 50), pageLength = 5))
    })
    
    output$log_table <- DT::renderDataTable({
        DT::datatable(ir.log(), options = list(lengthMenu = c(5, 30, 50), pageLength = 5))
    })
    
}

shinyApp(ui, server)
