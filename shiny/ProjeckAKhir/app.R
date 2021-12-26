#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(here)
library(vroom)
library(dplyr)
library(ggplot2)
library(plotly)
library(RColorBrewer)
library(plotly)
library(wordcloud)

# Define UI for application that draws a histogram
ui <- fluidPage(
    
    # Application title
    titlePanel("Sentiment Analysis Covid-19 on Twitter"),
    
    # Sidebar with a slider input for number of bins 
    navbarPage(
        "ProjekAkhir",
        tabPanel(
            "Col_Chart",
            sidebarLayout(
                sidebarPanel(
                    selectInput("plotdata", "Pilih Plot Berdasarkan: ", c("Sentiment","Source"))
                ),
                mainPanel(
                    conditionalPanel(
                        condition = "input.plotdata == 'Sentiment'",  
                        plotOutput("geom_col_sent")
                    ),
                    conditionalPanel(
                        condition = "input.plotdata == 'Source'",  
                        plotOutput("geom_col_source")
                    )
                )
            )
        ),
        tabPanel(
            "Pie Chart",
            sidebarLayout(
                sidebarPanel(
                    selectInput("plotdatapie", "Pilih Plot Berdasarkan: ", c("Sentiment","Source"))
                ),
                mainPanel(
                    conditionalPanel(
                        condition = "input.plotdatapie == 'Sentiment'",  
                        plotOutput("pie_chart_sent")
                    ),
                    conditionalPanel(
                        condition = "input.plotdatapie == 'Source'",  
                        plotOutput("pie_chart_source")
                    )
                )
            )
            
        ),
        tabPanel(
            "CloudWord",
            sidebarLayout(
                sidebarPanel(
                    selectInput("cloudword", "Pilih Plot Berdasarkan: ", c("Word","Tags"))
                ),
                mainPanel(
                    conditionalPanel(
                        condition = "input.cloudword == 'Word'",  
                        plotOutput("cloud_words")
                    ),
                    conditionalPanel(
                        condition = "input.cloudword == 'Tags'",  
                        plotOutput("cloud_word_tags")
                    )
                )
            )
            
        ),  
        tabPanel(
            "Sentiment",
            sidebarLayout(
                sidebarPanel(
                    selectInput(inputId = "sentiment","Pilih Sentiment",
                                c("positive","negative", "anger","anticipation",
                                  "disgust","fear","joy","sadness","surprise","trust"),
                                selected = "positive")
                ),
                mainPanel(
                    plotlyOutput(outputId = "aq_plot")
                )
            )
        ),
        tabPanel(
            "Sentiment_Tags",
            sidebarLayout(
                sidebarPanel(
                    selectInput(inputId = "sentiment_tags","Pilih Sentiment",
                                c("positive","negative", "anger","anticipation",
                                  "disgust","fear","joy","sadness","surprise","trust"),
                                selected = "positive")
                ),
                mainPanel(
                    plotlyOutput(outputId = "aq2_plot")
                )
            )
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
    
    output$geom_col_sent <- renderPlot({
        nrc_n%>%
            arrange(desc(n)) %>%
            ggplot(aes(reorder(sentiment,n), y=n, fill = sentiment, label = n)) +
            geom_col(show.legend = FALSE) +
            coord_flip() +
            labs(
                x = "Sentiment",
                y = "count"
            ) +
            geom_text(position = position_dodge(1), hjust = 0, vjust = 0.3)
        
    })
    
    output$geom_col_source <- renderPlot({
        tidy_source %>%
            ggplot(aes(reorder(source,n), y=n, fill = source, label = n)) +
            geom_col(show.legend = FALSE) +
            coord_flip() +
            labs(
                x = "Tags"
            ) + 
            geom_text(position = position_dodge(1), hjust = 0, vjust = 0.3)
    })
    
    output$pie_chart_sent <- renderPlot({
        
        myPalette <- brewer.pal(5, "Set2")
        nrc_n$n %>%
            pie(labels = nrc_n$sentiment, border = "white", col=myPalette, radius = 1)
    })
    
    output$pie_chart_source <- renderPlot({
        
        myPalette <- brewer.pal(5, "Set2")
        tidy_source$n %>%
            pie(labels = tidy_source$source, border = "white", col=myPalette, radius = 1)
    })
    
    output$cloud_words <- renderPlot({
        wordcloud(word_n$word,word_n$n, min.freq = 50, random.order = FALSE, rot.per = 0.25,
                  colors = brewer.pal(8,"Dark2"))
    })
    
    output$cloud_word_tags <- renderPlot({
        wordcloud(cloudtext$tags,cloudtext$n, random.order = FALSE, rot.per = 0.25,
                  colors = brewer.pal(8,"Dark2"))
    })
    
    aq_plot <- reactive({
        
        source_nrc %>%
            ggplot(aes_string(x="source", y=input$sentiment, fill = "source")) +
            geom_segment(aes_string(x="source", xend="source", y=0, yend=input$sentiment)) +
            geom_point(color="blue", size=4, alpha=0.6, show.legend = FALSE) +
            coord_flip() +
            labs(
                x = "Sentiment"
            ) +
            theme_light() +
            theme(panel.grid.major.y = element_blank(),
                  panel.border = element_blank(),
                  axis.ticks.y = element_blank())
    })
    
    output$aq_plot <- renderPlotly({
        ggplotly(aq_plot())
    })
    
    aq2_plot <- reactive({
        
        tags_nrc %>%
            arrange(desc(!!sym(input$sentiment_tags))) %>%
            head(10) %>%
            ggplot(aes(
                x=(reorder(tags, !!sym(input$sentiment_tags))), 
                y=!!sym(input$sentiment_tags), 
                fill = "tags")) +
            geom_segment(aes_string(x="tags", xend="tags", y=0, yend=input$sentiment_tags)) +
            geom_point(color="blue", size=4, alpha=0.6, show.legend = FALSE) +
            coord_flip() +
            labs(
                x = "Sentiment"
            ) +
            theme_light() +
            theme(panel.grid.major.y = element_blank(),
                  panel.border = element_blank(),
                  axis.ticks.y = element_blank())
    })
    
    output$aq2_plot <- renderPlotly({
        ggplotly(aq2_plot())
    })
}

# Run the application 
shinyApp(ui = ui, server = server, options = list(height = "500px"))
