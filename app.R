
library('rsconnect')
library(shiny)
library(readr)
library(ggplot2)
library(gganimate)

# Load the dataset
df <- read_csv("India_Menu.csv")
getwd()
setwd("C:/Users/nagwa/Desktop/Data Visulization/Mcdonald/India_Menu.csv")
ui <- navbarPage(
  title = "McDonald's Menu Comparison Dashboard",
  tabPanel(
    "Dashboard",
    fluidPage(
      tags$div(
        style = "color: white; background-color: red; padding: 10px; text-align: center;",
        tags$img(src = "https://upload.wikimedia.org/wikipedia/commons/4/4b/McDonald%27s_logo.svg", 
                 height = 60, width = 60, style = "vertical-align: middle; margin-right: 10px;"),
        tags$h1(style = "font-size: 20px; display: inline-block; margin: 0;", 
                "Fuel for Thought McDonald's Menu Nutrition Analysis ",
                tags$small("by Janak Nagwani", style = "font-size: 12px;")
        )
      ),
      sidebarLayout(
        sidebarPanel(
          selectInput("main_category", "Menu Category", 
                      choices = c("Select Menu Category", unique(df$`Menu Category`)), selected = "Select Menu Category"),
          selectInput("comparison_variable", "Select Variable for Comparison", 
                      choices = c("Select Variable", colnames(df)[4:12]), selected = "Select Variable"),
          # Add instructions for using the dashboard
          tags$div(style = "font-size: 14px; padding: 10px;", 
                   "Instructions:",
                   tags$ul(
                     tags$li("Select a 'Menu Category' from the dropdown menu."),
                     tags$li("Select a 'Variable for Comparison' from the dropdown menu."),
                     tags$li("The plot on the right will update based on your selections.")
                   )
          )
        ),
        mainPanel(
          plotOutput("menu_plot", height = "750px", width = "1050px")
        )
      )
    )
  ),
  tabPanel(
    "Reference",
    fluidPage(
      tags$div(
        style = "color: white; background-color: red; padding: 10px; text-align: center;",
        tags$img(src = "https://upload.wikimedia.org/wikipedia/commons/4/4b/McDonald%27s_logo.svg", 
                 height = 60, width = 60, style = "vertical-align: middle; margin-right: 10px;"),
        tags$h1(style = "font-size: 20px; display: inline-block; margin: 0;", "Fuel for Thought:"),
        tags$h1(style = "font-size: 20px; display: inline-block; margin: 0;", "McDonald's Menu Nutrition Analysis")
      ),
      h2("References"),
      p("Dataset Source:"),
      p("Deep Contractor. (2022). McDonald's India Menu Nutrition Facts [Dataset]. Kaggle. ", 
        tags$a(href = "https://www.kaggle.com/datasets/deepcontractor/mcdonalds-india-menu-nutrition-facts", "Link")),
      p("McDonald's Logo Source:"),
      p("McDonald's. (1940). McDonald's Logo . Retrieved from ", 
        tags$a(href = "https://upload.wikimedia.org/wikipedia/commons/4/4b/McDonald%27s_logo.svg", "Link")),
      h2("Code References"),
      p("Shiny by RStudio [Computer software]. (2021). Retrieved from ", 
        tags$a(href = "https://shiny.rstudio.com/", "Link")),
      p("navbarPage function (shiny package). (2021). In Shiny Reference. RStudio. ", 
        tags$a(href = "https://shiny.rstudio.com/reference/shiny/latest/navbarPage.html", "Link")),
      p("tabPanel function (shiny package). (2021). In Shiny Reference. RStudio. ", 
        tags$a(href = "https://shiny.rstudio.com/reference/shiny/latest/tabPanel.html", "Link")),
      p("fluidPage function (shiny package). (2021). In Shiny Reference. RStudio. ", 
        tags$a(href = "https://shiny.rstudio.com/reference/shiny/latest/fluidPage.html", "Link")),
      p("tags function (shiny package). (2021). In Shiny Reference. RStudio. ", 
        tags$a(href = "https://shiny.rstudio.com/articles/tag-glossary.html", "Link")),
      p("sidebarLayout function (shiny package). (2021). In Shiny Reference. RStudio. ", 
        tags$a(href = "https://shiny.rstudio.com/reference/shiny/latest/sidebarLayout.html", "Link")),
      p("selectInput function (shiny package). (2021). In Shiny Reference. RStudio. ", 
        tags$a(href = "https://shiny.rstudio.com/reference/shiny/latest/selectInput.html", "Link")),
      p("plotOutput function (shiny package). (2021). In Shiny Reference. RStudio. ", 
        tags$a(href = "https://shiny.rstudio.com/reference/shiny/latest/plotOutput.html", "Link")),
      p("renderPlot function (shiny package). (2021). In Shiny Reference. RStudio. ", 
        tags$a(href = "https://shiny.rstudio.com/reference/shiny/latest/renderPlot.html", "Link")),
      p("reactive function (shiny package). (2021). In Shiny Reference. RStudio. ", 
        tags$a(href = "https://shiny.rstudio.com/reference/shiny/latest/reactive.html", "Link"))
    )
  )
)

server <- function(input, output) {
  filtered_data <- reactive({
    req(input$main_category, input$comparison_variable)
    df[df$`Menu Category` == input$main_category, c("Menu Items", input$comparison_variable)]
  })
  
  output$menu_plot <- renderPlot({
    req(input$comparison_variable)
    if (input$comparison_variable == "Select Variable") {
      plot(1, type = "n", xlim = c(0, 1), ylim = c(0, 1), xlab = "", ylab = "", main = "Select a variable for comparison")
    } else {
      ggplot(filtered_data(), aes(y = reorder(`Menu Items`, !!sym(input$comparison_variable)), x = !!sym(input$comparison_variable))) +
        geom_bar(stat = "identity", aes(fill = !!sym(input$comparison_variable))) +
        scale_fill_gradient(high = "#D9272E", low = "#FFD700") +
        geom_text(aes(label = sprintf("%.2f", !!sym(input$comparison_variable)), hjust = 0, vjust = 0.5, fontface = "bold", family = "Fira Sans"), 
                  position = position_dodge(width = 1), size = 4) +
        labs(y = "Menu Items", x = input$comparison_variable, title = paste("Comparison of", input$comparison_variable, "by Menu Items")) +
        theme_minimal() +
        theme(
          axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 15),
          axis.text.y = element_text(size = 15),
          axis.title = element_text(size = 17),
          panel.grid = element_blank(),
          panel.background = element_rect(fill = "#EFF1F0")
        )
    }
  })
}

shinyApp(ui, server)
