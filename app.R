library(shiny)
library(tidyverse)
library(dplyr)
library(readr)
library(markdown)

data = read_csv(file = "data/uiuc-students-by-state.csv")
data = as.tibble(data)

example_data = data %>%
  filter(State == "Illinois") %>%
  filter(Year == "1999") %>%
  select("State","Year", "Undergrad")

Pivoted = data %>%
  pivot_longer(cols = c(Undergrad, Grad, Professional), 
               names_to = "Title", values_to = "Count") %>%
  select(-Total)

#Used the function Describe_Count that was mentioned in my R directory, in the
#file named UIUC_By_State.R 
Pivoted_Description = Pivoted %>%
  rowwise() %>%
  mutate(Description = Describe_Count(Count)) %>%
  ungroup()


ui = navbarPage(
  title = "Students By Year",
  tabPanel(
    title = "Input/Visualization",
    titlePanel(title =  "UIUC Students By State"),
    sidebarLayout(
      sidebarPanel(
        radioButtons(
          inputId = "PlotType",
          label = "Plot Type",
          choices = c("Trend over years" = "Trend",
                      "Single year" = "Single"
                      ), 
                      selected = "Trend"
        ),
        textOutput("trend_text"),
        selectInput(inputId = "State",
                    label = "State",
                    choices = unique(data$State), selected = "Illinois"),
        selectInput(inputId = "Year",
                    label = "Year",
                    choices = unique(data$Year), selected = "2017"),
        selectInput(inputId = "Title",
                    label = "Education Level",
                    choices = c("Undergrad","Professional","Grad"), selected = "Undergrad"),
        checkboxInput(inputId = "By_Year", label = "Filter Table to Year", value = FALSE)
      ),
      mainPanel(plotOutput("plot"))
    )
  ),
  
  
  tabPanel(title = "Table", dataTableOutput("table")),
  tabPanel(title = "About", includeMarkdown("About.Rmd"))
)


server = function(input, output) {
  State_State = reactive({
    Pivoted_Description %>%
      filter(State == input$State)
  })  
  
  observeEvent(eventExpr = input$State,
               handlerExpr = {
                 updateSelectInput(inputId = "Year", choices = sort(unique(State_State()$Year)))
               }
              )

  output$plot = renderPlot({
    if (input$PlotType == "Trend") {
      data_2 = Pivoted_Description %>%
        filter(State == input$State) %>%
        filter(Title == input$Title)
      ggplot(data_2, aes(x = Year, y = Count, group = 1)) +
        geom_line() +
        geom_point() +
        labs(title = paste("Students from", input$State, "over the Years for", input$Title),
             x = "Year", y = "Count of Students") +
        theme_minimal()
    } else {
      data_2 = Pivoted_Description %>%
        filter(State == input$State) %>%
        filter(Title == input$Title) %>%
        filter(Year == input$Year) 
      data_2 |>
        ggplot() +
        aes(x = State, y = Count, fill = Count) +
        geom_bar(stat = "identity") +
        labs(title = "Students by State", x = "State", y = "Count") +
        theme_minimal() +
        theme(text = element_text(size = 10),
              axis.text.x = element_text(angle = 90)) +
        scale_y_continuous(breaks = (seq(0,
            max(data_2$Count), (max(data_2$Count) / 20))),
            limits = c(0, max(data_2$Count)))
    }
  })
    
  output$trend_text = renderText({
    if (input$PlotType == "Trend") {
      "Selecting year is irrelevant for this choice, unless looking at table."
    } 
  })
  
  output$table = renderDataTable({
    State_State()
    if(input$By_Year) {
      Pivoted_Description |>
        filter(State == input$State) %>%
        filter(Year == input$Year)
    } else {
      State_State()
    }
  })
}

shinyApp(ui = ui, server = server)






