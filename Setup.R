library(tidyverse)
library(ggplot2)
library(dplyr)
data = read.csv("data/uiuc-students-by-state.csv")
data = as.tibble(data)
View(data)


  
#looks goofy, adjust later
data |>
filter(State == input$State)
filter(Year == input$Year)
ggplot() +
aes(x = State, y = Total, fill = Total) +
geom_bar(stat = "identity") +
labs(title = "Student Total State", x = "State", y = "Total") +
theme_minimal() +
theme(text = element_text(size = 10),
      axis.text.x = element_text(angle = 90)) +
scale_y_continuous(breaks=(seq(0, 32000, 1000)), limits = c(0, 32000))


example_data = data %>%
  filter(State == "Illinois") %>%
  filter(Year == "1999") %>%
  select("State","Year", "Undergrad")




Pivoted = data %>%
  pivot_longer(cols = c(Undergrad, Grad, Professional), 
               names_to = "Title", values_to = "Count") %>%
  select(-Total)



