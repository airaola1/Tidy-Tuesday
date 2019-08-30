#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(maps)
library(ggplot2)
library(ggthemes)
library(tidyverse)
library(janitor)
library(readxl)
library(glue)
# Loading our dataset
Enrollment <- read_excel("C:/Users/airaola/Desktop/TidyTuesdays/Week 9/Data Deck 2.0.xlsx")
Census_data <-read_excel("C:/Users/airaola/Desktop/TidyTuesdays/Week 9/Census Data.xlsx")

# Cleaning our data so we can werk with it
Enrollment <- Enrollment %>%
  clean_names() %>%
  filter(x2018_total_jd_enrollment!="NA", x2017_total_jd_enrollment!="NA") %>%
  select(school_state, x2018_total_jd_enrollment , x2017_total_jd_enrollment) %>%
  mutate(x2018_total_jd_enrollment=as.numeric(x2018_total_jd_enrollment)) %>%
  mutate(x2017_total_jd_enrollment=as.numeric(x2017_total_jd_enrollment)) %>%
  group_by(school_state) %>%
  summarise(Total_2018=sum(x2018_total_jd_enrollment, na.omit=TRUE), Total_2017=sum(x2017_total_jd_enrollment, na.omit=TRUE)) %>%
  ungroup()

# Now, let's add a new variable that captures the percent change between 2017 and 2018
Enrollment<- Enrollment %>%
  mutate(Percent_change=(Total_2018-Total_2017)/Total_2018)

us_states <- map_data ("state")
#head(us_states)

p <- ggplot(data = us_states,
            mapping=aes(x=long, y=lat,
                        group=group))

p <- p + geom_polygon(color="black", fill="white", size=0.1) + guides(fill=FALSE) + 
  theme_map()

# leftjoing enrollment to us_states by=c("school_state"="region")
semifinal_dataset <- left_join(us_states, Enrollment, by=c("region" = "school_state")) 

final_dataset <- left_join(semifinal_dataset, Census_data, by=c("region" = "State"))

final_dataset2 <- final_dataset %>%
  mutate(State_Percent_2017 = (Total_2017/est_2017)*100) %>%
  mutate(State_Percent_2018 = (Total_2018/est_2018)*100) %>%
  select(long, lat, group, region, State_Percent_2017, State_Percent_2018, Percent_change) %>%
  gather("year", "number", -long, -lat, -group, -region) %>%
  mutate(year=ifelse(year=="Percent_change", "Percent Change from 2017 to 2018", 
                     ifelse(year=="State_Percent_2018", "by State, as a Proportion of Each State's Population (2018)", 
                            "by State, as a Proportion of Each State's Population (2017)")))
                     



# Define UI for application 
ui <- fluidPage(
   
   # Application title
   titlePanel("Law School Enrollment"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
         radioButtons(inputId = "enrollment",
                     label = "Enrollment",
                     choices = c("Percent Change from 2017 to 2018", 
                                 "2017" = "by State, as a Proportion of Each State's Population (2017)", 
                                 "2018" = "by State, as a Proportion of Each State's Population (2018)"),
                     selected = "Percent Change from 2017 to 2018"
                     )
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
         plotOutput("enrollment_plot")
      )
   )
)

# Define server logic required 
server <- function(input, output) {
   
   output$enrollment_plot <- renderPlot({
      
     
     
     
     
     P2 <- final_dataset2 %>%
       filter(year == input$enrollment) %>%
       ggplot(aes(x=long, y=lat,
                  group=group, fill = number )) +
       geom_polygon(color="gray90", size=0.1) +
       coord_map(projection="albers", lat0=39, lat1=45) + 
       theme_map() + 
       labs(title=glue::glue("J.D. Enrollment {input$enrollment}")) 

     
       P2
     
     
     
     
     
     
     
     
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

