#Lauren Ode-Giles
#Week 5, ER Case Study I
#ESCI 599 - Shiny Apps

#Nothing fancy this week - I've been teaching a class at Whatcom Community College for the past few weeks. 
# Happy to have just wrapped up the last class tonight! Looking forward to having more time and mental space
# for making more interesting, fancier Shiny apps going forward, but I have no complaints about having an easier
# time of things this week. Phew! 

library(shiny)
library(vroom)
library(tidyverse)


#Call data for injuries, product types, and population (for weights vs US population calculations)

injuries <- vroom::vroom("injuries.tsv.gz")
products <- vroom::vroom("products.tsv")
population <- vroom::vroom("population.tsv")

#Hi!

#------------
#Set up elements that will be used inside the Shiny app UI and server

prod_codes <- setNames(products$prod_code, products$title) #make variable to hold product codes/names

#introduce new function to truncate number of entries shown; 
#grouping by `var`

count_top <- function(df, var, n = 5) {
  df %>%
    mutate({{ var }} := fct_lump(fct_infreq({{ var }}), n = n)) %>%
    group_by({{ var }}) %>%
    summarise(n = as.integer(sum(weight)))
}

#-------------
#set up Shiny app UI

ui <- fluidPage( #fluidPage holds all subsequent column and row mapping
  fluidRow(
    column(8, 
           selectInput("code", "Product",
                       choices = setNames(products$prod_code, products$title), #allow for selection of product by name
                       width = "100%"
           )
    ),
    column(2, selectInput("y", "Y axis", c("rate", "count")))
    # allows for selection of population display on the plot's y-axis by either rate (per 10k) or overall injury count
  ),
  
  fluidRow(
    column(4, tableOutput("diag")), #show values for diagnosis (injury type) across 1/3 (4 of 12) of the page width
    column(4, tableOutput("body_part")), #show values for injured body part across 1/3 (4 of 12) of the page width
    column(4, tableOutput("location")) #show values for where injury occurred across 1/3 (4 of 12) of the page width
  ),
  fluidRow(
    column(12, plotOutput("age_sex")), #plot the age and sex of injured parties across the entire width of the page
    
  fluidRow(
    column(2, actionButton("story", "Tell me a story")), #show labeled action button over 1/6 of the page width
    column(10, textOutput("narrative")) #show injury narrative across 5/6 (10 of 12) of the page width
  )
    
    
  )
)

#-------------
#set up Shiny app server

server <- function(input, output, session) {
  #reactive function; filter data by product code based on user input (user-selected value)
  selected <- reactive(injuries %>% filter(prod_code == input$code))
  
# create outputs based on user-selected product type; show tables based on 
  output$diag <- renderTable(count_top(selected(), diag), width = "100%") #force full width display
  output$body_part <- renderTable(count_top(selected(), body_part), width = "100%") #force full width display
  output$location <- renderTable(count_top(selected(), location), width = "100%") #force full width display
  
  #creates reactive function; summarizes count of age, sex, weight
  # calculates rate in population by weight/category and count (I think?)
  summary <- reactive({
    selected() %>%
      count(age, sex, wt = weight) %>%
      left_join(population, by = c("age", "sex")) %>%
      mutate(rate = n / population * 1e4)
  })
  
  #produce output to pass into UI for plotting
  output$age_sex <- renderPlot({
    if (input$y == "count") {
      summary() %>%
        ggplot(aes(age, n, colour = sex)) +
        geom_line() +
        labs(y = "Estimated number of injuries")
    } else {
      summary() %>%
        ggplot(aes(age, rate, colour = sex)) +
        geom_line(na.rm = TRUE) +
        labs(y = "Injuries per 10,000 people") +
        theme_bw() #updated ggplot theme
    }
  }, res = 96)
  
  #generate story
  narrative_sample <- eventReactive(
    list(input$story, selected()),
    selected() %>% pull(narrative) %>% sample(1)
  )
  #produce output to pass into UI for narrative
  output$narrative <- renderText(narrative_sample())
}

#-------------
#run Shiny app

shinyApp(ui, server)
