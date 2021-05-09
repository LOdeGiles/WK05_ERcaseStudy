#Lauren Ode-Giles
#Week 5, ER Case Study I
#ESCI 599 - Shiny Apps

#Nothing fancy this week - I've been teaching a class at Whatcom Community College for the past few weeks. 
# Happy to have just wrapped up the last class tonight! Looking forward to having more time and mental space
# for making more interesting, fancier Shiny apps going forward, but I have no complaints about having an easier
# time of things this week. Phew! 

library(shiny)
library(dplyr)
library(ggplot2)
library(forcats)
library(vroom)
library(tidyverse)


#Call data for injuries, product types, and population (for weights vs US population calculations)

injuries <- vroom::vroom("injuries.tsv.gz")
products <- vroom::vroom("products.tsv")
population <- vroom::vroom("population.tsv")

#------------
#Set up elements that will be used inside the Shiny app UI and server

prod_codes <- setNames(products$prod_code, products$title) #make variable to hold product codes/names

#introduce new function to truncate number of entries shown; 
#grouping by `var`

#to allow for user selection of # rows to show, update function from n = 5 to n = (variable)
# make sure that there are functions in the ui and server that allow the user to specify the number of rows to use

count_top <- function(df, var, n) { #changed from n = 5 to n as variable
  df %>%
    mutate({{ var }} := fct_lump(fct_infreq({{ var }}), {{ n }})) %>% #set n as variable with dbl brackets
    group_by({{ var }}) %>%
    summarise(n = as.integer(sum(weight)))
}

#-------------
#set up Shiny app UI
# all elements of the UI are being rendered in order

ui <- fluidPage( #fluidPage holds all subsequent column and row mapping
  h1("ER Injuries Summary Tool"),
  p("This app is designed to llow users to explore injuries reported at emergency rooms based on the cause of the injury, the age and sex of the injured party, the placement of the injury on the body, and the location where the injury occurred. 
    Data source: National Electronic Injury Surveillance System (NEISS), 2017."),
  hr(), #horizontal rule (line)
  p("Select a Product Code (product associated with causing an injury):"),
  fluidRow(
    column(8, 
           selectInput("code", "Product",
                       choices = setNames(products$prod_code, 
                                          products$title), #allow for selection of product by name
                       width = "100%")
    ),
    column(2, 
           selectInput("y", "Y axis", c("rate", "count")))),
    # allows for selection of population display on the plot's y-axis by either rate (per 10k) or overall injury count
  hr(),
  h1("Data Summary"),
  p("These columns display summary information on where an incident occurred (ex: at home), what body part was injured,
    and what the diagnosis of the injury was. Use the slider below to select how many values are shown in each column"),
  fluidRow(
    #allow user to select input for number of rows (variable n)
    column(4, sliderInput(inputId = "nRows",  #match this up to a server-side function
                          label = "Number of unique rows",
                          min = 2, #minimum slider value accepted
                          max = 20, #maximum slider value accepted
                          value = 6, #start value (default)
                          step = 1, #interval by which to allow slider to climb
                          ))),
  fluidRow(
    column(4, tableOutput("diag")), #show values for diagnosis (injury type) across 1/3 (4 of 12) of the page width
    column(4, tableOutput("body_part")), #show values for injured body part across 1/3 (4 of 12) of the page width
    column(4, tableOutput("location"))), #show values for where injury occurred across 1/3 (4 of 12) of the page width
  
  fluidRow(
    column(12, plotOutput("age_sex"))), #plot the age and sex of injured parties across the entire width of the page
    
  fluidRow(
    column(2, actionButton("story", "Random injury narrative")), #show labeled action button over 1/6 of the page width
    column(10, textOutput("narrative"))),
  fluidRow(
    column(2, actionButton("prevStory", "Previous story")),
    column(5, textOutput("narrative2")),
    column(2, actionButton("nextStory", "Next story")))) 
#show injury narrative across 5/6 (10 of 12) of the page width

#-------------
#set up Shiny app server

server <- function(input, output, session) {
  #reactive function; filter data by product code based on user input (user-selected value)
  selected <- reactive(injuries %>% filter(prod_code == input$code))
  
# create outputs based on user-selected product type; show tables based on 
  #building this inside of a function, sort of?
  output$diag <- renderTable({
    diagTable <- count_top(df = selected(), 
                           var = diag, 
                           n = input$nRows - 1)
    colnames(diagTable) <- c("Diagnosis", "Count")
    diagTable}, 
    width = "100%") #force full width display
  
  output$body_part <- renderTable({
    bodyTable <- count_top(selected(), 
                           var = body_part,
                           n = input$nRows - 1)
    bodyTable
    colnames(bodyTable) <- c("Injured body part", "Count")
    bodyTable}, 
    width = "100%") #force full width display
  
  output$location <- renderTable({
    locationTable <- count_top(selected(), 
                             var = location,
                             n = input$nRows - 1)
    locationTable},
    width = "100%") #force full width display
  
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
        labs(y = "Estimated number of injuries") +
        theme_bw() #updated ggplot theme
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
  
  narrative_list <- reactive(selected() %>% pull(narrative))

  #display random story
  #produce output to pass into UI for narrative
  output$narrative <- renderText(narrative_sample())
  
  #display step-through stories
  #make a loop 
  res <- reactiveValues(narrativeIndex = 1)
  
  observeEvent(input$nextStory, {
    if(res$narrativeIndex > length(narrative_list())) res$narrativeIndex <- 1
    else res$narrativeIndex <- res$narrativeIndex + 1
  })
  
  observeEvent(input$prevStory, {
    if(res$narrativeIndex < 2) res$narrativeIndex <- length(narrative_list())
    else res$narrativeIndex <- res$narrativeIndex - 1
  })
  
  output$narrative2 <- renderText({
    selNarrative <- narrative_list()
    selNarrative[res$narrativeIndex]
  })

}

#-------------
#run Shiny app

shinyApp(ui, server)
