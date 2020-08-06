library(shiny)
library(gutenbergr)
library(dplyr)

wuth = gutenberg_download(c(768))

ui <- fluidPage(
  titlePanel("Search Wuthering Heights for usage with a word"),
  
  sidebarLayout(
    sidebarPanel(
      textInput(inputId = 'word',
                label = "Type word to search",
                placeholder = "Type a word")
    ),
    
    mainPanel(
      tableOutput("sentences_used")
    )
  )
  
)

server <- function(input, output) {
  
filtered_df <- reactive({
  rn <- wuth %>% add_rownames() %>% 
  filter(grepl(input$word, text)) %>% 
  `[[`("rowname") %>%
  as.numeric()
  after = rn + 1
  prev = rn - 1
  combined = c(prev, rn, after) %>% sort
  filt = wuth %>% 
    add_rownames() %>% 
    filter(rowname %in% combined) %>% 
    mutate(position = ifelse(rowname %in% rn, "main line", 
                          ifelse(rowname %in% prev, "previous line",
                          ifelse(rowname %in% after, "next line", NA)))) %>% 
    select(rowname, position, text)
  return(filt)
  })

  
output$sentences_used =  renderTable(filtered_df())
  
}

shinyApp(ui = ui, server = server)

