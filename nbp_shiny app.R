###-----------------------------------------------------------------------------
### NBP SHINY APP
### 
### Jan
###
### 01/2025
###-----------------------------------------------------------------------------

##---- Packages ----------------------------------------------------------------
library(shiny)
library(bslib)
library(shinythemes)

library(tidyverse)
library(sf)
library(ggplot2)
library(ggthemes)
library(ggiraph)
library(ggspatial)

library(haven)

library(DT)
##---- Data --------------------------------------------------------------------
#df_admin <- st_read("world-administrative-boundaries.shp")
#df_cshapes_annual <- st_read("cshapes_annual.geojson")

df_map <- st_read("nbp_map_data.geojson")

df_map <- df_map %>%
  mutate(Monolingual = as.factor(Monolingual),
         MonolingualStrict = as.factor(MonolingualStrict))

ggplot() +
  geom_sf(data = df_map %>%
            filter(year == 1962), aes(fill = Monolingual)) + 
#  annotation_scale(location = "bl", width_hint = 0.3) +
#  annotation_north_arrow(location = "bl", which_north = "true", 
#                         pad_x = unit(0.75, "in"), pad_y = unit(0.5, "in"),
#                         style = north_arrow_fancy_orienteering) +
  theme_map()

ggplot() +
  geom_line(data = df_map %>%
              filter(Country == "United States of America"), aes(x = year, y = HI)) +
  theme_clean()

df_groups <- read_dta("NBP_groups_final.dta") %>%
  select(c(Country, Year, Group, Lang1, Reli1, AnyRestriLang1, AnyRestriReli1))
##---- APP ---------------------------------------------------------------------

# Define UI
ui <- fluidPage(
  shinythemes::themeSelector(),
  titlePanel(markdown("# **Ethnicgoods NBP**")),
  navbarPage(HTML('<span style="font-size: 30px; font-weight: bold;">MENU</span>'),
             tabPanel("HOME",
                      fluidRow(
                        column(12, h3("Page 1")),
                        column(12, p("e.g., Introduction here"))
                      )
             ),
             tabPanel("World Map",
                      fluidRow(
                        column(12, h3("Nation-Building Policies Around the World")),
                        column(12, p("Select a year and a variable to explore how states built nations!")),
                        sidebarLayout(
                          sidebarPanel(
                            selectInput("year", "Select Year:", choices = c(1945:2020), selected = 2000),
                            selectInput("mapvar", "Select Variable:", choices = names(df_map), selected = "HI"),
                            width = 3
                        ),
                        mainPanel(
                          tags$style(HTML("
                            .main-panel {
                              padding-top: 0px; /* Remove any padding above the plot */
                              margin-top: 0px;  /* Ensure no margin is added */
                              height: 100%;     /* Ensure the height is 100% */
                            }
                            #world_map_plot {
                              height: 100%;     /* Ensure the plot takes up the full height of the main panel */
                              margin-top: 0px;  /* Align the plot to the top of the panel */
                            }
                          ")),
                          girafeOutput("world_map_plot", width = "100%"),
                          width = 9
                        )
                        )
                      )
             ),
             tabPanel("Countries",
                      fluidRow(
                        column(12, h3("Page 3")),
                        column(12, p("Content for Page 3.")),
                        sidebarLayout(
                          sidebarPanel(
                            selectInput("country", "Select Country", choices = unique(df_map$Country)),
                            selectInput("cyear", "Select Year", choices = NULL)
                            #selectInput("cygroup", "Select Group", choices = NULL) 
                          ),
                          mainPanel(
                            plotOutput("cmap"), ## below country map filled with INCLUSION INDEX table with value for dummy vars for selected group in shown country year
                            DTOutput("groupTable")
                          )
                        )
                      )
             ),
             tabPanel("Index",
                      sidebarLayout(
                        sidebarPanel(
                          selectInput("index", "Select Index", choices = c("HI")),
                          selectInput("indexcountry", "Select Country", choices = unique(df_map$Country)),
                          sliderInput("syear", "From:", min = 1945, max = 2020, step = 1, sep = "", value = 1945),
                          sliderInput("eyear", "To:", min = 1945, max = 2020, step = 1, sep = "", value = 2020)
                        ),
                        mainPanel(
                          plotOutput("indexPlot")
                        )
                      ))
  ),
  fixedPanel(
    bottom = 0, left = "2%", width = "auto", right = "auto",
    HTML(
      '<a rel="license" href="http://creativecommons.org/licenses/by-nd/4.0/"><img alt="Creative Commons License" style="border-width:0" src="https://i.creativecommons.org/l/by-nd/4.0/88x31.png" /></a><br />This work is licensed under a <a rel="license" href="http://creativecommons.org/licenses/by-nd/4.0/">Creative Commons Attribution-NoDerivatives 4.0 International License</a>'
    ),
    br(),
    p(sprintf("Last updated on: %s", Sys.Date()))
    )
  )


# Define server logic

server <- function(input, output, session) {
  output$world_map_plot <- renderGirafe({
    map <- ggplot(data = df_map %>% filter(year == input$year)) +
      geom_sf_interactive(aes(fill = .data[[input$mapvar]], geometry = geometry, tooltip = cntry_name)) +
#      annotation_scale(location = "bl", width_hint = 0.3) +
#      annotation_north_arrow(location = "bl", which_north = "true",
#                             pad_x = unit(0.75, "in"), pad_y = unit(0.5, "in"),
#                             style = north_arrow_fancy_orienteering) +
      theme_map() +
      theme(
        #plot.margin = margin(5, 5, 5, 5),
        plot.title = element_blank()
      )

    girafe(ggobj = map)
  })
  # only years for which we include country to be selected
  observeEvent(input$country, {
    available_years <- unique(df_map$year[df_map$Country == input$country])
    updateSelectInput(session, "cyear", choices = available_years, selected = available_years[1])
  })
  #filter reactive for country and year
  filtered_data <- reactive({
    req(input$country, input$cyear)  # Ensure both inputs are selected
    df_map[df_map$Country == input$country & df_map$year == input$cyear, ]
  })

  output$cmap <- renderPlot({
    data <- filtered_data()

    if (nrow(data) == 0) return(NULL)

    ggplot(data) +
      geom_sf(aes(fill = HI)) +
      theme_map() +
      ggtitle(paste("Map of", input$country, "in", input$cyear))
  })
  # output$groupTable <- renderDT({
  #   req(input$country, input$cyear)  
  #   table_data <- df_groups[df_groups$Country == input$country & df_groups$Year == input$cyear, 
  #                           c("Group", "Lang1", "Reli1", "LangRestri1", "AnyRestriReli1")]
  #   
  #   datatable(table_data, options = list(pageLength = 5, autoWidth = TRUE))  # Interactive table
  # })
  filtered_data_groups <- reactive({
    req(input$country, input$cyear)
    df_groups %>% filter(Country == input$country,
                         Year == input$cyear)
  })
  #output$groupTable <- renderDataTable(filtered_data_groups() %>% select(-1, -2),
  #                                     options = list(pageLength = 5))
  output$groupTable <- renderDataTable({
    datatable(
      filtered_data_groups() %>% select(-1, -2),  
      options = list(pageLength = 5)  
    ) %>%
      formatStyle(
        c("AnyRestriLang1", "AnyRestriReli1"),  
        backgroundColor = styleEqual(
          c(0, 1), 
          c("lightgreen", "lightcoral")  
        )
      )
  })
  output$indexPlot <- renderPlot({
    ggplot(data = df_map %>%
             filter(Country == input$indexcountry), aes(x = year, y = .data[[input$index]])) +
      geom_line() +
      xlim(input$syear, input$eyear) +
      theme_minimal()
  })

}

# Run the app
shinyApp(ui = ui, server = server)
