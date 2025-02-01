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

##---- APP ---------------------------------------------------------------------

# Define UI
ui <- fluidPage(
  shinythemes::themeSelector(),
  navbarPage(HTML('<span style="font-size: 30px; font-weight: bold;">NBP</span>'),
             tabPanel("HOME",
                      fluidRow(
                        column(12, h3("Page 1")),
                        column(12, p("e.g., Introduction here"))
                      )
             ),
             tabPanel("World Map",
                      fluidRow(
                        column(12, h3("Page 2")),
                        column(12, p("Content for Page 2.")),
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
                        column(12, p("Content for Page 3."))
                      )
             )
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
server <- function(input, output) {
  output$world_map_plot <- renderGirafe({
    map <- ggplot(data = df_map %>% filter(year == input$year)) +
      geom_sf_interactive(aes(fill = input$mapvar, geometry = geometry, tooltip = cntry_name)) +
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
}

# Run the app
shinyApp(ui = ui, server = server)
