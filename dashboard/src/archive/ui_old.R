#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(leaflet)
library(readr)
library(plotly)
library(networkD3)
library(heatmaply)
library(sigmaNet)
library(shinyWidgets)
library(hwordcloud)
library(wordcloud2)
library(DT)
library(highcharter)
library(htmltools)

state <- 
  data.frame( selections = c("Aguascalientes",      "Baja California",     "Baja California Sur", "Campeche",            "Coahuila",           
                             "Colima",              "Chiapas",             "Chihuahua",           "Ciudad de México",    "Durango",           
                             "Guanajuato",          "Guerrero",            "Hidalgo",             "Jalisco",             "México",           
                             "Michoacán",           "Morelos",             "Nayarit",             "Nuevo León",          "Oaxaca",           
                             "Puebla",              "Querétaro",           "Quintana Roo",        "San Luis Potosí",     "Sinaloa",           
                             "Sonora",              "Tabasco",             "Tamaulipas",          "Tlaxcala",            "Veracruz",           
                             "Yucatán",             "Zacatecas"))

 

# Define UI for application that draws a histogram
navbarPage("Mexico MEP FCO - Jobs in the Energy Sector",
           tabPanel("Main Statistics", 
                    fluidPage(
                      column(width = 8, style = 'padding:3px',
                      fluidRow("Jobs by Location", 
                               tabsetPanel(
                      tabPanel("Municipal Map", leafletOutput("mun_map", width="95%", height="775px")),
                      tabPanel("State Map", leafletOutput("statemap", width="95%", height="775px"))
                               ))
                      ),
                      column(width = 4, style = 'padding:3px',
                             fluidRow("Job Postings Across Time", 
                                      tabsetPanel(
                      tabPanel("Aggregated",
                               tabsetPanel(
                                 tabPanel("Daily", plotlyOutput("p1", height="275px")),
                                 tabPanel("Weekly", plotlyOutput("p10", height="275px")),
                                 tabPanel("Monthly", plotlyOutput("p10.2", height="275px")))),
                      tabPanel("Sector",
                               tabsetPanel(
                      tabPanel("Daily", plotlyOutput("p2", height="275px")),
                      tabPanel("Weekly", plotlyOutput("p11", height="275px")),
                      tabPanel("Monthly", plotlyOutput("p11.1.1", height="275px")))),
                      tabPanel("Subsector",
                               tabsetPanel(
                                 tabPanel("Daily", plotlyOutput("p2.1", height="275px")),
                                 tabPanel("Weekly", plotlyOutput("p11.1", height="275px")),
                                 tabPanel("Monthly", plotlyOutput("p11.2", height="275px"))))),
                            fluidRow("Frequency of Company and Job Title",
                               pickerInput("state", "Select State:",  
                                           multiple = T, 
                                           selected = state$selections,
                                           choices= state$selections,
                                           options = list(`actions-box` = TRUE))
                               ),
                            fluidRow("Top Companies and Job Titles", 
                               tabsetPanel(
                                 tabPanel("Company", plotlyOutput("p3", height="350px")),
                                 tabPanel("Company - Subsector",
                                          selectInput("subsector", "Subsector Type:",  
                                                      choices=
                                                        c("Solar", 
                                                          "Energy Efficiency", 
                                                          "Solar PV", 
                                                          "Wind",
                                                          "Biomass",
                                                          "Electromobility", 
                                                          "Renewables Crosscutting",
                                                          "Midstream", 
                                                          "Downstream", 
                                                          "Upstream",
                                                          "Oil Gas Crosscutting")),
                                          plotlyOutput("p3.1", height="350px")),
                                 tabPanel("Job Title", plotlyOutput("p4", height="350px")))))
                      )
                    )
                    ),
          tabPanel("Key Features",
                   fluidPage(
                     dateRangeInput("daterange", "Date range:",
                                    start  = "2020-07-01",
                                    end    = Sys.Date(),
                                    min    = "2020-07-01",
                                    max    = Sys.Date(),
                                    format = "mm/dd/yy",
                                    separator = " - "),
                     column(width = 8,
                            fluidRow("Disaggregates",
                                     tabsetPanel(
                                       tabPanel("Sector", plotlyOutput("p5", height="300px")),
                                       tabPanel("Function", 
                                                tabsetPanel(
                                                tabPanel("All", plotlyOutput("p6", height="300px")),
                                                tabPanel("Renewable Energy", plotlyOutput("p7", height="300px")),
                                                tabPanel("Oil & Gas", plotlyOutput("p8", height="300px")))),
                                       tabPanel("Level", 
                                                tabsetPanel(
                                                  tabPanel("All", plotlyOutput("p14", height="300px")),
                                                  tabPanel("Renewable Energy", plotlyOutput("p12", height="300px")),
                                                  tabPanel("Oil & Gas", plotlyOutput("p13", height="300px")))),
                                       tabPanel("Type", 
                                                tabsetPanel(
                                                  tabPanel("All", plotlyOutput("p16", height="300px")),
                                                  tabPanel("Renewable Energy", plotlyOutput("p15.1", height="300px")),
                                                  tabPanel("Oil & Gas", plotlyOutput("p15", height="300px"))))
                                     )
                     )),
                     column(width = 4,
                            fluidRow("Subsector - Number of Jobs Classified by Subsector",
                            tabsetPanel(
                              tabPanel("Renewables", plotlyOutput("p17", height="300px")),
                              tabPanel("O&G", plotlyOutput("p18", height="300px")))
                            )
                            ),
                     column(width = 6, 
                            fluidRow("Distribution of Sectoral Jobs by Contract Type and Experience Level", 
                                     sankeyNetworkOutput("p9", height = "325px"))),
                     column(width = 6, 
                            fluidRow("Subsector Jobs Map",
                              selectInput("subsector_choice", "Subsector Type:",  
                                 choices=
                                 c("Solar" = "solar", 
                                   "Energy Efficiency" = "energy_efficiency", 
                                   "Solar PV" = "solar_pv", 
                                   "Biomass" = 'biomass',
                                   "Wind" = "wind",
                                   "Electromobility" = "electromobility", 
                                   "Renewables - Crosscutting" = "renewables_crosscutting",
                                   "Midstream" = "midstream", 
                                   "Downstream" = "downstream", 
                                   "Upstream" = "upstream",
                                   "O&G - Crosscutting" = "oil_gas_crosscutting"))),
                            fluidRow( 
                                     leafletOutput("map_subsector", height="300px"))
                   )
                   )
          ),
          tabPanel("Skills & Capacity",
                   fluidPage(
                     column(width = 6,
                            fluidRow("Distribution of Salary by Sector", plotlyOutput('p19', height="250px"))
                            ),
                     column(width = 6,
                            selectInput("wordcloudsector", "Select Sector", 
                                        choices = c("All" = "all_percent",
                                                    "Renewables" = "ren_eg_percent",
                                                    "Oil & Gas" = "og_percent")),
                            fluidRow("Skills in the Energy Sector", wordcloud2Output('wordcloud', height="250px"))
                            ),
                     column("Key Words by Subsector", width = 12,
                            selectInput("sector_type", "Select Sector", 
                                        choices = c("Oil & Gas" = "Oil & Gas",
                                                    "Renewables" = "Renewables")),
                            fluidRow(htmlOutput("p21", height="300px"))
                     )
                     )),
          tabPanel("Correlation",
                   fluidPage(
                     column(width = 12, offset = 0, style = 'padding:0px',
                            fluidRow("Heatmap - Which sector and function tags are most commonly used on the same job postings?", 
                                     tabsetPanel(
                                       tabPanel("Correlation between Sectors", plotlyOutput("heatmap", height="800px")),
                                       tabPanel("Correlation between Sector and Function", plotlyOutput("heatmap2", height="800px"))
                                     ))
                  )
                  )),
          tabPanel("Network", 
                   sidebarLayout(
                     sidebarPanel(
                       pickerInput("state_2", "Select State:", 
                                   multiple = T, 
                                   selected = state$selections,
                                   options = list(`actions-box` = TRUE),
                                   choices= state$selections)
                     ),
                   mainPanel(
                     sigmaNetOutput("network", height="800px")
                     )
                   )
                   ),
          tabPanel("Download",
                   fluidPage(
                     downloadButton("table", "Download")
                   )
          )
          )

