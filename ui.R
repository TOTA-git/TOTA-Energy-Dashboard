library(shiny)
library(bslib)
library(shinydashboard)
library(fresh)

TOTA_theme <- create_theme(
  adminlte_color(
    light_blue = "#004B55",
    yellow = "#BCB49E",
    orange = "#F6BC1A",
    teal = "#76ACA9",
    fuchsia = "#D11B4A"
    #red, yellow, aqua, blue, light-blue, green, navy, teal, olive, lime, orange, fuchsia, purple, maroon, black.
  )
)

footer_TOTA <- tags$footer( 
  tags$style(HTML("
    @media (max-width: 600px) {
      #footer-logo img {
        min-width: 60px;
      }
      #footer-text p {
        font-size: 12px !important;
      }
      #footer-credit p {
        font-size: 11px !important;
      }
    }
  ")),
  div(
    div(
      tags$img(
        src = "side_bar_logo.png",
        width = "80%",
        style = "display: block; margin: 0 auto;"
      ),
      id = "footer-logo",
      style = "width: 15%; display: flex; align-items: center; justify-content: center;"
    ),
    
    div(
      p("Thompson Okanagan Tourism Association",
        style = "font-weight: bold; margin-bottom: 5px;"
      ),
      
      tags$a(
        "www.totabc.org",
        href = "https://www.totabc.org/",
        target = "_blank",
        style ="color: white;"
      ),
      
      
      p("2280-D Leckie Road, Kelowna,",
        style = "margin-bottom: 2px;"
      ),
      
      p("British Columbia, V1X 6G6",
        style = "margin-bottom: 0;"
      ),
      id = "footer-text",
      style = "width: 60%; color: white; display: flex; flex-direction: column; justify-content: center; padding-left: 5%;"
    ),
    
    div(
      tags$p(
        tags$a(
          "Created by Alexis Samp",
          href = "https://ca.linkedin.com/in/alexis-samp-b89678342",
          target = "_blank",
          style ="color: white;"
        ),
        style = "color: white; width: 100%; font-size: 14px; text-align: center; text-decoration: underline;"
      ),
      id = "footer-credit"
    ),
    style = "width: 100%; background-color: #004B55; display: flex; align-items: center; padding: 20px 5%; box-sizing: border-box; color: white;"
  ),
  style = "position: relative; bottom: 0; left: 0; width: 100%; z-index: 9999;"
)

ui <- dashboardPage(
  dashboardHeader(
    title = "Energy Dashboard",
    tags$li(
      class = "dropdown",
      tags$a(
        href = "https://tota-insto-hub.share.connect.posit.cloud/",
        icon("globe"),
        " TOTA INSTO HUB",
        style = "
        color: white;
        font-size: 18px;
        padding: 15px;
        text-decoration: underline;",
      )
    )             
  ),
  
  dashboardSidebar(
    sidebarMenu(
      id = "tabs",
      menuItem("Overview", tabName = "overview", icon = icon("readme")),
      menuItem("EV Charging", tabName = "ev", icon = icon("charging-station")),
      menuItem("Renewable vs. Fossil Fuels", tabName = "renew_fossil", icon = icon("bolt")),
      menuItem("Transportation", tabName = "end_use", icon = icon("car")),
      menuItem("Back to Hub", tabName = NULL, icon = icon("globe"), href = "https://tota-insto-hub.share.connect.posit.cloud/", newtab = FALSE)
    ),
    
    tags$img(
      src = "side_bar_logo.png",
      width = "85%",
      style = "display: block; margin: 0 auto; padding-top: 40px;"
    ),
    
    tags$img(
      src = "UN_side_bar_logo.png",
      width = "85%",
      style = "display: block; margin: 0 auto; padding-top: 40px;"
    )
  ),
  
  dashboardBody(
    use_theme(TOTA_theme),
    
    tabItems(
      
      tabItem(
        tabName = "overview",
        
        tags$img(
          src = "banner.png",
          width = "100%",
          style = "position: relative;"
        ),
        p("Destination BC/Andrew Strain"),
        
        fluidRow(
          width = "100%",
          
          column(
            width = 12,
            h2("About this Dashboard"),
            
            p(
              "The purpose of this dashboard is to support TOTA's energy management reporting for the ",
              tags$a(
                href = "https://www.untourism.int/observatories/thompson-okanagan",
                "UN Tourism International Network of Sustainable Tourism Observatories (INSTO)",
                target = "_blank",
                style = "color: #76ACA9; text-decoration: underline;"
              ),
              " and contribute to ongoing efforts to better understand the relationship between energy use, transportation, and tourism sustainability
              in the Thompson Okanagan Region. Within INSTO, energy monitoring is an important component of measuring tourism's environmental impact and 
              supporting more sustainable destination management."
            ),
            br(),
            style = "font-size: 18px;"
          ),
          
          column(
            tags$head(tags$style(
              HTML(
                ".explore-link {
                                    display: block;
                                    font-size: 18px;
                                    font-weight: 600;
                                    margin-top: 10px;
                                    margin-bottom: 10px;
                                    color: #004B55;
                                    text-decoration: none;
                                  }
                                  .explore-link:hover {
                                    text-decoration: underline;
                                    cursor: pointer;
                                  }
                                "
              )
            )),
            
            width = 6,
            h2("What is Energy Management?"),
            
            p("Energy management involves monitoring, understanding, and reducing energy consumption while improving efficiency 
              and supporting the transition toward more sustainable energy sources. Tracking energy use, fuel consumption, and 
              the shift toward renewable energy helps understand resource demands, identifies opportunities to reduce greenhouse gas emissions, 
              and make informed decisions about long-term sustainability."),
            br(),
            
            h2("What Can Be Explored?"),
            
            actionLink(
              "ev_link",
              strong("Electric Vehicle Charging - How many charging stations are there?"),
              class = "explore-link",
            ),
            p("Find locations of charging stations and explore the growth of EV chargers over time."),

            actionLink(
              "renew_fossil_link",
              strong("Renewable Energy vs. Fossil Fuels - How is electricity generated in British Columbia?"),
              class = "explore-link"
            ),
            p("Compare renewable energy vs. fossil fuels and view electricity generation over time."),
            
            actionLink(
              "end_use_link",
              strong("Transportation - What is the energy demand for transportation in British Columbia?"),
              class = "explore-link"
            ),
            p("Compare demand between fuel types and view transportation energy demand over time."),
            br()
          ),
          
          column(
            width = 6,
            tags$img(
              src = "region_map_bg.png",
              width = "70%",
              style = "display: block; margin: 0 auto;"
            ),

            p("The scope of this dashboard is British Columbia", br(), "and the Thompson Okanagan Tourism Region", style = "text-align: center; font-size: 14px;"),
          ),
          
          tags$style(
            HTML(
              ".explore-link {
                              color: #42817A;
                              font-size: 20px;
                              font-weight: bold;
                              text-decoration: none;
                            }

                            .explore-link:hover {
                              text-decoration: underline;
                            }
                          "
            )
          ),
          style = "width: 100%; padding: 20px; font-size: 18px;"
        )
      ), 
      tabItem(
        tabName = "ev",
        
        tags$style(HTML("
          @media (max-width: 600px) {
            #StationMap { height: 350px !important; }
            #CityStationsPlot { height: 650px !important; }
            #FacilityPlot { height: 350px !important; }
            #GrowthPlot { height: 350px !important; }
            #ChargingTypePlot { height: 250px !important; }
          }
        ")),
        
        fluidRow(
          box(
            title = "Electric Vehicle Charging - How many charging stations are there?",
            solidHeader = TRUE,
            width = 12,
            p("Electric vehicle (EV) charging infrastructure is an important part of supporting 
              sustainable transportation and tourism across the Thompson-Okanagan region. This 
              dataset provides information on the location, availability, facility type, charging 
              levels, and number of charging plugs at EV charging stations throughout the region. 
              As more visitors travel by electric vehicle, access to reliable charging infrastructure 
              can influence travel routes, destination choices, and the ability to travel between communities. 
              Charging stations located near tourism destinations, accommodations, restaurants, attractions, 
              and major transportation corridors can help support visitors while also serving local residents 
              and businesses. Tracking the growth and distribution of EV charging infrastructure provides an 
              indication of the region’s capacity to accommodate EV travel and supports broader 
              efforts to reduce transportation-related emissions associated with tourism."),
            style = "font-size: 18px;"
          ),
          
          infoBox(
            title = "Data Freshness",
            width = 12,
            color = "light-blue",
            icon = icon("clock-rotate-left"),
            value = format(as.Date(df_freshness$last_updated), "%B %d, %Y"),
            subtitle = "Data is updated by the National Laboratory of the Rockies"
          ),
          
          valueBoxOutput("NumberOfStations", width = 3),
          valueBoxOutput("NumberOfPlugs", width = 3),
          valueBoxOutput("YOYPlugs", width = 3), 
          valueBoxOutput("HotelStations", width = 3),
          
          box(
            title = "Electric Charging Stations",
            solidHeader = TRUE,
            width = 6,
            leafletOutput("StationMap", height = 800)
          ),
          
          box(
            title = "Number of Charging Stations by Community",
            solidHeader = TRUE,
            width = 6,
            plotlyOutput("CityStationsPlot", height = 800)
          ),
          
          box(
            title = "Number of Charging Stations by Facility Type",
            solidHeader = TRUE,
            width = 12,
            plotlyOutput("FacilityPlot", height = 500)
          ),
          
          box(
            title = "Growth in Charging Stations",
            solidHeader = TRUE,
            width = 12,
            plotOutput("GrowthPlot", height = 500)
          ),
          
          box(
            title = paste0("Charger Types Across ", sum(df_stations_TO$ev_dc_fast_num, df_stations_TO$ev_level2_evse_num, df_stations_TO$ev_level1_evse_num, na.rm = TRUE), " Ports"),
            solidHeader = TRUE,
            width = 12,
            plotOutput("ChargingTypePlot", height = 500)
          ),
          
          box(
            title = "References",
            solidHeader = TRUE,
            width = 12,
            p("Data source: National Laboratory of the Rockies, Alternative Fuel Stations. 
              Retrieved from https://developer.nlr.gov/docs/transportation/alt-fuel-stations-v1/all/"),
            p("Government of Canada. Natural Resources Canada. Electric charging and alternative fuelling stations locator. https://natural-resources.canada.ca/energy-efficiency/transportation-energy-efficiency/electric-charging-alternative-fuelling-stationslocator-map#/find/nearest?fuel=ELEC")
            
          )
        )
      ),
      
      tabItem(
        tabName = "renew_fossil",
        
        tags$style(HTML("
          @media (max-width: 600px) {
            #EnergyDonutPlot { height: 250px !important; }
            #EnergyTechPlot { height: 350px !important; }
            #BCGenerationPlot { height: 350px !important; }
          }
        ")),
        
        fluidRow(
          box(
            title = "Renewable Energy vs. Fossil Fuels - How is electricity generated in British Columbia?",
            solidHeader = TRUE,
            width = 12,
            p("The objective of this data is to understand how energy resources are being used to generate electricity and how the region's electricity generation is changing over time.
              Electricity generation is an important component of regional sustainability. Understanding the energy sources that supply British Columbia help monitor 
              the region's transition toward lower-carbon energy systems. This is particularly relevant to tourism, where accommodation, attractions, transportation infrastructure
              and visitor services all depend on reliable electricity."),
            h3("What Are Fossil Fuels?"),
            p("Fossil fuels are energy sources formed from the remains of ancient plants and animals over millions of years. 
              The primary types in B.C. include natural gas and oil. These sources generate electricity through combustion.
              While reliable and energy-dense, this process releases significant carbon dioxide and other pollutants."),
            h3("What is Renewable Energy?"),
            p("Renewable energy uses naturally replenishing resources that won’t be depleted by human use. B.C. renewable sources include bioenergy, hydro, wind, and solar."),
            style = "font-size: 18px;"
          ),
          
          infoBox(
            title = "",
            width = 6,
            color = "light-blue",
            icon = icon("filter"),
            value = div(
              selectInput(
                "year_energy",
                "Select Year:",
                choices = sort(unique(df_genTech2023$Year)),
                selected = "2023"
              )
            )
          ),
          
          infoBox(
            title = "Data Freshness",
            width = 6,
            color = "light-blue",
            icon = icon("clock-rotate-left"),
            value = paste0(format(df_energy_date$`data$result$date_modified[1]`, "%B %d, %Y")),
            subtitle = "Updated by Government of Canada Energy Regulator"
          ),
          
          valueBoxOutput("TotalEnergy", width = 4),
          valueBoxOutput("TotalRenewableEnergy", width = 4),
          valueBoxOutput("leadingSource", width = 4),
          
          uiOutput("EnergyDonutPlotBox"),
          
          box(
            title = "Electricity Generation by Technology Over Time - British Columbia",
            solidHeader = TRUE,
            width = 12,
            plotOutput("EnergyTechPlot", height = 500)
          ),
          
          box(
            title = "Electricity Generation Over Time - British Columbia",
            solidHeader = TRUE,
            width = 12,
            plotlyOutput("BCGenerationPlot", height = 500)
          ),
        
          box(
            title = "References",
            solidHeader = TRUE,
            width = 12,
            p("Data source: Government of Canada, Canada’s Energy Future 2026: Energy Supply and Demand Projections to 2050 - electricity-generation-technology-2026. 
                Retrieved from https://open.canada.ca/data/en/dataset/07c42deb-9435-43b9-a416-7ce316f3893d/resource/a9ec6395-f2a8-46b8-9bd6-72e45bda2b1e"),
            p("Canada Energy Regulator. Exploring Canada's Energy Future. https://apps2.cer-rec.gc.ca/energy-future")
          )
        )
      ),
      
      tabItem(
        tabName = "end_use",
        
        tags$style(HTML("
          @media (max-width: 600px) {
            #FuelPlot { height: 350px !important; }
            #TransportationPlot { height: 350px !important; }
          }
        ")),
        
        fluidRow(
          
          box(
            title = "Transportation - What is the energy demand for transportation in British Columbia?",
            solidHeader = TRUE,
            width = 12,
            p("Tourism-related travel contributes to transportation energy demand through passenger vehicles, rental cars, tour operators, buses, aviation,
              and other modes of transport. Understanding the types and amounts of energy consumed by transportation helps identify BC's reliance on fossil fuels 
              and track the transition toward lower-carbon alternatives. While this dashboard presents energy consumption at the provincial level, the findings provide 
              valuable context for the Thompson-Okanagan region, where road travel plays an important role in connecting visitors to things like communities, wineries, 
              lakes, and ski resorts."),
            style = "font-size: 18px;"
          ),
          
          infoBox(
            title = "",
            width = 6,
            color = "light-blue",
            icon = icon("filter"),
            value = div(
              selectInput(
                "year_endUse",
                "Select Year:",
                choices = sort(unique(df_end_use2023$Year)),
                selected = 2023
              )
            )
          ),
          
          infoBox(
            title = "Data Freshness",
            width = 6,
            color = "light-blue",
            icon = icon("clock-rotate-left"),
            value = paste0(format(df_energy_date$`data$result$date_modified[1]`, "%B %d, %Y")),
            subtitle = "Updated by Government of Canada Energy Regulator"
          ),
          
          valueBoxOutput("TotalTransportDemand", width = 4),
          valueBoxOutput("GasShare", width = 4),
          valueBoxOutput("AviationShare", width = 4),
          
          uiOutput("FuelBarChartPlotBox"),

          box(
            title = "Transportation Energy Demand by Fuel Type - British Columbia",
            solidHeader = TRUE,
            width = 12,
            plotOutput("FuelPlot", height = 500)
          ),
          
          box(
            title = "Transportation Energy Demand Over Time - British Columbia",
            solidHeader = TRUE,
            width = 12,
            plotlyOutput("TransportationPlot", height = 500)
          ),
          
          box(
            title = "References",
            solidHeader = TRUE,
            width = 12,
            p("Data source: Government of Canada, Canada’s Energy Future 2026: Energy Supply and Demand Projections to 2050 - end-use-demand-2026. 
                Retrieved from https://open.canada.ca/data/en/dataset/07c42deb-9435-43b9-a416-7ce316f3893d/resource/b67e0e60-3200-4330-b2b2-afb7debca8bf"),
            p("Canada Energy Regulator. Exploring Canada's Energy Future. https://apps2.cer-rec.gc.ca/energy-future")
          )
        )
      )
    ),
    footer_TOTA
  )
)