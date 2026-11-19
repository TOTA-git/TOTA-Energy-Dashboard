server <- function(input, output, session) {
#EV CHARGING TAB =========================================================================================================== 
  #VALUE BOX OF TOTAL NUMBER OF CHARGING STATIONS IN T-O -------------------------------------------------------------------
  output$NumberOfStations <- renderValueBox({
      valueBox(
        value = length(unique(df_stations_TO$id)),
        subtitle = "Station locations",
        icon = icon("location-dot"),
        color = "orange"
      )
  })
  
  #VALUE BOX OF TOTAL NUMBER OF PLUGS IN T-O -------------------------------------------------------------------------------
  output$NumberOfPlugs <- renderValueBox({
      total <- sum(
        df_stations_TO$ev_dc_fast_num,
        df_stations_TO$ev_level2_evse_num,
        df_stations_TO$ev_level1_evse_num,
        na.rm = TRUE
      )

      valueBox(
        value = format(total, big.mark = ","),
        subtitle = "EV charging ports",
        icon = icon("charging-station"),
        color = "teal"
      )
  })
  
  #VALUE BOX OF YOY GROWTH IN PLUGS  ---------------------------------------------------------------------------------------
  output$YOYPlugs<- renderValueBox({
    
    df_stations <- df_stations_TO %>%
      mutate(
        Year = as.numeric(format(open_date, "%Y"))
      )
    
    recent_year <- max(df_stations$Year, na.rm = TRUE) - 1
    past_year <- max(df_stations$Year, na.rm = TRUE) - 2
    
    total_recent <- sum(
      df_stations %>%
        filter(Year <= recent_year) %>%
        select(ev_dc_fast_num, ev_level2_evse_num, ev_level1_evse_num),
      na.rm = TRUE
    )
    
    total_past <- sum(
      df_stations %>%
        filter(Year <= past_year) %>%
        select(ev_dc_fast_num, ev_level2_evse_num, ev_level1_evse_num),
      na.rm = TRUE
    )
    
    YoY <- ((total_recent - total_past) / total_past) * 100
    
    valueBox(
      value = paste0(round(YoY, 1),"%"),
      subtitle = paste0(recent_year, " annual growth in ports"),
      icon = icon("plug"),
      color = "orange"
    )
  })
  
  #VALUE BOX OF TOTAL NUMBER OF HOTEL STATIONS------------------------------------------------------------------------------
  output$HotelStations <- renderValueBox({
    
    hotel_stations <- df_stations_TO %>%
      filter(facility_type == "HOTEL") %>%
      summarise(hotel_stations = n_distinct(id)) %>%
      pull(hotel_stations)
    
    valueBox(
      value = format(hotel_stations, big.mark = ","),
      subtitle = "Hotel charging locations",
      icon = icon("hotel"),
      color = "teal"
    )
  })
  
  #MAP OF CHARGING STATIONS IN T-O -----------------------------------------------------------------------------------------------------
  output$StationMap <- renderLeaflet({
    
    station_colors <- c(
      "EV Charging Stations" = "#004B55",
      "Thompson Okanagan Tourism Region" = "#D11B4A"
    )
    
    m_base <- leaflet(data = df_TO_boundary)  
    m_tiles <- addTiles(m_base)
    m_bound <- addPolygons(m_tiles,
                           data = df_TO_boundary,
                           fill = FALSE,
                           color = "#D11B4A",
                           weight = 2,
                           opacity = 1
    ) 
    
    m_markers <- addCircleMarkers(m_bound,
                                  data = df_stations_TO,
                                  lng = ~longitude,
                                  lat = ~latitude,
                                  radius = 7,
                                  color = "#000",
                                  weight = 1,
                                  fillColor = "#004B55",
                                  stroke = TRUE,
                                  fillOpacity = 0.9,
                                  label = ~paste0(station_name),
                                  popup = ~paste0(
                                    "<b>", station_name, "</b><br>",
                                    street_address, "<br>",
                                    "<b>Access: </b>", access_code, "<br>",
                                    "<b>Hours of Operation: </b>", access_days_time, "<br>",
                                    "<b>Date last Confirmed: </b>", date_last_confirmed, "<br>",
                                    "<b>Status: </b>",
                                    ifelse(
                                      status_code == "E",
                                      "Available",
                                      ifelse(
                                        status_code == "P",
                                        "Planned",
                                        "Temporarily Unavailable"
                                      )
                                    ),
                                    "<br>",
                                    ifelse(
                                      !is.na(facility_type),
                                      paste0(
                                        "<b>Facility Type: </b>",
                                        str_to_title(gsub("_", " ", facility_type))
                                      ),
                                      ""
                                    )
                                  )
                                
    ) %>%
      addLegend(
        position = "bottomright",
        colors = unname(station_colors),
        labels = names(station_colors),
        title = "",
        opacity = 1
      )
    
    setView(m_markers, lng = -118.196086, lat = 50.998195, zoom = 7)
    
  })

  #NUMBER OF STATIONS BY CITY BAR CHART ------------------------------------------------------------------------------------------------
  output$CityStationsPlot <- renderPlotly({
    
    plot_width <- session$clientData$output_CityStationsPlot_width
    is_narrow <- !is.null(plot_width) && plot_width < 600
    
    df_city <- df_stations_TO %>%
      group_by(city) %>%
      summarise(
        stations = n_distinct(id),
        plugs = sum(
          ev_dc_fast_num,
          ev_level2_evse_num,
          ev_level1_evse_num,
          na.rm = TRUE
        ),
        .groups = "drop"
      ) %>%
      arrange(stations) %>%
      mutate(city = factor(city, levels = city))
    
    plot <- ggplot(df_city, aes(x = stations,
                                y = city,
                                text = paste0(
                                  "<b>", city, "</b>",
                                  "<br><b>Stations: </b>", format(stations, big.mark = ","),
                                  "<br><b>Ports: </b>", plugs
                                )))+
      
      geom_col(fill = "#76ACA9") +
      
      scale_x_continuous(
        breaks = seq(0, 10000, by = 10),
        expand = c(0, 0)
      )+
      
      labs(
        y = "",
        x = "Number of Stations") +
      
      theme_minimal() +
      
      theme(
        axis.title = element_text(size = if (is_narrow) 10 else 16),
        axis.text = element_text(size = if (is_narrow) 8 else 10),
      )
    
    ggplotly(plot, tooltip = "text") %>%
      layout(
        margin = list(l = 0, r = 10, t = 0, b = 50),
        hoverlabel = list(
          bgcolor = "#004B55",
          bordercolor = "#00363D",
          font = list(
            color = "white",
            size = 13,
            family = "Arial"
          ),
          align = "left"
        )
      ) %>%
      config(displayModeBar = FALSE)
  })
  
#RENEWABLE ENERGY VS FOSSIL FUEL TAB ===================================================================================================
  #NUMBER OF STATIONS BY FACILITY BAR CHART --------------------------------------------------------------------------------------------
  output$FacilityPlot <- renderPlotly({
    
    plot_width <- session$clientData$output_FacilityPlot_width
    is_narrow <- !is.null(plot_width) && plot_width < 600
    
    df_facility <- df_stations_TO %>%
      filter(!is.na(facility_type)) %>%
      mutate(facility_type = str_to_title(gsub("_", " ", facility_type))) %>%
      distinct(id, facility_type) %>%
      count(facility_type, name = "stations") %>%
      arrange(stations) %>%
      mutate(facility_type = factor(facility_type, levels = facility_type))
    
    plot <- ggplot(df_facility, aes(x = stations,
                                y = facility_type,
                                text = paste0(
                                  "<b>Facility: </b>", facility_type,
                                  "<br><b>Stations: </b>", format(stations, big.mark = ",")
                                )))+
      
      geom_col(fill = "#76ACA9") +
      
      scale_x_continuous(
        breaks = seq(0, 10000, by = 4),
        expand = c(0, 0)
      )+
      
      labs(
        y = "",
        x = "Number of Stations") +
      
      theme_minimal() +
      
      theme(
        axis.title = element_text(size = if (is_narrow) 10 else 16),
        axis.text = element_text(size = if (is_narrow) 8 else 12)
      )
    
    ggplotly(plot, tooltip = "text") %>%
      layout(
        margin = list(l = 0, r = 20, t = 0, b = 50),
        hoverlabel = list(
          bgcolor = "#004B55",
          bordercolor = "#00363D",
          font = list(
            color = "white",
            size = 13,
            family = "Arial"
          ),
          align = "left"
        )
      ) %>%
      config(displayModeBar = FALSE)
  })
  
  #GROWTH OF CHARGING STATIONS OVER TIME -----------------------------------------------------------------------------------------------
  output$GrowthPlot <- renderPlot({
    
    plot_width <- session$clientData$output_GrowthPlot_width
    is_narrow <- !is.null(plot_width) && plot_width < 600
    
    df_growth <- df_stations_TO %>%
      filter(!is.na(open_date)) %>%
      count(open_date, name = "stations_opened") %>%
      arrange(open_date) %>%
      mutate(
        total_stations = cumsum(stations_opened)
      )
    
 ggplot(df_growth, aes(x = open_date, 
                       y = total_stations))+
     
      geom_line(linewidth = 1.5, color = "#004B55") +

      scale_y_continuous(
        breaks = seq(0, 10000, by = 50),
        expand = expansion(mult = c(0, 0.1))
      ) +
     
     scale_x_date(
       date_breaks = if (is_narrow) "3 years" else "1 year",
       expand = expansion(mult = c(0, 0)),
       date_labels = "%Y"
     )+
     
      labs(
        x = "",
        y = "Total charging stations"
      ) +
     
     theme_minimal() +
     
      theme(
        axis.title = element_text(size = if (is_narrow) 10 else 18),
        axis.text = element_text(size = if (is_narrow) 8 else 12)
      )
   
  }, res = 96)
  
  #COMPARE CHARGING TYPES DOUGGHNUT CHART ----------------------------------------------------------------------------------------------
  output$ChargingTypePlot <- renderPlot({
    
    plot_width <- session$clientData$output_ChargingTypePlot_width
    is_narrow <- !is.null(plot_width) && plot_width < 600
    
    df_types <- df_stations_TO %>%
      select(ev_dc_fast_num, ev_level2_evse_num, ev_level1_evse_num) %>%
      rename( `DC Fast` = ev_dc_fast_num, `Level 2` = ev_level2_evse_num, `Level 1` = ev_level1_evse_num) %>%
      pivot_longer(
        cols = everything(),
        names_to = "charger_type"
      ) %>%
      filter(!is.na(value)) %>%
      group_by(charger_type) %>%
      summarise(total = sum(value), .groups = "drop") 

    df_types$fraction <- df_types$total / sum(df_types$total)
    
    df_types$ymax <- cumsum(df_types$fraction)
    
    df_types$ymin <- c(0, head(df_types$ymax, n= -1))
    
    
    # Create legend labels
    labels <- paste0(df_types$charger_type, " (", df_types$total, ")")
    
    ggplot(df_types, aes(ymax=ymax, ymin=ymin, xmax=4, xmin =3, fill = charger_type)) +
      geom_rect()+
      coord_polar(theta="y") +
      xlim(c(2, 4)) +
    
      scale_fill_manual(
        values = c(
          `DC Fast` = "#F6BC1A",
          `Level 2` = "#76ACA9",
          `Level 1` = "#D11B4A"
        ),
        labels = labels) +
      theme_void() +
      #MOVE LEDGEND BELOW WHEN SMALL SCREEN
      theme(plot.title = element_text(face = 'bold'), legend.title = element_blank(), 
            legend.text =  element_text(size = if (is_narrow) 12 else 18))
  })

#RENEWABLE ENERGY VS. FOSSIL FUEL TAB ============================================================================================
  #VALUE BOX TOTAL ENERGY GENERATION BY YEAR FILTER ------------------------------------------------------------------------------
  output$TotalEnergy<- renderValueBox({
    
    total_energy <- df_genTech2023 %>%
      filter(Year == input$year_energy, Variable == "Total")
    
    valueBox(
      value = paste0(scales::comma(round(total_energy$Value, 2)), " GW.h"),
      subtitle = paste0("Total energy generated for ", input$year_energy),
      icon = icon("bolt-lightning"),
      color = "orange"
    )
  })
  
  #VALUE BOX TOTAL RENEWABLE ENERGY GENERATION BY YEAR FILTER ------------------------------------------------------------------------------
  output$TotalRenewableEnergy<- renderValueBox({
    
    total_renewable_energy <- df_genTech2023 %>%
      filter(Year == input$year_energy, Variable %in% c("Bioenergy", "Hydro", "Onshore Wind", "Solar (Distributed)", "Solar (Utility scale)")) %>%
      summarise(
        total_renewable_energy = sum(Value, na.rm = TRUE)
      ) %>%
      pull(total_renewable_energy)
    
    total_energy <- df_genTech2023 %>%
      filter(Year == input$year_energy, !Variable %in% c("Total", "Hydrogen", "Bioenergy with CCUS"))
    
    Percentage = total_renewable_energy / sum(total_energy$Value) * 100
    
    valueBox(
      value = paste0(round(Percentage, 1), "% Renewable"),
      subtitle = paste0("Share of total generation, ", input$year_energy),
      icon = icon("arrows-rotate"),
      color = "teal"
    )
  })
  
  #VALUE BOX LARGEST TECH SOURCE GENERATION BY YEAR FILTER ------------------------------------------------------------------------------
  output$leadingSource<- renderValueBox({
    
    df_filtered <- df_genTech2023 %>%
      filter(Year == input$year_energy, Variable != "Total") %>%
      filter(Value == max(Value))
    
    valueBox(
      value = df_filtered$Variable,
      subtitle = "Leading generation source",
      icon = icon("water"),
      color = "orange"
    )
  })
  
  #COMPARE % RENEWABLE VS FOSSIL FUEL DOUGGHNUT CHART ----------------------------------------------------------------------------
  output$EnergyDonutPlot <- renderPlot({
    
    plot_width <- session$clientData$output_EnergyDonutPlot_width
    is_narrow <- !is.null(plot_width) && plot_width < 600
    
    df_energy_mix <- df_genTech2023 %>%
      filter(
        Year == input$year_energy,
        !Variable %in% c("Total", "Hydrogen", "Bioenergy with CCUS")
      ) %>%
      mutate(
        Energy_Type = case_when(
          Variable %in% c(
            "Bioenergy",
            "Hydro",
            "Onshore Wind",
            "Solar (Distributed)",
            "Solar (Utility scale)"
          ) ~ "Renewable",
          
          Variable %in% c(
            "Natural Gas",
            "Oil"
          ) ~ "Fossil Fuels"
        )
      ) %>%
      group_by(Energy_Type) %>%
      summarise(Value = sum(Value, na.rm = TRUE), .groups = "drop") %>%
      mutate(
        Percentage = Value / sum(Value) * 100,
        Label = paste0(Energy_Type, " ",round(Percentage, 1), "%")
      )
    
    ggplot(df_energy_mix, aes(x = 2, y = Value, fill = Energy_Type)) +
      geom_col(width = 1, color = "#76ACA9") +
      coord_polar(theta = "y") +
      xlim(0.5, 2.5) +
      geom_text(
        aes(label = paste0(round(Percentage, 1), "%")),
        position = position_stack(vjust = 0.5),
        color = "white",
        fontface = "bold",
        size = if (is_narrow) 6 else 10
      ) +
      scale_fill_manual(
        values = c(
          "Renewable" = "#76ACA9",
          "Fossil Fuels" = "#004B55"
        ),
        labels = df_energy_mix$Label
      ) +
      theme_void() +
      theme(legend.title = element_blank(),
            legend.text = element_text(size = if (is_narrow) 12 else 18),
            legend.position = "right"
      )
  })
  
  output$EnergyDonutPlotBox <- renderUI(
    box(
      title = paste0("Renewable vs. Fossil Fuel Electricity Generation (%) - British Columbia (", input$year_energy, ")"),
      solidHeader = TRUE,
      width = 12,
      plotOutput("EnergyDonutPlot", height = 500)
    )
  )
  
  #ELECTRICITY GENERATION BY TECH OVER TIME ---------------------------------------------------------------------------
  output$EnergyTechPlot <- renderPlot({
    
    plot_width <- session$clientData$output_EnergyTechPlot_width
    is_narrow <- !is.null(plot_width) && plot_width < 600
    
    df_genTech_filtered <- df_genTech %>%
      filter(Variable != "Total") %>%
      mutate(
        Variable = factor(
          Variable,
          levels = c(
            "Hydro",
            "Onshore Wind",
            "Bioenergy",
            "Solar (Distributed)",
            "Solar (Utility scale)",
            "Natural Gas",
            "Oil",
            "Hydrogen",
            "Bioenergy with CCUS"
          )
        )
      )
    
    ggplot(df_genTech_filtered, aes(x = Year,
                                    y = Value,
                                    fill = Variable
                                    )) +
      geom_area() +
      
      geom_vline(
        xintercept = 2023.5,
        color = "black",
        linewidth = 0.5,
        linetype = "dashed"
      )+
      
      annotate(
        "text",
        x = if (is_narrow) ((min(df_genTech_filtered$Year) + 2023) / 2) else 2020.5,
        y = max(df_genTech_filtered$Value, na.rm = TRUE) * 2,
        label = paste0("Historical"),
        size = if(is_narrow) 3 else 4.5,
        color = "black",
        fontface = "bold"
      ) +
      
      annotate(
        "text",
        x = if (is_narrow) ((2023 + max(df_genTech_filtered$Year)) / 2) else 2026.5,
        y = max(df_genTech_filtered$Value, na.rm = TRUE) * 2,
        label = paste0("Projection"),
        size = if(is_narrow) 3 else 4.5,
        color = "black",
        fontface = "bold"
      ) +
      
      scale_fill_manual(values = c("Hydro" = "#76ACA9",
                                     "Bioenergy" = "#F6BC1A",
                                     "Onshore Wind" = "#004B55",
                                     "Solar (Distributed)" = "#7895A5",
                                     "Solar (Utility scale)" = "#BCB49E",
                                     "Natural Gas" = "#589956",
                                     "Oil" = "#C78369",
                                     "Hydrogen" = "#967D91"),
                        guide = guide_legend(ncol = if (is_narrow) 2 else 1)
                        ) +
    
      scale_y_continuous(
        expand = expansion(mult = c(0.01, 0.1))
      ) +
      
      scale_x_continuous(
        breaks = seq(min(df_genTech_filtered$Year), max(df_genTech_filtered$Year), by = if (is_narrow) 10 else 5),
        expand = expansion(mult = c(0, 0))
      )+
      
      labs(
        x = "",
        y = "Gigawatt Hours",
        fill = "Technology"
      ) +
      
      theme_minimal() +
      
      theme(
        axis.title = element_text(size = if (is_narrow) 10 else 18),
        axis.text = element_text(size = if (is_narrow) 8 else 14),
        legend.title = element_text(size = if (is_narrow) 8 else 16),
        legend.text = element_text(size = if (is_narrow) 6 else 14),
        legend.position = if (is_narrow) "bottom" else "right",
        legend.direction = "vertical",
        legend.location = "plot",
        legend.justification = "left"
      )
  }, res = 97)
  
  #TOTAL BC GENERATION OVER TIME -----------------------------------------------------------------------------------
  output$BCGenerationPlot <- renderPlotly({
    
    plot_width <- session$clientData$output_BCGenerationPlot_width
    is_narrow <- !is.null(plot_width) && plot_width < 600
    
    df_genTech_filtered <- df_genTech %>%
      filter(Variable == "Total")
    
   plot <- ggplot(df_genTech_filtered, aes(x = Year,
                                           y = Value,
                                           text = paste0("<b>", Year, "</b><br>", "Total: ", scales::comma(Value), " GW.h")))+
      
      geom_col(fill = ifelse(df_genTech_filtered$Year > 2023, "#76ACA9", "#004B55")) +
      
      geom_vline(
          xintercept = 2023.5,
          color = "black",
          linewidth = 0.5,
          linetype = "dashed"
      )+
      
     annotate(
       "text",
       x = if (is_narrow) ((min(df_genTech_filtered$Year) + 2023) / 2) else 2020.5,
       y = max(df_genTech_filtered$Value, na.rm = TRUE) * 1.12,
       label = paste0("<b>Historical</b>"),
       size = if(is_narrow) 3 else 4,
       color = "#004B55"
     ) +
     
     annotate(
       "text",
       x = if (is_narrow) ((2023 + max(df_genTech_filtered$Year)) / 2) else 2026.5,
       y = max(df_genTech_filtered$Value, na.rm = TRUE) * 1.12,
       label = paste0("<b>Projection</b>"),
       size = if(is_narrow) 3 else 4,
       color = "#76ACA9"
     ) +
      
      scale_y_continuous(
        expand = expansion(mult = c(0.01, 0.2))
      ) +
      
      scale_x_continuous(
        breaks = seq(min(df_genTech_filtered$Year), max(df_genTech_filtered$Year), by = if (is_narrow) 10 else 5),
        expand = expansion(mult = c(0, 0))
      )+
      
      labs(
        x = "",
        y = "Gigawatt Hours"
      ) +
      
      theme_minimal() +
      
      theme(
        axis.title = element_text(size = if (is_narrow) 10 else 18),
        axis.text = element_text(size = if (is_narrow) 8 else 14)
      )
   
   ggplotly(plot, tooltip = "text") %>%
     layout(
       margin = list(l = 10, r = 20, t = 30, b = 50),
       hoverlabel = list(
         bgcolor = "#004B55",
         bordercolor = "#00363D",
         font = list(
           color = "white",
           size = 13,
           family = "Arial"
         ),
         align = "left"
       )
     ) %>%
     config(displayModeBar = FALSE)
  })
  
#END USE DEMAND TAB ============================================================================================
  #VALUE BOX FOR TOTAL TRANSPORT DEMAND FOR SELECTED YEAR ------------------------------------------------------
  output$TotalTransportDemand <- renderValueBox({
    Total_demand <- df_end_use2023 %>%
      filter(Year == input$year_endUse, Variable == "Total End-Use", Sector == "Transportation")
    
    valueBox(
      value = paste0(round(Total_demand$Value, 2), " Petajoules"),
      subtitle = paste0("Total transport demand, ", input$year_endUse),
      icon = icon("gas-pump"),
      color = "teal"
    )
  })
  
  #VALUE BOX FOR TOTAL SHARE OF GAS FOR SELECTED YEAR ------------------------------------------------------
  output$GasShare <- renderValueBox({
    total_gas <- df_end_use2023 %>%
      filter(Year == input$year_endUse, Variable == "Motor Gasoline", Sector == "Transportation")
    
    total <- df_end_use2023 %>%
      filter(Year == input$year_endUse, Variable == "Total End-Use", Sector == "Transportation")
    
    total_share = (total_gas$Value / total$Value) * 100
    
    valueBox(
      value = paste0(round(total_share, 1), "%"),
      subtitle = paste0("Share of motor gasoline, ", input$year_endUse),
      icon = icon("car-side"),
      color = "orange"
    )
  })
  
  #VALUE BOX FOR TOTAL SHARE OF GAS FOR SELECTED YEAR ------------------------------------------------------
  output$AviationShare <- renderValueBox({
    total_gas <- df_end_use2023 %>%
      filter(Year == input$year_endUse, Variable == "Aviation Fuel", Sector == "Transportation")
    
    total <- df_end_use2023 %>%
      filter(Year == input$year_endUse, Variable == "Total End-Use", Sector == "Transportation")
    
    total_share = (total_gas$Value / total$Value) * 100
    
    valueBox(
      value = paste0(round(total_share, 1), "%"),
      subtitle = paste0("Share of aviation fuel, ", input$year_endUse),
      icon = icon("plane"),
      color = "teal"
    )
  })
  
  #TOTAL BC TRANSPORTATION DEMAND FOR SELECTED YEAR ------------------------------------------------------------
  output$FuelBarChartPlot <- renderPlotly({
    
    plot_width <- session$clientData$output_FuelBarChartPlot_width
    is_narrow <- !is.null(plot_width) && plot_width < 600
    
    df_end_use_filtered <- df_end_use %>%
      filter(Year == input$year_endUse, Sector == "Transportation", Variable != "Total End-Use") %>%
      arrange(Value) %>%
      mutate(
        Variable = factor(Variable, levels = Variable)
      )
    
    plot <- ggplot(df_end_use_filtered, aes(x = Value,
                                            y = Variable,
                                            text = paste0("<b>", Variable, ": </b>", scales::comma(Value), " petajoules")))+
      
      geom_col(fill = "#76ACA9") +
      
      scale_y_discrete(
        expand = expansion(mult = c(0, 0))
      ) +
      
      scale_x_continuous(
        breaks = seq(0, max(df_end_use_filtered$Value), by = if (is_narrow) 50 else 10),
        expand = expansion(mult = c(0, 0.01))
      )+
      
      labs(
        x = "Petajoules",
        y = NULL
      ) +
      
      theme_minimal() +
      
      theme(
        axis.title = element_text(size = if (is_narrow) 10 else 16),
        axis.text = element_text(size = if (is_narrow) 8 else 14)
      )
    
    ggplotly(plot, tooltip = "text") %>%
      layout(
        margin = list(l = 0, r = 30, t = 0, b = 50),
        hoverlabel = list(
          bgcolor = "#004B55",
          bordercolor = "#00363D",
          font = list(
            color = "white",
            size = 13,
            family = "Arial"
          ),
          align = "left"
        )
      ) %>%
      config(displayModeBar = FALSE)
  })
  
  #DYNAMIC OUTPUT BOX FOR FUELBARCHART PLOT -----------------------------------------------------------------------
  output$FuelBarChartPlotBox <- renderUI({
    box(
      title = paste0("Transportation Energy Demand for ", input$year_endUse, "  - British Columbia"),
      solidHeader = TRUE,
      width = 12,
      plotlyOutput("FuelBarChartPlot", height = 500)
    )
  })
  
  #TRANSPORT ENERGY DEMAND BY FUEL TYPE ---------------------------------------------------------------------------
  output$FuelPlot <- renderPlot({
    
    plot_width <- session$clientData$output_FuelPlot_width
    is_narrow <- !is.null(plot_width) && plot_width < 600
    
    df_end_use_filtered <- df_end_use%>%
      filter(Variable != "Total End-Use", Sector == "Transportation") %>%
      mutate(
        Variable = factor(
          Variable,
          levels = c(
            "Motor Gasoline",
            "Diesel",
            "Aviation Fuel",
            "Heavy Fuel Oil",
            "LPG",
            "Lubricants",
            "Natural Gas",
            "Biofuels",
            "Hydrogen",
            "Electricity"
          )
        )
      )
    
    ggplot(df_end_use_filtered, aes(x = Year,
                                    y = Value,
                                    fill = Variable
    )) +
      geom_area() +
      
      geom_vline(
        xintercept = 2023.5,
        color = "black",
        linewidth = 0.5,
        linetype = "dashed"
      )+
      
      annotate(
        "text",
        x = if (is_narrow) ((min(df_end_use_filtered$Year) + 2023) / 2) else 2020.5,
        y = max(df_end_use_filtered$Value, na.rm = TRUE) * 3,
        label = paste0("Historical"),
        size = if(is_narrow) 3 else 4.5,
        color = "black",
        fontface = "bold"
      ) +
      
      annotate(
        "text",
        x = if (is_narrow) ((2023 + max(df_end_use_filtered$Year)) / 2) else 2026.5,
        y = max(df_end_use_filtered$Value, na.rm = TRUE) * 3,
        label = paste0("Projection"),
        size = if(is_narrow) 3 else 4.5,
        color = "black",
        fontface = "bold"
      ) +
      
      scale_fill_manual(values = c("Aviation Fuel" = "#BCB49E",
                                   "Biofuels" = "#F6BC1A",
                                   "Diesel" = "#004B55",
                                   "Electricity" = "#589956",
                                   "Heavy Fuel Oil" = "#7895A5",
                                   "Hydrogen" = "#B3B4F2",
                                   "LPG" = "#C78369",
                                   "Lubricants" = "#967D91",
                                   "Motor Gasoline" = "#76ACA9",
                                   "Natural Gas" = "#E87DBB"),
                        guide = guide_legend(ncol = if (is_narrow) 2 else 1)
      ) +
      
      scale_y_continuous(
        expand = expansion(mult = c(0.01, 0.1))
      ) +
      
      scale_x_continuous(
        breaks = seq(min(df_end_use_filtered$Year), max(df_end_use_filtered$Year), by = if (is_narrow) 10 else 5),
        expand = expansion(mult = c(0, 0))
      )+
      
      labs(
        x = "",
        y = "Petajoules",
        fill = "Fuel Type"
      ) +
      
      theme_minimal() +
      
      theme(
        axis.title = element_text(size = if (is_narrow) 10 else 18),
        axis.text = element_text(size = if (is_narrow) 8 else 16),
        legend.title = element_text(size = if (is_narrow) 8 else 16),
        legend.text = element_text(size = if (is_narrow) 8 else 14),
        legend.position = if (is_narrow) "bottom" else "right",
        legend.direction = "vertical",
        legend.location = "plot",
        legend.justification = "left"
      )
  }, res = 97)
  
  #TOTAL BC TRANSPORTATION GENERATION OVER TIME ---------------------------------------------------------------------------
  output$TransportationPlot <- renderPlotly({
    
    plot_width <- session$clientData$output_TransportationPlot_width
    is_narrow <- !is.null(plot_width) && plot_width < 600
    
    df_end_use_filtered <- df_end_use %>%
      filter(Variable == "Total End-Use", Sector == "Transportation")
    
    plot <- ggplot(df_end_use_filtered, aes(x = Year,
                                            y = Value,
                                            text = paste0("<b>", Year, "</b><br>", "Total: ", scales::comma(Value), " petajoules")))+
      
      geom_col(fill = ifelse(df_end_use_filtered$Year > 2023, "#76ACA9", "#004B55")) +
      
      geom_vline(
        xintercept = 2023.5,
        color = "black",
        linewidth = 0.5,
        linetype = "dashed"
      )+
      
      annotate(
        "text",
        x = if (is_narrow) ((min(df_end_use_filtered$Year) + 2023) / 2) else 2020.5,
        y = max(df_end_use_filtered$Value, na.rm = TRUE) * 1.12,
        label = paste0("<b>Historical</b>"),
        size = if(is_narrow) 3 else 4,
        color = "#004B55"
      ) +
      
      annotate(
        "text",
        x = if (is_narrow) ((2023 + max(df_end_use_filtered$Year)) / 2) else 2026.5,
        y = max(df_end_use_filtered$Value, na.rm = TRUE) * 1.12,
        label = paste0("<b>Projection</b>"),
        size = if(is_narrow) 3 else 4,
        color = "#76ACA9"
      ) +
      
      scale_y_continuous(
        expand = expansion(mult = c(0.01, 0.2))
      ) +
      
      scale_x_continuous(
        breaks = seq(min(df_end_use_filtered$Year), max(df_end_use_filtered$Year), by = if (is_narrow) 10 else 5),
        expand = expansion(mult = c(0, 0))
      )+
      
      labs(
        x = "",
        y = "Petajoules"
      ) +
      
      theme_minimal() +
      
      theme(
        axis.title = element_text(size = if (is_narrow) 10 else 18),
        axis.text = element_text(size = if (is_narrow) 8 else 14)
      )
    
    ggplotly(plot, tooltip = "text") %>%
      layout(
        margin = list(l = 10, r = 20, t = 30, b = 50),
        hoverlabel = list(
          bgcolor = "#004B55",
          bordercolor = "#00363D",
          font = list(
            color = "white",
            size = 13,
            family = "Arial"
          ),
          align = "left"
        )
      ) %>%
      config(displayModeBar = FALSE)
  })
  
  #OVERVIEW PAGE =======================================================================================================
  
  #CLICKABLE LINK FOR EV CHARGING TAB --------------------------------------------------------------------------------
  observeEvent(input$ev_link, {
    updateTabItems(session, "tabs", "ev")
  })
  
  observeEvent(input$renew_fossil_link, {
    updateTabItems(session, "tabs", "renew_fossil")
  })
  
  observeEvent(input$end_use_link, {
    updateTabItems(session, "tabs", "end_use")
  })
}
