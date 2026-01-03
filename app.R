#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Funded ELC Quality Explorer – Shiny App
# Author: Maryam Forghaniallahabadi
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#   Interactive dashboard to explore inspection quality of funded
#   Early Learning & Childcare (ELC) settings in Scotland.
#
#   The app allows users to:
#     - Filter by local authority and quality band
#     - Explore quality distribution nationally and by area
#     - Compare patterns by deprivation (SIMD) and rurality (UR8)
#     - View and download a table of filtered settings
#     - See a summary of QGIS-based access gap analysis
#     - Explore relationship between setting size and quality
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#Load required packages
library(shiny)
library(dplyr)
library(ggplot2)
library(readr)
library(scales)
library(DT)

# app.R  ------------------------------------------------------------
# 1. Load pre-prepared data and set factor levels
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# load the joined dataset created in Rstudio
# ensure key variables are properly typed
#--------------------------------------------------------------------
elc_joined <- read_csv("elc_joined.csv") %>%
  mutate(
    # make sure these are human-readable text, not numeric codes
    local_authority = as.character(local_authority),
    quality_band = factor(
      quality_band,
      levels = c("Low (1–2)", "Medium (3–4)", "High (5–6)"),
      ordered = TRUE
    ),
    simd2020v2_decile = factor(
      simd2020v2_decile,
      levels = as.character(1:10),
      ordered = TRUE
    )
  )

# Pre-compute unique values to populate UI filter choices
la_choices   <- c("All", sort(unique(elc_joined$local_authority)))
band_levels  <- levels(elc_joined$quality_band)
band_choices <- c("All", band_levels)

#---------------------------------------------------------------------#
# 2. User Interface (UI)
#---------------------------------------------------------------------#

ui <- fluidPage(
  titlePanel("Funded ELC Quality Explorer"),
  
  sidebarLayout(
    sidebarPanel(
      # Local authority filter (now uses text labels)
      selectInput(
        inputId  = "la_choice",
        label    = "Local authority:",
        choices  = la_choices,
        selected = "All"
      ),
      
      # Quality band filter
      selectInput(
        inputId  = "band_choice",
        label    = "Quality band filter:",
        choices  = band_choices,
        selected = "All"
      ),
      
      # A Checkbox to focus on missing quality band
      checkboxInput(
        inputId = "missing_qb",
        label   = "Show only settings with missing quality band",
        value   = FALSE
      ),
      
      br(),
      helpText("Tip: start with 'All' to see the national picture, ",
               "then filter to a single authority or band."),
      
      br(),
      
      downloadButton("download_low", "Download filtered settings")
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel(
          "Overview",
          h3("Overall quality distribution"),
          plotOutput("overview_bar")
        ),
        tabPanel(
          "Local authorities",
          h3("Share of settings by quality band and local authority"),
          textOutput("la_selected_text"),
          plotOutput("la_plot", height = "600px")
        ),
        tabPanel(
          "Deprivation (SIMD)",
          h3("Quality by deprivation level (SIMD)"),
          plotOutput("simd_plot")
        ),
        tabPanel(
          "Urban–rural (UR8)",
          h3("Quality across the urban–rural spectrum"),
          plotOutput("ur8_plot")
        ),
        tabPanel(
          "Settings by Quality and Authority",
          h3("Settings with low or selected quality band"),
          p(textOutput("current_filter_heading")),
          DTOutput("low_table")
        ),
        
        ## FOR QGIS TAB
        tabPanel(
          "Access gaps (QGIS)",
          h3("Access to high-quality ELC in SIMD 1–2 areas"),
          p("Summary based on separate QGIS analysis (1 km buffer around high-quality (5–6) ELC settings, using SIMD 2020v2 datazones)."),
          
          fluidRow(
            column(
              width = 4,
              wellPanel(
                h4("Total SIMD 1–2 datazones"),
                tags$h2("1,395"),
                p("Most deprived areas in Scotland")
              )
            ),
            column(
              width = 4,
              wellPanel(
                h4("WITH access to high-quality ELC (< 1 km)"),
                tags$h2("1,351"),
                p("≈ 96.8% of SIMD 1–2 datazones")
              )
            ),
            column(
              width = 4,
              wellPanel(
                h4("WITHOUT access to high-quality ELC"),
                tags$h2("278"),
                p("≈ 19.9% of SIMD 1–2 datazones")
              )
            )
          ),
          
          hr(),
          h4("Interpretation"),
          textOutput("access_gap_text")
        ),
        
        tabPanel(
          "Size vs quality",
          h3("Relationship between setting size and quality"),
          plotOutput("size_plot")
        )
      )
    )
  )
)

#---------------------------------------------------------------------#
# 3. Server Logic
#---------------------------------------------------------------------#

server <- function(input, output, session) {
  
  #---------------------------------------------------------------#
  # 3.1 Filtered dataset (LA + band + checkbox)
  #---------------------------------------------------------------#
  filtered_data <- reactive({
    df <- elc_joined
    
    # 3.1.1. Local authority filter
    if (input$la_choice != "All") {
      df <- df %>% filter(local_authority == input$la_choice)
    }
    
    # 3.1.2. Quality band filter
    if (input$band_choice != "All") {
      df <- df %>% filter(quality_band == input$band_choice)
    }
    
    # 3.1.3. Only rows where quality_band is missing
    if (isTRUE(input$missing_qb)) {
      df <- df %>% filter(is.na(quality_band))
    }
    
    df
  })
  
  # Adding a display band with "Missing" as a level
  add_band_display <- function(df) {
    df %>%
      mutate(
        band_display = ifelse(
          is.na(quality_band),
          "Missing",
          as.character(quality_band)
        )
      )
  }
  
  #-------------------------------------------------------------
  # 3.2 Overview Overview bar chart
  #-------------------------------------------------------------

  output$overview_bar <- renderPlot({
    
    df <- filtered_data() %>% add_band_display()
    
    # Count values per band
    counts <- df %>% 
      count(band_display)
    
    ggplot(df, aes(x = band_display, fill = band_display)) +
      geom_bar() +
      # ---- Adding count labels above bars ----
    geom_text(data = counts,
              aes(x = band_display, y = n, label = n),
              vjust = -0.5, size = 5, fontface = "bold") +
      labs(
        x = "Quality band",
        y = "Number of funded settings",
        title = "Quality distribution of funded ELC settings",
        subtitle = paste0("Local authority: ", input$la_choice, " | Band: ", input$band_choice)
      ) +
      theme_minimal() +
      theme(legend.position = "none")
  })
  #---------------------------------------------------------------
  # 3.3 Local-authority plot (uses filtered_data + checkbox)
  #---------------------------------------------------------------
  # Local-authority plot -----------------------------------------
  output$la_plot <- renderPlot({
    
    # Base data for this plot
    base_df <- if (isTRUE(input$missing_qb)) {
      elc_joined %>% dplyr::filter(is.na(quality_band))
    } else {
      elc_joined %>% dplyr::filter(!is.na(quality_band))
    }
    
    # --- Case 1: band_choice == "All"  → show counts ---------------
    if (input$band_choice == "All") {
      
      la_summary <- base_df %>%
        dplyr::group_by(local_authority) %>%
        dplyr::summarise(
          n_settings = dplyr::n(),
          .groups = "drop"
        ) %>%
        dplyr::arrange(dplyr::desc(n_settings))
      
      la_summary$highlight <- if (input$la_choice == "All") {
        "Other"
      } else {
        ifelse(la_summary$local_authority == input$la_choice,
               "Selected LA", "Other")
      }
      
      p <- ggplot(
        la_summary,
        aes(x = n_settings,
            y = reorder(local_authority, n_settings),
            fill = highlight)
      ) +
        geom_col() +
        scale_fill_manual(
          values = c("Selected LA" = "#d73027",  # red-ish
                     "Other"       = "#4575b4"), # blue-ish
          guide = if (input$la_choice == "All") "none" else guide_legend(title = "")
        ) +
        labs(
          title = if (isTRUE(input$missing_qb)) {
            "Number of funded ELC settings with missing quality band by local authority"
          } else {
            "Number of funded ELC settings by local authority"
          },
          subtitle = if (input$la_choice != "All") {
            paste("Local authority:", input$la_choice)
          } else {
            "Showing data for all local authorities"
          },
          x = "Number of funded settings",
          y = "Local authority"
        ) +
        theme_minimal()
      
      # Label the selected LA's bar with its number of settings
      if (input$la_choice != "All" &&
          input$la_choice %in% la_summary$local_authority) {
        
        p <- p +
          geom_text(
            data = subset(la_summary, local_authority == input$la_choice),
            aes(label = n_settings),
            hjust = -0.2,
            colour = "black",
            fontface = "bold"
          ) +
          # small extra space on x-axis so label isn’t cut off
          expand_limits(x = max(la_summary$n_settings) * 1.05)
      }
      
      return(p)
    }
    
    # --- Case 2: specific band selected (Low / Medium / High) --------
    band_to_use <- input$band_choice
    
    la_summary <- base_df %>%
      dplyr::mutate(in_band = (quality_band == band_to_use)) %>%
      dplyr::group_by(local_authority) %>%
      dplyr::summarise(
        pct_band = 100 * mean(in_band, na.rm = TRUE),
        n        = dplyr::n(),
        .groups  = "drop"
      ) %>%
      dplyr::arrange(dplyr::desc(pct_band))
    
    la_summary$highlight <- if (input$la_choice == "All") {
      "Other"
    } else {
      ifelse(la_summary$local_authority == input$la_choice,
             "Selected LA", "Other")
    }
    
    # build base plot
    p <- ggplot(
      la_summary,
      aes(x = pct_band,
          y = reorder(local_authority, pct_band),
          fill = highlight)
    ) +
      geom_col() +
      scale_fill_manual(
        values = c("Selected LA" = "#d73027",
                   "Other"       = "#4575b4"),
        guide = if (input$la_choice == "All") "none" else guide_legend(title = "")
      ) +
      labs(
        title = paste(
          "Share of", band_to_use,
          "funded ELC settings by local authority"
        ),
        x = paste0("% of settings rated ", band_to_use),
        y = "Local authority"
      ) +
      theme_minimal()
    
    # add % label for the selected LA (e.g. Angus)
    if (input$la_choice != "All" &&
        input$la_choice %in% la_summary$local_authority) {
      
      p <- p +
        geom_text(
          data = subset(la_summary, local_authority == input$la_choice),
          aes(label = paste0(round(pct_band, 1), "%")),
          hjust = -0.2,
          colour = "black",
          fontface = "bold"
        ) +
        # extra space so the label isn't cut off
        expand_limits(x = max(la_summary$pct_band) * 1.05)
    }
    
    p   
    
  })
  
  # Text summary for the selected local authority --------------------------
  output$la_selected_text <- renderText({
    
    # No specific LA selected
    if (input$la_choice == "All") {
      return("Tip: choose a local authority on the left to see its exact number of funded settings.")
    }
    
    # Respect the 'missing quality band' checkbox
    base_df <- if (isTRUE(input$missing_qb)) {
      elc_joined %>% dplyr::filter(is.na(quality_band))
    } else {
      elc_joined %>% dplyr::filter(!is.na(quality_band))
    }
    
    df_la <- base_df %>% dplyr::filter(local_authority == input$la_choice)
    
    # If no rows, say so
    if (nrow(df_la) == 0) {
      return(paste0("Selected authority: ", input$la_choice,
                    " – no funded settings in the current filters."))
    }
    
    # Case A: “All” bands → just count settings
    if (input$band_choice == "All") {
      n_all <- nrow(df_la)
      return(paste0("Selected authority: ", input$la_choice,
                    " – ", n_all, " funded ELC settings (all quality bands)."))
    }
    
    # Case B: specific band selected → numbers + %
    band_to_use <- input$band_choice
    n_all  <- nrow(df_la)
    n_band <- sum(df_la$quality_band == band_to_use, na.rm = TRUE)
    pct    <- round(100 * n_band / n_all, 1)
    
    paste0("Selected authority: ", input$la_choice,
           " – ", n_band, " of ", n_all, " settings (", pct,
           "%) are rated ", band_to_use, ".")
  })
  
  #--------------------------------------------------------------
  # 3.4 Quality by SIMD decile
  #   - All bands  ->  100% stacked bar with % labels
  #   - One band   ->  single-colour bars with % labels
  #--------------------------------------------------------------
  output$simd_plot <- renderPlot({
    # Base data for SIMD plot:
    # - respect local authority
    # - DO NOT filter by band_choice or missing_qb
    # - Having all settings for correct percentages
    base_df <- elc_joined
    
    if (input$la_choice != "All") {
      base_df <- base_df %>% dplyr::filter(local_authority == input$la_choice)
    }
    # No filtering on input$missing_qb here
    
    base_df <- base_df %>%
      dplyr::filter(!is.na(simd2020v2_decile)) %>%
      add_band_display()
    
    selected_LA   <- input$la_choice
    selected_band <- input$band_choice
    
    # ----- Show only settings with missing quality band -----
    if (isTRUE(input$missing_qb)) {
      title_text <- "Share of settings with missing quality band by deprivation level (SIMD)"
      
      simd_missing <- base_df %>%
        dplyr::group_by(simd2020v2_decile) %>%
        dplyr::summarise(
          prop = mean(is.na(quality_band), na.rm = TRUE),
          .groups = "drop"
        ) %>%
        dplyr::mutate(
          label = scales::percent(prop, accuracy = 1)
        )
      
      return(
        ggplot(simd_missing, aes(x = simd2020v2_decile, y = prop)) +
          geom_col(fill = "#c77cff") +  # a purple-ish colour for "Missing"
          geom_text(aes(label = label), vjust = -0.3, size = 3.5) +
          labs(
            x     = "SIMD decile (1 = most deprived, 10 = least deprived)",
            y     = "Share of settings with missing quality band",
            title = title_text
          ) +
          scale_y_continuous(
            limits  = c(0, 1),
            labels  = scales::percent_format(accuracy = 1),
            expand  = expansion(mult = c(0, 0.05))
          ) +
          theme_minimal() +
          theme(plot.title = element_text(size = 14, face = "bold"))
      )
    }
  
    
    # -------- Dynamic title text --------
    title_base <- "Quality by deprivation level (SIMD)"
    # Dynamically create the title and subtitle
    if (selected_LA != "All" && selected_band == "All") {
      title_text <- paste0(title_base, " – ", selected_LA, " (all bands)")
      subtitle <- paste0("Deprivation level for ", selected_LA)
    } else if (selected_LA == "All" && selected_band != "All") {
      title_text <- paste0(title_base, " – band: ", selected_band)
      subtitle <- paste0("Deprivation level for all local authorities | Band: ", selected_band)
    } else if (selected_LA != "All" && selected_band != "All") {
      title_text <- paste0(title_base, " – ", selected_LA, " | band: ", selected_band)
      subtitle <- paste0("Deprivation level for ", selected_LA, " | Band: ", selected_band)
    } else {
      title_text <- title_base
      subtitle <- "Deprivation level for all local authorities"
    }
    
    
    # ---------- CASE 1: ALL BANDS (stacked 100% bar) ----------
    if (selected_band == "All") {
      # proportions per decile & band
      lab_df <- base_df %>%
        dplyr::count(simd2020v2_decile, band_display) %>%
        dplyr::group_by(simd2020v2_decile) %>%
        dplyr::mutate(
          prop  = n / sum(n),
          label = scales::percent(prop, accuracy = 1)
        ) %>%
        dplyr::ungroup()
      
      ggplot(base_df, aes(x = simd2020v2_decile, fill = band_display)) +
        geom_bar(position = "fill") +
        geom_text(
          data     = lab_df,
          aes(x = simd2020v2_decile, y = prop, label = ifelse(prop < 0.05, "", label)),
          position = position_stack(vjust = 0.5),
          size     = 3
        ) +
        labs(
          x     = "SIMD decile (1 = most deprived, 10 = least deprived)",
          y     = "Proportion of settings",
          title = title_text,
          subtitle = subtitle,
          fill  = "Quality band"
        ) +
        scale_y_continuous(labels = scales::percent_format()) +
        theme_minimal() +
        theme(plot.title = element_text(size = 14, face = "bold"))
    } else {
      # ---------- CASE 2: ONE BAND (single-colour bars) ----------
      band_to_use <- selected_band
      
      simd_band <- base_df %>%
        dplyr::group_by(simd2020v2_decile) %>%
        dplyr::summarise(
          prop = mean(quality_band == band_to_use, na.rm = TRUE),
          .groups = "drop"
        ) %>%
        dplyr::mutate(
          label = scales::percent(prop, accuracy = 1)
        )
      
      ggplot(simd_band, aes(x = simd2020v2_decile, y = prop)) +
        geom_col(fill = "#f8766d") +
        geom_text(aes(label = label), vjust = -0.3, size = 3.5) +
        labs(
          x     = "SIMD decile (1 = most deprived, 10 = least deprived)",
          y     = "Share of settings in selected band",
          title = paste0("Share of settings rated ", band_to_use, " by deprivation level (SIMD)"),
          subtitle = subtitle
        ) +
        scale_y_continuous(
          limits  = c(0, 1),
          labels  = scales::percent_format(accuracy = 1),
          expand  = expansion(mult = c(0, 0.05))
        ) +
        theme_minimal() +
        theme(plot.title = element_text(size = 14, face = "bold"))
    }
  })
  
  
  #---------------------------------------------------------------
  # 3.5 Quality by Urban–Rural (UR8)
  #   - All bands  ->  100% stacked horizontal bar with % labels
  #   - One band   ->  single-colour bars with % labels
  #---------------------------------------------------------------
  output$ur8_plot <- renderPlot({
    # Base data:
    base_df <- elc_joined
    
    if (input$la_choice != "All") {
      base_df <- base_df %>% dplyr::filter(local_authority == input$la_choice)
    }
    # No filtering on input$missing_qb here
    
    base_df <- base_df %>%
      dplyr::filter(!is.na(ur8_name)) %>%
      add_band_display()
    
    selected_LA   <- input$la_choice
    selected_band <- input$band_choice
    
    # ----- SPECIAL CASE: show only settings with missing quality band -----
    if (isTRUE(input$missing_qb)) {
      title_text <- "Share of settings with missing quality band across the urban–rural spectrum"
      
      ur8_missing <- base_df %>%
        dplyr::group_by(ur8_name) %>%
        dplyr::summarise(
          prop = mean(is.na(quality_band), na.rm = TRUE),
          .groups = "drop"
        ) %>%
        dplyr::mutate(
          label = scales::percent(prop, accuracy = 1)
        )
      
      return(
        ggplot(ur8_missing, aes(x = ur8_name, y = prop)) +
          geom_col(fill = "#c77cff") +
          geom_text(aes(label = label), hjust = -0.1, size = 3.5) +
          coord_flip() +
          labs(
            x        = "Urban–rural classification (UR8)",
            y        = "Share of settings with missing quality band",
            title    = title_text,
            subtitle = paste0("Local authority: ", selected_LA)
          ) +
          scale_y_continuous(
            limits  = c(0, 1),
            labels  = scales::percent_format(accuracy = 1),
            expand  = expansion(mult = c(0, 0.05))
          ) +
          theme_minimal()
      )
    }
    # ----------------------------------------------------------------------
    
    # -------- Dynamic title and subtitle --------
    title_base <- "Quality across the urban–rural spectrum"
    subtitle <- paste0("Local authority: ", ifelse(selected_LA == "All", "All local authorities", selected_LA))
    
    if (selected_band == "All") {
      title_text <- paste0(title_base, " – ", "All quality bands")
    } else {
      title_text <- paste0(title_base, " – ", selected_band)
      subtitle <- paste0(subtitle, " | Band: ", selected_band)
    }
    
    
    
    # ---------- CASE 1: ALL BANDS (stacked 100% bar) ----------
    if (selected_band == "All") {
      lab_df <- base_df %>%
        dplyr::count(ur8_name, band_display) %>%
        dplyr::group_by(ur8_name) %>%
        dplyr::mutate(
          prop  = n / sum(n),
          label = scales::percent(prop, accuracy = 1)
        ) %>%
        dplyr::ungroup()
      
      ggplot(base_df, aes(x = ur8_name, fill = band_display)) +
        geom_bar(position = "fill") +
        geom_text(
          data     = lab_df,
          aes(x = ur8_name, y = prop, label = ifelse(prop < 0.05, "", label)),
          position = position_stack(vjust = 0.5),
          size     = 3
        ) +
        coord_flip() +
        labs(
          x        = "Urban–rural classification (UR8)",
          y        = "Proportion of settings",
          title    = title_text,
          subtitle = subtitle,
          fill     = "Quality band"
        ) +
        scale_y_continuous(labels = scales::percent_format()) +
        theme_minimal()
    } else {
      # ---------- CASE 2: ONE BAND (single-colour bars) ----------
      band_to_use <- selected_band
      
      ur8_band <- base_df %>%
        dplyr::group_by(ur8_name) %>%
        dplyr::summarise(
          prop = mean(quality_band == band_to_use, na.rm = TRUE),
          .groups = "drop"
        ) %>%
        dplyr::mutate(
          label = scales::percent(prop, accuracy = 1)
        )
      
      ggplot(ur8_band, aes(x = ur8_name, y = prop)) +
        geom_col(fill = "#f8766d") +
        geom_text(aes(label = label), hjust = -0.1, size = 3.5) +
        coord_flip() +
        labs(
          x     = "Urban–rural classification (UR8)",
          y     = "Share of settings in selected band",
          title = paste0("Share of settings rated ", band_to_use, " across the urban–rural spectrum"),
          subtitle = subtitle
        ) +
        scale_y_continuous(
          limits  = c(0, 1),
          labels  = scales::percent_format(accuracy = 1),
          expand  = expansion(mult = c(0, 0.05))
        ) +
        theme_minimal()
    }
  })
  
  #---------------------------------------------------------------#
  # 3.6 Low-quality (or selected band) table  +  download
  #---------------------------------------------------------------#
  
  # 3.6.1 Single source of truth for the table
  low_table_df <- reactive({
    df <- filtered_data()
    
    # If the user ticks “show only settings with missing quality band”
    if (isTRUE(input$missing_qb)) {
      df <- df %>% filter(is.na(quality_band))
    }
    
    # Flag high deprivation (SIMD 1–2)
    df <- df %>%
      mutate(
        high_deprivation = ifelse(simd2020v2_decile %in% c("1", "2", 1, 2),
                                  "Yes", "No"),
        # make missing bands visible
        quality_band = ifelse(is.na(quality_band),
                              "Missing",
                              as.character(quality_band))
      ) %>%
      select(
        local_authority,
        service_name,
        service_type,
        max_grade,
        quality_band,
        simd2020v2_decile,
        ur8_name,
        high_deprivation
      ) %>%
      arrange(desc(high_deprivation), simd2020v2_decile)
    
    df
  })
  
  # 3.6.2 Render the table
  output$low_table <- DT::renderDataTable({
    
    df <- low_table_df()          # <- use the reactive above
    band_focus <- input$band_choice
    
    caption_text <- if (band_focus == "All") {
      "Currently showing: all quality bands. High deprivation = SIMD deciles 1–2."
    } else {
      paste0("Currently showing band: ", band_focus,
             ". High deprivation = SIMD deciles 1–2.")
    }
    
    dt <- datatable(
      df,
      rownames = FALSE,
      options = list(
        pageLength = 20,
        autoWidth = TRUE,
        dom = "Bfrtip",
        buttons = c("copy", "csv", "excel"),
        columnDefs = list(
          list(
            targets = which(colnames(df) == "high_deprivation") - 1,
            render = JS(
              "function(data, type, row, meta) {",
              "  if(type === 'display') {",
              "    return data === 'Yes' ? ",
              "      '<span style=\"color:red;font-weight:bold\">Yes</span>' : data;",
              "  }",
              "  return data;",
              "}"
            )
          )
        )
      ),
      caption = htmltools::tags$caption(
        style = "caption-side: top; text-align: left;",
        caption_text
      )
    )
    
    dt
  })
  
  # 3.6.3 Download: always export what the table is showing
  output$download_low <- downloadHandler(
    filename = function() {
      paste0("ELC_filtered_settings_", Sys.Date(), ".csv")
    },
    content = function(file) {
      df_out <- low_table_df()
      readr::write_csv(df_out, file)
    })
  # 3.6.4. GIS Output
  output$access_gap_text <- renderText({
    paste(
      "Out of 1,395 SIMD 1–2 datazones in Scotland, 96.8% fall within a 1 km",
      "radius of at least one high-quality ELC setting (inspection grade 5–6).",
      "Only 278 deprived zones remain outside this radius, forming spatial pockets",
      "of inequity in access.",
      "",
      "This suggests that overall provision is well aligned with deprivation,",
      "but there are still priority areas – often at the urban edge and in rural",
      "or remote locations – where additional or alternative forms of ELC",
      "provision could be targeted........... Note: These summary indicators are based on a separate QGIS spatial analysis of the full national dataset (not dynamically recalculated in this dashboard)",
      sep = " "
    )
  })
  
  #---------------------------------------------------------------
  # 3.7 Size vs quality scatter
  #---------------------------------------------------------------
  output$size_plot <- renderPlot({
    df <- filtered_data() %>%
      filter(!is.na(registered_places), !is.na(max_grade)) %>%
      add_band_display()
    
    # Define title dynamically based on input
    title_text <- "Relationship between setting size and quality"
    
    if (input$la_choice != "All" && input$band_choice != "All") {
      title_text <- paste("Relationship between setting size and quality for", input$la_choice, "and", input$band_choice, "quality band")
    } else if (input$la_choice != "All") {
      title_text <- paste("Relationship between setting size and quality for", input$la_choice)
    } else if (input$band_choice != "All") {
      title_text <- paste("Relationship between setting size and quality for", input$band_choice, "quality band")
    }
    
    ggplot(df, aes(x = registered_places, y = max_grade, colour = band_display)) +
      geom_jitter(alpha = 0.5) +
      geom_smooth(method = "loess", se = FALSE, colour = "black") +
      labs(
        x = "Registered places (capacity)",
        y = "Inspection grade (1–6)",
        title = title_text,  # Dynamically set title here
        colour = "Quality band"
      ) +
      theme_minimal()
  })
  # 3.7.1 Dynamic Heading for Filter Table
  #---------------------------------------------------------------
  output$current_filter_heading <- renderText({
    # Build dynamic heading
    heading <- "Currently showing: "
    
    # Append the selected quality band
    if (input$band_choice != "All") {
      heading <- paste0(heading, input$band_choice, " quality band. ")
    } else {
      heading <- paste0(heading, "all quality bands. ")
    }
    
    # Append the local authority info
    if (input$la_choice != "All") {
      heading <- paste0(heading, "Local authority: ", input$la_choice, ".")
    } else {
      heading <- paste0(heading, "All local authorities.")
    }
    
    return(heading)
  })
}

#---------------------------------------------------------------------#
# 4. Run the app
#---------------------------------------------------------------------#

shinyApp(ui = ui, server = server)