# Load required libraries
library(shiny)
library(sf)
library(sp)
library(raster)
library(tidyverse)
library(gstat)
library(automap)
library(spdep)
library(ggplot2)
library(ggcorrplot)
library(ggpubr)
library(viridis)
library(modelsummary)
library(fields)
library(interp)
library(mgcv)
library(RColorBrewer)
library(tmap)
library(knitr)
library(DT)
library(kableExtra)
library(bslib)
library(shinyWidgets)
library(zip)
library(tools)
library(moments)

# UI Definition
ui <- page_fluid(
    theme = bs_theme(
        version = 5,
        bootswatch = "cerulean",
        primary = "#005226",
        "navbar-bg" = "#005226",
        success = "#28a745",
        info = "#17a2b8",
        warning = "#ffc107",
        danger = "#dc3545"
    ),

    # App header with logo
    card(
        style = css(
            background_color = "#005226",
            color = "white",
            padding = "10px",
            margin_bottom = "20px",
            border = "none"
        ),
        card_body(
            div(
                class = "d-flex align-items-center",
                div(
                    style = "width: 200px;",
                    img(src = "logo_educagis.png", height = "80px", width = "auto", style = "max-width: 100%;")
                ),
                div(
                    class = "ms-3 flex-grow-1",
                    h1("Spatial Soil Analysis", style = "color: white; margin: 0;")
                )
            )
        )
    ),

    # Main layout
    layout_sidebar(
        sidebar = sidebar(
            width = 300,
            bg = "#e8f0eb",
            padding = "15px",

            # Data Input Section
            card(
                card_header(h4("Data Input", class = "m-0")),
                card_body(
                    # CSV file input
                    fileInput("csv_file", "Upload CSV Data File",
                             accept = c("text/csv",
                                        "text/comma-separated-values,text/plain",
                                        ".csv")),

                    # Spatial file input
                    fileInput("spatial_file", "Upload Spatial Data File",
                             accept = c(".zip", ".shp", ".geojson", ".gpkg")),

                    # Raster file input
                    fileInput("raster_file", "Upload Covariate Raster (GeoTIFF)",
                             accept = c(".tif", ".tiff")),

                    # Variable selection dropdown (will be dynamically updated)
                    uiOutput("variable_selector"),

                    # Show covariate status
                    conditionalPanel(
                        condition = "input.raster_file != null",
                        div(
                            style = "margin-top: 10px;",
                            textOutput("covariate_status")
                        )
                    )
                )
            ),

            # Kriging settings
            conditionalPanel(
                condition = "output.data_loaded && input.mainTabs == 'Geostatistics'",
                card(
                    card_header(h4("Kriging Settings", class = "m-0")),
                    card_body(
                        sliderInput("grid_resolution",
                                   "Grid Resolution (meters):",
                                   min = 10,
                                   max = 200,
                                   value = 10,
                                   step = 10,
                                   ticks = TRUE)
                    )
                )
            ),

            # Show data status
            conditionalPanel(
                condition = "output.data_loaded",
                card(
                    fill = TRUE,
                    style = "background-color: #d4edda; color: #155724; margin-top: 15px;",
                    card_body(
                        icon("check"),
                        textOutput("data_status")
                    )
                )
            )
        ),

        # Main content area
        navset_card_tab(
            id = "mainTabs",
            # Home tab
            nav_panel(
                title = "Home",
                card(
                    card_header(h3("Welcome to Spatial Soil Analysis App", class = "text-center")),
                    card_body(
                        div(
                            class = "row",
                            div(
                                class = "col-md-12 mb-4",
                                h4("About This App", class = "text-success"),
                                p("This application provides a comprehensive suite of tools for spatial soil data analysis,
                                  visualization, and geostatistical modeling. It is designed for soil scientists,
                                  environmental researchers, agronomists, and GIS specialists who need to analyze
                                  and interpolate soil properties across landscapes."),
                                p("Upload your soil sample data (with coordinates) and a boundary file to get started!")
                            )
                        ),
                        div(
                            class = "row",
                            div(
                                class = "col-md-6",
                                h4("Key Features", class = "text-success"),
                                tags$ul(
                                    tags$li(strong("Data Exploration:"), " Summary statistics, correlation analysis, and distribution plots"),
                                    tags$li(strong("Spatial Visualization:"), " Interactive maps of soil properties"),
                                    tags$li(strong("Spatial Autocorrelation:"), " Moran's I statistics and LISA plots"),
                                    tags$li(strong("Geostatistical Analysis:"), " Variogram modeling and kriging interpolation"),
                                    tags$li(strong("Cross-Validation:"), " Model validation metrics"),
                                    tags$li(strong("Export Options:"), " Download results as GeoTIFF files for use in GIS software")
                                )
                            ),
                            div(
                                class = "col-md-6",
                                h4("How to Use", class = "text-success"),
                                tags$ol(
                                    tags$li("Upload a CSV file with soil data (must include X and Y coordinate columns)"),
                                    tags$li("Upload a spatial boundary file (GeoJSON, or GeoPackage)"),
                                    tags$li("Optionally upload a covariate raster for universal kriging"),
                                    tags$li("Select a soil property variable to analyze"),
                                    tags$li("Navigate through the tabs to explore different analyses"),
                                    tags$li("Download results as needed")
                                )
                            )
                        ),
                        div(
                            class = "row mt-4",
                            div(
                                class = "col-md-12",
                                h4("R Packages Used", class = "text-success"),
                                p("This application leverages several powerful R packages for spatial data analysis:"),
                                div(
                                    class = "row",
                                    div(
                                        class = "col-md-4",
                                        h5("Spatial Data Handling"),
                                        tags$ul(
                                            tags$li(tags$code("sf"), ": Simple Features for spatial vector data"),
                                            tags$li(tags$code("sp"), ": Classes and methods for spatial data"),
                                            tags$li(tags$code("raster"), ": Geographic data analysis and modeling"),
                                            tags$li(tags$code("tmap"), ": Thematic maps for spatial data")
                                        )
                                    ),
                                    div(
                                        class = "col-md-4",
                                        h5("Geostatistics"),
                                        tags$ul(
                                            tags$li(tags$code("gstat"), ": Spatial and spatio-temporal geostatistical modeling"),
                                            tags$li(tags$code("automap"), ": Automatic variogram fitting"),
                                            tags$li(tags$code("spdep"), ": Spatial dependence: weighting schemes and statistics"),
                                            tags$li(tags$code("fields"), ": Tools for spatial data including kriging")
                                        )
                                    ),
                                    div(
                                        class = "col-md-4",
                                        h5("Visualization & UI"),
                                        tags$ul(
                                            tags$li(tags$code("ggplot2"), ": Data visualization"),
                                            tags$li(tags$code("viridis"), ": Color palettes for data visualization"),
                                            tags$li(tags$code("shiny"), ": Interactive web applications"),
                                            tags$li(tags$code("bslib"), ": Bootstrap styling for Shiny apps")
                                        )
                                    )
                                )
                            )
                        )
                    )
                )
            ),
            nav_panel(
                title = "Data Summary",
                # Warning message for missing data
                conditionalPanel(
                    condition = "!output.data_loaded",
                    card(
                        fill = TRUE,
                        style = "background-color: #f8d7da; color: #721c24; margin-bottom: 20px;",
                        card_body(
                            icon("exclamation-triangle"),
                            "Please upload both CSV data (with X, Y coordinates) and a spatial boundary file to begin analysis."
                        )
                    )
                ),

                # Raw data view
                conditionalPanel(
                    condition = "output.data_loaded",
                    card(
                        card_header("Data Preview"),
                        card_body(
                            DTOutput("data_preview")
                        )
                    ),
                    card(
                        card_header("Summary Statistics"),
                        card_body(
                            uiOutput("summary_table")
                        )
                    ),
                    card(
                        card_header("Correlation Plot"),
                        card_body(
                            plotOutput("correlation_plot", height = "500px")
                        )
                    )
                )
            ),

            # Tab 2: EDA (Exploratory Data Analysis)
            nav_panel(
                title = "Exploratory Data Analysis",
                # Warning message for missing data
                conditionalPanel(
                    condition = "!output.data_loaded",
                    card(
                        fill = TRUE,
                        style = "background-color: #f8d7da; color: #721c24; margin-bottom: 20px;",
                        card_body(
                            icon("exclamation-triangle"),
                            "Please upload both CSV data (with X, Y coordinates) and a spatial boundary file to begin analysis."
                        )
                    )
                ),

                # EDA content
                conditionalPanel(
                    condition = "output.data_loaded",
                    layout_column_wrap(
                        width = 1/2,
                        card(
                            card_header("Distribution"),
                            card_body(
                                plotOutput("hist_box")
                            )
                        ),
                        card(
                            card_header("Density Plot"),
                            card_body(
                                plotOutput("density_plot")
                            )
                        )
                    ),
                    card(
                        card_header("Spatial Distribution"),
                        card_body(
                            plotOutput("spatial_map")
                        )
                    )
                )
            ),

            # Tab 3: Spatial Autocorrelation
            nav_panel(
                title = "Spatial Autocorrelation",
                # Warning message for missing data
                conditionalPanel(
                    condition = "!output.data_loaded",
                    card(
                        fill = TRUE,
                        style = "background-color: #f8d7da; color: #721c24; margin-bottom: 20px;",
                        card_body(
                            icon("exclamation-triangle"),
                            "Please upload both CSV data (with X, Y coordinates) and a spatial boundary file to begin analysis."
                        )
                    )
                ),

                # Spatial autocorrelation content
                conditionalPanel(
                    condition = "output.data_loaded",
                    layout_column_wrap(
                        width = 1/2,
                        card(
                            card_header("Moran's I Statistics"),
                            card_body(
                                tableOutput("moran_table")
                            )
                        ),
                        card(
                            card_header("Moran's I Plot"),
                            card_body(
                                plotOutput("moran_plot")
                            )
                        )
                    ),
                    card(
                        card_header("LISA Plot"),
                        card_body(
                            plotOutput("lisa_plot")
                        )
                    )
                )
            ),

            # Tab 4: Geostatistics
            nav_panel(
                title = "Geostatistics",
                # Warning message for missing data
                conditionalPanel(
                    condition = "!output.data_loaded",
                    card(
                        fill = TRUE,
                        style = "background-color: #f8d7da; color: #721c24; margin-bottom: 20px;",
                        card_body(
                            icon("exclamation-triangle"),
                            "Please upload both CSV data (with X, Y coordinates) and a spatial boundary file to begin analysis."
                        )
                    )
                ),

                # Geostatistics content
                conditionalPanel(
                    condition = "output.data_loaded",
                    layout_column_wrap(
                        width = 1/2,
                        card(
                            card_header("Empirical Variogram"),
                            card_body(
                                plotOutput("variogram")
                            )
                        ),
                        card(
                            card_header("Fitted Variogram"),
                            card_body(
                                plotOutput("fitted_variogram")
                            )
                        )
                    ),
                    card(
                        card_header("Variogram Parameters"),
                        card_body(
                            tableOutput("variogram_params")
                        )
                    ),
                    layout_column_wrap(
                        width = 1/2,
                        card(
                            card_header("Kriging Prediction Map"),
                            card_body(
                                plotOutput("kriging_map")
                            )
                        ),
                        card(
                            card_header("Kriging Variance Map"),
                            card_body(
                                plotOutput("kriging_variance")
                            )
                        )
                    ),
                    card(
                        card_header("Cross-Validation Results"),
                        card_body(
                            tableOutput("cv_metrics")
                        )
                    ),
                    card(
                        card_header("Download Results"),
                        card_body(
                            layout_column_wrap(
                                width = 1/2,
                                downloadButton("download_kriging_tif", "Download Prediction Raster (GeoTIFF)",
                                               class = "btn-success w-100 mb-2"),
                                downloadButton("download_variance_tif", "Download Variance Raster (GeoTIFF)",
                                               class = "btn-success w-100 mb-2")
                            ),
                            p("Download the kriging prediction and variance maps as GeoTIFF files for use in GIS software.",
                              style = "margin-top: 10px; color: #666;")
                        )
                    )
                )
            )
        )
    )
)

# Server Definition
server <- function(input, output, session) {
    # Create temp directory for unzipping files
    temp_dir <- tempdir()

    # Reactive values to store data
    rv <- reactiveValues(
        csv_data = NULL,
        csv_file_loaded = FALSE,
        spatial_data = NULL,
        spatial_file_loaded = FALSE,
        raster_data = NULL,
        raster_file_loaded = FALSE,
        data_loaded = FALSE,
        numeric_cols = NULL,
        coord_cols = NULL,
        crs = NULL,
        has_error = FALSE,
        error_message = "",
        debug_message = "",  # Added for debugging
        summary_data = NULL
    )

    # Process CSV file upload
    observeEvent(input$csv_file, {
        req(input$csv_file)

        # Reset error state
        rv$has_error <- FALSE
        rv$error_message <- ""

        tryCatch({
            # Read the CSV file
            csv_data <- read.csv(input$csv_file$datapath)

            # Check if X and Y columns exist
            if(!("X" %in% colnames(csv_data)) || !("Y" %in% colnames(csv_data))) {
                rv$has_error <- TRUE
                rv$error_message <- "Error: CSV file must contain 'X' and 'Y' columns for spatial coordinates."
                rv$csv_file_loaded <- FALSE
                return()
            }

            # Store data and set loaded flag
            rv$csv_data <- csv_data
            rv$csv_file_loaded <- TRUE

            # Identify numeric columns (excluding X, Y coordinates)
            rv$numeric_cols <- names(csv_data)[sapply(csv_data, is.numeric)]
            rv$numeric_cols <- setdiff(rv$numeric_cols, c("X", "Y"))

            # Store coordinate column names
            rv$coord_cols <- c("X", "Y")

            # Update variable choices
            updateSelectInput(session, "variable",
                              choices = rv$numeric_cols,
                              selected = rv$numeric_cols[1])

        }, error = function(e) {
            rv$has_error <- TRUE
            rv$error_message <- paste("Error reading CSV file:", e$message)
            rv$csv_file_loaded <- FALSE
        })

        # Check if both files are loaded
        rv$data_loaded <- rv$csv_file_loaded && rv$spatial_file_loaded
    })

    # Process spatial file upload
    observeEvent(input$spatial_file, {
        req(input$spatial_file)

        # Reset error state
        rv$has_error <- FALSE
        rv$error_message <- ""

        tryCatch({
            file_path <- input$spatial_file$datapath
            file_ext <- tools::file_ext(input$spatial_file$name)

            poly_data <- NULL

            if(file_ext == "zip") {
                # Handle zipped shapefile
                # Unzip to temp directory
                unzip(file_path, exdir = temp_dir)

                # Find .shp file in the unzipped directory
                shp_files <- list.files(temp_dir, pattern = "\\.shp$", full.names = TRUE)

                if(length(shp_files) == 0) {
                    rv$has_error <- TRUE
                    rv$error_message <- "Error: No .shp file found in the uploaded zip file."
                    rv$spatial_file_loaded <- FALSE
                    return()
                }

                # Read the first shapefile found
                poly_data <- st_read(shp_files[1], quiet = TRUE)
            } else if(file_ext == "geojson") {
                # Handle GeoJSON
                poly_data <- st_read(file_path, quiet = TRUE)
            } else if(file_ext == "gpkg") {
                # Handle GeoPackage
                poly_data <- st_read(file_path, quiet = TRUE)
            } else if(file_ext == "shp") {
                # Handle direct shapefile (not recommended but included for completeness)
                poly_data <- st_read(file_path, quiet = TRUE)
            } else {
                rv$has_error <- TRUE
                rv$error_message <- "Error: Unsupported spatial file format. Please upload a zipped shapefile, GeoJSON, or GeoPackage."
                rv$spatial_file_loaded <- FALSE
                return()
            }

            # Store spatial data and CRS
            rv$spatial_data <- poly_data
            rv$crs <- st_crs(poly_data)
            rv$spatial_file_loaded <- TRUE

        }, error = function(e) {
            rv$has_error <- TRUE
            rv$error_message <- paste("Error processing spatial file:", e$message)
            rv$spatial_file_loaded <- FALSE
        })

        # Check if both files are loaded
        rv$data_loaded <- rv$csv_file_loaded && rv$spatial_file_loaded
    })

    # Process raster file upload
    observeEvent(input$raster_file, {
        req(input$raster_file)

        # Reset error state
        rv$has_error <- FALSE
        rv$error_message <- ""

        tryCatch({
            # Read the raster file
            raster_data <- raster::raster(input$raster_file$datapath)

            # Store raster data
            rv$raster_data <- raster_data
            rv$raster_file_loaded <- TRUE

        }, error = function(e) {
            rv$has_error <- TRUE
            rv$error_message <- paste("Error reading raster file:", e$message)
            rv$raster_file_loaded <- FALSE
        })
    })

    # Create soil data reactively
    soil_data <- reactive({
        req(rv$csv_data, rv$csv_file_loaded)

        # Select only numeric columns for analysis (excluding coordinates)
        rv$csv_data |> dplyr::select(all_of(rv$numeric_cols))
    })

    # Create spatial data reactively
    spatial_data <- reactive({
        req(rv$csv_data, rv$spatial_data, rv$csv_file_loaded, rv$spatial_file_loaded)

        # Convert points data to sf object
        points_sf <- st_as_sf(rv$csv_data, coords = rv$coord_cols, crs = rv$crs)

        # Return list with polygon and points
        list(poly = rv$spatial_data, points = points_sf)
    })

    # Render covariate status
    output$covariate_status <- renderText({
        if(rv$raster_file_loaded) {
            "✓ Covariate raster loaded successfully. Universal Kriging will be used."
        } else {
            "ℹ No covariate raster loaded. Ordinary Kriging will be used."
        }
    })

    # Output variable selector UI
    output$variable_selector <- renderUI({
        req(rv$numeric_cols)

        selectInput("variable", "Select Variable for Analysis:",
                   choices = rv$numeric_cols,
                   selected = rv$numeric_cols[1])
    })

    # Data status output
    output$data_status <- renderText({
        if(rv$has_error) {
            return(rv$error_message)
        } else if(rv$data_loaded) {
            return(paste("Data loaded successfully. Available variables:", length(rv$numeric_cols)))
        } else {
            return("Waiting for data...")
        }
    })

    # Set data_loaded flag for conditional panels
    output$data_loaded <- reactive({
        return(rv$data_loaded)
    })
    outputOptions(output, "data_loaded", suspendWhenHidden = FALSE)

    # Data preview output
    output$data_preview <- renderDT({
        req(rv$csv_data, rv$csv_file_loaded)

        # Show the raw data with all columns
        datatable(rv$csv_data,
                 options = list(
                     pageLength = 5,
                     scrollX = TRUE,
                     scrollY = "300px"
                 ),
                 rownames = FALSE)
    })

    # Summary statistics with kableExtra
    output$summary_table <- renderUI({
        req(soil_data())

        # Create a summary table with proper formatting
        summary_data <- soil_data() %>%
            summarise(across(everything(),
                            list(
                                Min = ~min(., na.rm = TRUE),
                                Q1 = ~quantile(., 0.25, na.rm = TRUE),
                                Median = ~median(., na.rm = TRUE),
                                Mean = ~mean(., na.rm = TRUE),
                                Q3 = ~quantile(., 0.75, na.rm = TRUE),
                                Max = ~max(., na.rm = TRUE),
                                SD = ~sd(., na.rm = TRUE),
                                Skewness = ~skewness(., na.rm = TRUE),
                                Kurtosis = ~kurtosis(., na.rm = TRUE)
                            ))) %>%
            pivot_longer(cols = everything(),
                        names_to = c("Variable", "Statistic"),
                        names_sep = "_") %>%
            pivot_wider(names_from = "Statistic", values_from = "value")

        # Store the summary data in a reactive value for download
        rv$summary_data <- summary_data

        # Format the table using kableExtra
        kable_html <- kable(summary_data, format = "html", digits = 3) %>%
            kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"),
                          full_width = TRUE,
                          font_size = 14) %>%
            column_spec(1, bold = TRUE, color = "#005226") %>%
            row_spec(0, bold = TRUE, color = "white", background = "#005226")

        # Add download button and table
        tagList(
            div(
                style = "margin-bottom: 15px;",
                downloadButton("download_summary", "Download Summary Statistics (CSV)",
                              class = "btn-sm btn-success")
            ),
            HTML(kable_html)
        )
    })

    # Download handler for summary statistics
    output$download_summary <- downloadHandler(
        filename = function() {
            paste0("summary_statistics_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv")
        },
        content = function(file) {
            req(rv$summary_data)
            write.csv(rv$summary_data, file, row.names = FALSE)
        }
    )

    output$correlation_plot <- renderPlot({
        req(soil_data())

        if(ncol(soil_data()) < 2) {
            # If there's only one numeric column, show a message instead
            plot(0, 0, type="n", axes=FALSE, xlab="", ylab="")
            text(0, 0, "Need at least 2 numeric variables for correlation plot", col="#721c24")
            return()
        }

        cor_matrix <- cor(soil_data(), use = "pairwise.complete.obs")
        p_matrix <- cor_pmat(soil_data(), use = "pairwise.complete.obs")

        ggcorrplot(cor_matrix,
                  p.mat = p_matrix,
                  colors = c("#005226", "white", "#E46726"),
                  lab = TRUE,
                  type = "lower") +
            ggtitle("Correlation Matrix") +
            theme_minimal() +
            theme(
                plot.background = element_rect(fill = "white", color = NA),
                panel.background = element_rect(fill = "white", color = NA),
                panel.grid.major = element_line(color = "#e6e6e6"),
                panel.grid.minor = element_line(color = "#f0f0f0"),
                text = element_text(color = "#333"),
                plot.title = element_text(color = "#005226", hjust = 0.5, face = "bold")
            )
    })

    # Distribution plots
    output$hist_box <- renderPlot({
        req(soil_data(), input$variable)

        tryCatch({
            # Create a data frame for plotting
            plot_data <- data.frame(value = soil_data()[[input$variable]])

            # Create a layout with 2 plots side by side
            par(mfrow = c(1, 2), bg = "white", col.axis = "#333", col.lab = "#333", col.main = "#333")

            # Histogram (left)
            hist(soil_data()[[input$variable]],
                 xlab = input$variable,
                 ylab = "Frequency",
                 main = "Histogram",
                 col = "#005226",
                 border = "white")

            # Create violin plot (right) using ggplot2
            p <- ggplot(plot_data, aes(x = "", y = value)) +
                geom_violin(fill = "#005226", color = "black", alpha = 0.8, trim = FALSE) +
                geom_boxplot(width = 0.1, fill = "#E46726", color = "black", alpha = 0.7,
                             outlier.color = "red", outlier.size = 2) +
                labs(title = "Violin Plot",
                     x = "",
                     y = input$variable) +
                theme_minimal() +
                theme(
                    plot.background = element_rect(fill = "white", color = NA),
                    panel.background = element_rect(fill = "white", color = NA),
                    panel.grid.major.y = element_line(color = "#e6e6e6"),
                    panel.grid.major.x = element_blank(),
                    panel.grid.minor = element_blank(),
                    text = element_text(color = "#333"),
                    plot.title = element_text(color = "#005226", hjust = 0.5, face = "bold"),
                    axis.text.x = element_blank()
                )

            # Print the histogram (already done with par)
            # Now print the ggplot in the second panel
            print(p, vp = grid::viewport(x = 0.75, y = 0.5, width = 0.5, height = 1))

        }, error = function(e) {
            par(bg = "white")
            plot(0, 0, type = "n", axes = FALSE, xlab = "", ylab = "")
            text(0, 0, paste("Error generating plot:", e$message), col = "#721c24")
        })
    })

    output$density_plot <- renderPlot({
        req(soil_data(), input$variable)

        tryCatch({
            # Keep the original density plot
            ggdensity(soil_data(), x = input$variable, fill = "#005226",
                     title = paste("Density Plot:", input$variable)) +
                scale_x_continuous(limits = c(min(soil_data()[[input$variable]], na.rm = TRUE),
                                            max(soil_data()[[input$variable]], na.rm = TRUE))) +
                stat_overlay_normal_density(color = "#E46726", linetype = "dashed") +
                theme_minimal() +
                theme(
                    plot.background = element_rect(fill = "white", color = NA),
                    panel.background = element_rect(fill = "white", color = NA),
                    panel.grid.major = element_line(color = "#e6e6e6"),
                    panel.grid.minor = element_line(color = "#f0f0f0"),
                    text = element_text(color = "#333"),
                    plot.title = element_text(color = "#005226", hjust = 0.5, face = "bold")
                )
        }, error = function(e) {
            ggplot() +
                annotate("text", x = 0, y = 0, label = paste("Error generating plot:", e$message), color = "#721c24") +
                theme_void()
        })
    })

    output$spatial_map <- renderPlot({
        req(spatial_data(), input$variable)

        tryCatch({
            # Get file name without extension for title
            site_name <- "Site"
            if(!is.null(input$spatial_file)) {
                site_name <- tools::file_path_sans_ext(input$spatial_file$name)
            }

            ggplot() +
                geom_sf(data = spatial_data()$poly, fill = "#f5f8fa", color = "#2c3e50") +
                geom_sf(data = spatial_data()$points, size = 3, shape = 16, aes_string(color = input$variable)) +
                scale_color_viridis_c(alpha = .8, option = "turbo") +
                ggtitle(paste("Values of", input$variable), subtitle = site_name) +
                coord_sf() +
                theme_minimal() +
                theme(
                    plot.background = element_rect(fill = "white", color = NA),
                    panel.background = element_rect(fill = "white", color = NA),
                    panel.grid.major = element_line(color = "#e6e6e6"),
                    panel.grid.minor = element_line(color = "#f0f0f0"),
                    text = element_text(color = "#333"),
                    plot.title = element_text(color = "#005226", hjust = 0.5, face = "bold"),
                    plot.subtitle = element_text(hjust = 0.5)
                )
        }, error = function(e) {
            ggplot() +
                annotate("text", x = 0, y = 0, label = paste("Error generating map:", e$message), color = "#721c24") +
                theme_void()
        })
    })

    # Spatial Analysis outputs
    # Create spatial weights
    spatial_weights <- reactive({
        req(spatial_data(), input$variable)

        tryCatch({
            # Convert sf to sp for autocorrelation analysis
            sp_points <- as(spatial_data()$points, "Spatial")

            # Create a neighbor list based on the spatial points
            coords <- coordinates(sp_points)

            # Define neighbors - adjust k based on number of points
            k <- min(5, nrow(coords) - 1)  # Use at most 5 neighbors or n-1 if fewer points
            k_nearest <- knn2nb(knearneigh(coords, k = k))
            w_knn <- nb2listw(k_nearest, style = "W")

            list(sp_points = sp_points, weights = w_knn)
        }, error = function(e) {
            # Return NULL on error, which will be handled by dependent outputs
            return(NULL)
        })
    })

    output$moran_table <- renderTable({
        req(spatial_weights(), input$variable)

        tryCatch({
            # Calculate Moran's I for the selected variable
            moran_result <- moran.test(spatial_weights()$sp_points[[input$variable]],
                                      spatial_weights()$weights)

            # Create a data frame to display Moran's I results
            moran_df <- data.frame(
                Metric = c("Moran's I", "Expected I", "p-value", "z-score"),
                Value = c(
                    round(moran_result$estimate[1], 6),
                    round(moran_result$estimate[2], 6),
                    round(moran_result$p.value, 6),
                    round(moran_result$statistic, 6)
                )
            )

            moran_df
        }, error = function(e) {
            # Return a data frame with error message
            data.frame(
                Metric = "Error",
                Value = paste("Could not calculate Moran's I:", e$message)
            )
        })
    }, striped = TRUE, hover = TRUE, bordered = TRUE)

    output$moran_plot <- renderPlot({
        req(spatial_weights(), input$variable)

        tryCatch({
            # Set theme for base R plots
            par(bg = "white", col.axis = "#333", col.lab = "#333", col.main = "#333")

            # Generate a Moran scatter plot
            moran.plot(spatial_weights()$sp_points[[input$variable]], spatial_weights()$weights,
                      xlab = paste(input$variable, "values"),
                      ylab = paste("Spatially lagged", input$variable),
                      main = "Moran Scatter Plot",
                      col = "#005226",
                      pch = 19)
        }, error = function(e) {
            par(bg = "white")
            plot(0, 0, type="n", axes=FALSE, xlab="", ylab="")
            text(0, 0, paste("Error generating Moran plot:", e$message), col="#721c24")
        })
    })

    output$lisa_plot <- renderPlot({
        req(spatial_weights(), spatial_data(), input$variable)

        tryCatch({
            # Calculate local Moran's I (LISA)
            lisa <- localmoran(spatial_weights()$sp_points[[input$variable]], spatial_weights()$weights)
            sp_points <- spatial_weights()$sp_points
            sp_points$LISA <- lisa[, 1]  # Local Moran's I values
            sp_points$LISA_p <- lisa[, 5]  # p-values

            # Convert back to sf for mapping
            lisa_sf <- st_as_sf(sp_points)

            # Map the local Moran's I values
            ggplot() +
                geom_sf(data = spatial_data()$poly, fill = "#f5f8fa", color = "#2c3e50") +
                geom_sf(data = lisa_sf, aes(color = LISA), size = 4) +
                scale_color_viridis_c(
                    option = "plasma",
                    name = "Local Moran's I"
                ) +
                labs(
                    title = "Local Indicators of Spatial Association (LISA)",
                    subtitle = paste("For variable:", input$variable)
                ) +
                theme_minimal() +
                theme(
                    plot.background = element_rect(fill = "white", color = NA),
                    panel.background = element_rect(fill = "white", color = NA),
                    panel.grid.major = element_line(color = "#e6e6e6"),
                    panel.grid.minor = element_line(color = "#f0f0f0"),
                    text = element_text(color = "#333"),
                    legend.background = element_rect(fill = "white"),
                    legend.key = element_rect(fill = "white"),
                    plot.title = element_text(color = "#005226", hjust = 0.5, face = "bold"),
                    plot.subtitle = element_text(hjust = 0.5)
                )
        }, error = function(e) {
            ggplot() +
                annotate("text", x = 0, y = 0, label = paste("Error generating LISA plot:", e$message), color = "#721c24") +
                theme_void()
        })
    })

    # Geostatistics outputs
    variogram_data <- reactive({
        req(spatial_data(), input$variable)

        tryCatch({
            # Generate the empirical variogram
            var_ok1 <- gstat::variogram(
                as.formula(paste(input$variable, "~1")),
                as(spatial_data()$points, "Spatial"),
                cloud = FALSE
            )

            # Automatic variogram fitting using automap
            auto_fit <- autofitVariogram(
                as.formula(paste(input$variable, "~1")),
                as(spatial_data()$points, "Spatial")
            )

            list(empirical = var_ok1, fitted = auto_fit)
        }, error = function(e) {
            return(NULL)
        })
    })

    output$variogram <- renderPlot({
        req(variogram_data())

        tryCatch({
            par(bg = "white", col.axis = "#333", col.lab = "#333", col.main = "#333")
            plot(variogram_data()$empirical, col = "#005226", pch = 19,
                 main = paste("Empirical Variogram -", input$variable))
        }, error = function(e) {
            par(bg = "white")
            plot(0, 0, type="n", axes=FALSE, xlab="", ylab="")
            text(0, 0, paste("Error generating variogram:", e$message), col="#721c24")
        })
    })

    output$fitted_variogram <- renderPlot({
        req(variogram_data())

        tryCatch({
            auto_fit <- variogram_data()$fitted

            # Plot the empirical variogram with the automatically fitted model
            par(bg = "white", col.axis = "#333", col.lab = "#333", col.main = "#333")
            plot(auto_fit$exp_var, auto_fit$var_model,
                 col = "#005226",
                 xlab = "Distance",
                 ylab = "Semivariance",
                 main = paste("Best fitted model:", auto_fit$var_model$model[2]),
                 pch = 19,
                 col.line = "#E46726")
        }, error = function(e) {
            par(bg = "white")
            plot(0, 0, type="n", axes=FALSE, xlab="", ylab="")
            text(0, 0, paste("Error generating fitted variogram:", e$message), col="#721c24")
        })
    })

    output$variogram_params <- renderTable({
        req(variogram_data())

        tryCatch({
            auto_fit <- variogram_data()$fitted
            model <- auto_fit$var_model

            # Extract parameters from the model
            nugget <- model$psill[1]
            partial_sill <- model$psill[2]
            total_sill <- sum(model$psill)
            range <- model$range[2]
            model_type <- model$model[2]

            # Create a data frame with the parameters
            params_df <- data.frame(
                Parameter = c("Model Type", "Nugget", "Partial Sill", "Total Sill", "Range (distance)",
                              "Nugget/Sill Ratio (%)", "Spatial Dependence"),
                Value = c(model_type,
                         round(nugget, 4),
                         round(partial_sill, 4),
                         round(total_sill, 4),
                         round(range, 4),
                         round((nugget/total_sill)*100, 2),
                         ifelse((nugget/total_sill)*100 < 25, "Strong",
                                ifelse((nugget/total_sill)*100 < 75, "Moderate", "Weak")))
            )

            params_df
        }, error = function(e) {
            # Return a data frame with error message
            data.frame(
                Parameter = "Error",
                Value = paste("Could not calculate variogram parameters:", e$message)
            )
        })
    }, striped = TRUE, hover = TRUE, bordered = TRUE)

    kriging_results <- reactive({
        req(spatial_data(), variogram_data(), input$variable)

        tryCatch({
            # Convert sf to sp and ensure XY dimensions only
            data_soil <- st_zm(spatial_data()$points) %>% # Drop Z dimension if present
                as("Spatial")
            sp_poly <- st_zm(spatial_data()$poly) %>% # Drop Z dimension if present
                as("Spatial")

            # Limit domain of interpolation to polygon
            data_soil@bbox <- sp_poly@bbox

            # Use selected grid resolution in meters
            grid_resolution <- input$grid_resolution

            # Calculate grid dimensions from the desired resolution in meters
            x_range <- sp_poly@bbox[1,2] - sp_poly@bbox[1,1]
            y_range <- sp_poly@bbox[2,2] - sp_poly@bbox[2,1]

            # Calculate number of cells in each dimension
            ncols <- ceiling(x_range / grid_resolution)
            nrows <- ceiling(y_range / grid_resolution)

            # Create a raster with the desired resolution
            template_raster <- raster(
                ncols = ncols,
                nrows = nrows,
                xmn = sp_poly@bbox[1,1],
                xmx = sp_poly@bbox[1,2],
                ymn = sp_poly@bbox[2,1],
                ymx = sp_poly@bbox[2,2]
            )
            raster::crs(template_raster) <- raster::crs(data_soil)

            # Create a SpatialPixelsDataFrame grid from the raster
            grd1 <- as(template_raster, "SpatialPixelsDataFrame")

            # Get variogram model
            var_model <- variogram_data()$fitted$var_model

            # Prepare formula for kriging
            kriging_formula <- if(rv$raster_file_loaded) {
                # Extract covariate values at sample points
                covariate_values <- raster::extract(rv$raster_data, data_soil)
                data_soil$covariate <- covariate_values

                # Add covariate values to prediction grid
                grd1$covariate <- raster::extract(rv$raster_data, grd1)

                # Use Universal Kriging with covariate
                as.formula(paste(input$variable, "~ covariate"))
            } else {
                # Use Ordinary Kriging without covariate
                as.formula(paste(input$variable, "~1"))
            }

            # Perform kriging with explicit error handling
            dat.krig <- tryCatch({
                krige(
                    kriging_formula,
                    data_soil,
                    grd1,
                    var_model
                )
            }, error = function(e) {
                rv$has_error <- TRUE
                rv$error_message <- paste("Kriging error:", e$message)
                return(NULL)
            })

            if (is.null(dat.krig)) {
                return(NULL)
            }

            # Convert to raster
            rast_krig <- raster(dat.krig, layer = 'var1.pred')
            rast_krig <- raster::mask(rast_krig, sp_poly)

            # Variance raster
            r1 <- raster(dat.krig, layer = 'var1.var')
            r1 <- raster::mask(r1, sp_poly)

            list(prediction = rast_krig, variance = r1, data_points = data_soil)
        }, error = function(e) {
            rv$has_error <- TRUE
            rv$error_message <- paste("Kriging results error:", e$message)
            rv$debug_message <- paste("Error in kriging_results():", e$message)
            return(NULL)
        })
    })

    output$kriging_map <- renderPlot({
        req(kriging_results())

        tryCatch({
            # Check if we have valid prediction data
            if (is.null(kriging_results()$prediction)) {
                plot(0, 0, type="n", axes=FALSE, xlab="", ylab="")
                text(0, 0, "No prediction data available", col="#721c24")
                return()
            }

            par(bg = "white", col.axis = "#333", col.lab = "#333", col.main = "#333")
            # Use raster::plot for more reliable plotting
            raster::plot(kriging_results()$prediction, col = viridis::viridis(100, option = "turbo"))
            points(coordinates(kriging_results()$data_points), col = 'red', pch = 16, cex = 1)
            title(main = paste("Kriging Prediction Map -", input$variable))
        }, error = function(e) {
            par(bg = "white")
            plot(0, 0, type="n", axes=FALSE, xlab="", ylab="")
            text(0, 0, paste("Error generating kriging map:", e$message), col="#721c24")
        })
    })

    output$kriging_variance <- renderPlot({
        req(kriging_results())

        tryCatch({
            # Check if we have valid variance data
            if (is.null(kriging_results()$variance)) {
                plot(0, 0, type="n", axes=FALSE, xlab="", ylab="")
                text(0, 0, "No variance data available", col="#721c24")
                return()
            }

            par(bg = "white", col.axis = "#333", col.lab = "#333", col.main = "#333")
            # Use raster::plot for more reliable plotting
            raster::plot(kriging_results()$variance, col = viridis::viridis(100, option = "plasma"))
            points(coordinates(kriging_results()$data_points), pch = 16, col = 'black', cex = 1)
            title(main = paste("Kriging Variance Map -", input$variable))
        }, error = function(e) {
            par(bg = "white")
            plot(0, 0, type="n", axes=FALSE, xlab="", ylab="")
            text(0, 0, paste("Error generating variance map:", e$message), col="#721c24")
        })
    })

    cross_validation_results <- reactive({
        req(spatial_data(), variogram_data(), input$variable)

        tryCatch({
            # Calculate number of folds based on data points (min 2, max 10)
            n_points <- nrow(spatial_data()$points)
            n_folds <- min(5, max(2, floor(n_points / 5)))

            # Convert sf to sp - use the renamed variables
            data_soil <- as(spatial_data()$points, "Spatial")

            # Perform cross-validation of the model
            cv_result <- krige.cv(
                as.formula(paste(input$variable, "~1")),
                data_soil,
                variogram_data()$fitted$var_model,
                nfold = n_folds
            )

            # Create a data frame with the observed and predicted values
            cv_data <- data.frame(
                observed = cv_result@data$observed,
                predicted = cv_result@data$var1.pred
            )

            # Calculate evaluation metrics
            rmse <- sqrt(mean((cv_data$observed - cv_data$predicted)^2, na.rm = TRUE))
            mae <- mean(abs(cv_data$observed - cv_data$predicted), na.rm = TRUE)
            r_squared <- cor(cv_data$observed, cv_data$predicted, use = "pairwise.complete.obs")^2

            list(
                cv_data = cv_data,
                metrics = data.frame(
                    Metric = c("RMSE", "MAE", "R²", "Folds"),
                    Value = c(round(rmse, 4), round(mae, 4), round(r_squared, 4), n_folds)
                )
            )
        }, error = function(e) {
            return(NULL)
        })
    })

    output$cross_validation <- renderPlot({
        req(cross_validation_results())

        tryCatch({
            cv_data <- cross_validation_results()$cv_data
            r_squared <- cor(cv_data$observed, cv_data$predicted, use = "pairwise.complete.obs")^2

            ggplot(cv_data, aes(x = observed, y = predicted)) +
                geom_point(color = "#005226", size = 3) +
                geom_abline(intercept = 0, slope = 1, color = "#E46726", linetype = "dashed", size = 1) +
                labs(
                    x = "Observed Values",
                    y = "Predicted Values",
                    title = "Cross-Validation: Observed vs. Predicted",
                    subtitle = paste("R² =", round(r_squared, 4))
                ) +
                theme_minimal() +
                theme(
                    plot.background = element_rect(fill = "white", color = NA),
                    panel.background = element_rect(fill = "white", color = NA),
                    panel.grid.major = element_line(color = "#e6e6e6"),
                    panel.grid.minor = element_line(color = "#f0f0f0"),
                    text = element_text(color = "#333"),
                    plot.title = element_text(color = "#005226", hjust = 0.5, face = "bold"),
                    plot.subtitle = element_text(hjust = 0.5)
                )
        }, error = function(e) {
            ggplot() +
                annotate("text", x = 0, y = 0, label = paste("Error generating cross-validation plot:", e$message), color = "#721c24") +
                theme_void()
        })
    })

    output$cv_metrics <- renderTable({
        req(cross_validation_results())

        tryCatch({
            cross_validation_results()$metrics
        }, error = function(e) {
            # Return a data frame with error message
            data.frame(
                Metric = "Error",
                Value = paste("Could not calculate cross-validation metrics:", e$message)
            )
        })
    }, striped = TRUE, hover = TRUE, bordered = TRUE)

    # Handle downloading kriging result as TIF file
    output$download_kriging_tif <- downloadHandler(
        filename = function() {
            paste0("kriging_", input$variable, "_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".tif")
        },
        content = function(file) {
            # Ensure we have kriging results
            req(kriging_results())

            tryCatch({
                # Get the kriging prediction raster
                rast_krig <- kriging_results()$prediction

                # Write the raster to a GeoTIFF file
                raster::writeRaster(rast_krig, file, format = "GTiff", overwrite = TRUE)
            }, error = function(e) {
                # Log the error
                message(paste("Error saving kriging raster:", e$message))
                # Return a simple error message file if there's a problem
                writeLines(paste("Error generating TIF file:", e$message), file)
            })
        },
        contentType = "image/tiff"
    )

    # Add download handler for variance raster
    output$download_variance_tif <- downloadHandler(
        filename = function() {
            paste0("kriging_variance_", input$variable, "_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".tif")
        },
        content = function(file) {
            req(kriging_results())

            # Extract the variance raster from kriging results
            variance_raster <- kriging_results()$variance

            # Write to file
            writeRaster(variance_raster, file, format = "GTiff", overwrite = TRUE)
        }
    )
}

# Run the app
shinyApp(ui = ui, server = server)