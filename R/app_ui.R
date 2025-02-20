#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_ui <- function(request) {
  tagList(
    # Your application UI logic
    fluidPage(
      titlePanel("Isotope"),
      sidebarLayout(
        sidebarPanel(
          # Write text to descripe the input file needed
          p("Please upload a file with mz and rt columns. It does not matter what you call the columns as you select them below. The file should be a semicolon delimited csv file"),
          p(""),
          p("Please be aware of the locale: If the data have been saved by a danish Excel program, you should choose danish. If you don't know what to select, just try and import to see if the columns are parsed correctly"),
          # Make slider to select the locale of the file
          shinyWidgets::prettyToggle(
            inputId = "locale",
            label_on = "Danish",
            label_off = "English",
            status_off = 'info',
            value = TRUE
          ),

          # select file
          fileInput("file", "Choose a file"),

          p("Here you select the columns for the mz and rt variables"),
          # select mz column
          selectInput("mz", "Select mz column", choices = NULL),
          # select rt column
          selectInput("rt", "Select rt column", choices = NULL),

          p("Here you select the isotope mass difference, mass threshold and RT threshold used for computing the isotopes"),
          # Input numerical values for the isotope variable. Multiple values ideally,

          textInput("isotope", "Enter isotope mass difference(s) separated by commas"),
          p("This should be a list of your numbers separated by space (no comma) and having no NAs"),
          verbatimTextOutput("parsed_isotopes"),

          # Mass threshold
          numericInput("mass_threshold", "Select mass threshold", value = 0.005),

          # RT threshold
          numericInput("rt_threshold", "Select RT threshold (seconds)", value = 2),

          # Select compute isotopes!
          actionButton("compute", "Compute isotopes!"),

          # Save the computed dataset
          downloadButton("download", "Save the result")
        ),
        mainPanel(
          h3("Imported Data and Results"),
          p("This pane displays the first six and ten columns and rows in the imported data and the computed isotopes. When saving the results all info is saved."),
          textOutput("data_dim"),
          tableOutput("data"),
          textOutput("isotope_data_dim"),
          tableOutput("peaks")
        )
      )
    )
  )
}

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  add_resource_path(
    "www",
    app_sys("app/www")
  )

  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "isotope"
    )

  )
}
