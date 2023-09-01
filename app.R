rm(list=ls())

library(jsonlite)
library(dplyr)
library(tidyr)
#library(shiny)
library(shinythemes)
library(grid)
library(gridExtra)
library(metafor)
library(ggplot2)
library(magrittr)
library(igraph)
library(reshape2)
library(psych)


# Replace 'path/to/your/file.json' with the actual file path
json_file <- "C:\\Users\\roych\\Downloads\\ANY\\ANALYSIS\\user.json"

#json_file <-"https://turbbo.shef.ac.uk//user.json"
# Read the JSON file with multiple objects
data1 <- stream_in(file(json_file))

#View(data1)
extracted_data1 <- data1 %>% select(-c(correlations, samples))
extracted_data2 <- data1 %>% select(correlations)
extracted_data3 <- data1 %>% select(samples)


#######################Replication of data###############################

combined_result <- data.frame()  # Initialize an empty data frame

comma_counts <- data.frame()
# Loop through each row and count commas in the 'behaviour' column
for (i in 1:nrow(data1)) {
  length <- nchar(data1$behaviours[i]) - nchar(gsub(",", "", data1$behaviours[i]))
  comma_counts <- rbind(comma_counts, data.frame(row = i, length = length+1))
  
}

# Add the 'length' column to the extracted_data1 data frame
extracted_data1$length <- comma_counts$length
#View(extracted_data1)



for (i in 1:nrow(comma_counts)) {
  row <- comma_counts[i, ]
  replication_factor <- row$length
  
  replicated_row <- extracted_data1[i, ]
  
  if (replication_factor == 2) {
    replicated_row <- replicated_row %>% slice(rep(1:n(), each = 1))
  } else if (replication_factor == 3) {
    replicated_row <- replicated_row %>% slice(rep(1:n(), each = 3))
  } else if (replication_factor == 4) {
    replicated_row <- replicated_row %>% slice(rep(1:n(), each = 6))
  }
  
  # Add a column to store the original row index
  replicated_row <- cbind(original_index = i, replicated_row)
  
  # Append the replicated rows to the combined_result data frame
  combined_result <- bind_rows(combined_result, replicated_row)
}

#View(combined_result)

combined_result1=cbind(combined_result[2],combined_result[3],combined_result[4],combined_result[5],combined_result[6],combined_result[7],combined_result[8],combined_result[9],combined_result[10],combined_result[11],combined_result[12],combined_result[13],combined_result[14])

#View(combined_result1)

#############################################################
#############################################################
####################Behaviours###############################
#############################################################
#############################################################

# Function to split the original column into separate data frames
split_behaviours <- function(df, delimiter = ",", custom_names = NULL) {
  # Convert the "behaviours" column to character type
  df$behaviours <- as.character(df$behaviours)
  
  # Split the original column and create separate data frames for each "Behavior_"
  new_cols <- strsplit(df$behaviours, delimiter, fixed = TRUE)
  max_behaviours <- max(sapply(new_cols, length))
  
  # Create separate data frames for each "Behavior_"
  behaviour_dfs <- vector("list", max_behaviours)
  for (i in 1:max_behaviours) {
    if (is.null(custom_names)) {
      col_name <- paste0("Behavior_", i)
    } else {
      col_name <- custom_names[i]
    }
    behaviour_dfs[[i]] <- data.frame(mutate(df, !!col_name := sapply(new_cols, function(x) x[i])) %>% select(!!col_name))
  }
  
  return(behaviour_dfs)
}

# Applying the function to the sample data with custom names for behavior columns
behaviour_custom_names <- c("Behavior_1", "Behavior_2", "Behavior_3", "Behavior_4")
behaviour_dfs <- split_behaviours(extracted_data1, custom_names = behaviour_custom_names)

# Combining the individual data frames with the original columns
dat <- cbind(select(extracted_data1, -behaviours), do.call(cbind, behaviour_dfs))


#View(dat)

######################Create Behaviour A and BehaviourB######################

#2X2 MATRIX
dat$new_col1 <- ifelse(!is.na(dat$Behavior_1) & !is.na(dat$Behavior_2), dat$Behavior_1,dat$Behavior_1)
dat$new_col2 <- ifelse(!is.na(dat$Behavior_1) & is.na(dat$Behavior_2), dat$Behavior_2,dat$Behavior_2)

#3X3 MATRIX

dat$new_col3 <- ifelse(!is.na(dat$Behavior_1) & !is.na(dat$Behavior_3), dat$Behavior_1,dat$Behavior_1)
dat$new_col4 <- ifelse(!is.na(dat$Behavior_1) & is.na(dat$Behavior_3), dat$Behavior_3,dat$Behavior_3)

dat$new_col5 <- ifelse(!is.na(dat$Behavior_2) & !is.na(dat$Behavior_3), dat$Behavior_2,dat$Behavior_2)
dat$new_col6 <- ifelse(!is.na(dat$Behavior_2) & is.na(dat$Behavior_3), dat$Behavior_3,dat$Behavior_3)

#4X4 MATRIX

# Create new columns
dat$new_col7 <- ifelse(!is.na(dat$Behavior_1) & is.na(dat$Behavior_4), dat$Behavior_1, dat$Behavior_1)
dat$new_col8 <- ifelse(!is.na(dat$Behavior_1) & is.na(dat$Behavior_4), dat$Behavior_4, dat$Behavior_4)

# Create new columns
dat$new_col9 <- ifelse(!is.na(dat$Behavior_2) & is.na(dat$Behavior_4), dat$Behavior_2, dat$Behavior_2)
dat$new_col10 <- ifelse(!is.na(dat$Behavior_2) & is.na(dat$Behavior_4), dat$Behavior_4, dat$Behavior_4)

# Create new columns
dat$new_col11 <- ifelse(!is.na(dat$Behavior_3) & is.na(dat$Behavior_4), dat$Behavior_3, dat$Behavior_3)
dat$new_col12 <- ifelse(!is.na(dat$Behavior_3) & is.na(dat$Behavior_4), dat$Behavior_4, dat$Behavior_4)

# Combine columns to create new rows
new_rows <- c(dat$new_col1, dat$new_col3,dat$new_col5,dat$new_col7,dat$new_col9,dat$new_col11)

# Create a new dataframe with the combined rows
BehaviourA <- data.frame(BehaviourA = new_rows)


new_rows <- c(dat$new_col2, dat$new_col4,dat$new_col6,dat$new_col8,dat$new_col10,dat$new_col12)

# Create a new dataframe with the combined rows
BehaviourB <- data.frame(BehaviourB = new_rows)

new_data=data.frame(BehaviourA,BehaviourB)
new_data <- new_data[complete.cases(new_data), ]
data1 <- data.frame(lapply(new_data, function(x) replace(x, x == "", NA)))
# Remove rows with no values
data2 <- data1[complete.cases(data1), ]

# Reset row names
rownames(data2) <- NULL
#View(data2)

behaviour=cbind(combined_result1,data2)
#View(behaviour)


#############################################################
#############################################################
####################Correlation##############################
#############################################################
#############################################################

# Create a new data frame to store the comma counts

split_correlations <- function(df, delimiter = ",") {
  # Convert the "correlations" column to character type
  df$correlations <- as.character(df$correlations)
  
  # Split the original column and create separate data frames for each "correlation1_"
  new_cols <- strsplit(df$correlations, delimiter, fixed = TRUE)
  correlations_dfs <- lapply(new_cols, function(x) {
    num_values <- length(x)
    if (num_values >= 4) {
      if (num_values >= 16) {
        values_to_keep <- c(2, 3, 4, 7, 8, 12)
      } else if (num_values >= 9) {
        values_to_keep <- c(2, 3, 6)
      } else {
        values_to_keep <- 2
      }
      as.data.frame(matrix(x[values_to_keep], nrow = 1))
    } else {
      as.data.frame(matrix(NA, nrow = 1, ncol = 1))
    }
  })
  
  # Get the maximum number of behaviors in a cell
  max_correlations <- max(sapply(correlations_dfs, function(df) ncol(df)))
  
  # Create new column names without quotes and brackets
  new_col_names <- paste0("correlation_", 1:max_correlations)
  new_col_names <- gsub("[\"\\[\\]]", "", new_col_names)
  
  # Fill missing elements with NA for cases with less than max_correlations
  for (i in seq_along(correlations_dfs)) {
    len <- ncol(correlations_dfs[[i]])
    if (len < max_correlations) {
      correlations_dfs[[i]] <- cbind(correlations_dfs[[i]], matrix(NA, nrow = 1, ncol = max_correlations - len))
    }
    colnames(correlations_dfs[[i]]) <- new_col_names
  }
  
  return(correlations_dfs)
}

# Applying the function to the sample data
correlations_dfs <- split_correlations(extracted_data2)

# Combining the individual data frames as separate data frames
data3 <- do.call(rbind, correlations_dfs)


# Combine columns to create new rows
new_rows1 <- c(data3$correlation_1, data3$correlation_2,data3$correlation_3,data3$correlation_4,data3$correlation_5,data3$correlation_6)

# Create a new dataframe with the combined rows
Correlation <- data.frame(Correlation = new_rows1)

Correlation <- Correlation[complete.cases(Correlation), ]
# Reset row names
#View(Correlation)


#############################################################
#############################################################
####################Sample Size##############################
#############################################################
#############################################################

split_samples <- function(df, delimiter = ",") {
  # Convert the "samples" column to character type
  df$samples <- as.character(df$samples)
  
  # Split the original column and create separate data frames for each "samples1_"
  new_cols <- strsplit(df$samples, delimiter, fixed = TRUE)
  samples_dfs <- lapply(new_cols, function(x) {
    num_values <- length(x)
    if (num_values >= 4) {
      if (num_values >= 16) {
        values_to_keep <- c(2, 3, 4, 7, 8, 12)
      } else if (num_values >= 9) {
        values_to_keep <- c(2, 3, 6)
      } else {
        values_to_keep <- 2
      }
      as.data.frame(matrix(x[values_to_keep], nrow = 1))
    } else {
      as.data.frame(matrix(NA, nrow = 1, ncol = 1))
    }
  })
  
  # Get the maximum number of behaviors in a cell
  max_samples <- max(sapply(samples_dfs, function(df) ncol(df)))
  
  # Create new column names without quotes and brackets
  new_col_names <- paste0("sample_", 1:max_samples)
  new_col_names <- gsub("[\"\\[\\]]", "", new_col_names)
  
  # Fill missing elements with NA for cases with less than max_correlations
  for (i in seq_along(samples_dfs)) {
    len <- ncol(samples_dfs[[i]])
    if (len < max_samples) {
      samples_dfs[[i]] <- cbind(samples_dfs[[i]], matrix(NA, nrow = 1, ncol = max_samples - len))
    }
    colnames(samples_dfs[[i]]) <- new_col_names
  }
  
  return(samples_dfs)
}

# Applying the function to the sample data
samples_dfs <- split_samples(extracted_data3)

# Combining the individual data frames as separate data frames
data4 <- do.call(rbind, samples_dfs)

# View the result
#View(data4)


# Combine columns to create new rows
new_rows2 <- c(data4$sample_1, data4$sample_2,data4$sample_3,data4$sample_4,data4$sample_5,data4$sample_6)

# Create a new dataframe with the combined rows
Sample <- data.frame(Sample = new_rows2)

Sample<- Sample[complete.cases(Sample), ]
# Reset row names
#View(Sample)

data=data.frame(behaviour,Correlation,Sample)

#############Cleaning the data frame#############

data <- data[,-2]

# Remove double quotes from Correlation and Sample columns
data$Correlation <- gsub("\"", "", data$Correlation)
data$Sample <- gsub("\"", "", data$Sample)
data$BehaviourA<- gsub("\"", "",data$BehaviourA)
data$BehaviourB<- gsub("\"", "",data$BehaviourB)


# Remove brackets from BehaviourA and BehaviourB columns
data$BehaviourA <- gsub("\\(|\\)", "", data$BehaviourA)
data$BehaviourB <- gsub("\\(|\\)", "", data$BehaviourB)
data$BehaviourA <- gsub("^c", "", data$BehaviourA)
data <- data[, -2]
#View(data)
str(data$X_id)

##################Data Preparation for Meta Analysis########################
############################################################################
############################################################################
############################################################################
##########Fisher Z transformation##########################

# Convert Correlation column to numeric (if needed)
data$Correlation <- as.numeric(data$Correlation)
data$Sample<- as.numeric(data$Sample)
# Perform Fisher's z-transformation on Correlation column
data$rZ <- as.numeric(fisherz(data$Correlation))

############Standard Error################################

data$r2 <- (data$Correlation)*(data$Correlation)
data$n_minus_2 <- data$Sample - 2
# Calculate SE using the formula sqrt(1 - r^2 / (n - 2))
data$SE <- as.numeric(sqrt(1 - data$r2 / data$n_minus_2))

# Print the cleaned and transformed data
#View(data)
#print(data)
############ COUNT ###############
study_count <- length(unique(data$study.name))
effects<- length((data$Correlation))
behaviour=c(data$BehaviourA,data$BehaviourB)
behaviour=length(unique(trimws(behaviour)))

#####################APP####################################
sample <- as.numeric(data$Sample)
min=min(sample)
max=max(sample)

ui <- fluidPage(
  tags$head(
    tags$link(rel = "stylesheet", href = "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/5.15.3/css/all.min.css"),
    tags$style(
      HTML("
        ul.list-icons {
          list-style: none;
          padding-left: 0;
        }
        ul.list-icons li {
          margin-bottom: 10px;
        }
      ")
    )
  ),
  navbarPage(
    title = "TURBBO",
    theme = shinytheme("cerulean"),
    tabPanel(
      "About",
      div(
        style = "border: 3px solid lightblue; background-color: white; padding: 30px; font-size: 18px;",
        h4(style = "font-size: 24px; color: darkblue;", "Estimate Behavioural Relations - Analysis Tool"),
        p(HTML("<strong>If you are interested in understanding the direction and strength of relationship between two (or more) behaviours (e.g., smoking and watching TV):</strong>")),
        HTML('<ul class="list-icons">
                <li><i class="fas fa-database"></i> Check Database Availability: Discover if our database contains studies investigating your behavior-related queries.</li>
                <li><i class="fas fa-chart-line"></i> Conduct Comprehensive Meta-Analysis: Initiate a robust meta-analysis to unveil the direction and strength of these behavior relationships.</li>
                <li><i class="fas fa-cogs"></i> Refine Results with Sample Modification: Tailor meta-analysis outcomes based on sample size considerations for more accurate insights.</li>
              </ul>'),
        p(HTML("<strong>Behavior Ontology Integration</strong>")),
        p("Behaviours being selected are linked to an ontology of behaviour and so will (i) suggest concepts that likely reflect the behaviour of interest and (ii) will include children of the specified class, where relevant (e.g., if you select ‘physical activity’ then any study that measures any type of physical activity will be included."),
        p(HTML("<strong>Why choose?Random effects models for meta-analysis</strong>")),
        HTML('<ul class="list-icons">
                <li><i class="fas fa-chart-bar"></i> Accounts for variation within and between studies.</li>
                <li><i class="fas fa-balance-scale"></i> Deals with dependent effect sizes.</li>
                <li><i class="fas fa-sliders-h"></i> Flexibility in handling different sample sizes: Two RVE Estimation Modes (Large and Small Sample).</li>
                <li><i class="fas fa-balance-scale-left"></i> Diverse Weighting Strategies and Sample-Weighted Average Effect Size.</li>
              </ul>'),
        p(HTML("<strong>Meta-Analytic Statistics</strong>")),
        HTML('<ul class="list-icons">
                <li><i class="fas fa-chart-line"></i> Sample-Weighted Average Effect Size.</li>
                <li><i class="fas fa-chart-line"></i> Heterogeneity: Cochran\'s Q test for heterogeneity.</li>
                <li><i class="fas fa-chart-area"></i> Forest Plot: Display the effect sizes and confidence intervals of individual studies, along with the overall effect size estimate.</li>
                <li><i class="fas fa-search-plus"></i> Publication Bias: Detect and adjust for publication bias using funnel plots.</li>
              </ul>'),
        p("Ready to get started? Use the interactive features  to  uncover valuable insights into behavioural relationships.")
        
      ),
      div(
        style = "text-align: center; font-weight: bold; font-size: 20px; display: flex; justify-content: space-between;",
        div(style = "background-color: darkblue; padding: 20px; flex: 2; color: white;",
            p(HTML('<i class="fas fa-book"></i>'),
              HTML(paste("Number of studies:", study_count)))),
        div(style = "background-color: grey; padding: 20px; flex: 2; color: white;",
            p(HTML('<i class="fas fa-chart-bar"></i>'),
              HTML(paste("Number of effects:", effects)))),
        div(style = "background-color: darkblue; padding: 20px; flex: 2; color: white;",
            p(HTML('<i class="fas fa-cogs"></i>'),
              HTML(paste("Number of behaviors:", behaviour))))
      ),
      
      div(
        style = "border: 2px solid lightblue;text-align: center;font-size: 20px; padding: 20px;",
        p(HTML("<strong>Ontology explorer</strong>")),
        tags$iframe(src = "https://webprotege.stanford.edu/#projects/list", width = "100%", height = "400px")
      ),
      tags$footer(
        style = "background-color: lightblue; color: black; padding: 20px; text-align: center; font-weight: bold;",
        p(
          style = "font-size: 14px;",
          "Tools for Understanding the Relationship between behaviours using Ontologies",
          br(),
          "THE UNIVERSITY OF SHEFFIELD",
          br(),
          "Sheffield S10 2TN",
          br(),
          "Funded by ESRC."
        )
      )
    ),
    
    tabPanel(
      "Meta Analysis",
      fluidRow(
        # Add your content for Meta Analysis here
        sidebarLayout(
          sidebarPanel(
            selectInput(
              "var1",
              label = "Behaviour 1",
              choices = unique(data$BehaviourA)
            ),
            selectInput(
              "var2",
              label = "Behaviour 2",
              choices = unique(data$BehaviourB)
            ),
            
            sliderInput(
              inputId = "samplesize",
              label = "Sample size:",
              min = min(sample),
              max = max(sample),
              value = 1470
            ),
            sliderInput(
              inputId = "correlation_threshold",
              label = "Correlation Threshold:",
              min = -1,
              max = 1,
              value = 0, 
              step = 0.1 
            ),
            downloadButton("download_csv", "Download Data"),
            actionButton("run_analysis", "Meta-Analytic Results"),
            style = "background-color: lightblue;"
          ),
          mainPanel(
            tableOutput("filtered_data_table"),
            h3("Meta-Analysis",style = "text-align: center; font-weight: bold;",),
            verbatimTextOutput("summary"),
            downloadButton("download_summary", "Download Summary Output", class = "download-button"),
            h3("Forest Plot",style = "text-align: center; font-weight: bold;",),
            plotOutput("forest_plot"),
            downloadButton("download_forest_plot", "Download Forest Plot", class = "download-button"),
            h3("Funnel Plot",style = "text-align: center; font-weight: bold;",),
            plotOutput("funnel_plot"),
            downloadButton("download_funnel_plot", "Download Funnel Plot", class = "download-button")
            
          )
        ),
        
        tags$footer(
          style = "background-color:lightblue; color: black; padding: 20px; text-align: center; font-weight: bold;",
          p(
            style = "font-size: 14px;",
            "Tools for Understanding the Relationship between behaviours using Ontologies",
            br(),
            "THE UNIVERSITY OF SHEFFIELD",
            br(),
            "Sheffield S10 2TN",
            br(),
            "Funded by ESRC."
          )
        )
      )
    )
  )
)

server <- function(input, output) {
  filtered_data <- reactive({
    if (is.null(input$run_analysis) || (input$run_analysis == 0)) {
      return(data)
    }
    
    subset_data <- data
    
    samplesize <- input$samplesize
    
    subset_data <- subset_data[!is.na(subset_data$Sample) & subset_data$Sample >= min(subset_data$Sample) & subset_data$Sample <= samplesize,]
    
    if (!is.null(input$var1) && input$var1 != "All") {
      subset_data <- subset_data[subset_data$BehaviourA == input$var1,]
    }
    
    if (!is.null(input$var2) && input$var2 != "All") {
      subset_data <- subset_data[subset_data$BehaviourB == input$var2,]
    }
    
    # Check data types and convert to numeric if necessary
    numeric_columns <- sapply(subset_data, is.numeric)
    subset_data[, numeric_columns] <- lapply(subset_data[, numeric_columns], as.numeric)
    # Filter by correlation threshold
    correlation_threshold <- input$correlation_threshold
    subset_data <- subset_data[abs(subset_data$Correlation) < correlation_threshold, ]
    
    # Check data types and convert to numeric if necessary
    numeric_columns <- sapply(subset_data, is.numeric)
    subset_data[, numeric_columns] <- lapply(subset_data[, numeric_columns], as.numeric)
    
    subset_data
  })
  
  output$filtered_data_table <- renderTable({
    filtered_data_table <- filtered_data()
    
    if (nrow(filtered_data_table) == 0) {
      return(NULL)
    }
    
    # Select columns to display, excluding last 4 columns (SE, n_minus_2)
    columns_to_display <- names(filtered_data_table)[1:(length(names(filtered_data_table)) - 4)]
    filtered_data_table <- filtered_data_table[, c(columns_to_display), drop = FALSE]
  })
  
  # Extract the desired columns separately
  output$download_csv <- downloadHandler(
    filename = function() {
      paste("filtered_data_table", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      filtered_data_to_download <- filtered_data()
      columns_to_exclude <- c("SE", "n_minus_2", "rZ", "r2")
      filtered_data_to_download <- filtered_data_to_download[, !(names(filtered_data_to_download) %in% columns_to_exclude)]
      
      # Extract X_id column separately
      x_id_column <- filtered_data_to_download$X_id
      
      # Extract the other desired columns
      other_columns <- filtered_data_to_download[, c("study.type", "other.study.type", "doi.available", "study.doi", "study.surname.year", "study.name", "ethical.approval", "user", "created_at", "X__v", "BehaviourA", "BehaviourB", "Correlation", "Sample")]
      
      # Combine X_id column and other columns
      combined_data <- cbind(x_id_column, other_columns)
      
      # Write the combined data to CSV
      write.csv(combined_data, file, row.names = FALSE)
    }
  )
  
  
  # Perform meta-analysis
  meta_analysis <- eventReactive(input$run_analysis, {
    subset_data <- filtered_data()
    if (nrow(subset_data) > 0) {
      meta <- rma(rZ, sei = SE, data = subset_data, method = "REML")
      return(meta)
    } else {
      return(NULL)  # Return NULL if no studies found
    }
  })
  
  # Render forest plot
  output$forest_plot <- renderPlot({
    meta <- meta_analysis()
    if (!is.null(meta)) {
      forest(meta)
    }
  })
  
  # Render funnel plot
  output$funnel_plot <- renderPlot({
    meta <- meta_analysis()
    if (!is.null(meta)) {
      funnel(meta)
    }
  })
  
  # Render summary output
  output$summary <- renderPrint({
    meta <- meta_analysis()
    if (!is.null(meta)) {
      summary(meta)
    } else {
      print("No studies found.")
    }
  })
  # Download summary output as a text file
  output$download_summary <- downloadHandler(
    filename = function() {
      paste("meta_analysis_summary", Sys.Date(), ".txt", sep = "")
    },
    content = function(file) {
      meta <- meta_analysis()
      if (!is.null(meta)) {
        summary_text <- capture.output(summary(meta))
        writeLines(summary_text, file)
      } else {
        writeLines("No studies found.", file)
      }
    }
  )
  # Download forest plot as a PNG file
  output$download_forest_plot <- downloadHandler(
    filename = function() {
      paste("forest_plot", Sys.Date(), ".png", sep = "")
    },
    content = function(file) {
      meta <- meta_analysis()
      if (!is.null(meta)) {
        png(file)
      }
      forest(meta)
      dev.off()
    }
  )
  
  # Download funnel plot as a PNG file
  output$download_funnel_plot <- downloadHandler(
    filename = function() {
      paste("funnel_plot", Sys.Date(), ".png", sep = "")
    },
    content = function(file) {
      meta <- meta_analysis()
      if (!is.null(meta)) {
        png(file)
      }
      funnel(meta)
      dev.off()
    }
  )
} 



# Run the Shiny app
shinyApp(ui, server)


