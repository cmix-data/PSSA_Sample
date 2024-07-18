library(here)
library(quarto)
library(purrr)

report <- "_imprint.qmd"
path <- here("Quarto", report)
reportoutput <- "_imprint.html"
reportoutputpath <- here("Quarto", reportoutput)

load(here("Clean Data", "summary_tested.rda"))

school <- unique(tested_summary$Publication_Name)


render_report = function(selectedschool = params$selectedschool) {
  safe_school_name <- gsub("[[:space:]/&]", "_", selectedschool)
  output_file <- paste0(safe_school_name, ".html")
  
  # Full path to the input file
  full_input_path <- normalizePath(here(path))
  full_input_path_gsub <- gsub("\\\\", "/", full_input_path)
  
  # Create a temporary JSON file for execute_params
  temp_json <- tempfile(fileext = ".json")
  execute_params_list <- list(selectedschool = selectedschool)
  jsonlite::write_json(execute_params_list, temp_json)
  temp_json_gsub <- gsub("\\\\", "/", temp_json)
  
  # Print paths for debugging
  print(paste("Full input path: ", full_input_path))
  print(paste("Output file: ", output_file))
  
  cmd_args <- c(
    'quarto render',
    paste0('"', full_input_path_gsub, '"'),
    '--execute-params', paste0('"', temp_json_gsub, '"'),
    '--to html'
  )
  
  # Print the full command for debugging
  print(paste("Debug: Full command is ", paste(cmd_args, collapse=' ')))
  
  
  system(paste(cmd_args, collapse = ' '))
  
  #Rename the output file
  old_file_name <- reportoutputpath  
  new_file_name <- paste0(safe_school_name, ".html")
  new_file_name_path <- here("Quarto", new_file_name)
  file.rename(from = here(old_file_name), to = here(new_file_name_path))
  
  # Remove the temporary JSON file
  file.remove(temp_json)
}


# Loop through schools and render reports
for (school in school) {
  render_report(selectedschool = school)
  print(paste0('finished knitting ', school))
}
