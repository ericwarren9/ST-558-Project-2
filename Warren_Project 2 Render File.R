# Purpose: Make a File to Store produce a md file that produces our output from the corresponding Rmd file.

rmarkdown::render("~/ST-558-Project-2/Warren_ST 558 Project 2.Rmd", 
                  output_file = "README.md",
                  output_format = "github_document", 
                  output_options = list(
                    toc = TRUE, 
                    toc_depth = 3,
                    number_sections = TRUE,
                    df_print = "tibble",
                    html_preview = FALSE
                  )
)