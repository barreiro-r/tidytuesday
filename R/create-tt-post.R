#' Create a new tidytuesday post based on the template
#'
#' This function create a new post (dir and source.qmd) based on a template
#' and date of the tidytuesday
#'
#' @param template_file A string representing the file path to the template.
#' @param tt_date A string representing the URL of the GitHub repository.
#'
#' @return This function is intended to be used for its side effects,
#'         such as creating files or interacting with Git, so it may not
#'         have a return value.
#'
#' @examples
#' create_tt_post("R/notebook-template.txt", "2025-01-01")
#'
create_tt_post <- function(tt_date, template_file = "R/notebook-template.txt") {
  tt_year <- stringr::str_sub(tt_date, 1, 4)

  github_readme_url <- glue::glue(
    "https://raw.githubusercontent.com/rfordatascience/tidytuesday/refs/heads/main/data/{tt_year}/{tt_date}/readme.md"
  )

  # Get README from github
  readme <- tidyr::tibble(line = readr::read_lines(github_readme_url))

  # Get title
  tt_title <- readme$line[1] |> stringr::str_remove("# ")

  # Get description
  tt_description <-
    readme |>
    # Skip title
    dplyr::slice(3:nrow(readme)) |>
    # Go until the first quote
    dplyr::mutate(first_quote = stringr::str_detect(line, '^> ')) |>
    dplyr::mutate(first_quote = cumsum(first_quote) == 0 | first_quote) |>
    dplyr::filter(first_quote) |>
    # If there is no quote, we go until the end of the first section
    # (it ends on ## The Data section)
    dplyr::mutate(the_data = stringr::str_detect(line, '^## The Data')) |>
    dplyr::mutate(the_data = cumsum(the_data) == 0) |>
    dplyr::filter(the_data) |>
    # Get only the text
    dplyr::pull(line) |>
    stringr::str_c(collapse = '\n')

  # Read template as a single character
  template <- readr::read_file(template_file)

  # Print logs
  message(glue::glue("Creating post for {tt_date}"))
  message(glue::glue("Title: {tt_title}"))
  message(glue::glue("Description: {tt_description}"))

  # Replace placeholders with str_replace_all
  filled_source_content <- stringr::str_replace_all(
    template,
    c(
      '\\{\\{ tt_title \\}\\}' = tt_title,
      '\\{\\{ tt_date \\}\\}' = tt_date,
      '\\{\\{ tt_year \\}\\}' = tt_year,
      '\\{\\{ tt_description \\}\\}' = tt_description
    )
  )

  # Create directories
  dir.create(glue::glue("posts/{tt_date}"), showWarnings = FALSE)

  # Export
  message(glue::glue(
    "Exporting to {glue::glue('posts/{tt_date}/notebook.qmd')}"
  ))
  readr::write_file(
    filled_source_content,
    glue::glue("posts/{tt_date}/notebook.qmd")
  )
}
