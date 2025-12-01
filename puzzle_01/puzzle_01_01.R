#' puzzle_01_01.R
#' @name puzzle_01_01
#' @title puzzle_01_01
#' @description
#' Solution to part 01
#' of puzzle 01
#' of Advent of Code 2025
#' @author AXBRITT
#' @import here magrittr
#'

# Load required libraries ----
library(here)
library(magrittr)

# Setup relative paths ----
here::i_am(
  base::paste(
    "puzzle_01",
    "puzzle_01_01.R",
    sep = .Platform$file.sep
  )
)

# load input data ----
## Source: https://adventofcode.com/2025/day/1/input
input_data <- utils::read.csv(
  here::here(
    "puzzle_01",
    "input",
    "INPUT.csv"
  )
) |>
  base::as.data.frame()

# Define initial conditions ----
start_digit <- 50
digit_count <- 100 # 0 to 99
initial_row <- base::data.frame(
  INPUT = "",
  OUTPUT = start_digit
)

# Define functions to use to make this easier ----
## Ensure value is always a number from 0 to 99 by using modulus
this_value <- function(modified_value, digit_count) {
  return_value <- modified_value %% digit_count
  return(return_value)
}

## Calculate the output of each rotation
this_rotation <- function(this_input, current_value) {
  direction <- substr(this_input, 1, 1)
  current_value <- current_value |> base::as.integer()
  this_value <- base::substr(
    this_input,
    2,
    base::nchar(this_input)
  ) |> as.integer()
  switch(
    direction,
    L = return_value <- current_value - this_value,
    R = return_value <- current_value + this_value
  )
  return(return_value)
}

# Start looping through input_data ----
output_data <- initial_row
this_digit <- start_digit

for (i in base::seq_len(base::length(input_data$INPUT))) {
  this_input <- input_data[i, "INPUT"]
  this_digit <- this_rotation(this_input, this_digit)
  this_digit <- this_value(this_digit, digit_count)
  this_row <- base::data.frame(
    INPUT = this_input,
    OUTPUT = this_digit
  )
  output_data <- base::rbind(
    output_data,
    this_row
  )
}

# Handle output_data ----
## Show output_data and password to user ----
View(output_data)
password <- base::length(
  output_data[
    base::which(output_data$OUTPUT == 0),
    "OUTPUT"
  ]
)
print(password)
## Create output directory ----
if(!base::dir.exists(
  here::here(
    "puzzle_01",
    "output"
  )
)) {
  base::dir.create(
    here::here(
      "puzzle_01",
      "output"
    )
  )
}

## Write output_data to csv ----
utils::write.csv(
  output_data,
  file = here::here(
    "puzzle_01",
    "output",
    "OUTPUT_01.csv"
  )
)

## Write password to password.txt ----
base::cat(
  password,
  file = here::here(
    "puzzle_01",
    "output",
    "password_01.txt"
  )
)