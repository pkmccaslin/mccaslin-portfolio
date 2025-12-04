
# turn off scientific notation
options(scipen = 999)

# load Packages
source("fxns.R")
library(tidyverse)
library(ellmer)
library(glue)
library(lubridate)

movies <- read.csv("data/raw/horror_movie_locations.csv", header = FALSE) |> 
  clean_names() |> 
  # set column names
  rename(
    title = v1,
    state = v2,
    rating = v3,
    multiple_locations = v4,
    description = v5
  ) |> 
  # fixing mispelled names
  mutate(
    state = trimws(state),
    state = case_when(
      state == "Flordia" ~ "Florida",
      state == "District of Colombia" ~ "District of Columbia",
      state == "West Virgina" ~ "West Virginia",
      state == "New Mexco" ~ "New Mexico",
      state == "Illlinois" ~ "Illinois",
      state == "Lousiana" ~ "Louisiana",
      state == "Oho" ~ "Ohio",
      state == "Kanasa" ~ "Kansas",
      TRUE ~ state
    )
  ) |> 
  mutate(
    # Matches a four digit number enclosed by parentheses
    year = as.numeric(str_extract(title, "(?<=\\()\\d{4}(?=\\))")),
    # Removes the year in parentheses
    title = str_remove(title, "\\s*\\(\\d{4}\\)\\s*$"),
    # Floors the year to the nearest decade
    year_bin = floor(year/10)*10
  )

movies_maryland <- movies |> 
  filter(state == "Maryland")

## Non AI analysis

# Finding average and median ratings by state
movies_by_state <- movies |> 
  group_by(state) |> 
  summarize(
    count = n(),
    avg_rating = mean(rating, na.rm = TRUE),
    med_rating = median(rating, na.rm = TRUE),
  )

movies_by_state_year <- movies |> 
  group_by(state, year_bin) |> 
  summarize(
    count = n(),
    avg_rating = mean(rating, na.rm = TRUE),
    med_rating = median(rating, na.rm = TRUE),
  )

## Defining structured output

# Defining an enum object. The object is restricted to certain characteristics with certain options
type_characteristics <- type_object(
  primary_sub_genre = type_enum(
    c(
      "phobia",
      "madness",
      "home invasion",
      "slasher",
      "backwoods horror",
      "torture",
      "body horror",
      "splatter",
      "cannibal",
      "extreme",
      "zombies",
      "virus",
      "vampire",
      "werewolf",
      "classic and mythological",
      "neo-monsters",
      "nature",
      "giant creatures",
      "small creatures",
      "sci-fi and aliens",
      "ghost and spirits",
      "haunted house",
      "possession",
      "devils, demons, and hell",
      "witches",
      "supernatural power",
      "horror comedy",
      "parody horror",
      "Lovecraftian/Cosmic horror",
      "gothic horror",
      "found footage",
      "folk horror",
      "post-apocalyptic"
    ),
    "Identify the primary sub-genre of the movie"
  ),
  secondary_sub_genre = type_enum(
    c(
      "phobia",
      "madness",
      "home invasion",
      "slasher",
      "backwoods horror",
      "torture",
      "body horror",
      "splatter",
      "cannibal",
      "extreme",
      "zombies",
      "virus",
      "vampire",
      "werewolf",
      "classic and mythological",
      "neo-monsters",
      "nature",
      "giant creatures",
      "small creatures",
      "sci-fi and aliens",
      "ghost and spirits",
      "haunted house",
      "possession",
      "devils, demons, and hell",
      "witches",
      "supernatural power",
      "horror comedy",
      "parody horror",
      "Lovecraftian/Cosmic horror",
      "gothic horror",
      "found footage",
      "folk horror",
      "post-apocalyptic"
    ),
    "Identify the secondary sub-genre of the movie. You may not choose {primary_sub_genre} as it has been chosen already."
  ),
  tertiary_sub_genre = type_enum(
    c(
      "phobia",
      "madness",
      "home invasion",
      "slasher",
      "backwoods horror",
      "torture",
      "body horror",
      "splatter",
      "cannibal",
      "extreme",
      "zombies",
      "virus",
      "vampire",
      "werewolf",
      "classic and mythological",
      "neo-monsters",
      "nature",
      "giant creatures",
      "small creatures",
      "sci-fi and aliens",
      "ghost and spirits",
      "haunted house",
      "possession",
      "devils, demons, and hell",
      "witches",
      "supernatural power",
      "horror comedy",
      "parody horror",
      "Lovecraftian/Cosmic horror",
      "gothic horror",
      "found footage",
      "folk horror",
      "post-apocalyptic"
    ),
    "Identify the tertiary sub-genre of the movie. You may not choose {primary_sub_genre} or {secondary_sub_genre} as they have been chosen already."
  ),
  protagonist_type = type_enum(
    c(
      "human",
      "human with extra powers",
      "possessed human",
      "non-human entity",
      "unknown"
    ),
    "The protagonist is a "
  ),
  antagonist_type = type_enum(
    c(
      "human",
      "human with extra powers",
      "possessed human",
      "non-human entity",
      "unknown"
    ),
    "The antagonist is a "
  ),
  protagonist_gender = type_enum(
    c("male", "female", "unknown"),
    "The protagonist's gender is "
  ),
  antagonist_gender = type_enum(
    c("male", "female", "androgynous", "unknown"),
    "The antagonist's gender is "
  ),
  protagonist_age = type_enum(
    c("under 18", "18-30", "30-50", "50+", "unknown"),
    "The protagonist is within the following age range"
  ),
  antagonist_age = type_enum(
    c("under 18", "18-30", "30-50", "50+", "unknown"),
    "The antagonist is within the following age range"
  ),
  setting_time = type_enum(
    c("past", "present", "future"),
    "The movie was filmed in {year}. The movie is set in the "
  ),
  setting_location = type_enum(
    c(
      "house",
      "forest",
      "big city",
      "small town",
      "rural",
      "outer space",
      "unknown"
    ),
    "The movie is primarily located in the "
  )
)

## Declare global chat objects

# Read in the model prompt
system_prompt_movie <- readLines("prompt.md")

# Declaring several AI models with a custom system prompt to extract characteristics from each movie

# Declaring several different GPT models as ellmer objects
# If there's no OpenAI API key, then send an error
if (Sys.getenv("OPENAI_API_KEY") == "") {
  stop("ERROR: OPENAI_API_KEY environment variable is not set")
} else {
  gpt_api <- Sys.getenv("OPENAI_API_KEY")
}

chat_gpt_41 <- chat_openai(
  system_prompt = system_prompt_movie,
  model = "gpt-4.1",
  api_key = gpt_api
)

chat_gpt_41_mini <- chat_openai(
  system_prompt = system_prompt_movie,
  model = "gpt-4.1-mini",
  api_key = gpt_api
)

chat_gpt_5_mini <- chat_openai(
  system_prompt = system_prompt_movie,
  model = "gpt-5-mini",
  api_key = gpt_api
)

chat_gpt_5 <- chat_openai(
  system_prompt = system_prompt_movie,
  model = "gpt-5",
  api_key = gpt_api
)

# Declaring several different Claude models as ellmer objects
# If there's no Claude API key, then send an error
if (Sys.getenv("ANTHROPIC_API_KEY") == "") {
  stop("ERROR: ANTHROPIC_API_KEY environment variable is not set")
} else {
  anthropic_api <- Sys.getenv("ANTHROPIC_API_KEY")
}

chat_claude_sonnet_37 <- chat_anthropic(
  system_prompt = system_prompt_movie,
  model = "claude-3-7-sonnet-20250219",
  api_key = anthropic_api
)

chat_claude_sonnet_4 <- chat_anthropic(
  system_prompt = system_prompt_movie,
  model = "claude-sonnet-4-20250514",
  api_key = anthropic_api
)

# Declaring several different Gemini models as ellmer objects
# If there's no Gemini API key, then send an error
if (Sys.getenv("GOOGLE_API_KEY") == "") {
  stop("ERROR: GOOGLE_API_KEY environment variable is not set")
} else {
  gemini_api <- Sys.getenv("GOOGLE_API_KEY")
}

chat_gemini_flash_preview <- chat_google_gemini(
  system_prompt = system_prompt_movie,
  mode = "gemini-2.5-flash-preview-09-2025",
  api_key = gemini_api,
)

chat_gemini_pro <- chat_google_gemini(
  system_prompt = system_prompt_movie,
  mode = "gemini-2.5-pro",
  api_key = gemini_api
)

chat_gemini_flash <- chat_google_gemini(
  system_prompt = system_prompt_movie,
  mode = "gemini-2.5-flash",
  api_key = gemini_api
)

# List of valid models to later check if we're calling a declared object
valid_models <- list("gpt-4.1" = chat_gpt_41, 
                     "gpt-4.1-mini" = chat_gpt_41_mini,
                     "gpt-5-mini" = chat_gpt_5_mini,
                     "gpt-5" = chat_gpt_5,
                     "claude-3-7-sonnet-20250219" = chat_claude_sonnet_37,
                     "claude-sonnet-4-20250514" = chat_claude_sonnet_4,
                     "gemini-2.5-pro" = chat_gemini_pro,
                     "gemini-2.5-flash" = chat_gemini_flash,
                     "gemini-2.5-flash-preview-09-2025" = chat_gemini_flash_preview)

## Ellmer function

# Functions
address_ellmer <- function(chat, ai_info) {
  
  # Wrap entire function in tryCatch to prevent stopping execution
  tryCatch({
    
    # Ensure that it's calling a valid model
    if (!chat %in% names(valid_models)) {
      stop(paste("ERROR: chat must be one of:", toString(names(valid_models))))
    }
    
    if (missing(ai_info) || is.null(ai_info) || ai_info == "") {
      stop("ERROR: ai_info must be provided and cannot be empty")
    }
    
    # Define the chat object (used within the function) as the chat model that was called in the function
    chat_obj <- valid_models[[chat]]
    
    # Execute chat with error handling and retry logic
    result <- NULL
    max_retries <- 3
    retry_count <- 0
    last_error <- NULL
    
    # If the function hasn't tried three times and the result is null, then try executing again
    while (retry_count < max_retries && is.null(result)) {
      tryCatch({
        # Reset the chat history of the chat model
        chat_obj$set_turns(list())
        # Prompt the chat model with ai_info, extract an object of type type_characteristics and print into the console
        result <- chat_obj$chat_structured(ai_info, type = type_characteristics, echo = "all")
        
        # Check if result is valid and print error if it fails
        if (is.null(result) || (is.character(result) && result == "")) {
          warning("Empty result received from ", chat, " API")
          result <- NULL
          last_error <- "Empty result received from API"
        }
        
      }, error = function(e) {
        # Increment retry counter
        retry_count <<- retry_count + 1
        # Store the last error in case the function fails
        last_error <<- e$message
        
        if (retry_count < max_retries) {
          warning("Attempt ", retry_count, " failed for ", chat, " API: ", e$message, ". Retrying...")
          Sys.sleep(2^retry_count)  # Exponential backoff
        }
      })
    }
    
    # Final validation
    if (is.null(result)) {
      return(paste("ERROR: Failed to get valid response from", chat, "API after", max_retries, "attempts. Last error:", last_error))
    }
    
    return(result)
    
  }, error = function(e) {
    # Catch-all for any unexpected errors
    return(paste("ERROR: Unexpected error in address_ellmer:", e$message))
  })
}

## Test different models

movie_test <-
  movies |> 
  # Test each model using a movie I know well, an unpopular movie from MD, and a popular movie from MD
  filter(title %in% c("Smile","Camp Killer","The Blair Witch Project")) |> 
  # Each column is a model's output and its cost in dollars
  mutate(
    gpt_41 = map(ai_info, ~ address_ellmer("gpt-4.1", .x)),
    cost_gpt_41 = chat_gpt_41$get_cost(include = "all"),
    gpt_41_mini = map(ai_info, ~ address_ellmer("gpt-4.1-mini", .x)),
    cost_gpt_41_mini = chat_gpt_41_mini$get_cost(include = "all"),
    gpt_5_mini = map(ai_info, ~ address_ellmer("gpt-5-mini", .x)),
    cost_gpt_5_mini = chat_gpt_5_mini$get_cost(include = "all"),
    gpt_5 = map(ai_info, ~ address_ellmer("gpt-5", .x)),
    cost_gpt_5 = chat_gpt_5$get_cost(include = "all"),
    sonnet_37 = map(ai_info, ~ address_ellmer("claude-3-7-sonnet-20250219", .x)),
    cost_claude_sonnet_37 = chat_claude_sonnet_37$get_cost(include = "all"),
    sonnet_4 = map(ai_info, ~ address_ellmer("claude-sonnet-4-20250514", .x)),
    cost_claude_sonnet_4 = chat_claude_sonnet_4$get_cost(include = "all"),
    gemini_pro = map(ai_info, ~ address_ellmer("gemini-2.5-pro", .x)),
    cost_gemini_pro = chat_gemini_pro$get_cost(include = "all"),
    gemini_flash = map(ai_info, ~ address_ellmer("gemini-2.5-flash", .x)),
    cost_gemini_flash = chat_gemini_flash$get_cost(include = "all"),
    gemini_flash_preview = map(ai_info, ~ address_ellmer("gemini-2.5-flash-preview-09-2025", .x)),
    cost_gemini_flash_preview = chat_gemini_flash_preview$get_cost(include = "all")
  ) 

# Claude doesn't support my type_object and GPT-5 is too new
# 
# Smile:
# GPT 4.1 - 11/11 subgenres were madness, possession, phobia
# GPT 4.1 mini - 11/11 subgenres were madness, possession, phobia
# Gemini Pro - 10/11 subgenres were madness, possession, phobia, wrong setting
# Gemini Flash - 10/11 subgenres were madness, devils.., phobia, wrong setting
# Gemini Flash preview - 10/11 subgrenres were madness, possession, ghosts, wrong setting
# 
# Camp Killer:
# GPT 4.1 - 10/11 subgenres were slasher, madness, phobia of which slasher and madness were right. Should've included comedy or satire
# GPT 4.1 mini - 10/11 subgenres were slasher, home invasion, madness of which slasher was right. Should've included comedy or satire
# Gemini Pro - 9/11 subgenres were slasher, madness, phobia, got antagonist age and gender wrong
# Gemini Flash - 7/11 subgenres were slasher, madness, phobia, missed protagonist and antagonist age and gender
# Gemini Flash preview - 9/11 subgenres were slasher, madness, phobia, missed protagonist and antagonist gender
# 
# The Blair Witch Project:
# GPT 4.1 - 11/11 subgenres were found footage, folk horror, phobia
# GPT 4.1 mini - 11/11 subgenres were found footage, folk horror, phobia
# Gemini Pro - 11/11 subgenres were found footage, witches, folk horror
# Gemini Flash - 11/11 subgenres were found footage, witches, folk horror
# Gemini Flash preview - 11/11 subgenres were found footage, witches, folk horror


# Conducted a more extensive test on Maryland movies
movie_maryland_test <- movies_maryland |> 
  sample_n(10) |> 
  mutate(
    gpt_41 = map(ai_info, ~ address_ellmer("gpt-4.1", .x)),
    cost_gpt_41 = chat_gpt_41$get_cost(include = "all"),
    gpt_41_mini = map(ai_info, ~ address_ellmer("gpt-4.1-mini", .x)),
    cost_gpt_41_mini = chat_gpt_41_mini$get_cost(include = "all")
  )

```

# Last Night at Terrace Lanes:
# GPT 4.1 - 11/11
# GPT 4.1 mini - 9/11
# Multiple Maniacs:
# GPT 4.1 - 11/11
# GPT 4.1 mini - 10/11
# The Scooby Doo Project
# GPT 4.1 - 11/11
# GPT 4.1 mini - 9/11

# Manual batching

movies_maryland_classified <- movies |>
  filter(state == "Maryland")
  mutate(
    gpt_41 = map(ai_info, ~ address_ellmer("gpt-4.1", .x))
  )

movies_new_mexico_classified <- movies |> 
  filter(state == "New Mexico") |> 
  mutate(
    gpt_41 = map(ai_info, ~ address_ellmer("gpt-4.1", .x))
  )

movies_oregon_classified <- movies |> 
  filter(state == "Oregon") |> 
  mutate(
    gpt_41 = map(ai_info, ~ address_ellmer("gpt-4.1", .x))
  )

movies_maine_classified <- movies |> 
  filter(state == "Maine") |> 
  mutate(
    gpt_41 = map(ai_info, ~ address_ellmer("gpt-4.1", .x))
  )

movies_arizona_classified <- movies |> 
  filter(state == "Arizona") |> 
  mutate(
    gpt_41 = map(ai_info, ~ address_ellmer("gpt-4.1", .x))
  )

movies_washington_classified <- movies |> 
  filter(state == "Washington") |> 
  mutate(
    gpt_41 = map(ai_info, ~ address_ellmer("gpt-4.1", .x))
  )

movies_georgia_classified <- movies |> 
  filter(state == "Georgia") |> 
  mutate(
    gpt_41 = map(ai_info, ~ address_ellmer("gpt-4.1", .x))
  )

movies_ohio_classified <- movies |> 
  filter(state == "Ohio") |> 
  mutate(
    gpt_41 = map(ai_info, ~ address_ellmer("gpt-4.1", .x))
  )

movies_nevada_classified <- movies |> 
  filter(state == "Nevada") |> 
  mutate(
    gpt_41 = map(ai_info, ~ address_ellmer("gpt-4.1", .x))
  )

movies_illinois_classified <- movies |> 
  filter(state == "Illinois") |> 
  mutate(
    gpt_41 = map(ai_info, ~ address_ellmer("gpt-4.1", .x))
  )

movies_massachusetts_classified <- movies |> 
  filter(state == "Massachusetts") |> 
  mutate(
    gpt_41 = map(ai_info, ~ address_ellmer("gpt-4.1", .x))
  )

movies_jersey_classified <- movies |> 
  filter(state == "New Jersey") |> 
  mutate(
    gpt_41 = map(ai_info, ~ address_ellmer("gpt-4.1", .x))
  )

movies_pennsylvania_classified <- movies |> 
  filter(state == "Pennsylvania") |> 
  mutate(
    gpt_41 = map(ai_info, ~ address_ellmer("gpt-4.1", .x))
  )

movies_louisiana_classified <- movies |> 
  filter(state == "Louisiana") |> 
  mutate(
    gpt_41 = map(ai_info, ~ address_ellmer("gpt-4.1", .x))
  )

movies_florida_classified <- movies |> 
  filter(state == "Florida") |> 
  mutate(
    gpt_41 = map(ai_info, ~ address_ellmer("gpt-4.1", .x))
  )

movies_new_york_classified <- movies |> 
  filter(state == "New York") |> 
  mutate(
    gpt_41 = map(ai_info, ~ address_ellmer("gpt-4.1", .x))
  )

movies_texas_classified <- movies |> 
  filter(state == "Texas") |> 
  mutate(
    gpt_41 = map(ai_info, ~ address_ellmer("gpt-4.1", .x))
  )

movies_california_classified <- movies |> 
  filter(state == "California") |> 
  mutate(
    gpt_41 = map(ai_info, ~ address_ellmer("gpt-4.1", .x))
  )

movies_michigan_classified <- movies |> 
  filter(state == "Michigan") |> 
  mutate(
    gpt_41 = map(ai_info, ~ address_ellmer("gpt-4.1", .x))
  )

# Creating a list of states with fewer entries
lesser_states <- movies_by_state |> 
  arrange(-count) |> 
  slice(20:54) |> 
  pull(state)

# Classifying the rest of the states in one shot
movies_north_carolina_virgin_islands <- movies |> 
  filter(state %in% lesser_states) |> 
  mutate(
    gpt_41 = map(ai_info, ~ address_ellmer("gpt-4.1", .x))
  )

# Combining all classifications into one dataset
movies_classified <- bind_rows(
  movies_maryland_classified,
  movies_new_mexico_classified,
  movies_north_carolina_virgin_islands,
  movies_michigan_classified,
  movies_ohio_classified,
  movies_georgia_classified,
  movies_washington_classified,
  movies_arizona_classified,
  movies_maine_classified,
  movies_oregon_classified,
  movies_nevada_classified,
  movies_illinois_classified,
  movies_massachusetts_classified,
  movies_jersey_classified,
  movies_pennsylvania_classified,
  movies_louisiana_classified,
  movies_florida_classified,
  movies_new_york_classified,
  movies_texas_classified,
  movies_california_classified
) |> 
  unnest_wider(gpt_41, names_sep = "_")
