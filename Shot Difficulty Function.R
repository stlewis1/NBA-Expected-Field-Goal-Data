library(tidyverse)


game_shot_difficulty <- function(game_id) {
  #library(tidyverse)
  library(httr2)
  library(rvest)
  library(jsonlite)
  
  url <- paste0("https://www.nba.com/inside-the-game/blueprint/", game_id)
  
  html <- request(url) |>
    req_user_agent("Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 Chrome/124.0 Safari/537.36") |>
    req_headers(`Accept-Language` = "en-US,en;q=0.9") |>
    req_perform() |>
    resp_body_string()
  
  page <- read_html(html)
  
  # Embedded JSON: try __NEXT_DATA__ first, then any script containing "shot_difficulty"
  json_txt <- page |> html_element("script#__NEXT_DATA__") |> html_text2()
  if (is.na(json_txt)) {
    scripts <- page |> html_elements("script") |> html_text2()
    json_txt <- scripts[grepl("shot_difficulty", scripts, fixed = TRUE)][1]
  }
  
  dat <- fromJSON(json_txt, simplifyVector = FALSE)
  
  # Recursively locate the shot_difficulty node
  find_key <- function(x, key) {
    if (!is.list(x)) return(NULL)
    if (!is.null(names(x)) && key %in% names(x)) return(x[[key]])
    for (el in x) {
      r <- find_key(el, key)
      if (!is.null(r)) return(r)
    }
    NULL
  }
  
  sd <- find_key(dat, "shot_difficulty")
  shots <- sd$shotDifficulty
  
  flat <- function(s) {
    tibble(
      game_id = sd$gameId,
      period = s$period,
      game_clock = s$gameClockTime,
      shot_clock = s$shotClockTime %||% NA,
      shot_type = s$shotType,
      home_score = s$homeTeamScore,
      away_score = s$awayTeamScore,
      success = s$success,
      featured = s$featured,
      shooter_id = s$shooter$playerId,
      shooter = s$shooter$nameI,
      shooter_team = s$shooter$teamAbbreviation,
      defender_id = s$defender$playerId,
      defender = s$defender$nameI,
      defender_team = s$defender$teamAbbreviation,
      contest_distance = s$attributes$contestDistance,
      contest_height = s$attributes$contestHeight,
      shot_vertical = s$attributes$shotVertical,
      shooter_speed = s$attributes$shooterSpeed,
      shot_distance = s$attributes$shotDistance,
      release_time = s$attributes$releaseTime,
      result = s$attributes$result,
      exp_fg_pct = s$expFieldGoalPct,
      ngss_event_id = s$ngssEventId,
      x = s$x,
      y = s$y
    )
  }
  
  shot_df <- map_dfr(shots, flat)
  
  return(shot_df)
  
}





