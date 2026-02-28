# Load required libraries
library(jsonlite)
library(tidyverse)

# CHARACTER NAME STANDARDIZATION
standardize_character_name <- function(character_names) {
  
  # Define pattern-based rules (checked first - most flexible)
  pattern_rules <- list(
    # Main Characters - match first name with any last name
    list(pattern = "^Vex'?ahlia.*", standardized = "Vex'ahlia"),
    list(pattern = "^Vax'?ildan.*", standardized = "Vax'ildan"),
    list(pattern = "^Percy.*|^Percival.*|^Lord de Rolo$", standardized = "Percival"),
    list(pattern = "^Pike.*", standardized = "Pike"),
    list(pattern = "^Grog.*", standardized = "Grog"),
    list(pattern = "^Scanlan.*", standardized = "Scanlan"),
    list(pattern = "^Keyleth.*", standardized = "Keyleth"),
    
    # Cassandra de Rolo
    list(pattern = "^Cassandra.*|^Lady de Rolo$", standardized = "Cassandra de Rolo"),
    
    # Supporting Characters
    list(pattern = ".*Allura.*Vysoren.*|^Lady Allura$|^Allura$", standardized = "Lady Allura Vysoren"),
    list(pattern = ".*Kima.*Vord.*|^Lady Kima$|^Kima$", standardized = "Lady Kima of Vord"),
    list(pattern = ".*Uriel.*Tal'?Dorei.*|.*Uriel.*|^Uriel$", standardized = "Sovereign Uriel"),
    list(pattern = ".*Krieg.*|^Krieg$", standardized = "General Krieg"),
    list(pattern = ".*Fince.*", standardized = "Sir Fince"),
    list(pattern = "^Ripley$|^Anna Ripley$|.*Ripley.*", standardized = "Anna Ripley"),
    
    # Other characters
    list(pattern = "^Trinket$", standardized = "Trinket"),
    list(pattern = "^Narrator$", standardized = "Narrator")
  )
  
  # Exact mapping rules (checked second - for specific cases)
  exact_mapping <- c(
    "Agar" = "Agar",
    "Fisherman" = "Fisherman",
    "Tavern Keeper" = "Tavern Keeper",
    "Palace Guard" = "Palace Guard",
    "Boy" = "Boy",
    "Girl" = "Girl"
  )
  
  # Apply standardization
  standardized <- sapply(character_names, function(name) {
    # First, try pattern matching
    for (rule in pattern_rules) {
      if (grepl(rule$pattern, name, ignore.case = TRUE)) {
        return(rule$standardized)
      }
    }
    
    # Second, try exact matching
    if (name %in% names(exact_mapping)) {
      return(exact_mapping[name])
    }
    
    # If no match, return original name
    return(name)
  })
  
  return(unname(standardized))
}

# Function to extract season and episode from filename
extract_season_episode <- function(filename) {
  # Pattern: "Season X Episode Y" or "SXX EYY" etc.
  season_match <- str_extract(filename, "(?i)Season\\s*(\\d+)")
  episode_match <- str_extract(filename, "(?i)Episode\\s*(\\d+)")
  
  season <- as.integer(str_extract(season_match, "\\d+"))
  episode <- as.integer(str_extract(episode_match, "\\d+"))
  
  if (is.na(season)) season <- 1
  if (is.na(episode)) episode <- 1
  
  return(list(season = season, episode = episode))
}

# Function to parse single episode
parse_xray_json <- function(json_file, episode_num = NULL, season_num = NULL) {
  
  json_data <- fromJSON(json_file, simplifyVector = FALSE)
  center_widgets <- json_data$page$sections$center$widgets$widgetList
  
  time_indexed <- NULL
  for (widget in center_widgets) {
    if (!is.null(widget$widgets) && !is.null(widget$widgets$widgetList)) {
      for (sub_widget in widget$widgets$widgetList) {
        if (!is.null(sub_widget$`__type`) && 
            grepl("TimeIndexedCollection", sub_widget$`__type`)) {
          time_indexed <- sub_widget
          break
        }
      }
    }
    if (!is.null(time_indexed)) break
  }
  
  if (is.null(time_indexed)) {
    stop("Could not find TimeIndexedCollection in JSON structure")
  }
  
  # Extract season/episode from filename if not provided
  if (is.null(season_num) || is.null(episode_num)) {
    se <- extract_season_episode(basename(json_file))
    season_num <- se$season
    episode_num <- se$episode
  }
  
  # Episode metadata
  partitions <- time_indexed$partitionedChangeList
  last_partition <- partitions[[length(partitions)]]
  total_duration <- last_partition$timeRange$endTime
  
  episode_info <- data.frame(
    episode_file = basename(json_file),
    season = season_num,
    episode = episode_num,
    total_duration_ms = total_duration,
    total_duration_sec = total_duration / 1000,
    total_duration_min = total_duration / 60000,
    stringsAsFactors = FALSE
  )
  
  # Character master list
  items <- time_indexed$items
  character_master <- data.frame(
    item_id = character(),
    actor_name = character(),
    character_name = character(),
    stringsAsFactors = FALSE
  )
  
  for (item in items) {
    item_id <- item$id
    item_data <- item$item
    
    if (!is.null(item_data$textMap$PRIMARY)) {
      primary_text <- item_data$textMap$PRIMARY
      
      if (!grepl("Shop|Who's that actor", primary_text)) {
        actor_name <- primary_text
        character_name <- gsub("^.*/([^/]+)$", "\\1", item_id)
        
        character_master <- rbind(character_master, data.frame(
          item_id = item_id,
          actor_name = actor_name,
          character_name = character_name,
          stringsAsFactors = FALSE
        ))
      }
    }
  }
  
  character_master <- character_master %>% 
    distinct(item_id, .keep_all = TRUE) %>%
    mutate(character_name_standardized = standardize_character_name(character_name))
  
  # Raw appearance data
  appearances_raw <- data.frame(
    partition_id = integer(),
    item_id = character(),
    actor_name = character(),
    character_name = character(),
    time_position_ms = numeric(),
    segment_start_ms = numeric(),
    segment_end_ms = numeric(),
    stringsAsFactors = FALSE
  )
  
  for (i in seq_along(partitions)) {
    partition <- partitions[[i]]
    time_range <- partition$timeRange
    changes <- partition$changesCollection
    
    if (length(changes) > 0) {
      for (change in changes) {
        if (change$changeType == "AddItem") {
          item_id <- change$itemId
          time_pos <- change$timePosition
          
          char_info <- character_master %>% filter(item_id == !!item_id)
          
          if (nrow(char_info) > 0) {
            appearances_raw <- rbind(appearances_raw, data.frame(
              partition_id = i,
              item_id = item_id,
              actor_name = char_info$actor_name[1],
              character_name = char_info$character_name[1],
              time_position_ms = time_pos,
              segment_start_ms = time_range$startTime,
              segment_end_ms = time_range$endTime,
              stringsAsFactors = FALSE
            ))
          }
        }
      }
    }
  }
  
  appearances_raw <- appearances_raw %>%
    mutate(
      season = season_num,
      episode = episode_num,
      character_name_original = character_name,
      character_name = standardize_character_name(character_name),
      time_position_sec = time_position_ms / 1000,
      time_position_min = time_position_ms / 60000,
      segment_duration_ms = segment_end_ms - time_position_ms,
      segment_duration_sec = segment_duration_ms / 1000,
      segment_duration_min = segment_duration_ms / 60000
    )
  
  # Screen time calculation
  screen_time_summary <- appearances_raw %>%
    group_by(actor_name, character_name) %>%
    summarise(
      num_appearances = n(),
      first_appearance_ms = min(time_position_ms),
      last_appearance_ms = max(time_position_ms),
      .groups = "drop"
    )
  
  merge_time_segments <- function(times_df) {
    if (nrow(times_df) == 0) return(0)
    
    times_df <- times_df %>% arrange(start)
    
    merged_start <- times_df$start[1]
    merged_end <- times_df$end[1]
    total_time <- 0
    
    if (nrow(times_df) > 1) {
      for (i in 2:nrow(times_df)) {
        current_start <- times_df$start[i]
        current_end <- times_df$end[i]
        
        if (current_start <= merged_end) {
          merged_end <- max(merged_end, current_end)
        } else {
          total_time <- total_time + (merged_end - merged_start)
          merged_start <- current_start
          merged_end <- current_end
        }
      }
    }
    
    total_time <- total_time + (merged_end - merged_start)
    return(total_time)
  }
  
  screen_time_list <- list()
  
  for (i in 1:nrow(screen_time_summary)) {
    actor <- screen_time_summary$actor_name[i]
    character <- screen_time_summary$character_name[i]
    
    char_appearances <- appearances_raw %>%
      filter(actor_name == actor, character_name == character) %>%
      select(start = time_position_ms, end = segment_end_ms)
    
    merged_time <- merge_time_segments(char_appearances)
    
    screen_time_list[[i]] <- data.frame(
      actor_name = actor,
      character_name = character,
      total_screen_time_ms = merged_time,
      stringsAsFactors = FALSE
    )
  }
  
  screen_time_merged <- bind_rows(screen_time_list)
  
  screen_time_summary <- screen_time_summary %>%
    left_join(screen_time_merged, by = c("actor_name", "character_name")) %>%
    mutate(
      season = season_num,
      episode = episode_num,
      total_screen_time_sec = total_screen_time_ms / 1000,
      total_screen_time_min = total_screen_time_ms / 60000,
      first_appearance_min = first_appearance_ms / 60000,
      last_appearance_min = last_appearance_ms / 60000,
      percent_of_episode = (total_screen_time_ms / episode_info$total_duration_ms) * 100
    ) %>%
    arrange(desc(total_screen_time_min))
  
  actor_summary <- screen_time_summary %>%
    group_by(actor_name) %>%
    summarise(
      season = first(season),
      episode = first(episode),
      num_characters = n(),
      characters_played = paste(character_name, collapse = ", "),
      total_appearances = sum(num_appearances),
      total_screen_time_ms = sum(total_screen_time_ms),
      total_screen_time_sec = total_screen_time_ms / 1000,
      total_screen_time_min = total_screen_time_ms / 60000,
      percent_of_episode = (total_screen_time_ms / episode_info$total_duration_ms) * 100,
      .groups = "drop"
    ) %>%
    arrange(desc(total_screen_time_min))
  
  return(list(
    episode_info = episode_info,
    character_master = character_master,
    appearances_raw = appearances_raw,
    screen_time_summary = screen_time_summary,
    actor_summary = actor_summary
  ))
}

# Process all episodes from all seasons
process_all_seasons <- function(json_files, output_dir = "output") {
  
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }
  
  # Lists to store all data
  all_episode_info <- list()
  all_appearances <- list()
  all_screen_time <- list()
  all_actor_summary <- list()
  
  cat("Processing", length(json_files), "episodes across all seasons...\n\n")
  
  # Process each episode
  for (i in seq_along(json_files)) {
    json_file <- json_files[i]
    
    cat("Processing:", basename(json_file), "\n")
    
    data <- parse_xray_json(json_file)
    
    season_num <- data$episode_info$season
    episode_num <- data$episode_info$episode
    
    # Save individual episode files
    episode_prefix <- sprintf("S%02dE%02d", season_num, episode_num)
    
    write.csv(data$episode_info, 
              file.path(output_dir, paste0(episode_prefix, "_episode_info.csv")), 
              row.names = FALSE)
    write.csv(data$character_master, 
              file.path(output_dir, paste0(episode_prefix, "_character_master.csv")), 
              row.names = FALSE)
    write.csv(data$appearances_raw, 
              file.path(output_dir, paste0(episode_prefix, "_appearances_raw.csv")), 
              row.names = FALSE)
    write.csv(data$screen_time_summary, 
              file.path(output_dir, paste0(episode_prefix, "_screen_time_summary.csv")), 
              row.names = FALSE)
    write.csv(data$actor_summary, 
              file.path(output_dir, paste0(episode_prefix, "_actor_summary.csv")), 
              row.names = FALSE)
    
    # Add to combined lists
    all_episode_info[[i]] <- data$episode_info
    all_appearances[[i]] <- data$appearances_raw
    all_screen_time[[i]] <- data$screen_time_summary
    all_actor_summary[[i]] <- data$actor_summary
    
    cat("  ✓ Saved individual episode files\n\n")
  }
  
  # Combine all data
  cat("Creating aggregated files...\n\n")
  
  all_episode_info_df <- bind_rows(all_episode_info)
  all_appearances_df <- bind_rows(all_appearances)
  all_screen_time_df <- bind_rows(all_screen_time)
  all_actor_summary_df <- bind_rows(all_actor_summary)
  
  # Get unique seasons
  seasons <- unique(all_episode_info_df$season)
  
  # Create season-level aggregations
  for (season_num in seasons) {
    cat("Creating Season", season_num, "aggregations...\n")
    
    season_episodes <- all_episode_info_df %>% filter(season == season_num)
    season_appearances <- all_appearances_df %>% filter(season == season_num)
    season_screen_time <- all_screen_time_df %>% filter(season == season_num)
    season_actor <- all_actor_summary_df %>% filter(season == season_num)
    
    # Season character totals
    season_character_totals <- season_screen_time %>%
      group_by(actor_name, character_name) %>%
      summarise(
        season = first(season),
        episodes_appeared = n(),
        total_appearances = sum(num_appearances),
        total_screen_time_ms = sum(total_screen_time_ms),
        total_screen_time_sec = total_screen_time_ms / 1000,
        total_screen_time_min = total_screen_time_ms / 60000,
        avg_screen_time_per_episode_min = total_screen_time_min / episodes_appeared,
        first_episode = min(episode),
        last_episode = max(episode),
        .groups = "drop"
      ) %>%
      arrange(desc(total_screen_time_min))
    
    # Season character totals - COMBINED (all actors for same character)
    season_character_combined <- season_screen_time %>%
      group_by(character_name) %>%
      summarise(
        season = first(season),
        actors = paste(unique(actor_name), collapse = " / "),
        num_actors = n_distinct(actor_name),
        episodes_appeared = n_distinct(episode),
        total_appearances = sum(num_appearances),
        total_screen_time_ms = sum(total_screen_time_ms),
        total_screen_time_sec = total_screen_time_ms / 1000,
        total_screen_time_min = total_screen_time_ms / 60000,
        avg_screen_time_per_episode_min = total_screen_time_min / episodes_appeared,
        first_episode = min(episode),
        last_episode = max(episode),
        .groups = "drop"
      ) %>%
      arrange(desc(total_screen_time_min))
    
    # Season actor totals
    season_actor_totals <- season_actor %>%
      group_by(actor_name) %>%
      summarise(
        season = first(season),
        episodes_appeared = n(),
        characters_played = paste(unique(unlist(strsplit(characters_played, ", "))), collapse = ", "),
        total_screen_time_ms = sum(total_screen_time_ms),
        total_screen_time_sec = total_screen_time_ms / 1000,
        total_screen_time_min = total_screen_time_ms / 60000,
        avg_screen_time_per_episode_min = total_screen_time_min / episodes_appeared,
        .groups = "drop"
      ) %>%
      arrange(desc(total_screen_time_min))
    
    # Save season files
    season_prefix <- sprintf("Season_%d", season_num)
    
    write.csv(season_episodes, 
              file.path(output_dir, paste0(season_prefix, "_all_episodes_info.csv")), 
              row.names = FALSE)
    write.csv(season_appearances, 
              file.path(output_dir, paste0(season_prefix, "_all_appearances.csv")), 
              row.names = FALSE)
    write.csv(season_screen_time, 
              file.path(output_dir, paste0(season_prefix, "_all_screen_time_by_episode.csv")), 
              row.names = FALSE)
    write.csv(season_character_totals, 
              file.path(output_dir, paste0(season_prefix, "_character_totals_by_actor.csv")), 
              row.names = FALSE)
    write.csv(season_character_combined, 
              file.path(output_dir, paste0(season_prefix, "_character_totals_combined.csv")), 
              row.names = FALSE)
    write.csv(season_actor_totals, 
              file.path(output_dir, paste0(season_prefix, "_actor_totals.csv")), 
              row.names = FALSE)
    
    cat("  ✓ Season", season_num, "files saved\n\n")
  }
  
  # Create SHOW-LEVEL aggregations
  cat("Creating SHOW-LEVEL aggregations...\n")
  
  # Show character totals - BY ACTOR (keeps actors separate)
  show_character_totals <- all_screen_time_df %>%
    group_by(actor_name, character_name) %>%
    summarise(
      seasons_appeared = n_distinct(season),
      episodes_appeared = n(),
      total_appearances = sum(num_appearances),
      total_screen_time_ms = sum(total_screen_time_ms),
      total_screen_time_sec = total_screen_time_ms / 1000,
      total_screen_time_min = total_screen_time_ms / 60000,
      total_screen_time_hours = total_screen_time_min / 60,
      avg_screen_time_per_episode_min = total_screen_time_min / episodes_appeared,
      first_season = min(season),
      last_season = max(season),
      first_episode = min(episode),
      last_episode = max(episode),
      .groups = "drop"
    ) %>%
    arrange(desc(total_screen_time_min))
  
  # Show character totals - COMBINED (merges different actors for same character)
  show_character_combined <- all_screen_time_df %>%
    group_by(character_name) %>%
    summarise(
      actors = paste(unique(actor_name), collapse = " / "),
      num_actors = n_distinct(actor_name),
      seasons_appeared = n_distinct(season),
      episodes_appeared = n_distinct(paste(season, episode)),
      total_appearances = sum(num_appearances),
      total_screen_time_ms = sum(total_screen_time_ms),
      total_screen_time_sec = total_screen_time_ms / 1000,
      total_screen_time_min = total_screen_time_ms / 60000,
      total_screen_time_hours = total_screen_time_min / 60,
      avg_screen_time_per_episode_min = total_screen_time_min / episodes_appeared,
      first_season = min(season),
      last_season = max(season),
      .groups = "drop"
    ) %>%
    arrange(desc(total_screen_time_min))
  
  show_actor_totals <- all_actor_summary_df %>%
    group_by(actor_name) %>%
    summarise(
      seasons_appeared = n_distinct(season),
      episodes_appeared = n(),
      characters_played = paste(unique(unlist(strsplit(characters_played, ", "))), collapse = ", "),
      total_screen_time_ms = sum(total_screen_time_ms),
      total_screen_time_sec = total_screen_time_ms / 1000,
      total_screen_time_min = total_screen_time_ms / 60000,
      total_screen_time_hours = total_screen_time_min / 60,
      avg_screen_time_per_episode_min = total_screen_time_min / episodes_appeared,
      .groups = "drop"
    ) %>%
    arrange(desc(total_screen_time_min))
  
  # Save show-level files
  write.csv(all_episode_info_df, 
            file.path(output_dir, "SHOW_all_episodes_info.csv"), 
            row.names = FALSE)
  write.csv(all_appearances_df, 
            file.path(output_dir, "SHOW_all_appearances.csv"), 
            row.names = FALSE)
  write.csv(all_screen_time_df, 
            file.path(output_dir, "SHOW_all_screen_time_by_episode.csv"), 
            row.names = FALSE)
  write.csv(show_character_totals, 
            file.path(output_dir, "SHOW_character_totals_by_actor.csv"), 
            row.names = FALSE)
  write.csv(show_character_combined, 
            file.path(output_dir, "SHOW_character_totals_combined.csv"), 
            row.names = FALSE)
  write.csv(show_actor_totals, 
            file.path(output_dir, "SHOW_actor_totals.csv"), 
            row.names = FALSE)
  
  # Save complete RData
  save(all_episode_info_df, all_appearances_df, all_screen_time_df,
       show_character_totals, show_character_combined, show_actor_totals,
       file = file.path(output_dir, "SHOW_complete_data.RData"))
  
  cat("\n✓ SHOW-LEVEL files created!\n\n")
  
  # Print summary
  cat("=== COMPLETE SHOW SUMMARY ===\n")
  cat("Total seasons:", length(seasons), "\n")
  cat("Total episodes:", nrow(all_episode_info_df), "\n")
  cat("Total unique characters:", nrow(show_character_combined), "\n")
  cat("Total runtime:", sum(all_episode_info_df$total_duration_min), "minutes (", 
      round(sum(all_episode_info_df$total_duration_min) / 60, 1), "hours)\n\n")
  
  cat("Top 10 Characters by Total Screen Time (Entire Show - COMBINED):\n")
  print(head(show_character_combined %>% 
               select(character_name, actors, num_actors, seasons_appeared, 
                      episodes_appeared, total_screen_time_hours), 10))
  
  cat("\n\nCharacters with Multiple Actors (Flashbacks/Young versions):\n")
  multi_actor_chars <- show_character_combined %>% 
    filter(num_actors > 1) %>%
    select(character_name, actors, num_actors, total_screen_time_hours)
  
  if (nrow(multi_actor_chars) > 0) {
    print(multi_actor_chars)
  } else {
    cat("None found.\n")
  }
  
  # Check for unstandardized names
  cat("\n\n=== CHARACTER NAME VARIATIONS CHECK ===\n")
  cat("Review these to see if any names need standardization:\n\n")
  
  name_check <- show_character_totals %>%
    select(character_name, actor_name, episodes_appeared) %>%
    arrange(actor_name, character_name)
  
  print(name_check)
  
  return(list(
    all_episode_info = all_episode_info_df,
    all_appearances = all_appearances_df,
    all_screen_time = all_screen_time_df,
    show_character_totals = show_character_totals,
    show_character_combined = show_character_combined,
    show_actor_totals = show_actor_totals
  ))
}

# USAGE: Get all JSON files from current directory
json_files <- list.files(path = ".", 
                         pattern = "\\.json$", 
                         full.names = TRUE)

# Sort files to ensure correct order
json_files <- sort(json_files)

cat("Found", length(json_files), "JSON files:\n")
print(basename(json_files))
cat("\n")

# Process all seasons
show_data <- process_all_seasons(json_files, output_dir = "vox_machina_complete")

cat("\n=== FILES CREATED ===\n")
cat("Individual Episode Files: S01E01_*, S01E02_*, ... S03E12_*\n")
cat("\nSeason Aggregations:\n")
cat("  - Season_X_character_totals_by_actor.csv (keeps actors separate)\n")
cat("  - Season_X_character_totals_combined.csv (merges actors for same character)\n")
cat("  - Season_X_actor_totals.csv\n")
cat("\nShow-Level Aggregations:\n")
cat("  - SHOW_character_totals_by_actor.csv (keeps actors separate - for detailed analysis)\n")
cat("  - SHOW_character_totals_combined.csv (merges actors - for visualizations)\n")
cat("  - SHOW_actor_totals.csv\n")
cat("\nAll files saved in: vox_machina_complete/\n")
cat("\nTIP: Use *_combined.csv files for visualizations to show total character screen time.\n")
cat("     Use *_by_actor.csv files when you need to see actor details or flashback breakdowns.\n")