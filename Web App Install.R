library(rsconnect)
rsconnect::deployApp(
  appDir  = "C:/Users/benmu/OneDrive/Documents/ALL DATA Work/LOVM Project/LOVM R Project",
  appName = "lovm-dashboard",
  appFiles = c(
    "app.R",
    list.files("www", full.names = TRUE),
    "vox_machina_complete/SHOW_all_episodes_info.csv",
    "vox_machina_complete/SHOW_all_screen_time_by_episode.csv",
    "vox_machina_complete/SHOW_character_totals_combined.csv"
  )
)
