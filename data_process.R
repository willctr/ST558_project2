# reads the raw NFL play-by-play csv, keeps offensive plays, cleans and saves
# a small .rds file for the app. outputs nfl_pbp.rds in app folder


library(dplyr)
library(readr)
library(forcats)

raw_path <- "raw_data/nfl_pbp_2009_2016.csv"
out_path <- "app/nfl_pbp.rds"

team_lookup <- tribble(
  ~team, ~conference, ~division,
  "ATL", "NFC", "NFC South",
  "CAR", "NFC", "NFC South",
  "NO",  "NFC", "NFC South",
  "TB",  "NFC", "NFC South",
  "DAL", "NFC", "NFC East",
  "NYG", "NFC", "NFC East",
  "PHI", "NFC", "NFC East",
  "WAS", "NFC", "NFC East",
  "CHI", "NFC", "NFC North",
  "DET", "NFC", "NFC North",
  "GB",  "NFC", "NFC North", 
  "MIN", "NFC", "NFC North",
  "ARI", "NFC", "NFC West",  
  "LA",  "NFC", "NFC West",
  "SF",  "NFC", "NFC West",  
  "SEA", "NFC", "NFC West",
  "BUF", "AFC", "AFC East",  
  "MIA", "AFC", "AFC East",
  "NE",  "AFC", "AFC East",
  "NYJ", "AFC", "AFC East",
  "BAL", "AFC", "AFC North",
  "CIN", "AFC", "AFC North",
  "CLE", "AFC", "AFC North", 
  "PIT", "AFC", "AFC North",
  "HOU", "AFC", "AFC South", 
  "IND", "AFC", "AFC South",
  "JAX", "AFC", "AFC South", 
  "TEN", "AFC", "AFC South",
  "DEN", "AFC", "AFC West",
  "KC",  "AFC", "AFC West",
  "OAK", "AFC", "AFC West",
  "SD",  "AFC", "AFC West"
)

# standardize team codes that changed over these seasons.
# JAC to JAX, rams from STL to LA
fix_team_rename <- function(x) {
  ifelse(x == "JAC", "JAX",
         ifelse(x == "STL", "LA", x))
}

# read
raw <- read_csv(raw_path, guess_max = 60000, show_col_types = FALSE)

nfl <- raw |>
  filter(PlayType %in% c("Pass", "Run", "Sack"), !is.na(down)) |>
  transmute(
    # identifiers
    game_id = GameID,
    date = as.Date(Date),
    season = factor(Season),
    posteam = fix_team_rename(posteam),
    defteam = fix_team_rename(DefensiveTeam),
    desc,
    
    # game situation
    quarter = factor(qtr, levels = 1:5,
                        labels = c("Q1", "Q2", "Q3", "Q4", "OT")),
    down = factor(down, levels = 1:4,
                        labels = c("1st", "2nd", "3rd", "4th")),
    ydstogo,
    yrdline100,
    time_secs = TimeSecs, 
    score_diff = ScoreDiff,
    
    # play details
    play_type = factor(PlayType, levels = c("Pass", "Run", "Sack")),
    pass_length = factor(if_else(PassLength %in% c("Short", "Deep"),
                                   PassLength, NA_character_),
                           levels = c("Short", "Deep")),
    pass_location = factor(PassLocation, levels = c("left", "middle", "right")),
    pass_outcome = factor(PassOutcome, levels = c("Complete", "Incomplete Pass"),
                           labels = c("Complete", "Incomplete")),
    run_location = factor(RunLocation, levels = c("left", "middle", "right")),
    
    # outcomes
    yards_gained  = Yards.Gained,
    air_yards     = if_else(PlayType == "Pass", AirYards, NA_real_),
    yac           = if_else(PlayType == "Pass", YardsAfterCatch, NA_real_),
    first_down    = factor(FirstDown, levels = 0:1, labels = c("No", "Yes")),
    touchdown     = factor(Touchdown, levels = 0:1, labels = c("No", "Yes")),
    interception  = factor(InterceptionThrown, levels = 0:1, labels = c("No", "Yes")),
    fumble        = factor(Fumble, levels = 0:1, labels = c("No", "Yes")),
    epa           = EPA,
    wpa           = WPA,
    win_prob      = Win_Prob
  ) |>
  # derived groupings
  mutate(
    field_zone = cut(yrdline100,
                     breaks = c(0, 20, 50, 100),
                     labels = c("Red zone", "Opponent territory", "Own territory")),
    field_zone = fct_rev(field_zone),
    game_situation = factor(
      case_when(score_diff > 0 ~ "Leading",
                score_diff == 0 ~ "Tied",
                score_diff < 0 ~ "Trailing"),
      levels = c("Trailing", "Tied", "Leading")
    )
  ) |>
  left_join(team_lookup, by = c("posteam" = "team")) |>
  mutate(
    posteam    = factor(posteam),
    defteam    = factor(defteam),
    conference = factor(conference),
    division   = factor(division)
  )

# checks
stopifnot(!anyNA(nfl$conference))  # every team matched the lookup
cat("Rows:", nrow(nfl), " Columns:", ncol(nfl), "\n")
print(count(nfl, play_type))

# ---- Save ----
saveRDS(nfl, out_path, compress = "xz")
cat("Saved", out_path, "-", round(file.size(out_path) / 1e6, 1), "MB\n")