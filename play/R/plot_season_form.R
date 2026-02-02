

#' Plot Season Form
#' @param league name of league (example: "EPL")
#' @param season_start_year season start year
#' @import stringr
#' @import dplyr
#' @import ggplot2
#' @import purrr
#' @import tibble
#' @import forcats
#' @import tidyr
#' @import forcats
#' @import utils
#' @importFrom worldfootballR understat_league_match_results
#' @return plot with form of teams in specified season
#' @examples
#' plot_season_form("EPL", 2022)
#' @export



plot_season_form <- function(league, season_start_year) {

  epl_results <- worldfootballR::understat_league_match_results(league = league, season_start_year = season_start_year)

  team_list <- epl_results |> distinct(home_abbr) |>
    unname() |> unlist()



  oneline_result <- function(data, team_abbr) {

    home <- data |>
      select(home_abbr, away_abbr, home_goals, away_goals) |>
      filter(home_abbr == team_abbr) |>
      mutate(goal_diff = home_goals-away_goals) |>
      mutate(team = team_abbr) |>
      relocate(team) |>
      select(team, goal_diff)

    away <- data |>
      select(home_abbr, away_abbr, home_goals, away_goals) |>
      filter(away_abbr == team_abbr) |>
      mutate(goal_diff = away_goals-home_goals) |>
      mutate(team = team_abbr) |>
      relocate(team) |>
      select(team, goal_diff)

    results <- bind_rows(home, away) |>
      rownames_to_column("match_num") |>
      mutate(match_num = as.numeric(match_num)) |>
      arrange(match_num) |>
      rowid_to_column("match_order")

    oneline <- results |>
      select(match_order, goal_diff, team) |>
      tidyr::pivot_wider(names_from = match_order, values_from = goal_diff)

    oneline

  }


  current_table <- map_dfr(team_list, oneline_result, data = epl_results) |> arrange(team)


  current_table_simple <- current_table |>
    mutate_if(is.numeric, ~case_when(
      . < 0 ~ -1,
      . == 0 ~ 0,
      . > 0 ~ 1
    ))


  points <- current_table |>
    mutate_if(is.numeric, ~case_when(
      . < 0 ~ -1,
      . == 0 ~ 0,
      . > 0 ~ 1
    )) %>%
    mutate_if(is.numeric, ~case_when(
      . == -1 ~ 0,
      . == 0 ~ 1,
      . == 1 ~ 3
    )) |>
    rowwise() |>
    mutate(Total = sum(c_across(where(is.numeric)), na.rm = TRUE)) |>
    relocate(Total, .after = 1) |>
    arrange(desc(Total)) |> ungroup()



  plot_fun <- function(data) {

    epl_plot <- ggplot(data, aes(factor(match), team3, fill = count)) +
      geom_tile(color = "white",
                lwd = .5,
                linetype = 1,
                height = 0.4) +
      coord_fixed() +
      theme_minimal() +
      scale_x_discrete(position = "top") +
      xlab("Match") +
      ylab("") +
      theme(legend.position="none") +
      theme(text=element_text(family="mono")) +
      theme(
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())


    epl_plot
  }


  tidy_fun <- function(data) {
    data <- data |>
      tidyr::pivot_longer(!team, names_to = "match", values_to = "count") |>
      mutate(match = as.numeric(match)) |>
      mutate(team3 = fct_inorder(team, ordered = TRUE))

    data
  }



  plot <- tidy_fun(current_table_simple) |>
    plot_fun() +
    scale_y_discrete(limits = levels(fct_reorder(points$team, points$Total))) +
    scale_fill_gradient2(low = "#ef9a9a", mid = "#F5F5F5", high = "#A0BCD6", na.value = "white")


  plot
}


