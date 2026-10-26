#' Example cost matrices for assignment problems
#'
#' Small example datasets for demonstrating couplr functionality across
#' different assignment problem types: square, rectangular, sparse, and binary.
#'
#' @format A list containing four example cost matrices:
#' \describe{
#'   \item{simple_3x3}{A 3x3 cost matrix with costs ranging from 2-7. Optimal
#'     assignment: row 1 -> col 2 (cost 2), row 2 -> col 1 (cost 3),
#'     row 3 -> col 3 (cost 4). Total optimal cost: 9.}
#'   \item{rectangular_3x5}{A 3x5 rectangular cost matrix demonstrating
#'     assignment when rows < columns. Each of 3 rows is assigned to one of
#'     5 columns; 2 columns remain unassigned. Costs range 1-6.}
#'   \item{sparse_with_na}{A 3x3 matrix with NA values indicating forbidden
#'     assignments. Use this to test algorithms' handling of constraints.
#'     Position (1,3), (2,2), and (3,1) are forbidden.}
#'   \item{binary_costs}{A 3x3 matrix with binary (0/1) costs, suitable for
#'     testing the HK01 algorithm. Diagonal entries are 0 (preferred),
#'     off-diagonal entries are 1 (penalty).}
#' }
#'
#' @details
#' These matrices are designed to test different aspects of LAP solvers:
#'
#' \strong{simple_3x3}: Basic functionality test. Any correct solver should
#' find total cost = 9.
#'
#' \strong{rectangular_3x5}: Tests handling of non-square problems. The
#' optimal solution assigns all 3 rows with minimum total cost.
#'
#' \strong{sparse_with_na}: Tests constraint handling. Algorithms must avoid
#' NA positions while finding an optimal assignment among valid entries.
#'
#' \strong{binary_costs}: Tests specialized binary cost algorithms. The
#' optimal assignment uses all diagonal entries (total cost = 0).
#'
#' @examples
#' # Simple 3x3 assignment
#' result <- lap_solve(example_costs$simple_3x3)
#' print(result)
#' # Optimal: sources 1,2,3 -> targets 2,1,3 with cost 9
#'
#' # Rectangular problem (3 sources, 5 targets)
#' result <- lap_solve(example_costs$rectangular_3x5)
#' print(result)
#' # All 3 sources assigned; 2 targets unassigned
#'
#' # Sparse problem with forbidden assignments
#' result <- lap_solve(example_costs$sparse_with_na)
#' print(result)
#' # Avoids NA positions
#'
#' # Binary costs - test HK01 algorithm
#' result <- lap_solve(example_costs$binary_costs, method = "hk01")
#' print(result)
#' # Finds diagonal assignment (cost = 0)
#'
#' @seealso \code{\link{lap_solve}}, \code{\link{example_df}}
#' @export
example_costs <- list(
  simple_3x3 = matrix(c(
    4, 2, 5,
    3, 3, 6,
    7, 5, 4
  ), nrow = 3, byrow = TRUE),
  
  rectangular_3x5 = matrix(c(
    1, 2, 3, 4, 5,
    6, 5, 4, 3, 2,
    2, 3, 4, 5, 6
  ), nrow = 3, byrow = TRUE),
  
  sparse_with_na = matrix(c(
    4, 2, NA,
    3, NA, 6,
    NA, 5, 4
  ), nrow = 3, byrow = TRUE),
  
  binary_costs = matrix(c(
    0, 1, 1,
    1, 0, 1,
    1, 1, 0
  ), nrow = 3, byrow = TRUE)
)

#' Example assignment problem data frame
#'
#' A tidy data frame representation of assignment problems, suitable for
#' use with grouped workflows and batch solving. Contains two independent
#' 3x3 assignment problems in long format.
#'
#' @format A tibble with 18 rows and 4 columns:
#' \describe{
#'   \item{sim}{Simulation/problem identifier. Integer with values 1 or 2,
#'     distinguishing two independent assignment problems. Use with
#'     \code{group_by(sim)} for grouped solving.}
#'   \item{source}{Source node index. Integer 1-3 representing the row
#'     (source) in each 3x3 cost matrix.}
#'   \item{target}{Target node index. Integer 1-3 representing the column
#'     (target) in each 3x3 cost matrix.}
#'   \item{cost}{Cost of assigning source to target. Numeric values ranging
#'     from 1-7. Each source-target pair has exactly one cost entry.}
#' }
#'
#' @details
#' This dataset demonstrates couplr's data frame interface for LAP solving.
#' The long format (one row per source-target pair) is converted internally
#' to a cost matrix for solving.
#'
#' \strong{Simulation 1}: Costs from \code{example_costs$simple_3x3}
#' \itemize{
#'   \item Optimal assignment: (1->2, 2->1, 3->3)
#'   \item Total cost: 9
#' }
#'
#' \strong{Simulation 2}: Different cost structure
#' \itemize{
#'   \item Optimal assignment: (1->1, 2->3, 3->3) or equivalent
#'   \item Total cost: 4
#' }
#'
#' @examples
#' library(dplyr)
#'
#' # Solve both problems with grouped workflow
#' example_df |>
#'   group_by(sim) |>
#'   lap_solve(source, target, cost)
#'
#' # Batch solving for efficiency
#' example_df |>
#'   group_by(sim) |>
#'   lap_solve_batch(source, target, cost)
#'
#' # Inspect the data structure
#' example_df |>
#'   group_by(sim) |>
#'   summarise(
#'     n_pairs = n(),
#'     min_cost = min(cost),
#'     max_cost = max(cost)
#'   )
#'
#' @seealso \code{\link{lap_solve}}, \code{\link{lap_solve_batch}},
#'   \code{\link{example_costs}}
#' @export
example_df <- tibble::tibble(
  sim = rep(1:2, each = 9),
  source = rep(1:3, times = 6),
  target = rep(1:3, each = 3, times = 2),
  cost = c(
    # Simulation 1
    4, 2, 5,
    3, 3, 6,
    7, 5, 4,
    # Simulation 2
    1, 2, 3,
    4, 3, 2,
    5, 4, 1
  )
)

#' Hospital staff scheduling example dataset
#'
#' A comprehensive example dataset for demonstrating couplr functionality
#' across vignettes. Contains synthetic hospital staff scheduling data with
#' nurses, shifts, costs, and preference scores suitable for assignment
#' problems, as well as nurse characteristics for matching workflows.
#'
#' This dataset is used throughout the couplr documentation to provide
#' a consistent, realistic example that evolves in complexity. It supports
#' three use cases: (1) basic LAP solving with cost matrices, (2) batch
#' solving across multiple days, and (3) matching workflows comparing
#' nurse groups.
#'
#' @format A list containing eight related datasets:
#' \describe{
#'   \item{basic_costs}{A 10x10 numeric cost matrix for assigning 10 nurses
#'     to 10 shifts. Values range from approximately 1-15, where lower values
#'     indicate better fit (less overtime, matches skills, respects preferences).
#'     Use with \code{lap_solve()} for basic assignment.}
#'   \item{preferences}{A 10x10 numeric preference matrix on a 0-10 scale,
#'     where higher values indicate stronger nurse preference for a shift.
#'     Use with \code{lap_solve(..., maximize = TRUE)} to optimize preferences
#'     rather than minimize costs.}
#'   \item{schedule_df}{A tibble with 100 rows (10 nurses x 10 shifts) in
#'     long format for data frame workflows:
#'     \describe{
#'       \item{nurse_id}{Integer 1-10. Unique identifier for each nurse.}
#'       \item{shift_id}{Integer 1-10. Unique identifier for each shift.}
#'       \item{cost}{Numeric. Assignment cost (same values as basic_costs).}
#'       \item{preference}{Numeric 0-10. Nurse preference score.}
#'       \item{skill_match}{Integer 0/1. Binary indicator: 1 if nurse skills
#'         match shift requirements, 0 otherwise.}
#'     }
#'   }
#'   \item{nurses}{A tibble with 10 rows describing nurse characteristics:
#'     \describe{
#'       \item{nurse_id}{Integer 1-10. Links to schedule_df and basic_costs rows.}
#'       \item{experience_years}{Numeric 1-20. Years of nursing experience.}
#'       \item{department}{Character. Primary department: "ICU", "ER",
#'         "General", or "Pediatrics".}
#'       \item{shift_preference}{Character. Preferred shift type: "day",
#'         "evening", or "night".}
#'       \item{certification_level}{Integer 1-3. Certification level where
#'         3 is highest (e.g., 1=RN, 2=BSN, 3=MSN).}
#'     }
#'   }
#'   \item{shifts}{A tibble with 10 rows describing shift requirements:
#'     \describe{
#'       \item{shift_id}{Integer 1-10. Links to schedule_df and basic_costs cols.}
#'       \item{department}{Character. Department needing coverage.}
#'       \item{shift_type}{Character. Shift type: "day", "evening", or "night".}
#'       \item{min_experience}{Numeric. Minimum years of experience required.}
#'       \item{min_certification}{Integer 1-3. Minimum certification level.}
#'     }
#'   }
#'   \item{weekly_df}{A tibble for batch solving with 500 rows
#'     (5 days x 10 nurses x 10 shifts):
#'     \describe{
#'       \item{day}{Character. Day of week: "Mon", "Tue", "Wed", "Thu", "Fri".}
#'       \item{nurse_id}{Integer 1-10. Nurse identifier.}
#'       \item{shift_id}{Integer 1-10. Shift identifier.}
#'       \item{cost}{Numeric. Daily assignment cost (varies by day).}
#'       \item{preference}{Numeric 0-10. Daily preference score.}
#'     }
#'     Use with \code{group_by(day)} for solving each day's schedule.
#'   }
#'   \item{nurses_extended}{A tibble with 200 nurses for matching examples,
#'     representing a treatment group (e.g., full-time nurses):
#'     \describe{
#'       \item{nurse_id}{Integer 1-200. Unique identifier.}
#'       \item{age}{Numeric 22-65. Nurse age in years.}
#'       \item{experience_years}{Numeric 0-40. Years of nursing experience.}
#'       \item{hourly_rate}{Numeric 25-75. Hourly wage in dollars.}
#'       \item{department}{Character. Primary department assignment.}
#'       \item{certification_level}{Integer 1-3. Certification level.}
#'       \item{is_fulltime}{Logical. TRUE for full-time status.}
#'     }
#'   }
#'   \item{controls_extended}{A tibble with 300 potential control nurses
#'     (e.g., part-time or registry nurses) for matching. Same structure
#'     as nurses_extended. Designed to have systematic differences from
#'     nurses_extended (older, less experience on average) to demonstrate
#'     matching's ability to create comparable groups.}
#' }
#'
#' @details
#' The dataset is designed to demonstrate progressively complex scenarios:
#'
#' \strong{Basic LAP} (\code{vignette("getting-started")}):
#' \itemize{
#'   \item \code{basic_costs}: Simple 10x10 assignment
#'   \item \code{preferences}: Maximization problem
#'   \item \code{schedule_df}: Data frame input, grouped workflows
#'   \item \code{weekly_df}: Batch solving across days
#' }
#'
#' \strong{Algorithm comparison} (\code{vignette("algorithms")}):
#' \itemize{
#'   \item Use \code{basic_costs} to compare algorithm behavior
#'   \item Modify with NA values for sparse scenarios
#' }
#'
#' \strong{Matching workflows} (\code{vignette("matching-workflows")}):
#' \itemize{
#'   \item \code{nurses_extended}: Treatment group (full-time nurses)
#'   \item \code{controls_extended}: Control pool (part-time/registry nurses)
#'   \item Match on age, experience, department for causal analysis
#' }
#'
#' @examples
#' # Basic assignment: assign nurses to shifts minimizing cost
#' lap_solve(hospital_staff$basic_costs)
#'
#' # Maximize preferences instead
#' lap_solve(hospital_staff$preferences, maximize = TRUE)
#'
#' # Data frame workflow
#' library(dplyr)
#' hospital_staff$schedule_df |>
#'   lap_solve(nurse_id, shift_id, cost)
#'
#' # Batch solve weekly schedule
#' hospital_staff$weekly_df |>
#'   group_by(day) |>
#'   lap_solve(nurse_id, shift_id, cost)
#'
#' # Matching workflow: match full-time to part-time nurses
#' match_couples(
#'   left = hospital_staff$nurses_extended,
#'   right = hospital_staff$controls_extended,
#'   vars = c("age", "experience_years", "certification_level"),
#'   auto_scale = TRUE
#' )
#'
#' @seealso
#' \code{\link{lap_solve}} for basic assignment solving,
#' \code{\link{lap_solve_batch}} for batch solving,
#' \code{\link{match_couples}} for matching workflows,
#' \code{vignette("getting-started")} for introductory tutorial
#'
"hospital_staff"
