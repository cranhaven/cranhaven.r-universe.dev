

context("Join data")


testthat::setup({
    # remove indices if they exist
    remove_all_indices()
    #
    employee %>% 
        kc$push("employee", id_col = "emp_id")
    dept %>% 
        kc$push("dept", id_col = "dept_id")
    # to test 
    dplyr::starwars %>% 
        change_names() %>% 
        mutate_factors() %>% 
        kc$push("starwars")
})

testthat::teardown({
    # remove indices if they exist
    remove_all_indices()
    # # remove var from global env 
    # rm(ds_subset, envir = .GlobalEnv)
})


# -----------------------------------------------------

# all call private$join() with their own type (inner, full, anti, etc.), 
# join_type param, others params are as follow:
#   
#   join_cols = NULL, 
#   keep_metadata = FALSE
#
#   left_index = NULL,      right_index = NULL, 
#   left_fields = NULL,     right_fields = NULL, 
#   left_query = NULL,      right_query = NULL, 
#   left_bulk_size = 500,   right_bulk_size = 500, 
#   left_max_size = NULL,   right_max_size = NULL, 

# 
# to test joins, use random lines from ds
# ds_random_lines





# start args  ----

test_that("kibior::joins, no args", {
    expect_error(kc$inner_join())
    expect_error(kc$left_join())
    expect_error(kc$right_join())
    expect_error(kc$full_join())
    expect_error(kc$semi_join())
    expect_error(kc$anti_join())
})

test_that("kibior::joins, error when only one index", {
    for(i in names(ds)){
        expect_error(kc$inner_join(ds[[i]]))
        expect_error(kc$left_join(ds[[i]]))
        expect_error(kc$right_join(ds[[i]]))
        expect_error(kc$full_join(ds[[i]]))
        expect_error(kc$semi_join(ds[[i]]))
        expect_error(kc$anti_join(ds[[i]]))
    }
})

test_that("kibior::joins, error when only one index left", {
    for(i in names(ds)){
        expect_error(kc$inner_join(ds[[i]]))
        expect_error(kc$left_join(ds[[i]]))
        expect_error(kc$right_join(ds[[i]]))
        expect_error(kc$full_join(ds[[i]]))
        expect_error(kc$semi_join(ds[[i]]))
        expect_error(kc$anti_join(ds[[i]]))
    }
})

test_that("kibior::joins, error when only one index right", {
    for(i in names(ds)){
        expect_error(kc$inner_join(right_index = ds[[i]]))
        expect_error(kc$left_join(right_index = ds[[i]]))
        expect_error(kc$right_join(right_index = ds[[i]]))
        expect_error(kc$full_join(right_index = ds[[i]]))
        expect_error(kc$semi_join(right_index = ds[[i]]))
        expect_error(kc$anti_join(right_index = ds[[i]]))
    }
})

# from dplyr *_join
#
# by (param), a character vector of variables to join by. 
# If NULL, the default, *_join() will do a natural join, 
# using all variables with common names across the two tables.



## VALIDATING SAME DATASET JOIN

## local vs. local

test_that("kibior::joins, simple nomial, natural join onto itself, local vs. local, inner join", {
    r <- kc$inner_join(employee, employee)
    expect_setequal(names(r), names(employee))
    expect_equal(nrow(r), nrow(employee))
    expect_setequal(employee$emp_id, r$emp_id)
})

test_that("kibior::joins, simple nomial, natural join onto itself, local vs. local, full join", {
    r <- kc$full_join(employee, employee)
    expect_setequal(names(r), names(employee))
    expect_equal(nrow(r), nrow(employee))
    expect_setequal(employee$emp_id, r$emp_id)
})

test_that("kibior::joins, simple nomial, natural join onto itself, local vs. local, left join", {
    r <- kc$left_join(employee, employee)
    expect_setequal(names(r), names(employee))
    expect_equal(nrow(r), nrow(employee))
    expect_setequal(employee$emp_id, r$emp_id)
})

test_that("kibior::joins, simple nomial, natural join onto itself, local vs. local, right join", {
    r <- kc$right_join(employee, employee)
    expect_setequal(names(r), names(employee))
    expect_equal(nrow(r), nrow(employee))
    expect_setequal(employee$emp_id, r$emp_id)
})

test_that("kibior::joins, simple nomial, natural join onto itself, local vs. local, semi join", {
    r <- kc$semi_join(employee, employee)
    expect_setequal(names(r), names(employee))
    expect_equal(nrow(r), nrow(employee))
    expect_setequal(employee$emp_id, r$emp_id)
})

test_that("kibior::joins, simple nomial, natural join onto itself, local vs. local, anti join", {
    r <- kc$anti_join(employee, employee)
    expect_setequal(names(r), names(employee))
    expect_equal(nrow(r), 0)
})



## local vs. remote

test_that("kibior::joins, simple nomial, natural join onto itself, local vs. remote, inner join", {
    r <- kc$inner_join(employee, "employee")
    expect_setequal(names(r), names(employee))
    expect_equal(nrow(r), nrow(employee))
    expect_setequal(employee$emp_id, r$emp_id)
})

test_that("kibior::joins, simple nomial, natural join onto itself, local vs. remote, full join", {
    r <- kc$full_join(employee, "employee")
    expect_setequal(names(r), names(employee))
    expect_equal(nrow(r), nrow(employee))
    expect_setequal(employee$emp_id, r$emp_id)
})

test_that("kibior::joins, simple nomial, natural join onto itself, local vs. remote, left join", {
    r <- kc$left_join(employee, "employee")
    expect_setequal(names(r), names(employee))
    expect_equal(nrow(r), nrow(employee))
    expect_setequal(employee$emp_id, r$emp_id)
})

test_that("kibior::joins, simple nomial, natural join onto itself, local vs. remote, right join", {
    r <- kc$right_join(employee, "employee")
    expect_setequal(names(r), names(employee))
    expect_equal(nrow(r), nrow(employee))
    expect_setequal(employee$emp_id, r$emp_id)
})

test_that("kibior::joins, simple nomial, natural join onto itself, local vs. remote, semi join", {
    r <- kc$semi_join(employee, "employee")
    expect_setequal(names(r), names(employee))
    expect_equal(nrow(r), nrow(employee))
    expect_setequal(employee$emp_id, r$emp_id)
})

test_that("kibior::joins, simple nomial, natural join onto itself, local vs. remote, anti join", {
    r <- kc$anti_join(employee, "employee")
    expect_setequal(names(r), names(employee))
    expect_equal(nrow(r), 0)
})



## remote vs. local

test_that("kibior::joins, simple nomial, natural join onto itself, remote vs. local, inner join", {
    r <- kc$inner_join("employee", employee)
    expect_setequal(names(r), names(employee))
    expect_equal(nrow(r), nrow(employee))
    expect_setequal(employee$emp_id, r$emp_id)
})

test_that("kibior::joins, simple nomial, natural join onto itself, remote vs. local, full join", {
    r <- kc$full_join("employee", employee)
    expect_setequal(names(r), names(employee))
    expect_equal(nrow(r), nrow(employee))
    expect_setequal(employee$emp_id, r$emp_id)
})

test_that("kibior::joins, simple nomial, natural join onto itself, remote vs. local, left join", {
    r <- kc$left_join("employee", employee)
    expect_setequal(names(r), names(employee))
    expect_equal(nrow(r), nrow(employee))
    expect_setequal(employee$emp_id, r$emp_id)
})

test_that("kibior::joins, simple nomial, natural join onto itself, remote vs. local, right join", {
    r <- kc$right_join("employee", employee)
    expect_setequal(names(r), names(employee))
    expect_equal(nrow(r), nrow(employee))
    expect_setequal(employee$emp_id, r$emp_id)
})

test_that("kibior::joins, simple nomial, natural join onto itself, remote vs. local, semi join", {
    r <- kc$semi_join("employee", employee)
    expect_setequal(names(r), names(employee))
    expect_equal(nrow(r), nrow(employee))
    expect_setequal(employee$emp_id, r$emp_id)
})

test_that("kibior::joins, simple nomial, natural join onto itself, remote vs. local, anti join", {
    r <- kc$anti_join("employee", employee)
    expect_setequal(names(r), names(employee))
    expect_equal(nrow(r), 0)
})



## remote vs. remote

test_that("kibior::joins, simple nomial, natural join onto itself, remote vs. remote, inner join", {
    r <- kc$inner_join("employee", "employee")
    expect_setequal(names(r), names(employee))
    expect_equal(nrow(r), nrow(employee))
    expect_setequal(employee$emp_id, r$emp_id)
})

test_that("kibior::joins, simple nomial, natural join onto itself, remote vs. remote, full join", {
    r <- kc$full_join("employee", "employee")
    expect_setequal(names(r), names(employee))
    expect_equal(nrow(r), nrow(employee))
    expect_setequal(employee$emp_id, r$emp_id)
})

test_that("kibior::joins, simple nomial, natural join onto itself, remote vs. remote, left join", {
    r <- kc$left_join("employee", "employee")
    expect_setequal(names(r), names(employee))
    expect_equal(nrow(r), nrow(employee))
    expect_setequal(employee$emp_id, r$emp_id)
})

test_that("kibior::joins, simple nomial, natural join onto itself, remote vs. remote, right join", {
    r <- kc$right_join("employee", "employee")
    expect_setequal(names(r), names(employee))
    expect_equal(nrow(r), nrow(employee))
    expect_setequal(employee$emp_id, r$emp_id)
})

test_that("kibior::joins, simple nomial, natural join onto itself, remote vs. remote, semi join", {
    r <- kc$semi_join("employee", "employee")
    expect_setequal(names(r), names(employee))
    expect_equal(nrow(r), nrow(employee))
    expect_setequal(employee$emp_id, r$emp_id)
})

test_that("kibior::joins, simple nomial, natural join onto itself, remote vs. remote, anti join", {
    r <- kc$anti_join("employee", "employee")
    expect_setequal(names(r), names(employee))
    expect_equal(nrow(r), 0)
})




## VALIDATING no join possible if no names correspond


## local vs. local

test_that("kibior::joins, error when no by, local vs. local, inner join", {
    expect_error(kc$inner_join(employee, dept))
})

test_that("kibior::joins, error when no by, local vs. local, full join", {
    expect_error(kc$full_join(employee, dept))
})

test_that("kibior::joins, error when no by, local vs. local, left join", {
    expect_error(kc$left_join(employee, dept))
})

test_that("kibior::joins, error when no by, local vs. local, right join", {
    expect_error(kc$right_join(employee, dept))
})

test_that("kibior::joins, error when no by, local vs. local, semi join", {
    expect_error(kc$semi_join(employee, dept))
})

test_that("kibior::joins, error when no by, local vs. local, anti join", {
    expect_error(kc$anti_join(employee, dept))
})



## local vs. remote

test_that("kibior::joins, error when no by, local vs. remote, inner join", {
    expect_error(kc$inner_join(employee, "dept"))
})

test_that("kibior::joins, error when no by, local vs. remote, full join", {
    expect_error(kc$full_join(employee, "dept"))
})

test_that("kibior::joins, error when no by, local vs. remote, left join", {
    expect_error(kc$left_join(employee, "dept"))
})

test_that("kibior::joins, error when no by, local vs. remote, right join", {
    expect_error(kc$right_join(employee, "dept"))
})

test_that("kibior::joins, error when no by, local vs. remote, semi join", {
    expect_error(kc$semi_join(employee, "dept"))
})

test_that("kibior::joins, error when no by, local vs. remote, anti join", {
    expect_error(kc$anti_join(employee, "dept"))
})



## remote vs. local

test_that("kibior::joins, error when no by, remote vs. local, inner join", {
    expect_error(kc$inner_join("employee", dept))
})

test_that("kibior::joins, error when no by, remote vs. local, full join", {
    expect_error(kc$full_join("employee", dept))
})

test_that("kibior::joins, error when no by, remote vs. local, left join", {
    expect_error(kc$left_join("employee", dept))
})

test_that("kibior::joins, error when no by, remote vs. local, right join", {
    expect_error(kc$right_join("employee", dept))
})

test_that("kibior::joins, error when no by, remote vs. local, semi join", {
    expect_error(kc$semi_join("employee", dept))
})

test_that("kibior::joins, error when no by, remote vs. local, anti join", {
    expect_error(kc$anti_join("employee", dept))
})



## remote vs. remote

test_that("kibior::joins, error when no by, remote vs. remote, inner join", {
    expect_error(kc$inner_join("employee", "dept"))
})

test_that("kibior::joins, error when no by, remote vs. remote, full join", {
    expect_error(kc$full_join("employee", "dept"))
})

test_that("kibior::joins, error when no by, remote vs. remote, left join", {
    expect_error(kc$left_join("employee", "dept"))
})

test_that("kibior::joins, error when no by, remote vs. remote, right join", {
    expect_error(kc$right_join("employee", "dept"))
})

test_that("kibior::joins, error when no by, remote vs. remote, semi join", {
    expect_error(kc$semi_join("employee", "dept"))
})

test_that("kibior::joins, error when no by, remote vs. remote, anti join", {
    expect_error(kc$anti_join("employee", "dept"))
})







## VALIDATING keep_metadata


## local vs. local

test_that("kibior::joins, error when no by, local vs. local, inner join, with metadata", {
    expect_error(kc$inner_join(employee, dept), keep_metadata = TRUE)
})

test_that("kibior::joins, error when no by, local vs. local, full join, with metadata", {
    expect_error(kc$full_join(employee, dept), keep_metadata = TRUE)
})

test_that("kibior::joins, error when no by, local vs. local, left join, with metadata", {
    expect_error(kc$left_join(employee, dept), keep_metadata = TRUE)
})

test_that("kibior::joins, error when no by, local vs. local, right join, with metadata", {
    expect_error(kc$right_join(employee, dept), keep_metadata = TRUE)
})

test_that("kibior::joins, error when no by, local vs. local, semi join, with metadata", {
    expect_error(kc$semi_join(employee, dept), keep_metadata = TRUE)
})

test_that("kibior::joins, error when no by, local vs. local, anti join, with metadata", {
    expect_error(kc$anti_join(employee, dept), keep_metadata = TRUE)
})



## local vs. remote

test_that("kibior::joins, error when no by, local vs. remote, inner join, with metadata", {
    expect_error(kc$inner_join(employee, "dept"), keep_metadata = TRUE)
})

test_that("kibior::joins, error when no by, local vs. remote, full join, with metadata", {
    expect_error(kc$full_join(employee, "dept"), keep_metadata = TRUE)
})

test_that("kibior::joins, error when no by, local vs. remote, left join, with metadata", {
    expect_error(kc$left_join(employee, "dept"), keep_metadata = TRUE)
})

test_that("kibior::joins, error when no by, local vs. remote, right join, with metadata", {
    expect_error(kc$right_join(employee, "dept"), keep_metadata = TRUE)
})

test_that("kibior::joins, error when no by, local vs. remote, semi join, with metadata", {
    expect_error(kc$semi_join(employee, "dept"), keep_metadata = TRUE)
})

test_that("kibior::joins, error when no by, local vs. remote, anti join, with metadata", {
    expect_error(kc$anti_join(employee, "dept"), keep_metadata = TRUE)
})



## remote vs. local

test_that("kibior::joins, error when no by, remote vs. local, inner join, with metadata", {
    expect_error(kc$inner_join("employee", dept), keep_metadata = TRUE)
})

test_that("kibior::joins, error when no by, remote vs. local, full join, with metadata", {
    expect_error(kc$full_join("employee", dept), keep_metadata = TRUE)
})

test_that("kibior::joins, error when no by, remote vs. local, left join, with metadata", {
    expect_error(kc$left_join("employee", dept), keep_metadata = TRUE)
})

test_that("kibior::joins, error when no by, remote vs. local, right join, with metadata", {
    expect_error(kc$right_join("employee", dept), keep_metadata = TRUE)
})

test_that("kibior::joins, error when no by, remote vs. local, semi join, with metadata", {
    expect_error(kc$semi_join("employee", dept), keep_metadata = TRUE)
})

test_that("kibior::joins, error when no by, remote vs. local, anti join, with metadata", {
    expect_error(kc$anti_join("employee", dept), keep_metadata = TRUE)
})



## remote vs. remote

test_that("kibior::joins, error when no by, remote vs. remote, inner join, with metadata", {
    expect_error(kc$inner_join("employee", "dept"), keep_metadata = TRUE)
})

test_that("kibior::joins, error when no by, remote vs. remote, full join, with metadata", {
    expect_error(kc$full_join("employee", "dept"), keep_metadata = TRUE)
})

test_that("kibior::joins, error when no by, remote vs. remote, left join, with metadata", {
    expect_error(kc$left_join("employee", "dept"), keep_metadata = TRUE)
})

test_that("kibior::joins, error when no by, remote vs. remote, right join, with metadata", {
    expect_error(kc$right_join("employee", "dept"), keep_metadata = TRUE)
})

test_that("kibior::joins, error when no by, remote vs. remote, semi join, with metadata", {
    expect_error(kc$semi_join("employee", "dept"), keep_metadata = TRUE)
})

test_that("kibior::joins, error when no by, remote vs. remote, anti join, with metadata", {
    expect_error(kc$anti_join("employee", "dept"), keep_metadata = TRUE)
})







## VALIDATING Employee / Dept


# employee, id_col = emp_id (no kid)
#
# | emp_name | emp_id | dept_name       |
# |----------|--------|-----------------|
# | Harry    | 3415   | Finance         |
# | Sally    | 2241   | Sales           |
# | George   | 3401   | Finance         |
# | Harriet  | 2202   | Sales           |
# | Mary     | 1257   | Human Resources |
# | Tim      | 1123   | Executive       |

# dept, id_col = dept_id (no kid)
#
# | dept_id | name        | manager |
# |---------|-------------|---------|
# | 1       | Finance     | George  |
# | 2       | Sales       | Harriet |
# | 3       | Production  | Charles |


# col names  are different between the two ds to avoid "natural join" according 
# to names with dplyr *_joins. By default, same names will be used to join, but 
# these tests will not use this feature and try fixed col names.

# join_cols defined in helper.R


## local vs. local

test_that("kibior::joins, simple join with by, local vs. local, inner join", {
    r <- kc$inner_join(employee, dept, by = join_cols)
    dr <- dplyr::inner_join(employee, dept, by = join_cols)
    expect_setequal(names(r), names(dr))
    expect_equal(nrow(r), nrow(dr))
    expect_setequal(r$emp_id, dr$emp_id)
})

test_that("kibior::joins, simple join with by, local vs. local, full join", {
    r <- kc$full_join(employee, dept, by = join_cols)
    dr <- dplyr::full_join(employee, dept, by = join_cols)
    expect_setequal(names(r), names(dr))
    expect_equal(nrow(r), nrow(dr))
    expect_setequal(r$emp_id, dr$emp_id)
})

test_that("kibior::joins, simple join with by, local vs. local, left join", {
    r <- kc$left_join(employee, dept, by = join_cols)
    dr <- dplyr::left_join(employee, dept, by = join_cols)
    expect_setequal(names(r), names(dr))
    expect_equal(nrow(r), nrow(dr))
    expect_setequal(r$emp_id, dr$emp_id)
})

test_that("kibior::joins, simple join with by, local vs. local, right join", {
    r <- kc$right_join(employee, dept, by = join_cols)
    dr <- dplyr::right_join(employee, dept, by = join_cols)
    expect_setequal(names(r), names(dr))
    expect_equal(nrow(r), nrow(dr))
    expect_setequal(r$emp_id, dr$emp_id)
})

test_that("kibior::joins, simple join with by, local vs. local, semi join", {
    r <- kc$semi_join(employee, dept, by = join_cols)
    dr <- dplyr::semi_join(employee, dept, by = join_cols)
    expect_setequal(names(r), names(dr))
    expect_equal(nrow(r), nrow(dr))
    expect_setequal(r$emp_id, dr$emp_id)
})

test_that("kibior::joins, simple join with by, local vs. local, anti join", {
    r <- kc$anti_join(employee, dept, by = join_cols)
    dr <- dplyr::anti_join(employee, dept, by = join_cols)
    expect_setequal(names(r), names(dr))
    expect_equal(nrow(r), nrow(dr))
    expect_setequal(r$emp_id, dr$emp_id)
})



## local vs. remote

test_that("kibior::joins, simple join with by, local vs. remote, inner join", {
    r <- kc$inner_join(employee, "dept", by = join_cols)
    dr <- dplyr::inner_join(employee, dept, by = join_cols)
    expect_setequal(names(r), names(dr))
    expect_equal(nrow(r), nrow(dr))
    expect_setequal(r$emp_id, dr$emp_id)
})

test_that("kibior::joins, simple join with by, local vs. remote, full join", {
    r <- kc$full_join(employee, "dept", by = join_cols)
    dr <- dplyr::full_join(employee, dept, by = join_cols)
    expect_setequal(names(r), names(dr))
    expect_equal(nrow(r), nrow(dr))
    expect_setequal(r$emp_id, dr$emp_id)
})

test_that("kibior::joins, simple join with by, local vs. remote, left join", {
    r <- kc$left_join(employee, "dept", by = join_cols)
    dr <- dplyr::left_join(employee, dept, by = join_cols)
    expect_setequal(names(r), names(dr))
    expect_equal(nrow(r), nrow(dr))
    expect_setequal(r$emp_id, dr$emp_id)
})

test_that("kibior::joins, simple join with by, local vs. remote, right join", {
    r <- kc$right_join(employee, "dept", by = join_cols)
    dr <- dplyr::right_join(employee, dept, by = join_cols)
    expect_setequal(names(r), names(dr))
    expect_equal(nrow(r), nrow(dr))
    expect_setequal(r$emp_id, dr$emp_id)
})

test_that("kibior::joins, simple join with by, local vs. remote, semi join", {
    r <- kc$semi_join(employee, "dept", by = join_cols)
    dr <- dplyr::semi_join(employee, dept, by = join_cols)
    expect_setequal(names(r), names(dr))
    expect_equal(nrow(r), nrow(dr))
    expect_setequal(r$emp_id, dr$emp_id)
})

test_that("kibior::joins, simple join with by, local vs. remote, anti join", {
    r <- kc$anti_join(employee, "dept", by = join_cols)
    dr <- dplyr::anti_join(employee, dept, by = join_cols)
    expect_setequal(names(r), names(dr))
    expect_equal(nrow(r), nrow(dr))
    expect_setequal(r$emp_id, dr$emp_id)
})



## remote vs. local

test_that("kibior::joins, simple join with by, remote vs. local, inner join", {
    r <- kc$inner_join("employee", dept, by = join_cols)
    dr <- dplyr::inner_join(employee, dept, by = join_cols)
    expect_setequal(names(r), names(dr))
    expect_equal(nrow(r), nrow(dr))
    expect_setequal(r$emp_id, dr$emp_id)
})

test_that("kibior::joins, simple join with by, remote vs. local, full join", {
    r <- kc$full_join("employee", dept, by = join_cols)
    dr <- dplyr::full_join(employee, dept, by = join_cols)
    expect_setequal(names(r), names(dr))
    expect_equal(nrow(r), nrow(dr))
    expect_setequal(r$emp_id, dr$emp_id)
})

test_that("kibior::joins, simple join with by, remote vs. local, left join", {
    r <- kc$left_join("employee", dept, by = join_cols)
    dr <- dplyr::left_join(employee, dept, by = join_cols)
    expect_setequal(names(r), names(dr))
    expect_equal(nrow(r), nrow(dr))
    expect_setequal(r$emp_id, dr$emp_id)
})

test_that("kibior::joins, simple join with by, remote vs. local, right join", {
    r <- kc$right_join("employee", dept, by = join_cols)
    dr <- dplyr::right_join(employee, dept, by = join_cols)
    expect_setequal(names(r), names(dr))
    expect_equal(nrow(r), nrow(dr))
    expect_setequal(r$emp_id, dr$emp_id)
})

test_that("kibior::joins, simple join with by, remote vs. local, semi join", {
    r <- kc$semi_join("employee", dept, by = join_cols)
    dr <- dplyr::semi_join(employee, dept, by = join_cols)
    expect_setequal(names(r), names(dr))
    expect_equal(nrow(r), nrow(dr))
    expect_setequal(r$emp_id, dr$emp_id)
})

test_that("kibior::joins, simple join with by, remote vs. local, anti join", {
    r <- kc$anti_join("employee", dept, by = join_cols)
    dr <- dplyr::anti_join(employee, dept, by = join_cols)
    expect_setequal(names(r), names(dr))
    expect_equal(nrow(r), nrow(dr))
    expect_setequal(r$emp_id, dr$emp_id)
})



## remote vs. remote

test_that("kibior::joins, simple join with by, remote vs. remote, inner join", {
    r <- kc$inner_join("employee", "dept", by = join_cols)
    dr <- dplyr::inner_join(employee, dept, by = join_cols)
    expect_setequal(names(r), names(dr))
    expect_equal(nrow(r), nrow(dr))
    expect_setequal(r$emp_id, dr$emp_id)
})

test_that("kibior::joins, simple join with by, remote vs. remote, full join", {
    r <- kc$full_join("employee", "dept", by = join_cols)
    dr <- dplyr::full_join(employee, dept, by = join_cols)
    expect_setequal(names(r), names(dr))
    expect_equal(nrow(r), nrow(dr))
    expect_setequal(r$emp_id, dr$emp_id)
})

test_that("kibior::joins, simple join with by, remote vs. remote, left join", {
    r <- kc$left_join("employee", "dept", by = join_cols)
    dr <- dplyr::left_join(employee, dept, by = join_cols)
    expect_setequal(names(r), names(dr))
    expect_equal(nrow(r), nrow(dr))
    expect_setequal(r$emp_id, dr$emp_id)
})

test_that("kibior::joins, simple join with by, remote vs. remote, right join", {
    r <- kc$right_join("employee", "dept", by = join_cols)
    dr <- dplyr::right_join(employee, dept, by = join_cols)
    expect_setequal(names(r), names(dr))
    expect_equal(nrow(r), nrow(dr))
    expect_setequal(r$emp_id, dr$emp_id)
})

test_that("kibior::joins, simple join with by, remote vs. remote, semi join", {
    r <- kc$semi_join("employee", "dept", by = join_cols)
    dr <- dplyr::semi_join(employee, dept, by = join_cols)
    expect_setequal(names(r), names(dr))
    expect_equal(nrow(r), nrow(dr))
    expect_setequal(r$emp_id, dr$emp_id)
})

test_that("kibior::joins, simple join with by, remote vs. remote, anti join", {
    r <- kc$anti_join("employee", "dept", by = join_cols)
    dr <- dplyr::anti_join(employee, dept, by = join_cols)
    expect_setequal(names(r), names(dr))
    expect_equal(nrow(r), nrow(dr))
    expect_setequal(r$emp_id, dr$emp_id)
})






## With keep_metadata


## local vs. local
# No change since unused in local mode

test_that("kibior::joins, simple join with by, local vs. local, inner join, keep metadata", {
    r <- kc$inner_join(employee, dept, by = join_cols, keep_metadata = TRUE)
    dr <- dplyr::inner_join(employee, dept, by = join_cols)
    expect_setequal(names(r), names(dr))
    expect_equal(nrow(r), nrow(dr))
    expect_setequal(r$emp_id, dr$emp_id)
})

test_that("kibior::joins, simple join with by, local vs. local, full join, keep metadata", {
    r <- kc$full_join(employee, dept, by = join_cols, keep_metadata = TRUE)
    dr <- dplyr::full_join(employee, dept, by = join_cols)
    expect_setequal(names(r), names(dr))
    expect_equal(nrow(r), nrow(dr))
    expect_setequal(r$emp_id, dr$emp_id)
})

test_that("kibior::joins, simple join with by, local vs. local, left join, keep metadata", {
    r <- kc$left_join(employee, dept, by = join_cols, keep_metadata = TRUE)
    dr <- dplyr::left_join(employee, dept, by = join_cols)
    expect_setequal(names(r), names(dr))
    expect_equal(nrow(r), nrow(dr))
    expect_setequal(r$emp_id, dr$emp_id)
})

test_that("kibior::joins, simple join with by, local vs. local, right join, keep metadata", {
    r <- kc$right_join(employee, dept, by = join_cols, keep_metadata = TRUE)
    dr <- dplyr::right_join(employee, dept, by = join_cols)
    expect_setequal(names(r), names(dr))
    expect_equal(nrow(r), nrow(dr))
    expect_setequal(r$emp_id, dr$emp_id)
})

test_that("kibior::joins, simple join with by, local vs. local, semi join, keep metadata", {
    r <- kc$semi_join(employee, dept, by = join_cols, keep_metadata = TRUE)
    dr <- dplyr::semi_join(employee, dept, by = join_cols)
    expect_setequal(names(r), names(dr))
    expect_equal(nrow(r), nrow(dr))
    expect_setequal(r$emp_id, dr$emp_id)
})

test_that("kibior::joins, simple join with by, local vs. local, anti join, keep metadata", {
    r <- kc$anti_join(employee, dept, by = join_cols, keep_metadata = TRUE)
    dr <- dplyr::anti_join(employee, dept, by = join_cols)
    expect_setequal(names(r), names(dr))
    expect_equal(nrow(r), nrow(dr))
    expect_setequal(r$emp_id, dr$emp_id)
})



## local vs. remote

test_that("kibior::joins, simple join with by, local vs. remote, inner join, keep metadata", {
    r <- kc$inner_join(employee, "dept", by = join_cols, keep_metadata = TRUE)
    dr <- dplyr::inner_join(employee, dept, by = join_cols)
    expect_true("_index" %in% names(r))
    expect_true(length(names(r)) > length(names(dr)))
    expect_equal(nrow(r), nrow(dr))
    expect_setequal(r$emp_id, dr$emp_id)
})

test_that("kibior::joins, simple join with by, local vs. remote, full join, keep metadata", {
    r <- kc$full_join(employee, "dept", by = join_cols, keep_metadata = TRUE)
    dr <- dplyr::full_join(employee, dept, by = join_cols)
    expect_true("_index" %in% names(r))
    expect_true(length(names(r)) > length(names(dr)))
    expect_equal(nrow(r), nrow(dr))
    expect_setequal(r$emp_id, dr$emp_id)
})

test_that("kibior::joins, simple join with by, local vs. remote, left join, keep metadata", {
    r <- kc$left_join(employee, "dept", by = join_cols, keep_metadata = TRUE)
    dr <- dplyr::left_join(employee, dept, by = join_cols)
    expect_true("_index" %in% names(r))
    expect_true(length(names(r)) > length(names(dr)))
    expect_equal(nrow(r), nrow(dr))
    expect_setequal(r$emp_id, dr$emp_id)
})

test_that("kibior::joins, simple join with by, local vs. remote, right join, keep metadata", {
    r <- kc$right_join(employee, "dept", by = join_cols, keep_metadata = TRUE)
    dr <- dplyr::right_join(employee, dept, by = join_cols)
    expect_true("_index" %in% names(r))
    expect_true(length(names(r)) > length(names(dr)))
    expect_equal(nrow(r), nrow(dr))
    expect_setequal(r$emp_id, dr$emp_id)
})

test_that("kibior::joins, simple join with by, local vs. remote, semi join, keep metadata", {
    r <- kc$semi_join(employee, "dept", by = join_cols, keep_metadata = TRUE)
    dr <- dplyr::semi_join(employee, dept, by = join_cols)
    expect_false("_index" %in% names(r))                # false since semi join on smaller local data
    expect_false(length(names(r)) > length(names(dr)))  # false since semi join on smaller local data
    expect_equal(nrow(r), nrow(dr))
    expect_setequal(r$emp_id, dr$emp_id)
})

test_that("kibior::joins, simple join with by, local vs. remote, anti join, keep metadata", {
    r <- kc$anti_join(employee, "dept", by = join_cols, keep_metadata = TRUE)
    dr <- dplyr::anti_join(employee, dept, by = join_cols)
    expect_false("_index" %in% names(r))                # false since anti join on smaller local data
    expect_false(length(names(r)) > length(names(dr)))  # false since anti join on smaller local data
    expect_equal(nrow(r), nrow(dr))
    expect_setequal(r$emp_id, dr$emp_id)
})



## remote vs. local

test_that("kibior::joins, simple join with by, remote vs. local, inner join, keep metadata", {
    r <- kc$inner_join("employee", dept, by = join_cols, keep_metadata = TRUE)
    dr <- dplyr::inner_join(employee, dept, by = join_cols)
    expect_true("_index" %in% names(r))
    expect_true(length(names(r)) > length(names(dr)))
    expect_equal(nrow(r), nrow(dr))
    expect_setequal(r$emp_id, dr$emp_id)
})

test_that("kibior::joins, simple join with by, remote vs. local, full join, keep metadata", {
    r <- kc$full_join("employee", dept, by = join_cols, keep_metadata = TRUE)
    dr <- dplyr::full_join(employee, dept, by = join_cols)
    expect_true("_index" %in% names(r))
    expect_true(length(names(r)) > length(names(dr)))
    expect_equal(nrow(r), nrow(dr))
    expect_setequal(r$emp_id, dr$emp_id)
})

test_that("kibior::joins, simple join with by, remote vs. local, left join, keep metadata", {
    r <- kc$left_join("employee", dept, by = join_cols, keep_metadata = TRUE)
    dr <- dplyr::left_join(employee, dept, by = join_cols)
    expect_true("_index" %in% names(r))
    expect_true(length(names(r)) > length(names(dr)))
    expect_equal(nrow(r), nrow(dr))
    expect_setequal(r$emp_id, dr$emp_id)
})

test_that("kibior::joins, simple join with by, remote vs. local, right join, keep metadata", {
    r <- kc$right_join("employee", dept, by = join_cols, keep_metadata = TRUE)
    dr <- dplyr::right_join(employee, dept, by = join_cols)
    expect_true("_index" %in% names(r))
    expect_true(length(names(r)) > length(names(dr)))
    expect_equal(nrow(r), nrow(dr))
    expect_setequal(r$emp_id, dr$emp_id)
})

test_that("kibior::joins, simple join with by, remote vs. local, semi join, keep metadata", {
    r <- kc$semi_join("employee", dept, by = join_cols, keep_metadata = TRUE)
    dr <- dplyr::semi_join(employee, dept, by = join_cols)
    expect_true("_index" %in% names(r))
    expect_true(length(names(r)) > length(names(dr)))
    expect_equal(nrow(r), nrow(dr))
    expect_setequal(r$emp_id, dr$emp_id)
})

test_that("kibior::joins, simple join with by, remote vs. local, anti join, keep metadata", {
    r <- kc$anti_join("employee", dept, by = join_cols, keep_metadata = TRUE)
    dr <- dplyr::anti_join(employee, dept, by = join_cols)
    expect_true("_index" %in% names(r))
    expect_true(length(names(r)) > length(names(dr)))
    expect_equal(nrow(r), nrow(dr))
    expect_setequal(r$emp_id, dr$emp_id)
})



## remote vs. remote

test_that("kibior::joins, simple join with by, remote vs. remote, inner join, keep metadata", {
    r <- kc$inner_join("employee", "dept", by = join_cols, keep_metadata = TRUE)
    dr <- dplyr::inner_join(employee, dept, by = join_cols)
    expect_true("_index_left" %in% names(r))
    expect_true("_index_right" %in% names(r))
    expect_true(length(names(r)) > length(names(dr)))
    expect_equal(nrow(r), nrow(dr))
    expect_setequal(r$emp_id, dr$emp_id)
})

test_that("kibior::joins, simple join with by, remote vs. remote, full join, keep metadata", {
    r <- kc$full_join("employee", "dept", by = join_cols, keep_metadata = TRUE)
    dr <- dplyr::full_join(employee, dept, by = join_cols)
    expect_true("_index_left" %in% names(r))
    expect_true("_index_right" %in% names(r))
    expect_true(length(names(r)) > length(names(dr)))
    expect_equal(nrow(r), nrow(dr))
    expect_setequal(r$emp_id, dr$emp_id)
})

test_that("kibior::joins, simple join with by, remote vs. remote, left join, keep metadata", {
    r <- kc$left_join("employee", "dept", by = join_cols, keep_metadata = TRUE)
    dr <- dplyr::left_join(employee, dept, by = join_cols)
    expect_true("_index_left" %in% names(r))
    expect_true("_index_right" %in% names(r))
    expect_true(length(names(r)) > length(names(dr)))
    expect_equal(nrow(r), nrow(dr))
    expect_setequal(r$emp_id, dr$emp_id)
})

test_that("kibior::joins, simple join with by, remote vs. remote, right join, keep metadata", {
    r <- kc$right_join("employee", "dept", by = join_cols, keep_metadata = TRUE)
    dr <- dplyr::right_join(employee, dept, by = join_cols)
    expect_true("_index_left" %in% names(r))
    expect_true("_index_right" %in% names(r))
    expect_true(length(names(r)) > length(names(dr)))
    expect_equal(nrow(r), nrow(dr))
    expect_setequal(r$emp_id, dr$emp_id)
})

test_that("kibior::joins, simple join with by, remote vs. remote, semi join, keep metadata", {
    r <- kc$semi_join("employee", "dept", by = join_cols, keep_metadata = TRUE)
    dr <- dplyr::semi_join(employee, dept, by = join_cols)
    expect_false("_index_left" %in% names(r))
    expect_false("_index_right" %in% names(r))
    expect_true("_index" %in% names(r))
    expect_true(unique(r[["_index"]]) == "employee")
    expect_true(length(names(r)) > length(names(dr)))
    expect_equal(nrow(r), nrow(dr))
    expect_setequal(r$emp_id, dr$emp_id)
})

test_that("kibior::joins, simple join with by, remote vs. remote, anti join, keep metadata", {
    r <- kc$anti_join("employee", "dept", by = join_cols, keep_metadata = TRUE)
    dr <- dplyr::anti_join(employee, dept, by = join_cols)
    expect_false("_index_left" %in% names(r))
    expect_false("_index_right" %in% names(r))
    expect_true("_index" %in% names(r))
    expect_true(unique(r[["_index"]]) == "employee")
    expect_true(length(names(r)) > length(names(dr)))
    expect_equal(nrow(r), nrow(dr))
    expect_setequal(r$emp_id, dr$emp_id)
})






## No Query


## local vs. local
# query for local is thrown to dplyr::filter

test_that("kibior::joins, simple join without query, local vs. local, inner join", {
    r <- kc$inner_join(employee, dept, by = join_cols)
    dr <- dplyr::inner_join(employee, dept, by = join_cols)
    expect_setequal(names(r), names(dr))
    expect_equal(nrow(r), nrow(dr))
    expect_setequal(r$emp_id, dr$emp_id)
})

test_that("kibior::joins, simple join without query, local vs. local, full join", {
    r <- kc$full_join(employee, dept, by = join_cols)
    dr <- dplyr::full_join(employee, dept, by = join_cols)
    expect_setequal(names(r), names(dr))
    expect_equal(nrow(r), nrow(dr))
    expect_setequal(r$emp_id, dr$emp_id)
})

test_that("kibior::joins, simple join without query, local vs. local, left join", {
    r <- kc$left_join(employee, dept, by = join_cols)
    dr <- dplyr::left_join(employee, dept, by = join_cols)
    expect_setequal(names(r), names(dr))
    expect_equal(nrow(r), nrow(dr))
    expect_setequal(r$emp_id, dr$emp_id)
})

test_that("kibior::joins, simple join without query, local vs. local, right join", {
    r <- kc$right_join(employee, dept, by = join_cols)
    dr <- dplyr::right_join(employee, dept, by = join_cols)
    expect_setequal(names(r), names(dr))
    expect_equal(nrow(r), nrow(dr))
    expect_setequal(r$emp_id, dr$emp_id)
})

test_that("kibior::joins, simple join without query, local vs. local, semi join", {
    r <- kc$semi_join(employee, dept, by = join_cols)
    dr <- dplyr::semi_join(employee, dept, by = join_cols)
    expect_setequal(names(r), names(dr))
    expect_equal(nrow(r), nrow(dr))
    expect_setequal(r$emp_id, dr$emp_id)
})

test_that("kibior::joins, simple join without query, local vs. local, anti join", {
    r <- kc$anti_join(employee, dept, by = join_cols)
    dr <- dplyr::anti_join(employee, dept, by = join_cols)
    expect_setequal(names(r), names(dr))
    expect_equal(nrow(r), nrow(dr))
    expect_setequal(r$emp_id, dr$emp_id)
})



## local vs. remote

test_that("kibior::joins, simple join without query, local vs. remote, inner join", {
    r <- kc$inner_join(employee, "dept", by = join_cols)
    dr <- dplyr::inner_join(employee, dept, by = join_cols)
    expect_setequal(names(r), names(dr))
    expect_equal(nrow(r), nrow(dr))
    expect_setequal(r$emp_id, dr$emp_id)
})

test_that("kibior::joins, simple join without query, local vs. remote, full join", {
    r <- kc$full_join(employee, "dept", by = join_cols)
    dr <- dplyr::full_join(employee, dept, by = join_cols)
    expect_setequal(names(r), names(dr))
    expect_equal(nrow(r), nrow(dr))
    expect_setequal(r$emp_id, dr$emp_id)
})

test_that("kibior::joins, simple join without query, local vs. remote, left join", {
    r <- kc$left_join(employee, "dept", by = join_cols)
    dr <- dplyr::left_join(employee, dept, by = join_cols)
    expect_setequal(names(r), names(dr))
    expect_equal(nrow(r), nrow(dr))
    expect_setequal(r$emp_id, dr$emp_id)
})

test_that("kibior::joins, simple join without query, local vs. remote, right join", {
    r <- kc$right_join(employee, "dept", by = join_cols)
    dr <- dplyr::right_join(employee, dept, by = join_cols)
    expect_setequal(names(r), names(dr))
    expect_equal(nrow(r), nrow(dr))
    expect_setequal(r$emp_id, dr$emp_id)
})

test_that("kibior::joins, simple join without query, local vs. remote, semi join", {
    r <- kc$semi_join(employee, "dept", by = join_cols)
    dr <- dplyr::semi_join(employee, dept, by = join_cols)
    expect_equal(length(names(r)), length(names(dr)) ) # same length
    expect_equal(nrow(r), nrow(dr))
    expect_setequal(r$emp_id, dr$emp_id)
})

test_that("kibior::joins, simple join without query, local vs. remote, anti join", {
    r <- kc$anti_join(employee, "dept", by = join_cols)
    dr <- dplyr::anti_join(employee, dept, by = join_cols)
    expect_equal(length(names(r)), length(names(dr)) ) # same length
    expect_equal(nrow(r), nrow(dr))
    expect_setequal(r$emp_id, dr$emp_id)
})



## remote vs. local

test_that("kibior::joins, simple join without query, remote vs. local, inner join", {
    r <- kc$inner_join("employee", dept, by = join_cols)
    dr <- dplyr::inner_join(employee, dept, by = join_cols)
    expect_setequal(names(r), names(dr))
    expect_equal(nrow(r), nrow(dr))
    expect_setequal(r$emp_id, dr$emp_id)
})

test_that("kibior::joins, simple join without query, remote vs. local, full join", {
    r <- kc$full_join("employee", dept, by = join_cols)
    dr <- dplyr::full_join(employee, dept, by = join_cols)
    expect_setequal(names(r), names(dr))
    expect_equal(nrow(r), nrow(dr))
    expect_setequal(r$emp_id, dr$emp_id)
})

test_that("kibior::joins, simple join without query, remote vs. local, left join", {
    r <- kc$left_join("employee", dept, by = join_cols)
    dr <- dplyr::left_join(employee, dept, by = join_cols)
    expect_setequal(names(r), names(dr))
    expect_equal(nrow(r), nrow(dr))
    expect_setequal(r$emp_id, dr$emp_id)
})

test_that("kibior::joins, simple join without query, remote vs. local, right join", {
    r <- kc$right_join("employee", dept, by = join_cols)
    dr <- dplyr::right_join(employee, dept, by = join_cols)
    expect_setequal(names(r), names(dr))
    expect_equal(nrow(r), nrow(dr))
    expect_setequal(r$emp_id, dr$emp_id)
})

test_that("kibior::joins, simple join without query, remote vs. local, semi join", {
    r <- kc$semi_join("employee", dept, by = join_cols)
    dr <- dplyr::semi_join(employee, dept, by = join_cols)
    expect_equal(length(names(r)), length(names(dr)) ) # same length
    expect_equal(nrow(r), nrow(dr))
    expect_setequal(r$emp_id, dr$emp_id)
})

test_that("kibior::joins, simple join without query, remote vs. local, anti join", {
    r <- kc$anti_join("employee", dept, by = join_cols)
    dr <- dplyr::anti_join(employee, dept, by = join_cols)
    expect_equal(length(names(r)), length(names(dr)) ) # same length
    expect_equal(nrow(r), nrow(dr))
    expect_setequal(r$emp_id, dr$emp_id)
})



## remote vs. remote

test_that("kibior::joins, simple join without query, remote vs. remote, inner join", {
    r <- kc$inner_join("employee", "dept", by = join_cols)
    dr <- dplyr::inner_join(employee, dept, by = join_cols)
    expect_false("kid_left" %in% names(r))
    expect_false("kid_right" %in% names(r))
    expect_setequal(names(r), names(dr))
    expect_equal(nrow(r), nrow(dr))
    expect_setequal(r$emp_id, dr$emp_id)
})

test_that("kibior::joins, simple join without query, remote vs. remote, full join", {
    r <- kc$full_join("employee", "dept", by = join_cols)
    dr <- dplyr::full_join(employee, dept, by = join_cols)
    expect_false("kid_left" %in% names(r))
    expect_false("kid_right" %in% names(r))
    expect_setequal(names(r), names(dr))
    expect_equal(nrow(r), nrow(dr))
    expect_setequal(r$emp_id, dr$emp_id)
})

test_that("kibior::joins, simple join without query, remote vs. remote, left join", {
    r <- kc$left_join("employee", "dept", by = join_cols)
    dr <- dplyr::left_join(employee, dept, by = join_cols)
    expect_false("kid_left" %in% names(r))
    expect_false("kid_right" %in% names(r))
    expect_setequal(names(r), names(dr))
    expect_equal(nrow(r), nrow(dr))
    expect_setequal(r$emp_id, dr$emp_id)
})

test_that("kibior::joins, simple join without query, remote vs. remote, right join", {
    r <- kc$right_join("employee", "dept", by = join_cols)
    dr <- dplyr::right_join(employee, dept, by = join_cols)
    expect_false("kid_left" %in% names(r))
    expect_false("kid_right" %in% names(r))
    expect_setequal(names(r), names(dr))
    expect_equal(nrow(r), nrow(dr))
    expect_setequal(r$emp_id, dr$emp_id)
})

test_that("kibior::joins, simple join without query, remote vs. remote, semi join", {
    r <- kc$semi_join("employee", "dept", by = join_cols)
    dr <- dplyr::semi_join(employee, dept, by = join_cols)
    expect_false("kid_left" %in% names(r))
    expect_false("kid_right" %in% names(r))
    expect_setequal(names(r), names(dr))
    expect_equal(nrow(r), nrow(dr))
    expect_setequal(r$emp_id, dr$emp_id)
})

test_that("kibior::joins, simple join without query, remote vs. remote, anti join", {
    r <- kc$anti_join("employee", "dept", by = join_cols)
    dr <- dplyr::anti_join(employee, dept, by = join_cols)
    expect_false("kid_left" %in% names(r))
    expect_false("kid_right" %in% names(r))
    expect_setequal(names(r), names(dr))
    expect_equal(nrow(r), nrow(dr))
    expect_setequal(r$emp_id, dr$emp_id)
})











## Query


## local vs. local

test_that("kibior::joins, simple join with query, local vs. local, inner join", {
    ee <- employee %>% dplyr::filter(!!query_local)
    r <- kc$inner_join(ee, dept, by = join_cols)
    dr <- dplyr::inner_join(ee, dept, by = join_cols)
    expect_setequal(names(r), names(dr))
    expect_equal(nrow(r), nrow(dr))
    expect_setequal(r$emp_id, dr$emp_id)
})

test_that("kibior::joins, simple join with query, local vs. local, full join", {
    ee <- employee %>% dplyr::filter(!!query_local)
    r <- kc$full_join(ee, dept, by = join_cols)
    dr <- dplyr::full_join(ee, dept, by = join_cols)
    expect_setequal(names(r), names(dr))
    expect_equal(nrow(r), nrow(dr))
    expect_setequal(r$emp_id, dr$emp_id)
})

test_that("kibior::joins, simple join with query, local vs. local, left join", {
    ee <- employee %>% dplyr::filter(!!query_local)
    r <- kc$left_join(ee, dept, by = join_cols)
    dr <- dplyr::left_join(ee, dept, by = join_cols)
    expect_setequal(names(r), names(dr))
    expect_equal(nrow(r), nrow(dr))
    expect_setequal(r$emp_id, dr$emp_id)
})

test_that("kibior::joins, simple join with query, local vs. local, right join", {
    ee <- employee %>% dplyr::filter(!!query_local)
    r <- kc$right_join(ee, dept, by = join_cols)
    dr <- dplyr::right_join(ee, dept, by = join_cols)
    expect_setequal(names(r), names(dr))
    expect_equal(nrow(r), nrow(dr))
    expect_setequal(r$emp_id, dr$emp_id)
})

test_that("kibior::joins, simple join with query, local vs. local, semi join", {
    ee <- employee %>% dplyr::filter(!!query_local)
    r <- kc$semi_join(ee, dept, by = join_cols)
    dr <- dplyr::semi_join(ee, dept, by = join_cols)
    expect_setequal(names(r), names(dr))
    expect_equal(nrow(r), nrow(dr))
    expect_setequal(r$emp_id, dr$emp_id)
})

test_that("kibior::joins, simple join with query, local vs. local, anti join", {
    ee <- employee %>% dplyr::filter(!!query_local)
    r <- kc$anti_join(ee, dept, by = join_cols)
    dr <- dplyr::anti_join(ee, dept, by = join_cols)
    expect_setequal(names(r), names(dr))
    expect_equal(nrow(r), nrow(dr))
    expect_setequal(r$emp_id, dr$emp_id)
})



## local vs. remote

test_that("kibior::joins, simple join with query, local vs. remote, inner join", {
    ee <- employee %>% dplyr::filter(!!query_local)
    r <- kc$inner_join(ee, "dept", by = join_cols)
    dr <- dplyr::inner_join(ee, dept, by = join_cols)
    expect_setequal(names(r), names(dr))
    expect_equal(nrow(r), nrow(dr))
    expect_setequal(r$emp_id, dr$emp_id)
})

test_that("kibior::joins, simple join with query, local vs. remote, full join", {
    ee <- employee %>% dplyr::filter(!!query_local)
    r <- kc$full_join(ee, "dept", by = join_cols)
    dr <- dplyr::full_join(ee, dept, by = join_cols)
    expect_setequal(names(r), names(dr))
    expect_equal(nrow(r), nrow(dr))
    expect_setequal(r$emp_id, dr$emp_id)
})

test_that("kibior::joins, simple join with query, local vs. remote, left join", {
    ee <- employee %>% dplyr::filter(!!query_local)
    r <- kc$left_join(ee, "dept", by = join_cols)
    dr <- dplyr::left_join(ee, dept, by = join_cols)
    expect_setequal(names(r), names(dr))
    expect_equal(nrow(r), nrow(dr))
    expect_setequal(r$emp_id, dr$emp_id)
})

test_that("kibior::joins, simple join with query, local vs. remote, right join", {
    ee <- employee %>% dplyr::filter(!!query_local)
    r <- kc$right_join(ee, "dept", by = join_cols)
    dr <- dplyr::right_join(ee, dept, by = join_cols)
    expect_setequal(names(r), names(dr))
    expect_equal(nrow(r), nrow(dr))
    expect_setequal(r$emp_id, dr$emp_id)
})

test_that("kibior::joins, simple join with query, local vs. remote, semi join", {
    ee <- employee %>% dplyr::filter(!!query_local)
    r <- kc$semi_join(ee, "dept", by = join_cols)
    dr <- dplyr::semi_join(ee, dept, by = join_cols)
    expect_equal(length(names(r)), length(names(dr)) ) # same length
    expect_equal(nrow(r), nrow(dr))
    expect_setequal(r$emp_id, dr$emp_id)
})

test_that("kibior::joins, simple join with query, local vs. remote, anti join", {
    ee <- employee %>% dplyr::filter(!!query_local)
    r <- kc$anti_join(ee, "dept", by = join_cols)
    dr <- dplyr::anti_join(ee, dept, by = join_cols)
    expect_equal(length(names(r)), length(names(dr)) ) # same length
    expect_equal(nrow(r), nrow(dr))
    expect_setequal(r$emp_id, dr$emp_id)
})



## remote vs. local

test_that("kibior::joins, simple join with query, remote vs. local, inner join", {
    ee <- employee %>% dplyr::filter(!!query_local)
    r <- kc$inner_join("employee", dept, by = join_cols, left_query = query_remote)
    dr <- dplyr::inner_join(ee, dept, by = join_cols)
    expect_setequal(names(r), names(dr))
    expect_equal(nrow(r), nrow(dr))
    expect_setequal(r$emp_id, dr$emp_id)
})

test_that("kibior::joins, simple join with query, remote vs. local, full join", {
    ee <- employee %>% dplyr::filter(!!query_local)
    r <- kc$full_join("employee", dept, by = join_cols, left_query = query_remote)
    dr <- dplyr::full_join(ee, dept, by = join_cols)
    expect_setequal(names(r), names(dr))
    expect_equal(nrow(r), nrow(dr))
    expect_setequal(r$emp_id, dr$emp_id)
})

test_that("kibior::joins, simple join with query, remote vs. local, left join", {
    ee <- employee %>% dplyr::filter(!!query_local)
    r <- kc$left_join("employee", dept, by = join_cols, left_query = query_remote)
    dr <- dplyr::left_join(ee, dept, by = join_cols)
    expect_setequal(names(r), names(dr))
    expect_equal(nrow(r), nrow(dr))
    expect_setequal(r$emp_id, dr$emp_id)
})

test_that("kibior::joins, simple join with query, remote vs. local, right join", {
    ee <- employee %>% dplyr::filter(!!query_local)
    r <- kc$right_join("employee", dept, by = join_cols, left_query = query_remote)
    dr <- dplyr::right_join(ee, dept, by = join_cols)
    expect_setequal(names(r), names(dr))
    expect_equal(nrow(r), nrow(dr))
    expect_setequal(r$emp_id, dr$emp_id)
})

test_that("kibior::joins, simple join with query, remote vs. local, semi join", {
    ee <- employee %>% dplyr::filter(!!query_local)
    r <- kc$semi_join("employee", dept, by = join_cols, left_query = query_remote)
    dr <- dplyr::semi_join(ee, dept, by = join_cols)
    expect_equal(length(names(r)), length(names(dr)) ) # same length
    expect_equal(nrow(r), nrow(dr))
    expect_setequal(r$emp_id, dr$emp_id)
})

test_that("kibior::joins, simple join with query, remote vs. local, anti join", {
    ee <- employee %>% dplyr::filter(!!query_local)
    r <- kc$anti_join("employee", dept, by = join_cols, left_query = query_remote)
    dr <- dplyr::anti_join(ee, dept, by = join_cols)
    expect_equal(length(names(r)), length(names(dr)) ) # same length
    expect_equal(nrow(r), nrow(dr))
    expect_setequal(r$emp_id, dr$emp_id)
})



## remote vs. remote

test_that("kibior::joins, simple join with query, remote vs. remote, inner join", {
    ee <- employee %>% dplyr::filter(!!query_local)
    r <- kc$inner_join("employee", "dept", by = join_cols, left_query = query_remote)
    dr <- dplyr::inner_join(ee, dept, by = join_cols)
    expect_false("kid_left" %in% names(r))
    expect_false("kid_right" %in% names(r))
    expect_setequal(names(r), names(dr))
    expect_equal(nrow(r), nrow(dr))
    expect_setequal(r$emp_id, dr$emp_id)
})

test_that("kibior::joins, simple join with query, remote vs. remote, full join", {
    ee <- employee %>% dplyr::filter(!!query_local)
    r <- kc$full_join("employee", "dept", by = join_cols, left_query = query_remote)
    dr <- dplyr::full_join(ee, dept, by = join_cols)
    expect_false("kid_left" %in% names(r))
    expect_false("kid_right" %in% names(r))
    expect_setequal(names(r), names(dr))
    expect_equal(nrow(r), nrow(dr))
    expect_setequal(r$emp_id, dr$emp_id)
})

test_that("kibior::joins, simple join with query, remote vs. remote, left join", {
    ee <- employee %>% dplyr::filter(!!query_local)
    r <- kc$left_join("employee", "dept", by = join_cols, left_query = query_remote)
    dr <- dplyr::left_join(ee, dept, by = join_cols)
    expect_false("kid_left" %in% names(r))
    expect_false("kid_right" %in% names(r))
    expect_setequal(names(r), names(dr))
    expect_equal(nrow(r), nrow(dr))
    expect_setequal(r$emp_id, dr$emp_id)
})

test_that("kibior::joins, simple join with query, remote vs. remote, right join", {
    ee <- employee %>% dplyr::filter(!!query_local)
    r <- kc$right_join("employee", "dept", by = join_cols, left_query = query_remote)
    dr <- dplyr::right_join(ee, dept, by = join_cols)
    expect_false("kid_left" %in% names(r))
    expect_false("kid_right" %in% names(r))
    expect_setequal(names(r), names(dr))
    expect_equal(nrow(r), nrow(dr))
    expect_setequal(r$emp_id, dr$emp_id)
})

test_that("kibior::joins, simple join with query, remote vs. remote, semi join", {
    ee <- employee %>% dplyr::filter(!!query_local)
    r <- kc$semi_join("employee", "dept", by = join_cols, left_query = query_remote)
    dr <- dplyr::semi_join(ee, dept, by = join_cols)
    expect_false("kid_left" %in% names(r))
    expect_false("kid_right" %in% names(r))
    expect_setequal(names(r), names(dr))
    expect_equal(nrow(r), nrow(dr))
    expect_setequal(r$emp_id, dr$emp_id)
})

test_that("kibior::joins, simple join with query, remote vs. remote, anti join", {
    ee <- employee %>% dplyr::filter(!!query_local)
    r <- kc$anti_join("employee", "dept", by = join_cols, left_query = query_remote)
    dr <- dplyr::anti_join(ee, dept, by = join_cols)
    expect_false("kid_left" %in% names(r))
    expect_false("kid_right" %in% names(r))
    expect_setequal(names(r), names(dr))
    expect_equal(nrow(r), nrow(dr))
    expect_setequal(r$emp_id, dr$emp_id)
})








## SUPPLEMENT 


# fields
# bulk_size
# max_size