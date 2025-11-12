# client side
tbl <- pp_tbl("point")

queries <- list()
queries[[1]] <- filter(tbl, amenity == "fast_food")
queries[[2]] <- mutate(tbl, amenity = amenity)
queries[[3]] <- tbl |>
  group_by(amenity) |>
  summarise(amenity = amenity)
queries[[4]] <- tbl |>
  group_by(amenity) |>
  summarize(amenity = amenity)
queries[[5]] <- distinct(tbl, amenity)
queries[[6]] <- transmute(tbl, amenity = amenity)
queries[[7]] <- select(tbl, amenity)
queries[[8]] <- relocate(tbl, amenity, .after = building)
queries[[9]] <- arrange(tbl, amenity)
queries[[10]] <- rename(tbl, Amenity = amenity)
queries[[11]] <- rename_with(tbl, toupper, .cols = amenity)


test_that("sql conversion works in theory", {
  for (q in queries) {
    fmt <- dbplyr::remote_query(q)
    expect_match(fmt, "tags ->> 'amenity'", fixed = TRUE)
  }
})


test_that("sql conversion works in practice", {
  skip_on_cran()
  skip_if_offline("postpass.geofabrik.de")

  for (q in queries) {
    expect_no_failure(explain(q))
  }
})
