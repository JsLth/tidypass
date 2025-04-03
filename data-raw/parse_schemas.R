tables <- pp_tables(include_views = TRUE)
schemas <- lapply(tables, pp_schema)
names(schemas) <- tables
usethis::use_data(schemas, internal = TRUE, overwrite = TRUE)
