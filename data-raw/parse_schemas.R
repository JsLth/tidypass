library(parsermd)

"%||%" <- function(x, y) if (is.null(x)) y else x

md <- as_tibble(parse_rmd(
  "https://raw.githubusercontent.com/woodpeck/postpass-ops/refs/heads/main/SCHEMA.md",
  parse_yaml = FALSE
))
md <- md[md$sec_h2 %in% "Main (Geometry) Tables" & !is.na(md$sec_h3) & md$type %in% "rmd_markdown",]
sch_names <- md$sec_h3

schema_tbl <- lapply(md$ast, function(schema) {
  clean <- schema |>
    trimws() |>
    gsub(",", "", x = _) |>
    gsub("\"", "", x = _) |>
    as.vector()
  clean <- clean[seq(which(startsWith(clean, "CREATE TABLE")), length(clean))]
  cols <- gsub(" .*", "", x = clean)
  ptypes <- gsub(".* ", "", x = clean)

  # no idea how to handle public.hstore and geometries
  filt <- !startsWith(ptypes, "public") &
    !ptypes %in% c("(", ");", "")

  cols <- cols[filt]
  ptypes <- ptypes[filt]
  ptypes <- lapply(
    ptypes,
    switch,
    bigint = character(1),
    text = character(1),
    integer = integer(1),
    real = double(1)
  )
  names(ptypes) <- cols

  as.data.frame(ptypes, check.names = FALSE)
})
names(schema_tbl) <- sch_names

usethis::use_data(schema_tbl, internal = TRUE)
