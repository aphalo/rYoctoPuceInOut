library(jsonlite)

system2("YModule", "inventory", stdout = TRUE)
system2("YModule", "spectrl-01 get_allSettings", stdout = TRUE) |>
  paste(collapse = "\n", sep = "") |>
  gsub("^OK: .*= ", "", x = _) |>
  gsub("[.]$", "", x = _) |>
  fromJSON() -> module.settings

system2("YDataLogger", "spectrl-01 get_timeUTC", stdout = TRUE) |>
  gsub("^OK: .*= ", "", x = _) |>
  gsub(" \\[s\\] since Jan 1, 1970.$", "", x = _) |>
  as.numeric() |>
  as.POSIXct()
system2("YModule", "spectrl-01 get_lastLogs", stdout = TRUE) |>
  gsub("^OK: .*= ", "", x = _)

system2("YModule", "spectrl-01 get_allSettings", stdout = "test4json.txt")
