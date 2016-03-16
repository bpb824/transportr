sd_section(
  "Google API Utilities",
  "These functions are used to interact with Google APIs.",
  c("decodeLineR", "getGoogleDirections",
    "lines2points","route2shape","geocode_place",
    "travel_time","gDirsToShape")
)
sd_section(
  "GTFS Utilities",
  "These functions are used to download and prepare General Transit Feed Specification (GTFS) data.",
  c("fetchFeed","exportRouteShape",
    "gtfs_to_excel","exportStopShape")
)

sd_section(
  "Census Data Utilities",
  "These functions are used to query and/or analyze Census and American Community Survey Data",
  c("transit_pops")
)

sd_section(
  "General Use Functions",
  "These functions are used to peform various data handling and analysis tasks.",
  c("list2frame","naturalBreaks")
)
