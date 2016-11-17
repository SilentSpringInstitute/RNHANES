cycle_to_lookup_year <- function(cycle) {
  return(switch(cycle,
                "1999-2000" = "A 99-00",
                "2001-2002" = "A 01-02",
                "2003-2004" = "A 03-04",
                "2005-2006" = "A 05-06",
                "2007-2008" = "A 07-08",
                "2009-2010" = "A 09-10",
                "2011-2012" = "A 11-12"))
}

lookup <- function(column, cycle) {
  cycle <- cycle_to_lookup_year(cycle)
  return(lookup_table[lookup_table$match_column == column & lookup_table$year == cycle,])
}

lookup_dl <- function(column, cycle) {
  dl_column <- paste0("X", gsub('-', '.', cycle))

  if(!dl_column %in% names(lookup_table)) {
    return()
  }

  return(lookup(column, cycle)[,dl_column])
}
