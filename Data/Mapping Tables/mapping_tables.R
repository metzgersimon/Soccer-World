club_name_mapping <- function(club_name){
  
  if(is.na(club_name)){
    return(club_name)
  }
  
  club_name <- tolower(club_name)
  
  # currently bundesliga
  if(str_detect(club_name, ".*bayern.*")){
    if(endsWith(club_name, "ii")){
      return("FC Bayern Munich II")
    } else {
      return("FC Bayern Munich")
    }
  } else if(str_detect(club_name, "bor.*mund.*")){
    if(endsWith(club_name, "ii")){
      return("Borussia Dortmund II")
    } else {
      return("Borussia Dortmund")
    }
  } else if(str_detect(club_name, "rb leipzig")){
    return("RB Leipzig")
  } else if(str_detect(club_name, "bay.*leverkusen")){
    return("Bayer 04 Leverkusen")
  } else if(str_detect(club_name, ".*wolfs*")){
    return("VfL Wolfsburg")
  } else if(str_detect(club_name, "bor.*gladbach")){
    return("Borussia Monchengladbach")
  } else if(str_detect(club_name, ".*hof.enheim")){
    return("TSG 1899 Hoffenheim")
  } else if(str_detect(club_name, "e.*frankfurt")){
    return("Eintracht Frankfurt")
  } else if(str_detect(club_name, ".*stuttgart")){
    return("VfB Stuttgart")
  } else if(str_detect(club_name, ".*freiburg")){
    return("SC Freiburg")
  } else if(str_detect(club_name, ".*hertha.*")){
    return("Hertha BSC")
  } else if(str_detect(club_name, ".*mainz.*")){
    return("1. FSV Mainz 05")
  } else if(str_detect(club_name, ".*augsburg.*")){
    return("FC Augsburg")
  } else if(str_detect(club_name, ".*fc.*k.*ln")){
    return("1. FC Koln")
  } else if(str_detect(club_name, ".*union.*berlin.*")){
    return("1. FC Union Berlin")
  } else if(str_detect(club_name, ".*bielefeld")){
    return("Arminia Bielefeld")
  } else if(str_detect(club_name, ".*bochum")){
    return("VfL Bochum")
  } else if(str_detect(club_name, ".*greuther.*f.*rth")){
    return("SpVgg Greuther F\u00fcrth")
  } 
  
  # currently 2. bundesliga or lower
  else if(str_detect(club_name, ".*schalke.*04")){
    return("FC Schalke 04")
  } else if(str_detect(club_name, ".*werder.bremen.*")){
    if(endsWith(club_name, "ii")){
      return("SV Werder Bremen II")
    } else {
      return("SV Werder Bremen")
    }
  } else if(str_detect(club_name, ".*st.*pauli")){
    return("FC St. Pauli")
  } else if(str_detect(club_name, ".*karlsruher.*")){
    return("Karlsruher SC")
  } else if(str_detect(club_name, "f.*d.sseldorf")){
    return("Fortuna D\u00fcsseldorf")
  } else if(str_detect(club_name, ".*k.*lautern")){
    return("1. FC Kaiserslautern")
  } else if(str_detect(club_name, ".*uerdingen.*")){
    return("KFC Uerdingen 05")
  } else if(str_detect(club_name, "hamburger.*")){
    return("Hamburger SV")
  } else if(str_detect(club_name, ".*hansa.*")){
    return("FC Hansa Rostock")
  } else if(str_detect(club_name, ".*1860 m.*")){
    return("TSV 1860 Munich")
  } else if(str_detect(club_name, ".*duisburg")){
    return("MSV Duisburg")
  } else if(str_detect(club_name, ".*n[u\u00fc]r(em|n)berg")){
    return("1. FC N\u00fcrnberg")
  } else if(str_detect(club_name, ".*ulm.18.*")){
    return("SSV Ulm 1846")
  } else if(str_detect(club_name, ".*unterhach.*")){
    return("SpVgg Unterhaching")
  } else if(str_detect(club_name, ".*ener.*cottbus")){
    return("FC Energie Cottbus")
  } else if(str_detect(club_name, "hannover.*")){
    return("Hannover 96")
  } else if(str_detect(club_name, ".*aachen")){
    return("Alemannia Aachen")
  } else if(str_detect(club_name, ".*braunschweig")){
    return("Eintracht Braunschweig")
  } else if(str_detect(club_name, ".*ingolstadt.*")){
    return("FC Ingolstadt 04")
  } else if(str_detect(club_name, ".*darmstadt.*")){
    return("SV Darmstadt 98")
  } else if(str_detect(club_name, ".*paderborn.*")){
    return("SC Paderborn 07")
  }
    

  return(str_to_title(club_name))
}

# clubs_fifa <- unique(fifa_team_stats_over_time_clean$club)
# clubs_fifa_mapped <- sapply(clubs_fifa, FUN = club_name_mapping)
# as.data.frame(clubs_fifa_mapped)
