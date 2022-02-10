club_name_mapping <- function(club_name){
  
  if(is.na(club_name)){
    return(club_name)
  }
  
  # remove special characters such as apostrophs
  club_name <- stri_trans_general(club_name, id = "Latin-ASCII")
  
  club_name <- tolower(club_name)
  
  # currently bundesliga
  if(str_detect(club_name, ".*bayern.*")){
    if(endsWith(club_name, "ii")){
      return("FC Bayern Munich II")
    } else {
      return("FC Bayern Munich")
    }
  } else if(str_detect(club_name, ".*dortmund.*")){
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
  } else if(str_detect(club_name, "b.*gladbach")){
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
  } else if(str_detect(club_name, ".*fc.*k.*ln|cologne")){
    return("1. FC Koln")
  } else if(str_detect(club_name, ".*union.*berlin.*")){
    return("1. FC Union Berlin")
  } else if(str_detect(club_name, ".*bielefeld")){
    return("Arminia Bielefeld")
  } else if(str_detect(club_name, ".*bochum")){
    return("VfL Bochum")
  } else if(str_detect(club_name, ".*greuther.*f.*rth")){
    return("SpVgg Greuther Furth")
  } 
  
  # currently 2. bundesliga or lower
  else if(str_detect(club_name, ".*schalke.*[04]?")){
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
  } else if(str_detect(club_name, ".*d.sseldorf")){
    return("Fortuna Dusseldorf")
  } else if(str_detect(club_name, ".*k.*lautern")){
    return("1. FC Kaiserslautern")
  } else if(str_detect(club_name, ".*uerdingen.*")){
    return("KFC Uerdingen 05")
  } else if(str_detect(club_name, "hamburg.*")){
    return("Hamburger SV")
  } else if(str_detect(club_name, ".*hansa.*")){
    return("FC Hansa Rostock")
  } else if(str_detect(club_name, ".*1860 m.*")){
    return("TSV 1860 Munich")
  } else if(str_detect(club_name, ".*duisburg")){
    return("MSV Duisburg")
  } else if(str_detect(club_name, ".*n[u\u00fc]r(em|n)berg")){
    return("1. FC Nurnberg")
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
  } else if(str_detect(club_name, ".*heidenheim.*")){
    return("1. FC Heidenheim")
  }
  
  
  
  ################ Ligue 1 ##########################
  else if(str_detect(club_name, ".*bastia")){
    return("SC Bastia")
  } else if(str_detect(club_name, ".*monaco.*")){
    return("AS Monaco")
  } else if(str_detect(club_name, ".*bordeaux.*")){
    return("Girondins Bordeaux")
  } else if(str_detect(club_name, ".*caen.*")){
    return("SM Caen")
  } else if(str_detect(club_name, ".*montpellier.*")){
    return("HSC Montpellier")
  } else if(str_detect(club_name, ".*dijon.*")){
    return("FCO Dijon")
  } else if(str_detect(club_name, ".*metz.*")){
    return("FC Metz")
  } else if(str_detect(club_name, ".*nancy.*|.*lorraine.*")){
    return("AS Nancy-Lorraine")
  } else if(str_detect(club_name, ".*nice|nizza*")){
    return("OGC Nice")
  } else if(str_detect(club_name, ".*marseille")){
    return("Olympique Marseille")
  } else if(str_detect(club_name, ".*lyon.*")){
    return("Olympique Lyon")
  } else if(str_detect(club_name, ".*nantes.*")){
    return("FC Nantes")
  } else if(str_detect(club_name, ".*angers.*")){
    return("SCO Angers")
  } else if(str_detect(club_name, ".*lille.*")){
    return("OSC Lille")
  } else if(str_detect(club_name, ".*rennes|rennais.*")){
    return("Stade Rennes")
  } else if(str_detect(club_name, ".*toulouse")){
    return("FC Toulouse")
  } else if(str_detect(club_name, ".*lorient")){
    return("FC Lorient")
  } else if(str_detect(club_name, ".*guingamp.*")){
    return("EA Guingamp")
  } else if(str_detect(club_name, ".*s.*t.*etienne")){
    return("AS Saint-Etienne")
  } else if(str_detect(club_name, ".*paris st.*|psg")){
    return("Paris Saint-Germain")
  } else if(str_detect(club_name, ".*troyes.*")){
    return("ES Troyes AC")
  } else if(str_detect(club_name, ".*amiens")){
    return("SC Amiens")
  } else if(str_detect(club_name, ".*stra[sß]b[ou]?u?rg")){
    return("Racing Strasbourg")
  } else if(str_detect(club_name, ".*reims")){
    return("Stade Reims")
  } else if(str_detect(club_name, ".*nimes")){
    return("Olympique Nimes")
  } else if(str_detect(club_name, ".*brest")){
    return("Stade Brest")
  } else if(str_detect(club_name, ".*lens")){
    return("RC Lens")
  } else if(str_detect(club_name, ".*clermont.*")){
    return("Clermont Foot")
  } else if(str_detect(club_name, ".*auxerre")){
    return("AJ Auxerre")
  } else if(str_detect(club_name, "gfc|gazelec.*ajaccio")){
    return("GFC Ajaccio")
  } else if(str_detect(club_name, "ac.*ajaccio|ajaccio")){
    return("AC Ajaccio")
  } else if(str_detect(club_name, ".*evian.*")){
    return("FC Evian Thonon Gaillard")
  } else if(str_detect(club_name, ".*arles.*")){
    return("AC Arles-Avignon")
  } else if(str_detect(club_name, ".*valenciennes.*")){
    return("FC Valenciennes")
  } else if(str_detect(club_name, ".*sochaux.*")){
    return("FC Sochaux")
  }
  
  
  
  

  return(str_to_title(club_name))
}
