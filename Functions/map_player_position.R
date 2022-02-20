############## map_player_position #################
# inputs: actual_position
# outputs: should return the mapped position of a player 
# MAPPING:
# We want to map the player onto a grid of the following form:
# the grid is in a vertical form:i.e. the goalkeeper is at the bottom
# and the strikers at the top. 
# The grid has 6 rows and 5 columns. The position of the goalkeeper
# is therefore 1:3 (row 1 and column 3):whereas a striker is at 6:3
# (row 6:column 3)
map_player_position <- function(api_football_position, formation){
  mapped_position <- NULL
  
  if(formation == "5-4-1"){
    if(api_football_position == "1:1"){
      mapped_position <- "1:3.5"#"1:3.5)
    } else if(api_football_position == "2:5"){
      mapped_position <- "2:6"#"2:6)
    } else if(api_football_position == "2:4"){
      mapped_position <- "2:4.5"#"2:4.5)
    } else if(api_football_position == "2:3"){
      mapped_position <- "2:2.5"#"2:2.5)
    } else if(api_football_position == "2:2"){
      mapped_position <- "2:1.5"#"2:1.5)
    } else if(api_football_position == "2:1"){
      mapped_position <- "2:0"#"2:0)
    } else if(api_football_position == "3:4"){
      mapped_position <- "3:5.5"#"3:5.5)
    } else if(api_football_position == "3:3"){
      mapped_position <- "3:4.5"#"3:4.5)
    } else if(api_football_position == "3:2"){
      mapped_position <- "3:2.5"#"4:2.5)
    } else if(api_football_position == "3:1"){
      mapped_position <- "3:1.5"#"4:1.5)
    } else if(api_football_position == "4:1"){
      mapped_position <- "4:3.5"#"4:3.5)
    }
    
  } else if(formation == "5-3-2"){
    if(api_football_position == "1:1"){
      mapped_position <- "1:3.5"#"1:3.5)
    } else if(api_football_position == "2:5"){
      mapped_position <- "2:6"#"2:6)
    } else if(api_football_position == "2:4"){
      mapped_position <- "2:4.5"#"2:4.5)
    } else if(api_football_position == "2:3"){
      mapped_position <- "2:2.5"#"2:2.5)
    } else if(api_football_position == "2:2"){
      mapped_position <- "2:1.5"#"2:1.5)
    } else if(api_football_position == "2:1"){
      mapped_position <- "2:0"#"2:0)
    } else if(api_football_position == "3:3"){
      mapped_position <- "3:5.5"#"3:3.5)
    } else if(api_football_position == "3:2"){
      mapped_position <- "3:3.5"#"3:2)
    } else if(api_football_position == "3:1"){
      mapped_position <- "3:1.5"#"4:5)
    } else if(api_football_position == "4:2"){
      mapped_position <- "4:4.5"#"4:3.5)
    } else if(api_football_position == "4:1"){
      mapped_position <- "4:2.5"#"4:2)
    }
    
  } else if(formation == "4-5-1"){
    if(api_football_position == "1:1"){
      mapped_position <- "1:3.5"#"1:3.5)
    } else if(api_football_position == "2:4"){
      mapped_position <- "2:5.5"#"2:6)
    } else if(api_football_position == "2:3"){
      mapped_position <- "2:4.5"#"2:4.5)
    } else if(api_football_position == "2:2"){
      mapped_position <- "2:2.5"#"2:2.5)
    } else if(api_football_position == "2:1"){
      mapped_position <- "2:1.5"#"2:1.5)
    } else if(api_football_position == "3:5"){
      mapped_position <- "3:6"#"2:0)
    } else if(api_football_position == "3:4"){
      mapped_position <- "3:4.5"#"3:3.5)
    } else if(api_football_position == "3:3"){
      mapped_position <- "3:3"#"3:2)
    } else if(api_football_position == "3:2"){
      mapped_position <- "3:1.7"#"4:5)
    } else if(api_football_position == "3:1"){
      mapped_position <- "3:-0.5"#"4:3.5)
    } else if(api_football_position == "4:1"){
      mapped_position <- "4:2.5"#"4:2)
    }
    
  } else if(formation == "4-4-2"){
    if(api_football_position == "1:1"){
      mapped_position <- "1:3.5"
    } else if(api_football_position == "2:4"){
      mapped_position <- "2:5.5"
    } else if(api_football_position == "2:3"){
      mapped_position <- "2:4.5"
    } else if(api_football_position == "2:2"){
      mapped_position <- "2:2.5"
    } else if(api_football_position == "2:1"){
      mapped_position <- "2:1.5"
    } else if(api_football_position == "3:4"){
      mapped_position <- "3:5"
    } else if(api_football_position == "3:3"){
      mapped_position <- "3:4.5"
    } else if(api_football_position == "3:2"){
      mapped_position <- "3:2.5"
    } else if(api_football_position == "3:1"){
      mapped_position <- "3:1.5"
    } else if(api_football_position == "4:2"){
      mapped_position <- "4:4.5"
    } else if(api_football_position == "4:1"){
      mapped_position <- "4:2.5"
    }
  } else if(formation == "4-4-2 double 6"){
    if(api_football_position == "1:1"){
      mapped_position <- "1:3.5"
    } else if(api_football_position == "2:4"){
      mapped_position <- "2:5.5"
    } else if(api_football_position == "2:3"){
      mapped_position <- "2:4.5"
    } else if(api_football_position == "2:2"){
      mapped_position <- "2:2.5"
    } else if(api_football_position == "2:1"){
      mapped_position <- "2:1.5"
    } else if(api_football_position == "3:4"){
      mapped_position <- "3:5"
    } else if(api_football_position == "3:3"){
      mapped_position <- "3:4.5"
    } else if(api_football_position == "3:2"){
      mapped_position <- "3:2.5"
    } else if(api_football_position == "3:1"){
      mapped_position <- "3:1.5"
    } else if(api_football_position == "4:2"){
      mapped_position <- "4:2.5"
    } else if(api_football_position == "4:1"){
      mapped_position <- "4:1.5"
    }
  } else if(formation == "4-4-1-1"){
    if(api_football_position == "1:1"){
      mapped_position <- "1:3.5"
    } else if(api_football_position == "2:4"){
      mapped_position <- "2:5.5"
    } else if(api_football_position == "2:3"){
      mapped_position <- "2:4.5"
    } else if(api_football_position == "2:2"){
      mapped_position <- "2:2.5"
    } else if(api_football_position == "2:1"){
      mapped_position <- "2:1.5"
    } else if(api_football_position == "3:4"){
      mapped_position <- "3:5"
    } else if(api_football_position == "3:3"){
      mapped_position <- "3:4.5"
    } else if(api_football_position == "3:2"){
      mapped_position <- "3:2.5"
    } else if(api_football_position == "3:1"){
      mapped_position <- "3:1.5"
    } else if(api_football_position == "4:1"){
      mapped_position <- "4:3.5"
    } else if(api_football_position == "5:1"){
      mapped_position <- "5:3.5"
    }
  }else if(formation == "4-3-3"){
    if(api_football_position == "1:1"){
      mapped_position <- "1:3.5"
    } else if(api_football_position == "2:4"){
      mapped_position <- "2:5.5"
    } else if(api_football_position == "2:3"){
      mapped_position <- "2:4.5"
    } else if(api_football_position == "2:2"){
      mapped_position <- "2:2.5"
    } else if(api_football_position == "2:1"){
      mapped_position <- "2:1.5"
    } else if(api_football_position == "3:3"){
      mapped_position <- "3:5"
    } else if(api_football_position == "3:2"){
      mapped_position <- "3:3.5"
    } else if(api_football_position == "3:1"){
      mapped_position <- "3:2"
    } else if(api_football_position == "4:3"){
      mapped_position <- "4:5"
    } else if(api_football_position == "4:2"){
      mapped_position <- "4:3.5"
    } else if(api_football_position == "4:1"){
      mapped_position <- "4:2"
    }
    
  } else if(formation == "4-3-3 Attacking"){
    if(api_football_position == "1:1"){
      mapped_position <- "1:3.5"
    } else if(api_football_position == "2:4"){
      mapped_position <- "2:5.5"
    } else if(api_football_position == "2:3"){
      mapped_position <- "2:4.5"
    } else if(api_football_position == "2:2"){
      mapped_position <- "2:2.5"
    } else if(api_football_position == "2:1"){
      mapped_position <- "2:1.5"
    } else if(api_football_position == "3:3"){
      mapped_position <- "3:5"
    } else if(api_football_position == "3:2"){
      mapped_position <- "3:3.5"
    } else if(api_football_position == "3:1"){
      mapped_position <- "3:2"
    } else if(api_football_position == "4:3"){
      mapped_position <- "4:5"
    } else if(api_football_position == "4:2"){
      mapped_position <- "4:3.5"
    } else if(api_football_position == "4:1"){
      mapped_position <- "4:2"
    }
  } else if(formation == "4-3-3 Defending"){
    if(api_football_position == "1:1"){
      mapped_position <- "1:3.5"
    } else if(api_football_position == "2:4"){
      mapped_position <- "2:5.5"
    } else if(api_football_position == "2:3"){
      mapped_position <- "2:4.5"
    } else if(api_football_position == "2:2"){
      mapped_position <- "2:2.5"
    } else if(api_football_position == "2:1"){
      mapped_position <- "2:1.5"
    } else if(api_football_position == "3:3"){
      mapped_position <- "3:5"
    } else if(api_football_position == "3:2"){
      mapped_position <- "3:3.5"
    } else if(api_football_position == "3:1"){
      mapped_position <- "3:2"
    } else if(api_football_position == "4:3"){
      mapped_position <- "4:5"
    } else if(api_football_position == "4:2"){
      mapped_position <- "4:3.5"
    } else if(api_football_position == "4:1"){
      mapped_position <- "4:2"
    }
  } else if(formation == "4-3-2-1"){
    if(api_football_position == "1:1"){
      mapped_position <- "1:3.5"#"1:3.5)
    } else if(api_football_position == "2:4"){
      mapped_position <- "2:5.5"#"2:5.5)
    } else if(api_football_position == "2:3"){
      mapped_position <- "2:4.5"#"2:4.5)
    } else if(api_football_position == "2:2"){
      mapped_position <- "2:2.5"#"2:2.5)
    } else if(api_football_position == "2:1"){
      mapped_position <- "2:1.5"#"2:1.5)
    } else if(api_football_position == "3:3"){
      mapped_position <- "3:5"#"3:5)
    } else if(api_football_position == "3:2"){
      mapped_position <- "3:3.5"#"3:3.5)
    } else if(api_football_position == "3:1"){
      mapped_position <- "3:2"#"3:2)
    } else if(api_football_position == "4:2"){
      mapped_position <- "4:4.5"#"4:3.5)
    } else if(api_football_position == "4:1"){
      mapped_position <- "4:2.5"#"5:2.5)
    } else if(api_football_position == "5:1"){
      mapped_position <- "5:3.5"#"5:1.5)
    }
  } else if(formation == "4-3-1-2"){
    if(api_football_position == "1:1"){
      mapped_position <- "1:3.5"#"1:3.5)
    } else if(api_football_position == "2:4"){
      mapped_position <- "2:5.5"#"2:5.5)
    } else if(api_football_position == "2:3"){
      mapped_position <- "2:4.5"#"2:4.5)
    } else if(api_football_position == "2:2"){
      mapped_position <- "2:2.5"#"2:2.5)
    } else if(api_football_position == "2:1"){
      mapped_position <- "2:1.5"#"2:1.5)
    } else if(api_football_position == "3:3"){
      mapped_position <- "3:5"#"3:5)
    } else if(api_football_position == "3:2"){
      mapped_position <- "3:3.5"#"3:3.5)
    } else if(api_football_position == "3:1"){
      mapped_position <- "3:2"#"3:2)
    } else if(api_football_position == "4:1"){
      mapped_position <- "4:3.5"#"4:3.5)
    } else if(api_football_position == "5:2"){
      mapped_position <- "5:4.5"#"5:2.5)
    } else if(api_football_position == "5:1"){
      mapped_position <- "5:2.5"#"5:1.5)
    }
  } else if(formation == "4-2-3-1"){
    if(api_football_position == "1:1"){
      mapped_position <- "1:3.5"#"1:3.5)
    } else if(api_football_position == "2:4"){
      mapped_position <- "2:5.5"#"2:5.5)
    } else if(api_football_position == "2:3"){
      mapped_position <- "2:4.5"#"2:4.5)
    } else if(api_football_position == "2:2"){
      mapped_position <- "2:2.5"#"2:2.5)
    } else if(api_football_position == "2:1"){
      mapped_position <- "2:1.5"#"2:1.5)
    } else if(api_football_position == "3:2"){
      mapped_position <- "3:4.5"#"3:4.5)
    } else if(api_football_position == "3:1"){
      mapped_position <- "3:2.5"#"3:2.5)
    } else if(api_football_position == "4:3"){
      mapped_position <- "4:5"#"4:5)
    } else if(api_football_position == "4:2"){
      mapped_position <- "4:3.5"#"4:3.5)
    } else if(api_football_position == "4:1"){
      mapped_position <- "4:2"#"4:2)
    } else if(api_football_position == "5:1"){
      mapped_position <- "5:3.5"#"5:3.5)
    }
    
  } else if(formation == "4-2-2-2"){
    if(api_football_position == "1:1"){
      mapped_position <- "1:3.5"
    } else if(api_football_position == "2:4"){
      mapped_position <- "2:5.5"
    } else if(api_football_position == "2:3"){
      mapped_position <- "2:4.5"
    } else if(api_football_position == "2:2"){
      mapped_position <- "2:2.5"
    } else if(api_football_position == "2:1"){
      mapped_position <- "2:1.5"
    } else if(api_football_position == "3:2"){
      mapped_position <- "3:4.5"
    } else if(api_football_position == "3:1"){
      mapped_position <- "3:2.5"
    } else if(api_football_position == "4:2"){
      mapped_position <- "4:4.5"
    } else if(api_football_position == "4:1"){
      mapped_position <- "4:2.5"
    } else if(api_football_position == "5:2"){
      mapped_position <- "5:4.5"
    } else if(api_football_position == "5:1"){
      mapped_position <- "5:2.5"
    }
  } else if(formation == "4-1-4-1"){
    if(api_football_position == "1:1"){
      mapped_position <- "1:3.5"
    } else if(api_football_position == "2:4"){
      mapped_position <- "2:5.5"
    } else if(api_football_position == "2:3"){
      mapped_position <- "2:4.5"
    } else if(api_football_position == "2:2"){
      mapped_position <- "2:2.5"
    } else if(api_football_position == "2:1"){
      mapped_position <- "2:1.5"
    } else if(api_football_position == "3:1"){
      mapped_position <- "3:3.5"
    } else if(api_football_position == "4:4"){
      mapped_position <- "4:5.5"
    } else if(api_football_position == "4:3"){
      mapped_position <- "4:4.5"
    } else if(api_football_position == "4:2"){
      mapped_position <- "4:2.5"
    } else if(api_football_position == "4:1"){
      mapped_position <- "4:1.5"
    } else if(api_football_position == "5:1"){
      mapped_position <- "5:3.5"
    }
    
  } else if(formation == "4-1-3-2"){
    if(api_football_position == "1:1"){
      mapped_position <- "1:3.5"
    } else if(api_football_position == "2:4"){
      mapped_position <- "2:5.5"
    } else if(api_football_position == "2:3"){
      mapped_position <- "2:4.5"
    } else if(api_football_position == "2:2"){
      mapped_position <- "2:2.5"
    } else if(api_football_position == "2:1"){
      mapped_position <- "2:1.5"
    } else if(api_football_position == "3:1"){
      mapped_position <- "3:3.5"
    } else if(api_football_position == "4:3"){
      mapped_position <- "3:5"
    } else if(api_football_position == "4:2"){
      mapped_position <- "4:3.5"
    } else if(api_football_position == "4:1"){
      mapped_position <- "4:2"
    } else if(api_football_position == "5:2"){
      mapped_position <- "5:4.5"
    } else if(api_football_position == "5:1"){
      mapped_position <- "5:2.5"
    }
    
  } else if(formation == "3-5-2"){
    if(api_football_position == "1:1"){
      mapped_position <- "1:3"#"1:3.5)
    } else if(api_football_position == "2:3"){
      mapped_position <- "2:5.5"#"2:5.5)
    } else if(api_football_position == "2:2"){
      mapped_position <- "2:3"#"2:3.5)
    } else if(api_football_position == "2:1"){
      mapped_position <- "2:1.5"#"2:1.5)
    } else if(api_football_position == "3:5"){
      mapped_position <- "3:6"#"3:6)
    } else if(api_football_position == "3:4"){
      mapped_position <- "3:4.5"#"3:4.5)
    } else if(api_football_position == "3:3"){
      mapped_position <- "3:3"#"3:2.5)
    } else if(api_football_position == "3:2"){
      mapped_position <- "3:1.7"#"3:1.5)
    } else if(api_football_position == "3:1"){
      mapped_position <- "3:-0.5"#"3:0)
    } else if(api_football_position == "4:2"){
      mapped_position <- "4:4.5"#"4:3.5)
    } else if(api_football_position == "4:1"){
      mapped_position <- "4:2.5"#"4:2)
    }
    
  } else if(formation == "3-5-2 flat"){
    if(api_football_position == "1:1"){
      mapped_position <- "1:3.5"
    } else if(api_football_position == "2:3"){
      mapped_position <- "2:5.5"
    } else if(api_football_position == "2:2"){
      mapped_position <- "2:3.5"
    } else if(api_football_position == "2:1"){
      mapped_position <- "2:1.5"
    } else if(api_football_position == "3:5"){
      mapped_position <- "3:6"
    } else if(api_football_position == "3:4"){
      mapped_position <- "3:4.5"
    } else if(api_football_position == "3:3"){
      mapped_position <- "3:2.5"
    } else if(api_football_position == "3:2"){
      mapped_position <- "3:1.5"
    } else if(api_football_position == "3:1"){
      mapped_position <- "3:0"
    } else if(api_football_position == "4:2"){
      mapped_position <- "4:3.5"
    } else if(api_football_position == "4:1"){
      mapped_position <- "4:2"
    }
    
  }  else if(formation == "3-5-1-1"){
    if(api_football_position == "1:1"){
      mapped_position <- "1:3.5"
    } else if(api_football_position == "2:3"){
      mapped_position <- "2:5.5"
    } else if(api_football_position == "2:2"){
      mapped_position <- "2:3.5"
    } else if(api_football_position == "2:1"){
      mapped_position <- "2:1.5"
    } else if(api_football_position == "3:5"){
      mapped_position <- "3:6"
    } else if(api_football_position == "3:4"){
      mapped_position <- "3:4.5"
    } else if(api_football_position == "3:3"){
      mapped_position <- "3:2.5"
    } else if(api_football_position == "3:2"){
      mapped_position <- "3:1.5"
    } else if(api_football_position == "3:1"){
      mapped_position <- "3:0"
    } else if(api_football_position == "4:1"){
      mapped_position <- "4:3.5"
    } else if(api_football_position == "5:1"){
      mapped_position <- "5:3.5"
    }
    
  } else if(formation == "3-4-3"){
    if(api_football_position == "1:1"){
      mapped_position <- "1:3.5"
    } else if(api_football_position == "2:3"){
      mapped_position <- "2:5.5"
    } else if(api_football_position == "2:2"){
      mapped_position <- "2:3.5"
    } else if(api_football_position == "2:1"){
      mapped_position <- "2:1.5"
    } else if(api_football_position == "3:4"){
      mapped_position <- "3:5.5"
    } else if(api_football_position == "3:3"){
      mapped_position <- "3:4.5"
    } else if(api_football_position == "3:2"){
      mapped_position <- "3:2.5"
    } else if(api_football_position == "3:1"){
      mapped_position <- "3:1.5"
    } else if(api_football_position == "4:3"){
      mapped_position <- "4:5.5"
    } else if(api_football_position == "4:2"){
      mapped_position <- "4:3.5"
    } else if(api_football_position == "4:1"){
      mapped_position <- "4:1.5"
    }
    
  } else if(formation == "3-4-2-1"){
    if(api_football_position == "1:1"){
      mapped_position <- "1:3.5"#"1:3.5)
    } else if(api_football_position == "2:3"){
      mapped_position <- "2:5.5"#"2:5.5)
    } else if(api_football_position == "2:2"){
      mapped_position <- "2:3.5"#"2:3.5)
    } else if(api_football_position == "2:1"){
      mapped_position <- "2:1.5"#"2:1.5)
    } else if(api_football_position == "3:4"){
      mapped_position <- "3:5.5"#"3:5.5)
    } else if(api_football_position == "3:3"){
      mapped_position <- "3:4.5"#"3:4.5)
    } else if(api_football_position == "3:2"){
      mapped_position <- "3:2.5"#"3:2.5)
    } else if(api_football_position == "3:1"){
      mapped_position <- "3:1.5"#"3:1.5)
    } else if(api_football_position == "4:2"){
      mapped_position <- "4:4.5"#"4:2.5)
    } else if(api_football_position == "4:1"){
      mapped_position <- "4:2.5"#"4:1.5)
    } else if(api_football_position == "5:1"){
      mapped_position <- "5:3.5"#"5:3.5)
    }

  } else if(formation == "3-4-1-2"){
    if(api_football_position == "1:1"){
      mapped_position <- "1:3.5"
    } else if(api_football_position == "2:3"){
      mapped_position <- "2:5.5"
    } else if(api_football_position == "2:2"){
      mapped_position <- "2:3.5"
    } else if(api_football_position == "2:1"){
      mapped_position <- "2:1.5"
    } else if(api_football_position == "3:4"){
      mapped_position <- "3:5.5"
    } else if(api_football_position == "3:3"){
      mapped_position <- "3:4.5"
    } else if(api_football_position == "3:2"){
      mapped_position <- "3:2.5"
    } else if(api_football_position == "3:1"){
      mapped_position <- "3:1.5"
    } else if(api_football_position == "4:1"){
      mapped_position <- "4:3.5"
    } else if(api_football_position == "5:2"){
      mapped_position <- "5:4.5"
    } else if(api_football_position == "5:1"){
      mapped_position <- "5:2.5"
    }
    
  } else if(formation == "3-3-3-1"){
    if(api_football_position == "1:1"){
      mapped_position <- "1:3.5"
    } else if(api_football_position == "2:3"){
      mapped_position <- "2:5.5"
    } else if(api_football_position == "2:2"){
      mapped_position <- "2:3.5"
    } else if(api_football_position == "2:1"){
      mapped_position <- "2:1.5"
    } else if(api_football_position == "3:3"){
      mapped_position <- "3:5.5"
    } else if(api_football_position == "3:2"){
      mapped_position <- "3:3.5"
    } else if(api_football_position == "3:1"){
      mapped_position <- "3:1.5"
    } else if(api_football_position == "4:3"){
      mapped_position <- "3:5.5"
    } else if(api_football_position == "4:2"){
      mapped_position <- "4:3.5"
    } else if(api_football_position == "4:1"){
      mapped_position <- "4:1.5"
    } else if(api_football_position == "5:1"){
      mapped_position <- "5:3.5"
    }
    
  } else if(formation == "3-3-1-3"){
    if(api_football_position == "1:1"){
      mapped_position <- "1:3.5"
    } else if(api_football_position == "2:3"){
      mapped_position <- "2:5.5"
    } else if(api_football_position == "2:2"){
      mapped_position <- "2:3.5"
    } else if(api_football_position == "2:1"){
      mapped_position <- "2:1.5"
    } else if(api_football_position == "3:3"){
      mapped_position <- "3:5.5"
    } else if(api_football_position == "3:2"){
      mapped_position <- "3:3.5"
    } else if(api_football_position == "3:1"){
      mapped_position <- "3:1.5"
    } else if(api_football_position == "4:1"){
      mapped_position <- "4:3.5"
    } else if(api_football_position == "5:3"){
      mapped_position <- "5:5.5"
    } else if(api_football_position == "5:2"){
      mapped_position <- "5:3.5"
    } else if(api_football_position == "5:1"){
      mapped_position <- "5:1.5"
    }
    
  } else if(formation == "3-2-4-1"){
    if(api_football_position == "1:1"){
      mapped_position <- "1:3.5"
    } else if(api_football_position == "2:3"){
      mapped_position <- "2:5.5"
    } else if(api_football_position == "2:2"){
      mapped_position <- "2:3.5"
    } else if(api_football_position == "2:1"){
      mapped_position <- "2:1.5"
    } else if(api_football_position == "3:2"){
      mapped_position <- "3:4.5"
    } else if(api_football_position == "3:1"){
      mapped_position <- "3:2.5"
    } else if(api_football_position == "4:4"){
      mapped_position <- "4:5.5"
    } else if(api_football_position == "4:3"){
      mapped_position <- "4:4.5"
    } else if(api_football_position == "4:2"){
      mapped_position <- "4:2.5"
    } else if(api_football_position == "4:1"){
      mapped_position <- "4:1.5"
    } else if(api_football_position == "5:1"){
      mapped_position <- "5:3.5"
    }
    
  } else if(formation == "3-1-4-2"){
    if(api_football_position == "1:1"){
      mapped_position <- "1:3.5"
    } else if(api_football_position == "2:3"){
      mapped_position <- "2:5.5"
    } else if(api_football_position == "2:2"){
      mapped_position <- "2:3.5"
    } else if(api_football_position == "2:1"){
      mapped_position <- "2:1.5"
    } else if(api_football_position == "3:1"){
      mapped_position <- "3:3.5"
    } else if(api_football_position == "4:4"){
      mapped_position <- "3:5.5"
    } else if(api_football_position == "4:3"){
      mapped_position <- "3:4.5"
    } else if(api_football_position == "4:2"){
      mapped_position <- "3:2.5"
    } else if(api_football_position == "4:1"){
      mapped_position <- "4:1.5"
    } else if(api_football_position == "5:2"){
      mapped_position <- "5:4.5"
    } else if(api_football_position == "5:1"){
      mapped_position <- "5:2.5"
    }
  }

  return(mapped_position)
}