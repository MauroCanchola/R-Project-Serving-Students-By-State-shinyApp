Describe_Count = function(x) {
  if(is.na(x) == TRUE) {
    return("NA")
  } else if(x >= 500) {
    return("Very large amount of people")
  } else if (x < 500 & x >= 100) {
    return("Lots of people")
  } else if (x < 100 & x >= 50) {
    return("Decent amount of people")
  } else if (x < 50 & x >= 10) {
    return("A few people")
  } else if (x < 10) {
    return("Very few to no people")
  }
}
  
  