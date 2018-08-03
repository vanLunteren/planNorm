test_h0 <- function (test, T, n1, n2, alpha, delta){
  
  if (test == 1){
    if (n1 + n2 <= 2){ 
      if (1 - alpha > 0.5){
        quantil <- Inf
      } else if (1 - alpha > 0.5){
        quantil <- -Inf
      } else {
        quantil <- 0
      }
    } else {
      quantil <- qt(1 - alpha, n1 + n2 - 2)
    }
  } else {
    if (n1 + n2 <= 2){ 
      if (1 - alpha / 2 > 0.5){
        quantil <- Inf
      } else if (1 - alpha / 2 > 0.5){
        quantil <- -Inf
      } else {
        quantil <- 0
      }
    } else {
      quantil <- qt(1 - alpha / 2, n1 + n2 - 2)
    }
  }
  
  decision <- c()
  
  if (test == 1 & delta == 0){  
    if (T > quantil){
      decision <- c(decision, 1)
    } else {
      decision <- c(decision, 0)
    }
  } else if (test == 1 & delta != 0){  
    if (T < -quantil){
      decision <- c(decision, 1)
    } else {
      decision <- c(decision, 0)
    }
  } else if (test == 2){
    if (abs(T) > quantil){
      decision <- c(decision, 1)
    } else {
      decision <- c(decision, 0)
    }
  }
  return (decision)
}