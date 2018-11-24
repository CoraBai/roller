
#' @title check_times
#' @description check if time is positive integer larger or equal to 1
#' @param times input time
#' @return TRUE/FALSE of if time is qualified

check_times = function(times){
  if(times <1 | times%%1 != 0){
    stop("'times' need to be a positive integer greater than or equal to 1")
  }
  TRUE
}

#' @title rolls
#' @description print rollss information in an organized manner
#' @param list input roll list
#' @return print rolls info in an organized manner

print.rolls = function(list){
  cat('object "rolls" \n\n')
  print(list$rolls)
  invisible(list)
}

#' @title roll
#' @description return information of the roll
#' @param sides sides info of the object
#' @param prob prob info of the object
#' @return device information, including sides and probabilities
#' @export

roll = function(dev, times = 1){
  if(class(dev) != "device"){
    stop("The class is not 'device'")
  }
  if (check_times(times)== TRUE) {
    rolls_sample = sample(dev$sides, size = times, replace = TRUE, prob = dev$prob)
    lst = list(rolls = rolls_sample, sides = dev$sides, prob = dev$prob, total = times)
    class(lst) = "rolls"
  }
  return (lst)
}

#fair_die = device(sides = 1:6, prob = rep(1/6, 6))
#set.seed(123)
#fair_50rolls = roll(fair_die, times = 50)
#fair_50rolls

#fair_die_two = device()
#set.seed(123)
#fair50 = roll(fair_die_two, times = 50)
#names(fair50)
#fair50$rolls
#fair50$sides
#fair50$prob
#fair50$total
#string die
#str_die = device(sides = c('a', 'b', 'c', 'd', 'e', 'f'), prob = c(0.075, 0.1, 0.125, 0.15, 0.20, 0.35))
#set.seed(123)
#str_rolls = roll(str_die, times = 20)
#names(str_rolls)
#str_rolls
