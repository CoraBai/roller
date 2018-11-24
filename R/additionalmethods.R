#' @title make_rolls
#' @description amend changes in parameters of rolls
#' @param roll a list of roll information
#' @param rolls roll outcomes
#' @return amended side, count, probability summary of the input roll
make_rolls = function(roll, rolls){
  res = list(rolls = rolls,
             sides = roll$sides,
             prob = roll$prob,
             total = length(rolls))
  class(res) = "rolls"
  res
}
#set.seed(123)
#fair_dev <- device(sides = letters[1:8], prob = rep(1/8, 8))
#fair500 <- roll(fair_dev, times = 500)

#summary.rolls(fair500)

#extraction method
#' @title [.rolls
#' @description extract elements
#' @param roll roll outcomes of input device
#' @param i index of numbers being extracted
#' @return extracted number
#' @export

"[.rolls" = function(roll, i){
  roll$rolls[i]
}
#fair500[500]


#replacement method
#' @title [<- rolls
#' @description replacement method
#' @param x roll outcomes of input device
#' @param i index of element being changed
#' @param value value of element in replacement
#' @return new roll information
#' @export

"[<-.rolls" = function(x, i, value){
  x$rolls[i] = value
  make_rolls(x, x$rolls)
}
#fair500[500] <- 'a'
#fair500[500]
#summary.rolls(fair500)

#addition method
#' @title +. rolls
#' @description addition method
#' @param roll roll outcomes of input device
#' @param incr increase of roll times
#' @return new roll information
#' @export

"+.rolls" = function(roll, incr){
  if(length(incr)!= 1 | incr <=0){
    stop("\ninvalid addition")
  }
  dev = device(roll$sides, roll$prob)
  more_rolls = roll(dev, times = incr)
  make_rolls(roll, c(roll$rolls, more_rolls$rolls))
}
#fair600 <- fair500 + 100
#summary(fair600)
#plot method
#plot(fair500, 500)
