#' @title summary
#' @description return summary of the roll
#' @param roll roll outcomes of input device
#' @return side, count, probability summary of the input roll
#' @export
summary.rolls = function(roll) {
  side = roll$sides
  counts = c()
  prop = c()
  for(i in 1:length(side)) {
    temp_count = sum(roll$rolls == side[i])
    counts = c(counts, temp_count)
  }
  prop = counts / roll$total
  freqs = data.frame(side = side, count = counts, prop = prop)
  list = list(freqs = freqs)
  class(list) = "summary.rolls"
  return (list)
}

#' @export
print.summary.rolls = function(list) {
  cat('summary "rolls"\n\n')
  print(list$freqs)
}
#set.seed(123)
#fair_50rolls <- roll(fair_die, times = 50)
#fair50_sum <- summary(fair_50rolls)
#fair50_sum
#class(fair50_sum)
#names(fair50_sum)
#fair50_sum$freqs
