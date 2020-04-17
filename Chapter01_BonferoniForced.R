# This question generalizes the example of "evil-doers" visiting hotels, as in Section 1.2.3.
# Suppose as before that there are a billion people being monitored for 1000 days.
# Each person has a 1% probability of visiting a hotel on any given day, and hotels hold 100 people each, so there are 100,000 hotels.
# However, our test for evil-doers is different. We consider a group of p people evil-doers if they all stayed at the same hotel on d different days.
# Derive the formula for the expected number of sets of p people that will be suspected of evil-doing ("false accusations"),
# assuming that in fact there are no evil-doers, but all people behave at random,
# following the conditions stated in this problem (1% probability of visiting a hotel, etc.).
# Then, find in the list below, the choice that is the best approximation to the correct value for some d and p.
#
# Note: You may assume that d and p are sufficiently small that (1000 choose d) can be approximated as 1000d/d!, and similarly for p.
# Also note: all choices are either powers of 10 or 3 times a power of 10.
# We are looking for the choice that is the best approximation to the expected value for the given d and p.
#
#    d  p   f
#    2  2  2.5e5
#    2  3  0.833
#    3  2  10e-1
#    3  3  3e-14

suspects <- function(d=2,p=2){
  people <- 1e9
  days <- 1e3
  pr <- .01
  hotels <- 1e5
  p_same_hotel <- pr^p/hotels
  d_days_same_hotel <- p_same_hotel^d
  choose_p <-people^p/factorial(p)
  choose_d <- days^d/factorial(d)
  expected_events <- choose_p * choose_d * d_days_same_hotel
  if (p > 2) {
    expected_events <- expected_events/hotels^d
  }
  
  expected_events
}

suspects(d=2,p=2)
suspects(d=2,p=3)
suspects(d=3,p=2)
suspects(d=3,p=3)