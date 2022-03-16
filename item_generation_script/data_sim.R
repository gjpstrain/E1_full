# true Pearson's R values 
R_values <- runif(1000, 0.2, 0.9)
hist(R_values)
# participants' responses
# same parent distribution, but differ slightly due to human error
responses <- runif(1000, 0.2, 0.9)
hist(responses)
# DV: the difference between correct answer and response
# collapsed across conditions
diffs <- responses - R_values
hist(diffs)
# Simulating a difference between conditions
# shifting all responses up slightly
high_responses <- responses + 0.04
hist(high_responses)
# still looks like a uniform distribution
# shifting all responses down slightly
low_responses <- responses - 0.04
hist(low_responses)
# now calculating the DV separately for each condition
diffs_high <- high_responses - R_values
diffs_low <- low_responses - R_values
# plotting the difference between conditions
hist(diffs_high, col=rgb(0,0,1,1/4), xlim=c(-1, 1))
hist(diffs_low, col=rgb(1,0,0,1/4), xlim=c(-1, 1), add=T )
