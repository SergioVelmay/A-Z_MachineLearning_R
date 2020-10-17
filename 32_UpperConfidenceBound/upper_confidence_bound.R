# Upper Confidence Bound

# Importing the dataset
dataset = read.csv('Ads_CTR_Optimisation.csv')

# Implementing UCB
N = 1000 # number of rounds
d = 10 # number of adverts
ads_selected = integer(0) # N elements at the end
numbers_of_selections = integer(d) # Ni(n)
sums_of_rewards = integer(d) # Ri(n)
# integer(10) --> initialized as (0,0,0,0,0,0,0,0,0,0)
total_reward = 0

for (n in 1:N)
{
  ad = 0
  max_upper_bound = 0
  for (i in 1:d) 
  {
    if (numbers_of_selections[i] > 0) 
    {
      average_reward = sums_of_rewards[i] / numbers_of_selections[i]
      delta_i = sqrt(3/2 * log(n) / numbers_of_selections[i])
      upper_bound = average_reward + delta_i
    }
    else
    {
      upper_bound = 1e400 # super high value, almost infinity
    }
    if (upper_bound > max_upper_bound)
    {
      max_upper_bound = upper_bound
      ad = i
    }
  }
  ads_selected = append(ads_selected, ad)
  numbers_of_selections[ad] = numbers_of_selections[ad] + 1
  reward = dataset[n, ad] # from simulation in database
  sums_of_rewards[ad] = sums_of_rewards[ad] + reward
  total_reward = total_reward + reward
}

# Visualising the results
hist(ads_selected,
     col = 'blue',
     main = 'Histogram of ads selections',
     xlab = 'Ads',
     ylab = 'Number of times each ad was selected')