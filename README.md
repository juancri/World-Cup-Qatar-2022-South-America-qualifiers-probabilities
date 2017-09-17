# 2017-playoff-probabilities
### MLS Playoff probabilities for the 2017 season

The following code models MLS playoff probabilities, by simulating the games remaining in the season using the Dixon-Coles method for modeling scoring rates. This is just about the simplest thing we can implement. It does not account for player injuries or absences, upticks (or downticks) in recent form, or differences in play resulting from game states (this probably helps Atlanta), among many other things.

**data_prep.R**
First, run this script to prepare the `modeling_data.csv` file. It reads from `game_info.csv`, and transforms the typical Home-Away-Home Score-Away Score into a modeling file formatted for R's `glm` function. The idea is to model the rate at which a team will score goals in game, as a function of their offensive strength, their opponent's defense strength, and a home-field advantage indicator. So that is this modeling file arranges the data in that way.

**game_probs.R**
Next, run this script to get the model's esimated win-loss-draw probabilities for the remaining games on the schedule.

**sim_season.R**
Lastly, this script uses the probabilities output above to simulate the games still remaining in the season, 10,000 times, to get estimated points and final positions. It then takes those 10,000 seasons and turns the results into probablity estimates. Currently the model does not account for tiebreakers, the first of which is total wins, then goal differential, then goals for.

#### Further Reading
Dixon and Coles' original paper -
[Modeling Association Football Scores and Inefficiencies in the Football Betting Market](http://www.math.ku.dk/~rolf/teaching/thesis/DixonColes.pdf)

538's method for predicting match outcomes -
<https://fivethirtyeight.com/features/whats-new-in-our-2017-18-club-soccer-predictions/>