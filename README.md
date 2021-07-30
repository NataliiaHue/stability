# Stability Analyses


## Current plan

1. Obtain the classification taxonomy from glottolog. See directory _glottolog/_.

2. Construct a posterior distribution, using the glottolog classification taxonomy as a backbone. See directory _beast/_.

3. Loop over a sample of 1000 trees from this posterior distribution and fit all features onto each tree using _D_. 

4. Summarise results as mean/median/standard deviation of _D_ for each feature.

5. ...

## Original Idea

1. collect as many trees as we can from people:

* Koreanic, Japonic: Sean Lee
* Turkic: Sander
* Mongolic: there might be no Bayesian tree
* Tungusic: Sonya (under revision)

2. match data up to the languages on these trees (via glottocodes — Simon has this already for Koreanic, Japonic, Ainu, and Turkic)

3. loop over each tree in the posterior probability distribution, fitting each feature

=> a distribution of phylogenetic signal measures (_D_) for each feature in your data, and each family

4. compare the overall distributions of _D_: in which families do the structural traits fit badly or fit well

5. compare the traits across families in terms of phylogenetic signal: is there a correlation in _D_ between e.g. Turkic and Mongolic?

