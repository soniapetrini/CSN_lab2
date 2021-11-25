# CSN_lab2: Analysis of the degree distribution

__Syntactic Dependency Networks (SDNs)__ are spatial networks in which nodes are words and edges are syntactic relations. 
In directed SDNs, the in-degree distribution describes the syntactic links received by the dependent words from the heads. 
The goal of this report is to find the __best model__ to describe the __in-degree distribution of 10 languages from the Global SDN__. 
Since the typical hypothesis is a power-law, we explore three nested models from the Zeta Riemann function family, 
and compare them with two Null Models through Akaike Information Criterion (AIC). 
The comparison is made against the displaced Poisson and Geometric distributions, as they describe the degrees spectra
of randomly generated graphs (Erdos–Renyi and Random Geometric Graph respectively).
