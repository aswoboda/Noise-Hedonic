Estimating the Impact of Traffic Noise on Housing Prices
========================================================

From my FDE:
---------------------


Just as hedonic analysis can be used to determine how much extra consumers are willing to pay to live near amenities like parks, the method can also be used to determine how much consumers are willing to pay to avoid disamenities like pollution. Noise pollution from traffic is a ubiquitous nuisance in the urban built environment and represents another potential application of the hedonic method. This project's goal is to more accurately than ever before assess the effect of traffic noise on housing prices within the Twin Cities Metropolitan Region by: 1) using new estimates of traffic noise, and, 2) applying a spatial analysis technique new to this area of scholarship.

### Contribution of the work to the field
Years of previous work by Professor Nega and his research team have resulted in the creation of a spatially explicit map of traffic noise within the Twin Cities Metropolitan Region. The noise estimates, described in Nega et al.\ (2012), account for: 1) the location and characteristics of regional roads (traffic volume, vehicle type, and speed limits), as well as the ability for this noise to propagate over/through the landscape by considering, 2) the natural landscape (i.e. distance and elevation of the source noise and receiver), and 3) the location, size, shape, and type of buildings and vegetation through/around which the noise must pass. To our knowledge, such recent and accurate noise data have never before been used to estimate the economic costs of traffic noise within the Twin Cities Metropolitan Region. 

This project also seeks to explore how the impact of traffic noise varies over space. For instance, traffic noise may be felt more acutely in less urbanized areas rather than more urbanized areas. Locally weighted regression (also called geographically weighted regression) is a semi-parametric multiple regression technique designed to allow model coefficients to vary continuously over space. Stated another way, locally weighted regression allows for the impact of one variable on another to vary with location in the study area. The technique accomplishes this outcome by estimating numerous regression equations at different geographic locations within the dataset and giving greater weight within a given regression to local data. 

Given the real estate adage that the three most important determinants of real estate prices are: 1) location, 2) location, and 3) location, it seems appropriate to apply locally weighted regression techniques to housing markets. While currently requiring some computational prowess and time, locally weighted regression is becoming more common in the economics literature. See for instance, Cho et al.\ (2006), Sunding and Swoboda (2010), and McMillen and Redfearn (2010) as examples. Yet, to our knowledge no one has yet applied this method to questions of traffic noise. One potential reason for such an absence is that the method requires data with sufficient local variation in the variables included in the model (it is impossible to estimate the effect of traffic noise in an area using multiple regression if all houses in an area are exposed to the same level of traffic noise). However, the fine scale at which Nega et al. (2012) estimated the traffic noise data (10m$^2$ resolution) allows us to potentially be the first researchers to apply this technique to this problem.  

### Feasibility

Many potential hurdles to the completion of this work have already been leapt. We have an interesting research question. We already have the housing, traffic noise, and other local amenity data. These data have been cleaned and combined through the use of a Geographical Information System (GIS) to form an analyzable dataset. We also have a clear sense of the methodology needed to perform the empirical analysis (similar to that implemented in Sunding and Swoboda (2010)). 

A Faculty Development Endowment Grant would give me the time to implement the remaining work necessary to complete the project. As this project represents a bridge between the work of Professor Nega and myself, we still have some work to do in the literature review (I have experience in the area of hedonic analysis and he has experience working with traffic noise, but neither of us has studied the intersection of the two).  Additionally, due to the computational intensity of locally weighted regression and the large amount of data we currently have (>100,000 housing observations), we plan to restrict our analysis to a more manageable subset before performing the analysis. These tasks all appear to be straightforward and simply require time for their execution.

Moving Forward:
------------------

### Data
What data are we going to use?

### Model

The general model will be something along the lines of: 
$House\ Price = \alpha + \beta X + error$

Where X represents a vector of control variables.
