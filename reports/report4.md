Report 4: Trends in road safety and potential causal factors
================
Joey Talbot
20/08/2021

# 1 Introduction

The Saferactive project is funded by the Department for Transport. It
aims to investigate spatial and temporal patterns in road safety for
active travel.

We have used a range of nationally available datasets to estimate and
visualise road safety, including stats19 collision data, DfT and TfL
traffic counts, NTS survey returns and 2011 Census commute data.

# 2 Input data

TfL counter data (Central, Inner and Outer London count points only)

Mean count per Borough, over the period 2015 - 2020.

![](https://user-images.githubusercontent.com/52210872/132374206-40a7b092-d3f0-42cc-877e-a8552b6d8921.png)<!-- -->

Change since 2015 in each Borough.

![](https://user-images.githubusercontent.com/52210872/132371640-a9eedf6c-ce3e-41c5-97eb-5b97d9c6b6b3.png)<!-- -->

DfT counter data

Mean count per LAD, over the period 2010 - 2020.

# 3 `{r} # knitr::include_graphics("") #`

# 4 National trends

## 4.1 National trends in cycle volume

The number of cyclists saw a small increase over the ten years prior to
2019, according to three different data sources.

In 2020, the mean number of cyclists passing DfT traffic count points
was three times higher than in previous years.

![](https://user-images.githubusercontent.com/52210872/130480677-af7ea79d-94f8-4812-8ec6-9321a5b6b4e0.png)<!-- -->

Estimated change since 2011 in cycle flows at a national level, based on
a GAM generated using the DfT and TfL counter data

![](https://user-images.githubusercontent.com/52210872/133419356-5d57135f-29c8-4eb5-8237-95969387c352.png)<!-- -->

## 4.2 National trends in collisions and collision risk

Change in cycle KSI 2010 - 2019

# 5 `{r} # knitr::include_graphics("") #`

In the period 2010-2019, risk of cycle KSI has reduced. This reduction
has mainly occurred since 2014, and can be seen when casualty data are
assessed using all three measures of cycle volume.

![](https://user-images.githubusercontent.com/52210872/130476560-7aabd05c-b540-4deb-b0a4-4d994c3c7a2f.png)<!-- -->

# 6 Regional trends

## 6.1 Regional trends in cycle volume

Total distance cycled per year in each English region (based on NTS
survey)

![](https://user-images.githubusercontent.com/52210872/133634243-6449b3c1-61dd-429c-bc41-562841968ff4.png)<!-- -->

Mean AADF cycle flow per DfT counter in each region (including Scotland
and Wales). This is based on the raw flows per counter.

![](https://user-images.githubusercontent.com/52210872/132316899-1bbc1cc9-687e-4df3-9c7a-3074778dd107.png)<!-- -->

Mean change in AADF cycle flow per DfT counter in each region (including
Scotland and Wales). This is based on the change in flow at each
counter, calculated as the raw flow minus the minimum flow for the given
counter.

![](https://user-images.githubusercontent.com/52210872/133624726-227a7114-0fb1-4f62-bd06-a139f7245b60.png)<!-- -->

Mean AADF cycle flow per TfL counter

# 7 `{r} # knitr::include_graphics("") #`

Estimated mean change in cycle flows, according to GAM model based on
the DfT and TfL cycle counters

![](https://user-images.githubusercontent.com/52210872/133447294-0143da9d-5d66-47ce-ab14-741cdbb04147.png)<!-- -->

## 7.1 Regional trends in collisions and collision risk

Sum of cycle KSI in each region (including Scotland and Wales)

![](https://user-images.githubusercontent.com/52210872/132044428-c6f9e57c-478f-47d9-9247-5d338b9e2137.png)<!-- -->

Trends in KSI risk per km cycled, using NTS journey data

![](https://user-images.githubusercontent.com/52210872/133631183-2f5e6454-f34b-4aa3-bcac-fad88503e896.png)<!-- -->

Trends in KSI risk per DfT mean cycle count

![](https://user-images.githubusercontent.com/52210872/132327377-8daceacf-2de6-4513-b776-b5593ec2d11a.png)<!-- -->

Trends in KSI risk per TfL mean cycle count

# 8 `{r} # knitr::include_graphics("") #`

Trends in KSI risk for GAM outputs

![](https://user-images.githubusercontent.com/52210872/130441145-8a5edd19-6c34-4dda-8c78-4dd7bcb90ef7.png)<!-- -->

# 9 Sub-regional trends in cycle KSI and risk

We investigate trends in KSI, exposure and risk for commuter cycling at
the geographical scales of Upper Tier Local Authorities and Police Force
areas. Data related to KSI and km cycled are available for the years
2010 - 2020, while data related to population estimates, such as km
cycled per capita, are available for the years 2010 - 2019.

KSI and risk data are estimated based on the number of cyclists killed
and seriously injured during weekday peak hours (07:00-10:00 and
16:00-19:00), and estimates of km cycled derived from Census 2011 travel
to work by bicycle. These journeys are routed using the CycleStreets.net
fast routes algorithm, as used in the Propensity to Cycle Tool.

For all other years, we adjust the 2011 commuter cycle flows using DfT
counter data, based on mean AADF counts within the relevant geographical
zone. As a baseline for the annual adjustments of cycle flows, we reduce
volatility by using the mean AADF flows in a three year period centered
on 2011, e.g:

*K**m*<sub>2020</sub> = *K**m*<sub>2011</sub> \* (*A**A**D**F*<sub>2020</sub>/(*M**e**a**n*(*A**A**D**F*<sub>2010</sub>,*A**A**D**F*<sub>2011</sub>,*A**A**D**F*<sub>2012</sub>)))

where *K**m*<sub>2020</sub> = estimated km cycled for travel to work in
2020; *K**m*<sub>2011</sub> = km cycled for travel to work in 2011;
*A**A**D**F*<sub>2020</sub> = mean AADF of DfT counters within the
geographical region in 2020; *A**A**D**F*<sub>2010</sub> = mean AADF in
2010; *A**A**D**F*<sub>2011</sub> = mean AADF in 2011; and
*A**A**D**F*<sub>2012</sub> = mean AADF in 2012.

## 9.1 Upper Tier Local Authority trends

Change in KSI at LA level is seen below.

![](https://user-images.githubusercontent.com/52210872/136986982-540da20d-c37f-4aad-acdf-337fe899c1e2.png)<!-- -->

This can be compared with changes in km cycled at LA level.

![](https://user-images.githubusercontent.com/52210872/136987001-d5aa80a7-59fe-49fb-9c9d-c9ccd1f77ec8.png)<!-- -->

Combining the data from the previous two figures, we obtain estimates of
change in risk.

![](https://user-images.githubusercontent.com/52210872/136982364-660dbbba-6494-4aa5-9502-575a455b428a.png)<!-- -->

## 9.2 Police Force area trends

Similarly, we investigate changes in peak hour KSI at Police Force
level.

![](https://user-images.githubusercontent.com/52210872/136990432-56fb1df1-d8f2-4c71-ade4-a67323c1abb5.png)<!-- -->

Km cycled has risen noticeably in many regions.

![](https://user-images.githubusercontent.com/52210872/136990447-7879441a-7969-4082-874b-f371d0f0864f.png)<!-- -->

The result is a decrease in risk for cycle commuting across most Police
Force areas.

![](https://user-images.githubusercontent.com/52210872/136990471-f2dfd8a0-1319-44fb-8ed3-b7287d6be588.png)<!-- -->

## 9.3 Data downloads

The Upper Tier Local Authority level trends in peak hour cycling and
risk can be found at
<https://github.com/saferactive/saferactive/releases/download/0.1.4/ksi.csv>

The police force level trends in peak hour cycling and risk can be found
at
<https://github.com/saferactive/saferactive/releases/download/0.1.4/ksi-pf.csv>

# 10 Low Traffic Neighbourhoods and rat-runs

Traffic levels on minor roads have increased dramatically in recent
years…

Although the phrase ‘Low Traffic Neighbourhood’ is a recent invention,
the concept is not new. Modal filters preventing vehicle movement along
a street while allowing passage to pedestrians and cyclists have existed
for many decades. They can take a wide range of forms, such as bollards,
gaps, and continuations of pavement across the entrances of side
streets.

It can also be argued that any street, or any network of streets, which
does not provide a vehicle connection between two main roads, is in
effect part of a low traffic neighbourhood. These streets do not provide
worthwhile opportunities for ratrunning, so they are likely to be used
mainly by residents and those wishing to access destinations on the
street itself. This includes cul-de-sacs and wider networks of streets
for which all of the access points are on the same main road.

Using this definition of low traffic neighbourhoods, CycleStreets.net
have created a nationwide beta map of LTNs. All roads are assigned as
main roads, low traffic neighbourhood roads, ratruns, or ratruns with
traffic calming measures. Main roads are those classified as ‘A’, ‘B’ or
‘C’ roads. Data on traffic calming measures are obtained from OSM.

This map extract from Leeds gives an example of how roads are
classified.

![](https://user-images.githubusercontent.com/52210872/137929708-99296ffc-07c9-4c12-b052-e2a3e14d7cdb.png)<!-- -->

We have used this beta map with West Yorkshire as a case study region,
to assess the impact of Low Traffic Neighbourhoods on road safety.

## 10.1 Active travel KSI, road length and commuter cycling in West Yorkshire

For this case study we have divided the West Yorkshire road network into
a set of 1km grid cells. We see a clear correlation between total road
length within each 1km cell and the number of pedestrian KSI over the
period 2010-2019.

![Figure 10.1: Pedestrian KSI over the years 2010-2019 against road
length in km. Each point represents a 1km grid
cell](https://user-images.githubusercontent.com/52210872/136543444-d574755c-eb0d-4d4b-9981-a22fc18756ea.png)

There is also a positive association between road length and cycle KSI
but it is weaker, likely reflecting the fact that the cycling
distribution is more uneven than the pedestrian distribution.

![Figure 10.2: Cycle KSI over the years 2010-2019 against road length in
km. Each point represents a 1km grid
cell](https://user-images.githubusercontent.com/52210872/136543521-220b0a1d-432d-46dd-b796-ddbd20309573.png)

We can see a positive correlation between distance cycled for travel to
work and cycle KSI.

![Figure 10.3: Cycle KSI over the years 2010-2019 against km cycled for
travel to work according to the 2011 Census, using the fast route
network derived from the PCT. Each point represents a 1km grid cell,
with colour weighted by total road length within the grid
cell](https://user-images.githubusercontent.com/52210872/136543609-0a2e5e82-af09-4a97-85d8-bed82f4fb72f.png)

Switching to maps, we can see how these patterns play out across West
Yorkshire. The greatest concentration of cycle and walking KSI are found
in the urban areas such as Leeds and Bradford. Cycle commute km are
strongly concentrated within Leeds.

![Figure 10.4: Cycle and pedestrian KSI for the period 2010-2019 and km
cycled for travel to work in West Yorkshire. The map of km cycled only
includes cells that form part of the PCT-derived fast route network for
the 2011
Census](https://user-images.githubusercontent.com/52210872/137118045-8f0c8c93-e104-4583-a5e0-f5c600d0b6e9.png)

Now looking at the distribution of KSI risk, we see that for walking,
KSI risk per km road remains highest in the urban areas, especially
Bradford. For cycling, KSI risk per km road has a slightly more
scattered distribution with high values in Leeds. However, when we use
Bkm cycled as the denominator, the map is very different. Now some of
the lowest rates are found in Leeds. This map shows very high KSI rates
in some of the Pennine fringes of West Yorkshire, but these are probably
artefacts of the fact that we are using commuter cycling as the
denominator, while leisure and sport cycling dominate in these areas. A
more reliable finding is that KSI risk appears higher in Bradford than
in Leeds.

![Figure 10.5: Cycle and pedestrian KSI risk for the period 2010-2019
and KSI per Bkm cycled for travel to work in West Yorkshire, assuming
35% of cycle journeys are commutes (NTS mean for England in 2010-2019).
The third map only includes cells that form part of the PCT-derived fast
route network for the 2011
Census](https://user-images.githubusercontent.com/52210872/136543119-5548f711-49d6-4fa3-946d-80c5887f40c2.png)

## 10.2 KSI and road types in West Yorkshire

The next step is to investigate how these patterns relate to the
distribution of road types. The total road length within each 1km grid
cell varies greatly across the region, but so too does the distribution
of types of road within each grid cell.

![](https://user-images.githubusercontent.com/52210872/137931151-74a946aa-11a8-4989-be58-db89206792c0.png)<!-- -->

Weighting by the road length within a grid cell, we look at how
pedestrian and cyclist KSI vary with the proportion of roads of
different types. We expect that KSI will increase as the proportion of
ratruns increases, and decrease as the proportion of LTNs increases.

For pedestrians, a GLM shows a small increase in KSI per km road with
the proportion of roads classified as ratruns.

![](https://user-images.githubusercontent.com/52210872/137933108-d8fa1328-8a73-4943-a450-b685d01cf2b0.png)<!-- -->![](https://user-images.githubusercontent.com/52210872/137933146-c1c819c1-0735-49fa-9329-36808c2d23d5.png)<!-- -->

For cyclists, GLMs show small decreases in KSI per km road and KSI per
km cycled, with the proportion of roads classified as LTNs.

![](https://user-images.githubusercontent.com/52210872/137930345-8717150b-e99e-49a8-a77f-d234610db516.png)<!-- -->![](https://user-images.githubusercontent.com/52210872/137930392-c6aa4da3-6f9f-4d98-9a44-8cfaff4d7ebc.png)<!-- -->

# 11 Data visualisation

# 12 Web application

# 13 Discussion

# 14 Conclusion
