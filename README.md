# Cycling safety and comfort

This repository contains code in R for adding a level of traffic stress (LTS) classification to a network of nodes and edges, such as a network created from OpenStreetMap using the 'MATSim network for Melbourne' code at https://github.com/matsim-melbourne/network.

## Calculation of LTS

The code calculates level of traffic stress by reference to four variables - cycling intrastructure, road type, traffic volume, and speed limit - in accordance with the following grid.

![LTSgrid](https://github.com/user-attachments/assets/febd1ec7-eca5-4d87-89ca-2ea0986933a0)

## Adding the LTS classification

The function for adding the LTS classification is `addLTS` in `addLTS.R`.  It requires as inputs a network consisting of layer of nodes and a layer of links.

The links layer must have the following fields.

| Field          | Content                                                     |
|----------------|-------------------------------------------------------------|
| cycleway       | Cycleway infrastructure: bikepath, shared_path, separated_lane, simple_lane, or NA.  If there are other values, they will be treated as NA. |
| highway        | Type of road: primary (primary, primary_link), secondary (secondary, secondary_link), tertiary (tertiary, tertiary_link), local (residential, road, unclassified, living_street, service), or offroad (cycleway, track, pedestrian, footway, path, corridor, steps).  If there are other values (eg trunk), they will be allocated the highest LTS (level 4). |
| ADT            | Average daily traffic volume, in one direction, on the link.
| freespeed      | Speed of the link, in metres per second.                     |

Extraction of cycleway, highway and freespeed fields from OpenStreetMap is provided for by the code at https://github.com/matsim-melbourne/network.  ADT (traffic volume) must be sourced separately, either by simulation or otherwise.

Alternatively, if ADT is not available, the function `addLTSAssumedTraffic` in `addLTS.R` may be used instead.  This function adds assumed traffic volumes, which are set at the level which will attract the lower LTS classification in each case where traffic volume would affect the classification.  Using this function applies the following simplified grid.

![LTSgridassumedtraffic](https://github.com/user-attachments/assets/032b9127-acfc-4856-abf5-ddd1ef458642)

## Calculation of link impedance

The `addLTS` function also calculates an 'impedance' for each link by reference to its LTS.  The 'impedance' consists of the link's length, plus a penalty in the case of higher-stress links, and may be used as a weight or cost in network routing analysis.  The impedance is calculated by reference to (1) the LTS of the link itself, and (2) the LTS of other links with which it intersects at its end point.   

## Enhanced LTS: adjustments for inadequate lane width or parking separation

The `addLTS` function also provides for an optional parameter 'excludeInadequateLanes'.  If this is set to 'TRUE', then the following on-road cycle lanes will be treated as mixed traffic rather than as on-road cycle lanes: 
- lanes where the width, including any buffers from parking or traffic, is less than 1.2m (or 1.8m if the lane is adjacent to parking); and
- lanes where parking is allowed on the lane.

The use of this parameter accordingly provides more nuanced treatment of lanes that do not offer the full level of protection that might be expected for onroad cycle lanes.

This enhancement requires the links layer to contain the following additional fields: 
- 'bikelane_left_lane', 'bikelane_right_lane': type of cycleway infrastructure, if any, on the left and right sides of the road.
- 'bikelane_left_width', 'bikelane_right_width': width of the bikelanes, if any, on the left and right sides of the road.
- 'bikelane_left_traf_left', 'bikelane_left_traf_right', 'bikelane_right_traf_left', 'bikelane_right_traf_right': traffic conditions (eg, parking or vehicular traffic) to the left and right of the bikelanes, if any, on the left and right sides of the road.
- 'bikelane_left_buff_left', 'bikelane_left_buff_right', 'bikelane_right_buff_left', 'bikelane_right_buff_right': width of the buffers, if any, between the bikelanes on the left and right sides of the road, and the parking or traffic to their left or right.

These fields are derived from certain OpenStreetMap tags described in https://wiki.openstreetmap.org/wiki/Melbourne_Bike_Lane_Project.  Currently, the relevant tags in OpenStreetMap are well-developed in Melbourne and Bendigo, but not necessarily in other places.  We are working on the development and publication of network-generation code to include these fields in the network output.

## Adding traffic
The file `addTraffic.R` provides convenience functions for joining traffic volumes to the links layer when this has been generated by simulation.  The functions assume that the a MATSim simulation has been run to generate the traffic volumes.  The choice of function and its parameters will require adjustment depending on how the simulation output is structured.  

## Example output
An example of an output network for Melbourne, generated using the code at https://github.com/matsim-melbourne/network and with LTS classifications added using `addLTSAssumedTraffic`, will be made available at \[OSF repo location to be specified\]. 

## Acknowledgement
This code was developed by [Steve Pemberton](https://cur.org.au/people/steve-pemberton/) and [Afshin Jafari](https://cur.org.au/people/afshin-jafari/) from the Centre for Urban Research at RMIT University. To contact us, please email afshin.jafari@rmit.edu.au.  
We acknowledge the contributions from the [AToM](https://matsim-melbourne.github.io/) development team, the [JIBE project](https://jibeproject.com/) team, and the [iMOVE project 3-3033 team](https://imoveaustralia.com/project/modelling-cycling-investments-in-regional-areas/).
