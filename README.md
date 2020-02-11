# land-change-near-us-parks-and-protected-areas
This is the public repository for Python and R code files (described below) used to query and analyze data for the companion manuscript entitled <b>"Protection status and proximity to public-private boundaries influence land use intensification near U.S. parks and protected areas</b>." The manuscript is accepted for publication in the journal <i>Conservation Science and Practice</i>, and this file will be updated with the final citation when the article is published online.

Author Affiliations:
Jelena Vukomanovic(a,b), Kunwar K. Singh(c), John B. Vogler(b), and Ross K. Meentemeyer(b,d) 

(a)Department of Parks, Recreation, and Tourism Management, North Carolina State University, 2820 Faucette Drive, Raleigh, NC 27695, USA; (b)Center for Geospatial Analytics, North Carolina State University, 2800 Faucette Drive, Raleigh, NC 27695, USA; (c)Global Research Institute, AidData, The College of William and Mary, 424 Scotland Street, Williamsburg, VA 23185, USA; (d)Department of Forestry & Environmental Resources, North Carolina State University, 2820 Faucette Drive, Raleigh, NC 27695, USA

Corresponding Author: <b>Dr. Jelena Vukomanovic - jvukoma@ncsu.edu</b>

<b>File Descriptions</b>
1) rand_prot_areas.py: python script for randomizing polygons. Supply input data and output file names, and script randomizes polygons and outputs a shapefile.

2) prot_areas_analysis.R: R script uses data to extract housing density and impervious surface values within a buffer (both observed and random data) for ecoregions and states and produces a shapefile. This is a simplified script and is utilized to extract housing density values and percent impervious values for each GAP status and CONUS level.

3) hd_imp_summaries.R: R script utilizes the above-mentioned shapefile (#2) to create a spreadsheet of housing density and percent impervious surface summaries at ecoregion and state scale; utilized to run statistical analysis.

4) statistical_tests.R: R script utilizes observed and random data to run various statistical tests that are reported in the publication.


