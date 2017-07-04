
# BDTREScheck
A package for doing checks on the Tree Swallow databases. Will print a list with the results (problematic lines) of the different checks executed.

## Installation

To install, do:

```r

# install and load devtools to be able to install packages from GitHub with install_github
install.packages("devtools")
library(devtools)

# install package
install_github("frousseu/BDTREScheck")
library(BDTREScheck)
?checkBD

```

## Column Names Required

**adults**: ferme, nichoir, id, annee, nnich, idcouvee, heure, jjulien, prefixe, suffixe, idadult, condition, sexe_morpho, age_morpho, sexe_gen, locus_sexe_gen, couleur, age_exact, laile1, laile2, masse, tarse1, tarse2, trougauche, troudroite, pararectrice, plaqueincu, Cause_recapt, commentaire, observateur

**chicks**: ferme, nichoir, id, annee, nnich, idcouvee, heure, jjulien, prefixe, suffixe, idois, sexe_gen, locus_sexe_gen, condition, numero_oisillon, jour_suivi, envol, masse, 9primaires1, 9primaires2, tarse1, tarse2, commentaires, manipulateur

**broods**: idcouvee, id, ferme, nichoir, annee, codesp, nnich, noeufs, noisnes, noisenvol, noismort, dispa_ois, dispa_oeufs, abandon, pred_pot, dponte, dincub, declomin, declomax, denvomin, denvomax, dabanmin, dabanmax, idF1, idM1, idF2, idF3, idM2, idM3, Commentaires

## How To Use

Run checks:

```r

library(BDTREScheck)


path<-"C:/Users/rouf1703/Documents/UdeS/Consultation/ELefol/Doc/BD 2004-2015"

x<-checkBD(
  dsn=path,
  adultsNew="Adulte2016.xlsx",
  broodsNew="Couvee2016.xlsx",
  chicksNew="oisillons2016.xlsx",
  adultsOld="Adultes_2004-2015.xlsx",
  broodsOld="Couvee_2004-2015.xlsx",
  chicksOld="Oisillons_2004-2015.xls"
)

```
Display results:

```r

x
x[4]

```

Show the list of checks:

```r

checkShow(html=FALSE)

```

## TODO

- check sheet number argument and sheets of current databases
- add part at the end of Nghia script
- add a year argument to check some data in coimparison to the previous years
- find what to do with read_excel warnings on expectation about column format
- change default values of databases names
- think of a way to remove warnings associated with the check on the number of characters based on column names that are not in all databases
- think about the input db type given and arguments options (a data.frame already in the environment, a path to an excel, csv file, a remote database etc.)
- remove internal correction to wrong names idois2 and sufixe
- make a code for newly installed bands found in previous years
- make part about summarizing brood information and hatching

## Current List Of Checks

<table class='gmisc_table' style='border-collapse: collapse; margin-top: 1em; margin-bottom: 1em;' >
<thead>
<tr>
<th style='border-bottom: 1px solid grey; border-top: 2px solid grey; text-align: center;'>ID</th>
<th style='border-bottom: 1px solid grey; border-top: 2px solid grey; text-align: center;'>Checks</th>
</tr>
</thead>
<tbody>
<tr>
<td style='text-align: center;'>1</td>
<td style='text-align: center;'> "Remove rows with NA id's in broodsNew"</td>
</tr>
<tr>
<td style='text-align: center;'>2</td>
<td style='text-align: center;'> "Remove rows with NA id's in adultsNew"</td>
</tr>
<tr>
<td style='text-align: center;'>3</td>
<td style='text-align: center;'> "Remove rows with NA id's in chicksNew"</td>
</tr>
<tr>
<td style='text-align: center;'>4</td>
<td style='text-align: center;'> "Are column names in adult consistent?"</td>
</tr>
<tr>
<td style='text-align: center;'>5</td>
<td style='text-align: center;'> "Are column names in chicksNew consistent?"</td>
</tr>
<tr>
<td style='text-align: center;'>6</td>
<td style='text-align: center;'> "Are column names in broodsNew consistent?"</td>
</tr>
<tr>
<td style='text-align: center;'>7</td>
<td style='text-align: center;'> "Are column names consistant across old and new databases"</td>
</tr>
<tr>
<td style='text-align: center;'>8</td>
<td style='text-align: center;'> "Are number of characters consistent for id-type columns in adultsNew db?"</td>
</tr>
<tr>
<td style='text-align: center;'>9</td>
<td style='text-align: center;'> "Are number of characters consistent for id-type columns in chicksNew db?"</td>
</tr>
<tr>
<td style='text-align: center;'>10</td>
<td style='text-align: center;'> "Are number of characters consistent for id-type columns in broodsNew db?"</td>
</tr>
<tr>
<td style='text-align: center;'>11</td>
<td style='text-align: center;'> "Show all unique values in adultsNew columns for which the number of possible values is restricted"</td>
</tr>
<tr>
<td style='text-align: center;'>12</td>
<td style='text-align: center;'> "Show all unique values in broodsNew columns for which the number of possible values is restricted"</td>
</tr>
<tr>
<td style='text-align: center;'>13</td>
<td style='text-align: center;'> "Show all unique values in chicksNew columns for which the number of possible values is restricted"</td>
</tr>
<tr>
<td style='text-align: center;'>14</td>
<td style='text-align: center;'> "Females without matches in the broodsNew file"</td>
</tr>
<tr>
<td style='text-align: center;'>15</td>
<td style='text-align: center;'> "Adult females wrongly assigned to broodsNew"</td>
</tr>
<tr>
<td style='text-align: center;'>16</td>
<td style='text-align: center;'> "Males without matches in the broodsNew file"</td>
</tr>
<tr>
<td style='text-align: center;'>17</td>
<td style='text-align: center;'> c("Adult males wrongly assigned to broodsNew")</td>
</tr>
<tr>
<td style='text-align: center;'>18</td>
<td style='text-align: center;'> "Capture date is lower than the laying date and the individual has not been found dead"</td>
</tr>
<tr>
<td style='text-align: center;'>19</td>
<td style='text-align: center;'> "Adults in the adult DB that are not in the broodsNew DB"</td>
</tr>
<tr>
<td style='text-align: center;'>20</td>
<td style='text-align: center;'> "Capture date is later than the min or max departure date from the nest"</td>
</tr>
<tr>
<td style='text-align: center;'>21</td>
<td style='text-align: center;'> "Capture date is later than the min or max date of nest abandonment"</td>
</tr>
<tr>
<td style='text-align: center;'>22</td>
<td style='text-align: center;'> "Capture date of young is later than the minimal abandonment date if nest was abandoned"</td>
</tr>
<tr>
<td style='text-align: center;'>23</td>
<td style='text-align: center;'> "Capture date of young is before the laying date"</td>
</tr>
<tr>
<td style='text-align: center;'>24</td>
<td style='text-align: center;'> "Sex/age incoherencies"</td>
</tr>
<tr>
<td style='text-align: center;'>25</td>
<td style='text-align: center;'> paste("Capture time outside", mmh[1], "and", mmh[2])</td>
</tr>
<tr>
<td style='text-align: center;'>26</td>
<td style='text-align: center;'> "Some colors not in the list of possible values"</td>
</tr>
<tr>
<td style='text-align: center;'>27</td>
<td style='text-align: center;'> "Wing measurement outside the range of likely values"</td>
</tr>
<tr>
<td style='text-align: center;'>28</td>
<td style='text-align: center;'> "Weight measurement outside the range of likely values"</td>
</tr>
<tr>
<td style='text-align: center;'>29</td>
<td style='text-align: center;'> "Tarsus measurement outside the range of likely values"</td>
</tr>
<tr>
<td style='text-align: center;'>30</td>
<td style='text-align: center;'> "Male with brood patch"</td>
</tr>
<tr>
<td style='text-align: center;'>31</td>
<td style='text-align: center;'> "Newly installed band found in the previous years"</td>
</tr>
<tr>
<td style='text-align: center;'>32</td>
<td style='text-align: center;'> "Visits are not all 2 days apart for the following farms in the adult DB"</td>
</tr>
<tr>
<td style='text-align: center;'>33</td>
<td style='text-align: center;'> "Visits are not all 2 days apart for the following farms in the chicksNew DB"</td>
</tr>
<tr>
<td style='text-align: center;'>34</td>
<td style='text-align: center;'> "Check for spaces in ferme ids"</td>
</tr>
<tr>
<td style='text-align: center;'>35</td>
<td style='text-align: center;'> "Check for duplicates using all columns in adultsNew"</td>
</tr>
<tr>
<td style='text-align: center;'>36</td>
<td style='text-align: center;'> "Check for duplicates using all columns in broodsNew"</td>
</tr>
<tr>
<td style='text-align: center;'>37</td>
<td style='text-align: center;'> "Check for duplicates using all columns in chicksNew"</td>
</tr>
<tr>
<td style='text-align: center;'>38</td>
<td style='text-align: center;'> "Check for adults with more than one entry for a single date"</td>
</tr>
<tr>
<td style='text-align: center;'>39</td>
<td style='text-align: center;'> "Check for chicks with more than one entry for a single date"</td>
</tr>
<tr>
<td style='text-align: center;'>40</td>
<td style='text-align: center;'> "Check for adults found at more than one farm"</td>
</tr>
<tr>
<td style='text-align: center;'>41</td>
<td style='text-align: center;'> "Check for chicks found at more than one farm"</td>
</tr>
<tr>
<td style='text-align: center;'>42</td>
<td style='text-align: center;'> "Check for adults found at more than one nestbox"</td>
</tr>
<tr>
<td style='text-align: center;'>43</td>
<td style='text-align: center;'> "Check for chicks found at more than one nestbox"</td>
</tr>
<tr>
<td style='text-align: center;'>44</td>
<td style='text-align: center;'> "Make sure that chick conditions are from 3 possible values"</td>
</tr>
<tr>
<td style='text-align: center;'>45</td>
<td style='text-align: center;'> "Make sure that dead or disappeared chicks have 0 for flight code"</td>
</tr>
<tr>
<td style='text-align: center;'>46</td>
<td style='text-align: center;'> "Make sure that living chicks with a 0 flight code are eventually dead or disappeared"</td>
</tr>
<tr>
<td style='text-align: center;'>47</td>
<td style='text-align: center;'> "Make sure that no chick comes back to life"</td>
</tr>
<tr>
<td style='text-align: center;'>48</td>
<td style='text-align: center;'> "Chicks which were followed for 12 days or more should have a band number as id and otherwise they should have a farm/brood id"</td>
</tr>
<tr>
<td style='text-align: center;'>49</td>
<td style='text-align: center;'> "Chicks for which there is a band number but it does not correspond to the id of the chick"</td>
</tr>
<tr>
<td style='text-align: center;'>50</td>
<td style='text-align: center;'> "Broods that are in chicks db but not in broods db"</td>
</tr>
<tr>
<td style='border-bottom: 2px solid grey; text-align: center;'>51</td>
<td style='border-bottom: 2px solid grey; text-align: center;'> "Broods that are in broods db but not in chicks db"</td>
</tr>
</tbody>
</table>