
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
  year,
  adultsNew=NULL,
  broodsNew=NULL,
  chicksNew=NULL,
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
- find what to do with read_excel warnings on expectation about column format
- change default values of databases names
- think of a way to remove warnings associated with the check on the number of characters based on column names that are not in all databases
- think about the input db type given and arguments options (a data.frame already in the environment, a path to an excel, csv file, a remote database etc.)
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
<td style='text-align: center;'> "GENERAL: Remove rows with NA id's in broodsNew"</td>
</tr>
<tr>
<td style='text-align: center;'>2</td>
<td style='text-align: center;'> "GENERAL: Remove rows with NA id's in adultsNew"</td>
</tr>
<tr>
<td style='text-align: center;'>3</td>
<td style='text-align: center;'> "GENERAL: Remove rows with NA id's in chicksNew"</td>
</tr>
<tr>
<td style='text-align: center;'>4</td>
<td style='text-align: center;'> "GENERAL: Are column names in adult consistent?"</td>
</tr>
<tr>
<td style='text-align: center;'>5</td>
<td style='text-align: center;'> "GENERAL: Are column names in chicksNew consistent?"</td>
</tr>
<tr>
<td style='text-align: center;'>6</td>
<td style='text-align: center;'> "GENERAL: Are column names in broodsNew consistent?"</td>
</tr>
<tr>
<td style='text-align: center;'>7</td>
<td style='text-align: center;'> "GENERAL: Are column names consistant across old and new databases"</td>
</tr>
<tr>
<td style='text-align: center;'>8</td>
<td style='text-align: center;'> "ADULTS: Wrong ferme id observed"</td>
</tr>
<tr>
<td style='text-align: center;'>9</td>
<td style='text-align: center;'> "NESTLINGS: Wrong ferme id observed"</td>
</tr>
<tr>
<td style='text-align: center;'>10</td>
<td style='text-align: center;'> "BROODS: Wrong ferme id observed"</td>
</tr>
<tr>
<td style='text-align: center;'>11</td>
<td style='text-align: center;'> "ADULTS: Wrong nichoir id observed"</td>
</tr>
<tr>
<td style='text-align: center;'>12</td>
<td style='text-align: center;'> "NESTLINGS: Wrong nichoir id observed"</td>
</tr>
<tr>
<td style='text-align: center;'>13</td>
<td style='text-align: center;'> "BROODS: Wrong nichoir id observed"</td>
</tr>
<tr>
<td style='text-align: center;'>14</td>
<td style='text-align: center;'> "ADULTS: id column doesn't correspond to ferme + nichoir id"</td>
</tr>
<tr>
<td style='text-align: center;'>15</td>
<td style='text-align: center;'> "NESTLINGS: id column doesn't correspond to ferme + nichoir id"</td>
</tr>
<tr>
<td style='text-align: center;'>16</td>
<td style='text-align: center;'> "BROODS: id column doesn't correspond to ferme + nichoir id"</td>
</tr>
<tr>
<td style='text-align: center;'>17</td>
<td style='text-align: center;'> "ADULTS: idcouv doesn't correspond to ferme + nichoir + annee + nnich column"</td>
</tr>
<tr>
<td style='text-align: center;'>18</td>
<td style='text-align: center;'> "NESTLINGS: idcouv doesn't correspond to ferme + nichoir + annee + nnich column"</td>
</tr>
<tr>
<td style='text-align: center;'>19</td>
<td style='text-align: center;'> "BROODS: idcouv doesn't correspond to ferme + nichoir + annee + nnich column"</td>
</tr>
<tr>
<td style='text-align: center;'>20</td>
<td style='text-align: center;'> "ADULTS: Wrong prefixe name observed (is it a new prefixe?)"</td>
</tr>
<tr>
<td style='text-align: center;'>21</td>
<td style='text-align: center;'> "NESTLINGS: Wrong prefixe name observed (is it a new prefixe?)"</td>
</tr>
<tr>
<td style='text-align: center;'>22</td>
<td style='text-align: center;'> "ADULTS: Show all unique values in columns for which the number of possible values is restricted"</td>
</tr>
<tr>
<td style='text-align: center;'>23</td>
<td style='text-align: center;'> "BROODS: Show all unique values in columns for which the number of possible values is restricted"</td>
</tr>
<tr>
<td style='text-align: center;'>24</td>
<td style='text-align: center;'> "NESTLINGS: Show all unique values in columns for which the number of possible values is restricted"</td>
</tr>
<tr>
<td style='text-align: center;'>25</td>
<td style='text-align: center;'> "ADULTS/BROODS: Females assigned to an idcouv in adults db but no female is assigned to this idcouv in broods db (check capture dates)"</td>
</tr>
<tr>
<td style='text-align: center;'>26</td>
<td style='text-align: center;'> "ADULTS/BROODS: Females assigned to an idcouv in adults db but not referenced in broods db (idF2 or idF3)"</td>
</tr>
<tr>
<td style='text-align: center;'>27</td>
<td style='text-align: center;'> "ADULTS/BROODS: Females captured at a nestbox but not assigned to an idcouv in adults db (check nnich)"</td>
</tr>
<tr>
<td style='text-align: center;'>28</td>
<td style='text-align: center;'> "ADULTS/BROODS: Females captured at a nestbox but assigned to a wrong idcouv in adults db (check nnich)"</td>
</tr>
<tr>
<td style='text-align: center;'>29</td>
<td style='text-align: center;'> "ADULTS/BROODS: Males assigned to an idcouv in adults db but no male is assigned to this idcouv in broods db (check capture dates)"</td>
</tr>
<tr>
<td style='text-align: center;'>30</td>
<td style='text-align: center;'> "ADULTS/BROODS: Males assigned to an idcouv in adults db but not referenced in broods db (idM2 or idM3)"</td>
</tr>
<tr>
<td style='text-align: center;'>31</td>
<td style='text-align: center;'> "ADULTS/BROODS: Males captured at a nestbox but not assigned to an idcouv in adults db (check nnich)"</td>
</tr>
<tr>
<td style='text-align: center;'>32</td>
<td style='text-align: center;'> "ADULTS/BROODS: Males captured at a nestbox but assigned to a wrong idcouv in adults db (check nnich)"</td>
</tr>
<tr>
<td style='text-align: center;'>33</td>
<td style='text-align: center;'> "BROODS: Female reference as second female when only one female captured (change to idF1 - some exceptions possible)"</td>
</tr>
<tr>
<td style='text-align: center;'>34</td>
<td style='text-align: center;'> "BROODS: Male reference as second males when only one male captured (change to idM1)"</td>
</tr>
<tr>
<td style='text-align: center;'>35</td>
<td style='text-align: center;'> "BROODS: Two males captured in the same nestbox but not properly reported (idM2 and idM3, not idM1)"</td>
</tr>
<tr>
<td style='text-align: center;'>36</td>
<td style='text-align: center;'> "ADULTS: Sex/age incoherencies within the current year"</td>
</tr>
<tr>
<td style='text-align: center;'>37</td>
<td style='text-align: center;'> "ADULTS: sex/age incoherencies between years"</td>
</tr>
<tr>
<td style='text-align: center;'>38</td>
<td style='text-align: center;'> "ADULTS: Capture time outside 06:30 and 20:00"</td>
</tr>
<tr>
<td style='text-align: center;'>39</td>
<td style='text-align: center;'> "NESTLINGS: Capture time outside 06:30 and 20:00"</td>
</tr>
<tr>
<td style='text-align: center;'>40</td>
<td style='text-align: center;'> "ADULTS: Some colors not in the list of possible values"</td>
</tr>
<tr>
<td style='text-align: center;'>41</td>
<td style='text-align: center;'> "ADULTS: Wing measurement outside the range of likely values (105-125 mm)"</td>
</tr>
<tr>
<td style='text-align: center;'>42</td>
<td style='text-align: center;'> "ADULTS: Wing measurement 1 and 2 too far apart (>1 mm)"</td>
</tr>
<tr>
<td style='text-align: center;'>43</td>
<td style='text-align: center;'> "ADULTS: Weight measurements outside the range of likely values (15-30g)"</td>
</tr>
<tr>
<td style='text-align: center;'>44</td>
<td style='text-align: center;'> "ADULTS: Tarsus measurements outside the range of likely values (10-14 mm)"</td>
</tr>
<tr>
<td style='text-align: center;'>45</td>
<td style='text-align: center;'> "ADULTS: tarsus measurement 1 and 2 too far apart (>0.1 mm)"</td>
</tr>
<tr>
<td style='text-align: center;'>46</td>
<td style='text-align: center;'> "ADULTS: Wrong condition status"</td>
</tr>
<tr>
<td style='text-align: center;'>47</td>
<td style='text-align: center;'> "ADULTS: Wrong plaqueincu status"</td>
</tr>
<tr>
<td style='text-align: center;'>48</td>
<td style='text-align: center;'> "ADULTS: Male with brood patch (plaqueincu)"</td>
</tr>
<tr>
<td style='text-align: center;'>49</td>
<td style='text-align: center;'> "ADULTS: Wrong Cause_capture status"</td>
</tr>
<tr>
<td style='text-align: center;'>50</td>
<td style='text-align: center;'> "Newly installed band found in the previous years"</td>
</tr>
<tr>
<td style='text-align: center;'>51</td>
<td style='text-align: center;'> "ADULTS: Visits are not all 2 days apart for the following farms"</td>
</tr>
<tr>
<td style='text-align: center;'>52</td>
<td style='text-align: center;'> "NESTLINGS: Visits are not all 2 days apart for the following farms"</td>
</tr>
<tr>
<td style='text-align: center;'>53</td>
<td style='text-align: center;'> "ADULTS: Check for duplicates using all columns"</td>
</tr>
<tr>
<td style='text-align: center;'>54</td>
<td style='text-align: center;'> "BROODS: Check for duplicates using all columns"</td>
</tr>
<tr>
<td style='text-align: center;'>55</td>
<td style='text-align: center;'> "CHICKS: Check for duplicates using all columns"</td>
</tr>
<tr>
<td style='text-align: center;'>56</td>
<td style='text-align: center;'> "ADULTS: Check for adults with more than one entry for a single date"</td>
</tr>
<tr>
<td style='text-align: center;'>57</td>
<td style='text-align: center;'> "NESTLINGS: Check for chicks with more than one entry for a single date"</td>
</tr>
<tr>
<td style='text-align: center;'>58</td>
<td style='text-align: center;'> "NESTLINGS: Check for chicks with more than one entry for a single age"</td>
</tr>
<tr>
<td style='text-align: center;'>59</td>
<td style='text-align: center;'> "ADULTS: Check for adults found at more than one farm (maybe not an error)"</td>
</tr>
<tr>
<td style='text-align: center;'>60</td>
<td style='text-align: center;'> "NESTLINGS: Check for nestlings found at more than one nestbox"</td>
</tr>
<tr>
<td style='text-align: center;'>61</td>
<td style='text-align: center;'> "NESTLINGS/BROODS: Capture date of young is later than the minimal abandonment date if nest was abandoned"</td>
</tr>
<tr>
<td style='text-align: center;'>62</td>
<td style='text-align: center;'> "NESTLINGS/BROODS: Capture date of young is before the laying date"</td>
</tr>
<tr>
<td style='text-align: center;'>63</td>
<td style='text-align: center;'> "NESTLINGS/BROODS: jjulien of young that doesn't correspond to declomax + jour_suivi"</td>
</tr>
<tr>
<td style='text-align: center;'>64</td>
<td style='text-align: center;'> "NESTLINGS: Wrong chick conditions (4 possible values; vivant, disparu, mort or disparuj16)"</td>
</tr>
<tr>
<td style='text-align: center;'>65</td>
<td style='text-align: center;'> "NESTLINGS: Dead or disappeared nestlings without a 0 for flight code (few exceptions possibles, see comments)"</td>
</tr>
<tr>
<td style='text-align: center;'>66</td>
<td style='text-align: center;'> "NESTLINGS: Nestling with disparuj16 condition but without a 1 for flight code"</td>
</tr>
<tr>
<td style='text-align: center;'>67</td>
<td style='text-align: center;'> "NESTLINGS: Make sure that living nestlings with a 0 flight code are eventually dead or disappeared"</td>
</tr>
<tr>
<td style='text-align: center;'>68</td>
<td style='text-align: center;'> "NESTLINGS: Make sure that no nestling comes back to life"</td>
</tr>
<tr>
<td style='text-align: center;'>69</td>
<td style='text-align: center;'> "NESTLINGS: Nestlings which were followed for 12 days or more should have a band number as id and otherwise they should have a farm/brood id (maybe an exception, see comments)"</td>
</tr>
<tr>
<td style='text-align: center;'>70</td>
<td style='text-align: center;'> "NESTLINGS: Chicks for which there is a band number but it does not correspond to the id of the chick"</td>
</tr>
<tr>
<td style='text-align: center;'>71</td>
<td style='text-align: center;'> "NESTLINGS/BROODS: Broods that are in chicks db but not in broods db"</td>
</tr>
<tr>
<td style='border-bottom: 2px solid grey; text-align: center;'>72</td>
<td style='border-bottom: 2px solid grey; text-align: center;'> "NESTLINGS/BROODS: TRES broods with at least one nestling that are in broods db but not in chicks db"</td>
</tr>
</tbody>
</table>