
# BDTREScheck
A package for doing checks on the Tree Swallow databases. Will print a list with the results (problematic lines) of the different checks executed.


## Column names required

**adulte**: ferme, nichoir, id, annee, nnich, idcouvee, heure, jjulien, prefixe, suffixe, idadult, condition, sexe_morpho, age_morpho, sexe_gen, locus_sexe_gen, couleur, age_exact, laile1, laile2, masse, tarse1, tarse2, trougauche, troudroite, pararectrice, plaqueincu, Cause_recapt, commentaire, observateur

**oisillon**: ferme, nichoir, id, annee, nnich, idcouvee, heure, jjulien, prefixe, suffixe, idois2, sexe_gen, locus_sexe_gen, condition, numero_oisillon, jour_suivi, envol, masse, 9primaires1, 9primaires2, tarse1, tarse2, commentaires, manipulateur

**couvee**: idcouvee, id, ferme, nichoir, annee, codesp, nnich, noeufs, noisnes, noisenvol, noismort, dispa_ois, dispa_oeufs, abandon, pred_pot, dponte, dincub, declomin, declomax, denvomin, denvomax, dabanmin, dabanmax, idF1, idM1, idF2, idF3, idM2, idM3, Commentaires


## List of checks

<table class='gmisc_table' style='border-collapse: collapse; margin-top: 1em; margin-bottom: 1em;' >
<thead>
<tr>
<th style='border-bottom: 1px solid grey; border-top: 2px solid grey; text-align: center;'>temp_number</th>
<th style='border-bottom: 1px solid grey; border-top: 2px solid grey; text-align: center;'>check</th>
</tr>
</thead>
<tbody>
<tr>
<td style='text-align: center;'>1</td>
<td style='text-align: center;'>Are column names in each db consistant?</td>
</tr>
<tr>
<td style='text-align: center;'>2</td>
<td style='text-align: center;'>Are number of characters consistant for id-type columns?</td>
</tr>
<tr>
<td style='text-align: center;'>3</td>
<td style='text-align: center;'>Show all unique values in columns for which the number of possible values is restricted</td>
</tr>
<tr>
<td style='text-align: center;'>4</td>
<td style='text-align: center;'></td>
</tr>
<tr>
<td style='text-align: center;'>5</td>
<td style='text-align: center;'>Females without matches in the couvee file</td>
</tr>
<tr>
<td style='text-align: center;'>6</td>
<td style='text-align: center;'>Adult females wrongly assigned to couvee</td>
</tr>
<tr>
<td style='text-align: center;'>7</td>
<td style='text-align: center;'>Males without matches in the couvee file</td>
</tr>
<tr>
<td style='text-align: center;'>8</td>
<td style='text-align: center;'>Adult males wrongly assigned to couvee</td>
</tr>
<tr>
<td style='text-align: center;'>9</td>
<td style='text-align: center;'>Capture date is lower than the laying date and the individual has not been found dead</td>
</tr>
<tr>
<td style='text-align: center;'>10</td>
<td style='text-align: center;'>Adults in the adult DB that are not in the couvee DB</td>
</tr>
<tr>
<td style='text-align: center;'>11</td>
<td style='text-align: center;'>Capture date is later than the min or max departure date from the nest</td>
</tr>
<tr>
<td style='text-align: center;'>12</td>
<td style='text-align: center;'>Capture date is later than the min or max date of nest abandonment</td>
</tr>
<tr>
<td style='text-align: center;'>13</td>
<td style='text-align: center;'>Capture date of young is later than the minimal abandonment date if nest was abandoned</td>
</tr>
<tr>
<td style='text-align: center;'>14</td>
<td style='text-align: center;'>Capture date of young is before the laying date</td>
</tr>
<tr>
<td style='text-align: center;'>15</td>
<td style='text-align: center;'>Sex/age incoherencies</td>
</tr>
<tr>
<td style='text-align: center;'>16</td>
<td style='text-align: center;'>Capture time outside x and y</td>
</tr>
<tr>
<td style='text-align: center;'>17</td>
<td style='text-align: center;'>Some colors not in the list of possible values</td>
</tr>
<tr>
<td style='text-align: center;'>18</td>
<td style='text-align: center;'>Wing measurement outside the range of likely values</td>
</tr>
<tr>
<td style='text-align: center;'>19</td>
<td style='text-align: center;'>Weight measurement outside the range of likely values</td>
</tr>
<tr>
<td style='text-align: center;'>20</td>
<td style='text-align: center;'>Tarsus measurement outside the range of likely values</td>
</tr>
<tr>
<td style='text-align: center;'>21</td>
<td style='text-align: center;'>Male with brood patch</td>
</tr>
<tr>
<td style='text-align: center;'>22</td>
<td style='text-align: center;'>New installed band found in the previous years</td>
</tr>
<tr>
<td style='text-align: center;'>23</td>
<td style='text-align: center;'>Visits are not all 2 days apart for the following farms in the adult DB</td>
</tr>
<tr>
<td style='text-align: center;'>24</td>
<td style='text-align: center;'>Visits are not all 2 days apart for the following farms in the oisillon DB</td>
</tr>
<tr>
<td style='text-align: center;'>25</td>
<td style='text-align: center;'>Check for spaces in ferme ids</td>
</tr>
<tr>
<td style='text-align: center;'>26</td>
<td style='text-align: center;'>Check for duplicates using all columns in each database</td>
</tr>
<tr>
<td style='text-align: center;'>27</td>
<td style='text-align: center;'>Check for adults or chicks with more than one entry for a single date</td>
</tr>
<tr>
<td style='text-align: center;'>28</td>
<td style='text-align: center;'>Check for adults or chicks found at more than one farm</td>
</tr>
<tr>
<td style='text-align: center;'>29</td>
<td style='text-align: center;'>Check for adults or chicks found at more than one nestbox</td>
</tr>
<tr>
<td style='text-align: center;'>30</td>
<td style='text-align: center;'>Make sure that chick conditions are from 3 possible values</td>
</tr>
<tr>
<td style='text-align: center;'>31</td>
<td style='text-align: center;'>Make sure that dead or disappeared chicks have 0 for flight code</td>
</tr>
<tr>
<td style='text-align: center;'>32</td>
<td style='text-align: center;'>Make sure that living chicks with a 0 flight code are eventually dead or disappeared</td>
</tr>
<tr>
<td style='text-align: center;'>33</td>
<td style='text-align: center;'>Make sur that no chick comes back to life</td>
</tr>
<tr>
<td style='text-align: center;'>34</td>
<td style='text-align: center;'>Chicks which were followed for 12 days or more should have a band number as id and otherwise they should have a farm/brood id</td>
</tr>
<tr>
<td style='text-align: center;'>35</td>
<td style='text-align: center;'>Chicks for which there is a band number but it does not correspond to the id of the chick</td>
</tr>
<tr>
<td style='text-align: center;'>36</td>
<td style='text-align: center;'></td>
</tr>
<tr>
<td style='text-align: center;'>37</td>
<td style='text-align: center;'></td>
</tr>
<tr>
<td style='text-align: center;'>38</td>
<td style='text-align: center;'></td>
</tr>
<tr>
<td style='text-align: center;'>39</td>
<td style='text-align: center;'></td>
</tr>
<tr>
<td style='text-align: center;'>40</td>
<td style='text-align: center;'></td>
</tr>
<tr>
<td style='text-align: center;'>41</td>
<td style='text-align: center;'></td>
</tr>
<tr>
<td style='text-align: center;'>42</td>
<td style='text-align: center;'></td>
</tr>
<tr>
<td style='text-align: center;'>43</td>
<td style='text-align: center;'></td>
</tr>
<tr>
<td style='border-bottom: 2px solid grey; text-align: center;'>44</td>
<td style='border-bottom: 2px solid grey; text-align: center;'></td>
</tr>
</tbody>
</table>