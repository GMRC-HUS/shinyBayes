       
# <img src = "inst/app/www/hex.png" width = "20%"  style = "display block; margin-left: auto margin-right: auto; align= center">

  Un outil combinant R <img src="inst/app/www/logo.jpg"  height="25">, Shiny <img src="inst/app/www/shiny.png" class="img-fluid" width="25"> et STAN <img src="inst/app/www/stan_logo.png" class="img-fluid" width="25"> pour faciliter la réalisation des analyses statistiques Bayésiennes.

## Installation 

``` r

remotes::install_github("https://github.com/GMRC-HUS/ShiBA")


``` 

## Démarrage de l'application

``` r

ShiBA::run_app(options = list(launch.browser = TRUE))

``` 

SHIBA est une application créée par le Groupe Méthodes en Recherche Clinique (GMRC) des Hôpitaux Universitaires de Strasbourg et les membres de l’équipe IMAGES – RaDoScauba du laboratoire iCUBE (CNRS UMR 7357).</br></br> Elle est entièrement gratuite, sous licence <img src="inst/app/www/by-nc.eu.png" class="img-fluid" width="50">.


SHIBA est une interface graphique permettant de réaliser des analyses bayésiennes via le logiciel de statistiques R et les packages associés sans avoir à coder les instructions nécessaires pour obtenir ces analyses. 


Cet outil est proposé <b>sans aucune garantie de validité sur les résultats.


Si vous utilisez cet outil pour traiter des données médicales et a fortiori relevant de la recherche médicale, vous devez le faire dans le respect de la réglementation en vigueur. Les concepteurs de cet outil ne sont pas responsables de ce que vous en faites . 


