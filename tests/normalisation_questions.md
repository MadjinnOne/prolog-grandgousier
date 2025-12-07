# Questions de test pour la normalisation des vins

Ces exemples peuvent être posés au programme (ou utilisés via `nom_vins_uniforme/2`)
pour vérifier que chaque nom de vin est bien reconnu et remplacé par son
identifiant interne issu de `nom/2`.

## Nuits-Saint-Georges (les_chaboeufs_2013)
- "Quel nez présente le nuits saint georges 2013 ?"
- "Que donne le Nuits-Saint-Georges en bouche ?"

## Chambolle-Musigny (chambolle_musigny_2014)
- "Pourriez-vous m'en dire plus sur chambolle musigny 2014 ?"
- "Quel nez présente le Chambolle-Musigny ?"

## La Fleur de Pomys (fleur_de_pomys_2012)
- "Que donne la fleur de pomys en bouche ?"
- "Quel nez présente la fleur de pomys 2012 ?"

## Beaumes-de-Venise (beaumes_de_venise_2015)
- "Que donne le Beaumes-de-Venise 2015 en bouche ?"
- "Pourriez-vous m'en dire plus sur beaumes de venise ?"

## Autres vins de la base
- Pour chaque entrée `nom(Id, Label)` :
  - "Quel nez présente le <Label> ?"
  - "Que donne le <Label> en bouche ?"
  - "Pourriez-vous m'en dire plus sur <Label> ?"

Les variantes avec ou sans millésime doivent toutes être reconnues : le millésime
peut apparaître comme dernier mot ou être omis.
