:- module(base_vins, [
    nom/2, prix/2, nez/2, bouche/2, description/2]).

% -------- Base de connaissances sur les vins -------- %

nom(beaumes_de_venise_2015, 'Beaumes-de-Venise 2015').
nom(les_chaboeufs_2013, 'Nuits-Saint-Georges 1er Cru 2013, Les Chaboeufs').

prix(beaumes_de_venise_2015, 12.34).
prix(les_chaboeufs_2013, 42.35).

bouche(beaumes_de_venise_2015,
  [ [ 'les aromes de fraise, de violette cotoient les nuances' ],
    [ 'de baies de genevrier, de sureau et une delicate touche' ],
    [ 'de fleur d\'oranger. Cette intensite se poursuit en' ],
    [ 'bouche avec des saveurs juteuses, racees et tres elegantes', '.' ]
  ]).

nez(beaumes_de_venise_2015,
  [ [ nez, intensement, parfume, '.' ]
]).

description(beaumes_de_venise_2015,
  [ [ 'vignoble situe au sud-est des Dentelles de Montmirail', '.' ],
    [ 'grand vin', '.' ]
  ]).

