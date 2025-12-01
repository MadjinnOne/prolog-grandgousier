:- module(base_vins, [
    nom/2, prix/2, nez/2, bouche/2, description/2]).

% -------- Base de connaissances sur les vins -------- %

nom(beaumes_de_venise_2015, 'Beaumes-de-Venise 2015').
nom(les_chaboeufs_2013, 'Nuits-Saint-Georges 1er Cru 2013, Les Chaboeufs').
nom(ch_moulin_de_mallet_2014, 'Ch. Moulin de Mallet 2014 - Bordeaux').
nom(ch_la_fleur_baudron_2014, 'Ch. La Fleur Baudron 2014 - Bordeaux Superieur').
nom(ch_bois_vert_elegance_2014, 'Ch. Bois Vert Cuvee Elegance 2014 - Cotes de Bordeaux Blaye').
nom(ch_menota_cuvee_montagrede_2014, 'Ch. Menota Cuvee Montagrede 2014 - Graves').
nom(syrah_2015_vin_de_pays_doc, 'Syrah 2015 - Vin de Pays d\'Oc').
nom(coteaux_bourguignons_2014, 'Coteaux Bourguignons 2014').
nom(champagne_brut_reserve, 'Champagne Brut Reserve - J. Vesselle a Bouzy').

prix(beaumes_de_venise_2015, 12.34).
prix(les_chaboeufs_2013, 42.35).
prix(ch_moulin_de_mallet_2014, 6.85).
prix(ch_la_fleur_baudron_2014, 7.48).
prix(ch_bois_vert_elegance_2014, 8.11).
prix(ch_menota_cuvee_montagrede_2014, 9.46).
prix(syrah_2015_vin_de_pays_doc, 5.14).
prix(coteaux_bourguignons_2014, 7.99).
prix(champagne_brut_reserve, 23.14).

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

nez(ch_moulin_de_mallet_2014,
  [ [ nez, parfume, de, fruits, rouges, telles, que, cerise, et, mure, ',' ],
    [ 'boise integre', '.' ]
  ]).

bouche(ch_moulin_de_mallet_2014,
  [ [ 'tannins fins et bel equilibre avec une touche de boise', '.' ]
  ]).

description(ch_moulin_de_mallet_2014,
  [ [ 'robe grenat aux reflets rubis pour un bordeaux classique', '.' ],
    [ 'joli eventail aromatique et boise integre', '.' ]
  ]).

nez(ch_la_fleur_baudron_2014,
  [ [ 'aromes de fruits compotes et epices fines', '.' ],
    [ 'nuance florale', '.' ]
  ]).

bouche(ch_la_fleur_baudron_2014,
  [ [ 'texture riche et plaisante avec des tannins soyeux', '.' ]
  ]).

description(ch_la_fleur_baudron_2014,
  [ [ 'terroir graveleux et argilo calcaire a l\'arriere de Lussac Saint Emilion', '.' ],
    [ 'bordeaux superieur complet et veloute', '.' ]
  ]).

nez(ch_bois_vert_elegance_2014,
  [ [ 'dominante de violette et cassis avec nuance empyreumatique', '.' ]
  ]).

bouche(ch_bois_vert_elegance_2014,
  [ [ 'saveurs genereuses et rondes avec boise elegant', '.' ]
  ]).

description(ch_bois_vert_elegance_2014,
  [ [ 'appellation cotes de bordeaux blaye appreciee pour la souplesse des vins', '.' ],
    [ 'vin veloute et harmonieux', '.' ]
  ]).

nez(ch_menota_cuvee_montagrede_2014,
  [ [ 'fruits noirs et epices avec nuances empyreumatiques', '.' ]
  ]).

bouche(ch_menota_cuvee_montagrede_2014,
  [ [ 'texture ronde et veloutee aux tannins soyeux', '.' ]
  ]).

description(ch_menota_cuvee_montagrede_2014,
  [ [ 'propriete au sud de pessac leognan sur graves garonnaises', '.' ],
    [ 'finale cacaotee et equilibre remarquable', '.' ]
  ]).

nez(syrah_2015_vin_de_pays_doc,
  [ [ 'fruits rouges et agrumes avec notes epicees', '.' ]
  ]).

bouche(syrah_2015_vin_de_pays_doc,
  [ [ 'caractere enrobe de fruits rouges et nuance de reglisse', '.' ]
  ]).

description(syrah_2015_vin_de_pays_doc,
  [ [ 'elevage sous bois de douze mois pour fondre les tannins', '.' ],
    [ 'vin harmonieux et charmeur', '.' ]
  ]).

nez(coteaux_bourguignons_2014,
  [ [ 'aromes de cerise associes a des nuances florales', '.' ]
  ]).

bouche(coteaux_bourguignons_2014,
  [ [ 'saveurs juteuses dans un style gouleyant et rond', '.' ]
  ]).

description(coteaux_bourguignons_2014,
  [ [ 'vin plaisant et leger avec harmonie du fruit', '.' ],
    [ 'a boire entre 2017 et 2020', '.' ]
  ]).

nez(champagne_brut_reserve,
  [ [ 'nez complexe harmonieux et delicat', '.' ]
  ]).

bouche(champagne_brut_reserve,
  [ [ 'fine et elegante avec beaucoup de rondeur', '.' ]
  ]).

description(champagne_brut_reserve,
  [ [ 'champagne de ceremonie souple et delicat', '.' ],
    [ 'cuvee de tradition de la maison vesselle a bouzy', '.' ]
  ]).

