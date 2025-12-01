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
nom(ch_paret_2012, 'Ch. Paret 2012 - Cotes de Bordeaux Castillon').
nom(madiran_vieilles_vignes_2006, 'Madiran Vieilles Vignes 2006').
nom(ch_du_moulin_neuf_prestige_2014, 'Ch. du Moulin Neuf Cuvee Prestige 2014 - Medoc').
nom(ch_milon_la_grave_particuliere_2012, 'Ch. Milon La Grave Cuvee Particuliere 2012 - Lussac Saint Emilion').
nom(ch_roc_de_binet_2010, 'Ch. Roc de Binet 2010 - Montagne Saint Emilion').
nom(ch_ruat_petit_poujeaux_2010, 'Ch. Ruat Petit Poujeaux 2010 - Moulis en Medoc').
nom(ch_les_polyanthas_2010, 'Ch. Les Polyanthas 2010 - Saint Emilion').
nom(ch_la_menotte_2012, 'Ch. La Menotte 2012 - Lalande de Pomerol').
nom(la_fleur_de_pomys_2012, 'La Fleur de Pomys 2012 - Saint Estephe').
nom(pauillac_2011, 'Pauillac 2011 - Florilege').
nom(florilege_saint_julien_2010, 'Florilege Saint Julien 2010').
nom(florilege_pomerol_2012, 'Florilege Pomerol 2012').
nom(cotes_du_rhone_villages_pierre_charlotte_2014, 'Cotes du Rhone Villages 2014 - Cuvee Pierre et Charlotte').
nom(tautavel_2014, 'Tautavel 2014 - Cotes du Roussillon Villages').
nom(lirac_2015, 'Lirac 2015').
nom(cairanne_2014, 'Cairanne 2014 - Cotes du Rhone Villages').
nom(vacqueyras_2014, 'Vacqueyras 2014').
nom(saint_joseph_2014, 'Saint Joseph 2014').
nom(gigondas_2014, 'Gigondas 2014').
nom(chateauneuf_du_pape_rouge_2013, 'Chateauneuf du Pape rouge 2013').
nom(hermitage_rouge_2007, 'Hermitage rouge 2007').
nom(bourgogne_pinot_noir_les_marnes_2014, 'Bourgogne Pinot Noir 2014 - Cuvee les Marnes').
nom(hautes_cotes_de_nuits_2014, 'Hautes Cotes de Nuits 2014').
nom(savigny_les_beaune_2014, 'Savigny les Beaune 2014').
nom(savigny_les_beaune_premier_cru_2014, 'Savigny les Beaune 1er Cru 2014 - Aux Serpentieres').
nom(aloxe_corton_2014, 'Aloxe Corton 2014').
nom(chambolle_musigny_premier_cru_2012, 'Chambolle Musigny 1er Cru 2012 - Les Noirots').
nom(chiroubles_2013, 'Chiroubles 2013').
nom(fleurie_2015, 'Fleurie 2015').
nom(moulin_a_vent_2014, 'Moulin a Vent 2014').
nom(chinon_vieilles_vignes_2014, 'Chinon Vieilles Vignes 2014').
nom(sancerre_rouge_2015, 'Sancerre rouge 2015').
nom(les_guignards_2015, 'Les Guignards 2015 - AOC Cotes de Duras').
nom(chardonnay_exception_2016, 'Chardonnay Exception 2016 - Vin de Pays d\'Oc').
nom(cotes_du_rhone_blanc_pierre_charlotte_2016, 'Cotes du Rhone 2016 - Cuvee Pierre et Charlotte').
nom(ch_le_druc_2015, 'Ch. Le Druc 2015 - Graves').
nom(laudun_2016, 'Laudun 2016 - Cotes du Rhone Villages').
nom(vouvray_blanc_sec_2016, 'Vouvray blanc sec 2016').
nom(macon_villages_2015, 'Macon Villages 2015').
nom(pinot_gris_2015, 'Pinot Gris 2015').
nom(gewurztraminer_2015, 'Gewurztraminer 2015').
nom(vire_clesse_2016, 'Vire Clesse 2016').
nom(sancerre_blanc_verdigny_2015, 'Sancerre blanc 2015 - Vignoble de Verdigny').
nom(vacqueyras_blanc_2016, 'Vacqueyras 2016 - Blanc').
nom(hautes_cotes_de_beaune_2015, 'Hautes Cotes de Beaune 2015').
nom(pouilly_fuisse_2014, 'Pouilly Fuisse 2014').
nom(chablis_premier_cru_montmains_2014, 'Chablis 1er Cru Montmains 2014').
nom(condrieu_2015, 'Condrieu 2015').
nom(cremant_de_loire_brut, 'Cremant de Loire Brut').
nom(champagne_extra_brut, 'Champagne Extra Brut - J. Vesselle a Bouzy').
nom(champagne_brut_oeil_de_perdrix, 'Champagne Brut Oeil de Perdrix - J. Vesselle a Bouzy').
nom(champagne_brut_rose_de_saignee, 'Champagne Brut Rose de Saignee - J. Vesselle a Bouzy').
nom(champagne_brut_or_blanc, 'Champagne Brut Or Blanc - B. Cocteaux a Montgenost').
nom(champagne_brut_prestige, 'Champagne Brut Prestige - J. Vesselle a Bouzy').
nom(cognac_trois_etoiles, 'Cognac Trois Etoiles').
nom(cognac_fine_champagne_vsop, 'Cognac Fine Champagne VSOP').
nom(cognac_grande_champagne_hors_age, 'Cognac Grande Champagne Hors Age').

prix(beaumes_de_venise_2015, 12.34).
prix(les_chaboeufs_2013, 42.35).
prix(ch_moulin_de_mallet_2014, 6.85).
prix(ch_la_fleur_baudron_2014, 7.48).
prix(ch_bois_vert_elegance_2014, 8.11).
prix(ch_menota_cuvee_montagrede_2014, 9.46).
prix(syrah_2015_vin_de_pays_doc, 5.14).
prix(coteaux_bourguignons_2014, 7.99).
prix(champagne_brut_reserve, 23.14).
prix(ch_paret_2012, 8.68).
prix(madiran_vieilles_vignes_2006, 9.92).
prix(ch_du_moulin_neuf_prestige_2014, 10.30).
prix(ch_milon_la_grave_particuliere_2012, 12.54).
prix(ch_roc_de_binet_2010, 12.69).
prix(ch_ruat_petit_poujeaux_2010, 14.37).
prix(ch_les_polyanthas_2010, 15.06).
prix(ch_la_menotte_2012, 15.71).
prix(la_fleur_de_pomys_2012, 21.36).
prix(pauillac_2011, 30.65).
prix(florilege_saint_julien_2010, 30.65).
prix(florilege_pomerol_2012, 31.04).
prix(cotes_du_rhone_villages_pierre_charlotte_2014, 7.36).
prix(tautavel_2014, 7.37).
prix(lirac_2015, 9.23).
prix(cairanne_2014, 9.98).
prix(vacqueyras_2014, 13.92).
prix(saint_joseph_2014, 16.34).
prix(gigondas_2014, 16.77).
prix(chateauneuf_du_pape_rouge_2013, 23.87).
prix(hermitage_rouge_2007, 33.98).
prix(bourgogne_pinot_noir_les_marnes_2014, 11.92).
prix(hautes_cotes_de_nuits_2014, 15.16).
prix(savigny_les_beaune_2014, 20.46).
prix(savigny_les_beaune_premier_cru_2014, 25.82).
prix(aloxe_corton_2014, 26.02).
prix(chambolle_musigny_premier_cru_2012, 63.85).
prix(chiroubles_2013, 8.41).
prix(fleurie_2015, 11.00).
prix(moulin_a_vent_2014, 11.40).
prix(chinon_vieilles_vignes_2014, 8.39).
prix(sancerre_rouge_2015, 15.13).
prix(les_guignards_2015, 5.30).
prix(chardonnay_exception_2016, 6.47).
prix(cotes_du_rhone_blanc_pierre_charlotte_2016, 7.02).
prix(ch_le_druc_2015, 7.62).
prix(laudun_2016, 8.83).
prix(vouvray_blanc_sec_2016, 10.10).
prix(macon_villages_2015, 10.59).
prix(pinot_gris_2015, 10.59).
prix(gewurztraminer_2015, 12.28).
prix(vire_clesse_2016, 12.77).
prix(sancerre_blanc_verdigny_2015, 16.01).
prix(vacqueyras_blanc_2016, 16.37).
prix(hautes_cotes_de_beaune_2015, 16.58).
prix(pouilly_fuisse_2014, 18.67).
prix(chablis_premier_cru_montmains_2014, 21.13).
prix(condrieu_2015, 32.07).
prix(cremant_de_loire_brut, 10.95).
prix(champagne_extra_brut, 24.27).
prix(champagne_brut_oeil_de_perdrix, 24.47).
prix(champagne_brut_rose_de_saignee, 25.23).
prix(champagne_brut_or_blanc, 25.64).
prix(champagne_brut_prestige, 29.22).
prix(cognac_trois_etoiles, 20.87).
prix(cognac_fine_champagne_vsop, 22.26).
prix(cognac_grande_champagne_hors_age, 32.28).

bouche(beaumes_de_venise_2015,
  [ [ 'les aromes de fraise, de violette cotoient les nuances' ],
    [ 'de baies de genevrier, de sureau et une delicate touche' ],
    [ 'de fleur d\'oranger. Cette intensite se poursuit en' ],
    [ 'bouche avec des saveurs juteuses, racees et tres elegantes', '.' ]
  ]).

nez(beaumes_de_venise_2015,
  [ [ nez, intensement, parfume, '.' ]
]).

nez(les_chaboeufs_2013,
  [ [ 'belle intensite aromatique de griotte sous bois et champignon', '.' ]
  ]).

bouche(les_chaboeufs_2013,
  [ [ 'texture epaisse reunissant puissance richesse rondeur et longueur', '.' ]
  ]).

description(les_chaboeufs_2013,
  [ [ 'parcelle escarpee et pierreuse de nuits saint georges', '.' ],
    [ 'premier cru genereux et harmonieux', '.' ]
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

nez(ch_paret_2012,
  [ [ 'nez profond avec fruits noirs chocolat et sous bois', '.' ]
  ]).

bouche(ch_paret_2012,
  [ [ 'bouche dense riche et concentree avec grande maturite', '.' ]
  ]).

description(ch_paret_2012,
  [ [ 'coteaux argilo calcaire voisins de saint emilion', '.' ],
    [ 'vin complet de castillon au beau caractere', '.' ]
  ]).

nez(madiran_vieilles_vignes_2006,
  [ [ 'notes de fruits noirs avec touche cacao', '.' ]
  ]).

bouche(madiran_vieilles_vignes_2006,
  [ [ 'tanins riches avec du gras pour un madiran puissant', '.' ]
  ]).

description(madiran_vieilles_vignes_2006,
  [ [ 'cepage tannat charpente mais sans austere', '.' ],
    [ 'accompagne des plats forts en gout', '.' ]
  ]).

nez(ch_du_moulin_neuf_prestige_2014,
  [ [ 'fruits noirs confitures avec notes fumees et chocolatees', '.' ]
  ]).

bouche(ch_du_moulin_neuf_prestige_2014,
  [ [ 'belle maturite avec tannins murs et boise integre', '.' ]
  ]).

description(ch_du_moulin_neuf_prestige_2014,
  [ [ 'petite propriete sur terroir graveleux argilo ferrugineux', '.' ],
    [ 'dominance cabernet reussie en medoc 2014', '.' ]
  ]).

nez(ch_milon_la_grave_particuliere_2012,
  [ [ 'fruits rouges violette et nuances empyreumatiques', '.' ]
  ]).

bouche(ch_milon_la_grave_particuliere_2012,
  [ [ 'saveurs denses cremeuses et veloutees', '.' ]
  ]).

description(ch_milon_la_grave_particuliere_2012,
  [ [ 'cuvee presque exclusivement merlot style veloute', '.' ],
    [ 'finale distinguee savoureuse', '.' ]
  ]).

nez(ch_roc_de_binet_2010,
  [ [ 'cassis cerise violette et truffe', '.' ]
  ]).

bouche(ch_roc_de_binet_2010,
  [ [ 'vin riche dense avec tannins puissants enrobes', '.' ]
  ]).

description(ch_roc_de_binet_2010,
  [ [ 'montagne saint emilion dans une grande annee', '.' ],
    [ 'sensations de richesse et d etoffe', '.' ]
  ]).

nez(ch_ruat_petit_poujeaux_2010,
  [ [ 'bouquet intense de fruits noirs avec nuances torrefiees', '.' ]
  ]).

bouche(ch_ruat_petit_poujeaux_2010,
  [ [ 'saveurs charnues concentrees et onctueuses', '.' ]
  ]).

description(ch_ruat_petit_poujeaux_2010,
  [ [ 'terroir de fines graves blanches type haut medoc', '.' ],
    [ 'finale longue et distinguee', '.' ]
  ]).

nez(ch_les_polyanthas_2010,
  [ [ 'aromes truffes fruites et epices', '.' ]
  ]).

bouche(ch_les_polyanthas_2010,
  [ [ 'style onctueux rond et soyeux avec profondeur', '.' ]
  ]).

description(ch_les_polyanthas_2010,
  [ [ 'petite propriete de 4 hectares sur sables et graves', '.' ],
    [ 'saint emilion de grande classe', '.' ]
  ]).

nez(ch_la_menotte_2012,
  [ [ 'bouquet intense fruits fleurs nuances truffees et cafe', '.' ]
  ]).

bouche(ch_la_menotte_2012,
  [ [ 'texture veloutee racee et harmonieuse', '.' ]
  ]).

description(ch_la_menotte_2012,
  [ [ 'lalande de pomerol au charme et a l etoffe', '.' ],
    [ 'tres present tout au long de la bouche', '.' ]
  ]).

nez(la_fleur_de_pomys_2012,
  [ [ 'nez empyreumatique myrtille cassis et boise integre', '.' ]
  ]).

bouche(la_fleur_de_pomys_2012,
  [ [ 'saveurs riches concentrees avec grande densite', '.' ]
  ]).

description(la_fleur_de_pomys_2012,
  [ [ 'saint estephe argilo graveleux plus charpente', '.' ],
    [ 'vin complet dense et race', '.' ]
  ]).

nez(pauillac_2011,
  [ [ 'fruits noirs avec touche bois de vigne et reglisse', '.' ]
  ]).

bouche(pauillac_2011,
  [ [ 'puissante aux tannins elegants et enrobes', '.' ]
  ]).

description(pauillac_2011,
  [ [ 'florilege grand bordeaux profond et distingue', '.' ],
    [ 'compagnon ideal des plats de gibier', '.' ]
  ]).

nez(florilege_saint_julien_2010,
  [ [ 'nez dense cremeux avec trame tannique enrobee', '.' ]
  ]).

bouche(florilege_saint_julien_2010,
  [ [ 'sensation concentree puissante et longue', '.' ]
  ]).

description(florilege_saint_julien_2010,
  [ [ 'saint julien rac e associant concentration puissance et distinction', '.' ],
    [ 'vin de present et d avenir', '.' ]
  ]).

nez(florilege_pomerol_2012,
  [ [ 'dominante de fruits rouges avec nuances torrefiees et epicees', '.' ]
  ]).

bouche(florilege_pomerol_2012,
  [ [ 'saveurs riches intenses et veloutees', '.' ]
  ]).

description(florilege_pomerol_2012,
  [ [ 'terroir argile fer rugineuse specifique de pomerol', '.' ],
    [ 'ensemble cremeux dense et concentre', '.' ]
  ]).

nez(cotes_du_rhone_villages_pierre_charlotte_2014,
  [ [ 'bouquet charmant fruits rouges mures et fruits a noyau', ',' ],
    [ 'nuances de garrigue bois de cedre et agrumes', '.' ]
  ]).

bouche(cotes_du_rhone_villages_pierre_charlotte_2014,
  [ [ 'attaque fruitee epicee puis nuance reglisse', '.' ],
    [ 'finale fine et elegante', '.' ]
  ]).

description(cotes_du_rhone_villages_pierre_charlotte_2014,
  [ [ 'terroir argilo limoneux a proximite de l aygues', '.' ],
    [ 'cotes du rhone villages de charme', '.' ]
  ]).

nez(tautavel_2014,
  [ [ 'fruits noirs epices chocolat et garrigue', '.' ]
  ]).

bouche(tautavel_2014,
  [ [ 'saveurs de fruits frais et epices avec grande concentration', '.' ]
  ]).

description(tautavel_2014,
  [ [ 'cotes du roussillon villages a la finale cacao dense', '.' ],
    [ 'puissant juteux et harmonieux', '.' ]
  ]).

nez(lirac_2015,
  [ [ 'fruits a noyaux et epices cannelle reglisse', '.' ]
  ]).

bouche(lirac_2015,
  [ [ 'bouche dense et minerale avec fruits et epices', '.' ],
    [ 'finale agreable souvenirs d aiguilles de pin', '.' ]
  ]).

description(lirac_2015,
  [ [ 'rive droite du rhone face a chateauneuf du pape', '.' ],
    [ 'grande annee au souvenir tres plaisant', '.' ]
  ]).

nez(cairanne_2014,
  [ [ 'aromes tres expressifs guimauve genevrier et tabac', '.' ]
  ]).

bouche(cairanne_2014,
  [ [ 'vin equilibre aux tannins soyeux', '.' ]
  ]).

description(cairanne_2014,
  [ [ 'terroir en altitude a cote de rasteau', '.' ],
    [ 'finale de caractere et persistance', '.' ]
  ]).

nez(vacqueyras_2014,
  [ [ 'nez aromatique fruits surmuris guimauve reglisse', '.' ]
  ]).

bouche(vacqueyras_2014,
  [ [ 'saveurs intenses juteuses denses et veloutees', '.' ]
  ]).

description(vacqueyras_2014,
  [ [ 'sols caillouteux donnant des vins chaleureux', '.' ],
    [ 'cru plaisant et elegant', '.' ]
  ]).

nez(saint_joseph_2014,
  [ [ 'syrah nord fruits pamplemousse epices poivre et bois brule', '.' ]
  ]).

bouche(saint_joseph_2014,
  [ [ 'vin aromatique race genereux concentre et puissant', '.' ],
    [ 'belle harmonie gras fraicheur matiere', '.' ]
  ]).

description(saint_joseph_2014,
  [ [ 'equilibre remarquable pour ce saint joseph', '.' ]
  ]).

nez(gigondas_2014,
  [ [ 'nez riche et concentre avec fond remarquable', '.' ]
  ]).

bouche(gigondas_2014,
  [ [ 'bouche racee fruits noirs epices finale distinguee', '.' ]
  ]).

description(gigondas_2014,
  [ [ 'vignoble escarpe des dentelles de montmirail', '.' ],
    [ 'terroir caillouteux donnant un vin elegant', '.' ]
  ]).

nez(chateauneuf_du_pape_rouge_2013,
  [ [ 'nez complexe suave fruits rouges cannelle et epices', '.' ]
  ]).

bouche(chateauneuf_du_pape_rouge_2013,
  [ [ 'saveurs intenses racees riches et complexes', '.' ]
  ]).

description(chateauneuf_du_pape_rouge_2013,
  [ [ 'terrasses de galets roules produisant des vins charnus', '.' ],
    [ 'chateauneuf complet et harmonieux', '.' ]
  ]).

nez(hermitage_rouge_2007,
  [ [ 'fruits noirs agrumes vanille tabac et reglisse', '.' ]
  ]).

bouche(hermitage_rouge_2007,
  [ [ 'bouche harmonieuse profonde sans durete', '.' ]
  ]).

description(hermitage_rouge_2007,
  [ [ 'grand vin race et complexe qui s epanouit', '.' ]
  ]).

nez(bourgogne_pinot_noir_les_marnes_2014,
  [ [ 'parfum de griottes et fruits des bois avec notes epicees', '.' ]
  ]).

bouche(bourgogne_pinot_noir_les_marnes_2014,
  [ [ 'bourgogne fin et concentre genereux frais tendre et gourmand', '.' ]
  ]).

description(bourgogne_pinot_noir_les_marnes_2014,
  [ [ 'cuvee de la cote de nuits pleine de plenitude', '.' ],
    [ 'vin polyvalent', '.' ]
  ]).

nez(hautes_cotes_de_nuits_2014,
  [ [ 'nez expressif fruits rouges groseilles griottes nuances florales', '.' ]
  ]).

bouche(hautes_cotes_de_nuits_2014,
  [ [ 'grain joli texture souple et bien enrobee', '.' ]
  ]).

description(hautes_cotes_de_nuits_2014,
  [ [ 'plateau en altitude apportant finesse et personnalite', '.' ]
  ]).

nez(savigny_les_beaune_2014,
  [ [ 'nez intense charmeur de fruits murs et pain d epice', '.' ]
  ]).

bouche(savigny_les_beaune_2014,
  [ [ 'bouche velours avec volume rondeur et touche moka', '.' ]
  ]).

description(savigny_les_beaune_2014,
  [ [ 'savigny delicieux a boire entre 2017 et 2022', '.' ]
  ]).

nez(savigny_les_beaune_premier_cru_2014,
  [ [ 'nez gourmand fruits rouges encens et grain de cafe', '.' ]
  ]).

bouche(savigny_les_beaune_premier_cru_2014,
  [ [ 'attaque velours beau volume longueur et superbe gras', '.' ]
  ]).

description(savigny_les_beaune_premier_cru_2014,
  [ [ 'climat aux serpentieres dense et plein', '.' ],
    [ 'toute la classe d un premier cru', '.' ]
  ]).

nez(aloxe_corton_2014,
  [ [ 'intensite aromatique grotte sous bois grain de cafe et epices', '.' ]
  ]).

bouche(aloxe_corton_2014,
  [ [ 'saveurs corsees denses et concentrees', '.' ]
  ]).

description(aloxe_corton_2014,
  [ [ 'splendide aloxe laissant excellent souvenir', '.' ]
  ]).

nez(chambolle_musigny_premier_cru_2012,
  [ [ 'fruits rouges epices poivre nuances empyreumatiques et sous bois', '.' ]
  ]).

bouche(chambolle_musigny_premier_cru_2012,
  [ [ 'saveurs denses concentrees enrobees opulentes et puissantes', '.' ]
  ]).

description(chambolle_musigny_premier_cru_2012,
  [ [ 'parcelle les noirots proche du grand cru bonnes mares', '.' ],
    [ 'grand vin race et long', '.' ]
  ]).

nez(chiroubles_2013,
  [ [ 'fruits rouges groseilles fraises et epices', '.' ]
  ]).

bouche(chiroubles_2013,
  [ [ 'sensation fruitee avec nuance florale et minerale', '.' ]
  ]).

description(chiroubles_2013,
  [ [ 'terroir granitique et schisteux du beaujolais', '.' ],
    [ 'vin tendre et expressif', '.' ]
  ]).

nez(fleurie_2015,
  [ [ 'nez elegant melant fruit epices et nuances florales', '.' ]
  ]).

bouche(fleurie_2015,
  [ [ 'texture velours genereuse et harmonieuse', '.' ]
  ]).

description(fleurie_2015,
  [ [ 'reine des crus du beaujolais grand millesime coup de coeur', '.' ]
  ]).

nez(moulin_a_vent_2014,
  [ [ 'fruits sauvages grotte sous bois et nuance de cire', '.' ]
  ]).

bouche(moulin_a_vent_2014,
  [ [ 'structure enrobee finale riche et harmonieuse', '.' ]
  ]).

description(moulin_a_vent_2014,
  [ [ 'terroir donnant des gamay vineux tres concentres', '.' ]
  ]).

nez(chinon_vieilles_vignes_2014,
  [ [ 'cabernet franc mur reglisse noire vanille fruits rouges et sous bois', '.' ]
  ]).

bouche(chinon_vieilles_vignes_2014,
  [ [ 'saveurs intenses structure volume race et concentration', '.' ]
  ]).

description(chinon_vieilles_vignes_2014,
  [ [ 'magnifique chinon puissant et typique', '.' ],
    [ 'vin gastronomique', '.' ]
  ]).

nez(sancerre_rouge_2015,
  [ [ 'nez pinot cerise fruits des bois sous bois champignons', '.' ]
  ]).

bouche(sancerre_rouge_2015,
  [ [ 'notes de fruits et epices poivrees avec fraicheur et elegance', '.' ]
  ]).

description(sancerre_rouge_2015,
  [ [ 'vin rond ample souple et soyeux finale harmonieuse', '.' ]
  ]).

nez(les_guignards_2015,
  [ [ 'cassis et fruits du verger avec nuances d agrumes et de rose', '.' ]
  ]).

bouche(les_guignards_2015,
  [ [ 'vin expressif bien equilibre alliant fraicheur et tendresse', '.' ]
  ]).

description(les_guignards_2015,
  [ [ 'cotes de duras alliant sauvignon et muscadelle', '.' ],
    [ 'vin polyvalent delicieux', '.' ]
  ]).

nez(chardonnay_exception_2016,
  [ [ 'aromes intenses de fruits du verger avec touche boisee', '.' ]
  ]).

bouche(chardonnay_exception_2016,
  [ [ 'bouche epicee de caractere avec volume et finale ronde', '.' ]
  ]).

description(chardonnay_exception_2016,
  [ [ 'vin puissant laissant un excellent souvenir', '.' ],
    [ 'note boisee tout au long de la degustation', '.' ]
  ]).

nez(cotes_du_rhone_blanc_pierre_charlotte_2016,
  [ [ 'abricot et peche blanche avec nuances de fruits secs et epices', '.' ]
  ]).

bouche(cotes_du_rhone_blanc_pierre_charlotte_2016,
  [ [ 'bouche riche ronde fruitee avec touche boisee bien integree', '.' ]
  ]).

description(cotes_du_rhone_blanc_pierre_charlotte_2016,
  [ [ 'nord du vaucluse cuvee rac e souple et equilibree', '.' ],
    [ 'maitre achat frais et gourmand', '.' ]
  ]).

nez(ch_le_druc_2015,
  [ [ 'nez expressif d abricot agrumes et nuances florales grillees', '.' ]
  ]).

bouche(ch_le_druc_2015,
  [ [ 'attaque juteuse alliant gras et fraicheur', '.' ]
  ]).

description(ch_le_druc_2015,
  [ [ 'graves blanc au boise empyreumatique', '.' ],
    [ 'excellent rapport qualite prix', '.' ]
  ]).

nez(laudun_2016,
  [ [ 'nez intense fruits du verger epices et note citronnee', '.' ]
  ]).

bouche(laudun_2016,
  [ [ 'saveurs denses racees complexes finale persistante', '.' ]
  ]).

description(laudun_2016,
  [ [ 'cotes du rhone villages rive opposee de chateauneuf', '.' ],
    [ 'magnifique decouverte alluree', '.' ]
  ]).

nez(vouvray_blanc_sec_2016,
  [ [ 'aromes de chenin tilleul et nuance de rose', '.' ]
  ]).

bouche(vouvray_blanc_sec_2016,
  [ [ 'attaque genereuse puis expression minerale racee', '.' ]
  ]).

description(vouvray_blanc_sec_2016,
  [ [ 'vin de caractere tres elegant', '.' ]
  ]).

nez(macon_villages_2015,
  [ [ 'fruits pomme et coing avec notes de beurre et miel', '.' ]
  ]).

bouche(macon_villages_2015,
  [ [ 'bouche equilibree alliant harmonie ampleur et elegance', '.' ]
  ]).

description(macon_villages_2015,
  [ [ 'chardonnay de macon bourgogne blanc plaisant', '.' ]
  ]).

nez(pinot_gris_2015,
  [ [ 'nez parfume de fruits frais a chair blanche et exotiques', '.' ]
  ]).

bouche(pinot_gris_2015,
  [ [ 'bouche fruitee riche et elegante finale juteuse', '.' ]
  ]).

description(pinot_gris_2015,
  [ [ 'pinot gris plein de charme', '.' ]
  ]).

nez(gewurztraminer_2015,
  [ [ 'fruits a chair blanche et exotiques avec fleurs et epices', '.' ]
  ]).

bouche(gewurztraminer_2015,
  [ [ 'bouche genereuse equilibree et harmonieuse', '.' ]
  ]).

description(gewurztraminer_2015,
  [ [ 'sud alsace au pied du bolenberg', '.' ],
    [ 'finale distinguee', '.' ]
  ]).

nez(vire_clesse_2016,
  [ [ 'aromes intenses de pomme coing peche et miel', '.' ]
  ]).

bouche(vire_clesse_2016,
  [ [ 'saveurs fruitees minerales florales riches et expressives', '.' ]
  ]).

description(vire_clesse_2016,
  [ [ 'chardonnay maconnais alliant texture ronde et fraicheur', '.' ],
    [ 'vin race complexe et concentre', '.' ]
  ]).

nez(sancerre_blanc_verdigny_2015,
  [ [ 'nez race minéral avec fruits murs cassis eglantier et buis', '.' ]
  ]).

bouche(sancerre_blanc_verdigny_2015,
  [ [ 'expression marie mineralite et fruit avec belle fraicheur', '.' ]
  ]).

description(sancerre_blanc_verdigny_2015,
  [ [ 'grand classique du sauvignon sur terroir calcaire de verdigny', '.' ]
  ]).

nez(vacqueyras_blanc_2016,
  [ [ 'intensite aromatique abricot peche blanche raisin frais et poire', ',' ],
    [ 'nuances epicees pain grille et thym', '.' ]
  ]).

bouche(vacqueyras_blanc_2016,
  [ [ 'bouche pleine volume densite et longueur', '.' ]
  ]).

description(vacqueyras_blanc_2016,
  [ [ 'vin equilibre avec beaucoup de gras et peu d acidite', '.' ],
    [ 'rarete delicieuse de vacqueyras blanc', '.' ]
  ]).

nez(hautes_cotes_de_beaune_2015,
  [ [ 'nez parfume fleurs blanches fruits du verger et noisettes', '.' ]
  ]).

bouche(hautes_cotes_de_beaune_2015,
  [ [ 'bouche intense genereuse riche et beurre avec boise integre', '.' ]
  ]).

description(hautes_cotes_de_beaune_2015,
  [ [ 'bourgogne gastronomique sur les hauteurs de la cote de beaune', '.' ],
    [ 'excellent rapport plaisir prix', '.' ]
  ]).

nez(pouilly_fuisse_2014,
  [ [ 'nez mineral elegant notes de coing pomme noisette grillee et beurre', '.' ]
  ]).

bouche(pouilly_fuisse_2014,
  [ [ 'bouche minerale assez puissante avec fruit gras et touche boisee', '.' ]
  ]).

description(pouilly_fuisse_2014,
  [ [ 'grand classique pour crustaces et poissons en sauce', '.' ]
  ]).

nez(chablis_premier_cru_montmains_2014,
  [ [ 'nez intense de fruits du verger avec nuances iodees et noisettees', '.' ]
  ]).

bouche(chablis_premier_cru_montmains_2014,
  [ [ 'saveurs complexes racees minerales salees et iodees', '.' ]
  ]).

description(chablis_premier_cru_montmains_2014,
  [ [ 'chablis premier cru tres race grande longueur', '.' ]
  ]).

nez(condrieu_2015,
  [ [ 'intensite aromatique riche abricot peche et epices', '.' ]
  ]).

bouche(condrieu_2015,
  [ [ 'bouche alliant volume equilibre race mineralite et onctuosite', '.' ]
  ]).

description(condrieu_2015,
  [ [ 'tres grand vin des cotes du rhone issu d une petite propriete', '.' ],
    [ 'exposition parfaite pour 1.8 hectares', '.' ]
  ]).

nez(cremant_de_loire_brut,
  [ [ 'nez associant rondeur du chardonnay et caractere floral du chenin', '.' ]
  ]).

bouche(cremant_de_loire_brut,
  [ [ 'finesse et elegance avec beaucoup de rondeur', '.' ]
  ]).

description(cremant_de_loire_brut,
  [ [ 'methode traditionnelle vendanges manuelles', '.' ],
    [ 'cremant de loire tres digeste', '.' ]
  ]).

nez(champagne_extra_brut,
  [ [ 'nez de finesse et d elegance a l etat pur', '.' ]
  ]).

bouche(champagne_extra_brut,
  [ [ 'champagne tres digeste non dose pinot noir majoritaire', '.' ]
  ]).

description(champagne_extra_brut,
  [ [ 'champagne d aperitif superbe de finesse', '.' ]
  ]).

nez(champagne_brut_oeil_de_perdrix,
  [ [ 'notes de fruits rouges cerise et framboise avec sous bois', '.' ]
  ]).

bouche(champagne_brut_oeil_de_perdrix,
  [ [ 'style race elegant croquant et gourmand', '.' ]
  ]).

description(champagne_brut_oeil_de_perdrix,
  [ [ 'blanc de noirs a teinte ambree issu de raisins surmatures', '.' ],
    [ 'rarete a grande intensite aromatique', '.' ]
  ]).

nez(champagne_brut_rose_de_saignee,
  [ [ 'notes de fruits rouges cerise petite framboise et sous bois', '.' ]
  ]).

bouche(champagne_brut_rose_de_saignee,
  [ [ 'champagne croquant craquant gourmand et delicat', '.' ]
  ]).

description(champagne_brut_rose_de_saignee,
  [ [ 'rose de saignee rare au style race et elegant', '.' ]
  ]).

nez(champagne_brut_or_blanc,
  [ [ 'chardonnay mur sur notes de fleur d acacia citron et peche blanche', '.' ]
  ]).

bouche(champagne_brut_or_blanc,
  [ [ 'equilibre fraicheur et onctuosite avec effervescence fougueuse', '.' ]
  ]).

description(champagne_brut_or_blanc,
  [ [ 'blanc de blancs de b cocteaux a montgenost', '.' ],
    [ 'champagne pour toutes les occasions', '.' ]
  ]).

nez(champagne_brut_prestige,
  [ [ 'nez complexe puissant evolue fruits secs et miel', '.' ]
  ]).

bouche(champagne_brut_prestige,
  [ [ 'finesse et puissance pour accompagner aperitif ou repas', '.' ]
  ]).

description(champagne_brut_prestige,
  [ [ 'cuvee grand cru bouzy remarquable', '.' ]
  ]).

nez(cognac_trois_etoiles,
  [ [ 'belle intensite de fruit avec epices et ecorce d orange', '.' ]
  ]).

bouche(cognac_trois_etoiles,
  [ [ 'style soyeux au tres beau fruit', '.' ]
  ]).

description(cognac_trois_etoiles,
  [ [ 'assemblage de petite champagne fin bois et bon bois', '.' ]
  ]).

nez(cognac_fine_champagne_vsop,
  [ [ 'nez puissant avec beaucoup de finesse', '.' ]
  ]).

bouche(cognac_fine_champagne_vsop,
  [ [ 'bouche elegante avec personalite et allure', '.' ]
  ]).

description(cognac_fine_champagne_vsop,
  [ [ 'assemblage grande et petite champagne mention fine champagne', '.' ]
  ]).

nez(cognac_grande_champagne_hors_age,
  [ [ 'nez intense complexe et tres fin', '.' ]
  ]).

bouche(cognac_grande_champagne_hors_age,
  [ [ 'bouche ample fruits confits pain d epices grande persistance', '.' ]
  ]).

description(cognac_grande_champagne_hors_age,
  [ [ 'hors age issu de la meilleure aire grande champagne', '.' ],
    [ 'inclut une eau de vie des annees cinquante', '.' ]
  ]).

