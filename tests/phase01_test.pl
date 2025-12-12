:- use_module(library(plunit)).
:- use_module('../base_vins').
:- ensure_loaded('../grandgousier.pl').

:- begin_tests(phase01).

test(default_response_is_placeholder) :-
    produire_reponse([bonjour], Rep),
    nth0(0, Rep, [je, ne, sais, pas, '.']).

test(bouche_question_returns_database_text) :-
    produire_reponse([que, donne, le, nuits, saint, georges, en, bouche], Rep),
    Rep == [ [ 'texture epaisse reunissant puissance richesse rondeur et longueur', '.' ] ].

test(price_range_contains_expected_vins) :-
    lvins_prix_min_max(20,35,Lvins),
    memberchk((la_fleur_de_pomys_2012,21.36), Lvins),
    memberchk((hermitage_rouge_2007,33.98), Lvins).

test(provenance_facts_present) :-
    provenance(les_chaboeufs_2013, bourgogne),
    provenance(hermitage_rouge_2007, rhone_nord).

test(appellation_facts_present) :-
    appellation(ch_menota_cuvee_montagrede_2014, graves),
    appellation(la_fleur_de_pomys_2012, saint_estephe).

test(all_wines_have_taxonomy) :-
    findall(V, nom(V,_), Vins),
    forall(member(V,Vins), (provenance(V,_), appellation(V,_))).

test(normalizes_nuits_saint_georges_no_year) :-
    nom_vins_uniforme([que, donne, le, nuits, saint, georges, en, bouche], R),
    R = [que, donne, le, les_chaboeufs_2013, en, bouche].

test(normalizes_nuits_saint_georges_compact) :-
    nom_vins_uniforme([que, donne, le, nuitssaintgeorges, en, bouche], R),
    R = [que, donne, le, les_chaboeufs_2013, en, bouche].

test(normalizes_chambolle_musigny_premier) :-
    nom_vins_uniforme([quel, nez, presente, le, chambollemusigny, '1er', cru, 2012], R),
    R = [quel, nez, presente, le, chambolle_musigny_premier_cru_2012].

test(normalizes_la_fleur_de_pomys) :-
    nom_vins_uniforme([pourriezvous, men, dire, plus, sur, la, fleur, de, pomys], R),
    R = [pourriezvous, men, dire, plus, sur, la_fleur_de_pomys_2012].

test(normalizes_lafleurdepomys_compact) :-
    nom_vins_uniforme([pourriezvous, men, dire, plus, sur, lafleurdepomys, 2012], R),
    R = [pourriezvous, men, dire, plus, sur, la_fleur_de_pomys_2012].

test(normalizes_beaumes_de_venise) :-
    nom_vins_uniforme([parlez, moi, du, beaumesdevenise, 2015], R),
    R = [parlez, moi, du, beaumes_de_venise_2015].

test(normalizes_saint_emilion_appellation) :-
    normaliser_question([quels, vins, de, saint, emilion, me, conseillezvous], R),
    nth0(3, R, saint_emilion).

test(description_parlez_moi_du) :-
    produire_reponse([parlez, moi, du, la, fleur, de, pomys], Rep),
    description(la_fleur_de_pomys_2012, Rep).

test(description_pourriezvous_variant) :-
    produire_reponse([pourriezvous, men, dire, plus, sur, la, fleur, de, pomys], Rep),
    description(la_fleur_de_pomys_2012, Rep).

test(nez_variant) :-
    produire_reponse([quel, nez, pour, nuits, saint, georges], Rep),
    nez(les_chaboeufs_2013, Rep).

test(bouche_variant) :-
    produire_reponse([comment, est, nuits, saint, georges, en, bouche], Rep),
    bouche(les_chaboeufs_2013, Rep).

test(bouche_short_pattern) :-
    produire_reponse([bouche, de, nuits, saint, georges], Rep),
    bouche(les_chaboeufs_2013, Rep).

test(nez_short_pattern) :-
    produire_reponse([nez, de, nuits, saint, georges], Rep),
    nez(les_chaboeufs_2013, Rep).

test(price_question_variant) :-
    produire_reponse([auriez, vous, des, vins, entre, 20, et, 35, euros], Rep),
    Rep = [[oui, ',', je, vous, propose, ces, vins, ':']|Liste],
    memberchk([ '- ', 'La Fleur de Pomys 2012 - Saint Estephe', ' : ', 21.36, ' EUR'], Liste),
    memberchk([ '- ', 'Hermitage rouge 2007', ' : ', 33.98, ' EUR'], Liste).

test(price_question_empty) :-
    produire_reponse([avez, vous, des, vins, entre, 1, et, 2, euros], [[non, ',', je, n, '\'', ai, aucun, vin, dans, cette, gamme, '.']]).

test(price_less_than) :-
    produire_reponse([avezvous, des, vins, a, moins, de, 10, euros], [Intro|Liste]),
    Intro = [oui, ',', je, vous, propose, ces, vins, ':'],
    memberchk([ '- ', 'Coteaux Bourguignons 2014', ' : ', 7.99, ' EUR' ], Liste).

test(price_greater_than) :-
    produire_reponse([avezvous, des, vins, a, plus, de, 60, euros], [Intro|Liste]),
    Intro = [oui, ',', je, vous, propose, ces, vins, ':'],
    memberchk([ '- ', 'Chambolle Musigny 1er Cru 2012 - Les Noirots', ' : ', 63.85, ' EUR' ], Liste).

test(bourgogne_initial_recommendations) :-
    produire_reponse([quels, vins, de, bourgogne, me, conseillezvous], [Intro|Liste]),
    Intro = [voici, trois, vins, de, bourgogne, que, je, vous, conseille, ':'],
    memberchk([ '- ', 'Coteaux Bourguignons 2014', ' : ', 'vin gouleyant et harmonieux', ' (', 7.99, ' EUR )' ], Liste),
    memberchk([ '- ', 'Hautes Cotes de Nuits 2014', ' : ', 'fruit croquant, parfait pour la table', ' (', 15.16, ' EUR )' ], Liste).

test(bourgogne_other_recommendations) :-
    produire_reponse([auriezvous, dautres, vins, de, bourgogne], [Intro|Liste]),
    Intro = [j, '\'', ai, aussi, d, autres, vins, de, bourgogne, a, vous, proposer, ':'],
    memberchk([ '- ', 'Nuits-Saint-Georges 1er Cru 2013, Les Chaboeufs', ' : ', 'nuits saint georges 1er cru, puissant et race', ' (', 42.35, ' EUR )' ], Liste),
    memberchk([ '- ', 'Chambolle Musigny 1er Cru 2012 - Les Noirots', ' : ', 'grand pinot soyeux et complexe', ' (', 63.85, ' EUR )' ], Liste).

test(bourgogne_other_after_initial) :-
    retractall(dernier_filtre(_,_)),
    retractall(vins_proposes(_,_)),
    produire_reponse([quels, vins, de, bourgogne, me, conseillezvous], _),
    produire_reponse([auriezvous, dautres, vins, de, bourgogne], [Intro|Liste]),
    Intro = [j, '\'', ai, aussi, d, autres, vins, de, bourgogne, a, vous, proposer, ':'],
    memberchk([ '- ', 'Nuits-Saint-Georges 1er Cru 2013, Les Chaboeufs', ' : ', 'nuits saint georges 1er cru, puissant et race', ' (', 42.35, ' EUR )' ], Liste).

test(graves_generic_recommendation) :-
    retractall(dernier_filtre(_,_)),
    retractall(vins_proposes(_,_)),
    produire_reponse([quels, vins, de, graves, me, conseillezvous], [Intro|Liste]),
    Intro = [voici, quelques, vins, de, graves, que, je, peux, vous, proposer, ':'],
    memberchk([ '- ', 'Ch. Menota Cuvee Montagrede 2014 - Graves', ' : ', 'profil classique de graves', ' (', 9.46, ' EUR )' ], Liste),
    memberchk([ '- ', 'Ch. Le Druc 2015 - Graves', ' : ', 'profil classique de graves', ' (', 7.62, ' EUR )' ], Liste).

test(appellation_no_more_supplementaires_message) :-
    produire_reponse([auriezvous, dautres, vins, de, saint, emilion], Rep),
    Rep = [[je, n, '\'', ai, plus, d, autres, vins, pour, saint, emilion, '.']].

test(canard_recommendation) :-
    produire_reponse([je, cuisine, du, canard, quel, vin, me, conseillezvous], [Intro1,Intro2|Groupes]),
    Intro1 = [ 'Pour le canard, je vous conseille des vins rouges puissants aux notes epicees et fumees.' ],
    Intro2 = [ 'Voici des appellations qui fonctionnent tres bien :' ],
    memberchk([bordeaux, ': ', 'graves, saint_emilion, pomerol'], Groupes),
    memberchk([rhone_nord, ': ', 'cote_rotie, saint_joseph, hermitage'], Groupes).

test(appellation_definition) :-
    produire_reponse([que, recouvre, lappellation, haut, medoc], Rep),
    definition_appellation(haut_medoc,Rep).

:- end_tests(phase01).
