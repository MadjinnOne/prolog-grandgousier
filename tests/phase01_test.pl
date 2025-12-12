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

:- end_tests(phase01).
