% ==============================================================================
% SECTION: Chargement et declarations
% Rôle: Charger les dependances, la base de connaissance, et les directives moteur.
% Entrées: -
% Sorties: -
% Notes: base_vins fournit notamment nom/2, prix/2, appellation/2, provenance/2, description/2, bouche/2, nez/2.
% ==============================================================================
:- use_module(library(lists)).
:- use_module(library(pairs)).
:- use_module(base_vins).      % <--- on charge la base de connaissances
:- discontiguous regle_rep/4.
:- dynamic dernier_vin/1.
% ==============================================================================
% SECTION: Production de reponses
% Rôle: Dispatcher la question normalisee vers une regle et construire la reponse.
% Entrées: Liste de mots (tokens) issue de la lecture utilisateur.
% Sorties: Liste de lignes (listes de tokens) repondant au bot.
% Notes: regle_rep/4 definissent les patrons; match_pattern/2 teste sur tokens normalises.
% ==============================================================================
/* --------------------------------------------------------------------- */
/*                                                                       */
/*        PRODUIRE_REPONSE(L_Mots,L_Lignes_reponse) :                    */
/*                                                                       */
/*        Input : une liste de mots L_Mots representant la question      */
/*                de l'utilisateur                                       */
/*        Output : une liste de liste de lignes correspondant a la       */
/*                 reponse fournie par le bot                            */
/*                                                                       */
/*        NB Pour l'instant le predicat retourne dans tous les cas       */
/*            [  [je, ne, sais, pas, '.'],                               */
/*               [les, etudiants, vont, m, '\'', aider, '.'],            */
/*               ['vous le verrez !']                                    */
/*            ]                                                          */
/*                                                                       */
/*        Je ne doute pas que ce sera le cas ! Et vous souhaite autant   */
/*        d'amusement a coder le predicat que j'ai eu a ecrire           */
/*        cet enonce et ce squelette de solution !                       */
/*                                                                       */
/* --------------------------------------------------------------------- */


/*                      !!!    A MODIFIER   !!!                          */

% Predicat principal: selectionne une regle et produit la liste de lignes.
produire_reponse([fin],[L1]) :-
   L1 = [merci, de, m, '\'', avoir, consulte], !.

produire_reponse(L,Rep) :-
   normaliser_question(L,L_norm),
%   write(L_norm),
   mclef(M,_), member(M,L_norm),
   clause(regle_rep(M,_,Pattern,Rep),Body),
   match_pattern(Pattern,L),
   call(Body), !.

produire_reponse(_,[L1,L2, L3]) :-
   L1 = [je, ne, sais, pas, '.'],
   L2 = [les, etudiants, vont, m, '\'', aider, '.' ],
   L3 = ['vous le verrez !'].

% ==============================================================================
% SECTION: Normalisation et matching
% Rôle: Comparer une question a des patrons via une normalisation des tokens.
% Entrées: Liste de tokens bruts (mots/punctuation) d'une question utilisateur.
% Sorties: Tokens normalises ou succes/echec de correspondance.
% Notes: normaliser_question/2 applique synonymes, plats, appellations et expansions.
% ==============================================================================
match_pattern(Pattern,Lmots) :-
   normaliser_question(Lmots,Lmots_norm),
   sublist(Pattern,Lmots_norm).

sublist(SL,L) :-
   prefix(SL,L), !.
sublist(SL,[_|T]) :- sublist(SL,T).

% Pipeline de normalisation: unifie les noms de vins/appellations/plats puis abaisse les variantes.
normaliser_question(Lmots,L_out) :-
   nom_vins_uniforme(Lmots,Ltmp),
   normaliser_appellations_tokens(Ltmp,Lapp),
   normaliser_plats_tokens(Lapp,Lplats),
   expand_compound_tokens(Lplats,Lsplit),
   maplist(normaliser_mot,Lsplit,L_out).

% ==============================================================================
% SECTION: Normalisation lexicale
% Rôle: Etendre tokens composes et harmoniser variantes (vins, plats, appellations).
% Entrées: Listes de tokens (atoms) issues des questions utilisateur.
% Sorties: Listes de tokens homogenes pour le matching.
% Notes: Les remplacements privilegient les patrons les plus longs via un tri par longueur.
% ==============================================================================
expand_compound_tokens([],[]).
expand_compound_tokens([estce|Rest],[est,ce|Out]) :-
   expand_compound_tokens(Rest,Out).
expand_compound_tokens([estceque|Rest],[est,ce,que|Out]) :-
   expand_compound_tokens(Rest,Out).
expand_compound_tokens([peuton|Rest],[peut,on|Out]) :-
   expand_compound_tokens(Rest,Out).
expand_compound_tokens([puisje|Rest],[puis,je|Out]) :-
   expand_compound_tokens(Rest,Out).
expand_compound_tokens([Token|Rest],[Token|Out]) :-
   expand_compound_tokens(Rest,Out).

normaliser_mot(lappellation,appellation) :- !.
normaliser_mot(Mot,Mot).

nom_vins_uniforme(Lmots,L_mots_unif) :-
   normalisation_variants(Variants),
   remplace_variants(Variants,Lmots,L_mots_unif).

normaliser_appellations_tokens(Lin,Lout) :-
   findall(Key-app(App,Pattern),
      ( appellation_synonymes(App,Syns),
        member(Pattern,Syns),
        length(Pattern,Len),
        Key is -Len
      ),
      Raw),
   keysort(Raw,Sorted),
   pairs_values(Sorted,Variants),
   remplace_appellations(Variants,Lin,Lout).

remplace_appellations([],L,L).
remplace_appellations([app(App,Pattern)|Rest],Lin,Lout) :-
   replace_vin(Pattern,App,Lin,Linter),
   remplace_appellations(Rest,Linter,Lout).

normaliser_plats_tokens(Lin,Lout) :-
   findall(Key-plat(Plat,Pattern),
      ( plat_synonyme(Plat,Pattern),
        length(Pattern,Len),
        Key is -Len
      ),
      Raw),
   keysort(Raw,Sorted),
   pairs_values(Sorted,Variants),
   remplace_plats(Variants,Lin,Lout).

remplace_plats([],L,L).
remplace_plats([plat(Plat,Pattern)|Rest],Lin,Lout) :-
   replace_vin(Pattern,Plat,Lin,Linter),
   remplace_plats(Rest,Linter,Lout).

% ==============================================================================
% SECTION: Lexiques et synonymes
% Rôle: Declarer les patrons connus pour plats, appellations et vins.
% Entrées: Tokens normalises issus du preprocess.
% Sorties: Variantes standardisees (atoms) pour plat/appellation/vin.
% Notes: Les synonymes servent aux remplacements effectues plus haut.
% ==============================================================================
plat_synonyme(canard,[canard]).
plat_synonyme(boeuf,[boeuf]).
plat_synonyme(poisson,[poisson]).
plat_synonyme(boulets_liegeois,[boulets,liegeois]).
plat_synonyme(boulets_liegeois,[boulet,liegeois]).
plat_synonyme(boulets_liegeois,[boulets]).
plat_synonyme(carbonnade,[carbonnade]).
plat_synonyme(carbonnade,[carbonnades]).
plat_synonyme(carbonnade,[carbonnade,flamande]).
plat_synonyme(stoemp_saucisse,[stoemp,saucisse]).
plat_synonyme(stoemp_saucisse,[stoemp]).
plat_synonyme(stoemp_saucisse,[stoemp,et,saucisse]).
plat_synonyme(lapin_a_la_biere,[lapin,a,la,biere]).
plat_synonyme(lapin_a_la_biere,[lapin,biere]).
plat_synonyme(potee_liegeoise,[potee,liegeoise]).
plat_synonyme(potee_liegeoise,[potee]).
plat_synonyme(chicons_au_gratin,[chicons,au,gratin]).
plat_synonyme(chicons_au_gratin,[chicon,gratin]).
plat_synonyme(boudin_noir_aux_pommes,[boudin,noir,aux,pommes]).
plat_synonyme(boudin_noir_aux_pommes,[boudin,noir]).
plat_synonyme(filet_americain,[filet,americain]).
plat_synonyme(filet_americain,[filet]).
plat_synonyme(vol_au_vent,[vol,au,vent]).
plat_synonyme(vol_au_vent,[volauvent]).
plat_synonyme(tarte_al_djote,[tarte,al,djote]).
plat_synonyme(tarte_al_djote,[tarte,djote]).
plat_synonyme(waterzooi,[waterzooi]).
% Synonymes d'appellations definis en base ou derives des identifiants.
appellation_synonymes(App,Syns) :-
   ( var(App)
   -> setof(A, source_appellation(A), Apps), member(App,Apps)
   ;  source_appellation(App)
   ),
   findall(Pattern, base_appellation_synonyme(App,Pattern), Raw),
   sort(Raw,Syns),
   Syns \= [].

source_appellation(App) :-
   appellation_synonymes_fact(App,_).
source_appellation(App) :-
   appellation(_,App).

base_appellation_synonyme(App,Pattern) :-
   appellation_synonymes_fact(App,Syns),
   member(Pattern,Syns).
base_appellation_synonyme(App,Pattern) :-
   default_appellation_pattern(App,Pattern).

default_appellation_pattern(App,Pattern) :-
   atom(App),
   atomic_list_concat(Pattern,'_',App),
   Pattern \= [].

appellation_tokens(App,Tokens) :-
   default_appellation_pattern(App,Tokens).

humanize_appellation(App,Human) :-
   appellation_tokens(App,Tokens),
   atomic_list_concat(Tokens,' ',Human).

normalisation_variants(Variants) :-
   findall(Key-variant(Vin,Pattern),
      ( vin_synonyme(Vin,Pattern),
        length(Pattern,Len),
        Key is -Len
      ),
      Raw),
   keysort(Raw,Sorted),
   pairs_values(Sorted,Variants).

remplace_variants([],L,L).
remplace_variants([variant(Vin,Pattern)|Reste],Lin,Lout) :-
   replace_vin(Pattern,Vin,Lin,Linter),
   remplace_variants(Reste,Linter,Lout).

% Synonymes explicites de vins; complete ensuite par un pattern par defaut.
vin_synonyme(beaumes_de_venise_2015, [beaumes,de,venise,2015]).
vin_synonyme(beaumes_de_venise_2015, [beaumes,de,venise]).
vin_synonyme(beaumes_de_venise_2015, [beaumesdevenise,2015]).
vin_synonyme(beaumes_de_venise_2015, [beaumesdevenise]).

vin_synonyme(les_chaboeufs_2013, [les,chaboeufs,2013]).
vin_synonyme(les_chaboeufs_2013, [les,chaboeufs]).
vin_synonyme(les_chaboeufs_2013, [nuits,saint,georges,premier,cru,2013]).
vin_synonyme(les_chaboeufs_2013, [nuits,saint,georges,premier,cru]).
vin_synonyme(les_chaboeufs_2013, [nuits,saint,georges,'1er',cru,2013]).
vin_synonyme(les_chaboeufs_2013, [nuits,saint,georges,'1er',cru]).
vin_synonyme(les_chaboeufs_2013, [nuits,saint,georges,2013]).
vin_synonyme(les_chaboeufs_2013, [nuits,saint,georges]).
vin_synonyme(les_chaboeufs_2013, [nuitssaintgeorges,2013]).
vin_synonyme(les_chaboeufs_2013, [nuitssaintgeorges]).

vin_synonyme(chambolle_musigny_premier_cru_2012, [chambolle,musigny,premier,cru,2012]).
vin_synonyme(chambolle_musigny_premier_cru_2012, [chambolle,musigny,premier,cru]).
vin_synonyme(chambolle_musigny_premier_cru_2012, [chambolle,musigny,'1er',cru,2012]).
vin_synonyme(chambolle_musigny_premier_cru_2012, [chambolle,musigny,'1er',cru]).
vin_synonyme(chambolle_musigny_premier_cru_2012, [chambolle,musigny,2012]).
vin_synonyme(chambolle_musigny_premier_cru_2012, [chambolle,musigny]).
vin_synonyme(chambolle_musigny_premier_cru_2012, [chambollemusigny,premier,cru,2012]).
vin_synonyme(chambolle_musigny_premier_cru_2012, [chambollemusigny,premier,cru]).
vin_synonyme(chambolle_musigny_premier_cru_2012, [chambollemusigny,'1er',cru,2012]).
vin_synonyme(chambolle_musigny_premier_cru_2012, [chambollemusigny,'1er',cru]).
vin_synonyme(chambolle_musigny_premier_cru_2012, [chambollemusigny,2012]).
vin_synonyme(chambolle_musigny_premier_cru_2012, [chambollemusigny]).

vin_synonyme(la_fleur_de_pomys_2012, [la,fleur,de,pomys,2012]).
vin_synonyme(la_fleur_de_pomys_2012, [la,fleur,de,pomys]).
vin_synonyme(la_fleur_de_pomys_2012, [lafleurdepomys,2012]).
vin_synonyme(la_fleur_de_pomys_2012, [lafleurdepomys]).

vin_synonyme(Vin,Pattern) :-
   default_vin_pattern(Vin,Pattern).

replace_vin(L,X,In,Out) :-
   append(L,Suf,In), !, Out = [X|Suf].
replace_vin(_,_,[],[]) :- !.
replace_vin(L,X,[H|In],[H|Out]) :-
   replace_vin(L,X,In,Out).

% ==============================================================================
% SECTION: Mots-cles et mapping plats
% Rôle: Ponderer les mots clefs pour le dispatch et normaliser les plats.
% Entrées: Tokens normalises.
% Sorties: Poids (mclef/2) et identifiants de plat (plat_kw/2).
% Notes: mclef/2 est utilise par produire_reponse/2 via clause/2.
% ==============================================================================
% ----------------------------------------------------------------%


% Mots clefs et scores utilises pour choisir la regle la plus pertinente.
mclef(bouche,10).
mclef(nez,10).
mclef(vin,5).
mclef(vins,5).
mclef(pourriezvous, 10).
mclef(pourriez,10).
mclef(que,9).
mclef(dautres,7).
mclef(autres,7).
mclef(auriez,6).
mclef(moins,7).
mclef(plus,7).
mclef(boire,8).
mclef(boeuf,8).
mclef(poisson,8).
mclef(canard,8).
mclef(boulets_liegeois,7).
mclef(carbonnade,7).
mclef(stoemp_saucisse,7).
mclef(lapin_a_la_biere,7).
mclef(potee_liegeoise,7).
mclef(chicons_au_gratin,7).
mclef(boudin_noir_aux_pommes,7).
mclef(filet_americain,7).
mclef(vol_au_vent,7).
mclef(tarte_al_djote,6).
mclef(waterzooi,7).
mclef(parlez,2).
mclef(parle,2).
mclef(parlezvous,2).
mclef(appellation,8).

% Alias de plats pour matcher plusieurs patrons vers un meme identifiant.
plat_kw(canard,canard).
plat_kw(boeuf,boeuf).
plat_kw(poisson,poisson).
plat_kw(boulets_liegeois,boulets_liegeois).
plat_kw(carbonnade,carbonnade).
plat_kw(stoemp_saucisse,stoemp_saucisse).
plat_kw(lapin_a_la_biere,lapin_a_la_biere).
plat_kw(potee_liegeoise,potee_liegeoise).
plat_kw(chicons_au_gratin,chicons_au_gratin).
plat_kw(boudin_noir_aux_pommes,boudin_noir_aux_pommes).
plat_kw(filet_americain,filet_americain).
plat_kw(vol_au_vent,vol_au_vent).
plat_kw(tarte_al_djote,tarte_al_djote).
plat_kw(waterzooi,waterzooi).


% ==============================================================================
% SECTION: Regles degustation (bouche/nez)
% Rôle: Repondre aux questions sur le nez et la bouche d'un vin.
% Entrées: Patterns contenant un identifiant de vin.
% Sorties: Reponse textuelle (listes de tokens) issue de la base.
% Notes: memoriser_vin/1 stocke le dernier vin cite pour des questions suivantes.
% ==============================================================================
% ----------------------------------------------------------------%

% Convention: regle_rep(MotCle,Id,Pattern,Rep) ou Pattern est une liste de tokens.
regle_rep(bouche,1,
  [ que, donne, le, Vin, en, bouche ],
  Rep ) :-

     bouche_reponse(Vin,Rep).

regle_rep(bouche,2,
  [ que, donne, Vin, en, bouche ],
  Rep ) :-
     bouche_reponse(Vin,Rep).

regle_rep(bouche,3,
  [ comment, est, Vin, en, bouche ],
  Rep ) :-
     bouche_reponse(Vin,Rep).

regle_rep(bouche,4,
  [ que, donne, Vin, en, bouche, '?' ],
  Rep) :-
     bouche_reponse(Vin,Rep).

regle_rep(bouche,5,
  [ bouche, de, Vin ],
  Rep) :-
     bouche_reponse(Vin,Rep).

% ----------------------------------------------------------------%

regle_rep(nez,1,
  [ quel, nez, presente, le, Vin ],
  Rep) :-
    nez_reponse(Vin, Rep).

regle_rep(nez,2,
  [ quel, nez, presente, Vin ],
  Rep) :-
    nez_reponse(Vin, Rep).

regle_rep(nez,3,
  [ quel, nez, pour, Vin ],
  Rep) :-
    nez_reponse(Vin, Rep).

regle_rep(nez,4,
  [ quel, nez, pour, Vin, '?' ],
  Rep) :-
    nez_reponse(Vin, Rep).

regle_rep(nez,5,
  [ nez, de, Vin ],
  Rep) :-
    nez_reponse(Vin, Rep).

bouche_reponse(Vin,Rep) :-
    bouche(Vin,Rep),
    memoriser_vin(Vin).

nez_reponse(Vin,Rep) :-
    nez(Vin,Rep),
    memoriser_vin(Vin).

% ==============================================================================
% SECTION: Regles description et appellation
% Rôle: Fournir une description d'un vin ou d'une appellation cible.
% Entrées: Identifiant de vin ou nom d'appellation (atom).
% Sorties: Paragraphes de description issus de la base de connaissances.
% Notes: Peut basculer d'une appellation vers un vin representatif.
% ==============================================================================
% ----------------------------------------------------------------%

regle_rep(pourriezvous, 1,
  [ pourriezvous, men, dire, plus, sur, le, Vin ],
  Rep ) :-
    description_ou_appellation(Vin, Rep).

regle_rep(pourriezvous, 2,
  [ pourriezvous, men, dire, plus, sur, Vin ],
  Rep ) :-
    description_ou_appellation(Vin, Rep).

regle_rep(pourriez,1,
  [ pourriez, men, dire, plus, sur, le, Vin ],
  Rep) :-
    description_ou_appellation(Vin,Rep).

regle_rep(pourriez,2,
  [ pourriez, men, dire, plus, sur, Vin ],
  Rep) :-
    description_ou_appellation(Vin,Rep).

regle_rep(que,1,
  [ que, pouvezvous, me, dire, sur, le, Vin ],
  Rep) :-
    description_ou_appellation(Vin,Rep).

regle_rep(parlez,1,
  [ parlez, moi, du, Vin ],
  Rep) :-
    description_ou_appellation(Vin,Rep).

regle_rep(parlez,2,
  [ parlez, moi, de, Vin ],
  Rep) :-
    description_ou_appellation(Vin,Rep).

regle_rep(parle,1,
  [ parle, moi, du, Vin ],
  Rep) :-
    description_ou_appellation(Vin,Rep).

regle_rep(parlezvous,1,
  [ parlezvous, du, Vin ],
  Rep) :-
    description_ou_appellation(Vin,Rep).

description_ou_appellation(Vin,Rep) :-
    description(Vin,Rep),
    memoriser_vin(Vin), !.
description_ou_appellation(App,Rep) :-
    atom(App),
    collect_vins_appellation(App,[Vin|_]),
    description(Vin,Rep),
    memoriser_vin(Vin), !.

% ==============================================================================
% SECTION: Conseils par appellation
% Rôle: Recommander des vins pour une appellation donnee.
% Entrées: Appellation (atom) et type de selection (principales/supplementaires).
% Sorties: Lignes de recommandation formatees.
% Notes: Peut utiliser des recommandations predefinies ou generer a partir de la base.
% ==============================================================================
% ----- Conseils Bourgogne -----

conseil_appellation_fact(bourgogne, principales,
  [ voici, trois, vins, de, bourgogne, que, je, vous, conseille, ':' ],
  [
    rec(coteaux_bourguignons_2014, 'vin gouleyant et harmonieux'),
    rec(bourgogne_pinot_noir_les_marnes_2014, 'pinot noir charmeur, grande souplesse'),
    rec(hautes_cotes_de_nuits_2014, 'fruit croquant, parfait pour la table')
  ]).

conseil_appellation_fact(bourgogne, supplementaires,
  [ j, '\'', ai, aussi, d, autres, vins, de, bourgogne, a, vous, proposer, ':' ],
  [
    rec(les_chaboeufs_2013, 'nuits saint georges 1er cru, puissant et race'),
    rec(chambolle_musigny_premier_cru_2012, 'grand pinot soyeux et complexe')
  ]).

conseil_appellation(App,Type,Intro,Lrec) :-
   conseil_appellation_fact(App,Type,Intro,Lrec), !.
conseil_appellation(App,Type,Intro,Lrec) :-
   default_conseil_appellation(App,Type,Intro,Lrec).

regle_rep(vins,12,
  [ quels, vins, de, App, me, conseillezvous ],
  Rep) :-
     reponse_conseil(App,principales,Rep).

regle_rep(vins,13,
  [ quels, vins, de, App, me, conseillez, vous ],
  Rep) :-
     reponse_conseil(App,principales,Rep).

regle_rep(vins,14,
  [ auriezvous, des, vins, de, App ],
  Rep) :-
     reponse_conseil(App,principales,Rep).

regle_rep(vins,15,
  [ avezvous, des, vins, de, App ],
  Rep) :-
     reponse_conseil(App,principales,Rep).

regle_rep(vin,1,
  [ quel, vin, de, App, me, conseillezvous ],
  Rep) :-
     reponse_conseil(App,principales,Rep).

regle_rep(vin,2,
  [ quel, vin, de, App, me, conseillez, vous ],
  Rep) :-
     reponse_conseil(App,principales,Rep).

regle_rep(dautres,1,
  [ auriezvous, dautres, vins, de, App ],
  Rep) :-
     reponse_conseil(App,supplementaires,Rep).

regle_rep(autres,1,
  [ auriezvous, d, autres, vins, de, App ],
  Rep) :-
     reponse_conseil(App,supplementaires,Rep).

regle_rep(autres,2,
  [ auriez, vous, d, autres, vins, de, App ],
  Rep) :-
     reponse_conseil(App,supplementaires,Rep).

regle_rep(autres,3,
  [ auriezvous, autres, vins, de, App ],
  Rep) :-
     reponse_conseil(App,supplementaires,Rep).

regle_rep(autres,4,
  [ auriez, vous, autres, vins, de, App ],
  Rep) :-
     reponse_conseil(App,supplementaires,Rep).

regle_rep(auriez,1,
  [ vous, auriez, un, App ],
  Rep) :-
     reponse_conseil(App,principales,Rep).

regle_rep(auriez,2,
  [ vous, auriez, des, App ],
  Rep) :-
     reponse_conseil(App,principales,Rep).

regle_rep(auriez,3,
  [ auriez, vous, un, App ],
  Rep) :-
     reponse_conseil(App,principales,Rep).

regle_rep(auriez,4,
  [ auriez, vous, des, App ],
  Rep) :-
     reponse_conseil(App,principales,Rep).

% ==============================================================================
% SECTION: Maturite de degustation
% Rôle: Repondre aux questions sur l'accessibilite d'un vin (pret a boire ou non).
% Entrées: Identifiant de vin ou variable si le vin n'est pas explicite.
% Sorties: Reponse textuelle fondee sur les observations disponibles.
% Notes: Utilise dernier_vin/1 quand le vin est omis par l'utilisateur.
% ==============================================================================
% ----- Maturite de degustation -----

regle_rep(boire,1,
  [ est, ce, un, vin, que, je, peux, boire, tout, de, suite, Vin ],
  Rep) :-
    reponse_accessibilite(Vin,Rep).

regle_rep(boire,2,
  [ est, ce, un, vin, que, je, peux, boire, tout, de, suite, Det, Vin ],
  Rep) :-
    det_vin(Det),
    reponse_accessibilite(Vin,Rep).

regle_rep(boire,3,
  [ est, ce, que, je, peux, boire, Vin, tout, de, suite ],
  Rep) :-
    reponse_accessibilite(Vin,Rep).

regle_rep(boire,4,
  [ est, ce, que, je, peux, boire, Det, Vin, tout, de, suite ],
  Rep) :-
    det_vin(Det),
    reponse_accessibilite(Vin,Rep).

regle_rep(boire,5,
  [ est, ce, que, je, peux, boire, Vin, maintenant ],
  Rep) :-
    reponse_accessibilite(Vin,Rep).

regle_rep(boire,6,
  [ est, ce, que, je, peux, boire, Det, Vin, maintenant ],
  Rep) :-
    det_vin(Det),
    reponse_accessibilite(Vin,Rep).

regle_rep(boire,7,
  [ puis, je, boire, Vin, maintenant ],
  Rep) :-
    reponse_accessibilite(Vin,Rep).

regle_rep(boire,8,
  [ puis, je, boire, Det, Vin, maintenant ],
  Rep) :-
    det_vin(Det),
    reponse_accessibilite(Vin,Rep).

regle_rep(boire,9,
  [ puis, je, deja, boire, Vin ],
  Rep) :-
    reponse_accessibilite(Vin,Rep).

regle_rep(boire,10,
  [ puis, je, deja, boire, Det, Vin ],
  Rep) :-
    det_vin(Det),
    reponse_accessibilite(Vin,Rep).

regle_rep(boire,11,
  [ je, peux, deja, boire, Vin ],
  Rep) :-
    reponse_accessibilite(Vin,Rep).

regle_rep(boire,12,
  [ je, peux, deja, boire, Det, Vin ],
  Rep) :-
    det_vin(Det),
    reponse_accessibilite(Vin,Rep).

regle_rep(boire,13,
  [ peut, on, boire, Vin, tout, de, suite ],
  Rep) :-
    reponse_accessibilite(Vin,Rep).

regle_rep(boire,14,
  [ peut, on, boire, Det, Vin, tout, de, suite ],
  Rep) :-
    det_vin(Det),
    reponse_accessibilite(Vin,Rep).

regle_rep(boire,15,
  [ peut, on, boire, Vin, maintenant ],
  Rep) :-
    reponse_accessibilite(Vin,Rep).

regle_rep(boire,16,
  [ peut, on, boire, Det, Vin, maintenant ],
  Rep) :-
    det_vin(Det),
    reponse_accessibilite(Vin,Rep).

regle_rep(boire,17,
  [ Vin, est, il, pret, a, boire ],
  Rep) :-
    reponse_accessibilite(Vin,Rep).

regle_rep(boire,18,
  [ Det, Vin, est, il, pret, a, boire ],
  Rep) :-
    det_vin(Det),
    reponse_accessibilite(Vin,Rep).

regle_rep(boire,19,
  [ je, peux, boire, Vin, tout, de, suite ],
  Rep) :-
    reponse_accessibilite(Vin,Rep).

regle_rep(boire,20,
  [ je, peux, boire, Det, Vin, tout, de, suite ],
  Rep) :-
    det_vin(Det),
    reponse_accessibilite(Vin,Rep).

regle_rep(boire,21,
  [ je, peux, boire, Vin, maintenant ],
  Rep) :-
    reponse_accessibilite(Vin,Rep).

regle_rep(boire,22,
  [ je, peux, boire, Det, Vin, maintenant ],
  Rep) :-
    det_vin(Det),
    reponse_accessibilite(Vin,Rep).

regle_rep(boire,23,
  [ est, ce, un, vin, que, je, peux, boire, tout, de, suite ],
  Rep) :-
    reponse_accessibilite(_,Rep).

regle_rep(boire,24,
  [ est, ce, un, vin, que, je, peux, boire, maintenant ],
  Rep) :-
    reponse_accessibilite(_,Rep).

regle_rep(boire,25,
  [ est, ce, que, je, peux, le, boire, tout, de, suite ],
  Rep) :-
    reponse_accessibilite(_,Rep).

regle_rep(boire,26,
  [ est, ce, que, je, peux, le, boire, maintenant ],
  Rep) :-
    reponse_accessibilite(_,Rep).

reponse_accessibilite(Vin,Rep) :-
    var(Vin),
    dernier_vin(Dernier),
    nonvar(Dernier),
    reponse_accessibilite(Dernier,Rep), !.
reponse_accessibilite(Vin,[[pour, vous, repondre, j, ai, besoin, de, connaitre, le, vin, dont, vous, parlez, '.']]) :-
    var(Vin), !.
reponse_accessibilite(Vin,Rep) :-
    (  description(Vin,_)
    ;  bouche(Vin,_)
    ),
    memoriser_vin(Vin),
    collect_observations(Vin,Observations),
    decide_accessibilite(Observations,Decision),
    build_accessibilite_response(Decision,Observations,Rep), !.
reponse_accessibilite(_,[[je, ne, connais, pas, ce, vin, '.']]).

det_vin(le).
det_vin(la).
det_vin(l).
det_vin(ce).
det_vin(cet).
det_vin(cette).
det_vin(ces).

memoriser_vin(Vin) :-
    atom(Vin),
    retractall(dernier_vin(_)),
    asserta(dernier_vin(Vin)).
memoriser_vin(_).

% ==============================================================================
% SECTION: Heuristique d'accessibilite
% Rôle: Extraire des indices textuels et decider si le vin est pret a boire.
% Entrées: Identifiant de vin et textes associes (description/bouche).
% Sorties: Decision ready/wait/mix/unknown et lignes d'explication.
% Notes: NOTE: le scoring repose sur des mots-cles par sous-chaine, sensible aux variations.
% ==============================================================================
collect_observations(Vin,Observations) :-
    findall(obs(Side,Aspect,Source,Text),
       ( keyword_profile(Side,Aspect,Keyword),
         texte_vin(Vin,Source,Text),
         downcase_atom(Text,Lower),
         sub_atom(Lower,_,_,_,Keyword)
       ),
       Raw),
    sort(Raw,Observations).

decide_accessibilite([],unknown).
decide_accessibilite(Observations,Decision) :-
    score_side(Observations,ready,Ready),
    score_side(Observations,wait,Wait),
    (  Ready >= Wait + 1
    -> Decision = ready
    ;  Wait >= Ready + 1
    -> Decision = wait
    ;  Decision = mix
    ).

score_side(Observations,Side,Score) :-
    findall(1, member(obs(Side,_,_,_),Observations), On),
    length(On,Score).

build_accessibilite_response(unknown,_,[[je, n, '\'', ai, pas, suffisamment, d, indications, pour, dire, si, ce, vin, est, pret, a, boire, '.']]).
build_accessibilite_response(Decision,Observations,Rep) :-
    Decision \= unknown,
    decision_intro(Decision,Intro),
    detail_lines(Decision,Observations,Details),
    (  Details == []
    -> Rep = [Intro]
    ;  Rep = [Intro|Details]
    ).

decision_intro(ready,
  [ oui, ',', ce, vin, peut, etre, apprecie, des, maintenant, ',', car, il, montre, deja, une, belle, souplesse, '.']).
decision_intro(wait,
  [ il, est, preferable, d, attendre, encore, un, peu, ',', car, il, reste, tres, structure, '.']).
decision_intro(mix,
  [ oui, ',', mais, il, gagnera, encore, en, complexite, ',', car, il, garde, une, charpente, serieuse, '.']).

detail_lines(Decision,Observations,Lines) :-
    observations_lines(Observations,ready,ReadyLines),
    observations_lines(Observations,wait,WaitLines),
    (  Decision == ready
    -> Lines = ReadyLines
    ;  Decision == wait
    -> Lines = WaitLines
    ;  Decision == mix
    -> append(ReadyLines,WaitLines,Lines)
    ;  Lines = []
    ).

observations_lines(Observations,Side,Lines) :-
    observations_by_side(Observations,Side,SideObs),
    max_observations_per_side(Limit),
    take_n(SideObs,Limit,Selected,_),
    maplist(observation_line,Selected,Lines).

observations_by_side([],_,[]).
observations_by_side([obs(S,Aspect,Source,Text)|Rest],Side,[obs(S,Aspect,Source,Text)|Filtered]) :-
    S == Side,
    observations_by_side(Rest,Side,Filtered).
observations_by_side([obs(S,_,_,_)|Rest],Side,Filtered) :-
    S \= Side,
    observations_by_side(Rest,Side,Filtered).

observation_line(obs(Side,Aspect,Source,Text),Line) :-
    observation_phrase(Side,Aspect,Source,Text,Line).

observation_phrase(ready,structure,Source,Text,
  [ la, Source, mentionne, Text, ',', ce, qui, met, en, avant, une, structure, deja, souple, '.']).
observation_phrase(ready,tannins,_,Text,
  [ on, y, lit, Text, ',', ce, qui, montre, que, les, tannins, sont, deja, integres, '.']).
observation_phrase(ready,style,_,Text,
  [ ce, passage, Text, souligne, un, style, accessible, '.']).
observation_phrase(wait,structure,Source,Text,
  [ la, Source, insiste, sur, Text, ',', ce, qui, souligne, une, charpente, puissante, '.']).
observation_phrase(wait,tannins,_,Text,
  [ on, y, lit, Text, ',', ce, qui, laisse, penser, que, les, tannins, doivent, encore, se, fondre, '.']).
observation_phrase(wait,style,_,Text,
  [ ce, passage, Text, evoque, un, profil, de, garde, '.']).

max_observations_per_side(2).

texte_vin(Vin,description,Text) :-
    description(Vin,Paragraphes),
    member(Ligne,Paragraphes),
    member(Text,Ligne),
    Text \= '.'.
texte_vin(Vin,bouche,Text) :-
    bouche(Vin,Paragraphes),
    member(Ligne,Paragraphes),
    member(Text,Ligne),
    Text \= '.'.

keyword_profile(ready,structure,'soupl').
keyword_profile(ready,structure,'rond').
keyword_profile(ready,structure,'gouley').
keyword_profile(ready,structure,'velout').
keyword_profile(ready,structure,'velour').
keyword_profile(ready,structure,'soyeu').
keyword_profile(ready,structure,'harmon').
keyword_profile(ready,structure,'charme').
keyword_profile(ready,structure,'tendr').
keyword_profile(ready,structure,'plaisan').
keyword_profile(ready,structure,'gourmand').
keyword_profile(ready,structure,'elegant').
keyword_profile(ready,style,'charme').
keyword_profile(ready,style,'plaisan').
keyword_profile(ready,style,'gouley').
keyword_profile(ready,style,'gourmand').
keyword_profile(ready,style,'vin de present').
keyword_profile(ready,style,'a boire').
keyword_profile(ready,tannins,'tannins fin').
keyword_profile(ready,tannins,'tannins soyeu').
keyword_profile(ready,tannins,'tannins eleg').
keyword_profile(ready,tannins,'tannins mur').
keyword_profile(ready,tannins,'tannins enrobe').
keyword_profile(ready,tannins,'enrobe').
keyword_profile(ready,tannins,'enrobee').

keyword_profile(wait,structure,'puiss').
keyword_profile(wait,structure,'dens').
keyword_profile(wait,structure,'concentr').
keyword_profile(wait,structure,'charpent').
keyword_profile(wait,structure,'corse').
keyword_profile(wait,structure,'etoff').
keyword_profile(wait,style,'d avenir').
keyword_profile(wait,tannins,'tannins puiss').
keyword_profile(wait,tannins,'tanins riches').
keyword_profile(wait,tannins,'fondre les tannins').

reponse_conseil(App,Type,Rep) :-
   conseil_appellation(App,Type,Intro,Lrec),
   lignes_recommandations(Lrec,Intro,Rep).

lignes_recommandations([],Intro,[Intro]).
lignes_recommandations(Lrec,Intro,[Intro|Lines]) :-
    maplist(format_ligne_recommandation, Lrec, Lines).

format_ligne_recommandation(rec(Vin,Commentaire), Line) :-
    nom(Vin,Nom),
    prix(Vin,Prix),
    Line = [ '- ', Nom, ' : ', Commentaire, ' (', Prix, ' EUR )' ].

selection_limit(principales,3).
selection_limit(supplementaires,3).

default_conseil_appellation(App, principales, Intro, Lrec) :-
   collect_vins_appellation(App,Vins),
   ( Vins == []
   -> phrase_appellation([je, n, '\'', ai, aucun, vin, pour], App, ['.'], Intro),
      Lrec = []
   ;  selection_limit(principales,Limit),
      take_n(Vins,Limit,Selection,_),
      intro_principales(App,Intro),
      maplist(rec_appellation(App,principales),Selection,Lrec)
   ).

default_conseil_appellation(App, supplementaires, Intro, Lrec) :-
   collect_vins_appellation(App,Vins),
   selection_limit(principales,FirstBatch),
   drop_n(Vins,FirstBatch,Remaining),
   ( Remaining == []
   -> phrase_appellation([je, n, '\'', ai, plus, d, autres, vins, pour], App, ['.'], Intro),
      Lrec = []
   ;  selection_limit(supplementaires,Limit),
      take_n(Remaining,Limit,Selection,_),
      intro_supplementaires(App,Intro),
      maplist(rec_appellation(App,supplementaires),Selection,Lrec)
   ).

collect_vins_appellation(App,Vins) :-
   findall(Prix-Vin, (appellation(Vin,App), prix(Vin,Prix)), Exact),
   findall(Prix-Vin, (provenance(Vin,App), prix(Vin,Prix)), RegionPairs),
   include(region_extra(Exact), RegionPairs, RegionExtra),
   ( Exact \= []
   -> Raw = Exact
   ;  Raw = []
   ),
   append(Raw,RegionExtra,Combined),
   ( Combined \= []
   -> RawFinal = Combined
   ;  RawFinal = RegionPairs
   ),
   keysort(RawFinal,Pairs),
   pairs_values(Pairs,Vins).

region_extra(Exact,Pair) :-
   Pair = _-Vin,
   \+ memberchk(_-Vin,Exact).

rec_appellation(App,Type,Vin,rec(Vin,Commentaire)) :-
   commentaire_appellation(App,Type,Commentaire).

commentaire_appellation(App,principales,Commentaire) :-
   humanize_appellation(App,Human),
   format(atom(Commentaire), 'profil classique de ~w', [Human]).
commentaire_appellation(App,supplementaires,Commentaire) :-
   humanize_appellation(App,Human),
   format(atom(Commentaire), 'autre selection de ~w', [Human]).

intro_principales(App,Intro) :-
   phrase_appellation(
      [voici, quelques, vins, de],
      App,
      [que, je, peux, vous, proposer, ':'],
      Intro).

intro_supplementaires(App,Intro) :-
   phrase_appellation(
      [j, '\'', ai, aussi, d, autres, vins, de],
      App,
      [a, vous, proposer, ':'],
      Intro).

phrase_appellation(Prefix,App,Suffix,Ligne) :-
   appellation_tokens(App,Tokens),
   append(Prefix,Tokens,Temp),
   append(Temp,Suffix,Ligne).

take_n(List,N,Take,Rest) :-
   (  N =< 0
   -> Take = [],
      Rest = List
   ;  List = [H|T]
   -> Take = [H|R],
      N1 is N-1,
      take_n(T,N1,R,Rest)
   ;  Take = [],
      Rest = []
   ).

drop_n(List,N,Rest) :-
   (  N =< 0
   -> Rest = List
   ;  List = [_|T]
   -> N1 is N-1,
      drop_n(T,N1,Rest)
   ;  Rest = []
   ).

% ==============================================================================
% SECTION: Accords mets-vins
% Rôle: Proposer des appellations adapteees a un plat donne.
% Entrées: Plat (atom) et groupes d'appellations par region.
% Sorties: Lignes de reponse preformatees.
% Notes: Les profils sont declares manuellement via profil_plat/3.
% ==============================================================================
% ----- Accords mets-vins -----

profil_plat(canard,
  [ [ 'Pour le canard, je vous conseille des vins rouges puissants aux notes epicees et fumees.' ],
    [ 'Voici des appellations qui fonctionnent tres bien :' ] ],
  [ bordeaux-[graves, saint_emilion, pomerol],
    bourgogne-[marsannay, fixin, nuits_saint_georges, gevrey_chambertin],
    rhone_nord-[cote_rotie, saint_joseph, hermitage]
  ]).

profil_plat(boeuf,
  [ [ 'Pour le boeuf, misez sur des rouges charpentes qui accompagnent bien la viande.' ],
    [ 'Ces appellations offrent de beaux accords :' ] ],
  [ bordeaux-[pauillac, saint_julien, saint_estephe],
    rhone_nord-[hermitage, saint_joseph],
    sud_ouest-[madiran]
  ]).

profil_plat(poisson,
  [ [ 'Pour le poisson, privilegiez des blancs tendus et aromatiques.' ],
    [ 'Voici des pistes fiables :' ] ],
  [ loire-[sancerre, vouvray],
    bourgogne-[chablis_premier_cru_montmains, macon_villages],
    rhone_sud-[cotes_du_rhone]
  ]).

profil_plat(boulets_liegeois,
  [ [ 'Pour des boulets liegeois, il faut des rouges chaleureux mais digestes.' ],
    [ 'Ces appellations s accordent tres bien :' ] ],
  [ bordeaux-[cotes_de_bordeaux_blaye, bordeaux_superieur],
    val_de_loire-[chinon],
    beaujolais-[fleurie, chiroubles]
  ]).

profil_plat(carbonnade,
  [ [ 'La carbonnade demande des rouges amples capables de repondre aux saveurs sucre-sale.' ],
    [ 'Je vous recommande :' ] ],
  [ bourgogne-[bourgogne_pinot_noir, hautes_cotes_de_beaune],
    sud_ouest-[madiran],
    rhone_sud-[cotes_du_rhone_villages]
  ]).

profil_plat(stoemp_saucisse,
  [ [ 'Le stoemp saucisse aime les rouges ronds et epices qui reveillent le plat.' ],
    [ 'Essayez ces appellations :' ] ],
  [ rhone_sud-[cotes_du_rhone, cotes_du_rhone_villages_laudun],
    beaujolais-[moulin_a_vent],
    val_de_loire-[chinon]
  ]).

profil_plat(lapin_a_la_biere,
  [ [ 'Pour le lapin a la biere, cherchez un rouge souple avec une touche maltée.' ],
    [ 'Ces regions fonctionnent tres bien :' ] ],
  [ bordeaux-[lalande_de_pomerol, saint_emilion],
    bourgogne-[bourgogne_pinot_noir],
    rhone_nord-[saint_joseph]
  ]).

profil_plat(potee_liegeoise,
  [ [ 'La potee liegeoise appelle des blancs francs et aromatiques qui rafraichissent le palais.' ],
    [ 'Quelques suggestions :' ] ],
  [ alsace-[pinot_gris, gewurztraminer],
    bourgogne-[macon_villages],
    loire-[vouvray]
  ]).

profil_plat(chicons_au_gratin,
  [ [ 'Les chicons au gratin aiment les blancs cremes mais tendus.' ],
    [ 'Je vous conseille :' ] ],
  [ bourgogne-[macon_villages, chablis_premier_cru_montmains],
    loire-[cremant_de_loire]
  ]).

profil_plat(boudin_noir_aux_pommes,
  [ [ 'Pour le boudin noir aux pommes, il faut un rouge aux notes epicees et compotees.' ],
    [ 'Ces appellations marchent bien :' ] ],
  [ rhone_sud-[cairanne, cotes_du_rhone],
    rhone_sud_haut-[gigondas],
    sud_ouest-[madiran]
  ]).

profil_plat(filet_americain,
  [ [ 'Le filet americain aime la fraicheur des bulles ou des blancs acidules.' ],
    [ 'Essayez :' ] ],
  [ effervescents-[champagne, cremant_de_loire],
    loire-[sancerre, vouvray]
  ]).

profil_plat(vol_au_vent,
  [ [ 'Pour un vol-au-vent, privilegiez des blancs amples mais vifs.' ],
    [ 'Quelques idees :' ] ],
  [ bourgogne-[pouilly_fuisse, vire_clesse],
    loire-[cremant_de_loire]
  ]).

profil_plat(tarte_al_djote,
  [ [ 'La tarte al djote appelle des vins moelleux ou des bulles delicatement sucrees.' ],
    [ 'Je suggere :' ] ],
  [ sud_rhone_doux-[beaumes_de_venise],
    effervescents-[champagne]
  ]).

profil_plat(waterzooi,
  [ [ 'Pour un waterzooi, choisissez des blancs souples et floraux.' ],
    [ 'Ces pistes fonctionnent bien :' ] ],
  [ loire-[vouvray, sancerre],
    rhone_sud-[cotes_du_rhone]
  ]).

regle_rep(Plat,1,
  [ je, cuisine, du, Plat, quel, vin, me, conseillezvous ],
  Rep) :-
    plat_kw(Plat,Id),
    reponse_plat(Id,Rep).

regle_rep(Plat,2,
  [ je, cuisine, des, Plat, quel, vin, me, conseillezvous ],
  Rep) :-
    plat_kw(Plat,Id),
    reponse_plat(Id,Rep).

regle_rep(Plat,3,
  [ Plat, quel, vin, me, conseillezvous ],
  Rep) :-
    plat_kw(Plat,Id),
    reponse_plat(Id,Rep).

regle_rep(Plat,4,
  [ je, fais, un, Plat, quel, vin, servir ],
  Rep) :-
    plat_kw(Plat,Id),
    reponse_plat(Id,Rep).

regle_rep(Plat,5,
  [ quel, vin, avec, du, Plat ],
  Rep) :-
    plat_kw(Plat,Id),
    reponse_plat(Id,Rep).

regle_rep(Plat,6,
  [ quel, vin, avec, des, Plat ],
  Rep) :-
    plat_kw(Plat,Id),
    reponse_plat(Id,Rep).

regle_rep(Plat,7,
  [ quel, vin, pour, Plat ],
  Rep) :-
    plat_kw(Plat,Id),
    reponse_plat(Id,Rep).

reponse_plat(Plat,Rep) :-
    profil_plat(Plat,Intro,Groupes),
    maplist(format_ligne_plat,Groupes,Lignes),
    append(Intro,Lignes,Rep).

format_ligne_plat(Region-Appellations,Line) :-
    humanize_label(Region,RegionLabel),
    maplist(humanize_label,Appellations,PrettyApps),
    atomic_list_concat(PrettyApps,', ',AppsTexte),
    Line = [ RegionLabel, ': ', AppsTexte ].

humanize_label(Atom,Label) :-
    (   humanize_appellation(Atom,Label)
    ->  true
    ;   atom(Atom)
    ->  atomic_list_concat(Parts,'_',Atom),
        atomic_list_concat(Parts,' ',Label)
    ;   Label = Atom
    ).

% ==============================================================================
% SECTION: Definitions d'appellations
% Rôle: Donner une definition textuelle et gerer les synonymes d'appellations.
% Entrées: Nom d'appellation tel que mentionne par l'utilisateur.
% Sorties: Identifiant interne et paragraphes de definition.
% Notes: normaliser_appellation/2 retire les apostrophes simples avant matching.
% ==============================================================================
% ----- Definition d'appellations -----

definition_appellation(haut_medoc,
  [ [ 'Le haut medoc couvre la partie sud du medoc traditionnel.' ],
    [ 'On y trouve des terroirs de graves, souvent proches de la Garonne.' ],
    [ 'L appellation regroupe plusieurs communes comme Macau, Parempuyre, ou encore Avensan.' ] ]).

appellation_synonymes_fact(haut_medoc,
  [ [ haut, medoc ],
    [ hautmedoc ],
    [ l, '\'', appellation, haut, medoc ],
    [ appellation, haut, medoc ] ]).

regle_rep(appellation,1,
  [ que, recouvre, appellation, App ],
  Rep) :-
    normaliser_appellation(App,Id),
    definition_appellation(Id,Rep).

regle_rep(appellation,2,
  [ que, recouvre, appellation, App, '?' ],
  Rep) :-
    normaliser_appellation(App,Id),
    definition_appellation(Id,Rep).

regle_rep(appellation,3,
  [ que, recouvre, l, '\'', appellation, App ],
  Rep) :-
    normaliser_appellation(App,Id),
    definition_appellation(Id,Rep).

regle_rep(appellation,4,
  [ que, recouvre, l, '\'', appellation, App, '?' ],
  Rep) :-
    normaliser_appellation(App,Id),
    definition_appellation(Id,Rep).

normaliser_appellation(AppAtom,Id) :-
    atom_codes(AppAtom,Codes),
    exclude(=(39),Codes,CleanCodes),
    atom_codes(CleanAtom,CleanCodes),
    (   definition_appellation(CleanAtom,_)
    ->  Id = CleanAtom
    ;   appellation_synonymes(Id,Syns),
        member(Syn,Syns),
        atomic_list_concat(Syn,'_',CleanAtom)
    ), !.

% ==============================================================================
% SECTION: Requetes par prix
% Rôle: Filtrer les vins par intervalle de prix et formatter la reponse.
% Entrées: Bornes Min/Max (nombre ou inf).
% Sorties: Lignes listant les vins et leur prix.
% Notes: Les vins sont tries par prix croissant via predsort/3.
% ==============================================================================
% ----------------------------------------------------------------%

regle_rep(vins,1,
  [ auriezvous, des, vins, entre, X, et, Y, eur ],
  Rep) :-
     repondre_vins_prix(X,Y,Rep).

regle_rep(vins,2,
  [ auriez, vous, des, vins, entre, X, et, Y, eur ],
  Rep) :-
     repondre_vins_prix(X,Y,Rep).

regle_rep(vins,3,
  [ avezvous, des, vins, entre, X, et, Y, euros ],
  Rep) :-
     repondre_vins_prix(X,Y,Rep).

regle_rep(vins,4,
  [ avez, vous, des, vins, entre, X, et, Y, euros ],
  Rep) :-
     repondre_vins_prix(X,Y,Rep).

regle_rep(vins,5,
  [ auriezvous, des, vins, entre, X, et, Y ],
  Rep) :-
     repondre_vins_prix(X,Y,Rep).

regle_rep(vins,6,
  [ auriez, vous, des, vins, entre, X, et, Y, euros ],
  Rep) :-
     repondre_vins_prix(X,Y,Rep).

regle_rep(vins,7,
  [ auriezvous, des, vins, entre, X, et, Y, euros ],
  Rep) :-
     repondre_vins_prix(X,Y,Rep).

regle_rep(vins,8,
  [ avezvous, des, vins, a, moins, de, Max, euros ],
  Rep) :-
     repondre_vins_prix(0,Max,Rep).

regle_rep(vins,9,
  [ avezvous, des, vins, a, moins, de, Max ],
  Rep) :-
     repondre_vins_prix(0,Max,Rep).

regle_rep(vins,10,
  [ avezvous, des, vins, a, plus, de, Min, euros ],
  Rep) :-
     repondre_vins_prix(Min,inf,Rep).

regle_rep(vins,11,
  [ avezvous, des, vins, a, plus, de, Min ],
  Rep) :-
     repondre_vins_prix(Min,inf,Rep).

regle_rep(moins,1,
  [ des, vins, a, moins, de, Max, euros ],
  Rep) :-
     repondre_vins_prix(0,Max,Rep).

regle_rep(moins,2,
  [ des, vins, a, moins, de, Max ],
  Rep) :-
     repondre_vins_prix(0,Max,Rep).

regle_rep(plus,1,
  [ des, vins, a, plus, de, Min, euros ],
  Rep) :-
     repondre_vins_prix(Min,inf,Rep).

regle_rep(plus,2,
  [ des, vins, a, plus, de, Min, eur ],
  Rep) :-
     repondre_vins_prix(Min,inf,Rep).

regle_rep(plus,3,
  [ des, vins, a, plus, de, Min ],
  Rep) :-
     repondre_vins_prix(Min,inf,Rep).

repondre_vins_prix(X,Y,Rep) :-
     lvins_prix_min_max(X,Y,Lvins),
     rep_lvins_min_max(Lvins,Rep).

rep_lvins_min_max([], [[ non, ',', je, n, '\'', ai, aucun, vin, dans, cette, gamme, '.']]).
rep_lvins_min_max([H|T], [ [ oui, ',', je, vous, propose, ces, vins, ':' ] | L]) :-
   rep_litems_vin_min_max([H|T],L).

rep_litems_vin_min_max([],[]) :- !.
rep_litems_vin_min_max([(V,P)|L], [Irep|Ll]) :-
   nom(V,Appellation),
   Irep = [ '- ', Appellation, ' : ', P, ' EUR' ],
   rep_litems_vin_min_max(L,Ll).

prix_vin_min_max(Vin,P,Min,Max) :-
   prix(Vin,P),
   Min =< P,
   (Max == inf -> true ; P =< Max).

lvins_prix_min_max(Min,Max,Lvins) :-
   findall( (Vin,P) , prix_vin_min_max(Vin,P,Min,Max), Tmp),
   predsort(compare_prix, Tmp, Lvins).

compare_prix(<, (_,P1), (_,P2)) :- P1 =< P2, !.
compare_prix(>,_,_).

% ==============================================================================
% SECTION: Lecture et tokenisation brute
% Rôle: Lire une question utilisateur et la convertir en liste d'atomes.
% Entrées: Flux standard (ligne utilisateur).
% Sorties: Liste d'atomes (mots/nombres) en minuscules, sans ponctuation.
% Notes: Le nettoyage et la normalisation des caracteres se font plus bas.
% ==============================================================================




/* --------------------------------------------------------------------- */
/*                                                                       */
/*          CONVERSION D'UNE QUESTION DE L'UTILISATEUR EN                */
/*                        LISTE DE MOTS                                  */
/*                                                                       */
/* --------------------------------------------------------------------- */

% lire_question(L_Mots)

lire_question(LMots) :- read_atomics(LMots).



/*****************************************************************************/
% my_char_type(+Char,?Type)
%    Char is an ASCII code.
%    Type is whitespace, punctuation, numeric, alphabetic, or special.

my_char_type(46,period) :- !.
my_char_type(X,alphanumeric) :- X >= 65, X =< 90, !.
my_char_type(X,alphanumeric) :- X >= 97, X =< 123, !.
my_char_type(X,alphanumeric) :- X >= 48, X =< 57, !.
my_char_type(X,whitespace) :- X =< 32, !.
my_char_type(X,punctuation) :- X >= 33, X =< 47, !.
my_char_type(X,punctuation) :- X >= 58, X =< 64, !.
my_char_type(X,punctuation) :- X >= 91, X =< 96, !.
my_char_type(X,punctuation) :- X >= 123, X =< 126, !.
my_char_type(_,special).


/*****************************************************************************/
% lower_case(+C,?L)
%   If ASCII code C is an upper-case letter, then L is the
%   corresponding lower-case letter. Otherwise L=C.

lower_case(X,Y) :-
	X >= 65,
	X =< 90,
	Y is X + 32, !.
lower_case(224,224) :- !.

lower_case(X,X).


/*****************************************************************************/
% read_lc_string(-String)
%  Reads a line of input into String as a list of ASCII codes,
%  with all capital letters changed to lower case.

read_lc_string(String) :-
	get0(FirstChar),
	lower_case(FirstChar,LChar),
	read_lc_string_aux(LChar,String).

read_lc_string_aux(10,[]) :- !.  % end of line

read_lc_string_aux(-1,[]) :- !.  % end of file

read_lc_string_aux(LChar,[LChar|Rest]) :- read_lc_string(Rest).


/*****************************************************************************/
% extract_word(+String,-Rest,-Word) (final version)
%  Extracts the first Word from String; Rest is rest of String.
%  A word is a series of contiguous letters, or a series
%  of contiguous digits, or a single special character.
%  Assumes String does not begin with whitespace.

extract_word([C|Chars],Rest,[C|RestOfWord]) :-
	my_char_type(C,Type),
	extract_word_aux(Type,Chars,Rest,RestOfWord).

extract_word_aux(special,Rest,Rest,[]) :- !.
   % if Char is special, don't read more chars.

extract_word_aux(Type,[C|Chars],Rest,[C|RestOfWord]) :-
	my_char_type(C,Type), !,
	extract_word_aux(Type,Chars,Rest,RestOfWord).

extract_word_aux(_,Rest,Rest,[]).   % if previous clause did not succeed.


/*****************************************************************************/
% remove_initial_blanks(+X,?Y)
%   Removes whitespace characters from the
%   beginning of string X, giving string Y.

remove_initial_blanks([C|Chars],Result) :-
	my_char_type(C,whitespace), !,
	remove_initial_blanks(Chars,Result).

remove_initial_blanks(X,X).   % if previous clause did not succeed.


/*****************************************************************************/
% digit_value(?D,?V)
%  Where D is the ASCII code of a digit,
%  V is the corresponding number.

digit_value(48,0).
digit_value(49,1).
digit_value(50,2).
digit_value(51,3).
digit_value(52,4).
digit_value(53,5).
digit_value(54,6).
digit_value(55,7).
digit_value(56,8).
digit_value(57,9).


/*****************************************************************************/
% string_to_number(+S,-N)
%  Converts string S to the number that it
%  represents, e.g., "234" to 234.
%  Fails if S does not represent a nonnegative integer.

string_to_number(S,N) :-
	string_to_number_aux(S,0,N).

string_to_number_aux([D|Digits],ValueSoFar,Result) :-
	digit_value(D,V),
	NewValueSoFar is 10*ValueSoFar + V,
	string_to_number_aux(Digits,NewValueSoFar,Result).

string_to_number_aux([],Result,Result).


/*****************************************************************************/
% string_to_atomic(+String,-Atomic)
%  Converts String into the atom or number of
%  which it is the written representation.

string_to_atomic([C|Chars],Number) :-
	string_to_number([C|Chars],Number), !.

string_to_atomic(String,Atom) :- name(Atom,String).
  % assuming previous clause failed.


/*****************************************************************************/
% extract_atomics(+String,-ListOfAtomics) (second version)
%  Breaks String up into ListOfAtomics
%  e.g., " abc def  123 " into [abc,def,123].

extract_atomics(String,ListOfAtomics) :-
	remove_initial_blanks(String,NewString),
	extract_atomics_aux(NewString,ListOfAtomics).

extract_atomics_aux([C|Chars],[A|Atomics]) :-
	extract_word([C|Chars],Rest,Word),
	string_to_atomic(Word,A),       % <- this is the only change
	extract_atomics(Rest,Atomics).

extract_atomics_aux([],[]).

% ==============================================================================
% SECTION: Nettoyage et normalisation des caracteres
% Rôle: Supprimer la ponctuation et normaliser quelques caracteres UTF-8.
% Entrées: Liste de codes ASCII/UTF-8 lue sur l'entree standard.
% Sorties: Liste de codes normalises, sans ponctuation.
% Notes: NOTE: la couverture UTF-8 est limitee a quelques sequences explicites.
% ==============================================================================

/*****************************************************************************/
% clean_string(+String,-Cleanstring)
%  removes all punctuation characters from String and return Cleanstring

clean_string([],[]) :- !.
clean_string([C|Chars],L) :-
	decode_char(C,Chars,Decoded,Rest),
	(  Decoded == skip
	-> clean_string(Rest,L)
	;  normalize_char(Decoded,N),
	   (  my_char_type(N,punctuation)
	   -> clean_string(Rest,L)
	   ;  L = [N|R],
	      clean_string(Rest,R)
	   )
	).

decode_char(195,[Next|Rest],Code,Rest) :-
	utf8_c3_pair(Next,Code), !.
decode_char(197,[Next|Rest],Code,Rest) :-
	utf8_c5_pair(Next,Code), !.
decode_char(194,[160|Rest],32,Rest) :- !. % non-breaking space
decode_char(226,[128,153|Rest],39,Rest) :- !. % right single quote
decode_char(160,Chars,32,Chars) :- !.
decode_char(8217,Chars,39,Chars) :- !.
decode_char(Code,Chars,Code,Chars).

utf8_c3_pair(Byte,Code) :-
	utf8_c3_map(Byte,Char),
	char_code(Char,Code).

utf8_c3_map(128,a).
utf8_c3_map(129,a).
utf8_c3_map(130,a).
utf8_c3_map(131,a).
utf8_c3_map(132,a).
utf8_c3_map(133,a).
utf8_c3_map(134,a).
utf8_c3_map(135,c).
utf8_c3_map(136,e).
utf8_c3_map(137,e).
utf8_c3_map(138,e).
utf8_c3_map(139,e).
utf8_c3_map(140,i).
utf8_c3_map(141,i).
utf8_c3_map(142,i).
utf8_c3_map(143,i).
utf8_c3_map(145,n).
utf8_c3_map(146,o).
utf8_c3_map(147,o).
utf8_c3_map(148,o).
utf8_c3_map(149,o).
utf8_c3_map(150,o).
utf8_c3_map(153,u).
utf8_c3_map(154,u).
utf8_c3_map(155,u).
utf8_c3_map(156,u).
utf8_c3_map(160,a).
utf8_c3_map(161,a).
utf8_c3_map(162,a).
utf8_c3_map(163,a).
utf8_c3_map(164,a).
utf8_c3_map(165,a).
utf8_c3_map(166,a).
utf8_c3_map(167,c).
utf8_c3_map(168,e).
utf8_c3_map(169,e).
utf8_c3_map(170,e).
utf8_c3_map(171,e).
utf8_c3_map(172,i).
utf8_c3_map(173,i).
utf8_c3_map(174,i).
utf8_c3_map(175,i).
utf8_c3_map(177,n).
utf8_c3_map(178,o).
utf8_c3_map(179,o).
utf8_c3_map(180,o).
utf8_c3_map(181,o).
utf8_c3_map(182,o).
utf8_c3_map(185,u).
utf8_c3_map(186,u).
utf8_c3_map(187,u).
utf8_c3_map(188,u).
utf8_c3_map(189,y).
utf8_c3_map(190,y).
utf8_c3_map(191,y).

utf8_c5_pair(Byte,Code) :-
	utf8_c5_map(Byte,Char),
	char_code(Char,Code).

utf8_c5_map(146,o).
utf8_c5_map(147,o).

normalize_char(Code,Normalized) :-
	accent_char(Code,Normalized), !.
normalize_char(Code,Code).

accent_char(224,97).  % à -> a
accent_char(225,97).  % á -> a
accent_char(226,97).  % â -> a
accent_char(227,97).  % ã -> a
accent_char(228,97).  % ä -> a
accent_char(192,65).  % À -> A
accent_char(193,65).  % Á -> A
accent_char(194,65).  % Â -> A
accent_char(195,65).  % Ã -> A
accent_char(196,65).  % Ä -> A
accent_char(231,99).  % ç -> c
accent_char(199,67).  % Ç -> C
accent_char(232,101). % è -> e
accent_char(233,101). % é -> e
accent_char(234,101). % ê -> e
accent_char(235,101). % ë -> e
accent_char(200,69).  % È -> E
accent_char(201,69).  % É -> E
accent_char(202,69).  % Ê -> E
accent_char(203,69).  % Ë -> E
accent_char(236,105). % ì -> i
accent_char(237,105). % í -> i
accent_char(238,105). % î -> i
accent_char(239,105). % ï -> i
accent_char(204,73).  % Ì -> I
accent_char(205,73).  % Í -> I
accent_char(206,73).  % Î -> I
accent_char(207,73).  % Ï -> I
accent_char(242,111). % ò -> o
accent_char(243,111). % ó -> o
accent_char(244,111). % ô -> o
accent_char(245,111). % õ -> o
accent_char(246,111). % ö -> o
accent_char(210,79).  % Ò -> O
accent_char(211,79).  % Ó -> O
accent_char(212,79).  % Ô -> O
accent_char(213,79).  % Õ -> O
accent_char(214,79).  % Ö -> O
accent_char(249,117). % ù -> u
accent_char(250,117). % ú -> u
accent_char(251,117). % û -> u
accent_char(252,117). % ü -> u
accent_char(217,85).  % Ù -> U
accent_char(218,85).  % Ú -> U
accent_char(219,85).  % Û -> U
accent_char(220,85).  % Ü -> U


/*****************************************************************************/
% read_atomics(-ListOfAtomics)
%  Reads a line of input, removes all punctuation characters, and converts
%  it into a list of atomic terms, e.g., [this,is,an,example].

read_atomics(ListOfAtomics) :-
	read_lc_string(String),
	clean_string(String,Cleanstring),
	extract_atomics(Cleanstring,ListOfAtomics).

% ==============================================================================
% SECTION: Patterns par defaut des vins
% Rôle: Deriver des tokens a partir des noms de vins quand aucun synonyme explicite n'existe.
% Entrées: Identifiant de vin et nom associe (nom/2).
% Sorties: Liste de tokens variantisee pour le matching.
% Notes: Les articles initiaux et les millesimes finaux peuvent etre supprimes.
% ==============================================================================
default_vin_pattern(Vin,Tokens) :-
    nom(Vin,Nom),
    atom_string(Nom,NomStr),
    normalize_label_tokens(NomStr,BaseTokens),
    tokens_variant(BaseTokens,Tokens),
    Tokens \= [].
default_vin_pattern(Vin,Tokens) :-
    nom(Vin,Nom),
    atom_string(Nom,NomStr),
    sub_string(NomStr,Before,_,_, "-"),
    sub_string(NomStr,0,Before,_,Prefix),
    normalize_label_tokens(Prefix,BaseTokens),
    tokens_variant(BaseTokens,Tokens),
    Tokens \= [].

normalize_label_tokens(Text,Tokens) :-
    string_lower(Text,Lower),
    string_codes(Lower,Codes),
    clean_string(Codes,Clean),
    extract_atomics(Clean,Tokens).

tokens_variant(Tokens,Variant) :-
    base_tokens_variant(Tokens,Base),
    remove_trailing_year(Base,Variant).

base_tokens_variant(Tokens,Tokens).
base_tokens_variant([First|Rest],Rest) :-
    article_token(First),
    Rest \= [].

remove_trailing_year(Tokens,Tokens).
remove_trailing_year(Tokens,Core) :-
    append(Core,[Last],Tokens),
    (   integer(Last)
    ;   atom(Last), atom_number(Last,_)
    ),
    Core \= [].

article_token(le).
article_token(la).
article_token(l).
article_token(les).



% ==============================================================================
% SECTION: Ecriture de reponse
% Rôle: Afficher une reponse en lignes avec gestion de la ponctuation.
% Entrées: Listes de mots (tokens) a ecrire.
% Sorties: Texte affiche sur la sortie standard.
% Notes: Respecte la capitalisation et les espaces autour de la ponctuation.
% ==============================================================================
/* --------------------------------------------------------------------- */
/*                                                                       */
/*        ECRIRE_REPONSE : ecrit une suite de lignes de texte            */
/*                                                                       */
/* --------------------------------------------------------------------- */

ecrire_reponse(L) :-
   nl, write('GGS :'),
   ecrire_li_reponse(L,1,1).

% ecrire_li_reponse(Ll,M,E)
% input : Ll, liste de listes de mots (tout en minuscules)
%         M, indique si le premier caractere du premier mot de
%            la premiere ligne doit etre mis en majuscule (1 si oui, 0 si non)
%         E, indique le nombre d'espaces avant ce premier mot

ecrire_li_reponse([],_,_) :-
    nl.

ecrire_li_reponse([Li|Lls],Mi,Ei) :-
   ecrire_ligne(Li,Mi,Ei,Mf),
   ecrire_li_reponse(Lls,Mf,2).

% ecrire_ligne(Li,Mi,Ei,Mf)
% input : Li, liste de mots a ecrire
%         Mi, Ei booleens tels que decrits ci-dessus
% output : Mf, booleen tel que decrit ci-dessus a appliquer
%          a la ligne suivante, si elle existe

ecrire_ligne([],M,_,M) :-
   nl.

ecrire_ligne([M|L],Mi,Ei,Mf) :-
   ecrire_mot(M,Mi,Maux,Ei,Eaux),
   ecrire_ligne(L,Maux,Eaux,Mf).

% ecrire_mot(M,B1,B2,E1,E2)
% input : M, le mot a ecrire
%         B1, indique s'il faut une majuscule (1 si oui, 0 si non)
%         E1, indique s'il faut un espace avant le mot (1 si oui, 0 si non)
% output : B2, indique si le mot suivant prend une majuscule
%          E2, indique si le mot suivant doit etre precede d'un espace

ecrire_mot('.',_,1,_,1) :-
   write('. '), !.
ecrire_mot('\'',X,X,_,0) :-
   write('\''), !.
ecrire_mot(',',X,X,E,1) :-
   espace(E), write(','), !.
ecrire_mot(M,0,0,E,1) :-
   espace(E), write(M).
ecrire_mot(M,1,0,E,1) :-
   name(M,[C|L]),
   upper_initial(C,D),
   name(N,[D|L]),
   espace(E), write(N).

upper_initial(C,D) :-
   C >= 97,
   C =< 122,
   !,
   D is C - 32.
upper_initial(C,C).

espace(0).
espace(N) :- N>0, Nn is N-1, write(' '), espace(Nn).


% ==============================================================================
% SECTION: Controle de session
% Rôle: Determiner si l'utilisateur souhaite terminer la session.
% Entrées: Liste de tokens de la derniere question.
% Sorties: Succes/echec du predicat fin/1.
% Notes: Utilise par la boucle principale pour arreter le dialogue.
% ==============================================================================
/* --------------------------------------------------------------------- */
/*                                                                       */
/*                            TEST DE FIN                                */
/*                                                                       */
/* --------------------------------------------------------------------- */

fin(L) :- member(fin,L).


% ==============================================================================
% SECTION: Boucle principale
% Rôle: Orchestrer la lecture, la reponse et l'affichage.
% Entrées: Flux standard (questions utilisateur).
% Sorties: Reponses ecrites sur la sortie standard.
% Notes: Le predicat grandgousier/0 est le point d'entree interactif.
% ==============================================================================
/* --------------------------------------------------------------------- */
/*                                                                       */
/*                         BOUCLE PRINCIPALE                             */
/*                                                                       */
/* --------------------------------------------------------------------- */

grandgousier :-

   nl, nl, nl,
   write('Bonjour, je suis Grandgousier, GGS pour les intimes,'), nl,
   write('conseiller en vin. En quoi puis-je vous etre utile ?'),
   nl, nl,

   repeat,
      write('Vous : '),
      lire_question(L_Mots),
      produire_reponse(L_Mots,L_ligne_reponse),
      ecrire_reponse(L_ligne_reponse),
   fin(L_Mots), !.


/* --------------------------------------------------------------------- */
/*                                                                       */
/*             ACTIVATION DU PROGRAMME APRES COMPILATION                 */
/*                                                                       */
/*  Le predicat grandgousier/0 n'est plus invoque automatiquement afin   */
/*  de faciliter les tests automatisees. Utiliser scripts/run.sh pour    */
/*  lancer l'assistant ou appeler grandgousier/0 manuellement.           */
/*                                                                       */
/* --------------------------------------------------------------------- */
