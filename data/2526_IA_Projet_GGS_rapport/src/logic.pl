% ==========================================================
%  PROJET IA-SYMB (UNamur 2025)
%  U2 - Traversée du Pont
%
%  Auteur : Mathieu Crotteux
%  Description :
%     Modélisation et résolution du problème "U2 Crossing"
%     en logique déclarative (Prolog).
%
%     Quatre membres du groupe U2 doivent traverser un pont
%     de nuit en respectant des contraintes de vitesse et
%     de transport de lampe.
%
% ==========================================================


% ---------------------------
% 1️⃣ Temps de traversée
% ---------------------------
% Chaque membre du groupe a une vitesse de traversée
% différente, exprimée en minutes.
% Ces faits servent de base de connaissances pour calculer
% le coût temporel de chaque déplacement.

temps(bono, 1).   % Bono traverse en 1 minute
temps(edge, 2).   % Edge traverse en 2 minutes
temps(adam, 5).   % Adam traverse en 5 minutes
temps(larry, 10). % Larry traverse en 10 minutes


% ---------------------------
% 2️⃣ Représentation d’un état
% ---------------------------
% Un état est représenté sous la forme :
%    etat(Gauche, Droite, CoteLampe, Temps)
%
% - Gauche : liste des personnes sur la rive gauche
% - Droite : liste des personnes sur la rive droite
% - CoteLampe : indique où se trouve la lampe (gauche/droite)
% - Temps : temps total écoulé depuis le début

% État initial : tout le monde est à gauche, lampe à gauche, 0 minute.
etat_initial(etat([bono, edge, adam, larry], [], gauche, 0)).

% État final : tout le monde est à droite, lampe à droite,
%              et le temps total ne dépasse pas 17 minutes.
% ⚙️ Ajustement : on ne fixe plus l'ordre des personnes à droite.
%                 On valide "Gauche vide + lampe à droite + T ≤ 17".
etat_final(etat([], _Droite, droite, T)) :-
    T =< 17.


% ---------------------------
% 2️⃣➕ Outils internes (canonicité & utilitaires)
% ---------------------------
% 🎯 Objectif :
%   - Stabiliser la comparaison d’états (anti-boucle efficace)
%   - Éviter les doublons de paires (P1,P2) vs (P2,P1)
%
% canonical_state/2 :
%   On retire le temps et on trie les listes pour comparer
%   uniquement la configuration logique (côté lampe inclus).
%
% select_two_unique/4 :
%   On force A @< B pour ne générer chaque duo qu'une fois.

% Représentation canonique d'un état (sans temps, avec listes triées)
canonical_state(etat(G, D, Cote, _T), state(Gs, Ds, Cote)) :-
    msort(G, Gs),
    msort(D, Ds).

% Sélection d'une paire unique A<B et retrait des deux éléments
select_two_unique(L0, A, B, L2) :-
    select(A, L0, L1),
    select(B, L1, L2),
    A @< B.  % élimine (B,A) quand (A,B) a déjà été généré


% ---------------------------
% 3️⃣ Opérateurs de déplacement (move/3)
% ---------------------------
% move(+EtatCourant, -NouvelEtat, -Action)
%
% Définit les transitions possibles entre deux états.
% Deux cas :
%   1. Si la lampe est à gauche → deux personnes traversent vers la droite.
%   2. Si la lampe est à droite → une personne revient vers la gauche.
%
% Action contient la description du mouvement effectué.
%
% select(X, Liste, Reste) est un prédicat standard :
% il choisit un élément X dans une liste et renvoie le reste.

% --- Cas 1 : Traversée de gauche vers droite (deux personnes) ---
move(etat(G, D, gauche, T), etat(G2, D2, droite, T2), Action) :-

    % Sélection de deux personnes sur la rive gauche
    % 🔧 Ajustement : éviter les doublons en imposant A @< B
    select_two_unique(G, P1, P2, G2),

    % Calcul de la durée de traversée (plus lent des deux)
    temps(P1, T1),
    temps(P2, T2p),
    Tcross is max(T1, T2p),

    % Mise à jour du temps total écoulé
    T2 is T + Tcross,

    % 🔧 Pruning : on ne génère pas d'état si on dépasse 17 minutes
    T2 =< 17,

    % Mise à jour des personnes sur la rive droite
    D2 = [P1, P2 | D],

    % Définition de l'action effectuée (pour affichage)
    Action = traverse([P1, P2], droite, Tcross).


% --- Cas 2 : Retour de droite vers gauche (une personne) ---
move(etat(G, D, droite, T), etat(G2, D2, gauche, T2), Action) :-

    % Sélection d'une seule personne sur la rive droite
    select(P, D, D2),

    % Calcul du temps pour cette personne seule
    temps(P, Tp),

    % Mise à jour du temps total
    T2 is T + Tp,

    % 🔧 Pruning : on ne génère pas d'état si on dépasse 17 minutes
    T2 =< 17,

    % Ajout de la personne sur la rive gauche
    G2 = [P | G],

    % Description de l'action (retour avec la lampe)
    Action = traverse([P], gauche, Tp).


% ---------------------------
% 4️⃣ Recherche de solution (DFS)
% ---------------------------
% solve/0 : point d’entrée principal du moteur de recherche
%
% Étapes :
%   1. On récupère l’état initial.
%   2. On lance la recherche récursive (search/3).
%   3. On inverse la liste des actions (elles sont accumulées à l’envers).
%   4. On affiche la solution finale.

solve :-
    etat_initial(E0),          % point de départ
    canonical_state(E0, C0),   % 🔧 anti-boucle : forme canonique sans temps
    search(E0, [C0], Sol),     % recherche en profondeur
    reverse(Sol, Path),        % inversion pour affichage dans l’ordre
    print_solution(Path).      % impression des actions trouvées


% Renvoie la solution comme une liste d'actions (dans l'ordre)
solve_path(Path) :-
    etat_initial(E0),
    canonical_state(E0, C0),   % 🔧 anti-boucle : forme canonique sans temps
    search(E0, [C0], Sol),
    reverse(Sol, Path).



% ---------------------------
% 5️⃣ Moteur de recherche DFS
% ---------------------------
% search(+EtatCourant, +Visites, -ListeActions)
%
% explore récursivement les états du problème jusqu’à
% atteindre un état final.
% Les actions menant à la solution sont renvoyées sous
% forme de liste.

% Cas de base : si l’état courant est un état final,
% la liste d’actions est vide.
search(E, _, []) :-
    etat_final(E), !.  % le cut empêche d’explorer d’autres chemins après succès

% Cas récursif :
% - on choisit un mouvement possible (move/3)
% - on évite les états déjà visités
% - on poursuit la recherche à partir du nouvel état
search(E, Visited, [Action|Rest]) :-
    move(E, E2, Action),               % trouver un nouvel état accessible
    canonical_state(E2, C2),           % 🔧 comparaison logique (sans temps)
    \+ member(C2, Visited),            % éviter les cycles (déjà visité)
    search(E2, [C2|Visited], Rest).    % exploration récursive


% ---------------------------
% 6️⃣ Affichage de la solution
% ---------------------------
% print_solution(+ListeActions)
%
% Parcourt la liste d’actions et les affiche dans l’ordre.

print_solution(Sol) :-
    writeln('--- Solution ---'),
    forall(member(A, Sol), writeln(A)).
