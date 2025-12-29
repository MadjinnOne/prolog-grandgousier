:- use_module(library(pce)).
:- ensure_loaded('logic').   % charge la logique (sans module)

% ---------------------------
% Paramètres d’animation (fluidité)
% ---------------------------
sim_speed(0.35).        % secondes d'animation par "minute" du plan
frame_interval(0.02).   % ~50 FPS

% ---------------------------
% Canvas & coordonnées d’affichage
% ---------------------------
canvas_size(820, 260).

person_x(gauche, 120).
person_x(droite, 680).
lamp_x(gauche, 80).
lamp_x(droite, 640).

person_y(bono, 60).
person_y(edge, 110).
person_y(adam, 160).
person_y(larry, 210).

% ---------------------------
% Utilitaires XPCE (sécurité)
% ---------------------------
safe_send(Obj, Msg) :-
    (   object(Obj)
    ->  send(Obj, Msg)
    ;   true).

safe_display(Pic, Obj, Pt) :-
    (   object(Pic)
    ->  send(Pic, display, Obj, Pt)
    ;   true).

% Bornage générique (empêche de sortir du canvas)
bounded_xy(Obj, X, Y, Xi, Yi) :-
    canvas_size(Wc, Hc),
    (   get(Obj, width,  W0) -> W = W0 ; W = 0),
    (   get(Obj, height, H0) -> H = H0 ; H = 0),
    Xmin is 0,           Xmax is Wc - W,
    Ymin is 0,           Ymax is Hc - H,
    X1 is max(Xmin, min(X, Xmax)),
    Y1 is max(Ymin, min(Y, Ymax)),
    Xi is round(X1),
    Yi is round(Y1).

% Cible bornée pour une personne selon la rive
target_xy(Obj, Side, Person, Xb, Yb) :-
    person_x(Side, X0),
    person_y(Person, Y0),
    bounded_xy(Obj, X0, Y0, Xb, Yb).

% ---------------------------
% Entrée principale
% ---------------------------
xpce_run :-
    solve_path(Path),
    gui_show(Path).

/* ---------- GUI ---------- */

gui_show(Path) :-
    new(F, frame('U2 – XPCE Animation')),
    send(F, done_message, message(F, destroy)),

    new(P, picture('Canvas')),
    canvas_size(Wc, Hc),
    send(P, size, size(Wc, Hc)),
    send(F, append, P),

    % ---- FOND UNI (BLANC) ----
    new(BG, box(Wc, Hc)),
    send(BG, fill_pattern, colour(white)),
    send(BG, pen, 0),
    safe_display(P, BG, point(0, 0)),

    % Libellés des rives (remontés)
    new(TL, text('GAUCHE')),
    send(TL, font, font(helvetica, bold, 14)),
    safe_display(P, TL, point(50, 8)),

    new(TR, text('DROITE')),
    send(TR, font, font(helvetica, bold, 14)),
    safe_display(P, TR, point(Wc-90, 8)),

    % Pont plus joli (couleurs nommées uniquement)
    draw_pretty_bridge(P, Wc, Hc),

    % Compteur de temps (Timer)
    new(Timer, text('Temps : 0 / 17 min')),
    send(Timer, font, font(helvetica, bold, 16)),
    safe_display(P, Timer, point(320, 6)),

    % Objets pour chaque personne
    make_person(P, bono,  ObjB),
    make_person(P, edge,  ObjE),
    make_person(P, adam,  ObjA),
    make_person(P, larry, ObjL),

    % Lampe : petit cercle jaune
    new(Lamp, circle(14)),
    send(Lamp, fill_pattern, colour(yellow)),
    lamp_x(gauche, LX0),
    safe_display(P, Lamp, point(LX0, 30)),

    % Ouvrir la fenêtre
    send(F, open),

    % Lancer l'animation
    animate_actions(Path,
                    [bono-ObjB, edge-ObjE, adam-ObjA, larry-ObjL],
                    Lamp, 0, Timer, P).

draw_pretty_bridge(P, _Wc, Hc) :-
    DeckX is 150,            % origine tablier
    DeckW is 520,            % largeur tablier
    DeckY is Hc - 40,        % hauteur tablier
    DeckH is 12,

    % --- Rivière : UNIQUEMENT sous le pont ---
    RiverY is DeckY + DeckH + 2,   % commence juste sous le tablier
    RiverH is 26,                  % hauteur d'eau
    new(River, box(DeckW, RiverH)),
    send(River, fill_pattern, colour(skyblue)),
    send(River, pen, 0),
    safe_display(P, River, point(DeckX, RiverY)),

    % --- Tablier ---
    new(Deck, box(DeckW, DeckH)),
    send(Deck, fill_pattern, colour(grey85)),
    send(Deck, pen, 1),
    safe_display(P, Deck, point(DeckX, DeckY)),

    % --- Garde-corps ---
    RailYTop is DeckY - 6,
    RailYBot is DeckY + DeckH + 6,
    new(RailTop, line(DeckX, RailYTop, DeckX+DeckW, RailYTop)),
    new(RailBot, line(DeckX, RailYBot, DeckX+DeckW, RailYBot)),
    send(RailTop, colour, colour(grey40)),
    send(RailBot, colour, colour(grey40)),
    safe_display(P, RailTop, point(0,0)),
    safe_display(P, RailBot, point(0,0)),

    % --- Planches verticales ---
    PlankStep is 40,
    draw_planks(P, DeckX, DeckY, DeckH, DeckW, PlankStep),

    % --- Piliers aux extrémités ---
    new(PillarL, box(16, 26)),
    new(PillarR, box(16, 26)),
    send(PillarL, fill_pattern, colour(grey70)),
    send(PillarR, fill_pattern, colour(grey70)),
    send(PillarL, pen, 0),
    send(PillarR, pen, 0),
    safe_display(P, PillarL, point(DeckX-18, DeckY+DeckH-2)),
    safe_display(P, PillarR, point(DeckX+DeckW+2, DeckY+DeckH-2)).

draw_planks(P, DeckX, DeckY, DeckH, DeckW, Step) :-
    EndX is DeckX + DeckW,
    draw_planks_(P, DeckX+Step, EndX-1, DeckY, DeckH, Step).

draw_planks_(P, X, EndX, DeckY, DeckH, Step) :-
    (   X >= EndX
    ->  true
    ;   Y0 is DeckY - 4,
        Y1 is DeckY + DeckH + 4,
        new(Pl, line(X, Y0, X, Y1)),
        send(Pl, colour, colour(grey65)),
        safe_display(P, Pl, point(0,0)),
        Xn is X + Step,
        draw_planks_(P, Xn, EndX, DeckY, DeckH, Step)
    ).

% Fabrique un objet text pour une personne et le place côté gauche (borné)
make_person(Pic, Person, Obj) :-
    person_y(Person, Y0),
    person_x(gauche, X0),
    atom_string(Person, Label),
    new(Obj, text(Label)),
    send(Obj, font, font(helvetica, bold, 14)),
    bounded_xy(Obj, X0, Y0, Xb, Yb),
    safe_display(Pic, Obj, point(Xb, Yb)).

% Met à jour le texte du compteur "Temps : T / 17 min"
update_timer(Timer, T) :-
    new(S, string('Temps : %d / 17 min', T)),
    safe_send(Timer, string(S)).

% Colore le Timer selon la contrainte de 17 minutes
finalize_timer(Timer, T) :-
    (   T =< 17
    ->  safe_send(Timer, colour(colour(green)))
    ;   safe_send(Timer, colour(colour(red)))
    ),
    update_timer(Timer, T).

/* ---------- Animation (version fluide & synchronisée) ---------- */

% Fin
animate_actions([], _, _, T, Timer, _Pic) :-
    finalize_timer(Timer, T), !.

% Pas suivant : on construit un "groupe" (lampe + personnes) et on le déplace en lockstep
animate_actions([traverse(Persons, Side, DT)|Rest], Objs, Lamp, T0, Timer, Pic) :-

    % 1) cible de la lampe (version simple, fixe par rive)
    lamp_x(Side, LX), Ly = 30,

    % 2) cibles des personnes
    findall(obj(Obj, Xp, Yp),
            ( member(P, Persons),
              member(P-Obj, Objs),
              target_xy(Obj, Side, P, Xp, Yp)
            ),
            PersonTargets),

    % 3) groupe = lampe + personnes
    Group = [obj(Lamp, LX, Ly) | PersonTargets],

    % 4) durée unique basée sur DT (tout le monde bouge ensemble)
    minutes_to_seconds(DT, Secs),
    clamp(Secs, 0.25, 3.5, Dur),

    % 5) déplacement synchronisé du groupe
    smooth_move_group(Group, Dur, Pic),

    % 6) timer logique
    T1 is T0 + DT,
    update_timer(Timer, T1),

    % 7) suite
    animate_actions(Rest, Objs, Lamp, T1, Timer, Pic).

% Conversion minutes (plan) -> secondes d'animation
minutes_to_seconds(Minutes, Seconds) :-
    sim_speed(Factor),
    Seconds is Minutes * Factor.

% Clamp : limite une valeur entre Min et Max
clamp(V, Min, Max, R) :-
    ( V < Min -> R = Min
    ; V > Max -> R = Max
    ; R = V
    ).

% -------- Mouvement synchronisé d’un groupe d’objets --------
% Group = [obj(Obj, Xtarget, Ytarget), ...]
smooth_move_group(Group, DurationSec, Pic) :-
    (   object(Pic)
    ->  frame_interval(Delta),
        (   DurationSec =< 0.0001
        ->  % snap immédiat à la cible (borné)
            forall(member(obj(Obj, X2, Y2), Group),
                   ( bounded_xy(Obj, X2, Y2, Xi, Yi),
                     safe_send(Obj, move(point(Xi, Yi)))
                   )),
            send(Pic, flush)
        ;   Steps is max(1, round(DurationSec / Delta)),
            init_states(Group, Steps, States0),
            move_states(Pic, 0, Steps, Delta, States0)
        )
    ;   true).

% Prépare l'état initial de chaque objet du groupe
% State = st(Obj, Xcur, Ycur, DX, DY, Xtarget, Ytarget)
init_states([], _, []).
init_states([obj(Obj, X2, Y2)|Rest], Steps, [st(Obj, X0, Y0, DX, DY, X2, Y2)|SRest]) :-
    ( get(Obj, position, point(X0_, Y0_)) -> X0a = X0_, Y0a = Y0_ ; X0a = X2, Y0a = Y2 ),
    bounded_xy(Obj, X2, Y2, Xt, Yt),
    DX is (Xt - X0a) / Steps,
    DY is (Yt - Y0a) / Steps,
    X0 is X0a, Y0 is Y0a,
    init_states(Rest, Steps, SRest).

% Boucle d'animation synchronisée (lockstep)
move_states(Pic, I, Steps, _Delta, States) :-
    I >= Steps, !,
    forall(member(st(Obj, X0, Y0, DX, DY, _Xt, _Yt), States),
           ( Xf is X0 + DX*Steps,
             Yf is Y0 + DY*Steps,
             bounded_xy(Obj, Xf, Yf, Xb, Yb),
             safe_send(Obj, move(point(Xb, Yb)))
           )),
    send(Pic, flush).

move_states(Pic, I, Steps, Delta, States0) :-
    I1 is I + 1,
    advance_all(States0, States1, Pic),
    send(Pic, flush),
    sleep(Delta),
    move_states(Pic, I1, Steps, Delta, States1).

% Avance d'un pas tous les objets, retourne les nouveaux états courants
advance_all([], [], _Pic).
advance_all([st(Obj, Xc, Yc, DX, DY, Xt, Yt)|R],
            [st(Obj, Xn, Yn, DX, DY, Xt, Yt)|R2], Pic) :-
    Xtemp is Xc + DX,
    Ytemp is Yc + DY,
    bounded_xy(Obj, Xtemp, Ytemp, Xb, Yb),
    safe_send(Obj, move(point(Xb, Yb))),
    Xn = Xb, Yn = Yb,
    advance_all(R, R2, Pic).

/* ---------- Lancement logique + GUI ---------- */

:- dynamic u2_already_ran/0.
