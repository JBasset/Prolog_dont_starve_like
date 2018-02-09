% Projet Don't starve

% SOMMAIRE DU CODE

% Mode d'emploi du jeu
% Etat du jeu.
% Règle qui démarre le jeu.
% prédicats dynamiques du jeu
% Initialisation des éléments, des cases, des drops, ...
% Outils de syntaxe 
% Gestion du temps et de l'attente
% fonctions de calcul liees au temps
% Gestion des ressources du joueur (pv, faim, ...)
% Gestion des combats, nombre d'ennemis, drop
% Gestion de l'inventaire 
% Observation, description, interface utilisateur 
% Gestion de la map


/* ______________________________________________________________________________________ */
/* mode d'emploi du jeu. */

mode_emploi :-
        nl,
        write("Entrez les commandes avec la syntaxe Prolog standard."), nl,
        write("Les commandes disponibles sont :"), nl,
        write('demarrer.			-- pour commencer une partie.'), nl,
        write('n. s. e. o.			-- pour aller dans une direction.'), nl,
        write('regarder.			-- pour regarder autour de toi.'), nl,
        write('inventaire.			-- pour regarder ton inventaire.'), nl,
        write('ramasser.			-- pour ramasser tous les objets a disposition.'), nl,
        write('jeter.				-- pour laisser tomber un objet.'), nl,
        write('listeRecettes.			-- afficher les recettes que tu connais.'), nl,
        write('consulterRecette(X).		-- pour affiche la recette de l''objet X.'), nl,
        write('craft.				-- pour fabriquer un objet de ta liste de recettes.'), nl,
        write('manger.				-- pour manger.'), nl,
        write('cuire.				-- pour cuire de la nourriture.'), nl,
        write('attaquer.			-- pour attaquer un ennemi.'), nl,
        write('ouvrir.				-- pour ouvrir quelque-chose ?'), nl,
        write("mode_emploi.			-- pour afficher ce mode d'emploi de nouveau."), nl,
        write('halt.				-- pour terminer la partie.'), nl,
        write('compteRendu.			-- pour afficher les stats, la position et le temps restant avant la nuit.'), nl,
        write('attendre(duree).		-- pour ne rien faire pendant un certain temps'), nl,
        write('afficherMap.			-- pour afficher la map (avec une croix pour savoir ou on est)'), nl,
        write('consulterCarte.			-- pour consulter une carte au tresor... Si tu en trouve une.'), nl,
        nl.

/* Etat du jeu. */

compteRendu :- 
write("temps restant avant la nuit : "), tempsRestant(T), write(T), nl, 
write("pv : "), pv(PV), write(PV), nl,
write("mental : "), mental(M), write(M), nl,
write("faim : "), faim(F), write(F), nl,
write("case actuelle : "), je_suis_a(Ici), write(Ici), nl.

/* Règle qui démarre le jeu. */

demarrer :-
	nl, nl, 
	write("Dans ce jeu, tu incarnes Wilson, un gentleman scientifique intrepide et ambitieux qui s'est tres recemment retrouve dans un univers sauvage et mysterieux suite a une altercation avec un demon denomme Maxwell. Abandonne sur une ile deserte, il te faudra tenter de survivre, et chercher un moyen de te sortir de ce mauvais pas... Tu n'as pas l'air au meilleur de ta forme. Tu ferais bien de trouver quelque chose a te mettre sous la dent avant que la nuit ne tombe ..."),
	nl,nl,
	write("Au cours de la partie, tu devras tenir compte des donnees suivantes : le temps s'ecoule (et il vaut mieux etre eclaire lorsque la nuit tombe !), la sante physique importe (une blessure est si vite arrivee avec toutes les creatures etranges qu'on peut croiser ici !), la sante mentale importe egalement (difficile de garder son calme, surtout en mangeant n'importe quoi...) et surtout, surtout : la faim."),nl,
	write("Ne meurs pas de faim."), 
	nl,
	nl, nl, write(" ## DEBUT DE LA PARTIE ## "), nl, nl,
	write("Tu es au milieu de nulle part. "),
	je_suis_a(X),
	decrire(X),
	nl, nl, write("> pour regarder autour, utilise 'regarder.'."),
	nl, write("N'hesite pas a souvent regarder autour de toi, tu pourrais rater des elements importants ou te jeter dans un piege..."), nl, 
	write("> pour avoir une idee de la carte du monde, utilise 'afficherMap.' ;"), nl, 
	write("> pour voir le mode d'emploi, utilise 'mode_emploi.' ").


% Les différents prédicats dynamiques du jeu ;
:- dynamic tempsRestant/1, pv/1, faim/1, mental/1, je_suis_a/1, nombre/1, vivant/1, estSur/2, estSur/3, combat/2, dansInventaire/1, dansListeRecettes/1,compteur/2,choix/2.
:- retractall(tempsRestant(_)), retractall(pv(_)), retractall(faim(_)), retractall(mental(_)),retractall(je_suis_a(_)), retractall(dansInventaire(_,_)), retractall(nombre(_)), retractall(vivant(_)),retractall(estSur(_,_)),retractall(estSur(_,_,_)), retractall(combat(_,_)), retractall(dansListeRecettes(_)), retractall(compteur(_,_)),retractall(choix(_,_)).

/* ______________________________________________________________________________________ */
/* Initialisation des éléments, des cases, des drops, ... */

% case de départ du joueur
je_suis_a(54).

% compétence de combat
combat(moi,0). % le joueur a une competence de combat de 0 initialement
combat(X,3) :- araignee(X). % les araignees ont une competence de combat de 3
combat(X,5) :- chien(X); arbre(X).
combat(X,0) :- lapin(X) ; chester(X).
%combat(X,1000) :- lapinBlancDeCaerbannog(X).
combat(X,10) :- echassier(X).

% on retract vivant/1 lorsque la créature meurt
vivant(araignee1).
vivant(araignee2).
vivant(araignee3).
vivant(araignee4).
vivant(araignee5).
vivant(chien1).
vivant(chien2).
vivant(chien3).
vivant(lapin1).
vivant(lapin2).
vivant(lapin3).
vivant(lapin4).
vivant(lapin5).
vivant(chester1).
vivant(echassier1).
vivant(arbre5). % lui il est mechant

% caracterisation des créatures
araignee(araignee1).
araignee(araignee2).
araignee(araignee3).
araignee(araignee4).
araignee(araignee5).
chien(chien1).
chien(chien2).
chien(chien3).
lapin(lapin1).
lapin(lapin2).
lapin(lapin3).
lapin(lapin4).
lapin(lapin5).
chester(chester1).
echassier(echassier1).
arbre(arbre1).
arbre(arbre2).
arbre(arbre3).
arbre(arbre4).
arbre(arbre5). % lui il est mechant
arbre(arbre6).
arbre(arbre7).

% manger influence la jauge de faim... et parfois la jauge de vie !
apportNutritionnel(carotte,10).
apportNutritionnel(carotte_cuite,20).
apportNutritionnel(viandeMonstre,20).
apportNutritionnel(viande_de_lapin_crue,20).
apportNutritionnel(viande_de_lapin_cuite,40).
apportNutritionnel(viandeMonstreCuite,40).
apportNutritionnel(champignon,20).
apportNutritionnel(champignon_cuit,40).
apportNutritionnel(oeuf,0). % l'oeuf ne sera pas mangé
apportNutritionnel(oeufCuit,0). % l'oeuf ne sera pas mangé

% les aliments toxiques, qui retireront de la vie au joueur s'il les mange
toxique(viande_de_lapin_crue,10) :- nl, write("Tu n'aurais peut etre pas du manger ca cru...").
toxique(viandeMonstre,30) :- nl, write("quelle idee de manger ca aussi ! Peut etre en la cuisant ?").
toxique(viandeMonstreCuite,15) :- nl, write("C'est vraiment pas bon... Mieux que cru, mais tu te sens quand meme assez mal apres avoir mange.").

% Les champignons retirent de la santée mentale au joueur quand ils sont mangés
hallucinogene(champignon,40) :- nl, write("Le monde semble encore plus etrange depuis que tu as mange ce champignon...").
hallucinogene(champignon_cuit,20) :- nl, write("Le monde semble encore plus etrange depuis que tu as mange ce champignon...").

% les objets cuisinables donnent leur version cuite quand passés au feu
cuisinable(carotte,carotte_cuite).
cuisinable(viande_de_lapin_crue,viande_de_lapin_cuite).
cuisinable(viandeMonstre,viandeMonstreCuite).
cuisinable(champignon,champignon_cuit).
cuisinable(oeuf,oeufCuit) :- retract(compteur(oeuf,_)).

%les objet brulables sont détruits quand passés au feu
brulable(branche).
brulable(toile).
brulable(buche).
brulable(X) :- carte(X,_).

%objets que le joueur peut ramasser
ramassable(carotte).
ramassable(toile).
ramassable(viandeMonstre).
ramassable(viande_de_lapin_crue).
ramassable(oeuf).
ramassable(buche).
ramassable(silex).
ramassable(champignon).
ramassable(branche).
ramassable(recetteBateau).
ramassable(X) :- carte(X,_),!.

%la carte, la case vers laquelle elle pointe
carte(carte1,1). % pointe sur la carte 2
carte(carte2,130). % pointe sur la carte 3
carte(carte3,24). % pointe sur chester

% estSur/2 : estSur(Entité , position sur la map)
estSur(araignee1,65).
estSur(araignee2,63).
estSur(araignee3,1).
estSur(araignee4,12).
estSur(araignee5,134).
estSur(araignee6,9).

estSur(chien1,115).
estSur(chien3,29).
estSur(lapin1,53).
estSur(lapin2,20).
estSur(lapin3,143).
estSur(lapin4,86).
estSur(lapin5,25).
estSur(echassier1,99). % il n'y aura qu'un echassier dans ce jeu, aussi on appelera plus tard directement echassier1
estSur(chien2,24).

estSur(chester,24).

estSur(arbre1,128).
estSur(arbre2,35).
estSur(arbre3,26).
estSur(arbre4,122).
estSur(arbre5,74).  % lui, il est mechant
estSur(arbre6,71).
estSur(arbre7,143).

% estSur/3 : estSur(Objet ramassable , position sur la map, nombre)
estSur(carte1,98,1).
estSur(carte2,1,1).
estSur(carte3,130,1).

estSur(branche,100,2).
estSur(branche,44,2).
estSur(branche,91,2).
estSur(branche,93,2).
estSur(branche,118,2).
estSur(branche,140,2).
estSur(branche,54,2).
estSur(branche,9,2).

estSur(silex,1,2).
estSur(silex,2,3).
estSur(silex,3,1).
estSur(silex,80,1).
estSur(silex,13,10).
estSur(silex,25,4).
estSur(silex,15,2).
estSur(silex,37,2).
estSur(silex,139,2).
estSur(silex,143,2).
estSur(silex,8,2).
estSur(silex,40,2).

estSur(champignon,39,1).
estSur(champignon,100,1).
estSur(champignon,75,1).
estSur(champignon,115,1).
estSur(champignon,83,1).

estSur(carotte,1,1).
estSur(carotte,3,1).
estSur(carotte,8,1).
estSur(carotte,9,1).
estSur(carotte,24,1).
estSur(carotte,46,1).
estSur(carotte,71,2).
estSur(carotte,84,1).
estSur(carotte,108,1).
estSur(carotte,106,1).
estSur(carotte,142,1).
estSur(carotte,141,1).
estSur(carotte,128,1).
estSur(carotte,127,1).
estSur(carotte,102,1).
estSur(carotte,100,1).
estSur(carotte,134,2).
estSur(carotte,135,1).
estSur(carotte,50,1).

% drop/3 : drop(Entité, Objet ramassable, nombre)
drop(araignee1,toile,1).
drop(araignee1,viandeMonstre,2).
drop(araignee2,toile,5).
drop(araignee2,viandeMonstre,1).
drop(araignee3,toile,2).
drop(araignee3,viandeMonstre,2).
drop(araignee4,toile,1).
drop(araignee4,viandeMonstre,2).
drop(araignee5,toile,5).
drop(araignee5,viandeMonstre,1).
drop(araignee6,toile,3).
drop(araignee6,viandeMonstre,1).
drop(echassier1,oeuf,1).
drop(X,viandeMonstre,1) :- chien(X).
drop(X,viande_de_lapin_crue,1) :- lapin(X).
drop(arbre5,buche,5).
drop(X,buche,1) :- not(X == arbre5), arbre(X).
drop(X,branche,5) :- arbre(X).
drop(X,buche,2) :- arbre(X).


ennemi(Objet):- 
	araignee(Objet); chien(Objet); echassier(Objet); Objet == arbre5.
	
mobile(Objet) :- vivant(Objet), (araignee(Objet); lapin(Objet)).

equipable(Objet) :- Objet == lance. 

/* ______________________________________________________________________________________ */
/* Outils de syntaxe  */

nomDefini(Objet):-araignee(Objet), write('l''araignee '),!.
nomDefini(Objet):-chien(Objet), write('le chien '),!.
nomDefini(Objet):-lapin(Objet), write('le lapin '),!.
nomDefini(Objet):-echassier(Objet), write('le grand oiseau '),!.
nomDefini(Objet):-arbre(Objet), write('l''arbre '),!.
nomDefini(carotte) :- write("la carotte"),!.
nomDefini(buche) :- write("la buche"),!.
nomDefini(branche) :- write("la branche"),!.
nomDefini(silex) :- write("le silex"),!.
nomDefini(toile) :- write("la toile"),!.
nomDefini(viande_de_lapin_crue) :- write("la viande de lapin crue"),!.
nomDefini(viandeMonstre) :- write("la viande de monstre"),!.
nomDefini(viandeMonstreCuite) :- write("la viande de monstre cuite"),!.
nomDefini(oeuf) :- write("l'oeuf"),!.
nomDefini(hache) :- write("la hache "),!.
nomDefini(lance) :- write("la lance "),!.
nomDefini(feu) :- write("le feu "),!.
nomDefini(chester) :- write("Chester "),!.
nomDefini(recetteBateau) :- write("le bout de papier "),!.
nomDefini(carotte_cuite) :- write("la carotte cuite "),!.
nomDefini(oeufCuit) :- write("l'oeuf cuit "),!.
nomDefini(viande_de_lapin_cuite) :- write("la viande de lapin cuite "),!.
nomDefini(carotte_cuite) :- write("la carotte cuite "),!.
nomDefini(carte1) :- write("carte n 1 "),!.
nomDefini(carte2) :- write("carte n 2 "),!.
nomDefini(carte3) :- write("carte n 3 "),!.
nomDefini(champignon) :- write("le champignon "),!.
nomDefini(champignon_cuit) :- write("le champignon cuit"),!.
nomDefini(bateau) :- write("le bateau"),!.
nomDefini(_):- write('l''objet que les dev ont oublie de nommer. '),!.

nomIndefini(Objet):-araignee(Objet), write('une araignee '),!.
nomIndefini(Objet):-chien(Objet), write('un chien '),!.
nomIndefini(Objet):-lapin(Objet), write('un lapin '),!.
nomIndefini(Objet):-echassier(Objet), write('un grand oiseau '),!.
nomIndefini(Objet):-arbre(Objet), write('un arbre '),!.
nomIndefini(carotte) :- write("une carotte"),!.
nomIndefini(buche) :- write("une buche"),!.
nomIndefini(branche) :- write("une branche"),!.
nomIndefini(silex) :- write("un silex"),!.
nomIndefini(toile):- write('une toile'),!.
nomIndefini(viandeMonstreCuite):- write('un steack de monstre cuit '),!.
nomIndefini(viandeMonstre):- write('un steack de monstre '),!.
nomIndefini(oeuf):- write('un oeuf '),!.
nomIndefini(viande_de_lapin_crue):- write('une viande de lapin crue '),!.
nomIndefini(viande_de_lapin_cuite):- write('une viande de lapin cuite '),!.
nomIndefini(hache):- write('une hache '),!.
nomIndefini(lance):- write('une lance '),!.
nomIndefini(feu):- write('un feu '),!.
nomIndefini(champignon):- write('un champignon '),!.
nomIndefini(champignon_cuit):- write('un champignon cuit '),!.
nomIndefini(chester) :- write("un etrange coffre poilu "),!.
nomIndefini(recetteBateau) :- write("un bout de papier "),!.
nomIndefini(carotte_cuite) :- write("une carotte cuite "),!.
nomIndefini(oeufCuit) :- write("un oeuf cuit "),!.
nomIndefini(X) :- carte(X,_), write("une carte "),!.
nomIndefini(bateau) :- write("un bateau "),!.
nomIndefini(_):- write('un objet que les dev ont oublie de nommer. '),!.

nomPluriel(Objet):-araignee(Objet), write('araignees '),!.
nomPluriel(Objet):-chien(Objet), write('chiens '),!.
nomPluriel(Objet):-lapin(Objet), write('lapins '),!.
nomPluriel(Objet):-echassier(Objet), write("c'est amusant que cette ligne de code soit appelee puisqu'il n'y a qu'un echassier dans le jeu"),!.
nomPluriel(recetteBateau) :- write("c'est amusant que cette ligne de code soit appelee puisqu'il n'y a qu'une recetteBateau dans le jeu"),!.
nomPluriel(Objet):-arbre(Objet), write('arbres '),!.
nomPluriel(carotte) :- write("carottes"),!.
nomPluriel(buche) :- write("buches"),!.
nomPluriel(branche) :- write("branches"),!.
nomPluriel(silex) :- write("silex"),!.
nomPluriel(toile) :- write("toiles"),!.
nomPluriel(viandeMonstreCuite) :- write("steacks de monstre cuits"),!.
nomPluriel(viandeMonstre) :- write("steacks de monstre cuit"),!.
nomPluriel(viande_de_lapin_crue) :- write("viandes de lapin crues"),!.
nomPluriel(viande_de_lapin_cuite) :- write("viandes de lapin cuites"),!.
nomPluriel(oeuf) :- write("oeufs"),!.
nomPluriel(hache) :- write("haches"),!.
nomPluriel(lance) :- write("lances"),!.
nomPluriel(feu) :- write("feux"),!.
nomPluriel(hache) :- write("lances"),!.
nomPluriel(champignon):- write('champignons '),!.
nomPluriel(champignon_cuit):- write('champignons cuits '),!.
nomPluriel(chester) :- write("c'est amusant que cette ligne de code soit appelee puisqu'il n'y a qu'un chester dans le jeu"),!.
nomPluriel(carotte_cuite) :- write("carottes cuites"),!.
nomPluriel(oeufCuit) :- write("oeufs cuits"),!.
nomPluriel(X) :- carte(X,_), write("des cartes"),!.
nomPluriel(_):- write('objets que les dev ont oublie de nommer. '),!.

/* ______________________________________________________________________________________ */
/* Gestion du temps et de l'attente'  */


/* On modélise pour ce jeu le temps restant avant la nuit (tempsRestant(X)). Chaque action effectuée par le joueur a une durée en temps, connue ou non du joueur, et appelera donc le prédicat attendre(X) pour faire passer X unités de temps. */

tempsRestant(100).

tempsEcoule :- 
	tempsRestant(X),
	X =< 0,
	retractall(tempsRestant(X)),
	assert(tempsRestant(100)),
	nuit.
	
nuit :-
	nl,write("La nuit tombe !"),
	(
		je_suis_a(Ici),
		estSur(feu,Ici),
		nl,write("Tu te couche a cote de ton feu pour passer la nuit. Au petit matin, tu te reveille (plus ou moins) frais et dispo ! Ton feu s'est eteint pendant la nuit."),nl,nl,
		write("Une nouvelle journee commence ! Explore l'ile pour trouver une issue, et n'hesite pas a utiliser 'regarder.' pour ne pas rater d'indices..."),
		nl,nl,retractall(estSur(feu,_)),!
	);
	(
		je_suis_a(Ici),
		not(estSur(feu,Ici)),
		nl,write("Tu es dans le noir complet ! tu entends des bruits autour de toi, mais sans lumiere qui sait de qui - de quoi? - ils proviennent ? Tu sens soudain un presence derriere toi, un chox sourd et... plus rien. "),
		blesser(100)
	).

% prédicat à appeler pour tout passage de temps
attendre(TempsAttendu) :- 
	tempsRestant(Y),
	calculFaim(TempsAttendu,Y,Faim),
	famine(Faim),
	calculDeplEnnemis(TempsAttendu,NbDeplacements),
	toutDeplacer(NbDeplacements),
	avancerCompteurs(TempsAttendu),
	je_suis_a(Ici), decrire(Ici),
	retractall(tempsRestant(Y)),
	Z is Y - TempsAttendu,
	assert(tempsRestant(Z)),
	not(tempsEcoule),
	nl,write("Il s'est ecoule "),write(TempsAttendu), write(" unite(s) de temps."),
	nl,write(" ## petit compte-rendu ## "),nl,compteRendu,nl,
	(
		(
			Z < 10, je_suis_a(Ici),not(estSur(feu,Ici)),
			nl,write("Attention, la nuit arrive... Tu ferais mieux de te faire un feu pour eloigner les betes sauvages."),
			nl,!
		);true
	).


% L'attente fait avancer les differents compteurs actifs
avancerCompteurs(X) :-
	tempsRestant(Y),
	Z is Y - X,
	(
		(Z > 0, TpsEcoule is X, !); 
		(Z =< 0, TpsEcoule is Y, !)
	),
	(forall(compteur(Objet,Tps),
	(
		retract(compteur(Objet,Tps)),
		NvTps is Tps - TpsEcoule,
		(
			(
				NvTps > 0, assert(compteur(Objet,NvTps)), !
			);
			(
				% si l'Objet considéré par le forall est un oeuf...
				Objet == oeuf,
				compteurEcoule(oeuf),!
			);
			(
				% si l'Objet considéré par le forall est un chien...
				chien(Objet),
				compteurEcoule(Objet),!
			)
		)
	)),!);%finforall
	true.

compteurEcoule(Objet) :-
	(
	% si l'Objet considéré par le forall est un oeuf...
		Objet == oeuf,
		nl,write("Oh ? tututututututu tututu tututu... Ton oeuf a eclot ! L'oiseau qui en sort te regarde et se jette sur toi !... Pour se blottir contre ta jambe. On dirait qu'il te prend pour sa maman. Il est deja assez grand (pas etonnant quand on voit la taille de son geniteur...), il pourra sans aucun doute t'aider a te defendre. Il te donne 3 de puisssance de combat en plus."),nl,
	% ... l'oiseau nait et augmente la puissance de combat du joueur
		assert(vivant(oiseau)),
		combat(moi,C),
		NC is C + 3,
		retract(combat(moi,C)),
		assert(combat(moi,NC)), !
	);
	(
	% si l'Objet considéré par le forall est un chien...
		chien(Objet),
		nl,write("Et pourtant tu t'en doutais ! Le chien t'attaque par derriere, le combat s'engage assez mal..."),
		combat(Objet,C), NC is C*2,
		retractall(combat(Objet,C)),
		assert(combat(Objet,NC)),
		% ... on force l'attaque sur le chien
		attaquer(Objet)
	).
	
% l'attente engendre des déplacements (voir plus bas pour la gestion de la map)
toutDeplacer(0):- !.
toutDeplacer(NbDeplacements):-	
	NbDeplacements > 0,
	forall( mobile(Objet),deplacer(Objet)), 
	X is NbDeplacements - 1,
	toutDeplacer(X).

/* fonctions de calcul liees au temps, avec TempsAttendu le temps d'attente et Y le temps restant avant la nuit. */
% calculFaim : la Faim se calcule soit avec le temps d'attente, soit avec le temps restant avant la nuit si le temps d'attente est supérieur à ce dernier
calculFaim(TempsAttendu,Y,Faim) :- (Y - TempsAttendu > 0, Faim is TempsAttendu // 2, !) ; (Faim is Y // 2).
calculDeplEnnemis(TempsAttendu,NbDeplacements) :- (NbDeplacements is TempsAttendu // 3).


/* ______________________________________________________________________________________ */
/* Gestion des ressources du joueur  */

/* 
Les ressources sont :
ses points de vie, pv
la faim qu'il ressent (plus le chiffre est bas plus le joueur a faim)
sa santé mentale (plus le chiffre est bas plus le joueur devient fou)
*/

pv(100).

mort :- 
	pv(X), X =< 0, 
	nl, 
	nl, 
	nl, 
	write("TU ES MORT !!! Partie terminee."),nl , write("Tape la commande halt"),nl,abort.

blesser(X) :- % prédicat à appeler pour toute perte de points de vie
	pv(Y),
	retractall(pv(Y)),
	Z is Y - X,
	assert(pv(Z)),
	nl,write(" Tu as perdu "), write(X), write(" points de vie."), nl,
	not(mort).

soigner(Soin) :- % prédicat à appeler pour tout gain de points de vie
	pv(X),
	retract(pv(X)),
	Y is X + Soin,
	((Y > 100, assert(pv(100)));
	(Y < 100, assert(pv(Y)))),
	pv(Z),
	nl, write("Tu as recupere "), write(Soin), write(" pv. "), 
	write("Tu as maintenant "), write(Z), write(" pv"),nl.
	
faim(100).

affame :- % jauge de faim vide ?
	faim(X), X =< 0,
	retractall(faim(X)),
	assert(faim(0)),
	Y is -X,
	nl, write("La faim te taraude..."),
	blesser(Y).

famine(X) :- % prédicat à appeler pour toute perte de points de faim
	faim(Y),
	retractall(faim(Y)),
	Z is Y - X,
	assert(faim(Z)),
	not(affame).

rienAManger :- % permet de déterminer s'il n'y a rien a manger
	retractall(nombre(_)),
	assert(nombre(0)),
	forall((
		dansInventaire(Objet,_),
		apportNutritionnel(Objet,_)
	),
		(nombre(X),
		retractall(nombre(_)),
		X1 is X + 1,
		assert(nombre(X1)))
		),%fin forall
	nombre(Y),
	0 =:= Y.

manger :- % choix du joueur
	(
		rienAManger, nl, write("rien ne se mange dans ton inventaire"), nl, !
	);
	(
		findall(Objet,
		(
			dansInventaire(Objet,_),
			apportNutritionnel(Objet,_)
		),
		LObjet),%fin findall
		retractall(choix(_,_)),
		retractall(nombre(_)),
		assert(nombre(0)),
		nl,write("Que voulez vous manger ? (entrez le numero de votre choix) "),
		forall(dansListe(Nourriture,LObjet),
		(
			nl,
			nombre(X),
			retractall(nombre(X)),
			Y is X + 1,
			assert(nombre(Y)),
			assert(choix(Y,Nourriture)),
			write(Y),write(" : "),nomIndefini(Nourriture)
		)), %fin forall
		nl,nl,
		read(Choix),
		choix(Choix,NourritureChoisie),
		manger(NourritureChoisie)
	).
	

dansListe(E,[E|_]).
dansListe(E,[_|T]) :- dansListe(E,T).
	
manger(Objet) :-
	(
		(Objet == oeuf ; Objet == oeufCuit),retirerInv(Objet,1),mangerOeuf(Objet),!,attendre(4)
	);
	(
		retirerInv(Objet,1),
		apportNutritionnel(Objet,Apport),
		faim(X),
		retract(faim(X)),
		Y is X + Apport,
		(
			(Y < 100, assert(faim(Y)));
			(Y >= 100, assert(faim(100)))
		),
		faim(Faim), % la jauge de faim monte
		nl, write("tu as recupere "), 
		write(Apport), write(" points de faim en mangeant "), 
		nomDefini(Objet), write(", ta faim est maintenant a "), write(Faim),nl,
		(
			(
				toxique(Objet,Toxicite),
				blesser(Toxicite)
			);
			(
				hallucinogene(Objet,Toxicite),
				degenerer(Toxicite)
			);
			(
				nl,write("Manger te soigne."),
				Soin is Apport / 2,
				soigner(Soin)
			)
		),
		!,attendre(4)
	);
	(
		(
			apportNutritionnel(Objet,_),write("Tu n'en a pas dans ton inventaire."),!,attendre(4)
		);
		(
			nl,write("Tu ne peux pas manger ca !"),attendre(4)
		)
	).

mangerOeuf(Objet) :-
	(
		Objet == oeuf,
		nl, write("Malheur ! Dans l'oeuf, aucune trace de ta future omelette, mais le corps d'un petit oisillon... Vu sa taille, il etait deja sur le point de briser sa coquille, mais le choc quand tu as casse l'oeuf l'a tue... Te voila sans nourriture et assassin d'oisillon, sacre coup pour le moral!"),nl,
		degenerer(20),!
	);
	(
		Objet == oeufCuit,
		nl, write("Malheur ! Dans l'oeuf, aucune trace de ta future omelette, mais le corps d'un petit oisillon... Vu sa taille, il etait deja sur le point de briser sa coquille, mais il n'a evidemment pas survecu a la cuisson. Son corps carbonise git entre tes doigts, ses orbites maintenant vides poitees vers toi comme une derniere accusation... Te voila sans nourriture et assassin d'oisillon, sacre coup pour le moral!"),nl,
		degenerer(40)
	).

mental(100).

fou :-
	mental(X),
	X =< 0,
	nl,write("C'est la goutte d'eau qui met le feu aux poudres ! Malmene par les evenements, ton esprit t'abandonne lachement pour aller voir ailleurs si tu y es. Dans tes derniers instants d'hysterie, tu te jettes a l'eau pour quitter cette ile-prison a la nage. Et non, chevaucher ce requin pour aller plus vite n'etait pas une bonne idee..."),nl,
	blesser(100).

degenerer(X) :-% prédicat à appeler pour toute perte de points de mental
	mental(Y),
	retractall(mental(Y)),
	Z is Y - X,
	assert(mental(Z)),
	nl, write("Tu perds "), write(X), write(" point(s) de mental. Ton mental est maitenant a "), write(Z),nl,
	not(fou).

/* ______________________________________________________________________________________ */
/* Gestion des combats  */
		
attaquer :-
	je_suis_a(Ici),
	findall(X,(estSur(X,Ici),vivant(X)),L),
	length(L,Len1),
	(
	(Len1 =:= 0, write("Il n'y a rien a attaquer ici"), nl, !);
	(Len1 =:= 1, estSur(Objet,Ici),vivant(Objet),attaquer(Objet), !);
	(Len1 > 1,
	findall(Y,(estSur(Y,Ici),ennemi(Y)),LE),
	length(LE,Len2),
	(
	(Len2 =:= 0, write("Precise ce que tu veux attaquer."), nl, !);
	(Len2 =:= 1,estSur(Objet2,Ici),ennemi(Objet2),attaquer(Objet2), !);
	(Len2 > 1, write("Tu n'aurais pas du attaquer plusieurs adversaires en meme temps... Tu te bas vaillement, mais tu ne peux rien faire contre leur surnombre."),nl,pv(X),blesser(X),!)))
	), !.

attaquer(X):- not(vivant(X)),write('Attaque impossible.'),nl.

attaquer(lapin) :-
	je_suis_a(Ici),
	estSur(Objet,Ici),
	lapin(Objet),
	attaquer(Objet),nl,!.
	
attaquer(araignee):-
	je_suis_a(Ici),
	estSur(Objet,Ici),
	araignee(Objet),
	attaquer(Objet), nl,!.
	
attaquer(chien):-
	je_suis_a(Ici),
	estSur(Objet,Ici),
	chien(Objet),
	attaquer(Objet), nl,!.
	
attaquer(Objet):-
	je_suis_a(Ici),
	estSur(Objet,Ici),
	vivant(Objet),
	(
	not(ennemi(Objet)),
	tuer(Objet),nl,!
	);
	(
	ennemi(Objet),
	combattre(Objet),
	tuer(Objet), nl,!
	).
	
combattre(X) :-
	combat(moi,CombatJoueur),
	combat(X,CombatEnnemi),
	Blessure is CombatEnnemi * 10 - CombatJoueur * 10,
	(
		(
			Blessure > 0, write("Tu es blesse au cours du combat ; "),nl, 
			blesser(Blessure),!
		);
		(
			Blessure =:= 0, write("Tu n'as pas pris un seul coup ! "),nl
		)
	),
	pv(Pv),Pv > 0,write('C''etait un beau combat ! '),
	!.

nbEnnemis(Case,Y) :- 
	retractall(nombre(_)),
	assert(nombre(0)),
	forall((ennemi(Objet),estSur(Objet,Case)),
		(nombre(X),
		retractall(nombre(_)),
		X1 is X + 1,
		assert(nombre(X1)))),
	nombre(Y).
	
tuer(Objet):- 
(
	not(vivant(Objet)),
	write('Cet objet n''est pas vivant. '),
	nl, !, fail
).

tuer(Objet):- 
	((chien(Objet),retractall(compteur(Objet,_)));true),
	retract(vivant(Objet)),
	((
		nbDrop(Objet,Y),
		0 =:= Y,
		write("Il n'y a rien de recuperable sur le corps. "),!
	);
	(
		nbDrop(Objet,Y),
		1 =:= Y,
		drop(Objet,NomDrop,_),
		write('Autour du corps de '),nomDefini(Objet),write('tu trouves '),nomIndefini(NomDrop),nl,
		deposeDrop(Objet,NomDrop,1),
		!
	);
	(
		write('Autour du corps de '),nomDefini(Objet),write('tu trouves...'),nl,
		forall(drop(Objet,NomDrop,Nb),
			(write('- '),
			nomIndefini(NomDrop),
			write("(*"),write(Nb),write(") "),nl,
			deposeDrop(Objet,NomDrop,Nb))
		)
	)),
	nl,
	retractall(estSur(Objet,_)).

deposeDrop(_,_,0) :- !.

deposeDrop(Objet,NomDrop,Nb) :-
	Nb > 0,
	estSur(Objet,Ici),
	(
		(
			not(estSur(NomDrop,Ici,_)),
			assert(estSur(NomDrop,Ici,1)),
			X is Nb - 1,
			deposeDrop(Objet,NomDrop,X),!
		);
		(
			estSur(NomDrop,Ici,X),
			Y is X + 1,
			retractall(estSur(NomDrop,Ici,X)),
			assert(estSur(NomDrop,Ici,Y)),
			Z is Nb - 1,
			deposeDrop(Objet,NomDrop,Z),!
		)
	).



nbDrop(Objet,Y) :- 
	retractall(nombre(_)),
	assert(nombre(0)),
	forall(drop(Objet,_,Nb),
		(nombre(X),
		retractall(nombre(_)),
		X1 is X + Nb,
		assert(nombre(X1)))),
	nombre(Y).


/* ______________________________________________________________________________________ */
/* Gestion de l'inventaire */


inventaireVide :-
	retractall(nombre(_)),
	assert(nombre(0)),
	forall(dansInventaire(_,_),
		(nombre(X),
		retractall(nombre(_)),
		X1 is X + 1,
		assert(nombre(X1)))),
	nombre(Y),
	0 =:= Y.

inventaire :-
	(inventaireVide, !, nl, write("Ton inventaire est vide. "), nl, nl) ;
	nl, write("Ton inventaire contient pour le moment : "), nl,
	forall(dansInventaire(X,Y),
	(((Y =:= 1, nomIndefini(X));
	write(Y), write(" "), nomPluriel(X)), nl)),
	nl, nl.

% ce predicat ne doti pas etre appele par le joueur
retirerInv(Objet,NbRetire) :-
	dansInventaire(Objet,Nb),
	Nb1 is Nb - NbRetire,
	retract(dansInventaire(Objet,Nb)),
	((Nb1 > 0, assert(dansInventaire(Objet,Nb1)),!);true).
	
jeter :-
(
	not(rienAJeter),
	retractall(nombre(_)),
	retractall(choix(_,_)),
	assert(nombre(0)),
	nl,write("Que veux tu jeter ? (entre le numero de ton choix) "),
	forall(dansInventaire(Objet,_),
	(
		nl,
		nombre(X),
		retractall(nombre(X)),
		Y is X + 1,
		assert(nombre(Y)),
		assert(choix(Y,Objet)),
		write(Y),write(" : "),nomIndefini(Objet)
	)),nl,nl,
	read(ChoixObjet),
	choix(ChoixObjet,ObjetChoisi),
	nl,write("Combien veux tu en jeter ? "),nl,
	read(ChoixNb),
	jeter(ObjetChoisi,ChoixNb)
);
(
		nl,write("Tu n'as rien a jeter"),nl,!
).

rienAJeter :- % permet de déterminer s'il n'y a rien a jeter
	retractall(nombre(_)),
	assert(nombre(0)),
	forall(dansInventaire(_,_),
		(nombre(X),
		retractall(nombre(_)),
		X1 is X + 1,
		assert(nombre(X1)))
		),%fin forall
	nombre(Y),
	0 =:= Y.

jeter(Objet, Nb) :-
	dansInventaire(Objet,NbInv),
	((equipable(Objet), nl,write("Tu en as besoin pour te defendre, tu ne peux pas jeter ca ! "),nl,!,fail);true),
	je_suis_a(Ici),
	retirerInv(Objet,Nb),
	((Objet == oeuf, retract(compteur(Objet,_))),!;true),
	X is NbInv - Nb,
	(
	(
		(
			X > 0,
			(nl,write("Tu jetes "),write(Nb),write(" "),
			((Nb > 1, nomPluriel(Objet));(Nb =:= 1, write(Objet))),nl,
			(
				estSur(Objet,Ici,NbObjIci),
				Y is NbObjIci + X,
				retract(estSur(Objet,Ici,NbObjIci)),
				assert(estSur(Objet,Ici,Y)),!
			);
			(
				assert(estSur(Objet,Ici,Nb)),!
			))
		);
		(
			X =< 0,
			(
				nl,write("Tu n'en as que "), write(NbInv), write(". Tu jetes "),write(NbInv),write(" "),
				((NbInv > 1, nomPluriel(Objet));(NbInv =:= 1, write(Objet))),nl,
				(
					estSur(Objet,Ici,NbObjIci),
					Y is NbObjIci + NbInv,
					retract(estSur(Objet,Ici,_)),
					assert(estSur(Objet,Ici,Y)),!
				);
				(
					assert(estSur(Objet,Ici,NbInv)),!
				)
			)
		);
		write("Tu ne peux pas jeter ca "),attendre(1)
	); write("Tu ne peux pas jeter ca "),attendre(1)
	).

ramasser :-
	(
		findall(X,(je_suis_a(Ici),estSur(X,Ici,_),ramassable(X)),L),
		length(L,Length),
		Length =:= 0,
		nl,write("Il n'y a rien a ramasser ici. "),nl,attendre(1),!
	);
	(
		forall((je_suis_a(Ici),estSur(X,Ici,_),ramassable(X)),
		(
			ramasser(X)
		)
		)%finforall
	).

ramasser(recetteBateau) :-
(
	je_suis_a(Ici),estSur(recetteBateau,Ici,1),
	(
		not(dansListeRecettes(bateau)), 
		assert(dansListeRecettes(bateau)),
		retractall(estSur(recetteBateau,Ici,_)),
		nl,
		write("Tiens, le papier ressemble a un manuel de fabrication de bateau. Tu sais maintenant comment fabriquer un bateau !"),
		write(" Il y a peut-etre un espoir de s'enfuir d'ici ..."),nl,nl, 
		write("> pour consulter la liste des recettes a ta disposition, utilise 'listeRecettes' ;"), nl,
		write("> pour voir le mode d'emploi, utilise 'mode_emploi.' "), !
	);
	(
		dansListeRecettes(bateau), 
		nl, write("Oh ? Tu connaissais deja cette recette ?")
	)
).

ramasser(Objet) :-
	(
		je_suis_a(Ici),
		estSur(Objet,Ici,Nb),
		ramassable(Objet)
	),
	(
		(Objet == oeuf, assert(compteur(oeuf,60)),!);true
	),
	(
		(
			nl,write("Tu ramasses "),
			(Nb =:= 1, nomIndefini(Objet));
			(Nb > 1, write(Nb), write(' '), nomPluriel(Objet))
			,nl
		),
		retract(estSur(Objet,Ici,_)),
		(
			dansInventaire(Objet,ExNb),
			NewNb is ExNb + Nb,
			retractall(dansInventaire(Objet,_)),
			assert(dansInventaire(Objet,NewNb))
		);
		(
			assert(dansInventaire(Objet,Nb))
		)
	).

ramasser(X) :- vivant(X), nl, write("Faut pas vendre la peau du boeuf avant de l'avoir vole... Tu ne vas quand meme pas ramasser un truc vivant ! "), nl, !, fail.

ramasser(_) :- nl, write("Tu ne peux pas ramasser ca ! "), nl, fail.


/* ______________________________________________________________________________________ */
/* Gestion du craft et des recettes  */


/* ATTENTION : (exemple du bateau)
- on fait ramasser(recetteBateau) pour l'objet mais c'est bien dansListeRecettes(bateau).
- Il faut declarer ramasser(recetteBateau) et ramassable(recetteBateau) !! */

recette(bateau).
recette(hache).
recette(lance).
recette(feu).

dansRecette(bateau, branche, 20).
dansRecette(bateau, buche, 10).
dansRecette(bateau, silex, 10).
dansRecette(bateau, toile, 7).

dansRecette(hache, branche, 2).
dansRecette(hache, silex, 3).

dansRecette(lance, branche, 2).
dansRecette(lance, silex, 2).

dansRecette(feu,branche,3).
dansRecette(feu,buche,1).
dansRecette(feu,silex,1).

dansListeRecettes(hache).
dansListeRecettes(lance).
dansListeRecettes(feu).

listeRecettesVide:-
	retractall(nombre(_)),
	assert(nombre(0)),
	forall(dansListeRecettes(X),
		(nombre(X),
		retractall(nombre(_)),
		X1 is X + 1,
		assert(nombre(X1)))),
	nombre(Y),
	0 =:= Y.

listeRecettes :-
	(
		listeRecettesVide, nl, write("Tiens, tu ne connais pas de recettes pour le moment. C'est etrange parce que les developpeurs ont normalement mis d'office au moins une hache et une lance... ils doivent avoir oublie de relancer SWI-Prolog."), nl, nl, !
	) ;
	(
		nl, write(" Tu connais des recettes pour faire : "), nl,
		forall(dansListeRecettes(X),(nomIndefini(X), nl)),
		nl
	).

recetteVide(Objet):-
	retractall(nombre(_)),
	assert(nombre(0)),
	forall(dansRecette(Objet,_,_),
		(nombre(X),
		retractall(nombre(_)),
		X1 is X + 1,
		assert(nombre(X1)))),
	nombre(Y),
	0 =:= Y.

consulterRecette(Objet):- 
	recette(Objet), not(dansListeRecettes(Objet)),
	nl, write("Cette recette t'es inconnue ! "), nl, !.

consulterRecette(Objet):- 
	recette(Objet), 
	recetteVide(Objet), 
	nl, write("les dev ont oublie de mettre des ingredients pour cette recette "),nl, !.

consulterRecette(Objet):- 
	recette(Objet),
	nl,write("Tu prends le temps de consulter une recette : "), nl,nl,
	write("Il te faut "), 
	forall(dansRecette(Objet,Element,Nb), 
		(
			(
			(Nb =:= 1, nomIndefini(Element));
			(Nb > 1, write(Nb), write(' '), nomPluriel(Element))
			), 
		write(", ")
	)), nl,nl,nl,write("Ici, "),attendre(1), !.
	
consulterRecette(_):- nl, write("ce n'est pas une recette ... "),nl, !.


ressourcesSuffisantes(Objet,Nb):- 
	dansInventaire(Objet, X), NbManquant is Nb-X, NbManquant=<0.

ressourcesSuffisantes(Objet):-
	(
	( % c'est pas une recette
		not(recette(Objet)), write("erreur : ce n'est pas une recette "), nl,
		fail, !
	) ;
	( % c'est une recette
		forall(dansRecette(Objet,Element,Nb),
			ressourcesSuffisantes(Element,Nb)
		)% fin forall
	)
	).

craft(araignee):- nl, write("c'est malheureusement impossible... "), nl, !.

craft(bateau) :- 
	not(dansListeRecettes(bateau)), 
	write("C'est la porte ouverte à toutes les fenêtres ! A moins que tu ne l'ai appris dans une vie anterieure (?) tu ne sais pas comment on fabrique un bateau... "), !.

craft(Objet) :- 
	recette(Objet), not(dansListeRecettes(Objet)), write("Tu ne connais pas encore cette recette. "), !.
	
craft(Objet):- 
	recette(Objet), dansListeRecettes(Objet), 
	not(ressourcesSuffisantes(Objet)), nl, 
	write("Faut pas vendre la peau du boeuf avant de l'avoir vole ! "),nl,
	write("Il te manque des elements pour fabriquer "), 
	nomIndefini(Objet), write(" ! "), 
	nl, 
	write("> pour consulter la liste des recettes a ta disposition, utilise 'listeRecettes' ;"), nl,
	write("> pour verifier ton inventaire, utilise la commande 'inventaire.' "),nl,
	write("> pour voir le mode d'emploi, utilise 'mode_emploi.' "),
	nl, nl, write("Ici, "), attendre(1), !.

craft(Objet):- not(recette(Objet)), nl, write("pas de recette pour cet objet ! "), nl,nl, 
	write("> pour consulter la liste des recettes a ta disposition, utilise 'listeRecettes' ;"), nl,
	write("> pour voir le mode d'emploi, utilise 'mode_emploi.' "), attendre(1), !.

craft(Objet):- 
	(Objet == feu, je_suis_a(Ici),estSur(feu,Ici),nl,write("il y a deja un feu ici. "),nl,!);
	recette(Objet), dansListeRecettes(Objet), 
	ressourcesSuffisantes(Objet),
	appliquerCraft(Objet),
	(
	(Objet == feu, allumerFeu,!);
	(Objet == bateau, bateauConstruit, !)
	);
	(
		(
			dansInventaire(Objet,X),
			Y is X + 1,
			((equipable(Objet),equiper(Objet),!);true),
			retract(dansInventaire(Objet,X)),
			assert(dansInventaire(Objet,Y))
		);
		(
			((equipable(Objet),equiper(Objet),!);true),
			assert(dansInventaire(Objet,1))
		)
	),
	nl, write("Tu as reussi a craft "), nomIndefini(Objet), nl, attendre(2), !.
	
craft(Objet):- 
	recette(Objet), nl, write("Il semble qu'il y a un probleme avec le craft de cette recette "), !.

craft(_):- nl, write("impossible de craft cet objet ! "), !.

appliquerCraft(Objet) :- 
	forall(dansRecette(Objet,Element,Nb),
			retirerInv(Element,Nb)
		)% fin forall
		.

bateauConstruit :-
	nl,nl,nl,nl,
	write("Il est fier ton navire, il est beau ton bateau ! C'est un fameux trois m... radeau, fin comme un oiseau ! Tu peux enfin fuir cette ile de malheur et rejoindre la civilisation. Alors que tu met les voiles vers l'horizon, tu te demande quelle sera ta prochaine aventure, avant d'enfin rentrer chez toi..."),nl,nl,
	write("Tu as gagné la partie ! tape la commande 'halt.' pour quitter le jeu."),nl,abort.
		
allumerFeu :-
	nl,write("Tu allumes un feu, te voila en securite pour la nuit "),nl,
	je_suis_a(Ici),
	assert(estSur(feu,Ici)).

rienACuire :- % permet de déterminer s'il n'y a rien a cuire
	retractall(nombre(_)),
	assert(nombre(0)),
	forall((
		dansInventaire(Objet,_),
		(cuisinable(Objet,_);brulable(Objet))
		),
		(nombre(X),
		retractall(nombre(_)),
		X1 is X + 1,
		assert(nombre(X1)))
		),%fin forall
	nombre(Y),
	0 =:= Y.

cuire :-
	(
		rienACuire, write("Tu n'as rien a cuire...'"), nl, !
	);
	(
		findall(Objet,
		(
			dansInventaire(Objet,_),
			(cuisinable(Objet,_);brulable(Objet))
		),LObjet),
		retractall(nombre(_)),
		retractall(choix(_,_)),
		assert(nombre(0)),
		nl,write("Que veux tu cuire ? (entre le numero de ton choix) "),nl,
		forall(dansListe(Objet,LObjet),
		(
			nl,
			nombre(X),
			retractall(nombre(X)),
			Y is X + 1,
			assert(nombre(Y)),
			assert(choix(Y,Objet)),
			write(Y),write(" : "),nomIndefini(Objet)
		)),nl,nl,
		read(ChoixCuire),
		choix(ChoixCuire,ObjetACuire),
		cuire(ObjetACuire)
	).
	
cuire(Objet) :-
	(
		je_suis_a(Ici),estSur(feu,Ici),
		dansInventaire(Objet,_),
		retirerInv(Objet,1),
		(
			cuisinable(Objet,ObjetCuit),
			nl,nomDefini(Objet),write(" cuit sur le feu. Tu obtiens "),nomIndefini(ObjetCuit),write("."),
			((
				dansInventaire(ObjetCuit,Nb),
				retract(dansInventaire(ObjetCuit,Nb)),
				NvNb is Nb+1,
				assert(dansInventaire(ObjetCuit,NvNb)),!
			);
			(

				not(dansInventaire(ObjetCuit,_)),
				assert(dansInventaire(ObjetCuit,1))
			)),!
		);
		(
			brulable(Objet),
			nl,nomDefini(Objet),write(" brule. Tu t'attendais a quoi ?"),nl
		),!
	),attendre(4);
	(
		je_suis_a(Ici),not(estSur(feu,Ici)),
		nl,write("Tu ne peux rien cuire sans feu..."),nl,!,fail
	);
	(
		nl,write("Tu ne peux pas cuire ca !"),nl,fail
	).
	
% par facilite, tous les objets equipables dans ce jeu augmentent la puissance de combat du joueur de 2
equiper(Objet) :-
	(
		dansInventaire(Objet,_),
		nl,write("Tu a deja "),nomIndefini(Objet),write(" dans ton inventaire. En avoir plus ne change rien..."),nl,!
	);
	(
		not(dansInventaire(Objet,_)),
		nl,write("Tu equipes "),nomIndefini(Objet),write(". Ta puissance de combat augmente de 2."),nl,
		combat(moi,X),
		Y is X + 2,
		retract(combat(moi,X)),
		assert(combat(moi,Y))
	).

ouvrir :-
	(
		je_suis_a(Ici),
		not(estSur(chester,Ici)),
		decrire(Ici),
		nl, write("... je ne sais pas quoi ouvrir. "),nl, 
		write("> pour voir le mode d'emploi, utilise 'mode_emploi.' "),nl,
		!,fail
	);
	(
		je_suis_a(Ici),
		estSur(chester,Ici),
		nl, write("Apres lui avoir gentiement gratouille le dessus de la tete, tu ouvres Chester."),nl,
		(
			(
				not(estSur(recetteBateau,Ici,_)),not(dansListeRecettes(bateau)),
				assert(estSur(recetteBateau,Ici,1)),
				nl, write("Il depose un bout de papier a tes pieds. Il a l'air content."), nl, attendre(1), !
			);
			(
				estSur(recetteBateau,Ici,_),
				nl,write("Il est vide."),nl,write("Mais tu peux continuer a lui gratouiller la tete : il adore ca !"),
				nl, write("Oh ? Il y a un papier au sol..."), nl, attendre(1), !, fail
			);
			(
				dansListeRecettes(bateau),
				nl, write("Il t'a deja donne une super recette ! Ne lui en demande pas trop..."), nl, attendre(1), !, fail
			)
		),
		!
	).

couper :- 
	(
		je_suis_a(Ici),
		forall(estSur(Objet,Ici),
			not(arbre(Objet))
		),% fin forall
		decrire(Ici),
		nl, write("... je ne sais pas quoi couper. "),nl, 
		write("> pour voir le mode d'emploi, utilise 'mode_emploi.' "),nl,
		attendre(1),
		!,fail
	);
	(
		dansInventaire(hache,_),
		je_suis_a(Ici),
		estSur(Objet,Ici),
		arbre(Objet), 
		nl,
		(
			(% soit on coupe l'arbre
				not(Objet == arbre5),
				write("Autour de l'arbre fraichement coupe, tu trouves..."),nl,
					forall(drop(Objet,NomDrop,Nb),
						(
							write('- '),
							(
								(Nb =:= 1, nomIndefini(NomDrop));
								(Nb > 1, write(Nb), write(' '), nomPluriel(NomDrop))	
							),
							deposeDrop(Objet,NomDrop,Nb),nl
						)
					),% fin forall
				nl, nl, retract(estSur(Objet,Ici)),
				attendre(4),
				!
			);
			(% soit l'arbre se defend
				Objet == arbre5,
				write("L'arbre change. C'est maintenant une grande figure semblable a un homme, presque un troll, d'au moins quatorze pieds de haut, tres robuste, avec une tete haute et a peine un cou."),nl,
				write("Il est difficile de dire s'il est vetu d'etoffes vertes et grises comme de l'ecorce - ou si c'est sa peau. "),nl,
				write("En tout cas, ses bras, a une courte distance du tronc, ne sont pas froisses mais couverts d'une peau brune et lisse."),nl,
				write("Ses grands pieds ont sept orteils chacun. La partie inferieure de sa longue face est couverte d'une barbe grise et grumeleuse, touffue, presque tordue aux racines, fine et moussue aux extremites."),nl,
				nl,nl, write("Mais en ce moment, tu ne remarques que les yeux. Ces yeux profonds qui t'examinent, lents et solennels, mais tres penetrants."),nl,
				nl, nl, write("Il attaque."),nl,
				attaquer(arbre5),
				retractall(estSur(arbre5,_)),
				attendre(4),
				!
			)
		)
	);
	(
		not(dansInventaire(hache,_)),
		write("Les fils de ta barbe couperont difficilement autre chose que du beurre. "),nl, 
		write("> pour consulter la liste des recettes a ta disposition, utilise 'listeRecettes' ;"), nl,
		write("> pour voir le mode d'emploi, utilise 'mode_emploi.' "),nl,
		attendre(1)
	).

/* ______________________________________________________________________________________ */
/* Observation, description, interface utilisateur  */


decrire(Case) :- 
	(
		dansleau(Case), 
		write("c'est de l'eau'"), nl, !
	) ; 
	(
		caseVide(Case), 
		write("on dirait qu'il n'y a rien d'interessant."), nl, !
	) ; 
	(
	write("tu peux voir :"),nl,
	forall(
			estSur(Objet,Case),
			(Objet == echassier1,decrireEchassier);
			(Objet == chester,decrireChester);
			(chien(Objet),decrireChien(Objet));
			(write('* '),nomIndefini(Objet),nl)
		),
	forall(
			estSur(Objet,Case,X),
			(X =:= 1, write('* '), nomIndefini(Objet),nl);
			(X > 1, write('* '),write(X),write(' '),nomPluriel(Objet),nl)
		),!
	).

decrireChien(Chien) :-
	(
		je_suis_a(Ici),
		estSur(Chien,Ici),
		assert(compteur(Chien,5)),
		write("* Un chien. Il te regarde en grognant, lui tourner le dos semble une très mauvaise idée..."),nl,!
	);
	(
		je_suis_a(Ici),
		not(estSur(Chien,Ici)),
		write('* '), nomIndefini(Chien),nl
	).
	
decrireChester :-
	(
		je_suis_a(Ici), 
		estSur(chester,Ici),
		write("* Chester. C'est un adorable coffre poilu pourvu de pattes. Tu peux peut-etre l'ouvrir... "), nl, !
	);
	(
		je_suis_a(Ici), 
		not(estSur(chester,Ici)),
		write("* "), nomIndefini(chester), nl
	).
	
decrireEchassier :-
	(
		je_suis_a(Ici),
		estSur(echassier1,Ici),
		write('* '), nomIndefini(echassier1),nl
	);
	(
		je_suis_a(Ici),
		not(estSur(echassier1,Ici)),
		write("* Un grand oiseau. Il te regarde avec un air mechant, et semble pret a t'attaquer si tu t'approche. On dirait qu'il protege quelquechose ? Sois prudent, l'animal fait au moins 2 metres de haut, un combat contre lui risque de ne pas etre une partie de plaisir..."),nl,!
	).

caseVide(Case) :- % permet de déterminer si une case est vide
	retractall(nombre(_)),
	assert(nombre(0)),
	forall(estSur(_,Case),
		(nombre(X),
		retractall(nombre(_)),
		X1 is X + 1,
		assert(nombre(X1)))),
	forall(estSur(_,Case,Nb),
		(nombre(X),
		retractall(nombre(_)),
		X1 is X + Nb,
		assert(nombre(X1)))),
	nombre(Y),
	0 =:= Y.

regarder :-
		nl,
		je_suis_a(Ici),
		write('A proximite immediate, '),decrire(Ici),nl,
		
		write('Au nord, '),
		(
		chemin(Ici, n, AuNord),decrire(AuNord);
		write('de l''eau...'),nl
		),
		nl,write('Au sud, '),
		(
		chemin(Ici, s, AuSud),decrire(AuSud);
		write('de l''eau...'),nl
		),
		nl,write('A l''est, '),
		(
		chemin(Ici, e, AlEst),decrire(AlEst);
		write('de l''eau...'),nl
		),
		nl,write('A l''ouest, '),
		(
		chemin(Ici, o, AlOuest),decrire(AlOuest);
		write('de l''eau...'),nl
		),
		nl,write('Que fais tu ?'),
		nl, write("> pour avoir une idee de la carte du monde, utilise 'afficherMap.' ;"), nl, 
		write("> pour voir le mode d'emploi, utilise 'mode_emploi.' "),!.
		

/* ______________________________________________________________________________________ */
/* Gestion de la map */


/* Les faits et prédicats suivants permettent de modéliser et de se déplacer sur l'ile sur laquelle se passe ce jeu. L'ile est un rectangle de 12*12 cases, dont certaines sont considérées comme dans l'eau. */


dansleau(4).
dansleau(5).
dansleau(6).
dansleau(7).
dansleau(18).
dansleau(19).
dansleau(47).
dansleau(48).
dansleau(59).
dansleau(60).
dansleau(97).
dansleau(109).
dansleau(121).
dansleau(124).
dansleau(125).
dansleau(126).
dansleau(132).
dansleau(133).
dansleau(136).
dansleau(137).
dansleau(138).
dansleau(144).

alest(A,B) :- A > 0,
	(B is A+1),
	B =< 144,
	(not(0 =:= A mod 12)). % B est à l'est de A
	
alouest(A,B) :- A =< 144,
	(B is A-1),

	B > 0,
	(not(1 =:= A mod 12)). % B est à l'ouest de A

ausud(A,B) :- A > 0,
	(B is A+12),
	B =< 144. % B est au sud de A
	
aunord(A,B) :- A =< 144,
	(B is A-12),
	B > 0. % B est au nord de A
	
adjacentes(A,B) :- 
	(ausud(A,B);alest(A,B);aunord(A,B);alouest(A,B)),
	not(dansleau(B)).

/* Règle pour se déplacer dans une direction donnée */


chemin(A,X,B):-(
				(s == X, ausud(A,B)); %B au sud de A
				(e == X, alest(A,B));
				(n == X, aunord(A,B));
				(o == X, alouest(A,B))
				),not(dansleau(B)). 

/* Directions pour aller/1. */
		
n :- aller(nord), !.
s :- aller(sud), !.
e :- aller(est), !.
o :- aller(ouest), !.
aller(nord) :- aller(n), !.
aller(sud) :- aller(s), !.
aller(est) :- aller(e), !.
aller(ouest) :- aller(o), !.
				
aller(Direction) :-
		(
			tempsRestant(X), X=<5, 
			nl, write("La nuit arrive bien vite... Tu n'as meme pas le temps de bouger."),
			nl, write("Autant t'installer et dormir ici."),
			nl, write("> pour attendre X unites de temps, utilise 'attendre(X).' ;"), nl,
			nl,!
		);
      (
      	je_suis_a(Ici),
      	chemin(Ici, Direction, Labas),
      	retract(je_suis_a(Ici)),
      	assert(je_suis_a(Labas)),
      	attendre(5),
		((estSur(echassier1,Labas),combattreEchassier);true),
		nl,!
		);
		( 
			n == Direction,
			write("Impossible d'aller par la !"),nl,
			write("Au nord, des vagues t'empechent d'aller plus loin..."),nl,!
		);
		(
			s == Direction,
			write("Impossible d'aller par la !"),nl,
			write("Au sud, des vagues t'empechent d'aller plus loin..."),nl,!
		);
		(
			e == Direction,
			write("Impossible d'aller par la !"),nl,
			write("A l'est, des vagues t'empechent d'aller plus loin..."),nl,!
		);
		(
			o == Direction,
			write("Impossible d'aller par la !"),nl,
			write("'A l'ouest, des vagues t'empechent d'aller plus loin..."),nl,!
		);
		nl, write('Il semble que ce deplacement est impossible. '), nl.
		

randomDirection(Direction):-
random(1,5,Number),
		(
		(Number is 1, n = Direction);
		(Number is 2, e = Direction);
		(Number is 3, s = Direction);
		(Number is 4, o = Direction)
		).
	
deplacer(Objet) :-
		(
			estSur(Objet,Position),
			randomDirection(Direction),
			chemin(Position,Direction,Labas),
		     retract(estSur(Objet,Position)),
		     assert(estSur(Objet,Labas)),!
		); deplacer(Objet).


% dans le cas (moderement improbable) ou une araignee se deplace sur la case de l'echassier en meme temps que le joueur, le joueur se retrouve inevitablement en combat contre deux adversaires (puisque le combat est declenche automatiquement). Le joueur meurt donc dans ce cas, quoiqu'il arrive. On decide de conserver cet aspect, bien que considerable comme injuste, car il reste dans l'esprit du jeu don't starve.
combattreEchassier :-
	nl,write("Le grand oiseau a l'air tres agressif, et ne perd pas une seconde pour t'attaquer. Tu n'as plus le temps de fuir, le combat est inevitable. "),nl,
	attaquer(echassier1),nl,!.

afficherLigneVagues:- write("[~][~][~][~][~][~][~][~][~][~][~][~][~][~]").
afficherLigneGrossesVagues:- write("[ ~ ][ ~ ][ ~ ][ ~ ][ ~ ][ ~ ][ ~ ][ ~ ][ ~ ][ ~ ][ ~ ][ ~ ][ ~ ][ ~ ]").


afficherCaseMapBase(Case):- ( je_suis_a(Case), write("[X]"), ! ) ; ( dansleau(Case), write("[~]"), !) ; write("[ ]").

afficherCaseCarteTresor(Case,Carte):- ( carte(Carte,Case), write("[X]"), ! ) ; ( dansleau(Case), write("[~]"), !) ; write("[ ]").

% ce predicat a servi a tester si le deplacement des araignees fonctionne, et ne sera pas utilise dans le jeu tel quel
afficherCaseMapWithAraignees(Case):- 
( je_suis_a(Case), write("[X]"), !) ; 
( estSur(araignee1, Case), write("[1]"), !) ; 
( estSur(araignee2, Case), write("[2]"), !) ; 
( dansleau(Case), write("[~]")) ; 
write("[ ]").

afficherCaseMapNumerotee(Case):- 
( 
	je_suis_a(Case), write("[ X ]"), !
) ; 
( 
	dansleau(Case), write("[ ~ ]"), !
) ; 
( 
	Case < 10, write("[ "), write(Case), write(" ]"), !
) ;
( 
	Case < 100, write("[ "), write(Case), write("]"), !
) ;
(	
	write("["), write(Case), write("]") 
).

			
afficherMap :- 
			nl, 
			nl, write("Voici une carte du monde pour t'eclairer"),
			nl, write("La croix indique ta position. Les vagues indiquent... la mer."),
			nl,afficherLigneVagues,
			nl, write("[~]"),foreach(between(1,12,X), afficherCaseMapBase(X) ),write("[~]"),
			nl, write("[~]"),foreach(between(13,24,X), afficherCaseMapBase(X) ),write("[~]"),
			nl, write("[~]"),foreach(between(25,36,X), afficherCaseMapBase(X) ),write("[~]"),
			nl, write("[~]"),foreach(between(37,48,X), afficherCaseMapBase(X) ),write("[~]"),
			nl, write("[~]"),foreach(between(49,60,X), afficherCaseMapBase(X) ),write("[~]"),
			nl, write("[~]"),foreach(between(61,72,X), afficherCaseMapBase(X) ),write("[~]"),
			nl, write("[~]"),foreach(between(73,84,X), afficherCaseMapBase(X) ),write("[~]"),
			nl, write("[~]"),foreach(between(85,96,X), afficherCaseMapBase(X) ),write("[~]"),
			nl, write("[~]"),foreach(between(97,108,X), afficherCaseMapBase(X) ),write("[~]"),
			nl, write("[~]"),foreach(between(109,120,X), afficherCaseMapBase(X) ),write("[~]"),
			nl, write("[~]"),foreach(between(121,132,X), afficherCaseMapBase(X) ),write("[~]"),
			nl, write("[~]"),foreach(between(133,144,X), afficherCaseMapBase(X) ),write("[~]"),
			nl,afficherLigneVagues.		

pasDeCarte :- % permet de déterminer si le joueur ne possede pas de carte au trésor a consulter
	retractall(nombre(_)),
	assert(nombre(0)),
	forall((
		dansInventaire(Objet,_),
		carte(Objet,_)
		),
		(nombre(X),
		retractall(nombre(_)),
		X1 is X + 1,
		assert(nombre(X1)))
		),%fin forall
	nombre(Y),
	0 =:= Y.

consulterCarte :-
	(
		pasDeCarte, write("Tu n'as pas de carte a consulter...'"), nl, !
	);
	(
		findall(Objet,
		(
			dansInventaire(Objet,_),
			carte(Objet,_)
		),LObjet),
		retractall(nombre(_)),
		retractall(choix(_,_)),
		assert(nombre(0)),
		nl,write("Quelle carte veux tu consulter ? (entre le numero de ton choix) "),nl,
		forall(dansListe(Objet,LObjet),
		(
			nl,
			nombre(X),
			retractall(nombre(X)),
			Y is X + 1,
			assert(nombre(Y)),
			assert(choix(Y,Objet)),
			write(Y),write(" : "),nomDefini(Objet)
		)),nl,nl,
		read(ChoixCuire),
		choix(ChoixCuire,ObjetACuire),
		afficherCarteAuTresor(ObjetACuire)
	).
	

afficherCarteAuTresor(Carte) :- 
			nl, 
			nl, write("Dans le coin de la carte, il est ecrit '"),nomDefini(Carte),write("'"),
			nl, write("La carte semble representer l'ile. Une petite croix est dessinee dessus. Que peut-il bien y avoir la-bas ?"),
			nl,afficherLigneVagues,
			nl, write("[~]"),foreach(between(1,12,X), afficherCaseCarteTresor(X,Carte) ),write("[~]"),
			nl, write("[~]"),foreach(between(13,24,X), afficherCaseCarteTresor(X,Carte) ),write("[~]"),
			nl, write("[~]"),foreach(between(25,36,X), afficherCaseCarteTresor(X,Carte) ),write("[~]"),
			nl, write("[~]"),foreach(between(37,48,X), afficherCaseCarteTresor(X,Carte) ),write("[~]"),
			nl, write("[~]"),foreach(between(49,60,X), afficherCaseCarteTresor(X,Carte) ),write("[~]"),
			nl, write("[~]"),foreach(between(61,72,X), afficherCaseCarteTresor(X,Carte) ),write("[~]"),
			nl, write("[~]"),foreach(between(73,84,X), afficherCaseCarteTresor(X,Carte) ),write("[~]"),
			nl, write("[~]"),foreach(between(85,96,X), afficherCaseCarteTresor(X,Carte) ),write("[~]"),
			nl, write("[~]"),foreach(between(97,108,X), afficherCaseCarteTresor(X,Carte) ),write("[~]"),
			nl, write("[~]"),foreach(between(109,120,X), afficherCaseCarteTresor(X,Carte) ),write("[~]"),
			nl, write("[~]"),foreach(between(121,132,X), afficherCaseCarteTresor(X,Carte) ),write("[~]"),
			nl, write("[~]"),foreach(between(133,144,X), afficherCaseCarteTresor(X,Carte) ),write("[~]"),
			nl,afficherLigneVagues.
			
afficherMapNumerotee :- 
			nl, 
			nl, write("Voici une carte du monde pour t'eclairer"),
			nl, write("La croix indique ta position. Les vagues indiquent... la mer."),
			nl,afficherLigneGrossesVagues,
			nl, write("[ ~ ]"),foreach(between(1,12,X), afficherCaseMapNumerotee(X) ),write("[ ~ ]"),
			nl, write("[ ~ ]"),foreach(between(13,24,X), afficherCaseMapNumerotee(X) ),write("[ ~ ]"),
			nl, write("[ ~ ]"),foreach(between(25,36,X), afficherCaseMapNumerotee(X) ),write("[ ~ ]"),
			nl, write("[ ~ ]"),foreach(between(37,48,X), afficherCaseMapNumerotee(X) ),write("[ ~ ]"),
			nl, write("[ ~ ]"),foreach(between(49,60,X), afficherCaseMapNumerotee(X) ),write("[ ~ ]"),
			nl, write("[ ~ ]"),foreach(between(61,72,X), afficherCaseMapNumerotee(X) ),write("[ ~ ]"),
			nl, write("[ ~ ]"),foreach(between(73,84,X), afficherCaseMapNumerotee(X) ),write("[ ~ ]"),
			nl, write("[ ~ ]"),foreach(between(85,96,X), afficherCaseMapNumerotee(X) ),write("[ ~ ]"),
			nl, write("[ ~ ]"),foreach(between(97,108,X), afficherCaseMapNumerotee(X) ),write("[ ~ ]"),
			nl, write("[ ~ ]"),foreach(between(109,120,X), afficherCaseMapNumerotee(X) ),write("[ ~ ]"),
			nl, write("[ ~ ]"),foreach(between(121,132,X), afficherCaseMapNumerotee(X) ),write("[ ~ ]"),
			nl, write("[ ~ ]"),foreach(between(133,144,X), afficherCaseMapNumerotee(X) ),write("[ ~ ]"),
			nl,afficherLigneGrossesVagues.

% ce predicat a servi a tester si le deplacement des araignees fonctionne, et ne sera pas utilise dans le jeu tel quel
afficherMapAraignees :- 
			nl, write("La croix indique votre position. Les vagues indiquent... la mer."),nl,
			nl,afficherLigneVagues,
			nl, write("[~]"),foreach(between(1,12,X), afficherCaseMapWithAraignees(X) ),write("[~]"),
			nl, write("[~]"),foreach(between(13,24,X), afficherCaseMapWithAraignees(X) ),write("[~]"),
			nl, write("[~]"),foreach(between(25,36,X), afficherCaseMapWithAraignees(X) ),write("[~]"),
			nl, write("[~]"),foreach(between(37,48,X), afficherCaseMapWithAraignees(X) ),write("[~]"),
			nl, write("[~]"),foreach(between(49,60,X), afficherCaseMapWithAraignees(X) ),write("[~]"),
			nl, write("[~]"),foreach(between(61,72,X), afficherCaseMapWithAraignees(X) ),write("[~]"),
			nl, write("[~]"),foreach(between(73,84,X), afficherCaseMapWithAraignees(X) ),write("[~]"),
			nl, write("[~]"),foreach(between(85,96,X), afficherCaseMapWithAraignees(X) ),write("[~]"),
			nl, write("[~]"),foreach(between(97,108,X), afficherCaseMapWithAraignees(X) ),write("[~]"),
			nl, write("[~]"),foreach(between(109,120,X), afficherCaseMapWithAraignees(X) ),write("[~]"),
			nl, write("[~]"),foreach(between(121,132,X), afficherCaseMapWithAraignees(X) ),write("[~]"),
			nl, write("[~]"),foreach(between(133,144,X), afficherCaseMapWithAraignees(X) ),write("[~]"),
			nl,afficherLigneVagues.
