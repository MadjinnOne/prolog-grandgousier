# prolog-grandgousier

Projet Cours AI 2025-2026

## Lancer le bot en local
1. Installez [SWI-Prolog](https://www.swi-prolog.org/).
2. Depuis la racine du dépôt, exécutez `./scripts/run.sh` pour démarrer Grandgousier dans votre terminal.
3. Posez vos questions en français directement dans l'interface SWI-Prolog (par exemple : `produire_reponse("parle moi du nez de chateau guillot", R).`).

## Dépannage : dépôt absent dans le menu déroulant d'un connecteur
Si vous configurez un connecteur (par exemple un connecteur GitHub) et que le dépôt n'apparaît pas dans la liste :
- Vérifiez que vous êtes connecté avec le bon compte GitHub et que le dépôt **prolog-grandgousier** se trouve bien sous cette organisation/compte.
- Assurez-vous d'avoir accepté l'accès de l'application du connecteur au dépôt (souvent via un bouton "Request" ou "Grant" lors de l'installation).
- Réessayez après avoir actualisé la page ou ré-ouvert la fenêtre du connecteur : certains outils mettent la liste à jour uniquement au chargement.
- Si vous travaillez dans une organisation, demandez à un administrateur de confirmer que l'application du connecteur est autorisée pour ce dépôt ou qu'il n'est pas limité par une politique de visibilité privée.
- En dernier recours, spécifiez l'URL du dépôt manuellement si l'outil le permet, ou réinstallez l'application du connecteur pour forcer la prise en compte des autorisations.
