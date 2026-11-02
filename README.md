# Site du L3C

Site du Laboratoire Cognition-Corps-Contexte (UQTR), construit avec [Quarto](https://quarto.org) et hébergé sur Netlify.

## Mettre le site à jour

1. Modifier `CVdata.xlsx`, `myarticles.bib` ou `confs.bib`, ou recompiler le CV.
2. Double-cliquer sur `Mettre_a_jour_le_site.bat`.

Le script copie le CV, compile le site, vérifie la confidentialité, puis l'envoie sur GitHub. Netlify le republie ensuite automatiquement. Pour le prévisualiser sans le publier, lancer `quarto preview` dans ce dossier.

## D'où viennent les contenus

| Page | Source |
|---|---|
| Accueil, biographie | Textes dans `index.qmd` et `equipe.qmd` |
| Publications | `myarticles.bib` |
| Conférences | `confs.bib` |
| Équipe | CVdata : `supervision`, `assistanat`, `now`, `education` |
| Projets | CVdata : `subvention` (subventions obtenues uniquement) |
| Cours | CVdata : `teaching_uqtr`, `past_teaching` |
| CV (menu) | `../../01_CV/GVallet_Vitae/docs/GVallet_CV.pdf`, copié par le `.bat` |

Les chemins des sources, la traduction des mots-clés et l'affichage des montants se règlent dans `sources.yml`. Le dossier `_generated/` est produit automatiquement : il ne faut pas le modifier à la main.

- **Photos de l'équipe :** déposer l'image dans `files/profiles/` et inscrire son nom de fichier dans la colonne `photo` de l'onglet `supervision`.
- **Nouveau type de mot-clé :** l'ajouter dans `sources.yml`. Le script signale les mots non traduits.
- **Formulaire de participation :** il est géré par Netlify Forms. Les réponses se consultent dans Netlify, onglet *Forms*, et les notifications par courriel se règlent dans *Project configuration → Notifications*.

## Confidentialité

Les fichiers sources restent sur l'ordinateur et sont exclus du dépôt. Seules les colonnes lues dans `scripts/build_data.R` sont publiées. Les onglets `articles`, `jury`, `expertise`, `selection_prof` et `clinique` ne sont pas lus.

À chaque compilation, `scripts/verifier_confidentialite.R` recherche dans le site les titres d'articles évalués, leurs identifiants et les demandes de subvention refusées. S'il en trouve un, la compilation échoue et rien n'est publié.
