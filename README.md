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
| Équipe | CVdata : `supervision`, `assistanat` |
| Projets | CVdata : `subvention` (subventions obtenues uniquement) |
| Cours (masquée par défaut) | CVdata : `teaching_uqtr`, `past_teaching` |
| CV (menu) | `../../01_CV/GVallet_Vitae/docs/GVallet_CV.pdf`, copié par le `.bat` |

Les chemins des sources, la traduction des mots-clés (en français et en anglais), l'affichage des montants et celui de la page Cours (`afficher_cours`) se règlent dans `sources.yml`. Le dossier `_generated/` est produit automatiquement : il ne faut pas le modifier à la main.

- **Photos de l'équipe :** déposer l'image dans `files/profiles/` et inscrire son nom de fichier dans la colonne `photo` de l'onglet `supervision`.
- **Nouveau type de mot-clé :** l'ajouter dans `sources.yml`. Le script signale les mots non traduits.
- **Formulaire de participation :** il est géré par Netlify Forms. Les réponses se consultent dans Netlify, onglet *Forms*, et les notifications par courriel se règlent dans *Project configuration → Notifications*.
- **Version anglaise :** les pages sont dans `en/`, avec le même nom de fichier que la page française. `quarto render` compile le français, puis l'anglais dans `docs/en`. Les titres, programmes et cours restent dans leur langue d'origine.
- **Intranet :** le menu mène à `intranet.html`, dont le formulaire est vérifié par `netlify/edge-functions/intranet.ts` avant de rediriger vers mnesis.ca. Le mot de passe n'est pas dans le dépôt : il se règle dans Netlify, *Project configuration → Environment variables*, variable `INTRANET_MOT_DE_PASSE`. Ce contrôle protège le lien, pas mnesis.ca, qui reste accessible directement.

## Confidentialité

Les fichiers sources restent sur l'ordinateur et sont exclus du dépôt. Seules les colonnes lues dans `scripts/build_data.R` sont publiées. Les onglets `articles`, `jury`, `expertise`, `selection_prof` et `clinique` ne sont pas lus.

À chaque compilation, `scripts/verifier_confidentialite.R` recherche dans le site les titres d'articles évalués, leurs identifiants et les demandes de subvention refusées. S'il en trouve un, la compilation échoue et rien n'est publié.
