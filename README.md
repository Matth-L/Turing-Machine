# Manuel d'utilisation

Le but de ce projet scolaire a été de créer un programme en Ocaml imitant la Machine De Turing. Ce programme peut : 

### Lire une chaine d'instruction composé de : 
- Write 
  - Permet d'écrire un caractère sur le ruban
- Left 
  - Décale la tête de la machine vers la gauche
- Right
  - Décale la tête de la machine vers la droite
- Delete
  - Supprime un caractère de tous le ruban
- Invert
  - Inverse le ruban et positionne la tête au bon endroit.
- Caesar
  - Applique l'encodage de caesar pour les caractères ascii uniquement.


### Encodé un fichier: 
Avec les instructions ci-dessus ce programme est également capable d'encoder des mots afin que ceux-ci prennent le moins d'instruction possible.

## Installation 

Pour utiliser ce programme, il est nécessaire que le language Ocaml soit installé sur votre machine. Voir : 
[Installer Ocaml](https://v2.ocaml.org/docs/install.fr.html)

## Utilisation
Afin d'utiliser ce progamme, il est nécessaire de le compiler, puis de l'éxécuter avec les bon paramètres. 

1. Compiler le programme

```bash
ocamlopt -o exe projet.ml
```
2. Executer le programme

L'éxécutable prend en paramètre 1 ou 2 pour décoder un texte. 
Le paramètre 3 sert à encoder.

```bash
./exe <1|2|3> file
```
## Demo

Décoder un fichier :

```bash
./exe 1 "invert.prog"
# return -> 1abd
```

Encoder un fichier (les fichiers t[1;5].prog sont les différents fichiers de test): 

```bash
./exe 3 "t3.prog"
# F(2,[W(c);R;W(o);R;W(u);R;]);W( );R;W(e);R;W(t);R;W( );R;W(h);R;W(e);R;F(21,[W(l);R;]);W(o);R                                          
```

