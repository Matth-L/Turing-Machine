
(** 
    Cette partie du programme NE DOIT EN AUCUN CAS être modifiée sous peine
    de voir votre code ne pas passer les tests 
*)
type instruction = (* représentation d'une instruction reçu *)
  | Left (* Déplacement du curseur vers la gauche *)
  | Right (* Déplacement du curseur vers la droite *)
  | Write of char (* Écriture du caractère sur le ruban *)
  | Repeat of (int * instruction list) (* Repeter n fois la liste d'instructions *)
  | Caesar of int (* Caesar n à partir de la phase 3Aiens *)
  | Delete of char (* Delete(a) suppression du caractère a du message à partir de la phase 3Aiens *)
  | Invert (* Invert : retournement du message  à partir de la phase 3Aiens *)
  
type program = instruction list (* Un programme est simplement une liste d'instruction *)

(**
   VOUS N'AVEZ PAS BESOIN DE LIRE OU DE COMPRENDRE CETTE PARTIE DU CODE (LES SIX FONCTIONS SUIVANTES). 
   ELLE EST VOLONTAIREMENT NON COMMENTÉE ET RÉDIGÉE DE MANIÈRE PEU CLAIRE 
 *)
let rec analyse_comment fd =
  let c = Scanf.bscanf fd "%c" (fun c -> c) in
  if c = '\n'
  then ()
  else analyse_comment fd
             
let rec analyse_program_aux =
  fun fd lvl  ->
  try 
    let c = Scanf.bscanf fd "%c" (fun a  -> a) in
    if c = '#'
    then
      let _ = analyse_comment fd in
      analyse_program_aux fd lvl
    else if c = ';' || c = '\n' || c = ' '
    then analyse_program_aux fd lvl
    else if c = ']'
    then
      if lvl > 0
      then []
      else raise (Invalid_argument "Error on char ]")
    else if c = 'W'
      then
        let i= Write(Scanf.bscanf fd "(%c)" (fun a -> a)) in
        let li = analyse_program_aux fd lvl in
        i::li
    else if c = 'C'
      then
        let i= Caesar(Scanf.bscanf fd "(%d)" (fun a -> a)) in
        let li = analyse_program_aux fd lvl in
        i::li
    else if c = 'D'
    then
      let a = Scanf.bscanf fd "(%c)" (fun a -> a) in 
      let i= Delete(a) in
      let li = analyse_program_aux fd lvl in
      i::li
    else if c = 'R'
    then let li = analyse_program_aux fd lvl in
         Right::li
    else if c = 'I'
    then Invert::analyse_program_aux fd lvl
    else if c = 'L'
    then Left::analyse_program_aux fd lvl
    else if c = 'F'
    then
      let n = Scanf.bscanf fd "(%d,[" (fun n -> n) in
      let l = analyse_program_aux fd (lvl + 1) in
      let c = Scanf.bscanf fd "%c" (fun a -> a) in
      if c <> ')' then raise (Invalid_argument ("Error found '"^String.make 1 c^"' instead of ')'"))
      else
        let li = analyse_program_aux fd lvl in
        Repeat(n,l)::li
    else
      let _ = Format.printf  "ERROR '%c' (%d)@." c (Char.code c) in
      assert false
  with End_of_file -> []

let rec read_file_aux =
  fun acc fd ->
  try
    let c = Scanf.bscanf fd "%c" (fun x -> x) in
    read_file_aux (c::acc) fd
  with End_of_file -> acc

let read_file file =
  try
    if Sys.is_directory file
    then raise (Invalid_argument "is a directory")
    else
      let fd = Scanf.Scanning.open_in file in 
      List.rev (read_file_aux [] fd)
  with exc ->
    Format.fprintf Format.err_formatter "Problème à l'ouverture du fichier %s (exception %s)@." file (Printexc.to_string exc);
    exit 127

                    
let rec fprintf_instruction fmt i =
  match i with
  | Write c -> Format.fprintf fmt "W(%c)" c
  | Right -> Format.fprintf fmt "R"
  | Left -> Format.fprintf fmt "L"
  | Repeat(n,li) ->
     Format.fprintf fmt "F(%d,[%a])" n (fun fmt li -> List.iter (fun i -> Format.fprintf fmt "%a;" fprintf_instruction i) li) li
  | Caesar n -> Format.fprintf fmt "C(%d)" n
  | Delete(a) -> Format.fprintf fmt "D(%c)" a
  | Invert -> Format.fprintf fmt "I"
            
let fprintf_program fmt l =
  List.iter (fun i -> Format.fprintf fmt "%a;" fprintf_instruction i) l

(*** 
     Retour au côté clair
*)


(* 
   [print_program] : program -> unit 
   affiche un [program] sur la sortie standard de l'executable
 *)  
let print_program p = Format.printf "%a" fprintf_program p

(*
  [analyse_program] : unit -> program
  Lit un programme 1Aien ou 2Aien, l'analyse et le retourne sous forme d'une valeur de type [program]
 *)                    
let analyse_program file =
  try
    if Sys.is_directory file
    then raise (Invalid_argument "is a directory")
    else
      let fd = Scanf.Scanning.open_in file in 
      analyse_program_aux fd 0
  with exc ->
    Format.fprintf Format.err_formatter "Problème à l'ouverture du fichier %s (exception %s)@." file (Printexc.to_string exc);
    exit 127









    
(** Votre code doit commencer à partir de ce point.

    NE MODIFIER RIEN AU DESSUS DE CE POINT 
*)

(*********************************************************
1.Définir le type ruban

type : couple de liste de char et un char
@requires 2 liste et un char ('' ne compte pas comme un char)
donc si '' -> ne compile pas
@ensures création d'un ruban
*********************************************************)

type ruban = Ruban of (char list * char * char list);;

(*********************************************************
2.Implanter la fonction execute_program : program -> ruban
qui prend en argument un programme permettant le décodage 
du message

Un program est une liste d'instruction

ex :
En supposant un ruban vide 
[Write 'a'; Right; Write 'b'; Left; Write 'c'; Right; Right]

+ ([]   ,  a ,[]   ) 
+ ([a]  ,    ,[]   ) ->
+ ([a]  ,  b ,[]   )
+ ([]   ,  a ,[b]  ) <- 
+ ([]   ,  c ,[b]  )
+ ([c]  ,  b ,[]   ) ->
+ ([c;b],    ,[]   ) ->
*********************************************************)

(*********************************************************
fonction pushLastPosition :
-> ajoute un élément à la fin d'une liste

type : 'a -> 'a list -> 'a list
@requires une liste et un élément
@ensures une liste avec l'élément ajouté à la fin
*********************************************************)
let pushLastPosition n list = 
  List.fold_right (fun e acc -> e::acc) 
                  list 
                  (n::[]);;

(*********************************************************
fonction listWithoutLastElement :
-> supprime le dernier élément d'une liste

type : 'a list -> 'a list
@requires la liste doit être non vide
@ensures une liste sans le dernier élément
@raises Une erreur si on insère une liste vide
************************************************************)
let rec listWithoutLastElement list = 
  match list with
  |[] -> failwith "listWithoutLastElement : La liste ne doit pas être vide"
  |_::[] -> []
  |a::b -> a::(listWithoutLastElement b);;

(*********************************************************
fonction lastElement :
-> retourne le dernier élément d'une liste

On peut utiliser la fonction List.rev pour inverser une liste 
puis prendre le premier élément de la liste

type : 'a list -> 'a
@requires la liste doit être non vide
@ensures Le dernier élément de la liste
@raises Une erreur si on insère une liste vide
*********************************************************)
let rec lastElement list = match List.rev (list) with
|[] -> failwith "la liste ne doit pas être vide"
|a::b -> a;;

(*********************************************************
fonction writeRuban :
-> écrit un char sur le ruban

type : char -> ruban -> ruban
@requires le ruban doit être du type ruban
@ensures un ruban modifié avec le char écrit
*********************************************************)
let writeRuban c ruban = 
  match ruban with
  |Ruban(l1,_,l2) -> Ruban(l1,c,l2);;

(*********************************************************
fonction rightRuban :
-> déplace le curseur du ruban vers la droite

Si on va à droite on prend le premier élément de la 
liste de droite

+ ([a;b;c],d,[e;f;g]) -> ([a;b;c;d],e,[f;g]

type : ruban -> ruban
@requires un ruban 
@ensures un ruban modifié
*********************************************************)
let rightRuban ruban = 
  match ruban with
  |Ruban(l1,head,[]) -> Ruban(pushLastPosition head l1,' ',[])
  |Ruban(l1,head,a::b) -> Ruban(pushLastPosition head l1,a,b );;
  
(*********************************************************
fonction leftRuban :
-> déplace le curseur du ruban vers la gauche

Si on va à gauche on prend le dernier élement 
de la liste gauche

+ ([a;b;c],d,[e;f;g]) -> ([a;b],c,[d;e;f;g])

type : ruban -> ruban
@requires un ruban
@ensures un ruban modifié
*********************************************************)
let leftRuban ruban = 
  match ruban with
  |Ruban([],head,l2) -> Ruban([],' ',head::l2)
  |Ruban(l1,head,l2) -> Ruban(listWithoutLastElement l1,lastElement l1,head::l2);;

(*********************************************************
fonction runInstruction :
-> exécute une instruction élémentaire ne nécessitant
aucun parcours de program sur un ruban
Version 1 de l'énoncé 

type : instruction -> ruban -> ruban
@requires il faut que l'instruction existe 
@ensures un ruban modifié
@raises une erreur si l'instruction n'existe pas 
*********************************************************)
let runInstruction instruction ruban = 
  match instruction with
  |Write c -> writeRuban c ruban
  |Right -> rightRuban ruban
  |Left -> leftRuban ruban
  |_ -> failwith "runInstruction : Command"
  ;;

(**************TEST DE LA FONCTION runInstruction*********

let ruban = Ruban(['a'],'b',['c']);;
let _ = runInstruction (Write 'd') ruban;; 
(* Expected : Ruban(['a'],'d',['c']) *)
(*OK*)
let _ = runInstruction Right ruban;;
(* Expected : Ruban (['a';'d'],'c',[]) Ok *)
(*OK*)
let _ = runInstruction Left ruban;;
(* Expected : Ruban([] , 'a', ['b';'c']) Ok *)
(*OK*)

let ruban2 = Ruban(['z';'w';'a'],'b',['c';'x';'x']);; 
let _ = runInstruction Right ruban2;;(* Expected : Ruban (['z';'w';'a';'b'],'c',['x';'x'])*)
(*OK*)
let _ = runInstruction Left ruban2;;(* Expected : Ruban(['z';'w';'a'],'b',['c';'x';'x'])*)
(*OK*)
let _ = runInstruction Left (Ruban([],' ',[]));; (* Expected : Ruban([],' ',[]) *)
(*OK*)

*********************************************************)


(************************************************************
fonction runListInstruction :
-> exécute une liste d'instruction (program) sur un ruban

type : instruction list -> ruban -> ruban
@requires Une liste d'instruction
@ensures un ruban modifié
************************************************************)

let rec runListInstruction program ruban = 
  match program with
    |[] -> ruban
    |Repeat(n,li) :: b -> (
      (************************************************************
      function forList :
      -> exécute n fois un programme sur un ruban
      
      type : int -> instruction list -> ruban -> ruban
      @requires n>0 
      @ensures un ruban modifié avec le programme exécuté n fois
      @raises Une erreur si n<0
      *************************************************************)
      let rec forList n program ruban = 
        match n with
          |0 -> ruban
          |_ -> if(n<0) then failwith "forList : n<0" else forList(n-1) program  (runListInstruction program ruban)
        in
        runListInstruction b (forList n li ruban)
      )  
      |a::b -> runListInstruction b (runInstruction a ruban)
    ;;  
    
(**************TEST DE LA FONCTION forList*******************

let tape = Ruban(['a';'b';'c';'d'],'e',['f';'g';'h'])
let _ = forList 3 [Write 'z';Right] tape
(*Expected : Ruban(['a';'b';'c';'d';'z';'z';'z']'h',[]) Ok*)
let _ = forList 0 [Write 'z';Right] tape
(*Expected : Ruban(['a';'b';'c';'d'],'e',['f';'g';'h']) Ok*)

*************************************************************)


(**************TEST DE LA FONCTION runListInstruction*********

let tape = Ruban(['a';'b';'c';'d'],'e',['f';'g';'h']);;
let _ = runListInstruction [Write 'z';Right;Right;Write 'z'] tape;;
(*Expected : Ruban(['a';'b';'c';'d';'z';'f'],'z',['h']) Ok*)
let _ = runListInstruction [Write 'z';Right;Right;Write 'z';Repeat(4,[Right])] tape;;
(*Expected : Ruban(['a';'b';'c';'d';'z';'f';'z';'h';' '; ' '],' ',[]) Ok*)
let _ = runListInstruction [Write 'z';Right;Right;Write 'z';Repeat(4,[Right]);Repeat(6,[Left])] tape;;
(*Expected : Ruban (['a'; 'b'; 'c'; 'd'; 'z'; 'f'; 'z'; 'h'; ' '; ' '], ' ', []) *)

let _ = runListInstruction Repeat(8,[Right]) (Ruban([],' ',[]));; 
(*ne fonctionne pas la fonction attend une liste*)

let _ = runListInstruction [Repeat(4,[Right])] (Ruban([],' ', []));;
(*Expected Ruban ([' '; ' '; ' '; ' '], ' ', [])*)

************************************************************)

(************************************************************
fonction execute_program :
-> exécute un programme sur un ruban vide

type : instruction list -> ruban
@requires Une liste d'instruction
@ensures un ruban modifié
************************************************************)
let execute_program program = 
  let tape = Ruban([],' ',[]) in 
  runListInstruction program tape
  ;;
  
(************************************************************

3.Implanter la fonction fold_ruban : 
('a -> char -> 'a) -> 'a -> ruban -> 'a
qui parcours le ruban de gauche à droite en appliquant la 
fonction f. 

v0 est la valeur par défaut quand la liste est vide

+ fonction f prend 2 paramètre : 
  un type a et un char et renvoie a

+ v0 est de type a

+ et un ruban

=>le fold_ruban renvoie donc une réponse de type v0

on veut parcourir de gauche à droite donc fold_left

la fonction f s'utiliserait donc comme cela : 
'a -> char -> 'a
f v0 'a' = v1

donc pour une liste il faudrait faire :
List.fold_left (fun v_n newChar -> f v_n newChar ) v0 liste
************************************************************)

(************************************************************
fonction rubanToList :
-> transforme un ruban en liste

type : ruban -> char list
@requires un ruban
@ensures une liste de char
*************************************************************)
let rubanToList ruban =
  match ruban with
  |Ruban(l1,head,l2) -> List.fold_right (fun x acc -> x::acc) l1 (head::l2);;

(**************TEST DE LA FONCTION rubanToList***************

let _ = rubanToList (Ruban(['a';'b';'c';'d'],'e',['f';'g';'h']));;
(*Expected : ['a';'b';'c';'d';'e';'f';'g';'h'] *)
(*OK*)
let _ = rubanToList (Ruban([],' ',[]));;
(*Expected : [' '] *)
(*OK*)
let _ = rubanToList (Ruban(['a';'b';'c';'d'],'e',[]));;
(*Expected : ['a';'b';'c';'d';'e'] *)
(*OK*)
let _ = rubanToList (Ruban([],'a',['b';'c';'d']));;
(*Expected : ['a';'b';'c';'d'] *)
(*OK*)

*************************************************************)

(*************************************************************
fonction fold_ruban :
-> parcours le ruban de gauche à droite en effectuant un fold

type : ('a -> char -> 'a) -> 'a -> ruban -> 'a
@requires une fonction f, une valeur v0 et un ruban
@ensures une valeur de type v0
**************************************************************)
let fold_ruban f v0 ruban = 
    let tape = rubanToList ruban in 
    List.fold_left (fun v_n newChar -> f v_n newChar) v0 tape
  ;;

(***********TEST AVEC LES FICHIERS DE L'ENONCÉ******************

let testRuban = analyse_program "first.prog";;
let a = execute_program testRuban;; (*Fonctionne*)
let _ = rubanToList a;;(*Fonctionne*)

let testRuban = analyse_program "hello_left.prog";;
let b = execute_program testRuban;; (*Fonctionne*)
let _ = rubanToList b;;(*Fonctionne*)

let testRuban = analyse_program "hello.prog";;
let c = execute_program testRuban;; (*Fonctionne*)
let _ = rubanToList c;;(*Fonctionne*)

let testRuban = analyse_program "message_3aien.prog";;
let d = execute_program testRuban;; (*Fonctionne*)
let _ = rubanToList d;;(*Fonctionne*)

************************************************************)

(* 3.2 *)
(************************************************************
fonction caesarChar :
-> effectue un décalage circulaire de n sur un char

Les majuscules sont de 65 à 90
Les minuscules sont de 97 à 122

Cela aurait pu être fait avec des if comme demandé mais 
"when" est plus esthétique

type : int -> char -> char
@requires char doit être une lettre en ascii 
@ensures un char décalé de n selon caesar
@raises une erreur si le char n'est pas une lettre ascii
************************************************************)
let caesarChar n character = 
  let charNb = Char.code character in
  match charNb with
  |a when a>=122 || a<65  -> failwith "caesarChar : not a letter"
  |a when a >= 97 -> Char.chr (((a+n-97) mod 26)+97)
  |a when a >= 65 -> Char.chr (((a+n-65) mod 26)+65)
  |_ -> failwith "caesarChar : unreachable"
;;

(***********************************************************
fonction caesarList :
-> applique la fonction caesarChar sur une liste de char

type : int -> char list -> char list
@requires une liste de char
@ensures une liste de char décalée de n selon caesar
************************************************************)
let caesarList n list = List.map (caesarChar n) list;;

(***********************************************************
fonction caesarRuban :
-> applique la fonction caesarChar sur un ruban

type : int -> ruban -> ruban
@requires un ruban
@ensures un ruban décalé de n selon caesar 
***********************************************************)
let caesarRuban n ruban = 
  match ruban with
  |Ruban(l1,head,l2) -> Ruban(caesarList n l1,caesarChar n head,caesarList n l2)
  ;;

(***********************************************************
fonction deleteChar :
-> remplace un char par ' ' (donc le supprime) 

type : char -> char -> char
@requires un char et un char(a supprimer)
@ensures un char (le même si différent du char à supprimer)
************************************************************)
let deleteChar c charInput = if (charInput=c) then ' ' else charInput ;;

(***********************************************************
fonction deleteCharInList :
-> applique la fonction deleteChar sur une liste de char

type : char -> char list -> char list
@requires un char à supprimer et une liste de char
@ensures une liste sans le char
***********************************************************)
let deleteCharInList c list = 
  (*un fold left inverserais la liste*)
  List.fold_right (fun e acc -> (deleteChar c e) :: acc) list []
;;

(***********************************************************
fonction deleteRuban :
-> supprime un character d'un ruban

@requires un char et un ruban
@ensures un ruban sans le char
************************************************************)
let deleteRuban a ruban = 
  match ruban with
  |Ruban(l1,head,l2) -> Ruban(deleteCharInList a l1,deleteChar a head,deleteCharInList a l2)
;;

(***********************************************************
fonction invertRuban :
-> inverse un ruban

type : ruban -> ruban
@requires un ruban
@ensures un ruban inversé
***********************************************************)
let invertRuban ruban = 
  match ruban with
  |Ruban(l1,head,l2) -> Ruban(List.rev l2, head, List.rev l1)
;;

(***********************************************************
fonction runInstruction :
-> exécute une instruction sur un ruban , 
    prend en compte caesar, delete, invert 
Version 2

type : instruction -> ruban -> ruban
@requires une instruction et un ruban
@ensures un ruban modifié par l'instruction
@raises une erreur si l'instruction n'est pas reconnue
************************************************************)
let runInstruction instruction ruban = 
  match instruction with
  |Write c -> writeRuban c ruban
  |Right -> rightRuban ruban (*->*)
  |Left -> leftRuban ruban (*<-*)
  |Caesar n -> caesarRuban n ruban
  |Delete a -> deleteRuban a ruban
  |Invert -> invertRuban ruban
  |_ -> failwith "runInstruction : Command not found " 
  ;;

(*pas commenté car les mêmes que plus haut*)
let rec runListInstruction program ruban = 
  match program with
    |[] -> ruban
    |Repeat(n,li) :: b -> (
      let rec forList n program ruban = 
        match n with
          |0 -> ruban
          |_ -> if(n<0) then failwith "forList : n<0" else forList(n-1) program  (runListInstruction program ruban)
        in
        runListInstruction b (forList n li ruban)
      )  
      |a::b -> runListInstruction b (runInstruction a ruban)
    ;;  
    
let execute_program program = 
  let tape = Ruban([],' ',[]) in 
  runListInstruction program tape
  ;;
(**************TEST AVEC LES FICHIERS DE L'ENONCE************

let testRuban = analyse_program "invert.prog";;
let a = execute_programV2 testRuban;; (*Fonctionne*)
let _ = rubanToList a;;(*Fonctionne*)

let testRuban = analyse_program "message_3aien.prog";;
let a = execute_programV2 testRuban;; 
let _ = rubanToList a;;(*Fonctionne toujours *)


*************************************************************)

(************************************************************
3.3
Objectif : prendre un message et l’encoder en 2Aien 
de telle sorte que le nouveau message comporte aussi peu 
d’instructions que possible
Vous disposez pour cela de la fonction 

print program : program -> unit 

let _ = print_program [Write 'a';Right];;
W(a);R;- : unit = ()

qui affiche sur la sortie standard le code d’un programme et 
de la fonction 

read file : string -> char list 
qui récupère le contenu d’un fichier et retourne la
liste des ses caractères.

let _ = read_file "t1.prog"
- : char list = ['c'; 'o'; 'u'; 'c'; 'o'; 'u']
************************************************************)


(************************************************************
Pour rendre ça efficace il va valloir trouver un moyen
que :
coucou -> F(2,"cou");
hello -> 'h'+'e'+ F(2,l) + o
on va commencer par faire ça pour un mot , 

(cas 1)
+[]['h','e','l','l','o']
+['h']['e','l','l','o']
+['h','e']['l','l','o']
+['h','e']['l','l','o'] regarde si lettre double si oui le met directement traité dans l'autre
+['h','e',F(2,'l')]['o']
+['h','e',F(2,'l'),'o'][]

Il va faloir : 
+comparer 2 liste jusqu'a trouver la + longue séquence commune
+ encode une liste de charactere en une liste d'instruction
************************************************************)

(************************************************************
fonction howMany :
-> compte le nombre de char consécutif au premier charactere

type : 'a list -> 'a -> int
@requires une liste et un charactere
@ensures le nombre de char consécutif
************************************************************)
let rec howMany l value = match l with
|[] -> 0
|a::b -> if(a=value) then 1 + howMany b value else 0;;

(************************************************************
fonction noDupOf1st :
-> supprime les premiers char consécutif au 
charactere passé en paramètre

ex : ['l';'l';'o';'l'] 'l' -> ['o';'l']
type : 'a list -> 'a -> 'a list
@requires une liste et un charactere (celui qu'on souhaite enlever)
@ensures une liste sans charactere consécutif à "char"
*************************************************************)
let rec noDupOf1st l char =
  match l with
  | [] -> []
  | a::[] -> if a=char then [] else [a]
  | a::b ->
    if a = char then noDupOf1st b char
    else a::b
  ;;

(******************TEST HOWMANY + NEWLNODUP*****************
let _ = howMany ['l';'l';'o';'l'] 'l';; (*2*)
let _ = howMany ['l';'p';'l'] 'p';; (*0 car p n'est pas le premier élément de la liste*)
let _ = howMany ['l';'p';'l'] 'l';; (*1*)
let _ = howMany ['l';'l';'l'] 'l';; (*3*)

let _ = noDupOf1st ['l';'l';'o';'a';'l';'u'] 'l';; 
let _ = noDupOf1st ['l';'l'] 'l' ;;
let _ = noDupOf1st ['l';'l';'l'] 'l';; 
let _ = noDupOf1st ['l';'l';'l';'o';'l';'u'] 'l';; 
let _ = noDupOf1st ['l';'l';'a';'o';'l';'l'] 'l';; 
************************************************************)


(************************************************************
fonction encode :
-> encode une liste de charactere en une liste d'instruction
ne traite que le cas des lettres doubles

type : 'a list -> instruction list
@requires une liste de charactere
@ensures une liste d'instruction
************************************************************)
let rec encode l =  
  match l with
  |[] -> []
  |a::[] -> Write a :: Right :: []
  |a :: b :: c -> 

    if (a=b) then  
    (*on entre dans un cas de répétitions*)

    let count = 2 + howMany (c) (a) in (*on compte le nombre de répétitions*)

    (* on crée une liste ou toutes les occurences de b qui se suivent sont enlevé*)
    (*a (ou b) contient l'élément à enlever on le met en début de liste*)
    let rest = noDupOf1st (c) (a) in 
    Repeat(count,[Write a; Right]) :: encode rest

  else 
    Write a:: Right :: encode (b::c)
  ;;
  
(************************TEST ENCODE*************************

Traite le cas des lettres qui se suive et se répéte dans hello, 
elle hellllllllllo (si la personne est vraiment contente), etc. 
et encode avec Write et Right , ne s'occupe pas des mots tel que 
coucou qui pourrait donne Repeat(2,cou) pour le moment

(* test avec un seul ensemble de lettre qui se répète*)
let _ = encode ['h';'e';'l';'l';'o'] ;;
let _ = encode ['e';'l';'l';'l';'e'] ;;
let _ = encode ['t';'r';'o';'t';'t';'o';'i';'r'] ;;

(* test avec plusieurs ensemble de lettre qui se répète*)
let _ = encode ['e';'l';'l';'l';'e';'e';'l';'l';'l';'e'] ;;
let _ = encode ['h';'e';'a';'a';'o';'l';'l'] ;; 

************************************************************)

(************************************************************
on va traiter le cas des mot tel que coucou , coco , bobo etc

le but va être de mettre le pattern dans la liste et tant qu'une
lettre n'a pas été répété on parcourt

(cas 2 )
+ [] ['c','o','u','c','o','u',...]
+ ['c'] ['o','u','c','o','u',...]
+ ['c','o'] ['u','c','o','u',...]
+ ['c','o','u'] ['c','o','u',...] Stop car  se rend compte 
qu'il y a cou comme premier element de la 2e liste
=> [F(2,'cou')][...]  
************************************************************)

(************************************************************
fonction subListOf :
-> renvoie une sous liste de l de taille num

type : 'a list -> int -> 'a list
@requires une liste et un entier
@ensures une liste de taille num
*************************************************************)
let rec subListOf num l = 
  match (num,l) with
  |(0,_)|(_,[]) -> []
  |(n,a::b) -> a::subListOf (n-1) b
  ;;
(************************TEST SUBLISTOF*************************
let _ = subListOf 2 ['c';'o';' ';'l';'a']  ;; (*['c';'o']*)
let _ = subListOf 4 ['c';'o';' ';'l';'a']  ;; (*['c';'o';' ';'l']*)
let _ = subListOf 0 ['c';'o';' ';'l';'a']  ;; (*[]*)
let _ = subListOf 7 ['c';'o';' ';'l';'a']  ;; (*[]*)
***************************************************************)

(************************************************************
fonction isSameList:
-> renvoie true si les premiers élément de la liste l 
    sont répété 

type : 'a list -> 'a list -> bool
@requires une liste et un pattern
@ensures true si la liste l est répété
************************************************************)
let isSameList l pattern = 
  let sizeOfPattern = List.length pattern in
  let subList = subListOf sizeOfPattern l in
  match subList with
  |[] -> false
  |_ -> (subList = pattern) 
;;

(************************TEST ISREPEATED*************************
let _ = isSameList ['c';'o';'u';'p';'e'] ['c';'o';'u'];; 
(*true*)
let _ = isSameList ['c';'o';'u';'p';'e'] [];; 
(*ne respecte pas les preconditions*)
let _ = isSameList ['c';'o';'u';'p';'e'] ['c'];; 
(*true*)
let _ = isSameList ['c';'o';'u';'p';'e'] ['a';'c'];; 
(*false*)
let _ = isSameList ['p';'e';'c';'o';'u'] ['c';'o';'u'];; 
(*false*)
***************************************************************)

(*************************************************************
fonction removeNelement :
-> renvoie une liste sans les num premiers éléments de l

type : int -> 'a list -> 'a list
@requires une liste et un entier (plus petit que la taille de la liste)
@ensures une liste sans les num premiers éléments de l
@raises si la liste est trop courte 
*************************************************************)
let rec removeNelement num list =
    match (num,list) with
    |(0,l) -> l
    |(n,[]) -> failwith "liste trop courte removeNelement" 
    |(n,a::b) -> removeNelement (n-1) b
;;

(************************TEST REMOVENELEMENT*******************
let _ = removeNelement 2 ['c';'o';'u';'u';'l';'u';'l'];;
let _ = removeNelement 0 ['c';'o';'u';'u';'l';'u';'l'];;
let _ = removeNelement 10 [];;
***************************************************************)

(************************************************************
fonction listWithoutPattern :
-> renvoie une liste sans le pattern et le nombre de fois 
    qu'il a été enlevé, ne s'applique qu'aux premiers éléments 
    de la liste qui respecte cette condition
  
type : 'a list -> 'a list -> (int * 'a list)

@requires une liste et un pattern
@ensures une liste sans le pattern et le nombre de fois 
        qu'il a été enlevé
@raises si le pattern est vide
*************************************************************)
let listWithoutPattern l pattern = 
  let rec aux count l pattern = 
    let size = List.length pattern  in
    match size with
    |0 -> failwith "le pattern est vide"
    |_ -> 
        (*on prend une sous liste de la taille du pattern*)
        let subList = subListOf size l in 

        (*on regarde si cette sous liste est la meme *)
          if (isSameList subList pattern) then

            let newList = removeNelement size l in
            (*si oui on l'enleve*)
            aux (count+1) newList pattern
          else
            (count,l)
          in 
          (*on a forcément vu aux moins une fois la lettre si on la test*)
          aux 1 l pattern
;;

(************************TEST LISTWITHOUTPATTERN*******************
let _ = listWithoutPattern ['c';'c';'c';'c';'o';'u';'c';'o';'u';'c';'o';'u'] ['c'] ;;
let _ = listWithoutPattern ['c';'c'] ['c'] ;;
let _ = listWithoutPattern ['c';'o';'u';'c';'o';'u';'c';'o';'u';'c';'o';'u'] [] ;;
let _ = listWithoutPattern ['c';'o';'u';'c';'o';'u';'c';'o';'u';'c';'o';'u'] ['c';'o';'u'] ;;
let _ = listWithoutPattern ['c';'o';'u';'c';'o';'u';'c';'o';'u';'c';'o';'u';'l'] ['c';'o';'u'] ;;
let _ = listWithoutPattern ['a';'b'] ['a'] ;;
***************************************************************)

(************************************************************
fonction encodeChar :
-> encode un char en une instruction

type : char -> instruction
@requires un char
@ensures une instruction
*************************************************************)
let encodeChar c = Write c;;

(************************************************************
fonction listWithoutFirstElement :
-> renvoie une liste sans le premier élément

type : 'a list -> 'a list
@requires une liste
@ensures une liste sans le premier élément
********************************************************)
let listWithoutFirstElement l = match l with
|[]->[]
|a::b->b
;;

(**************************************************************
fonction finalEncode :
-> renvoie une liste d'instruction pour encoder une liste de char
le plus efficacement possible

le but de cette fonction est de regarder si un element est repété.
Chaque element sera ajouté dans le pattern
le pattern est un accumulateur des lettres qui ne forme pas un pattern 
avec le reste de la liste.
Il sera testé avec les n premiers élements de la liste. 
Si le pattern est trouvé on l'enlève de la liste autant de fois qu'il 
apparait et on l'encode avec le nombre de fois qu'il apparait. 
sinon on l'encode en enlevant les lettres doubles.

type : 'char list -> instruction list
@requires une liste de char
@ensures une liste d'instruction pour encoder une liste de char
        le plus efficacement possible
***************************************************************)
let finalEncode l = 
  let rec testPattern listToEncode pattern = 
    match listToEncode with
    (*si on arrive la , on a tous evaluer ou on a pas trouver de pattern*)
    |[] -> encode pattern 
    (*pattern trouvé , on encode ce qu'il reste en enlevant les lettres doubles*)
    |a::b ->( 

      (*si on dépile le mot sera à l'envers, donc on le met à la fin*)
      let newPattern = pushLastPosition a pattern in
      
      (*on crée la liste sans le pattern*)
      (*count est le nombre de fois que le pattern a été trouvé*)
      let (count,listPatternRemoved) = listWithoutPattern b newPattern in

      match count with
      (*on a pas trouvé de pattern , on essaye avec l'élement suivant*)
      |1 -> testPattern b newPattern
      (*on a trouvé un pattern , on l'enleve et on essaye les autres en remettant l'accumulateur à vide*)
      |_-> Repeat(count,encode newPattern) :: testPattern listPatternRemoved []
    )
    in
    testPattern l [] 
  ;;

(************************TEST FINAL ENCODE*******************
let _ = finalEncode [];;
let _ = finalEncode ['a'];;
let _ = finalEncode ['a';'b'];;
let _ = finalEncode ['a';'a';'b'];;
let _ = finalEncode ['a';'a';'b';'b'];;
let _ = finalEncode ['a';' ';'a';' ';'a'];;
let _ = finalEncode ['a';'a';' ';'b';'b'];;
let _ = finalEncode ['c';'o';'c';'o';'l'];;
let _ = finalEncode ['o';'c';'c';'c';'c';'c';'c';'c';'c';'o']
let _ = finalEncode ['c';'o';'c';'o';'c';'o'];;
let _ = finalEncode ['c';'o';'l';'c';'o'];;
let _ = finalEncode ['c';'o';'u';'c';'o';'u';'l';'l';'l';'u'];;
let _ = finalEncode ['a';'b';'c';'a';'b';'c'];;
***************************************************************)

let generate_program msg = finalEncode msg;;

(************************TEST GENERATE PROGRAM*******************
let test1 = read_file "t2.prog";;
let _ = generate_program test1;;
let test2 = read_file "t2.prog";;
let _ = generate_program test2;;
***************************************************************)

(** Votre code doit s'arreter à partir de ce point.

    NE MODIFIER RIEN EN DESSOUS DE CE POINT 
 *)











                         
                         
let die i = 
  let _ = Format.fprintf Format.err_formatter "Usage: %s <1|2|3> <file>@." Sys.argv.(0) in
  exit i
  
let main phase file =
    if phase = "1" || phase = "2"
    then
       let li = analyse_program file in
       let rub = execute_program li in
       let _ = fold_ruban (fun _ c -> Format.printf "%c" c) () rub in
       Format.printf "@."
    else if phase = "3"
    then
      let msg = read_file file in
      let p = generate_program msg in
      print_program p
    else die 1


let _ =
  if Array.length Sys.argv = 3 
  then
    main Sys.argv.(1) Sys.argv.(2)
  else die 2
