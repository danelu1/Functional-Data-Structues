module BinaryNumber where

import Data.List
import Data.Tuple (swap)

{-
    Reprezentarea unui număr binar ca listă finită sau infinită de biți.
    Primul bit este cel mai puțin semnificativ.

    De exemplu, 6 poate fi reprezentat ca [0, 1, 1, 0, 0, ...].

    type introduce un sinonim de tip, similar cu typedef din C.
-}
type BinaryNumber = [Int]

{-
    *** TODO ***

    Transformă un număr din reprezentarea binară finită în cea zecimală.
    
    Constrângeri: funcția trebuie definită
    * în stil point-free (fără explicitarea parametrului formal)
    * utilizând o funcțională (fără recursivitate explicită).

    Exemple:
    
    > toDecimal [0,1,1]
    6
-}

{-
    Pentru implementare m-am gandit la varianta in care functia e definita
    clasic, folosind functionala "foldl" ce primeste ca parametru o functie
    anonima ce dubleaza de fiecare data acumulatorul si adauga bit-ul actual
    din sirul de biti si acumulatorul(considerat "0" initial), iar pentru
    a putea fi procesata corect, lista cu care lucram trebuie initial
    inversata(varianta asta este chiar cea de sub functia pe care am lasat-o
    in final). Am modificat apoi pentru cazul meu pentru a fi cat mai concis
    si scurt.
-}
toDecimal :: BinaryNumber -> Int
toDecimal = foldl ((+) . (* 2)) 0 . reverse
-- toDecimal = foldl (\acc x -> 2 * acc + x) 0 . reverse

{-
    *** TODO ***

    Transformă un număr din reprezentarea zecimală în cea binară infinită.

    Constrângeri: pt bonus 10p, funcția trebuie definită
    * în stil point-free (fără explicitarea parametrului formal)
    * utilizând funcționala unfoldr (fără recursivitate explicită).

    Spre deosebire de foldr, care împăturește o listă la o singură valoare,
    unfoldr desfășoară procesul invers, construind o listă pe baza unei valori
    inițiale.
    
    Hint: divMod.

    Exemple:
    
    > take 10 $ toBinary 6
    [0,1,1,0,0,0,0,0,0,0]
-}

{-
    Pentru implementare am folosit functionala "unfoldr" care primeste
    ca parametru o functie anonima ce verifica pentru un anumit "x" daca
    e "0", caz in care in care adauga in lista ce urmeaza a fi formata
    zerouri la infinit, iar in caz contrar, cand "x" ia orice valoare
    pozitiva diferita de "0", este adaugat in lista finala restul
    impartirii lui "x" la 2, iar valoarea lui "x" devine catul obtinut
    prin impartirea la 2. Am folosit "divMod x 2" pentru primul caz din
    "case" pentru a obtine de fiecare data o valoare noua pentru "x"
    (in fapt, acesta va ramane "0" tot timpul), deoarece astfel vor fi
    adaugati o infinitate de zerouri la lista finala.
-}
toBinary :: Int -> BinaryNumber
toBinary = unfoldr (\x -> case x of 
    0 -> Just (divMod x 2)
    _ -> Just (swap $ divMod x 2))
{-
    *** TODO ***

    Incrementează cu 1 reprezentarea finită sau infinită a unui număr binar.
    Atenție la transport!

    Constrângeri: utilizați
    * recursivitate explicită
    * pattern matching.

    Exemple:

    > inc [0,1,1] 
    [1,1,1]

    > inc [1,1,1]
    [0,0,0,1]
-}

{-
    Pentru implementarea acestei functii am considerat cazurile de baza
    ale incrementarii binare si pe rand am considerat ca daca lista mea
    de biti este de forma (0:bits) atunci prin incrementare obtinem pur
    si simplu lista (1:bits), iar in cazul in care pe prima pozitie ar
    fi fost "1" in loc de "0", atunci am fi adaugat "0" la lista obtinuta
    recursiv prin incrementarea urmatoarelor valori.
-}
inc :: BinaryNumber -> BinaryNumber
inc [0] = [1]
inc [1] = [0]
inc (0 : bits) = 1 : bits
inc (1 : bits) = 0 : inc bits

{-
   *** TODO ***

    Decrementează cu 1 reprezentarea finită sau infinită a unui număr binar.
    Atenție la împrumut!

    Constrângeri: utilizați
    * recursivitate explicită
    * pattern matching.

    Exemple:

    > dec [1,1,1]
    [0,1,1]

    > dec [0,0,0,1]
    [1,1,1]
-}

{-
    Aceeasi implementare ca functia anterioara, doar ca de data aceasta
    operatiile trebuiesc facute invers.
-}
dec :: BinaryNumber -> BinaryNumber
dec [0] = [1]
dec [1] = [0]
dec (0 : bits) = 1 : dec bits
dec (1 : bits) = 0 : bits

{-
    *** TODO ***

    Adună două numere binare, asumând reprezentări infinite, pentru ușurința
    aplicării funcționalelor.

    Constrângeri: utilizați
    * where sau let
    * pt bonus 10p, funcționala mapAccumL (fără recursivitate explicită).

    mapAccumL are tipul (a -> b -> (a, c)) -> a -> [b] -> (a, [c]).
    Așa cum sugerează numele, combină comportamentele funcționalelor:
    * map, transformând element cu element [b] în [c]
    * foldl, utilizând în același timp un acumulator de tipul a.

    Exemple:

    > take 10 $ add (toBinary 74) (toBinary 123)
    [1,0,1,0,0,0,1,1,0,0]
    
    > toDecimal $ take 10 $ add (toBinary 74) (toBinary 123)
    197
-}

{-
    Pentru implementare am folosit functionala "mapAccumL" pentru care am
    construit o functie anonima ce primeste ca parametru un acumulator si
    un tuplu reprezentand un element din lista de tupluri obtinuta prin
    imperecherea listelor "bits1" si "bits2" si calculeaza de fiecare data
    suma celor 2 biti din pereche(bitii aflati pe aceleasi pozitii in listele
    initiale adica) la care se adauga si acumulatorul, acesta reprezentand in
    fapt numarul pe care il tii minte in urma adunarilor(initial fiind 0).
    Functia anonima intoarce perechea formata din "ce tinem minte"(adica 
    elementul obtinut prin impartirea sumei la 2) si "ce trebuie sa punem
    in lista finala"(adica restul impartirii sumei la 2). Inca de la inceput
    trebuie sa extragem al doilea element din tuplul intors de aplicarea 
    functionalei "mapAccumL", deoarece al doilea este lista formata din
    bitii "ce trebuiesc pusi".
-}
add :: BinaryNumber -> BinaryNumber -> BinaryNumber
add bits1 bits2 = snd $ mapAccumL (\acc (x, y) ->
     let sum = x + y + acc
         (keepInMind, bitToPlace) = divMod sum 2
     in (keepInMind, bitToPlace))
        0 $ zip bits1 bits2
{-
    *** TODO ***

    În pregătirea operației de înmulțire a două numere binare, cu reprezentări
    infinite, stivuiește copii deplasate la dreapta ale lui bits1, înmulțite
    cu bit-ul curent din bits2. Deplasarea se face adăugând la stânga lui bits1
    un număr de 0-uri dat de numărul liniei curente. Întoarce o listă infinită
    de liste infinite.

    Vizual:

    0 1 1 0 ... *   <- bits1
    1 0 1 0 ...     <- bits2
    -----------
   |0 1 1 0 ...        înmulțire bits1 cu 1 și deplasare 0 poziții la dreapta
    0|0 0 0 0 ...      înmulțire bits1 cu 0 și deplasare 1 poziție la dreapta
    0 0|0 1 1 0 ...    înmulțire bits1 cu 1 și deplasare 2 poziții la dreapta

    Constrângeri:
    * Corpul funcției trebuie să fie un list comprehension.
    * Nu utilizați recursivitate explicită.

    Hint: iterate pt generarea secvențelor de deplasare spre dreapta cu biți 0.

    Exemple:

    (exemplul vizual)
    > take 3 $ map (take 6) $ stack (toBinary 6) (toBinary 5)
    [[0,1,1,0,0,0],[0,0,0,0,0,0],[0,0,0,1,1,0]]
-}

{-
    Pentru implementarea acestei functii am imperecheat fiecare bit din
    "bits2" cu fiecare numar natural, pentru a nu realiza un produs cartezian
    infint intre numerele din "bits2" si cele din lista infinita de numere
    naturale, dupa care am realizat toate inmultirile dintre "bits1" si fiecare
    bit din "bits2", generand astfel o infinitate de liste infinite la care adaug
    de fiecare data la inceput un numar de "i" copii ale lui "0" pentru a forma
    listele descrise de enuntul cerintei.
-}
stack :: BinaryNumber -> BinaryNumber -> [BinaryNumber]
stack bits1 bits2 = [replicate i 0 ++ [x * y | x <- bits1] | (i, y) <- zip [0..] bits2]

{-
    *** TODO ***

    Întoarce o listă infinită de numere binare, care pe poziția i >= 0 conține
    rezultatul înmulțirii lui bits1 cu numărul format din primii i biți
    ai lui bits2, i.e. suma primelor i linii întoarse de stack.

    Constrângeri:
    * Utilizați funcționala scanl (fără recursivitate explicită).

    Spre deosebire de foldl, care întoarce acumulatorul final, scanl întoarce
    o listă cu toate acumulatoarele intermediare.

    Exemple:
    
    > take 5 $ map (toDecimal . take 10) $ multiply (toBinary 6) (toBinary 5) 
    [0,6,6,30,30]
-}

{-
    Pentru implementare am folosit functionala "scanl" ce primeste ca parametrii
    functia binara "add", o lista infinita ce descrie reprezentarea binara a lui
    "0" obtinuta cu ajutorul functiei "toBinary"(acumulatorul initial) si lista
    infinita intoarsa de functia "stack". Functia intoarce lista ce contine
    acumulatorii obtinuti la fiecare pas prin adunarea cu fiecare lista din cea
    intoarsa de "stack". 
-}
multiply :: BinaryNumber -> BinaryNumber -> [BinaryNumber]
multiply bits1 bits2 = scanl add (toBinary 0) $ stack bits1 bits2

{-
    *** TODO ***

    Întrebare de reflecție, la care veți răspunde la prezentarea temei.

    Având în vedere că liniile întoarse de stack care conțin exclusiv biți 0
    nu contribuie la rezultatul unei înmulțiri, să presupunem că modificăm
    definiția funcției stack astfel încât să construiască doar liniile utile;
    de exemplu, folosind filter sau pattern matching în list comprehension
    pt a păstra doar biții 1 din bits2. Ce probleme ar putea crea această
    abordare?
-}
