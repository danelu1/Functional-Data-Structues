module BinomialHeap where

import Data.Function (on)
import Data.List ( minimumBy, intercalate, mapAccumL )
import Data.Foldable
import Data.Char (toUpper)
{-
    Reprezentarea unui arbore binomial, având priorități de tipul p și chei
    de tipul k. Conform proprietății definitorii a acestor arbori, fiecare
    copil i dintre cei r copii ai unui nod de rang r, cu 1 <= i <= r, trebuie
    să aibă exact r-i copii la rândul său, adică r-1, r-2, ..., 1, 0 copii,
    exact în această ordine descrescătoare. Dacă rădăcina are r copii, atunci,
    în conjuncție cu proprietate anterioară, întregul arbore are 2^r noduri.
-}
data BinomialTree p k
    = EmptyTree
    | Node { prio :: p, key :: k, children :: [BinomialTree p k] }
    deriving (Eq)

{-
    Reprezentarea unui heap binomial, ca listă de arbori binomiali ce respectă
    proprietatea de heap. Mai precis, considerăm că, în fiecare arbore binomial,
    rădăcina are cea mai mică prioritate; analog pt subarbori. Câmpul size
    desemnează numărul de elemente din heap (nu numărul de arbori). Mai precis,
    dimensiunea este egală cu suma dimensiunilor arborilor din listă. Cum
    dimensiunea unui arbore de rang r este 2^r, dimensiunea este o sumă
    de puteri ale lui 2, unde exponenții sunt dimensiunile câmpurilor children
    (rangurile) din rădăcinile arborilor nevizi.
-}
data BinomialHeap p k = BinomialHeap { size :: Int, trees :: [BinomialTree p k] }
    deriving (Eq)

{-
    *** TODO ***

    Construiește recursiv un arbore binomial de rang r din doi arbori binomiali
    de rang r-1, atașând unul dintre arbori drept prim copil al rădăcinii
    celuilalt. Maniera în care se realizează atașarea trebuie să țină cont
    de faptul că cei doi arbori respectă proprietatea de heap, și că arborele
    rezultant trebuie de asemenea să o respecte. Astfel, arborele cu cheia mai
    mică din rădăcină trebuie să încorporeze arborele cu cheia mai mare.

    Atenție! Cei doi arbori primiți ca parametru au întotdeauna același rang,
    conform principiului de construcție. Funcția nu necesită parcurgeri
    recursive, putând opera direct la nivelul rădăcinilor.

    Constrângeri: utilizați gărzi.

    Hint: pt pattern matching, pot fi utile alias-urile (@).

    Exemple:

    > attach (Node 0 'a' []) (Node 1 'b' [])
    Node {prio = 0, key = 'a', children = [Node {prio = 1, key = 'b', children = []}]}

    > attach (Node 1 'b' []) (Node 0 'a' [])
    Node {prio = 0, key = 'a', children = [Node {prio = 1, key = 'b', children = []}]}
-}

{-
    Pentru implementarea acestei functii am considerat cazurile de baza in care prin atasarea
    unui arbore biomial nevid la un arbore vid se obtine chiar arborele nevid considerat
    anterior.
    Am scris cei doi arbori din antetul functiei utilizand alias-uri, astfel am la dispozitie
    atat prioritatea si cheia unui nod din arbore, cat si copiii acestuia. Compar cele 2
    prioritati si construiesc un nou nod cu aceeasi prioritate si cheie cu cea a arborelui
    binomial cu prioritate mai mica, si lista de copii, la inceputul careia adaug arborele
    binomial cu prioritate mai mare.
-}
attach :: Ord p => BinomialTree p k -> BinomialTree p k -> BinomialTree p k
attach tree EmptyTree = tree
attach EmptyTree tree = tree
attach tree1@(Node p1 k1 children1) tree2@(Node p2 k2 children2)
    | p1 < p2 = Node p1 k1 (tree2: children1)
    | otherwise = Node p2 k2 (tree1:children2)

{-
    *** TODO ***

    Introduce un arbore binomial nevid într-o listă cu alți arbori binomiali,
    toți arborii respectând proprietatea de heap. Cum nu pot exista simultan
    în listă doi arbori binomiali cu același rang, la întâlnirea unui arbore
    cu același rang cu cel care se dorește introdus, este necesară atșarea
    unuia la celălalt, cu crearea unui transport.

    Operația o oglindește pe cea de incrementare a unui număr binar din etapa 1.
    Astfel, elementele EmptyTree sunt analoagele biților 0, iar elementele Node,
    biților 1. O diferență este că, în această etapă, biții 1 „nu mai arată toți
    la fel”, ci elementele Node au rangul dat de poziția în listă. Spre exemplu:
    * un element Node de pe poziția 0 din listă trebuie să aibă rangul 0
      (dimensiunea 2^0 = 1)
    * un element Node de pe poziția 1 din listă trebuie să aibă rangul 1
      (dimensiunea 2^1 = 2)
    * un element Node de pe poziția 2 din listă trebuie să aibă rangul 2
      (dimensiunea 2^2 = 4)
    etc.

    Gestiunea transportului apărut în incrementare corespunde operației attach.
    Modul în care va fi utilizată mai departe funcția insertTree garantează
    respectarea presupunerilor funcției attach, cum că arborii primiți ca
    parametru au întotdeauna același rang.

    Constrângeri: utilizați
    * construcția case
    * funcția attach.

    Exemple:

    > insertTree (Node 1 'a' []) []
    [Node {prio = 1, key = 'a', children = []}]

    > insertTree (Node 2 'b' []) $ insertTree (Node 1 'a' []) []
    [ EmptyTree
    , Node {prio = 1, key = 'a', children = [Node {prio = 2, key = 'b', children = []}]}
    ]

    > insertTree (Node 3 'c' []) $ insertTree (Node 2 'b' []) $ insertTree (Node 1 'a' []) []
    [ Node {prio = 3, key = 'c', children = []}
    , Node {prio = 1, key = 'a', children = [Node {prio = 2, key = 'b', children = []}]}
    ]
-}

{-
    Pentru implementarea acestei functii am considerat cazul in care vreau sa introduc un
    arbore binomial intr-o lista vida si sa obtin o lista formata doar din arborele introdus.
    Am folosit constructia (t:trees) pentru un apel recursiv mai simplu(e mai usor sa scrii
    asa decat sa scrii "tail trees") si am luat cele 2 cazuri posibile, asemanatoare cu operatia
    de incrementare binara implementata in etapa anterioara:
        -> in cazul in care la inceput avem un arbore vid(analog unui bit de "0") atunci doar
           adaugam arborele binomial "tree" in restul listei de arbori "trees".
        -> in caz contrar adaugam un arbore vid la inceputul listei formate prin apelul recursiv
           al functiei pe arborele format din atasarea lui "tree" la "t" si restul listei.
    Implementarea este aceeasi cu a functiei "inc bits" din etapa 1.
-}
insertTree :: Ord p => BinomialTree p k -> [BinomialTree p k] -> [BinomialTree p k]
insertTree tree [] = [tree]
insertTree tree (t:trees) = case t of
    EmptyTree -> tree:trees
    _ -> EmptyTree : insertTree (attach tree t) trees

{-
    *** TODO ***

    Heap-ul vid.
-}
emptyHeap :: BinomialHeap p k
emptyHeap = BinomialHeap 0 []

{-
    *** TODO ***

    Introduce o cheie cu prioritatea aferentă într-un heap binomial.

    Constrângeri: utilizați funcția insertTree.

    Exemple:

    > insert 1 'a' emptyHeap
    BinomialHeap 
        { size = 1
        , trees = [Node {prio = 1, key = 'a', children = []}]
        }

    > insert 2 'b' $ insert 1 'a' emptyHeap
    BinomialHeap
        { size = 2
        , trees = [ EmptyTree
                  , Node {prio = 1, key = 'a', children = [Node {prio = 2, key = 'b', children = []}]}
                  ]
        }

    > insert 3 'c' $ insert 2 'b' $ insert 1 'a' emptyHeap
    BinomialHeap 
        { size = 3
        , trees = [ Node {prio = 3, key = 'c', children = []}
                  , Node {prio = 1, key = 'a', children = [Node {prio = 2, key = 'b', children = []}]}
                  ]
        }
-}

{-
    Pentru implementarea acestei functii am scris heap-ul primit ca parametru folosind alias(pentru a putea
    evidentia caracteristicile principale ale acestuia) si construiesc un nou heap de dimensiune "size + 1"
    (din cauza introducerii unui nou element creste si dimensiunea heap-ului) si arborii asociati acestuia
    obtinuti prin prisma functiei "insertTree" implementata mai sus, care introduce un nod nou, cu prioritatea
    "prio", cheia "key" si o lista vida de copii asociati acestuia, in lista de arbori.
-}
insert :: Ord p => p -> k -> BinomialHeap p k -> BinomialHeap p k
insert prio key heap@(BinomialHeap size trees) = BinomialHeap (size + 1) $ insertTree (Node prio key []) trees

{-
    *** TODO ***

    Dacă heap-ul nu este vid, întoarce perechea formată din prioritatea minimă
    și cheia aferentă; în caz contrar, întoarce Nothing. Cum toți arborii din
    listă respectă proprietatea de heap, este suficient să parcurgeți doar
    rădăcinile pt a o determina pe cea cu prioritate minimă, fără a fi necesară
    explorarea nivelurilor inferioare ale arborilor.

    Constrângeri: pt selectarea arborilor nevizi din listă (ignorând elementele
    EmptyTree), utilizați list comprehension cu pattern matching.

    Hint: pt determinarea elementului minim dintr-o listă pe baza valorii
    calculate de o funcție numită criteriu, utilizați o expresie de forma:
    minimumBy (compare `on` criteriu) lista.

    Exemple:

    > findMin emptyHeap
    Nothing

    > findMin $ insert 3 'c' $ insert 2 'b' $ insert 1 'a' emptyHeap
    Just (1,'a')
-}

{-
    Pentru implementarea acestei functii am considerat urmatoarele cazuri:
        -> perechea "(size, trees)"(corespunzatoare heap-ului primit ca parametru) este (0, [])
           (pereche corespunzatoare heap-ului vid, dar cu constructia "emptyHeap -> ..." nu mergea
           si nu stiu de ce), caz in care este intors "Nothing".
        -> orice alta pereche, caz in care construim lista ce contine toate elemente nevide din
           lista "trees" corespunzatoare heap-ului si nodul cu prioritate minima din lista astfel
           formata.
    La final intoarcem perechea ce contine prioritatea si cheia corespunzatoare nodului gasit.
-}
findMin :: Ord p => BinomialHeap p k -> Maybe (p, k)
findMin heap@(BinomialHeap size trees) = case (size, trees) of
    (0, []) -> Nothing
    _ -> let notEmpty = [tree | tree@(Node {}) <- trees]
             (Node prio key _) = minimumBy (\(Node p1 _ _) (Node p2 _ _) -> compare p1 p2) notEmpty
         in Just (prio, key)

{-
    Funcția zipExtend este similară funcției predefinite zip. Scopul ei este
    de a compensa limitarea funcției zip, care se oprește când atinge sfârșitul
    listei mai scurte. Primii doi parametri reprezintă valori cu care se extind
    prima, respectiv a doua listă, în funcție de care listă este mai scurtă.
    O puteți folosi în cele ce urmează.

    Exemple:

    > zipExtend 0 'z' [1,2] "abcd"
    [(1,'a'),(2,'b'),(0,'c'),(0,'d')]

    > zipExtend 0 'z' [1,2,3,4] "ab"
    [(1,'a'),(2,'b'),(3,'z'),(4,'z')]
-}
zipExtend :: a -> b -> [a] -> [b] -> [(a, b)]
zipExtend a' _  [] bs = zip (repeat a') bs
zipExtend _  b' as [] = zip as (repeat b')
zipExtend a' b' (a : as) (b : bs) = (a, b) : zipExtend a' b' as bs

{-
    *** TODO ***

    Combină două liste de arbori binomiali care respectă proprietatea de heap.
    Observațiile din comentariile funcției insertTree, legate de necesitatea
    atașării arborilor cu același rang, rămân valabile.

    Operația o oglindește pe cea de adunare a două numere binare din etapa 1.

    Constrângeri:
    * evitați recursivitatea explicită
    * utilizați funcția zipExtend pt a facilita aplicarea unor funcționale.

    Exemple:

    > mergeTrees [Node 1 'a' []] []
    [Node {prio = 1, key = 'a', children = []}]

    > mergeTrees [Node 1 'a' []] [Node 2 'b' []]
    [ EmptyTree
    , Node {prio = 1, key = 'a', children = [Node {prio = 2, key = 'b', children = []}]}
    ]

    > mergeTrees [Node 3 'c' []] $ mergeTrees [Node 1 'a' []] [Node 2 'b' []]
    [ Node {prio = 3, key = 'c', children = []}
    , Node {prio = 1, key = 'a', children = [Node {prio = 2, key = 'b', children = []}]}
    ]
-}

{-
    Pentru implementare acestei functii am considerat lista formata cu ajutorul functiei "zipExtend"
    (primita in schelet) si perechea formata cu ajutorul funtionalei "mapAccumL". Functia, dupa cum
    e descris si la explicatii, este similara functiei de adunare binara implementata in etapa
    anterioara, deci am folosit functionala "mapAccumL" ce primeste un acumulator(initial "EmptyTree")
    si fiecare pereche din lista "pairs" si verifica fiecare caz posibil de "adunare" a doi parametrii:
        -> "x" vid -> "y" vid -> "acc" vid -> nu avem nimic de tinut in minte, intoarcem perechea formata
                                              din (EmptyTree, EmptyTree) (n-avem rest adica).
                              -> "acc" nevid -> perechea (EmptyTree, acc) (din nou n-avem rest).
                   -> "y" nevid -> "acc" vid -> perechea (EmptyTree, y) (n-avem rest din nou).
                                -> "acc" nevid -> perechea (attach y acc, EmptyTree) (pastram ce se formeaza
                                                  in minte, adica arborele obtinut prin atasarea lui x la
                                                  restul deja existent, si adaugam in lista finala de arbori
                                                  "EmptyTree", deoarece e ca la adunarea binara, 1 + 1 = 0).
    Toate celelalte cazuri pentru "x" nevid sunt complet analoage.
    La final verific daca primul element din pereche e vid sau nevid, deoarece in cazul in care acesta este
    vid, inseamna ca nu mai am niciun "rest" pe care trebuie sa-l adaug in lista formata cu functionala
    "mapAccumL", deci pot intoarce direct lista de arbori(adica al doilea element din pereche). In cazul
    in care elementul respectiv este nevid, inseamna ca in urma ultimei operatii am ramas cu un "rest"
    (primul element din pereche) pe care il adaug la sfarsitul listei de arbori formate.
-}
mergeTrees :: Ord p => [BinomialTree p k] -> [BinomialTree p k] -> [BinomialTree p k]
mergeTrees trees1 trees2 = 
    let pairs = zipExtend EmptyTree EmptyTree trees1 trees2
        mergedTrees = mapAccumL (\acc (x, y) -> case x of
            EmptyTree -> case y of
                EmptyTree -> case acc of
                    EmptyTree -> (EmptyTree, EmptyTree)
                    _ -> (EmptyTree, acc)
                _ -> case acc of
                    EmptyTree -> (EmptyTree, y)
                    _ -> (attach y acc, EmptyTree)
            _ -> case y of
                EmptyTree -> case acc of
                    EmptyTree -> (EmptyTree, x)
                    _ -> (attach x acc, EmptyTree)
                _ -> case acc of
                    EmptyTree -> (attach x y, EmptyTree)
                    _ -> (attach x y, acc))
                    EmptyTree pairs
    in case fst mergedTrees of
        EmptyTree -> snd mergedTrees
        _ -> snd mergedTrees ++ [fst mergedTrees]

{-
    *** TODO ***

    Combină două heap-uri binomiale.

    Constrângeri: utilizați funcția mergeTrees.

    Exemple: similare cu cele de la mergeTrees.
-}

{-
    Pentru implementarea acestei functii am construit heap-ul obtinut prin imbinarea celor 2 heap-uri
    primite ca parametrii, acesta avand dimensiunea "size1 + size2"(suma celor 2 initiale) si arborii
    asociati obtinuti prin imbinarea celor 2 liste cu ajutorul functiei "mergeTrees".
-}
merge :: Ord p => BinomialHeap p k -> BinomialHeap p k -> BinomialHeap p k
merge heap1@(BinomialHeap size1 trees1) heap2@(BinomialHeap size2 trees2) = BinomialHeap (size1 + size2) $ mergeTrees trees1 trees2

----------------------------------- Etapa 3 ------------------------------------

{-
    *** TODO ***

    Funcție ajutătoare, care izolează, pe rând, câte un element al unei liste,
    înlocuindu-l în cadrul listei cu un altul primit ca parametru. Întoarce
    o listă de perechi, care conțin pe prima poziție un element al listei
    originale, iar pe a doua, lista din care provine elementul, dar după
    înlocuirea de mai sus. Se va evita introducerea elementului înlocuitor
    pe ultima poziție din listă.

    Constrângeri:
    * puteți utiliza și recursivitate explicită, dar utilizați funcționale
      pe cât posibil (măcar pt părți din rezolvare)
    * NU este permisă adăugarea de asumpții asupra tipului a, e.g. Eq a.

    Exemple:

    > isolate 0 [1,2,3]
    [(1,[0,2,3]),(2,[1,0,3]),(3,[1,2])]  -- fără 0 în ultima listă
-}

{-
    Pentru implementarea acestei functii am construit prin intermediul unui "list comprehension" toate listele
    ce se puteau forma prin inlocuirea fiecarui element aflat la un index "i" din lista initiala, cu
    "placeHolder"-ul primit ca parametru. Deoarece ultima lista generata va contine pe ultima pozitie elementul
    "placeHolder", o vom lua separat pe aceasta si o vom prelucra(adica luam toate elemente din ea, fara ultimul).
    La final aduagam la lista formata din toate listele generate anterior(fara ultima lista) lista pe care tocmai
    am prelucrat-o si folosim "zip" pentru imperechere.
-}
isolate :: a -> [a] -> [(a, [a])]
isolate placeHolder list =
  let len = length list
      partialLists = [take i list ++ [placeHolder] ++ drop (i + 1) list | i <- [0..(len - 1)]]
      lastList = last partialLists
      correctLastList = take (len - 1) lastList
      correctLists = take (len - 1) partialLists ++ [correctLastList]
      in zip list correctLists

{-
    *** TODO ***

    Elimină din heap prima rădăcină de prioritate minimă. Aceasta presupune
    înlăturarea întregului arbore cu rădăcina de prioritate minimă din lista
    de arbori ai heap-ului (prin înlocuirea lui cu EmptyTree) și apoi
    combinarea (mergeTrees) noii liste de arbori cu lista de subarbori (orfani)
    ai rădăcinii tocmai înlăturate.

    Atenție! Având în vedere că lista de arbori ai heap-ului este ordonată
    crescător în raport cu rangul, iar lista de subarbori orfani este ordonată
    descrescător (conform structurii arborilor binomiali), este necesară
    inversarea ultimeia înainte de combinarea celor două liste!

    Constrângeri: utilizați isolate.

    Hint: vedeți indicațiile de la findMin pentru determinarea primei rădăcini
    de prioritate minimă.

    Exemple:

    > removeMin emptyHeap
    BinomialHeap {size = 0, trees = []}

    > removeMin $ insert 1 'a' emptyHeap
    BinomialHeap {size = 0, trees = []}

    > removeMin $ insert 2 'b' $ insert 1 'a' emptyHeap
    BinomialHeap {size = 1, trees = [Node {prio = 2, key = 'b', children = []}]}

    > removeMin $ insert 3 'c' $ insert 2 'b' $ insert 1 'a' emptyHeap
    BinomialHeap
        { size = 2
        , trees = [ EmptyTree
                  , Node {prio = 2, key = 'b', children = [Node {prio = 3, key = 'c', children = []}]}
                  ]
        }

    După ce implementați, răspundeți la întrebarea: Care este contribuția
    evaluării leneșe la utilizarea eficientă a funcției isolate?
-}

{-
    Functie auxiliara cu o implementara similara cu cea a functiei "findMin", care intoarce de data
    aceasta nodul cu prioritate minima din "heap".
-}
findMinRoot :: Ord p => BinomialHeap p k -> BinomialTree p k
findMinRoot (BinomialHeap size trees) = case (size, trees) of
    (0, []) -> EmptyTree
    _ -> let notEmpty = [tree | tree@(Node {}) <- trees]
             root@(Node _ _ _) = minimumBy (\(Node p1 _ _) (Node p2 _ _) -> compare p1 p2) notEmpty
         in root


{-
    Pentru implementarea acestei functii am considerat nodul de prioritate minima calculat prin intermediul
    functiei de mai sus, lista formata cu ajutorul functiei "isolate" aplicata pe lista de arbori din "heap"-ul
    initial pentru a inlocui fiecare nod parinte cu "EmptyTree"(izolarea acestora in fapt), lista partiala
    obtinuta din cea descrisa anterior prin cautarea perechii ce contine ca prim element chiar nodul de
    prioritate minima, din care urmeaza sa extragem orfanii ramasi dupa eliminarea sa si, in final, lista
    obtinuta prin combinarea listei initiale de copii ai nodului "minRoot"(ce nu se va mai afla in "heap")
    si a celei de orfani. La final se intoarce "heap"-ul de dimensiune mai mica cu 1 decat cel initial, ce
    contine lista de arbori obtinuta prin "mergeTree".
-}
removeMin :: (Ord p, Eq k) => BinomialHeap p k -> BinomialHeap p k
removeMin heap@(BinomialHeap size trees) =
    case (size, trees) of
    (0, []) -> emptyHeap
    _ -> let minRoot@(Node _ _ children) = findMinRoot heap
             list = isolate EmptyTree trees
             partial = filter (\x -> fst x == minRoot) list
             orphans = snd $ head partial
             mergedTrees = mergeTrees (reverse children) orphans
        in BinomialHeap (size - 1) mergedTrees

{-
    *** TODO ***

    Instanțiați clasa Show pt tipul (BinomialTree p k) în următorul mod:
    * un arbore vid este reprezentat prin "*"
    * un arbore nevid este reprezentat prin:

        prioritate (cheie)
          <copil 1>
          ...
          <copil n>
      
      unde reprezentarea copiilor este indentată cu 2 spații față de cea
      a părintelui.
    
    Hint: replicate și intercalate.

    Exemple:

    > EmptyTree
    *

    > Node 1 'a' []
    1 ('a')

    > Node 1 'a' [Node 2 'b' []]
    1 ('a')
      2 ('b')

    > Node 1 'a' [Node 3 'c' [Node 4 'd' []], Node 2 'b' []]
    1 ('a')
      3 ('c')
        4 ('d')
      2 ('b')
-}

{-
    Pentru implementare am considerat casul "EmptyTree", in care e specificat in enunt ca se returneaza "*",
    iar in caz contrar consideram o functie ce primeste ca parametru un nod si un nivel de adancime in arbore
    si intoarce reprezentarea arborelui incepand de la un anumit nivel. In interiorul acestei functii am
    considerat de fiecare data indentarea corespunzatoare pentru fiecare element, in functie de locul sau
    in lista de copii si in mod recursiv am construit lista formata din "string"-uri de forma "prio (key)",
    pentru reprezentarea componentelor unui nod, la care adaug de fiecare data indentarea.
    Folosesc "intercalate \n" pentru a trece fiecare nod pe cate o linie separat, dupa care apelam functia
    cu nivelul "0" de adancime.
-}
instance (Show p, Show k) => Show (BinomialTree p k) where
    show tree = case tree of
        EmptyTree -> "*"
        _ -> let
                treeToString EmptyTree _ = "*"
                treeToString (Node prio key children) depth =
                    let twoSpace = replicate (2 * depth) ' '
                        node = show prio ++ " (" ++ show key ++ ")"
                        childrenString =  [treeToString c (depth + 1) | c <- children]
                    in intercalate "\n" ((twoSpace ++ node) : childrenString)
             in treeToString tree 0

{-
    *** TODO ***

    Instanțiați clasa Show pt tipul (BinomialHeap p k) cu reprezentarea:

    <arbore 1>
    ...
    <arbore n>
    
    Exemple:

    > insert 3 'c' $ insert 2 'b' $ insert 1 'a' emptyHeap                                        
    3 ('c')
    1 ('a')
      2 ('b')
    
    > insert 5 'e' $ insert 4 'd' $ insert 3 'c' $ insert 2 'b' $ insert 1 'a' emptyHeap
    5 ('e')
    *
    1 ('a')
      3 ('c')
        4 ('d')
      2 ('b')
-}

{-
    Pentru implementarea am aplicat functia "show" pe fiecare arbore din lista de arbori a unui "heap".
-}
instance (Show p, Show k) => Show (BinomialHeap p k) where
    show (BinomialHeap size trees) = 
        case (size, trees) of
        (0, []) -> ""
        _ -> case trees of
            [] -> ""
            _ -> case tail trees of
                [] -> show (last trees)
                _ -> show (head trees) ++ "\n" ++ show (BinomialHeap (size - 1) (tail trees))

{-
    *** TODO ***

    Instanțiați clasa Functor cu constructorul (BinomialTree p). Observați
    că clasa Functor așteaptă constructori unari de tip, dar BinomialTree
    este binar; acest lucru înseamnă că BinomialTree trebuie aplicat parțial
    pe tipul priorităților, care conduce la fixarea acestuia, și varierea doar
    a tipului cheilor (vedeți tipul lui fmap mai jos).

    fmap aplică o funcție pe cheile din arbore, fără a altera structura acestuia.

    Exemple:

    > fmap toUpper $ Node 1 'a' [Node 3 'c' [Node 4 'd' []], Node 2 'b' []]
    1 ('A')
      3 ('C')
        4 ('D')
      2 ('B')
-}

{-
    Pentru implementare am aplicat recursiv "fmap" pe fiecare fiecare copil din lista de copii
    corespunzatoare arborelui, prin aplicarea initiala pe fiecare cheie.
-}
instance Functor (BinomialTree p) where
    -- fmap :: (k1 -> k2) -> BinomialTree p k1 -> BinomialTree p k2
    fmap _ EmptyTree = EmptyTree
    fmap f (Node prio key []) = Node prio (f key) []
    fmap f tree@(Node prio key children) = Node prio (f key) [fmap f child | child <- children]

{-
    *** TODO ***

    Instanțiați clasa Functor cu constructorul (BinomialHeap p). Observațiile
    aferente instanței pt (BinomialTree p) de mai sus sunt valabile și aici.

    fmap aplică o funcție pe cheile din heap, fără a altera structura listei
    de arbori sau a arborilor înșiși.

    Exemple:

    > fmap toUpper $ insert 5 'e' $ insert 4 'd' $ insert 3 'c' $ insert 2 'b' $ insert 1 'a' emptyHeap
    5 ('E')
    *
    1 ('A')
      3 ('C')
        4 ('D')
      2 ('B')
-}

{-
    La fel ca la functia anterioara, doar ca acum pe fiecare arbore.
-}
instance Functor (BinomialHeap p) where
    -- fmap :: (k1 -> k2) -> BinomialHeap p k1 -> BinomialHeap p k2
    fmap _ (BinomialHeap 0 []) = BinomialHeap 0 []
    fmap f heap@(BinomialHeap size trees) = BinomialHeap size [fmap f tree | tree <- trees]

{-
    *** TODO BONUS ***

    Instanțiați clasa Foldable cu constructorul (BinomialTree p) astfel încât
    funcția foldr să împăturească toate cheile din arbore la o singură valoare
    (vedeți tipul lui foldr).

    Dacă încercați să dați o definiție directă, similară celei pe listele
    standard, veți observa că, la un nod din arbore, aveți la dispoziție o cheie
    de tipul k și o listă de acumulatori având tipul [b] (rezultată din
    împăturirea recursivă a copiilor), astfel încât nu este clar pe ce parametru
    de tipul b trebuie aplicată funcția f de la foldr. Cumva, ar trebui să
    combinăm toți acumulatorii din listă într-unul singur de tipul b, dar
    nu știm să facem acest lucru pt orice tip b.

    Prin urmare, vom căuta o abordare alternativă. Observăm că, prin aplicarea
    parțială a funcției f asupra unei chei de tipul k, obținem o funcție cu
    tipul (b -> b). Dacă ne mulțumim doar cu aceste aplicații parțiale, putem
    obține prin împăturirea recursivă a copiilor o listă cu tipul [b -> b], în
    loc de [b]. Pe aceste funcții le putem combina întotdeauna prin compunere,
    indiferent de tipul b. În final, la nivelul rădăcinii, obținem o singură
    funcție, pe care o putem aplica asupra acumulatorului inițial de la foldr.

    Constrângeri: Nu se acceptă liniarizarea prealabilă a arborelui într-o listă
    de chei și aplicarea foldr pe lista rezultantă. Dimpotrivă, în exemplele
    de mai jos, liniarizarea însăși este definită pe baza lui foldr implementat
    de voi.

    Exemple:

    -- Cheia maximă din arbore (caracterele sunt ordonate)
    > foldr max 'a' $ Node 1 'a' [Node 3 'c' [Node 4 'd' []], Node 2 'b' []]
    'd'

    -- maximum este predefinit, într-o manieră similară celei cu foldr de mai sus.
    > maximum $ Node 1 'a' [Node 3 'c' [Node 4 'd' []], Node 2 'b' []]
    'd'

    -- Liniarizarea arborelui, i.e. lista cheilor (String = [Char])
    > foldr (:) [] $ Node 1 'a' [Node 3 'c' [Node 4 'd' []], Node 2 'b' []]
    "acdb"

    -- toList este predefinit prin foldr ca mai sus.
    > toList $ Node 1 'a' [Node 3 'c' [Node 4 'd' []], Node 2 'b' []]
    "acdb"
-}

{-
    Pentru implementare am abordat o rezolvare similara cu cea din cadrul functiei "show" pentru
    un arbore binomial, considerand o functie interioara(prin intermediul unui "let") ce primeste
    ca parametru o functie, un nod si un nivel de adancime in lista de copii si formeaza lista 
    de functii obtinuta prin aplicarea partiala a lui "g" pe fiecare cheie corespunzatoare unui
    copil. La final compunem fiecare functie din lista respectiva, acumulatarul fiind functia
    identitate. Folosim functia creeata pentru a o aplica cu parametrii initiali primiti in
    functia principala, cu nivelul de adancime "0".
-}
instance Foldable (BinomialTree p) where
    -- foldr :: (k -> b -> b) -> b -> BinomialTree p k -> b
    foldr _ acc EmptyTree = acc
    foldr f acc tree =
        let
            getFunctions g EmptyTree _ = id
            getFunctions g (Node _ key children) depth =
                let functions = g key : [getFunctions g child (depth + 1) | child <- children]
                    result = foldl (.) id functions
                in result
        in 
            let composition = getFunctions f tree 0
            in composition acc