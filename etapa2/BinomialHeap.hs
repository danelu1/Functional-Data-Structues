module BinomialHeap where

import Data.Function (on)
import Data.List

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
    deriving (Show, Eq)

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
    deriving (Show, Eq)

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
                    EmptyTree -> (attach y x, EmptyTree)
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
