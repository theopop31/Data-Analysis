Popescu Ioan Emanuel Theodor - 323CBa

    ===>TASKSET 1<===

    -->Task 1<--
    Pentru rezolvarea acestui task am creat mai intai o functie care transforma
o lista de string-uri cu valori reale intr-o lista de valori reale. Pentru a
transforma un string in float vom folosi functia reads, pe care o obligam sa 
returneze tupluri de forma (Float, String), din care luam doar prima valoare,
daca lista returnata de reads nu este vida. Am mai facut o functie care ia o
lista de string-uri si returneaza o lista de string-uri care reprezina mediile,
si pe care o dam ca parametru unui map pe un Table.

    -->Task 2<--
    Pentru acest task am refolosit functia string_to_float de la task-ul anterior
si am integrat-o in diferite alte functii.

    -->Task 3<--
    Pentru acest task am facut o functie care transforma un Table intr-o matrice
de float-uri. O alta functie care primeste o matrice de float-uri va calcula suma
de pasi pe fiecare ora, adunand coloanele matricei. Functia divideSteps ia lista
cu numarul de pasi pe fiecare ora si imparte fiecare element la numarul de persoane
din Table, iar lista cu mediile rezultate va fi transmisa ca parametru unei functii
care face conversia la lista de string-uri.

    -->Task 4<--
    Pentru acest task am implementat o functie care ia un Table si extrage din el
o matrice care contine minutele. Functia getColumn extrage o coloana dintr-o matrice
oarecare, pe care o vom folosi ca sa obtinem numarul de persoane pentru fiecare range.

    -->Task 5<--
    Lucrul nou care apare in acest task este sortarea. Din cauza faptului ca sortarea
trebuie facuta in functie de 2 parametrii, am ales sa fac o lista de tupluri cu elementele
care trebuiesc sortate, pe care o voi sorta cu sortBy, pentru care am implementat o functie
myCompare care intoarce un ordering in functie de numarul de pasi, iar la egalitate folosesc
functia compare din Haskell pentru compararea string-urilor. Dupa sortare, folosesc functia
decodePairs ca sa transform rezultatul intr-un Table.

    -->Task 6<--
    Am aplicat aceeasi idee ca la task-ul 5, numai ca in loc de tupluri am facut perechi de
4 elemente pentru sortare (nume, average first, average last si diff), sortez dupa diferenta
si apoi decodez perechile.

    -->Task 7<--
    Nu cred ca e mult de mentionat aici, un map pe Tale pe care aplic alt map cu functia data,
deci map-ul dat ca functie va avea ca elemente String-uri, iar map-ul exterior coloane din Table.

    -->Task 8<--
    Din cate am inteles, lista de String-uri data ca parametru la rmap ar trebui sa reprezinte
noile nume de coloane, deci le adaug la inceputul Table-ului rezultat in urma aplicarii unui map
cu functia data ca param. Pentru get_sleep_total am refolosit functia rowSum de la task-ul 2 care
face suma pe un Row.

    ===>TASKSET 2<===

    -->Task 1<--
    Pentru sortare, am folosit tot functia sortBy impreuna cu o functie de comparare myCompare3
care compara 2 Row-uri c1 si c2 dupa elementul de la pozitia cNum, indice care este obtinut cu
ajutorul functiei getColumnNumber, care primeste un nume de coloana, un Row si intoarce indexul
coloanei cautate in Row-ul respectiv.

    -->Task 2<--
    Functia checkHeader verifica daca 2 tabele au aceleasi nume de coloane. Pentru uniune, daca
header-ele sunt identice, vom face o simpla concatenare a tabelului t2 (fara header) la sfarsitul
tabelului t1.

    -->Task 3<--
    Pentru hunion, am implementat mai intai o functie care genereaza un Row gol de o anumita
dimensiune, pe care am folosit-o in functia addEmptyRows, care realizeaza practic padding-ul daca
avem 2 tabele de dimensiuni diferite. Dupa ce aducem ambele tabele la aceeasi dimensiune, uniunea
se reduce la un simplu zipWith cu operatia de concatenare intre cele 2 tabele.

    -->Task 4<--
    Ceea ce am facut in mare pentru acest task a fost sa caut corespondentul din tabelul t2 al
elementului cu cheia cautata din tabelul t1. Daca am gasit aceasta corespondenta, atunci modific
randul prin concatenarea a 2 randuri. In primul rand o sa inlocuiesc elementele de la pozitiile
comune din al doilea daca acestea nu sunt "", iar din al doilea o sa stereg elementele de pe
pozitiile comune.

    -->Task 5<--
    Pentru produsul cartezian, am implementat mai intai o functie care aplica o operatie intre un
Row si fiecare Row dintr-un table. Functia allRows aplica pentru fiecare intrare a tabelului
functia oneRow cu tabelul t2 si concateneaza toate rezultatele, creand astfel produsul cartezian.
Functia cartesian adauga header-ul la rezultatul functiei allRows.

    -->Task 6<--
    Pentru acest task, am implementat mai intai o functie care intoarce o lista cu toti indicii
coloanelor specificate prin nume dintr-un tabel. Functia columnsList intoarce lista cu coloanele
cerute. Pentru ca functia columnsList transforma practic coloanele cerute in randuri, trebuie sa
facem conversia inapoi la coloane, iar functia listToTable face conversia dintr-un rand intr-o
coloana, pe care o transforma intr-un tabel. Functia convertColumnList aplica functia descrisa
anterior pe un intreg tabel, rezultatul fiind o lista de tabele. Pe aceasta lista cu tabele vom
aplica un foldl cu operatia hunion.

    -->Task 7<--
    Pentru acest task, tot ce am facut este sa aplic un filter pe tabel, iar functia de filtrare
data ca parametru este aplicata pe coloana ceruta a fiecarui rand.

    ===>TASKSET 3<===

    -->Task 1<--
    Tot ce am facut pentru acest task a fost sa folosesc functiile de la taskset-urile 1 si 2
pentru a inrola Query in clasa Eval.

    -->Task 2<--
    In cazul inrolarii Float in clasa FEval, a trebuit sa compar ref cu valoarea in Float a
elementului de pe coloana colname. Pentru String, nu mai este nevoie de conversie, din moment ce
si ref este un string.

    -->Task 3<--
    Primul pas in rezolvarea acestui task a fost sa aplic o operatie EdgeOp intre un Row si un Table,
ceea ce se face prin functia rowConversion. Rezultatul acestei functii este un Table de tip Graph, cu
header-ul specificat. Urmatorul pas a fost functa tableConversion, care aplica functia descrisa anterior
pe fiecare rand dintr-un tabel, iar rezultatul este o lista de Tabele. Tot ceea ce a mai ramas de facut
a fost sa transform lista de tabele intr-un singur tabel, pe care sa il sortam dupa criteriul cerut si
si sa adaug header-ul la inceput.