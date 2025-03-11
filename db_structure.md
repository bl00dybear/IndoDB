# Structura Fișierului DB

## Pagina 0 - Header-ul bazei de date
```
- Semnătură (ex: "MYDB01")
- Versiune
- Dimensiunea paginii (ex: 4096 bytes)
- Offset către root-ul tabelei principale
- Offset către lista de indexuri
- Alte metadate
```

## Pagina 1 - Root B-Tree al tabelei principale
```
- Tip pagină: B-Tree root
- Chei (ID-uri) și pointeri către date
- Structură nod B-Tree:
  (cheie ID, ptr stânga, ptr dreapta)
```

## Pagini 2-N - Pagini de date ale tabelei
```
- Fiecare pagină conține mai multe rânduri
- Structură rând:
  (ID, Valoare1, Valoare2, Valoare3)
- Legături pentru scanări secvențiale
```

## Pagina M - Root B-Tree al indexului pe Val1
```
- Tip pagină: B-Tree index
- Chei (Valoare1) și pointeri către ID-uri
- Structură nod B-Tree:
  (cheie Valoare1, pointer către ID)
```

## Pagini M+1 - X - Noduri pentru index Val1
```
- Structură similară cu root-ul B-Tree
```

## Pagina Y - Root B-Tree al indexului pe Val2
```
- Structură similară cu indexul pe Val1
```

## Pagini Y+1 - Z - Noduri pentru index Val2
```
- Structură similară cu indexul pe Val1
```

## Pagina INDEX_LIST - Metadate despre indexuri
```
- Lista indexurilor create
- Offset-uri către root-urile B-Tree ale indexurilor
```

---

## Note Suplimentare
- Paginile sunt de dimensiune fixă (ex: 4KB)
- Tabela principală are un singur B-Tree pe ID
- Fiecare index este un B-Tree separat
- Se păstrează o listă a indexurilor în metadate
- Pointerii din index pot referi fie ID-uri, fie direct locații în fișier

