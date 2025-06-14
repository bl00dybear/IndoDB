# Prompt Engineering

### [prompt1.md](./prompts/prompt1.md) – Arhitectură pentru un DBMS scris în C + Haskell

Acest prompt descrie cum se poate construi un sistem de gestiune a bazelor de date (similar cu SQLite) folosind C pentru motorul principal și Haskell pentru parsarea SQL-ului. Explică arhitectura completă, structura arborelui de operații, sistemul de paginare, B-Tree indexing și tranzacțiile.

---

### [prompt2.md](./prompts/prompt2.md) – Parser SQL în Haskell cu output JSON

Aici este prezentat un parser complet pentru comenzi SQL de bază (`SELECT`, `CREATE`, `INSERT`, `UPDATE`, `DROP`), scris în Haskell. Codul convertește AST-ul în format JSON utilizabil pentru aplicații care au nevoie de interpretare sau validare SQL.

---

### [prompt3.md](./prompts/prompt3.md) – Funcții POSIX pentru fișiere/directoare în C

Promptul solicită o implementare în C folosind doar apeluri POSIX. Codul rezultat oferă funcții pentru copiere de fișiere, copiere și ștergere recursivă a directoarelor și redenumire, toate cu gestionare de erori via `fprintf(stderr, ...)`.

---
