# User Stories și Backlog

Această secțiune documentează funcționalitățile de bază ale aplicației IndoDB sub formă de **user stories** și definește un **backlog** inițial care reflectă nevoile utilizatorilor finali. Fiecare poveste este însoțită de criterii clare de acceptare.

## Backlog

### 1. Creare și gestionare a bazei de date

- **Story**: Ca utilizator, vreau să pot crea o bază de date într-un singur fișier, astfel încât toate datele să fie stocate într-un mod organizat și eficient.
- **Criterii de acceptare**:
  - Comanda `CREATE DATABASE nume_baza;` creează un fișier corespunzător.
  - Fișierul conține o pagină de metadate care descrie schema bazei de date.

---

### 2. Crearea tabelelor

- **Story**: Ca utilizator, vreau să pot crea tabele, astfel încât să pot organiza datele conform nevoilor mele.
- **Criterii de acceptare**:
  - Comanda `CREATE TABLE users (id INT, name TEXT);` creează o structură nouă în baza de date.

---

### 3. Ștergerea tabelelor

- **Story**: Ca utilizator, vreau să pot șterge tabele, astfel încât să nu păstrez informații inutile.
- **Criterii de acceptare**:
  - `DROP TABLE users;` elimină schema și paginile asociate din fișierul bazei de date.

---

### 4. Inserarea datelor într-un tabel

- **Story**: Ca utilizator, vreau să pot insera date într-un tabel, astfel încât să pot popula baza de date cu informații utile.
- **Criterii de acceptare**:
  - Comanda `INSERT INTO users (id, name) VALUES (1, 'Alice');` scrie datele în fișier.
  - Rândurile sunt adăugate într-o pagină de date disponibilă.

---

### 5. Citirea datelor dintr-un tabel

- **Story**: Ca utilizator, vreau să pot citi datele dintr-un tabel folosind comenzi SQL, pentru a analiza informațiile stocate.
- **Criterii de acceptare**:
  - `SELECT * FROM users;` returnează toate rândurile.
  - `SELECT name FROM users WHERE id = 1;` returnează datele filtrate.

---

### 6. Actualizarea datelor într-un tabel

- **Story**: Ca utilizator, vreau să pot modifica datele existente într-un tabel, pentru a menține informațiile actualizate.
- **Criterii de acceptare**:
  - `UPDATE users SET name = 'Bob' WHERE id = 1;` modifică rândul.
  - Modificările sunt jurnalizate și persistente.

---

### 7. Ștergerea rândurilor dintr-un tabel

- **Story**: Ca utilizator, vreau să pot șterge datele dintr-un tabel, pentru a elibera spațiu și elimina informații inutile.
- **Criterii de acceptare**:
  - `DELETE FROM users WHERE id = 1;` elimină rândul.
  - Spațiul este marcat ca reutilizabil.

---

### 8. Suport pentru tranzacții (ACID)

- **Story**: Ca utilizator, vreau modificările să fie sigure și reversibile, pentru a preveni coruperea datelor.
- **Criterii de acceptare**:
  - `BEGIN TRANSACTION;` inițiază o tranzacție.
  - `COMMIT;` finalizează modificările.
  - `ROLLBACK;` anulează modificările din tranzacție.

---

### 9. Implementarea mecanismului de paginare

- **Story**: Ca utilizator, nu vreau să am limite legate de numărul de tabele sau rânduri.
- **Criterii de acceptare**:
  - Se implementează pagini de 4 KB.
  - Paginile sunt identificate prin Page ID.
  - Navigare eficientă între pagini.

---

### 10. Interpret SQL minim

- **Story**: Ca utilizator, vreau să pot introduce comenzi SQL într-un terminal și să văd rezultatele.
- **Criterii de acceptare**:
  - DBMS-ul procesează comenzi SQL într-un mod interactiv.
  - `SELECT * FROM users;` returnează un rezultat formatat.

---

### 11. Gestionarea erorilor

- **Story**: Ca utilizator, vreau ca toate erorile să fie gestionate corect, pentru a evita blocaje sau pierderi de date.
- **Criterii de acceptare**:
  - Erorile sunt capturate și tratate adecvat, cu mesaje explicite și comportament predictibil.

---
