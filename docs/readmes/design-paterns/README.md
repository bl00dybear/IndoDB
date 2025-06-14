# Design Patterns

Acest document detaliază **design patterns**-urile identificate în implementarea proiectului nostru, oferind o perspectivă asupra arhitecturii și deciziilor de design utilizate pentru a structura și organiza codul.

---

### 1. **Command Pattern**

- **Loc:** `Statement` și funcția `execute_statement`.
- **Descriere:** Fiecare comandă SQL (SELECT, INSERT, UPDATE, DELETE) este reprezentată printr-o structură `Statement`. Execuția se face polimorf, în funcție de tipul comenzii. Astfel, comanda este tratată ca un obiect care „știe” cum să se execute.

---

### 2. **Factory Pattern**

- **Loc:** Funcțiile `parse_statement` și parserul Haskell/JSON.
- **Descriere:** Creează instanțe de `Statement` și structuri asociate pe baza inputului SQL. Separă logica de parsare de instanțierea efectivă a obiectelor din business logic.

---

### 3. **Visitor Pattern**

- **Loc:** Funcții precum `display_all_rows`, `recursive_update_rows`, `recursive_delete_rows`.
- **Descriere:** Permite aplicarea unor operații diferite (afișare, actualizare, ștergere) asupra nodurilor arborelui B fără a modifica structura sa internă.

---

### 4. **Strategy Pattern**

- **Loc:** Serializarea/deserializarea rândurilor și validarea tipurilor de date.
- **Descriere:** Selectarea algoritmului potrivit (strategie) pentru manipularea valorilor, în funcție de tipul coloanei (int, varchar, etc).

---

### 5. **Facade Pattern**

- **Loc:** Funcția `execute_statement`, împreună cu interfața CLI.
- **Descriere:** Ascunde complexitatea logicii interne și oferă o interfață unificată pentru executarea comenzilor SQL.

---

### 6. **Singleton Pattern (implicit)**

- **Loc:** Variabile globale (`root`, `df`, `global_id`, etc).
- **Descriere:** Asigură existența unei singure instanțe de stare globală a bazei de date în cadrul aplicației.

---

### 7. **Chain of Responsibility**

- **Loc:** Pipeline-ul de procesare SQL → Validare → Execuție.
- **Descriere:** Fiecare etapă are posibilitatea de a prelucra inputul sau de a opri propagarea dacă apare o eroare.

---

### 8. **Adapter Pattern**

- **Loc:** Integrarea parserului Haskell cu logica C prin pipe-uri.
- **Descriere:** Transformă outputul AST-ului Haskell în formate compatibile cu structurile utilizate în C.

---

### 9. **Builder Pattern**

- **Loc:** Construirea rândurilor în `insert_updated_row`.
- **Descriere:** Rândurile sunt construite incremental, câmp cu câmp, facilitând validarea și controlul complet asupra structurii.

---

### 10. **Template Method Pattern**

- **Loc:** Operațiile pe arborele B (inserare, actualizare, ștergere).
- **Descriere:** Toate urmează aceeași schemă logică de bază, dar permit pași personalizați în funcție de operație.

---

### 11. **Observer Pattern (rudimentar)**

- **Loc:** Comunicarea cu procesul Haskell prin pipe-uri.
- **Descriere:** Procesul principal „ascultă” outputul parserului pentru a determina execuția condițiilor în WHERE.

---

### 12. **Layered Architecture**

- **Structură:**

  - **CLI Layer:** Interfață utilizator + parsing comenzilor
  - **Business Logic Layer:** Interpretarea și execuția comenzilor
  - **Data Access Layer:** Operații pe arborele B și pe fișiere
  - **Storage Layer:** Manipularea fișierelor prin memory-mapping

---

### 13. **Pipe and Filter Architecture**

- **Loc:** Fluxul de date: SQL → JSON → Statement → Execuție → Persistență.
- **Descriere:** Datele sunt procesate secvențial printr-o serie de transformări, fiecare componentă fiind independentă și focalizată pe un singur pas.
