### Parser SQL

#### Introducere

Acest parser SQL în Haskell este capabil să preia comenzi SQL de bază și să le convertească într-un format JSON standardizat. Parserul include implementări pentru comenzi SQL precum `SELECT`, `CREATE TABLE`, `INSERT INTO`, `UPDATE`, și `DROP TABLE`. Fiecare comandă SQL este transformată într-o structură de date Haskell (AST), iar apoi într-un format JSON utilizabil pentru procesare ulterioară sau persistență.

#### Cum se transformă query-urile în JSON

1. **SELECT**:

   - Comanda SQL:
     ```sql
     SELECT col1, col2, col3 FROM myTable;
     ```
   - Reprezentarea JSON:
     ```json
     {
       "columns": ["col1", "col2", "col3"],
       "condition": null,
       "statement": "SelectStmt",
       "table": "myTable"
     }
     ```

2. **CREATE TABLE**:

   - Comanda SQL:
     ```sql
     CREATE TABLE myTable (col1 INT PRIMARY KEY, col2 VARCHAR NOT NULL, col3 DATE);
     ```
   - Reprezentarea JSON:
     ```json
     {
       "columns": [
         { "constraint": "PrimaryKey", "name": "col1", "type": "Int" },
         { "constraint": "NotNull", "name": "col2", "type": "VarChar" },
         { "constraint": null, "name": "col3", "type": "Date" }
       ],
       "statement": "CreateStmt",
       "table": "myTable"
     }
     ```

3. **INSERT INTO**:

   - Comanda SQL:
     ```sql
     INSERT INTO myTable VALUES (1, 'myName', '24-12-2025');
     ```
   - Reprezentarea JSON:
     ```json
     {
       "columns": null,
       "statement": "InsertStmt",
       "table": "myTable",
       "values": [
         { "type": "SQLInt", "value": "1" },
         { "type": "SQLString", "value": "myName" },
         { "type": "SQLDate", "value": "24-12-2025" }
       ]
     }
     ```

4. **UPDATE**:

   - Comanda SQL:
     ```sql
     UPDATE myTable SET col1 = 'val1', col2 = 2;
     ```
   - Reprezentarea JSON:
     ```json
     {
       "condition": null,
       "statement": "UpdateStmt",
       "table": "myTable",
       "updates": [
         { "column": "col1", "value": "SQLString \"val1\"" },
         { "column": "col2", "value": "SQLInt 2" }
       ]
     }
     ```

5. **DROP TABLE**:
   - Comanda SQL:
     ```sql
     DROP TABLE myTable;
     ```
   - Reprezentarea JSON:
     ```json
     {
       "statement": "DropStmt",
       "table": "myTable"
     }
     ```

#### Descrierea Codului

##### 1. Definirea Tipurilor de Date

Codul utilizează tipuri de date Haskell pentru a reprezenta valorile și comenzile SQL:

- **SQLValue**: Reprezintă valorile SQL de tipuri variate (șiruri, întregi, date, float).
- **ConstraintType**: Tipuri de constrângeri pentru coloane (NotNull, PrimaryKey, ForeignKey).
- **DataType**: Tipuri de date pentru coloane (IntType, FloatType, VarCharType, DateType).
- **SQLStatement**: Reprezintă comenzi SQL (SELECT, CREATE TABLE, INSERT INTO, UPDATE, DROP TABLE).
- **Condition**: Reprezintă condițiile dintr-o comandă SQL, cum ar fi comparațiile și logicile AND/OR.

##### 2. Implementarea JSON

Fiecare tip de date este instanțiat cu o implementare a clasei `ToJSON` din librăria `Aeson`, pentru a transforma structurile în format JSON. De exemplu, `SQLValue` este transformat într-un obiect JSON cu tipul valorii și valoarea propriu-zisă:

```haskell
instance ToJSON SQLValue where
    toJSON (SQLString s) = object ["valueType" .= ("String" :: String), "value" .= s]
    toJSON (SQLInt i) = object ["valueType" .= ("Int" :: String), "value" .= i]
    toJSON (SQLFloat f) = object ["valueType" .= ("Float" :: String), "value" .= f]
    toJSON (SQLDate d) = object ["valueType" .= ("Date" :: String), "value" .= d]
```

##### 3. Parcurgerea Comenzilor SQL

Fiecare comandă SQL este parsată folosind librăria `Parsec`:

- **parseSelect**: Parsează comenzile `SELECT` și extrage coloanele și condițiile.
- **parseCreate**: Parsează comanda `CREATE TABLE` și extrage tabelul și coloanele.
- **parseInsert**: Parsează comanda `INSERT INTO` și extrage valorile de inserat.
- **parseUpdate**: Parsează comanda `UPDATE` și extrage modificările.
- **parseDrop**: Parsează comanda `DROP TABLE`.

Fiecare parser returnează un tip corespunzător, cum ar fi `SelectStmt`, `CreateStmt`, etc.

##### 4. Main Program

În funcția principală `main`, utilizatorul poate introduce un query SQL. Acesta este procesat de parser, iar rezultatul (AST) este transformat într-un obiect JSON și salvat într-un fișier:

```haskell
main :: IO ()
main = do
    input <- getLine
    case parse (parseSQL <* eof) "" input of
        Left err -> do
            print err
            B.writeFile "./src/output.json" (encode Null)
        Right ast -> do
            let jsonOutput = encode ast
            B.writeFile "./src/output.json" jsonOutput
```

##### 5. Utilizarea Librăriilor

- **GHC.Generics**: Permite folosirea derivațiilor automatizate pentru tipurile de date generice.
- **Data.Aeson**: Folosit pentru a implementa funcționalitatea de serializare în JSON.
- **Text.Parsec**: Librărie de parsare pentru procesarea sintaxei SQL.
- **Control.Monad**: Folosit pentru manipularea monadelor în Haskell (de exemplu, `void` pentru a ignora rezultate).

#### Concluzie

Acest parser SQL simplu, implementat în Haskell, poate fi utilizat pentru a analiza și transforma comenzi SQL de bază într-un format JSON. Codul oferă o bază solidă pentru extinderea suportului pentru alte tipuri de comenzi SQL și pentru integrarea în aplicații mai mari ce necesită procesarea SQL.
