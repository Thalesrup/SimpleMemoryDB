# SimpleMemoryDB

**Um banco de dados chave-valor simples em Erlang, inspirado no Redis.**

## Descrição
SimpleMemoryDB implementa um banco de dados chave-valor básico em Erlang. O objetivo é fornecer uma solução simples e eficiente para armazenar e recuperar pares chave-valor em memória.

## Funcionalidades
* **Armazenamento de pares chave-valor:** Permite armazenar qualquer tipo de dados como valor associado a uma chave.
* **Recuperação de valores:** Permite recuperar o valor associado a uma chave específica.
* **Remoção de pares:** Permite remover um par chave-valor.
* **Persistência (opcional):** Pode ser facilmente estendido para persistir os dados em disco.

## Como usar
1. **Compilação:**
   ```bash
   erlc src/*.erl

2. **Executar:**
    ```bash
    simple_memory_db:start().
    simple_memory_server:start().
