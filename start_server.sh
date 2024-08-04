#!/bin/bash

PORT=6579

free_port() {
  PID=$(sudo lsof -t -i:$PORT)
  if [ -n "$PID" ]; then
    echo "Finalizando processo $PID usando a porta $PORT"
    sudo kill -9 $PID
  else
    echo "Porta $PORT está livre."
  fi
}

# Liberar a porta
free_port

# Compilar os arquivos Erlang
echo "Compilando arquivos Erlang..."
erlc -o ebin simple_memory_db.erl
erlc -o ebin simple_memory_serve.erl

# Iniciar o shell Erlang e adicionar os caminhos necessários
sudo erl -pa ebin deps/jsx/ebin -eval "simple_memory_db:start(), simple_memory_serve:start()"
