Starten des Systems

cd exercise1/
#Kompilieren
erl -make
#Starten der Erlang shell mit kompilierten Dateien
erl -pa ebin/
#In der Erlang Shell:
server:startMe().
client:startClients().

Timeouts, Wartezeiten und Anzahl der Clients können in den Dateien server.cfg und client.cfg in /config bearbeitet werden.