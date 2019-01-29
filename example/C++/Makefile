CC=g++
CFLAGS=-Wextra -Wall -I/usr/include/boost
LDFLAGS=-L/usr/lib/x86_64-linux-gnu -lutil -lboost_iostreams -lboost_system -lboost_filesystem -L. -lCNStream
EXEC=prog
SRCS= $(wildcard src/*.cpp)
OBJ= $(SRCS:.cpp=.o)

all: $(EXEC)

prog: $(OBJ)
	$(CC) -o $@ $^ $(LDFLAGS) $(LIBS)

%.o: %.cpp
	$(CC) -o $@ -c $< $(CFLAGS)

.PHONY: clean

clean:
	rm -f ./src/*.o
	rm -f $(EXEC)


