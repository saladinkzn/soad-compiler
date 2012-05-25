all: parser

OBJS = parser.o  \
       codegen.o \
       main.o    \
       tokens.o  \

LLVM_MODULES = all

CPPFLAGS = `llvm-config --cppflags $(LLVM_MODULES)`
LDFLAGS = `llvm-config --ldflags $(LLVM_MODULES)`
LIBS = `llvm-config --libs $(LLVM_MODULES)`

clean:
	$(RM) -rf parser.cpp parser.hpp parser tokens.cpp $(OBJS)

parser.cpp: parser.y
	bison -d -o $@ $^
	
parser.hpp: parser.cpp

tokens.cpp: tokens.l parser.hpp
	flex -o $@ $^

%.o: %.cpp
	g++ -c $(CPPFLAGS) -o $@ $< -pthread -ldl


parser: $(OBJS)
	g++ -o $@ $(LDFLAGS) $(OBJS) $(LIBS) -pthread -ldl


