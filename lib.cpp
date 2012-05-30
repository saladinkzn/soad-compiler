#include <iostream>
#include <string.h>
using namespace std;
extern "C" void tellDouble(double d) {
	std::cout << d << endl;
}

extern "C" void tellInteger(int d) {
	std::cout << d << endl;
}

extern "C" void tellString(char* d) {
	std::cout << d << endl;
}

extern "C" void tellChar(char d) {
	std::cout << d << endl;
}

extern "C" double readDouble() {
	double d;
	std::cin >> d;
	return d;
}

extern "C" char* readString() {
	char* d = new char[100];
	std::cin >> d;
	return d;
}

extern "C" int readInteger() {
	int d;
	std::cin >> d;
	return d;
}

extern "C" char* concat(char* a, char* b) {
	char* buf = new char[strlen(a)+strlen(b)+1];
	strcpy(buf, a);
	strcat(buf, b);
	return buf;
}


