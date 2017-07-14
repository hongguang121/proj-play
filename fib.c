#include <stdio.h>
#include <stdlib.h>

long fib(int n){
    return n < 2 ? n : fib(n - 1) + fib(n - 2); 
}

int main(int argc, char** argv) {
    int n = 45;
    long res = fib(n);
    printf("%ld\n", res);
    return 0;
}