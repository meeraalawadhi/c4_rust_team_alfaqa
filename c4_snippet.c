// c4_snippet.c
#include <stdio.h>
int main() {
    char *s = "hello\nworld";
    if (s != 0) {
        printf("%s", s);
        return 0xFF;
    }
    return 0;
}