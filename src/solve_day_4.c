#include <openssl/md5.h>
#include <string.h>
#include <stdio.h>
#include <stdlib.h>

void fast_md5(char* buf, const char* str) {
    unsigned char digest[MD5_DIGEST_LENGTH];
    static char out[33];
    MD5((unsigned char*)str, strlen(str), (unsigned char*)&digest);
    for(int i = 0; i < 16; ++i)
        sprintf(&out[i*2], "%02x", (unsigned int)digest[i]);
    out[32] = '\0';
    strcpy(buf, out);
}

int check_string( const char* to_check, const char* check_against ) {
  for ( int i = 0; i < strlen(check_against); ++i )
    if ( to_check[i] != check_against[i] ) return 0;
  return 1;
}

void build_string( char* strbuf, int i ) {
  char num_buf[128] = {0};
  sprintf(num_buf, "%d", i);
  size_t len_base = strlen(strbuf);
  size_t len_i = strlen(num_buf);
  strcpy(strbuf + len_base, num_buf);

}

int solve( const char* base_str, const char* check_against ) {
  char hashbuf[128] = {0};
  char strbuf[128] = {0};

  int i = 0;
  do {
    // copy base string into string buffer
    strcpy( strbuf, base_str );

    // append the next number inside the buffer
    build_string( strbuf, i );

    // hash the result
    fast_md5( hashbuf, strbuf );

    // check it
    if ( check_string( hashbuf, check_against ) ) return i;
    // if it wasn't the number we're looking for, increment `i` and try again
    ++i;
  } while (1);

}
